import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, mkdirSync, writeFileSync, readFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawn } from 'node:child_process';
import { createInterface } from 'node:readline';
import { once } from 'node:events';

function fixture(t, sdkBody) {
  const root = mkdtempSync(join(tmpdir(), 'claude-quota-test-'));
  const adapter = join(root, 'adapter.js');
  const sdk = join(root, 'node_modules/@anthropic-ai/claude-agent-sdk');
  const log = join(root, 'calls.jsonl');
  mkdirSync(sdk, { recursive: true });
  writeFileSync(adapter, '');
  writeFileSync(join(sdk, 'package.json'), JSON.stringify({ type: 'module', exports: './sdk.mjs' }));
  writeFileSync(join(sdk, 'sdk.mjs'), `
    import { appendFileSync } from 'node:fs';
    function log(value) { appendFileSync(${JSON.stringify(log)}, JSON.stringify(value) + '\\n'); }
    ${sdkBody}
  `);
  const child = spawn(process.execPath,
    [fileURLToPath(new URL('./claude-quota.mjs', import.meta.url)), adapter, '/test/claude'],
    { stdio: ['pipe', 'pipe', 'pipe'] });
  const lines = createInterface({ input: child.stdout });
  t.after(async () => {
    lines.close();
    if (child.exitCode === null) {
      child.kill('SIGTERM');
      await once(child, 'exit');
    }
    rmSync(root, { recursive: true, force: true });
  });
  return {
    async read(id) {
      const response = once(lines, 'line');
      child.stdin.write(JSON.stringify({ id, method: 'usage/read' }) + '\n');
      return JSON.parse((await response)[0]);
    },
    calls() { return readFileSync(log, 'utf8').trim().split('\n').map(JSON.parse); },
  };
}

test('quota reads reuse one isolated SDK connection without yielding prompts', { timeout: 5000 }, async t => {
  const client = fixture(t, `
    export function query({ prompt, options }) {
      log({ method: 'query', options: { ...options, env: { ANTHROPIC_API_KEY: options.env.ANTHROPIC_API_KEY } } });
      const iterator = prompt[Symbol.asyncIterator]();
      return {
        async initializationResult() { log({ method: 'initialize' }); },
        async usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET(options) {
          log({ method: 'usage', options });
          const yielded = await Promise.race([
            iterator.next().then(() => true),
            new Promise(resolve => setTimeout(() => resolve(false), 10)),
          ]);
          if (yielded) throw new Error('Unexpected prompt');
          return {
            rate_limits_available: true,
            rate_limits: {
              five_hour: { utilization: 3, resets_at: '2033-05-18T03:33:20Z', private: 'discard' },
              seven_day: null,
            },
            account: 'private account identity',
          };
        },
        close() { log({ method: 'close' }); },
      };
    }
  `);
  for (const id of [1, 2]) {
    assert.deepEqual(await client.read(id), {
      id, result: { five_hour: { utilization: 3, resets_at: '2033-05-18T03:33:20Z' }, seven_day: null },
    });
  }
  const calls = client.calls();
  assert.deepEqual(calls.map(call => call.method), ['query', 'initialize', 'usage', 'usage']);
  const options = calls[0].options;
  assert.equal(options.pathToClaudeCodeExecutable, '/test/claude');
  assert.deepEqual(options.settingSources, []);
  assert.deepEqual(options.tools, []);
  assert.deepEqual(options.mcpServers, {});
  assert.equal(options.settings.disableAllHooks, true);
  assert.equal(options.persistSession, false);
  assert.equal(options.env.ANTHROPIC_API_KEY, '');
  assert.deepEqual(calls[2].options, { skipBehaviors: true });
});

test('SDK failures are sanitized and a later interaction starts a fresh reader', { timeout: 5000 }, async t => {
  const client = fixture(t, `
    export function query() {
      log({ method: 'query' });
      return {
        async initializationResult() {},
        async usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET() {
          throw new Error('PRIVATE TOKEN OR ACCOUNT DATA');
        },
        close() { log({ method: 'close' }); },
      };
    }
  `);
  for (const id of [1, 2]) {
    const response = await client.read(id);
    assert.equal(response.id, id);
    assert.match(response.error.message, /Claude quota unavailable/);
    assert.doesNotMatch(JSON.stringify(response), /PRIVATE/);
  }
  assert.deepEqual(client.calls().map(call => call.method), ['query', 'close', 'query', 'close']);
});

test('accounts without subscription quota return unavailable', { timeout: 5000 }, async t => {
  const client = fixture(t, `
    export function query() {
      return {
        async initializationResult() {},
        async usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET() {
          return { rate_limits_available: false, rate_limits: null };
        },
        close() {},
      };
    }
  `);
  assert.ok((await client.read(1)).error);
});
