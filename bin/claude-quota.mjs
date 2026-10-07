// Quota-only SDK connection. No user messages are ever yielded to Claude.
import { realpathSync } from 'node:fs';
import { createRequire } from 'node:module';
import { pathToFileURL } from 'node:url';
import { createInterface } from 'node:readline';

const [acpExecutable, claudeExecutable] = process.argv.slice(2);
let connection;
let releaseInput;
let busy = false;

function close() {
  try { connection?.close(); } catch {}
  connection = undefined;
  releaseInput?.();
}

async function readQuota() {
  if (!connection) {
    // Resolve the SDK already installed with the ACP adapter, including symlinks.
    const require = createRequire(realpathSync(acpExecutable));
    const { query } = await import(pathToFileURL(require.resolve('@anthropic-ai/claude-agent-sdk')));
    const inputClosed = new Promise(resolve => { releaseInput = resolve; });
    connection = query({
      prompt: (async function* () { await inputClosed; })(),
      options: {
        pathToClaudeCodeExecutable: claudeExecutable,
        settingSources: [],
        settings: { disableAllHooks: true },
        tools: [],
        mcpServers: {},
        extraArgs: { 'strict-mcp-config': null },
        persistSession: false,
        // Match agent-shell's subscription authentication.
        env: { ...process.env, ANTHROPIC_API_KEY: '' },
      },
    });
    await connection.initializationResult();
  }
  const usage = await connection.usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET({ skipBehaviors: true });
  if (usage.rate_limits_available !== true || !usage.rate_limits) {
    throw new Error('Subscription quota unavailable');
  }
  // Return only quota windows; no account identity, tokens, or credentials.
  const window = value => value == null ? null : {
    utilization: value.utilization,
    resets_at: value.resets_at,
  };
  return { five_hour: window(usage.rate_limits.five_hour), seven_day: window(usage.rate_limits.seven_day) };
}

const lines = createInterface({ input: process.stdin, crlfDelay: Infinity });
lines.on('line', async line => {
  if (busy) return;
  let request;
  try {
    request = JSON.parse(line);
    if (request.method !== 'usage/read' || !Number.isInteger(request.id)) return;
    busy = true;
    const result = await readQuota();
    process.stdout.write(JSON.stringify({ id: request.id, result }) + '\n');
  } catch {
    close();
    // SDK errors may contain private data. Emit a fixed message instead.
    process.stdout.write(JSON.stringify({ id: request?.id, error: { message: 'Claude quota unavailable; check your subscription login and SDK version.' } }) + '\n');
  } finally {
    busy = false;
  }
});
lines.on('close', close);
process.on('SIGTERM', () => { close(); process.exit(0); });
process.on('SIGINT', () => { close(); process.exit(0); });
