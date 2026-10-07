# Local package sources

`local/agent-shell` is a Git subtree: its source and custom changes are ordinary
files in the Emacs configuration repository. A clone of this repository includes
them without initializing a submodule or restoring a separate package checkout.
`local/packages.json` records the upstream revision and the provenance of the
imported local changes. Future changes are recorded in `.emacs.d` commits.

## agent-shell

Straight uses `:type nil` and the existing `local/agent-shell` source path. It
does not fetch or reset this source during a normal package update. There is no
nested `.git` directory and no separate active `sdb/main` branch here. Running
Git from the package directory operates on the Emacs configuration repository.

The initial import uses upstream commit
`6c91b1fd3d0eaf6c41aadd23e111e37ff737d6b4`, followed by separate commits for:

- Nested command output, streaming capture, TAB navigation, and tests, originally
  `9c008bf8796708a5d9d55c569343716b1c82cd90`.
- Extensible text/SVG header indicators, originally
  `5076c7985d6bfe7bc808374b23cae21b69c1f38f`.

These original identifiers describe the former package repository. The Emacs
repository's import and feature commit identifiers are recorded in
`local/packages.json`. The source files loaded by Emacs remained identical during
the conversion. The subtree import has an upstream snapshot commit and an
integration merge, matching `git subtree add --squash`; the custom changes have
their own subsequent commits. Obtain approval before each future commit.

## Developing and updating

Develop additional functionality on branches of the Emacs configuration repo.
Keep source changes in `local/agent-shell`, personal extensions in `sdb/`, and
configuration in the existing `use-package` block. Review focused diffs and
obtain approval before each commit. For example, from `~/.emacs.d`:

```sh
rtk git diff -- local/agent-shell
rtk git log -- local/agent-shell
rtk proxy /Applications/Emacs.app/Contents/MacOS/Emacs --batch -Q \
  -l bin/test-agent-shell-command-output.el
```

Upstream updates are deliberate subtree imports. Start with a clean repository
and no active agent request. Fetch and inspect the upstream changes first:

```sh
rtk git fetch --no-tags https://github.com/xenodium/agent-shell.git main
rtk git diff <recorded-upstream-commit> FETCH_HEAD
```

Prepare and review the resulting source changes, resolve conflicts preserving
the local features, and run the source suite above and the relevant personal
extension suites documented below. After obtaining the necessary
commit approvals, the standard update operation is:

```sh
rtk git subtree pull --prefix=local/agent-shell --squash \
  https://github.com/xenodium/agent-shell.git main
```

This command creates commits automatically; do not run it as a review-only
operation. When separate approval is needed for each generated commit, prepare
the snapshot and integration commits separately, as for the initial import.
Record the new upstream revision and tested source tree in `local/packages.json`
after validation. Rebuild with `M-x straight-rebuild-package RET agent-shell RET`,
restart Emacs, and check output folds, TAB navigation, and quota headers. No push
to the upstream agent-shell repository is required.

## Backup and restoration

The complete original package Git metadata, including `sdb/main`, local refs,
configuration, and reflogs, is preserved under the ignored
`backups/local-packages/agent-shell-pre-subtree.git/` directory. A verified bundle,
checksum, and verification metadata are also kept under `backups/local-packages/`.
The bundle created on 2026-10-07 restores all 168 refs and the original source at `5076c79`.
Legacy patch exports remain in `backups/local-packages/legacy-patches/`.

Normal restoration now comes from the Emacs configuration Git history. The
backup below is only for recovering the original independent package repository,
in a separate destination outside the active subtree. Substitute the chosen
bundle's absolute path and run:

```sh
rtk git init --initial-branch=sdb/restore-placeholder /tmp/agent-shell-recovery
rtk git -C /tmp/agent-shell-recovery fetch --no-tags \
  /absolute/path/agent-shell-YYYYMMDDTHHMMSSZ.bundle '+refs/*:refs/*'
rtk git -C /tmp/agent-shell-recovery switch sdb/main
rtk git -C /tmp/agent-shell-recovery remote add origin \
  https://github.com/xenodium/agent-shell.git
rtk git -C /tmp/agent-shell-recovery fsck --full
```

This explicit fetch avoids the duplicate-tag error encountered with a mirror
clone using the installed Git version. Include local backups in a regular backup
to another disk or service; backups stored only on this Mac do not cover disk
loss. New source changes are preserved by committing them in `.emacs.d` and
backing up that repository.

## Claude and Codex quota header extension

Approved on 2026-10-07: extend the Codex display to Claude, showing remaining
5-hour and 7-day account quota with the same colors, reset times, and stale
markers. Both providers refresh asynchronously when a session opens, input is
submitted, or a turn completes. This replaces the once-a-minute Codex polling
approved on 2026-10-06. There is no recurring quota timer while idle.

The integration is `sdb/agent-shell-quota.el`, enabled in the agent-shell
`use-package` block. Each provider has its own shared connection and snapshot:

- Codex uses `codex app-server` and `account/rateLimits/read` with its local
  login, without starting a thread or inference turn.
- Claude uses `bin/claude-quota.mjs` and the SDK already bundled with the
  installed `claude-agent-acp`. It opens an ephemeral SDK connection using the
  local Claude subscription login, disables tools, hooks, MCP servers, and
  transcript persistence, and never yields a user prompt. The experimental
  structured usage control query supplies the percentages and reset times;
  SDK changes or missing subscription quota show unavailable data. No SDK
  package installation or direct credential handling is needed.

Overlapping activity requests are combined into one pending read and, when
needed, one follow-up read. Failures retain the last snapshot as stale and
retry on the next interaction. Readers stop when their provider's last shell
closes or the mode is disabled. Enabling the mode attaches to existing shells;
`agent-shell-mode-hook` attaches to future ones. Reloading cancels the old
Codex polling timer.

Successful snapshots retain their percentage colors between interactions.
Idle time alone does not dim them; failed reads, missing refresh timestamps,
and elapsed reset times mark them stale. Hover details show the last refresh.
This corrected the inherited two-minute age rule on 2026-10-07; a live Claude
snapshot over five minutes old retained `agent-shell-success` after reloading
and redrawing, without another quota read.

Text and graphical headers use `agent-shell-header-extra-indicators-function`.
The display uses `agent-shell-success`, `agent-shell-warning`, and
`agent-shell-error` faces: warning at 60% used and error at 85% used. Stale
values use `shadow`; theme changes redraw the header. Reset times appear in
local-time brackets and full hover details. Weekly resets include the weekday,
for example `[Sunday 16:28]`. Set `sdb/agent-shell-quota-show-reset-time` to nil
for hover details only. Disable with `M-x sdb/agent-shell-quota-mode` or refresh
both providers manually with `M-x sdb/agent-shell-quota-refresh`.

Validation completed on 2026-10-07: 38 quota/header tests, 3 mocked SDK helper
tests, and 20 annotation tests passed. Both Lisp files compile with warnings
configured as errors and no warnings emitted. Live account reads succeeded for
both providers. The update is loaded into running Emacs: all five existing
Claude/Codex shells have one quota activity subscription, quota headers include
percentages and reset times, and the old polling timer is absent. No model
prompt was sent for validation. Ready for user review; no commit or push.

The quota suite covers normalization, provider isolation, failures, activity
subscriptions, overlapping requests, cleanup, timer migration, and native
text/SVG headers, including viewport state lookup. The mocked SDK helper tests
check that no prompts are yielded, the connection is reused, output omits
private fields, and failures retry safely.

```sh
rtk proxy ./bin/test-agent-shell-quota.sh
rtk proxy ./bin/test-agent-shell-annotations.sh
```

## Local file-link viewers

Approved on 2026-10-06: ordinary local HTML/HTM links open in the browser;
HTML source citations keep Emacs line/range/column navigation. Extended on
2026-10-07: PNG, JPEG (jpg/jpeg), SVG, GIF, TIFF (tif/tiff), WebP, and PDF
links open in Emacs with the configured native viewers. Other links retain
the existing behavior.

`sdb/agent-shell-file-links.el` is loaded and enabled by the agent-shell
`use-package` block. Its named advice routes local viewer links before
agent-shell's binary-file heuristic can send images/PDFs to the operating
system. It preserves `agent-shell-markdown-open-file-function`, including
custom window placement. File URLs with percent-encoded spaces are supported.
Directories and remote paths retain the existing handler. Source references
on image/PDF links are ignored because those viewers do not use source lines.
SVG displays in Image mode; `C-c C-c` toggles its image and XML source.

This integration lives in the Emacs configuration, without a new patch in the
agent-shell checkout. Its advice uses private Markdown parsing/navigation
functions, so rerun the routing suite after upstream updates:

```sh
rtk proxy ./bin/test-agent-shell-file-links.sh
```

Validation: 15 routing tests passed, covering source range/column navigation,
all binary image extensions and uppercase variants, text SVG fixtures, encoded
SVG filenames, and repeated setup. The browser/external openers are stubbed.
A temporary SVG also rendered in Image mode in the running graphical Emacs;
the temporary file and buffer were removed afterward. The helper and tests
are kept in the Emacs configuration repository.

## Magit review links

Approved on 2026-10-06: add a restricted Markdown link handler for reviewing
staged changes in Magit and test it against the currently proposed commit.
Committing remains subject to explicit approval for each commit and a review
patch supplied beforehand.

`sdb/agent-shell-magit-links.el` is loaded and enabled by the agent-shell
`use-package` block. `magit:staged` opens the current repository's staged diff;
`magit:staged?repo=%2Fpath%2Fto%2Frepo` selects a local repository explicitly.
Paths can contain percent-encoded spaces. Other actions, extra queries,
malformed escapes, remote paths, and non-Git directories are rejected. The
handler calls only the staged-diff viewer with explicit arguments and leaves
ordinary links unchanged. It also gives Magit links accurate hover hints.

Run the tests after changing this helper or updating agent-shell/Magit:

```sh
rtk proxy ./bin/test-agent-shell-magit-links.sh
```

Validation: 11 tests passed, including the rendered RET action and a real
temporary Git repository where HEAD, staged changes, and unstaged changes
remain unchanged after opening the viewer. The 15 file-viewer tests also
passed. Commit approval is separate from implementation approval.
