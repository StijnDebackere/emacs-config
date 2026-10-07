#!/usr/bin/env bash
# Run quota/header tests and the mocked Claude reader without either provider.
set -euo pipefail
quota_test_cache="$(mktemp -d "${TMPDIR:-/tmp}/sdb-quota-tests.XXXXXX")"
trap 'rm -rf "$quota_test_cache"' EXIT
export XDG_CACHE_HOME="$quota_test_cache"
emacs_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
emacs_binary="${EMACS_BINARY:-emacs}"
if ! command -v "$emacs_binary" >/dev/null 2>&1; then
  emacs_binary=/Applications/Emacs.app/Contents/MacOS/Emacs
fi
load_path_args=()
for dir in "$emacs_dir"/straight/build/*; do
  if [ -d "$dir" ]; then load_path_args+=(-L "$dir"); fi
done
node --test "$emacs_dir/bin/test-claude-quota.mjs"
"$emacs_binary" -Q --batch --eval '(setq load-prefer-newer t)' \
  "${load_path_args[@]}" -L "$emacs_dir/local/agent-shell" -L "$emacs_dir/sdb" \
  -l "$emacs_dir/local/agent-shell/tests/agent-shell-tests.el" \
  -l agent-shell-quota-tests.el \
  --eval '(ert-run-tests-batch-and-exit "^sdb/agent-shell-quota-\\|^agent-shell--make-header-")'
