#!/usr/bin/env bash
# Test staged-diff links without loading init.el or launching an agent.
set -euo pipefail
emacs_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
emacs_binary="${EMACS_BINARY:-emacs}"
if ! command -v "$emacs_binary" >/dev/null 2>&1; then
  emacs_binary=/Applications/Emacs.app/Contents/MacOS/Emacs
fi
load_path_args=()
for dir in "$emacs_dir"/straight/build/*; do
  if [ -d "$dir" ]; then load_path_args+=(-L "$dir"); fi
done
"$emacs_binary" -Q --batch --eval '(setq load-prefer-newer t)' \
  "${load_path_args[@]}" -L "$emacs_dir/local/agent-shell" -L "$emacs_dir/sdb" \
  -l agent-shell-magit-links-tests.el \
  --eval '(ert-run-tests-batch-and-exit "^sdb/agent-shell-magit-links-")'
