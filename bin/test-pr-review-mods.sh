#!/usr/bin/env bash
# Run the pr-review-mods ERT test suite in batch mode.
#
# This is never invoked from init.el -- it's a standalone script you run
# manually (or wire into a CI job) after touching sdb/pr-review-mods.el
# or after emacs-pr-review/magit/forge update.
set -euo pipefail

emacs_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
straight_build="$emacs_dir/straight/build"

load_path_args=()
if [ -d "$straight_build" ]; then
  while IFS= read -r -d '' dir; do
    load_path_args+=(-L "$dir")
  done < <(find "$straight_build" -mindepth 1 -maxdepth 1 -type d -print0)
fi

exec emacs -Q --batch \
  "${load_path_args[@]}" \
  -L "$emacs_dir/sdb" \
  -l pr-review-mods.el \
  -l pr-review-mods-tests.el \
  -f ert-run-tests-batch-and-exit
