#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="$ROOT/runtime/browser-v1"
DST="$ROOT/docs/assets/demo"

FILES=(
  runtime.js
  identity.js
  commit.js
  invariants.js
  merge.js
  projection.js
  svgParser.js
  canonicalize.js
  constants.js
  ndjson.js
  index.js
  webrtc.js
)

status=0

for f in "${FILES[@]}"; do
  src="$SRC/$f"
  dst="$DST/$f"
  if [[ ! -f "$dst" ]]; then
    echo "MISSING: docs copy for $f"
    status=1
    continue
  fi
  if ! cmp -s "$src" "$dst"; then
    echo "DRIFT: $f differs between runtime/browser-v1 and docs/assets/demo"
    status=1
  fi
done

if [[ ! -f "$DST/app.js" ]]; then
  echo "MISSING: docs/assets/demo/app.js"
  status=1
fi

if [[ ! -f "$ROOT/docs/assets/data/fano-garden.svg" ]]; then
  echo "MISSING: docs/assets/data/fano-garden.svg"
  status=1
fi

if [[ "$status" -eq 0 ]]; then
  echo "OK: docs demo snapshot is in sync for copied runtime modules."
fi

exit "$status"
