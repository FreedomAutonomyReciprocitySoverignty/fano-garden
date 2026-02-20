# fano-garden

fano-garden is a browser-first runtime framework for deterministic Fano-plane execution with append-only NDJSON commits, hash-link integrity, and projection into SVG/3D views.

## Consumer Site

GitHub Pages site (after deployment):

- `https://freedomautonomyreciprocitysoverignty.github.io/fano-garden/`
- Quickstart: `https://freedomautonomyreciprocitysoverignty.github.io/fano-garden/quickstart/`
- Live demo: `https://freedomautonomyreciprocitysoverignty.github.io/fano-garden/demo/`
- Reference docs: `https://freedomautonomyreciprocitysoverignty.github.io/fano-garden/reference/`

## Local Preview of docs/

```bash
cd docs
python3 -m http.server 8080
# open http://localhost:8080
```

## Runtime Dev (browser-v1)

```bash
node runtime/browser-v1/dev-server.mjs
# open http://localhost:4173/runtime/browser-v1/demo/index.html
```

## Runtime Compatibility Matrix (Consumer View)

This project currently maintains browser-v1 as the normative reference and a wire-compatible POSIX/Haskell runtime baseline.

| Capability | Browser v1 | POSIX/Haskell | Status |
|---|---|---|---|
| NDJSON commit envelope (`id`, `type`, `status`, `self_hash`, `prev_hash`, `merkle`) | Yes | Yes | Parity |
| Enum wire values (channel/commit/face/status) | Yes | Yes | Parity |
| Signing message selection (`merkle.root` fallback `self_hash`) | Yes | Yes | Parity |
| Optional fields (`lc`, `merkle`) | Yes | Yes | Parity |
| Deterministic merge/tip ordering (`lc` -> `t` -> lexical tie-break) | Yes | Yes | Parity |
| Canonical payload + Merkle meta key alignment | Yes | Yes | Parity |
| CLI workflow (`init`, `commit`, `validate`, `merge`, `tip`, `replay`) | N/A (library/demo runtime) | Yes | Expected difference |
| Browser-native projection UI (SVG/Three demo page) | Yes | Not primary target | Expected difference |

### Known Deltas

- Browser v1 is the normative behavior source for protocol interpretation and reference docs.
- POSIX/Haskell is currently a working baseline focused on wire compatibility and CLI operation.
- UI/demo experience is browser-first; POSIX/Haskell emphasizes reproducible headless workflows.
