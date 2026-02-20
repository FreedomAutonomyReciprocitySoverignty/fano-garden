# Browser-First v1 Runtime Scaffold

This folder contains a minimal reference scaffold aligned to:
- `dev-docs/reference-implementation-browser-v1.md`
- `dev-docs/reference-implementation-schemas.ndjson.md`

## Modules
- `svgParser.js`: extract canonical 7-point Fano coordinates
- `identity.js`: map points to HD-path-based public identities
- `invariants.js`: evaluate canonical line closure + centroid metric
- `commit.js`: build/validate hash-linked commits
- `merge.js`: deterministic dedupe + tip selection
- `projection.js`: NDJSON state to SVG/Three projection state
- `webrtc.js`: tiny DataChannel gossip adapter
- `runtime.js`: orchestration class for init/state/commit/merge

## Demo
Start the local server:

```bash
node runtime/browser-v1/dev-server.mjs
```

Then open:

- `http://localhost:4173/runtime/browser-v1/demo/index.html`

(Or just `http://localhost:4173/` which routes to the demo.)

The demo:
1. loads `dev-docs/artifacts/fano-garden.svg`
2. initializes runtime with ethers HD derivation
3. opens canonical Fano edges
4. signs and verifies one commit with ethers
5. prints state and projection summary

## Optional ethers integration
`identity.js` includes `ethersPublicDeriver(ethersApi, phrase)` for deterministic address/public-key derivation when ethers v6 is available.
`commit.js` includes:
- `createEthersSignatureBuilder(ethersApi, signerWallet)`
- `createEthersSignatureVerifier(ethersApi, resolveExpectedAddress?)`

## Smoke test (Node 18+)
Run:

```bash
node runtime/browser-v1/tests/smoke.mjs
```

Merkle integrity test:

```bash
node runtime/browser-v1/tests/merkle.mjs
```

Merkle ordering semantics test (v1 ordered arrays):

```bash
node runtime/browser-v1/tests/merkle-ordering.mjs
```

v1 semantics note: `edges` and `faces` are treated as ordered arrays in hashing; reordering without re-signing is invalid.

Deterministic logical clock test:

```bash
node runtime/browser-v1/tests/logical-clock.mjs
```
