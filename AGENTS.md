# AGENTS

## v1 Mission
Work in parallel on this repo without breaking the frozen v1 consensus surface.

## Normative Contract
Authoritative docs:
- `dev-docs/reference-implementation-browser-v1.md`
- `dev-docs/reference-implementation-schemas.ndjson.md`
- `dev-docs/reference-implementation-test-plan.md`
- `dev-docs/reference-implementation-posix-haskell-v1.md`
- `CONTRACT.md`

Non-negotiable invariants:
1. Signing message: `commit.merkle.root` if present, else `commit.self_hash`.
2. Merkle v1 sections + fixed leaf order:
- `identities_hash`, `vertex_hash`, `edges_hash`, `faces_hash`, `centroid_hash`, `meta_hash`.
3. Merkle pair hashing strips `0x` prefixes before hashing `left|right`.
4. Backward compatibility:
- `lc` optional; only binds to merkle meta when present.
- `merkle` optional; validation still allowed.
5. Merge/tip ordering:
- commits must respect parent causality.
- if two commits are causally independent: higher `lc`, then higher `t`, then lexical minimum `self_hash`.
6. `edges` and `faces` are ordered arrays in v1.
7. Quarantine policy A:
- invalid commits are quarantined
- replay/fingerprint proceeds from last valid tip.
8. Canonicalization:
- stable JSON ordering is required before hashing.
- hashing algorithm is SHA-256.
- hash inputs must match canonicalized JSON byte-for-byte across runtimes.
9. Test suites are normative:
- browser and POSIX/Haskell interop suites define executable consensus.
- if behavior and documentation diverge, tests are the authority.

## Merge Gates (Required)
From repo root:

```bash
# Browser runtime
node runtime/browser-v1/tests/smoke.mjs
node runtime/browser-v1/tests/signature-hooks.mjs
node runtime/browser-v1/tests/merkle.mjs
node runtime/browser-v1/tests/merkle-ordering.mjs
node runtime/browser-v1/tests/logical-clock.mjs

# POSIX/Haskell runtime
cd runtime/posix-haskell
cabal build exe:ulp-runtime
./test/cli-smoke.sh
./test/vector-check.sh
./test/reverse-vector-check.sh

# Interop suites
export ULP_TEST_PRIVKEY=0x0123456789012345678901234567890123456789012345678901234567890123
./test/sig-suite.sh
./test/replay-suite.sh
./test/interop-suite.sh
```

CI must enforce all gates before merge. No manual override without a documented v2 plan.

## Worktree Swarm Workflow
Branch naming:
- `agent/<name>/<topic>`

PR expectations:
- small, contract-preserving, single responsibility
- include at least one golden and one negative test for behavior changes
- avoid broad refactors in protocol-critical modules

Area-specific minimum tests:
- `area:runtime-js`: run all browser tests + `vector-check.sh`
- `area:runtime-hs`: `cabal build` + `cli-smoke.sh` + `reverse-vector-check.sh`
- `area:interop`: run `interop-suite.sh`
- `area:projection-*`: browser tests + include projection fixture/check details in PR

## Allowed vs Requires v2
Allowed in v1:
- docs clarifications
- test additions
- implementation bug fixes that preserve contract semantics
- internal refactors with identical wire/hash/merkle/ordering outcomes

Requires v2 plan + migration:
- wire key/schema changes
- canonicalization/hash changes
- merkle structure/leaf order changes
- signing message rule changes
- merge/tip ordering rule changes
- changing ordered-array semantics for `edges`/`faces`

## Vector/Test Locations
- Browser tests: `runtime/browser-v1/tests/`
- Haskell tests: `runtime/posix-haskell/test/`
- Golden fixtures: `runtime/posix-haskell/test/vectors/`

## Security and Privacy Rules
- Never commit mnemonic/private keys/xprv.
- Runtime logs and NDJSON must not include secret key material.
- Test-only deterministic signer is allowed via env var hooks:
- `ULP_TEST_PRIVKEY`
- `ULP_TEST_SIGNER_JS`
- `ULP_TEST_VERIFIER_JS`
- `ULP_TEST_EXPECTED_SIGNER` (or legacy `ULP_TEST_EXPECTED_ADDRESS` where used)

## Definition of Done (PR)
- [ ] Contract invariants unchanged (or v2 plan included)
- [ ] Required tests pass for changed area
- [ ] New behavior has golden + negative coverage
- [ ] Docs updated when semantics/commands changed
- [ ] Deterministic replay produces identical fingerprint across JS and Haskell
- [ ] No secrets added to repo/log artifacts

## Explicitly Out of Scope for v1
- Network protocol design
- P2P transport semantics
- API server abstraction
- Performance optimizations that change observable ordering
