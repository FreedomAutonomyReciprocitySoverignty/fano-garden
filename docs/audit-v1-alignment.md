# v1 Alignment Audit

## Scope Reviewed
- `dev-docs/reference-implementation-browser-v1.md`
- `dev-docs/reference-implementation-schemas.ndjson.md`
- `dev-docs/reference-implementation-glossary.md`
- `dev-docs/reference-implementation-test-plan.md`
- `dev-docs/2026-02-20-Branch_·_Yes_No_Stop_Communication.md`
- `runtime/browser-v1/**`
- `runtime/posix-haskell/**`

## Alignment Table

| Contract Point | Doc Source | Code Source | Test Enforcement | Status |
|---|---|---|---|---|
| Signing message: `merkle.root` else `self_hash` | `dev-docs/reference-implementation-browser-v1.md` (Compatibility + R5), `CONTRACT.md` | `runtime/browser-v1/commit.js#getSigningMessage`, `runtime/posix-haskell/src/ULP/Merkle.hs#getSigningMessage` | `runtime/posix-haskell/test/sig-suite.sh`, `runtime/posix-haskell/test/vector-check.sh`, `runtime/posix-haskell/test/reverse-vector-check.sh` | Aligned |
| Merkle leaf sections + fixed `leaf_order` | `dev-docs/reference-implementation-browser-v1.md` (Public Types + rules), `dev-docs/reference-implementation-posix-haskell-v1.md` | `runtime/browser-v1/commit.js#computeCommitMerkle`, `runtime/posix-haskell/src/ULP/Merkle.hs#computeCommitMerkle` | `runtime/browser-v1/tests/merkle.mjs`, `runtime/posix-haskell/test/vector-check.sh` | Aligned |
| Merkle pair hash strips `0x` prefixes before hashing `left|right` | `dev-docs/reference-implementation-posix-haskell-v1.md`, `CONTRACT.md` | `runtime/browser-v1/commit.js#hashPair`, `runtime/posix-haskell/src/ULP/Merkle.hs#pairHash` | `runtime/posix-haskell/test/vector-check.sh` | Aligned |
| Backward compatibility: `lc` optional, only bound into Merkle meta when present | `dev-docs/reference-implementation-browser-v1.md`, `dev-docs/reference-implementation-schemas.ndjson.md` | `runtime/browser-v1/commit.js#computeCommitMerkle`, `runtime/posix-haskell/src/ULP/Merkle.hs#computeCommitMerkle` | `runtime/browser-v1/tests/logical-clock.mjs`, `runtime/posix-haskell/test/replay-suite.sh` | Aligned |
| `merkle` optional; validation falls back to `self_hash` signing message | `dev-docs/reference-implementation-browser-v1.md`, `dev-docs/reference-implementation-posix-haskell-v1.md` | `runtime/browser-v1/commit.js#getSigningMessage`, `runtime/posix-haskell/src/ULP/Merkle.hs#getSigningMessage` | `runtime/posix-haskell/test/sig-suite.sh` | Aligned |
| Deterministic merge/tip ordering: causal parents then `lc`, `t`, lexical tie-break | `dev-docs/reference-implementation-browser-v1.md` (R6), `CONTRACT.md` | `runtime/browser-v1/merge.js` (parent ranks + `lc/t/id`), `runtime/posix-haskell/src/ULP/Merge.hs` (parentRanks + `lc/t/cid`) | `runtime/browser-v1/tests/logical-clock.mjs`, `runtime/posix-haskell/test/replay-suite.sh` | Aligned (fixed in this pass) |
| Ordered array semantics (`edges`/`faces`) | `dev-docs/reference-implementation-browser-v1.md`, `runtime/browser-v1/README.md`, `CONTRACT.md` | Canonical hashing over ordered arrays in both runtimes | `runtime/browser-v1/tests/merkle-ordering.mjs` | Aligned |
| Quarantine policy A: invalid commits quarantined; replay continues from last valid tip | `dev-docs/reference-implementation-test-plan.md`, `CONTRACT.md` | `runtime/posix-haskell/src/ULP/Runtime.hs#validateSequence`, replay scripts using valid-prefix fingerprint | `runtime/posix-haskell/test/replay-suite.sh` | Aligned |
| Interop gates list and expected command surface | `CI.md`, `CONTRACT.md`, `runtime/posix-haskell/README.md` | `.github/workflows/interop.yml`, test scripts in `runtime/posix-haskell/test/` | `runtime/posix-haskell/test/interop-suite.sh` | Aligned |
| Structured evidence payloads (`evidence` as JSON object/value) | `dev-docs/reference-implementation-browser-v1.md` | `runtime/browser-v1` accepts object evidence, `runtime/posix-haskell/src/ULP/Types.hs` (`evidence :: Maybe Value`) | `runtime/posix-haskell/test/vector-check.sh` | Aligned |

## Mismatches Found and Resolved
1. **Merge causal ordering mismatch**: code previously sorted by `lc/t/id` only.
- Resolved by implementing parent-rank causal priority in both runtimes (`runtime/browser-v1/merge.js`, `runtime/posix-haskell/src/ULP/Merge.hs`).

## Intentional Out of Scope
- No v2 surface changes.
- No external dependency additions.
- No projection checksum gate added yet (documented but not enforced by CI workflow in this pass).
