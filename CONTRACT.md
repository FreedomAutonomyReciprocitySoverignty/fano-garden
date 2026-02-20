# CONTRACT (v1)

This repository has a frozen v1 protocol contract for browser and POSIX/Haskell runtimes.

## Frozen Sources of Truth
- `dev-docs/reference-implementation-browser-v1.md`
- `dev-docs/reference-implementation-schemas.ndjson.md`
- `dev-docs/reference-implementation-test-plan.md`
- `dev-docs/reference-implementation-posix-haskell-v1.md`

## Contract Rules
1. NDJSON wire shape and canonicalization are frozen for v1.
2. Commit validity semantics are frozen:
- hash-chain continuity (`prev_hash` / `self_hash`)
- merkle integrity (when present)
- `lc` semantics (optional, non-negative)
- deterministic merge/tip ordering (`lc` -> `t` -> lexical tie-break)
3. Quarantine policy is frozen: invalid commits are quarantined and valid replay continues from last valid state.
4. Signing message selection is frozen:
- `merkle.root` when present, else `self_hash`.
5. `edges`/`faces` retain ordered-array semantics in v1.

## Compatibility Promise
- `lc` is optional in v1.
- `merkle` is optional in v1.
- Pre-`lc` and pre-`merkle` records remain valid if other checks pass.

## Change Policy
Changes that alter wire format, canonicalization, hashing, Merkle behavior, merge/tip ordering, or replay/quarantine semantics are **breaking** and require:
1. version bump to v2,
2. explicit migration notes,
3. new vectors and test matrix updates.

## Required Interop Gates
A PR is merge-eligible only if all required suites pass:
- browser runtime tests
- POSIX/Haskell smoke tests
- cross-runtime vectors (both directions)
- signature suite (golden + negative)
- replay suite (golden + negative)

See `CI.md` for exact commands and workflow behavior.
