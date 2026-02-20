# Reference Implementation (POSIX/Haskell v1)

## Status
Normative for POSIX/Haskell target. Must remain wire-compatible with `browser-v1` commit semantics.

## Summary
This document defines a local-first POSIX/Haskell runtime that is deterministic, replayable, and cryptographically verifiable using the same commit model as browser-v1.

Core outcomes:
- append-only NDJSON commit log
- canonical hash-chain validation
- optional Merkle validation/signing message binding
- optional logical counter (`lc`) with deterministic merge/tip behavior
- optional signature verification hooks

## Compatibility Contract with Browser-v1

### Signing message
- If `commit.merkle.root` exists: signing message is `merkle.root`
- Else: signing message is `self_hash`

### Backward compatibility
- `lc` absent: commit is valid if all other checks pass
- `merkle` absent: commit is valid if all other checks pass
- If `lc` present: it must be integer `>= 0`

### Merge/tip ordering
- causal order via `parents`
- deterministic ordering/tie-break: `lc` (if present), then `t`, then lexical `id`
- deterministic tip preference for competing sealed commits: highest `lc`, then latest `t`, then lexical minimum `self_hash`

## Canonicalization and Hashing (Consensus Surface)

### Canonical JSON rules
1. UTF-8 encoding
2. object keys sorted lexicographically
3. arrays preserve original order (ordered semantics)
4. no insignificant whitespace
5. explicit `null` only when field exists and is null; missing fields remain missing
6. integer fields serialized as decimal integers

### Hash function
- SHA-256
- output as `0x`-prefixed lowercase hex

### Commit payload hashing
- `self_hash = sha256Hex(canonicalPayload(commit_without_self_hash_and_sig))`
- signature excluded from canonical payload

### Merkle hashing
Leaf sections (fixed order):
1. `identities_hash`
2. `vertex_hash`
3. `edges_hash`
4. `faces_hash`
5. `centroid_hash`
6. `meta_hash`

`meta_hash` fields:
- `id`, `t`, `type`, `parents`, `status`, `prev_hash`
- include `lc` only when present

Merkle pairing:
- hash adjacent leaves in order
- duplicate last leaf on odd count
- repeat until single root

## Data Model

### CommitEvent
Required:
- `id`, `t`, `type`, `parents`, `vertex`, `edges`, `faces`, `centroid`, `status`, `prev_hash`, `self_hash`, `sig`

Optional:
- `lc` (`>= 0`, monotonic per runtime instance)
- `merkle`
- `identities`

### Status values
- `pending`, `validated`, `sealed`, `quarantined`

### Integrity flags
- invalid structure -> `quarantined`
- hash/merkle/signature mismatch -> `quarantined`

## Haskell Module Layout

### `ULP.Types`
- `Commit`, `Merkle`, `VertexIdentity`, `EdgeState`, `FaceInvariant`, `CentroidState`
- strict type constructors for enums (`Status`, `CommitType`, `ChannelState`)

### `ULP.Canonical`
- `stableJson :: Value -> ByteString`
- `canonicalPayload :: Commit -> ByteString`
- `sha256Hex :: ByteString -> Text`

### `ULP.Merkle`
- `computeCommitMerkle :: CommitLike -> Merkle`
- `validateCommitMerkle :: Commit -> Bool`
- `getSigningMessage :: Commit -> Text`

### `ULP.NDJSON`
- `decodeStream :: Handle -> ConduitT () Commit m ()`
- `encodeLine :: Commit -> ByteString`
- ordered line-preserving read/write

### `ULP.Validate`
- `validateCommit :: ValidationOptions -> Maybe Commit -> Commit -> ValidationResult`
- checks:
  - structural validity
  - `prev_hash` continuity
  - `self_hash` recomputation
  - merkle validity (if present)
  - `lc` validity (if present)
  - signature verification hook (optional)
  - invariant hook (optional)

### `ULP.Merge`
- `mergeCommits :: [Commit] -> [Commit] -> [Commit]`
- `chooseTip :: [Commit] -> Maybe Commit`
- dedupe by `self_hash`
- deterministic sort and tip selection using compatibility contract above

### `ULP.Storage`
- append-only write to `log.ndjson`
- stream replay from `log.ndjson`
- optional derived `heads.json`

### `ULP.Runtime`
- `RuntimeOptions`
  - `clock :: IO Int64`
  - `counterStart :: Int`
  - `signer :: Maybe Signer`
  - `verifier :: Maybe Verifier`
  - `storageRoot :: FilePath`
- `initRuntime :: RuntimeOptions -> IO Runtime`
- `appendCommit :: Runtime -> CommitType -> IO Commit`
- `validateLog :: Runtime -> FilePath -> IO ValidationSummary`
- `mergeLog :: Runtime -> FilePath -> IO MergeSummary`

## POSIX File Layout

```text
<storageRoot>/
  log.ndjson
  heads.json           # optional derived state
  quarantine.ndjson    # optional rejected records
```

## CLI Surface (`ulp-runtime`)

- `init --root <dir>`
- `commit --root <dir> --type <type>`
- `validate --root <dir> [--log <path>]`
- `merge --root <dir> --from <remote.ndjson>`
- `tip --root <dir>`
- `replay --root <dir>`

## Signature Backend Strategy

v1 recommendation:
- verifier in-runtime (pure)
- signer pluggable:
  - delegated external signer (default for safety)
  - optional secp256k1/EIP-191 adapter later

No private key material may be written to NDJSON or runtime logs.

## Determinism Rules

- runtime must support injected deterministic `clock`
- runtime-local monotonic `lc`
- same input + same clock + same signer behavior => identical commit hashes
- merge/tip results must be deterministic regardless of input file ordering

## Test Matrix (POSIX/Haskell v1)

1. canonical serialization parity test (known vectors)
2. hash-chain continuity test
3. merkle recomputation test
4. signing message fallback test (`merkle.root` vs `self_hash`)
5. `lc` monotonicity test
6. deterministic replay test (same input => same state hash)
7. merge determinism test (shuffled input order => same tip)
8. quarantine behavior test for tampered commit

## Rollout Plan

Milestone 1 (minimal viable runtime):
- types, canonical hash, NDJSON read/write, validate, merge, tip

Milestone 2:
- merkle and signing hooks

Milestone 3:
- deterministic clock + replay harness + cross-runtime vectors

Milestone 4:
- optional networking adapters (gossip transport)

## Recommended Build Shape

Use **library-first + thin CLI wrapper**.

Reason:
- keeps deterministic core reusable for daemon, tests, and future adapters
- CLI remains stable operational entrypoint without coupling protocol logic to process I/O
