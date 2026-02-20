# Reference Implementation (Browser-First v1)

## Status
Normative specification. This document is the source of truth for v1 behavior.

## Scope
This specification defines a browser-first runtime for a Fano-plane-based SVG virtual machine with:
- Local-first execution
- Append-only NDJSON event log
- Hash-linked commits for tamper evidence
- WebRTC DataChannel peer synchronization
- Deterministic HDNode-derived public identity binding to Fano vertices
- Dual projection targets: SVG overlays and Three.js scene transforms

POSIX/Haskell references in prior conversation are non-normative for v1 and are documented only for compatibility notes.

## Canonical Architecture
1. Identity Layer
- Input: deterministic root public descriptor and derivation template
- Output: `VertexIdentity` records for the 7 Fano points
- Rule: runtime artifacts may contain public keys and addresses only

2. Geometry Layer
- Input: canonical Fano SVG
- Output: point coordinates, line incidences, and normalized ordering
- Rule: parser extracts exactly 7 points and 7 line triples

3. Runtime Layer
- Maintains vertex, edge, face, and centroid state
- Computes closure and `stop_metric` from face invariants
- Emits immutable commit events

4. Persistence Layer
- NDJSON append-only log
- Each commit references previous hash: `prev_hash`
- Each commit includes deterministic digest: `self_hash`

5. Network Layer
- Local-first operation with eventual peer convergence
- WebRTC DataChannel gossip for commit exchange
- Merge by causal order + conflict policy in this spec

6. Projection Layer
- NDJSON to SVG overlay state
- NDJSON to Three.js transform state
- Same commit must produce topologically equivalent projections

## Normative Types

### VertexIdentity
- `vertex_id`: string, stable id for point
- `path`: string, derivation path (public mapping only)
- `address`: string, checksummed EVM address
- `pubkey`: string, compressed or uncompressed public key
- `fano_point_id`: integer in `[1..7]`

Constraints:
- one-to-one mapping between `fano_point_id` and `address`
- no mnemonic, seed phrase, xprv, or private key fields allowed

### EdgeState
- `edge_id`: string
- `from`: `vertex_id`
- `to`: `vertex_id`
- `channel_state`: enum `down|opening|open|closing|closed`
- `last_seq`: non-negative integer

### FaceInvariant
- `face_id`: string
- `vertices`: array of 3 `vertex_id`
- `invariant_name`: string
- `status`: enum `pass|fail|unknown`
- `evidence`: object (implementation-defined, deterministic fields only)

### CentroidState
- `stop_metric`: number in `[0,1]`
- `closure_ratio`: number in `[0,1]`
- `sabbath`: boolean
- `reason`: string

### CommitEvent (NDJSON)
- `id`: string (content-addressable id or UUID)
- `t`: integer unix ms timestamp
- `type`: enum `vertex_init|edge_update|face_eval|commit|projection|sync`
- `parents`: array of prior commit ids (possibly empty for genesis)
- `vertex`: object or null
- `edges`: array of `EdgeState`
- `faces`: array of `FaceInvariant`
- `centroid`: `CentroidState`
- `status`: enum `pending|validated|sealed|quarantined`
- `prev_hash`: hex string or `null` for genesis
- `self_hash`: hex string
- `sig`: string (signature over canonicalized payload)

### ProjectionEvent
- `source_commit`: commit id
- `svg_overlay_id`: string
- `scene_transform`: object with `position`, `rotation`, `scale`
- `render_hints`: object (color, opacity, z-index, animation flags)

## Deterministic Rules

### R1. Vertex Ordering from SVG
Order points by:
1. canonical point label if present (`1..7`), else
2. stable sort by `(y asc, x asc)`, else
3. tie-break by original element order in parsed SVG DOM

### R2. HD Path Mapping
Fixed template for v1:
- `m/44'/60'/0'/0/{i}` where `i = fano_point_id - 1`

The mapping is frozen for interoperability. Implementations may support alternative templates only behind explicit version bumps.

### R3. Fano Line Table
Canonical line triples by point id:
- `L1 = {1,2,4}`
- `L2 = {1,3,5}`
- `L3 = {1,6,7}`
- `L4 = {2,3,6}`
- `L5 = {2,5,7}`
- `L6 = {3,4,6}`
- `L7 = {4,5,7}`

Each runtime face evaluation must reference this table or a strict renaming equivalent.

### R4. Stop Metric
Let:
- `F_total = 7`
- `F_pass = count(face.status == pass)`

Then:
- `closure_ratio = F_pass / F_total`
- `stop_metric = closure_ratio`
- `sabbath = (stop_metric == 1.0)`

Reason strings:
- `all_invariants_closed` when `sabbath=true`
- otherwise `incomplete_faces:{F_pass}/7`

### R5. Commit Validity
A commit is valid iff all are true:
1. Required fields exist with correct type
2. `sig` is present and verifies against canonical payload
3. `prev_hash` equals prior accepted commit `self_hash` (or null for genesis)
4. `self_hash` matches canonical hash of commit payload
5. face and centroid values are consistent with deterministic rules

Invalid commits must be marked `quarantined` and excluded from tip selection.

### R6. Merge Semantics
- Causal ordering: topological order by parent links, then timestamp, then lexical `id`
- Duplicate suppression: identical `self_hash` kept once
- Conflict policy: `sealed` supersedes `pending` only when hash-valid and invariant-complete
- If two valid sealed commits compete at same height, choose deterministic winner by lexical minimum `self_hash`; retain loser as alternate branch metadata

## Event Flow (Normative)
1. Parse canonical SVG
2. Derive identities for point set
3. Initialize edge and face state
4. Evaluate invariants and centroid
5. Emit `commit` event with hash/signature
6. Append to NDJSON
7. Broadcast via WebRTC
8. Merge remote commits and recompute tip
9. Update SVG and Three.js projections from selected tip

## Security and Privacy Requirements
- Never serialize mnemonic/private key/xprv in NDJSON, WebRTC payloads, logs, or UI state
- Treat `sig` as public proof only
- Reject unsigned commits by default
- Quarantine tampered or replay-invalid records

## Non-Normative Compatibility Notes
- Existing files in `dev-docs/artifacts` are implementation precedents, not protocol truth
- Prior POSIX/Haskell design maps conceptually to this model but does not define v1 behavior

## Versioning
- Spec identifier: `browser-v1`
- Backward-incompatible changes require `browser-v2`
