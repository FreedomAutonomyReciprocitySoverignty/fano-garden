# NDJSON Schemas (Frozen for browser-v1)

## Purpose
Frozen schema definitions and examples for NDJSON event interchange.

## Canonicalization for Hash/Signature
Before computing `self_hash` or verifying `sig`:
1. UTF-8 encode JSON with sorted object keys
2. Use no insignificant whitespace
3. Exclude transport envelope fields
4. Hash algorithm: `sha256`

## Record: CommitEvent
Required fields:
- `id` string
- `t` integer (unix ms)
- `type` enum: `vertex_init|edge_update|face_eval|commit|projection|sync`
- `parents` array of strings
- `vertex` object or null
- `edges` array
- `faces` array (`evidence` entries may contain structured JSON objects)
- `centroid` object
- `status` enum: `pending|validated|sealed|quarantined`
- `prev_hash` string or null
- `self_hash` string
- `sig` string

Optional fields:
- `lc` integer logical counter (`>= 0`), monotonic within a runtime instance
- `merkle` object (when present)
- `identities` array (when present)

Example:
```json
{"id":"cmt-1-1760918400000","t":1760918400000,"lc":1,"type":"commit","parents":[],"vertex":{"vertex_id":"v1","path":"m/44'/60'/0'/0/0","address":"0x1111111111111111111111111111111111111111","pubkey":"0x02abc...","fano_point_id":1},"edges":[{"edge_id":"e12","from":"v1","to":"v2","channel_state":"open","last_seq":4}],"faces":[{"face_id":"L1","vertices":["v1","v2","v4"],"invariant_name":"fano_line_closure","status":"pass","evidence":{"rule":"L1","seq":4}}],"centroid":{"stop_metric":0.1428571429,"closure_ratio":0.1428571429,"sabbath":false,"reason":"incomplete_faces:1/7"},"status":"validated","prev_hash":null,"self_hash":"0x5f4d...","sig":"0x9abc..."}
```

## Record: ProjectionEvent
Required fields:
- `id`, `t`, `type="projection"`, `source_commit`, `svg_overlay_id`, `scene_transform`, `render_hints`, `prev_hash`, `self_hash`, `sig`

Example:
```json
{"id":"prj-0001","t":1760918401000,"type":"projection","source_commit":"cmt-0001","svg_overlay_id":"overlay-main","scene_transform":{"position":[0,0,0],"rotation":[0,0,0],"scale":[1,1,1]},"render_hints":{"vertex_color":"#c9b99a","edge_color":"#3a3530","opacity":0.9},"prev_hash":"0x5f4d...","self_hash":"0x8e11...","sig":"0xbeef..."}
```

## Record: SyncEvent
Required fields:
- `id`, `t`, `type="sync"`, `peer_id`, `known_tip`, `advertised_hashes`, `accepted`, `rejected`, `prev_hash`, `self_hash`, `sig`

Example:
```json
{"id":"syn-0001","t":1760918402000,"type":"sync","peer_id":"peer-A","known_tip":"0x8e11...","advertised_hashes":["0x5f4d...","0x8e11..."],"accepted":["0x8e11..."],"rejected":[],"prev_hash":"0x8e11...","self_hash":"0x7dd1...","sig":"0xd00d..."}
```

## Validation Rules
- Unknown top-level fields: allowed but ignored for validity
- Missing required fields: invalid -> `quarantined`
- If `lc` is present, it must be an integer `>= 0`
- Numeric ranges:
  - `stop_metric` in `[0,1]`
  - `closure_ratio` in `[0,1]`
- `sabbath=true` requires both metrics equal `1.0`
- `self_hash` mismatch invalidates the record

## Interop Notes
- Producers must emit one JSON object per line
- Consumers must tolerate extra fields and preserve line ordering
- Replay state must be reconstructable from NDJSON alone
