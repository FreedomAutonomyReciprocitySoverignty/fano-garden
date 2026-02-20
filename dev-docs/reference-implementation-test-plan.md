# Reference Implementation Test Plan (browser-v1)

## Acceptance Criteria
A build is acceptable when all required tests pass and no critical privacy/integrity violations are observed.

## Required Test Matrix

### T1. Deterministic Identity
- Given same canonical SVG and same public derivation root/template
- When identities are regenerated
- Then all `fano_point_id -> address` mappings are identical

Pass: exact mapping equality across runs.

### T2. NDJSON Append Continuity
- Given a valid commit chain
- When a new commit is appended
- Then `prev_hash` equals prior `self_hash`

Pass: zero continuity breaks.

### T3. Full Invariant Closure
- Given all 7 canonical line invariants marked pass
- When centroid is recomputed
- Then `stop_metric=1.0`, `closure_ratio=1.0`, `sabbath=true`

Pass: exact values and reason set to `all_invariants_closed`.

### T4. Partial/Invalid Closure
- Given missing or failed face invariants
- When centroid is recomputed
- Then `sabbath=false`

Pass: `stop_metric < 1.0` and reason includes incomplete face count.

### T5. WebRTC Convergence
- Given peers A and B with divergent valid branches
- When gossip/merge completes
- Then both peers select the same deterministic tip

Pass: tip `self_hash` equality on both peers.

### T6. Replay Determinism
- Given only NDJSON log input
- When runtime rebuild executes
- Then state (vertices, faces, centroid, tip) matches previously materialized state

Pass: full state hash equality.

### T7. Projection Consistency
- Given a selected commit id
- When rendering SVG and Three.js views
- Then incidence topology (vertex ids and line membership) is identical

Pass: topology checksum equality across both projections.

### T8. Tamper Quarantine
- Given one record with altered payload but unchanged hash/signature
- When validation runs
- Then record is marked `quarantined` and excluded from tip selection

Pass: quarantine event emitted and canonical chain unchanged.

### T9. Identity Privacy
- Given full log export and network traffic capture
- When scanned for secret material patterns
- Then no mnemonic/private key/xprv appears

Pass: zero secret matches.

## Failure-Mode Scenarios
- F1. Missing SVG points (not 7): parser hard-fails before identity derivation
- F2. Non-canonical line table: face evaluation rejects configuration
- F3. Clock skew in peers: ordering resolved by causal links then deterministic tie-break
- F4. Duplicate delivery: duplicate suppression by `self_hash`
- F5. Unsigned commit: auto-quarantine
- F6. Corrupt NDJSON line: skip line, emit validation error event

## Suggested Automation Layout
- Unit: ordering, mapping, hash/signature, invariant math
- Integration: append/replay, merge logic, WebRTC sync
- End-to-end: parse SVG -> generate commits -> sync peers -> render projections

## Exit Gates
- All T1-T9 passing
- No high severity failures in F1-F6
- Deterministic reproducibility verified on two separate runs
