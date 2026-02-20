# Reference Glossary

## Purpose
This glossary maps symbolic language from the chat history into implementation terms used by the browser-first v1 spec.

## Symbolic -> Technical
- `Stop` -> closure condition where no additional face invariants are required for current commit
- `Sabbath` -> terminal state for a cycle (`sabbath=true`, `stop_metric=1.0`)
- `Yes/No` -> extension admitted / extension refused in commit evolution
- `Boundary operator` -> deterministic closure rule over face invariants
- `Genesis gate` -> canonical origin commit or central reference point in projection
- `Fano plane` -> 7-point/7-line incidence model used for identity and invariants
- `Line` -> 3-point incidence triple used as face/invariant unit
- `Vertex` -> identity-bearing runtime node bound to HD derivation index
- `Edge` -> communication state between two vertices (WebRTC channel abstraction)
- `Face` -> invariant evaluation unit over 3 vertices
- `Centroid` -> aggregate closure evaluator producing `stop_metric`
- `Trie / sealed gate` -> append-only commit history with irreversible prefixes
- `Blockchain of NDJSON` -> hash-linked append-only NDJSON commit chain
- `Path of words` -> ordered event sequence in NDJSON
- `Narrative metric` -> closure progression measured by invariant completion ratio
- `Metatron/Lambda cube` -> meta-layer/type-system intuition (non-normative rationale)
- `Merkaba/instantiation point` -> concrete runtime commit event
- `Unknown unknowns` -> states not yet represented in valid commit history
- `Rumsfeld quadrants` -> optional categorization overlay; not part of core validity rules

## Runtime Vocabulary (Preferred)
Use these terms in protocol and code docs:
- `commit event`
- `face invariant`
- `closure ratio`
- `stop metric`
- `sealed commit`
- `quarantined commit`
- `tip selection`
- `projection state`

## Out of Scope for Normative v1
These terms may appear in rationale but must not define protocol behavior:
- theological framing
- symbolic persona naming
- metaphysical claims
- non-deterministic interpretation layers
