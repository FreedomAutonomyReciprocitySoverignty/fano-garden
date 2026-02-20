export const SPEC_VERSION = "browser-v1";

export const HD_PATH_TEMPLATE = "m/44'/60'/0'/0/{i}";

// Canonical Fano line table (point ids 1..7)
export const FANO_LINE_TABLE = [
  { line_id: "L1", points: [1, 2, 4] },
  { line_id: "L2", points: [1, 3, 5] },
  { line_id: "L3", points: [1, 6, 7] },
  { line_id: "L4", points: [2, 3, 6] },
  { line_id: "L5", points: [2, 5, 7] },
  { line_id: "L6", points: [3, 4, 6] },
  { line_id: "L7", points: [4, 5, 7] },
];

export const REQUIRED_COMMIT_FIELDS = [
  "id",
  "t",
  "type",
  "parents",
  "vertex",
  "edges",
  "faces",
  "centroid",
  "status",
  "prev_hash",
  "self_hash",
  "sig",
];

export const COMMIT_TYPES = new Set([
  "vertex_init",
  "edge_update",
  "face_eval",
  "commit",
  "projection",
  "sync",
]);

export const COMMIT_STATUSES = new Set([
  "pending",
  "validated",
  "sealed",
  "quarantined",
]);

export const CHANNEL_STATES = new Set([
  "down",
  "opening",
  "open",
  "closing",
  "closed",
]);
