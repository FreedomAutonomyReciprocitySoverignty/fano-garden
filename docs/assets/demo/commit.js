import {
  COMMIT_STATUSES,
  COMMIT_TYPES,
  REQUIRED_COMMIT_FIELDS,
  CHANNEL_STATES,
} from "./constants.js";
import { canonicalPayload, sha256Hex, stableStringify } from "./canonicalize.js";

function validateEdge(edge) {
  return (
    edge &&
    typeof edge.edge_id === "string" &&
    typeof edge.from === "string" &&
    typeof edge.to === "string" &&
    CHANNEL_STATES.has(edge.channel_state) &&
    Number.isInteger(edge.last_seq) &&
    edge.last_seq >= 0
  );
}

function validateFace(face) {
  return (
    face &&
    typeof face.face_id === "string" &&
    Array.isArray(face.vertices) &&
    face.vertices.length === 3 &&
    typeof face.invariant_name === "string" &&
    ["pass", "fail", "unknown"].includes(face.status) &&
    face.evidence &&
    typeof face.evidence === "object"
  );
}

function validateCentroid(centroid) {
  if (!centroid || typeof centroid !== "object") return false;
  const inRange = (n) => typeof n === "number" && n >= 0 && n <= 1;
  if (!inRange(centroid.stop_metric) || !inRange(centroid.closure_ratio)) return false;
  if (typeof centroid.sabbath !== "boolean") return false;
  if (typeof centroid.reason !== "string") return false;
  if (centroid.sabbath && (centroid.stop_metric !== 1 || centroid.closure_ratio !== 1)) return false;
  return true;
}

function validateLogicalCounter(lc) {
  return Number.isInteger(lc) && lc >= 0;
}

async function hashObject(value) {
  return sha256Hex(stableStringify(value));
}

function strip0x(hex) {
  return String(hex || "").replace(/^0x/, "");
}

async function hashPair(leftHex, rightHex) {
  return sha256Hex(`${strip0x(leftHex)}|${strip0x(rightHex)}`);
}

async function merkleRoot(leaves) {
  if (leaves.length === 0) return sha256Hex("empty");
  let level = [...leaves];
  while (level.length > 1) {
    const next = [];
    for (let i = 0; i < level.length; i += 2) {
      const left = level[i];
      const right = i + 1 < level.length ? level[i + 1] : level[i];
      next.push(await hashPair(left, right));
    }
    level = next;
  }
  return level[0];
}

export async function computeCommitMerkle(commitLike) {
  const meta = {
    id: commitLike.id,
    t: commitLike.t,
    type: commitLike.type,
    parents: commitLike.parents || [],
    status: commitLike.status,
    prev_hash: commitLike.prev_hash ?? null,
  };
  // Preserve backward compatibility: only bind lc when present.
  if (commitLike.lc !== undefined) {
    meta.lc = commitLike.lc;
  }

  const sections = {
    identities_hash: await hashObject(commitLike.identities || []),
    vertex_hash: await hashObject(commitLike.vertex || null),
    edges_hash: await hashObject(commitLike.edges || []),
    faces_hash: await hashObject(commitLike.faces || []),
    centroid_hash: await hashObject(commitLike.centroid || null),
    meta_hash: await hashObject(meta),
  };

  const leaf_order = [
    "identities_hash",
    "vertex_hash",
    "edges_hash",
    "faces_hash",
    "centroid_hash",
    "meta_hash",
  ];
  const leaves = leaf_order.map((k) => sections[k]);
  const root = await merkleRoot(leaves);

  return {
    version: "v1",
    sections,
    leaf_order,
    root,
  };
}

export function getSigningMessage(commit) {
  if (commit && commit.merkle && typeof commit.merkle.root === "string") {
    return commit.merkle.root;
  }
  return commit.self_hash;
}

async function validateMerkle(commit) {
  if (!("merkle" in commit)) return true;
  if (!commit.merkle || typeof commit.merkle !== "object") return false;
  if (typeof commit.merkle.root !== "string") return false;
  if (!commit.merkle.sections || typeof commit.merkle.sections !== "object") return false;

  const expected = await computeCommitMerkle(commit);
  const actual = commit.merkle;
  if (actual.version !== expected.version) return false;
  if (actual.root !== expected.root) return false;

  for (const key of expected.leaf_order) {
    if (actual.sections[key] !== expected.sections[key]) return false;
  }
  return true;
}

export async function buildCommit(input, previous, options = {}) {
  const base = {
    id: input.id,
    t: input.t,
    lc: input.lc,
    type: input.type,
    parents: input.parents || [],
    identities: input.identities || [],
    vertex: input.vertex || null,
    edges: input.edges || [],
    faces: input.faces || [],
    centroid: input.centroid,
    status: input.status || "pending",
    prev_hash: previous ? previous.self_hash : null,
    merkle: null,
    self_hash: "",
    sig: input.sig || "unsigned-local",
  };

  if (options.withMerkle !== false) {
    base.merkle = await computeCommitMerkle(base);
  }

  const payload = canonicalPayload(base);
  base.self_hash = await sha256Hex(payload);
  if (typeof options.signatureBuilder === "function") {
    base.sig = await options.signatureBuilder(base, getSigningMessage(base));
  }
  return base;
}

export async function validateCommit(commit, previous, options = {}) {
  const errors = [];

  for (const field of REQUIRED_COMMIT_FIELDS) {
    if (!(field in commit)) errors.push(`missing:${field}`);
  }

  if (!COMMIT_TYPES.has(commit.type)) errors.push("invalid:type");
  if (!COMMIT_STATUSES.has(commit.status)) errors.push("invalid:status");
  if ("lc" in commit && !validateLogicalCounter(commit.lc)) errors.push("invalid:lc");

  if (!Array.isArray(commit.parents)) errors.push("invalid:parents");
  if (!Array.isArray(commit.edges) || commit.edges.some((e) => !validateEdge(e))) errors.push("invalid:edges");
  if (!Array.isArray(commit.faces) || commit.faces.some((f) => !validateFace(f))) errors.push("invalid:faces");
  if (!validateCentroid(commit.centroid)) errors.push("invalid:centroid");

  if (previous) {
    if (commit.prev_hash !== previous.self_hash) errors.push("invalid:prev_hash");
  } else if (commit.prev_hash !== null) {
    errors.push("invalid:genesis_prev_hash");
  }

  const expectedHash = await sha256Hex(canonicalPayload(commit));
  if (expectedHash !== commit.self_hash) errors.push("invalid:self_hash");
  if (!(await validateMerkle(commit))) errors.push("invalid:merkle");

  const signatureVerifier =
    options.signatureVerifier ||
    (async (c) => typeof c.sig === "string" && c.sig.trim().length > 0);
  const sigOk = await signatureVerifier(commit, getSigningMessage(commit));
  if (!sigOk) errors.push("invalid:sig");

  return {
    valid: errors.length === 0,
    errors,
  };
}

export function createEthersSignatureBuilder(ethersApi, signerWallet) {
  if (!ethersApi) throw new Error("ethers API is required");
  if (!signerWallet || typeof signerWallet.signMessage !== "function") {
    throw new Error("signer wallet with signMessage() is required");
  }

  return async (commit, message) => signerWallet.signMessage(message || getSigningMessage(commit));
}

export function createEthersSignatureVerifier(ethersApi, resolveExpectedAddress) {
  if (!ethersApi || typeof ethersApi.verifyMessage !== "function") {
    throw new Error("ethers.verifyMessage is required");
  }

  const resolver =
    resolveExpectedAddress ||
    ((commit) => (commit.vertex && commit.vertex.address ? commit.vertex.address : null));

  return async (commit) => {
    if (!commit || typeof commit.sig !== "string" || commit.sig.trim().length === 0) {
      return false;
    }

    const expected = resolver(commit);
    if (!expected) return false;

    try {
      const recovered = ethersApi.verifyMessage(getSigningMessage(commit), commit.sig);
      return String(recovered).toLowerCase() === String(expected).toLowerCase();
    } catch {
      return false;
    }
  };
}
