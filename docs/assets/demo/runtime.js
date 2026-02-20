import { parseFanoSvg } from "./svgParser.js";
import { mapPointsToIdentities, assertNoPrivateMaterial } from "./identity.js";
import { buildCanonicalEdges, evaluateFaceInvariants, computeCentroidState } from "./invariants.js";
import { buildCommit, validateCommit } from "./commit.js";
import { mergeCommits, chooseTip } from "./merge.js";
import { toSvgOverlayState, toThreeSceneTransform } from "./projection.js";

export class BrowserV1Runtime {
  constructor(options = {}) {
    this.options = options;
    this.clock = typeof options.clock === "function" ? options.clock : () => Date.now();
    this.logicalCounter = Number.isInteger(options.counterStart) ? options.counterStart : 0;
    this.identities = [];
    this.edges = [];
    this.faces = [];
    this.centroid = {
      stop_metric: 0,
      closure_ratio: 0,
      sabbath: false,
      reason: "uninitialized",
    };
    this.commits = [];
  }

  async initFromSvg(svgText, identityOptions = {}) {
    const parsed = parseFanoSvg(svgText);
    this.identities = await mapPointsToIdentities(parsed.points, identityOptions);
    this.edges = buildCanonicalEdges(this.identities.map((v) => v.vertex_id));
    this.faces = evaluateFaceInvariants(this.identities, this.edges);
    this.centroid = computeCentroidState(this.faces);
    return this.getState();
  }

  setEdgeState(from, to, channelState, seq = 0) {
    this.edges = this.edges.map((e) => {
      const samePair =
        (e.from === from && e.to === to) ||
        (e.from === to && e.to === from);
      if (!samePair) return e;
      return {
        ...e,
        channel_state: channelState,
        last_seq: seq,
      };
    });
    this.faces = evaluateFaceInvariants(this.identities, this.edges);
    this.centroid = computeCentroidState(this.faces);
  }

  async commit(type = "commit", status) {
    const previous = this.commits.length > 0 ? this.commits[this.commits.length - 1] : null;
    const nowRaw = this.clock();
    const now = Number.isFinite(nowRaw) ? Math.trunc(nowRaw) : Date.now();
    const lc = this.logicalCounter + 1;
    this.logicalCounter = lc;
    const signerVertexId = this.options.signing && this.options.signing.vertex_id;
    const signerVertex =
      this.identities.find((v) => v.vertex_id === signerVertexId) ||
      this.identities[0] ||
      null;

    const next = await buildCommit(
      {
        id: `cmt-${lc}-${now}`,
        t: now,
        lc,
        type,
        parents: previous ? [previous.id] : [],
        identities: this.identities.map((v) => ({
          vertex_id: v.vertex_id,
          path: v.path,
          address: v.address,
          pubkey: v.pubkey,
          fano_point_id: v.fano_point_id,
        })),
        vertex: signerVertex
          ? {
              vertex_id: signerVertex.vertex_id,
              path: signerVertex.path,
              address: signerVertex.address,
              pubkey: signerVertex.pubkey,
              fano_point_id: signerVertex.fano_point_id,
            }
          : null,
        edges: this.edges,
        faces: this.faces,
        centroid: this.centroid,
        status: status || (this.centroid.sabbath ? "sealed" : "validated"),
        sig: "unsigned-local",
      },
      previous,
      {
        signatureBuilder: this.options.signing && this.options.signing.signatureBuilder,
        withMerkle: true,
      }
    );

    const validation = await validateCommit(next, previous, this.options.validation || {});
    if (!validation.valid) {
      next.status = "quarantined";
      next.validation_errors = validation.errors;
    }

    const serialized = JSON.stringify(next);
    if (!assertNoPrivateMaterial(serialized)) {
      next.status = "quarantined";
      next.validation_errors = [...(next.validation_errors || []), "privacy:forbidden_material"]; 
    }

    this.commits.push(next);
    return next;
  }

  mergeRemoteCommits(remoteCommits) {
    this.commits = mergeCommits(this.commits, remoteCommits);
    return chooseTip(this.commits);
  }

  getProjectionState() {
    return {
      svg: toSvgOverlayState(this.identities, this.edges, this.centroid),
      three: toThreeSceneTransform(this.centroid),
    };
  }

  getState() {
    return {
      identities: this.identities,
      edges: this.edges,
      faces: this.faces,
      centroid: this.centroid,
      commits: this.commits,
    };
  }
}
