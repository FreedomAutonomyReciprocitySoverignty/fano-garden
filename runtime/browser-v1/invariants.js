import { FANO_LINE_TABLE } from "./constants.js";

function edgeKey(a, b) {
  return a < b ? `${a}:${b}` : `${b}:${a}`;
}

export function buildCanonicalEdges(vertexIds) {
  const edges = [];
  for (let i = 0; i < vertexIds.length; i += 1) {
    for (let j = i + 1; j < vertexIds.length; j += 1) {
      edges.push({
        edge_id: `e-${vertexIds[i]}-${vertexIds[j]}`,
        from: vertexIds[i],
        to: vertexIds[j],
        channel_state: "down",
        last_seq: 0,
      });
    }
  }
  return edges;
}

export function evaluateFaceInvariants(identities, edges) {
  const pointToVertex = new Map(identities.map((v) => [v.fano_point_id, v.vertex_id]));
  const edgeState = new Map(
    edges.map((e) => [edgeKey(e.from, e.to), e.channel_state])
  );

  return FANO_LINE_TABLE.map((line) => {
    const vertices = line.points.map((pid) => pointToVertex.get(pid));
    const pairs = [
      [vertices[0], vertices[1]],
      [vertices[1], vertices[2]],
      [vertices[0], vertices[2]],
    ];

    const openCount = pairs.reduce((acc, [a, b]) => {
      const state = edgeState.get(edgeKey(a, b));
      return acc + (state === "open" ? 1 : 0);
    }, 0);

    const status = openCount === 3 ? "pass" : "fail";

    return {
      face_id: line.line_id,
      vertices,
      invariant_name: "fano_line_closure",
      status,
      evidence: {
        rule: line.line_id,
        open_edges: openCount,
      },
    };
  });
}

export function computeCentroidState(faces) {
  const total = FANO_LINE_TABLE.length;
  const passed = faces.filter((f) => f.status === "pass").length;
  const ratio = total === 0 ? 0 : passed / total;
  const sabbath = ratio === 1;

  return {
    stop_metric: ratio,
    closure_ratio: ratio,
    sabbath,
    reason: sabbath ? "all_invariants_closed" : `incomplete_faces:${passed}/${total}`,
  };
}
