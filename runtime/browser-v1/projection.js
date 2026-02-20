export function toSvgOverlayState(identities, edges, centroid, options = {}) {
  const vertexColor = centroid.sabbath ? "#ff9030" : "#c9b99a";
  const edgeColor = centroid.sabbath ? "#c9b99a" : "#3a3530";

  return {
    svg_overlay_id: options.svg_overlay_id || "overlay-main",
    vertices: identities.map((v) => ({
      id: v.vertex_id,
      x: v.x,
      y: v.y,
      label: `${v.fano_point_id}`,
      color: vertexColor,
      address: v.address,
    })),
    edges: edges.map((e) => ({
      id: e.edge_id,
      from: e.from,
      to: e.to,
      color: edgeColor,
      state: e.channel_state,
    })),
    render_hints: {
      opacity: centroid.sabbath ? 1.0 : 0.85,
    },
  };
}

export function toThreeSceneTransform(centroid) {
  const z = Number((centroid.stop_metric * 10).toFixed(4));
  return {
    position: [0, 0, z],
    rotation: [0, 0, 0],
    scale: [1, 1, 1],
  };
}
