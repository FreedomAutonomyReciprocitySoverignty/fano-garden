import { FANO_LINE_TABLE } from "./constants.js";

function dist2(a, b) {
  const dx = a.x - b.x;
  const dy = a.y - b.y;
  return dx * dx + dy * dy;
}

function parseAttrs(tag) {
  const attrs = {};
  const re = /([a-zA-Z_:][-a-zA-Z0-9_:.]*)\s*=\s*"([^"]*)"/g;
  let m;
  while ((m = re.exec(tag)) !== null) {
    attrs[m[1]] = m[2];
  }
  return attrs;
}

function parseWithRegex(svgText) {
  const circles = [];
  const circleRe = /<circle\b[^>]*>/g;
  let cm;
  let idx = 0;
  while ((cm = circleRe.exec(svgText)) !== null) {
    const attrs = parseAttrs(cm[0]);
    const cx = Number(attrs.cx);
    const cy = Number(attrs.cy);
    if (!Number.isFinite(cx) || !Number.isFinite(cy)) continue;
    circles.push({
      source_index: idx++,
      x: cx,
      y: cy,
      id_attr: attrs.id || "",
      data_index: attrs["data-index"] || attrs["data-point-id"] || "",
    });
  }

  const labels = [];
  const textRe = /<text\b([^>]*)>([^<]*)<\/text>/g;
  let tm;
  while ((tm = textRe.exec(svgText)) !== null) {
    const attrs = parseAttrs(`<text ${tm[1]}>`);
    const x = Number(attrs.x);
    const y = Number(attrs.y);
    const txt = String(tm[2] || "").trim();
    if (!Number.isFinite(x) || !Number.isFinite(y)) continue;
    if (!/^\d+$/.test(txt)) continue;
    labels.push({ x, y, value: Number(txt) });
  }

  return { circles, labels };
}

function parseWithDom(svgText) {
  if (typeof DOMParser === "undefined") return null;

  const parser = new DOMParser();
  const doc = parser.parseFromString(svgText, "image/svg+xml");
  const circles = Array.from(doc.querySelectorAll("circle"))
    .map((el, i) => ({
      source_index: i,
      x: Number(el.getAttribute("cx")),
      y: Number(el.getAttribute("cy")),
      id_attr: el.getAttribute("id") || "",
      data_index: el.getAttribute("data-index") || el.getAttribute("data-point-id") || "",
    }))
    .filter((p) => Number.isFinite(p.x) && Number.isFinite(p.y));

  const labels = Array.from(doc.querySelectorAll("text"))
    .map((el) => ({
      x: Number(el.getAttribute("x")),
      y: Number(el.getAttribute("y")),
      text: String(el.textContent || "").trim(),
    }))
    .filter((t) => Number.isFinite(t.x) && Number.isFinite(t.y) && /^\d+$/.test(t.text))
    .map((t) => ({ x: t.x, y: t.y, value: Number(t.text) }));

  return { circles, labels };
}

function inferPointId(circle, labels) {
  if (/^\d+$/.test(circle.data_index)) return Number(circle.data_index) + 1;
  const idDigits = circle.id_attr.match(/(\d+)/);
  if (idDigits) return Number(idDigits[1]);

  if (labels.length === 0) return null;
  let nearest = labels[0];
  let min = dist2(circle, nearest);
  for (let i = 1; i < labels.length; i += 1) {
    const d = dist2(circle, labels[i]);
    if (d < min) {
      nearest = labels[i];
      min = d;
    }
  }
  // Threshold to avoid grabbing unrelated labels.
  return min <= 28 * 28 ? nearest.value : null;
}

export function parseFanoSvg(svgText) {
  const parsed = parseWithDom(svgText) || parseWithRegex(svgText);
  const circles = parsed.circles;
  const labels = parsed.labels;

  if (circles.length < 7) {
    throw new Error(`Expected at least 7 circles, found ${circles.length}`);
  }

  let points = circles.map((c) => ({
    ...c,
    fano_point_id: inferPointId(c, labels),
  }));

  const usedIds = new Set();
  for (const p of points) {
    if (!Number.isInteger(p.fano_point_id) || p.fano_point_id < 1 || p.fano_point_id > 7 || usedIds.has(p.fano_point_id)) {
      p.fano_point_id = null;
    } else {
      usedIds.add(p.fano_point_id);
    }
  }

  // Fallback deterministic ordering by (y, x, source_index)
  const fallbackSorted = [...points].sort((a, b) => {
    if (a.y !== b.y) return a.y - b.y;
    if (a.x !== b.x) return a.x - b.x;
    return a.source_index - b.source_index;
  });

  let fallbackId = 1;
  for (const p of fallbackSorted) {
    if (!p.fano_point_id) {
      while (usedIds.has(fallbackId) && fallbackId <= 7) fallbackId += 1;
      if (fallbackId <= 7) {
        p.fano_point_id = fallbackId;
        usedIds.add(fallbackId);
      }
    }
  }

  points = points
    .filter((p) => Number.isInteger(p.fano_point_id) && p.fano_point_id >= 1 && p.fano_point_id <= 7)
    .sort((a, b) => {
      if (a.fano_point_id !== b.fano_point_id) return a.fano_point_id - b.fano_point_id;
      if (a.y !== b.y) return a.y - b.y;
      if (a.x !== b.x) return a.x - b.x;
      return a.source_index - b.source_index;
    })
    .slice(0, 7)
    .map((p, i) => ({
      vertex_id: `v${i + 1}`,
      fano_point_id: p.fano_point_id,
      x: p.x,
      y: p.y,
      source_index: p.source_index,
    }));

  if (points.length !== 7) {
    throw new Error("Unable to resolve exactly 7 canonical Fano points.");
  }

  return {
    points,
    lines: FANO_LINE_TABLE,
  };
}
