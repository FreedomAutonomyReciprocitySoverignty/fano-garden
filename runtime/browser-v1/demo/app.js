import { BrowserV1Runtime } from "../runtime.js";
import { ethersPublicDeriver } from "../identity.js";
import {
  createEthersSignatureBuilder,
  createEthersSignatureVerifier,
} from "../commit.js";

async function loadText(path) {
  const res = await fetch(path);
  if (!res.ok) throw new Error(`Failed to fetch ${path}: ${res.status}`);
  return res.text();
}

function log(obj) {
  const pre = document.getElementById("log");
  pre.textContent = `${typeof obj === "string" ? obj : JSON.stringify(obj, null, 2)}\n${pre.textContent}`;
}

function renderOverlay(container, overlay) {
  const lines = overlay.edges
    .map((e) => {
      const a = overlay.vertices.find((v) => v.id === e.from);
      const b = overlay.vertices.find((v) => v.id === e.to);
      if (!a || !b) return "";
      return `<line x1="${a.x}" y1="${a.y}" x2="${b.x}" y2="${b.y}" stroke="${e.color}" stroke-width="1"/>`;
    })
    .join("\n");

  const dots = overlay.vertices
    .map(
      (v) =>
        `<g><circle cx="${v.x}" cy="${v.y}" r="4" fill="${v.color}"/><text x="${v.x + 6}" y="${v.y - 6}" font-size="8" fill="#ddd">${v.label}</text></g>`
    )
    .join("\n");

  container.innerHTML = `<svg viewBox="0 0 400 300" width="100%" height="100%">${lines}${dots}</svg>`;
}

async function main() {
  if (!window.ethers || !window.ethers.HDNodeWallet) {
    throw new Error("Ethers v6 was not loaded.");
  }

  // Demo-only phrase. Never log or persist this.
  const phrase = "test test test test test test test test test test test junk";
  const signer = window.ethers.HDNodeWallet.fromPhrase(phrase, undefined, "m").derivePath("m/44'/60'/0'/0/0");

  const runtime = new BrowserV1Runtime({
    signing: {
      vertex_id: "v1",
      signatureBuilder: createEthersSignatureBuilder(window.ethers, signer),
    },
    validation: {
      signatureVerifier: createEthersSignatureVerifier(window.ethers),
    },
  });

  // Use an existing repo artifact as seed geometry.
  const svgText = await loadText("../../../dev-docs/artifacts/fano-garden.svg");
  await runtime.initFromSvg(svgText, {
    deriver: ethersPublicDeriver(window.ethers, phrase),
  });
  log({ phase: "init", centroid: runtime.centroid, identities: runtime.identities.length });

  for (const edge of runtime.edges) {
    runtime.setEdgeState(edge.from, edge.to, "open", 1);
  }

  const commit = await runtime.commit("commit");
  const projection = runtime.getProjectionState();

  renderOverlay(document.getElementById("overlay"), projection.svg);

  log({
    phase: "commit",
    commit: {
      id: commit.id,
      status: commit.status,
      self_hash: commit.self_hash,
      merkle_root: commit.merkle && commit.merkle.root,
      signer: commit.vertex && commit.vertex.address,
      sig_prefix: String(commit.sig).slice(0, 16),
    },
  });
  log({ phase: "projection", three: projection.three, centroid: runtime.centroid });
}

main().catch((err) => {
  log({ error: err.message });
});
