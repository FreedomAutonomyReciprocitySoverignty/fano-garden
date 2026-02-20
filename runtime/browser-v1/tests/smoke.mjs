import { readFile } from "node:fs/promises";
import { BrowserV1Runtime } from "../runtime.js";

async function main() {
  const svg = await readFile("dev-docs/artifacts/fano-garden.svg", "utf8");
  const rt = new BrowserV1Runtime();

  await rt.initFromSvg(svg);

  // Open all canonical edges so all faces close.
  for (const edge of rt.edges) {
    rt.setEdgeState(edge.from, edge.to, "open", 1);
  }

  const commit = await rt.commit("commit");
  const tip = rt.mergeRemoteCommits([]);

  if (!commit || !commit.self_hash) throw new Error("commit not produced");
  if (!commit.merkle || !commit.merkle.root) throw new Error("merkle root not produced");
  if (!rt.centroid.sabbath) throw new Error("expected sabbath=true after full edge open");
  if (!tip || tip.self_hash !== commit.self_hash) throw new Error("tip selection mismatch");

  console.log("smoke:ok", {
    identities: rt.identities.length,
    faces: rt.faces.length,
    stop_metric: rt.centroid.stop_metric,
    sabbath: rt.centroid.sabbath,
    tip: tip.self_hash,
  });
}

main().catch((err) => {
  console.error("smoke:fail", err.message);
  process.exit(1);
});
