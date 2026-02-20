import { readFile } from "node:fs/promises";
import { BrowserV1Runtime } from "../runtime.js";
import { validateCommit } from "../commit.js";

async function run() {
  const svg = await readFile("dev-docs/artifacts/fano-garden.svg", "utf8");

  const rt = new BrowserV1Runtime({
    signing: { signatureBuilder: async () => "sig-ok" },
    validation: { signatureVerifier: async (c) => c.sig === "sig-ok" },
  });

  await rt.initFromSvg(svg);
  for (const edge of rt.edges) rt.setEdgeState(edge.from, edge.to, "open", 1);

  const original = await rt.commit("commit");
  const reordered = JSON.parse(JSON.stringify(original));
  reordered.edges = [...reordered.edges].reverse();

  const result = await validateCommit(reordered, null, rt.options.validation);
  if (result.valid) {
    throw new Error("expected reordered edges commit invalid under ordered semantics");
  }

  if (!result.errors.includes("invalid:merkle") && !result.errors.includes("invalid:self_hash")) {
    throw new Error(`expected invalid:merkle or invalid:self_hash, got: ${JSON.stringify(result.errors)}`);
  }

  console.log("merkle-ordering:ok", {
    original_root: String(original.merkle && original.merkle.root).slice(0, 18),
    errors: result.errors,
  });
}

run().catch((err) => {
  console.error("merkle-ordering:fail", err.message);
  process.exit(1);
});
