import { readFile } from "node:fs/promises";
import { BrowserV1Runtime } from "../runtime.js";
import { validateCommit } from "../commit.js";

async function run() {
  const svg = await readFile("dev-docs/artifacts/fano-garden.svg", "utf8");

  const rt = new BrowserV1Runtime({
    signing: {
      signatureBuilder: async () => "sig-ok",
    },
    validation: {
      signatureVerifier: async (commit) => commit.sig === "sig-ok",
    },
  });

  await rt.initFromSvg(svg);
  for (const edge of rt.edges) rt.setEdgeState(edge.from, edge.to, "open", 1);

  const good = await rt.commit("commit");
  if (!good.merkle || !good.merkle.root) {
    throw new Error("expected commit merkle root");
  }

  const goodValidation = await validateCommit(good, null, rt.options.validation);
  if (!goodValidation.valid) {
    throw new Error(`expected good commit valid, got: ${JSON.stringify(goodValidation.errors)}`);
  }

  const tampered = JSON.parse(JSON.stringify(good));
  tampered.faces[0].status = tampered.faces[0].status === "pass" ? "fail" : "pass";

  const badValidation = await validateCommit(tampered, null, rt.options.validation);
  if (badValidation.valid) {
    throw new Error("expected tampered merkle commit invalid");
  }
  if (!badValidation.errors.includes("invalid:merkle") && !badValidation.errors.includes("invalid:self_hash")) {
    throw new Error(`expected merkle or self_hash error, got: ${JSON.stringify(badValidation.errors)}`);
  }

  console.log("merkle:ok", {
    root_prefix: String(good.merkle.root).slice(0, 18),
    bad_errors: badValidation.errors,
  });
}

run().catch((err) => {
  console.error("merkle:fail", err.message);
  process.exit(1);
});
