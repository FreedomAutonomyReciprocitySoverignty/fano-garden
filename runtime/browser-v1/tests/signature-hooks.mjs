import { readFile } from "node:fs/promises";
import { BrowserV1Runtime } from "../runtime.js";

async function run() {
  const svg = await readFile("dev-docs/artifacts/fano-garden.svg", "utf8");

  const rt = new BrowserV1Runtime({
    signing: {
      signatureBuilder: async () => "sig-ok",
    },
    validation: {
      signatureVerifier: async (commit) => commit.sig === "sig-ok" && !!(commit.vertex && commit.vertex.address),
    },
  });

  await rt.initFromSvg(svg);
  for (const edge of rt.edges) rt.setEdgeState(edge.from, edge.to, "open", 1);

  const commit = await rt.commit("commit");
  if (commit.status === "quarantined") {
    throw new Error(`expected signed commit to validate: ${JSON.stringify(commit.validation_errors || [])}`);
  }

  const rtFail = new BrowserV1Runtime({
    signing: {
      signatureBuilder: async () => "bad-sig",
    },
    validation: {
      signatureVerifier: async () => false,
    },
  });

  await rtFail.initFromSvg(svg);
  const bad = await rtFail.commit("commit");
  if (bad.status !== "quarantined") {
    throw new Error("expected invalid signature commit to be quarantined");
  }

  console.log("signature-hooks:ok", {
    good_status: commit.status,
    bad_status: bad.status,
  });
}

run().catch((err) => {
  console.error("signature-hooks:fail", err.message);
  process.exit(1);
});
