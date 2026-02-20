import { readFile } from "node:fs/promises";
import { BrowserV1Runtime } from "../runtime.js";
import { chooseTip } from "../merge.js";

function makeClock(values) {
  let i = 0;
  return () => {
    const v = values[Math.min(i, values.length - 1)];
    i += 1;
    return v;
  };
}

async function buildTwoCommits(clockValues) {
  const svg = await readFile("dev-docs/artifacts/fano-garden.svg", "utf8");

  const rt = new BrowserV1Runtime({
    clock: makeClock(clockValues),
    counterStart: 0,
    signing: { signatureBuilder: async () => "sig-ok" },
    validation: { signatureVerifier: async (c) => c.sig === "sig-ok" },
  });

  await rt.initFromSvg(svg);

  for (const edge of rt.edges) {
    rt.setEdgeState(edge.from, edge.to, "open", 1);
  }

  const c1 = await rt.commit("commit");
  const c2 = await rt.commit("commit");
  return [c1, c2];
}

async function run() {
  const [a1, a2] = await buildTwoCommits([1000, 1001]);
  const [b1, b2] = await buildTwoCommits([1000, 1001]);

  if (a1.self_hash !== b1.self_hash || a2.self_hash !== b2.self_hash) {
    throw new Error("expected deterministic hashes across identical clocked runs");
  }

  if (a1.lc !== 1 || a2.lc !== 2) {
    throw new Error(`expected monotonic lc sequence 1,2 got ${a1.lc},${a2.lc}`);
  }

  const higherLcOlderTime = { ...a2, t: 1, status: "sealed" };
  const lowerLcNewerTime = { ...a1, t: 999999, status: "sealed" };
  const tip = chooseTip([lowerLcNewerTime, higherLcOlderTime]);

  if (tip.lc !== 2) {
    throw new Error("expected chooseTip to prioritize higher logical counter");
  }

  console.log("logical-clock:ok", {
    run1: [a1.self_hash.slice(0, 16), a2.self_hash.slice(0, 16)],
    lc: [a1.lc, a2.lc],
    tip_lc: tip.lc,
  });
}

run().catch((err) => {
  console.error("logical-clock:fail", err.message);
  process.exit(1);
});
