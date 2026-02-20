import { HD_PATH_TEMPLATE } from "./constants.js";
import { sha256Hex } from "./canonicalize.js";

function pathForPoint(fanoPointId, template = HD_PATH_TEMPLATE) {
  const i = fanoPointId - 1;
  return template.replace("{i}", String(i));
}

function pseudoAddressFromHash(hex) {
  const body = hex.replace(/^0x/, "").slice(0, 40).padEnd(40, "0");
  return `0x${body}`;
}

function pseudoPubkeyFromHash(hex) {
  const body = hex.replace(/^0x/, "").slice(0, 66).padEnd(66, "0");
  return `0x02${body.slice(2, 66)}`;
}

export async function defaultPublicDeriver(path) {
  const digest = await sha256Hex(path);
  return {
    address: pseudoAddressFromHash(digest),
    pubkey: pseudoPubkeyFromHash(digest),
  };
}

export function ethersPublicDeriver(ethersApi, phrase) {
  if (!ethersApi || !ethersApi.HDNodeWallet) {
    throw new Error("ethers v6 HDNodeWallet API is required for ethersPublicDeriver");
  }
  // Force root node depth so absolute derivation paths like m/44'/... are valid.
  const wallet = ethersApi.HDNodeWallet.fromPhrase(phrase, undefined, "m");

  return async (path) => {
    const child = wallet.derivePath(path);
    return {
      address: child.address,
      pubkey: child.publicKey,
    };
  };
}

export async function mapPointsToIdentities(points, options = {}) {
  const template = options.pathTemplate || HD_PATH_TEMPLATE;
  const derive = options.deriver || defaultPublicDeriver;

  const out = [];
  for (const p of points) {
    const path = pathForPoint(p.fano_point_id, template);
    const { address, pubkey } = await derive(path, p.fano_point_id, p);

    out.push({
      vertex_id: p.vertex_id,
      path,
      address,
      pubkey,
      fano_point_id: p.fano_point_id,
      x: p.x,
      y: p.y,
    });
  }

  return out.sort((a, b) => a.fano_point_id - b.fano_point_id);
}

export function assertNoPrivateMaterial(text) {
  const forbiddenPatterns = [
    /\b(?:[a-z]+\s+){11}[a-z]+\b/i, // naive 12-word mnemonic detector
    /\bxprv[1-9A-HJ-NP-Za-km-z]+\b/,
    /\bprivate[_\- ]?key\b/i,
    /\bmnemonic\b/i,
  ];
  return !forbiddenPatterns.some((rx) => rx.test(text));
}
