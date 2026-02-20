function isObject(value) {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}

export function stableStringify(value) {
  if (Array.isArray(value)) {
    return `[${value.map((v) => stableStringify(v)).join(",")}]`;
  }
  if (isObject(value)) {
    const keys = Object.keys(value).sort();
    const entries = keys.map((k) => `${JSON.stringify(k)}:${stableStringify(value[k])}`);
    return `{${entries.join(",")}}`;
  }
  return JSON.stringify(value);
}

function toHex(bytes) {
  return `0x${Array.from(bytes)
    .map((b) => b.toString(16).padStart(2, "0"))
    .join("")}`;
}

async function hashInBrowser(text) {
  const bytes = new TextEncoder().encode(text);
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return toHex(new Uint8Array(digest));
}

async function hashInNode(text) {
  const { createHash } = await import("node:crypto");
  return `0x${createHash("sha256").update(text).digest("hex")}`;
}

export async function sha256Hex(text) {
  if (typeof crypto !== "undefined" && crypto.subtle) {
    return hashInBrowser(text);
  }
  return hashInNode(text);
}

export function canonicalPayload(commit) {
  const clone = { ...commit };
  delete clone.self_hash;
  delete clone.sig;
  return stableStringify(clone);
}
