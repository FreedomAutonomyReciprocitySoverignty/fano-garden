export function parseNdjson(text) {
  return text
    .split(/\r?\n/)
    .map((line) => line.trim())
    .filter(Boolean)
    .map((line, idx) => {
      try {
        return JSON.parse(line);
      } catch (err) {
        throw new Error(`Invalid NDJSON line ${idx + 1}: ${err.message}`);
      }
    });
}

export function serializeNdjson(records) {
  return records.map((r) => JSON.stringify(r)).join("\n");
}

export function appendRecord(existingText, record) {
  const prefix = existingText && existingText.trim().length > 0 ? `${existingText.trim()}\n` : "";
  return `${prefix}${JSON.stringify(record)}`;
}
