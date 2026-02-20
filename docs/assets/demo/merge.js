function logicalCounterOf(commit) {
  return Number.isInteger(commit.lc) ? commit.lc : -1;
}

function compareCommits(a, b) {
  const alc = logicalCounterOf(a);
  const blc = logicalCounterOf(b);
  if (alc !== blc) return alc - blc;
  if (a.t !== b.t) return a.t - b.t;
  return String(a.id).localeCompare(String(b.id));
}

export function dedupeByHash(commits) {
  const seen = new Set();
  const out = [];
  for (const c of commits) {
    if (seen.has(c.self_hash)) continue;
    seen.add(c.self_hash);
    out.push(c);
  }
  return out;
}

export function mergeCommits(localCommits, remoteCommits) {
  const merged = dedupeByHash([...localCommits, ...remoteCommits]).sort(compareCommits);
  return merged;
}

export function chooseTip(commits) {
  if (commits.length === 0) return null;

  const eligible = commits.filter((c) => c.status === "sealed");
  const pool = eligible.length > 0 ? eligible : commits;

  // Deterministic winner: highest logical counter, then latest timestamp, then lexical minimum self_hash.
  const maxLc = Math.max(...pool.map((c) => logicalCounterOf(c)));
  const atLc = pool.filter((c) => logicalCounterOf(c) === maxLc);
  const maxTime = Math.max(...atLc.map((c) => c.t));
  const atTime = atLc.filter((c) => c.t === maxTime);
  return atTime.sort((a, b) => String(a.self_hash).localeCompare(String(b.self_hash)))[0];
}
