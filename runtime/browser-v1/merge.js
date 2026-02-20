function logicalCounterOf(commit) {
  return Number.isInteger(commit.lc) ? commit.lc : -1;
}

function computeParentRanks(commits) {
  const byId = new Map(commits.map((c) => [String(c.id), c]));
  const memo = new Map();

  function rankFor(commit, visiting = new Set()) {
    const id = String(commit.id);
    if (memo.has(id)) return memo.get(id);
    if (visiting.has(id)) return 0; // cycle guard

    visiting.add(id);
    const parents = Array.isArray(commit.parents) ? commit.parents : [];
    const parentRanks = parents.map((pid) => {
      const p = byId.get(String(pid));
      return p ? rankFor(p, visiting) : 0;
    });
    visiting.delete(id);

    const r = parentRanks.length === 0 ? 0 : 1 + Math.max(...parentRanks);
    memo.set(id, r);
    return r;
  }

  for (const c of commits) rankFor(c);
  return memo;
}

function compareCommits(a, b, ranks) {
  const ar = ranks.get(String(a.id)) ?? 0;
  const br = ranks.get(String(b.id)) ?? 0;
  if (ar !== br) return ar - br;

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
  const merged = dedupeByHash([...localCommits, ...remoteCommits]);
  const ranks = computeParentRanks(merged);
  merged.sort((a, b) => compareCommits(a, b, ranks));
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
