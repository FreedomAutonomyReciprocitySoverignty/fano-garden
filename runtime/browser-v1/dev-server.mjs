import { createServer } from "node:http";
import { readFile, stat } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const repoRoot = path.resolve(__dirname, "..", "..");

const port = Number(process.env.PORT || 4173);

const contentTypes = {
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".ndjson": "application/x-ndjson; charset=utf-8",
  ".svg": "image/svg+xml",
  ".css": "text/css; charset=utf-8",
  ".png": "image/png",
  ".jpg": "image/jpeg",
  ".jpeg": "image/jpeg",
  ".webp": "image/webp",
  ".txt": "text/plain; charset=utf-8",
};

function safeResolve(urlPath) {
  const cleaned = decodeURIComponent(urlPath.split("?")[0]);
  const rel = cleaned === "/" ? "/runtime/browser-v1/demo/index.html" : cleaned;
  const abs = path.resolve(repoRoot, `.${rel}`);
  if (!abs.startsWith(repoRoot)) return null;
  return abs;
}

const server = createServer(async (req, res) => {
  try {
    const absPath = safeResolve(req.url || "/");
    if (!absPath) {
      res.writeHead(403, { "content-type": "text/plain; charset=utf-8" });
      res.end("Forbidden\n");
      return;
    }

    let target = absPath;
    try {
      const s = await stat(target);
      if (s.isDirectory()) {
        target = path.join(target, "index.html");
      }
    } catch {
      // Keep target; readFile below returns 404 path.
    }

    const data = await readFile(target);
    const ext = path.extname(target).toLowerCase();
    const ct = contentTypes[ext] || "application/octet-stream";

    res.writeHead(200, {
      "content-type": ct,
      "cache-control": "no-cache",
      "access-control-allow-origin": "*",
    });
    res.end(data);
  } catch {
    res.writeHead(404, { "content-type": "text/plain; charset=utf-8" });
    res.end("Not Found\n");
  }
});

server.listen(port, () => {
  console.log(`[browser-v1] dev server running at http://localhost:${port}`);
  console.log(`[browser-v1] demo: http://localhost:${port}/runtime/browser-v1/demo/index.html`);
});
