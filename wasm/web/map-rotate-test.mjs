import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join } from "node:path";
import { chromium } from "playwright";
const ROOT = new URL("..", import.meta.url).pathname;
const server = createServer(async (req, res) => {
  try { const p = join(ROOT, new URL(req.url, "http://x").pathname);
    const body = await readFile(p);
    res.writeHead(200, { "content-type": { ".html": "text/html", ".js": "text/javascript" }[extname(p)] || "text/plain" });
    res.end(body); } catch { res.writeHead(404); res.end(); }
}).listen(8767);
const b = await chromium.launch(); const page = await b.newPage({ viewport: { width: 1730, height: 330 } });
await page.goto("http://localhost:8767/web/map-rotate-test.html"); await page.waitForTimeout(500);
await page.screenshot({ path: "dist/map-rotate.png" }); await b.close(); server.close();
