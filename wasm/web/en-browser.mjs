// English original in the browser: language switch, a few commands, screenshot.
import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join } from "node:path";
import { chromium } from "playwright";
const ROOT = new URL("..", import.meta.url).pathname;
const TYPES = { ".html": "text/html", ".js": "text/javascript", ".wasm": "application/wasm" };
const server = createServer(async (req, res) => {
  try { const p = join(ROOT, decodeURIComponent(new URL(req.url, "http://x").pathname));
    const body = await readFile(p);
    res.writeHead(200, { "content-type": TYPES[extname(p)] ?? "application/octet-stream" });
    res.end(body); } catch { res.writeHead(404); res.end(); }
}).listen(8770);
const b = await chromium.launch();
const page = await b.newPage({ viewport: { width: 1180, height: 820 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
await page.goto("http://localhost:8770/web/index.html?styl=moderni&lang=en");
for (const c of ["LOOK", "GO WEST", "TAKE SHOVEL", "LIST", "SAY HELLO"]) {
  await page.waitForSelector("#cmd:not([disabled])");
  await page.fill("#cmd", c); await page.press("#cmd", "Enter"); await page.waitForTimeout(150);
}
await page.waitForTimeout(300);
console.log((await page.textContent("#screen")).slice(-600));
console.log("panel visible:", await page.locator("aside").isVisible(), "· EN pressed:", await page.getAttribute("#lang-en", "aria-pressed"));
await page.screenshot({ path: "dist/en.png" });
console.log("ERRORS:", errors.length ? errors : "none");
await b.close(); server.close();
