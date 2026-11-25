// The "you are here" frame flows between rooms: sample its position mid-move.
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
}).listen(8772);
const b = await chromium.launch();
const page = await b.newPage({ viewport: { width: 1500, height: 860 } });
await page.goto("http://localhost:8772/web/index.html?styl=moderni");
await page.waitForSelector("#cmd:not([disabled])");
await page.fill("#cmd", "Anim"); await page.press("#cmd", "Enter");
await page.click("#tab-both");
await page.locator("aside").getByRole("button", { name: "Z", exact: true }).click();
await page.waitForTimeout(400);
const pos = () => page.evaluate(() => getComputedStyle(document.querySelector("#map .hl")).transform);
await page.locator("aside h2:has-text('Východy') + .chips button:not(.closed)").first().click();
const samples = [];
for (const t of [60, 160, 260, 600]) {
  await page.waitForTimeout(t - (samples.length ? [60, 160, 260, 600][samples.length - 1] : 0));
  samples.push(await pos());
  await page.screenshot({ path: `dist/anim-${t}.png`, clip: { x: 0, y: 40, width: 1000, height: 420 } });
}
console.log(samples.join("\n"));
await b.close(); server.close();
