// In the dense forest (no compass) the map turns smoothly around the player.
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
}).listen(8775);
const b = await chromium.launch();
const page = await b.newPage({ viewport: { width: 1500, height: 900 } });
await page.goto("http://localhost:8775/web/index.html?styl=moderni");
const cmd = async (c) => { await page.waitForSelector("#cmd:not([disabled])"); await page.fill("#cmd", c); await page.press("#cmd", "Enter"); await page.waitForTimeout(700); };
await cmd("Les");
await page.click("#tab-both");
for (const c of ["JÍT ZÁPADNĚ", "JÍT SEVERNĚ"]) await cmd(c);
console.log("here:", (await page.textContent("aside .place")).trim(), "|", await page.textContent("#orient"));
const rot = () => page.evaluate(() => getComputedStyle(document.querySelector("#map .world")).transform);
for (const [i, c] of ["JÍT VPRAVO", "JÍT VPRAVO", "JÍT VLEVO"].entries()) {
  await page.waitForSelector("#cmd:not([disabled])"); await page.fill("#cmd", c); await page.press("#cmd", "Enter");
  await page.waitForTimeout(250);
  const mid = await rot();
  await page.screenshot({ path: `dist/forest-${i}.png`, clip: { x: 230, y: 110, width: 630, height: 380 } });
  await page.waitForTimeout(700);
  console.log(c, "| mid:", mid, "| end:", await rot(), "|", (await page.textContent("aside .place")).trim());
}
// switch rotation off
await page.click("#orient .orient-toggle");
await page.waitForTimeout(700);
console.log("after 'neotáčet':", await rot(), "|", await page.textContent("#orient"));
await b.close(); server.close();
