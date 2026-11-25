// Map check: wander randomly by clicking exits, then screenshot the map.
//   docker run --rm -v "$PWD/wasm":/w -w /w mcr.microsoft.com/playwright:v1.56.0-noble \
//     bash -c "npm i --no-save playwright@1.56.0 >/dev/null && node web/map-browser.mjs [steps]"
import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join } from "node:path";
import { chromium } from "playwright";

const steps = +(process.argv[2] ?? 60);
const ROOT = new URL("..", import.meta.url).pathname;
const TYPES = { ".html": "text/html", ".js": "text/javascript", ".wasm": "application/wasm" };
const server = createServer(async (req, res) => {
  try {
    const path = join(ROOT, decodeURIComponent(new URL(req.url, "http://x").pathname));
    const body = await readFile(path);
    res.writeHead(200, { "content-type": TYPES[extname(path)] ?? "application/octet-stream" });
    res.end(body);
  } catch { res.writeHead(404); res.end(); }
}).listen(8766);

const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1280, height: 860 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
await page.goto("http://localhost:8766/web/index.html?styl=moderni");
await page.waitForSelector("#cmd:not([disabled])");
await page.fill("#cmd", "Mapovac");
await page.press("#cmd", "Enter");

let rng = 12345;
const rand = (n) => { rng ^= rng << 13; rng ^= rng >>> 17; rng ^= rng << 5; return (rng >>> 0) % n; };
const extra = ["OTEVŘÍT BRÁNU", "ROZSVÍTIT LUCERNU", "VZÍT LUCERNU", "VZÍT KOMPAS"];
for (let i = 0; i < steps; i++) {
  await page.waitForSelector("aside .chip:not([disabled])", { timeout: 10000 }).catch(() => {});
  if (!(await page.locator("#cmd:not([disabled])").count())) break;
  const exits = page.locator("aside h2:has-text('Východy') + .chips button:not(.closed)");
  const n = await exits.count();
  if (n === 0 || rand(8) === 0) {
    await page.fill("#cmd", extra[rand(extra.length)]);
    await page.press("#cmd", "Enter");
  } else await exits.nth(rand(n)).click();
  await page.waitForTimeout(60);
  // answer any question (ANO/NE) with NE
  if (!(await page.locator("aside .chip:not([disabled])").count()) && await page.locator("#cmd:not([disabled])").count()) {
    await page.fill("#cmd", "NE");
    await page.press("#cmd", "Enter");
  }
}
// click-to-walk: pick the farthest reachable room and let the map walk there
await page.click("#tab-map");
await page.waitForTimeout(300);
const far = page.locator("#map .room.far");
const nFar = await far.count();
let best = null, bestSteps = 0;
for (let i = 0; i < nFar; i++) {
  const t = await far.nth(i).locator("title").textContent();
  const m = t.match(/\((\d+) kroků\)/);
  if (m && +m[1] > bestSteps) { bestSteps = +m[1]; best = i; }
}
if (best !== null) {
  const title = await far.nth(best).locator("title").textContent();
  const target = title.split(" · ")[0];
  await far.nth(best).dispatchEvent("click");
  let last = "", same = 0;
  while (same < 8) {                       // wait until the walk settles
    await page.waitForTimeout(250);
    const now = await page.textContent("aside .place");
    same = now === last ? same + 1 : 0; last = now;
  }
  console.log(`WALK: ${bestSteps} steps to "${target}" -> now "${last.trim()}"`,
    last.trim() === target || target === "tmavá místnost" ? "OK" : "CHECK");
} else console.log("WALK: no far room to test");
console.log((await page.textContent("#screen")).slice(-700));
await page.waitForTimeout(400);
await page.screenshot({ path: "dist/map.png" });
await page.click("#tab-both");
await page.waitForTimeout(300);
await page.screenshot({ path: "dist/both-narrow.png" });
await page.setViewportSize({ width: 1720, height: 860 });
await page.waitForTimeout(400);
await page.screenshot({ path: "dist/both-wide.png" });
const rooms = await page.locator("#map .room").count();
console.log(`rooms on map: ${rooms}; here: ${await page.textContent("aside .place")}`);
console.log("ERRORS:", errors.length ? errors : "none");
await browser.close();
server.close();
