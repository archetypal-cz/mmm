// Synthetic: facing north -> east without a compass; the world group turns smoothly.
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
}).listen(8776);
const b = await chromium.launch();
const page = await b.newPage({ viewport: { width: 1400, height: 900 } });
await page.goto("http://localhost:8776/web/index.html?styl=moderni");
await page.waitForSelector("#cmd:not([disabled])");
await page.evaluate(async () => {
  const { AutoMap } = await import("./map.js");
  const { LAYOUT } = await import("./layout.js");
  window.m = new AutoMap(); m.key = "x"; m.rooms = {};
  for (const r of [10, 11, 12, 13, 14, 15, 16, 17, 18]) { const l = LAYOUT[r]; m.rooms[r] = { x: l[0], y: l[1], z: l[2], lit: true, name: "místnost " + r }; }
  window.fake = (f) => ({ room: 11, facing: f, compassNeeded: true, hasCompass: false, exits: [], exitsOf: () => [], nameOf: (r) => "m" + r });
  document.querySelector(".app").classList.add("map");
  m.render(document.getElementById("map"), fake(1), () => {});
});
await page.waitForTimeout(300);
await page.screenshot({ path: "dist/rot-0.png", clip: { x: 240, y: 110, width: 620, height: 380 } });
await page.evaluate(() => m.render(document.getElementById("map"), fake(2), () => {}));
const tr = () => page.evaluate(() => getComputedStyle(document.querySelector("#map .world")).transform);
await page.waitForTimeout(200);
const mid = await tr();
await page.screenshot({ path: "dist/rot-mid.png", clip: { x: 240, y: 110, width: 620, height: 380 } });
await page.waitForTimeout(600);
console.log("mid:", mid, "\nend:", await tr());
const lbl = await page.evaluate(() => getComputedStyle(document.querySelector("#map .room .upright")).transform);
console.log("label counter-rotation:", lbl);
await page.screenshot({ path: "dist/rot-end.png", clip: { x: 240, y: 110, width: 620, height: 380 } });
await b.close(); server.close();
