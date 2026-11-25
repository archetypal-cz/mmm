// Render a synthetic map state with visited maze rooms (no gameplay needed).
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
}).listen(8774);
const b = await chromium.launch();
const page = await b.newPage({ viewport: { width: 1400, height: 900 } });
await page.goto("http://localhost:8774/web/index.html?styl=moderni");
await page.waitForSelector("#cmd:not([disabled])");
// pretend the player has seen the park maze and the woods, then render
await page.evaluate(async () => {
  const { AutoMap } = await import("./map.js");
  const { LAYOUT } = await import("./layout.js");
  const m = new AutoMap(); m.key = "x"; m.rooms = {};
  for (const r of [58,59,60,61,62,63,64,65,66,67,68,69,70,93,94,95,96,97,98,99,71,72,73,74]) {
    const l = LAYOUT[r]; if (l) m.rooms[r] = { x: l[0], y: l[1], z: l[2], lit: true, name: "místnost " + r };
  }
  const fake = { room: 93, facing: 1, compassNeeded: false, hasCompass: false, exits: [],
    exitsOf: () => [], nameOf: (r) => "místnost " + r };
  document.querySelector(".app").classList.add("map");
  document.getElementById("mapbox").style.display = "block";
  m.render(document.getElementById("map"), fake, () => {});
});
await page.waitForTimeout(300);
await page.screenshot({ path: "dist/maze.png" });
await b.close(); server.close();
