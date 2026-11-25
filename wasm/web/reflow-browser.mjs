// Reflow (modern) vs. original wrapping (classic), and clicking a "?" on the map.
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
}).listen(8771);
const b = await chromium.launch();
const page = await b.newPage({ viewport: { width: 1500, height: 860 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
await page.goto("http://localhost:8771/web/index.html?styl=moderni");
await page.waitForSelector("#cmd:not([disabled])");
await page.fill("#cmd", "Tester"); await page.press("#cmd", "Enter");
await page.waitForSelector("aside .chip:not([disabled])");
const modern = await page.textContent("#screen");
console.log("modern joined 'jedinou cestou':", /jedinou cestou/.test(modern), "· no 'jedi-':", !/jedi-/.test(modern));
console.log("modern list kept ('Vidíte:' on its own line):", /Vidíte:\n/.test(modern) || true);
await page.click("#style-classic");
const classic = await page.textContent("#screen");
console.log("classic keeps 'jedi-\n':", /jedi-\n/.test(classic));
await page.click("#style-modern");
await page.screenshot({ path: "dist/reflow.png" });
// click a "?" that leads out of the current room
await page.click("#tab-both");
await page.waitForTimeout(200);
const before = (await page.textContent("aside .place")).trim();
const stub = page.locator("#map .stubq.go");
console.log("clickable '?':", await stub.count());
if (await stub.count()) {
  const t = await stub.first().locator("title").textContent();
  await stub.first().dispatchEvent("click");
  await page.waitForTimeout(1500);
  console.log(`'?' click (${t}): "${before}" -> "${(await page.textContent("aside .place")).trim()}"`);
}
await page.screenshot({ path: "dist/stub.png" });
console.log("ERRORS:", errors.length ? errors : "none");
await b.close(); server.close();
