// Items the game hides in the room text (lantern on the hook at the gate,
// wedge in the woodpile) are not listed in "Vidíte" and only become
// clickable once some game text names them.
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
const page = await b.newPage({ viewport: { width: 1500, height: 860 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
await page.goto("http://localhost:8774/web/index.html?styl=moderni");
await page.waitForSelector("#cmd:not([disabled])");
await page.fill("#cmd", "Tester"); await page.press("#cmd", "Enter");
await page.waitForSelector("#screen .kw");
const say = async (c) => { await page.fill("#cmd", c); await page.press("#cmd", "Enter"); await page.waitForTimeout(350); };
const report = async (tag) => {
  const side = (await page.textContent("aside")).replace(/\s+/g, " ");
  const vid = side.match(/Vidíte(.*?)(Je zde|$)/)?.[1] ?? "";
  console.log(`[${tag}] place: ${(await page.textContent("aside .place")).trim()}`);
  console.log(`   Vidíte: ${vid.trim()}`);
  console.log(`   keywords: ${(await page.locator("#screen .kw.here").allTextContents()).join(", ")}`);
};
await say("ROZHLÉDNOUT SE"); await report("gate");
const gate = page.locator("#screen .kw.here", { hasText: /brán/i }).first();
console.log("gate word clickable:", await gate.count());
if (await gate.count()) {
  await gate.click(); await page.waitForSelector("#pop:not([hidden])");
  console.log("gate menu:", (await page.locator("#pop .chips").first().allTextContents()).join(" "));
  await page.screenshot({ path: "dist/gate-pop.png" });
  await page.locator("#pop button", { hasText: /^podívat se na$/ }).click();
  await page.waitForTimeout(400);
  console.log("after:", (await page.textContent("#screen")).trim().split(String.fromCharCode(10)).slice(-3).join(" | "));
}
console.log("ERRORS:", errors.length ? errors : "none");
await b.close(); server.close();
