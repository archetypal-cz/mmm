// Clickable keywords in the latest description: directions, items, people.
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
}).listen(8773);
const b = await chromium.launch();
const page = await b.newPage({ viewport: { width: 1500, height: 860 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
await page.goto("http://localhost:8773/web/index.html?styl=moderni");
await page.waitForSelector("#cmd:not([disabled])");
await page.fill("#cmd", "Tester"); await page.press("#cmd", "Enter");
await page.waitForSelector("#screen .kw");
await page.fill("#cmd", "ROZHLÉDNOUT SE"); await page.press("#cmd", "Enter");
await page.waitForTimeout(500);
const kws = await page.locator("#screen .kw").allTextContents();
console.log("keywords at the gate:", kws.join(", "));
// click the west direction word
await page.locator("#screen .kw.dir", { hasText: /Z[AÁ]PADNĚ/ }).first().click();
await page.waitForTimeout(500);
console.log("after clicking ZÁPADNĚ:", (await page.textContent("aside .place")).trim());
console.log("keywords now:", (await page.locator("#screen .kw").allTextContents()).join(", "));
console.log("old block links left:", await page.locator("#screen span:not(:last-of-type) .kw").count());
const item = page.locator("#screen .kw.here").first();
if (await item.count()) {
  await item.click();
  await page.waitForSelector("#pop:not([hidden])");
  await page.screenshot({ path: "dist/keywords.png" });
  await page.locator("#pop button", { hasText: "vzít" }).click();
  await page.waitForTimeout(400);
  console.log("after 'vzít' from popover:", (await page.textContent("#screen")).trim().split("\n").slice(-2).join(" | "));
}
console.log("ERRORS:", errors.length ? errors : "none");
await b.close(); server.close();
