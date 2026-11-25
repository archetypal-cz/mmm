// Browser smoke test: serve wasm/, open the page in headless Chromium,
// play a few commands, save a screenshot.
//   docker run --rm -v "$PWD/wasm":/w -w /w mcr.microsoft.com/playwright:v1.56.0-noble \
//     bash -c "npm i --no-save playwright@1.56.0 >/dev/null && node web/smoke-browser.mjs"
import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join } from "node:path";
import { chromium } from "playwright";

const ROOT = new URL("..", import.meta.url).pathname;
const TYPES = { ".html": "text/html", ".js": "text/javascript", ".mjs": "text/javascript", ".wasm": "application/wasm" };
const server = createServer(async (req, res) => {
  try {
    const path = join(ROOT, decodeURIComponent(new URL(req.url, "http://x").pathname));
    const body = await readFile(path);
    res.writeHead(200, { "content-type": TYPES[extname(path)] ?? "application/octet-stream" });
    res.end(body);
  } catch { res.writeHead(404); res.end(); }
}).listen(8765);

const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1180, height: 820 } });
const errors = [];
page.on("pageerror", (e) => errors.push(String(e)));
page.on("console", (m) => m.type() === "error" && errors.push(m.text()));
await page.goto("http://localhost:8765/web/index.html?styl=moderni&lang=" + (process.env.LANG_MMM || "puvodni"));

const send = async (text) => {
  await page.waitForSelector("#cmd:not([disabled])");
  await page.fill("#cmd", text);
  await page.press("#cmd", "Enter");
  await page.waitForTimeout(150);
};
// clicks wait until the button is enabled (= game waits at its prompt)
const click = async (name, scope = "aside") => {
  await page.locator(scope).getByRole("button", { name, exact: true }).first().click();
  await page.waitForTimeout(150);
};
await send("Stanislav");
await click("Z");                  // exit west
await click("rezavou lopatku");    // item menu
await click("vzít");
console.log("PANEL:", (await page.textContent("aside")).replace(/\s+/g, " "));
if (await page.locator("aside").getByRole("button", { name: "lesník", exact: true }).count()) {
  await click("lesník");             // person menu
  await click("dát…");
  await page.screenshot({ path: "dist/smoke-menu.png" });
  await click("rezavou lopatku", "aside .menu");   // DÁT LOPATKU LESNÍKA
} else console.log("(lesník odešel)");
await send("SKÓRE");

// saved games: save here, walk away, load, check we are back; survive reload
const place = async () => (await page.textContent("aside .place")).trim();
const before = await place();
await page.fill("aside .savename", "TEST1");
await click("uložit");
await click("J");
const moved = await place();
await click("načíst");
await page.waitForTimeout(300);
const after = await place();
console.log(`SAVE/LOAD: before="${before}" moved="${moved}" after="${after}"`,
  before === after && moved !== before ? "OK" : "FAIL");
await page.screenshot({ path: "dist/smoke.png" });
const text = await page.textContent("#screen");
console.log(text.slice(-900));
await page.reload();
await send("Znovu");
await page.waitForTimeout(300);
console.log("AFTER RELOAD saves:", await page.locator("aside .saverow span").allTextContents());
console.log("ERRORS:", errors.length ? errors : "none");
await browser.close();
server.close();

// classic style + first-visit chooser
{
  const b2 = await chromium.launch();
  const p2 = await b2.newPage({ viewport: { width: 1180, height: 820 } });
  const srv = createServer(async (req, res) => {
    try {
      const path = join(ROOT, decodeURIComponent(new URL(req.url, "http://x").pathname));
      const body = await readFile(path);
      res.writeHead(200, { "content-type": TYPES[extname(path)] ?? "application/octet-stream" });
      res.end(body);
    } catch { res.writeHead(404); res.end(); }
  }).listen(8769);
  await p2.goto("http://localhost:8769/web/index.html");
  await p2.waitForSelector("#choose.show");
  await p2.screenshot({ path: "dist/choose.png" });
  await p2.click("#choose [data-style=classic]");
  await p2.waitForSelector("#cmd:not([disabled])");
  for (const c of ["Stanislav", "SEVER", "JÍT SEVERNĚ", "VOLE"]) {
    await p2.waitForSelector("#cmd:not([disabled])");
    await p2.fill("#cmd", c); await p2.press("#cmd", "Enter"); await p2.waitForTimeout(150);
  }
  await p2.waitForTimeout(300);
  console.log("CLASSIC: panel visible =", await p2.locator("aside").isVisible(), "map visible =", await p2.locator("#mapbox").isVisible());
  await p2.screenshot({ path: "dist/classic.png" });
  await b2.close(); srv.close();
}
