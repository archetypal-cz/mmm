// Icon generator (ASCII-art mansion in the game's terminal look, variant "d" is in use).
//   docker run --rm -v "$PWD/wasm/web":/t -v "$PWD/wasm/web":/out -v /c/Windows/Fonts:/fonts:ro \
//     mcr.microsoft.com/playwright:v1.56.0-noble bash -c "npm i --no-save playwright@1.56.0 >/dev/null && node /t/gen-icons.mjs d /out"
// Needs CascadiaMono.ttf (OFL) in /fonts.
// Render ASCII-art mansion icon variants in the game's terminal look.
// Text between « » is the lit (yellow) part, between ‹ › dim green.
//   node ascii.mjs preview            -> /t/ascii-preview.png (all variants)
//   node ascii.mjs <variant> <outdir> -> icon PNGs for that variant
import { chromium } from "playwright";
import { readFileSync } from "node:fs";

export const ART = {
  a: String.raw`
    |>        |>
   /^\  _/\_ /^\
  /___\/____\___\
  |‹[]›|‹[]›«[]»|‹[]›|
  |‹[]›| /\ |‹[]›|
__|___|_||_|___|__`,
  b: String.raw`
   /\    /\    /\
  /__\  /__\  /__\
  |‹[]›|__|‹[]›|__|«[]»|
  |‹[]›|‹[]›| __ |‹[]›|‹[]›|
__|__|__|_||_|__|__|__`,
  c: String.raw`
     ‹*›      .
   /\   _/\_   /\
  /__\_/____\_/__\
  |‹[]›| ‹[]›  «[]» |‹[]›|
  |‹[]›| ‹[]› /\ |‹[]›|
__|__|____||__|__|__`,
  d: String.raw`
  |>          |>
  /\    /\    /\
 /  \__/  \__/  \
 |‹[]›| ‹[]›  ‹[]› |«[]»|
 |‹[]›| ‹[]›  ‹[]› |‹[]›|
 |‹[]›|  _/\_  |‹[]›|
_|__|__|  |__|__|_`,
};

const esc = (s) => s.replace(/[&<>]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;" })[c]);
const html = (k) => esc(ART[k].replace(/^\n/, ""))
  .replace(/«(.*?)»/g, '<b class="y">$1</b>').replace(/‹(.*?)›/g, '<b class="d">$1</b>');
const font = readFileSync("/fonts/CascadiaMono.ttf").toString("base64");
const css = `@font-face{font-family:C;src:url(data:font/ttf;base64,${font})}
body{margin:0;background:#0d1110}
pre{margin:0;color:#b8f0c0;font-family:C,monospace;font-weight:700;line-height:1.08;
 text-shadow:0 0 .22em rgba(184,240,192,.6)}
.y{color:#f3e3a1;text-shadow:0 0 .35em #f3e3a1,0 0 .8em rgba(243,227,161,.5)}
.d{color:#6f9c77;text-shadow:none}
.tile{display:grid;place-items:center;background:#0d1110;
 background-image:repeating-linear-gradient(0deg,rgba(0,0,0,.25) 0 1px,transparent 1px 3px)}
.sheet{display:flex;gap:20px;padding:20px;background:#2a2a2a;flex-wrap:wrap;font:14px sans-serif;color:#ccc}`;

// fit the art into `inner` px: font size from the longest line
const tile = (k, size, inner) => {
  const lines = ART[k].replace(/^\n/, "").replace(/[«»‹›]/g, "").split("\n");
  const cols = Math.max(...lines.map((l) => l.length)), rows = lines.length;
  const px = Math.min(inner / (cols * 0.6), inner / (rows * 1.08));
  return `<div class="tile" style="width:${size}px;height:${size}px"><pre style="font-size:${px}px">${html(k)}</pre></div>`;
};

const b = await chromium.launch();
const p = await b.newPage();
const [mode, out] = process.argv.slice(2);
if (mode === "preview") {
  await p.setViewportSize({ width: 1200, height: 800 });
  await p.setContent(`<style>${css}</style><div class="sheet">` + Object.keys(ART).map((k) =>
    `<div>${tile(k, 256, 220)}<p>${k} · 256 px</p><div style="display:flex;gap:10px;align-items:end">${tile(k, 96, 84)}${tile(k, 48, 44)}</div></div>`).join("") + "</div>");
  await p.screenshot({ path: "/t/ascii-preview.png", fullPage: true });
} else {
  for (const [name, size, frac] of [["icon-192.png", 192, 0.88], ["icon-512.png", 512, 0.88],
       ["apple-touch-icon.png", 180, 0.84], ["icon-maskable-512.png", 512, 0.7], ["favicon-48.png", 48, 0.94]]) {
    await p.setViewportSize({ width: size, height: size });
    await p.setContent(`<style>${css}</style>${tile(mode, size, size * frac)}`);
    await p.screenshot({ path: `${out}/${name}` });
  }
}
await b.close();
