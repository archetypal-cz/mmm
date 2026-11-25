// OG image for the essay (1200x630 -> /out/og-esej.png) + header screenshot. Needs CascadiaMono.ttf and georgiab.ttf in /fonts (docker line: see gen-icons.mjs).
import { chromium } from "playwright";
import { readFileSync } from "node:fs";
import net from "node:net";
net.createServer((c) => { const u = net.connect(8765, "host.docker.internal"); c.pipe(u).pipe(c); u.on("error", () => c.destroy()); c.on("error", () => u.destroy()); }).listen(8765, "127.0.0.1");

const font = readFileSync("/fonts/CascadiaMono.ttf").toString("base64");
const serif = readFileSync("/fonts/georgiab.ttf").toString("base64");
const art = String.raw`  |>          |>
  /\          /\
 /  \        /  \
 |‹[]›|\      /|«[]»|
 |‹[]›| \    / |‹[]›|
 |‹[]›|  \__/  |‹[]›|
 |‹[]›| ‹[]›  ‹[]› |‹[]›|
 |‹[]›|  _/\_  |‹[]›|
_|__|__|  |__|__|_`;
const esc = (s) => s.replace(/[&<>]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;" })[c]);
const html = esc(art).replace(/«(.*?)»/g, '<b class="y">$1</b>').replace(/‹(.*?)›/g, '<b class="d">$1</b>');

const b = await chromium.launch();
const p = await b.newPage({ viewport: { width: 1200, height: 630 } });
await p.setContent(`<style>
@font-face{font-family:C;src:url(data:font/ttf;base64,${font})}
@font-face{font-family:G;src:url(data:font/ttf;base64,${serif})}
body{margin:0;width:1200px;height:630px;background:#0d1110;color:#b8f0c0;display:flex;align-items:center;gap:56px;padding:0 70px;box-sizing:border-box;
 background-image:repeating-linear-gradient(0deg,rgba(0,0,0,.22) 0 1px,transparent 1px 3px)}
pre{margin:0;font:700 31px/1.08 C;text-shadow:0 0 .22em rgba(184,240,192,.6)}
.y{color:#f3e3a1;text-shadow:0 0 .35em #f3e3a1,0 0 .8em rgba(243,227,161,.5)} .d{color:#6f9c77;text-shadow:none}
.t{display:flex;flex-direction:column;gap:22px}
.k{font:600 22px C;color:#6f9c77;letter-spacing:.08em;text-transform:uppercase}
h1{margin:0;font:700 62px/1.08 G;color:#f3e3a1}
.s{font:26px/1.4 G;font-style:italic;color:#9fc2a6}
.u{font:21px C;color:#8fd3ff}
</style>
<pre>${html}</pre>
<div class="t"><div class="k">Magical Mystery Mansion · esej</div>
<h1>Dvě písmena do jednoho slova</h1>
<div class="s">Jak se celý strašidelný zámek vešel do 64 kilobajtů a co to udělalo s mozky jeho programátorů</div>
<div class="u">&gt; HP 1000 · Fortran · 1982–1988_</div></div>`);
await p.screenshot({ path: "/out/og-esej.png" });

const q = await b.newPage({ viewport: { width: 1100, height: 760 } });
await q.goto("http://localhost:8765/web/esej.html");
await q.waitForTimeout(500);
await q.screenshot({ path: "/t/esej-head.png" });
await b.close(); process.exit(0);
