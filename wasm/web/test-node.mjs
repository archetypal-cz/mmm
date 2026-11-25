// Runs the *web* build in Node through the async console path.
// Usage: node test-node.mjs <input-file-cp852> [HHMMSS] [puvodni|cz]   -> CP852 output on stdout
import { readFileSync } from "node:fs";
import { decode, encode } from "./cp852.js";
import { startGame } from "./game.js";

const [file, time, lang] = process.argv.slice(2);
const lines = decode(readFileSync(file)).split("\n");
if (lines.at(-1) === "") lines.pop();
let i = 0;
const chunks = [];
await new Promise((resolve) => {
  startGame({
    fixedTime: time ? parseInt(time, 10) : undefined,
    lang: lang || undefined,
    onOutput: (t) => chunks.push(t),
    readLine: async () => (i < lines.length ? lines[i++] : null),
    onExit: resolve,
  }).catch((e) => {
    chunks.push(`\n<<ERROR ${e}>>`);
    resolve();
  });
});
process.stdout.write(Buffer.from(encode(chunks.join(""))));
