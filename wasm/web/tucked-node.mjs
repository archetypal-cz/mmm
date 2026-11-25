// Walk from the gate to the ballroom (14) and check the wedge stays out of
// "Vidíte" (state.tucked) until the game text names it.   node tucked-node.mjs
import { startGame } from "./game.js";
import { readState } from "./state.js";
import { writeSync } from "node:fs";
const log = (...a) => writeSync(1, a.map((x) => typeof x === "string" ? x : JSON.stringify(x)).join(" ") + "\n");
let out = "", step = 0, phase = 0;
const plan = ["PODÍVAT SE NA DŘEVO", "VZÍT KLÍN", "PODÍVAT SE NA DŘEVO"];
await new Promise((resolve) => startGame({
  fixedTime: 120000, lang: "puvodni",
  onOutput: (t) => (out += t),
  onExit: resolve,
  readLine: async (m) => {
    if (step++ === 0) return "Tester";
    if (step === 2) {   // test shortcut: put the player in the ballroom (ITST(1) = 14)
      new Int16Array(m.HEAPU8.buffer, m._mmm_common_mmbc(), 2048)[1776] = 14;
      new Int16Array(m.HEAPU8.buffer, m._mmm_common_mmbc(), 2048)[1261 + 30] = 21100;   // lit matches (IVEN(31))
      return "ROZHLÉDNOUT SE";
    }
    if (step > 80) { log("gave up", out.slice(-300)); process.exit(1); }
    const s = readState(m);
    if (s.room === 14) {
      if (phase === 0) log("ballroom: here", s.here.map((t) => t.word), "fixed", s.fixed.map((t) => t.word), "tucked", s.tucked.map((t) => t.word));
      else log(" >", plan[phase - 1], "->", out.trim().split("\n").slice(-3).join(" | "), "| tucked", s.tucked.map((t) => t.word), "inv", s.inventory.map((t) => t.word));
      out = "";
      if (phase >= plan.length) process.exit(0);
      return plan[phase++];
    }
    const prev = new Map([[s.room, null]]), q = [s.room];
    while (q.length) {
      const r = q.shift();
      for (const e of s.exitsOf(r)) if (!e.closed && !prev.has(e.target)) { prev.set(e.target, [r, e.dir]); q.push(e.target); }
    }
    if (!prev.has(14)) { log("no path from", s.room, out.slice(-300)); process.exit(1); }
    let t = 14; while (prev.get(t)[0] !== s.room) t = prev.get(t)[0];
    const d = prev.get(t)[1];
    out = "";
    const ex = s.exits.find((e) => e.dir === d); if (!ex) { log("no exit", d, "in", s.room, JSON.stringify(s.exits), JSON.stringify(s.exitsOf(s.room)), out.slice(-400)); process.exit(1); } return ex.cmd;
  },
}));
process.on("exit", (c) => log("EXIT", c, out.slice(-600)));
