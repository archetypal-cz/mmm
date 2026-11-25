// Checks that every noun form the UI sends is in the game's vocabulary,
// by asking the native build "PODÍVAT SE NA <form>" for each.
import { execFileSync } from "node:child_process";
import { ITEM_WORDS } from "./data.js";
import { accusative } from "./state.js";
import { encode, decode } from "./cp852.js";
const NPCS = ["MRTVOLA","PANÍ","SLUHA","KRTEK","VAMPÍR","LESNÍK","SADAŘ","SLUŽKA","KUCHAŘ","VLKODLAK","SKŘÍTEK","LOVEC","SKŘET","PÁN","VLK","RYTÍŘ"];
const forms = [...new Set([...ITEM_WORDS.slice(0, 107).filter(Boolean), ...NPCS].map(accusative))];
const input = ["T", ...forms.map((f) => `PODÍVAT SE NA ${f}`), "KONEC", "ANO"].join("\n") + "\n";
const out = decode(execFileSync("../native/build/mmm", { input: Buffer.from(encode(input)), env: { MMM_FIXED_TIME: "101010" } }));
const answers = out.split(" >").slice(1, forms.length + 1);
let bad = 0;
forms.forEach((f, i) => { if (/neobsahuje slovo/.test(answers[i] ?? "")) { bad++; console.log("UNKNOWN:", f); } });
console.log(`${forms.length - bad}/${forms.length} forms understood`);
