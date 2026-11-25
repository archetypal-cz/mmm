// Reads the game state straight from the Fortran COMMON /MMBC/ in WASM
// memory and derives what the player can know, using the game's own rules
// (MMRI item listing, MMRL light check, MMSB "Je zde s Vámi", MMSC exits).
import { decode } from "./cp852.js";
import { ITEM_WORDS, ITEM_SHOWN, SCENERY } from "./data.js";

// COMMON /MMBC/ layout (INTEGER*2 units), in MMCO.FOR order; packed.
const SIZES = [
  ["IWRD", 81], ["IVRB", 356], ["IRNM", 528], ["IROM", 99], ["IRSN", 68],
  ["IRES", 17], ["IPRP", 68], ["IDTN", 44], ["IVEN", 107], ["IXT", 360],
  ["ICLS", 48], ["ITST", 40], ["NAME", 3], ["NTAG", 10], ["IWP", 4],
];
const OFF = {};
{ let o = 0; for (const [k, n] of SIZES) { OFF[k] = o; o += n; } }

// digit helpers from the Fortran statement functions
const IFSD = (i) => i % 100;                       // last two digits (room)
const ITFD = (i) => Math.trunc(i / 100) % 100;     // digits 3-4 (exit target)
const IFD = (i) => Math.trunc(i / 1000) % 10;      // thousands digit (state)

export const DIRS = ["SEVERNĚ", "VÝCHODNĚ", "JIŽNĚ", "ZÁPADNĚ", "NAHORU", "DOLŮ"];
const DIR_SHORT = ["S", "V", "J", "Z", "nahoru", "dolů"];

function reader(mod) {
  const base = mod._mmm_common_mmbc();
  const h16 = new Int16Array(mod.HEAPU8.buffer, base, 2048);
  const at = (arr, ...idx) => {          // Fortran 1-based, column-major
    const dims = { IWRD: [9], IVRB: [4], IRNM: [8], IRSN: [4], IPRP: [4], IDTN: [4], ICLS: [4] }[arr] || [];
    let lin = idx[0] - 1, mul = 1;
    for (let d = 0; d < dims.length; d++) { mul *= dims[d]; lin += (idx[d + 1] - 1) * mul; }
    return h16[OFF[arr] + lin];
  };
  const text = (arr, col, words) => {    // words*2 bytes of text in column col
    const start = base + 2 * (OFF[arr] + (col - 1) * words);
    return decode(mod.HEAPU8.subarray(start, start + 2 * words)).trimEnd();
  };
  return { at, text };
}

// Czech object form for commands ("VZÍT LOPATKU"): the game echoes the word
// the player typed, and IWORD (B. Michal, 1982) accepts declined forms.
const ACC = {
  OLEJNIČK: "OLEJNIČKU", PAPOUŠEK: "PAPOUŠKA", TRPASLÍK: "TRPASLÍKA", KRÁL: "KRÁLE",
  SPOJAŘ: "SPOJAŘE", HASIČ: "HASIČE", BÝK: "BÝKA", OPICE: "OPICI",
  PANÍ: "PANÍ", SLUHA: "SLUHU", KRTEK: "KRTKA", VAMPÍR: "VAMPÍRA", LESNÍK: "LESNÍKA",
  SADAŘ: "SADAŘE", SLUŽKA: "SLUŽKU", KUCHAŘ: "KUCHAŘE", VLKODLAK: "VLKODLAKA",
  SKŘÍTEK: "SKŘÍTKA", LOVEC: "LOVCE", SKŘET: "SKŘETA", PÁN: "PÁNA", VLK: "VLKA",
  RYTÍŘ: "RYTÍŘE", MRTVOLA: "MRTVOLU",
};
export const accusative = (w) => ACC[w] ?? (w.endsWith("A") ? w.slice(0, -1) + "U" : w);

/** Snapshot of everything the UI may show. Call only while the game waits for input. */
export function readState(mod) {
  const { at, text } = reader(mod);
  const ITST = (i) => at("ITST", i);
  const IVEN = (i) => at("IVEN", i);
  const IRES = (i) => at("IRES", i);
  const IROM = (i) => at("IROM", i);
  const IXT = (i) => at("IXT", i);

  const ir = ITST(1);
  const turn = ITST(5);
  const facing = Math.trunc(ITST(4) / 1000);          // 1..4 = S,V,J,Z

  // room name with the game's preposition (MMPRP): "v lese", "na cestě", "u hlavní brány"
  const nameOf = (r) => {
    const n = IFSD(IROM(r));
    if (n <= 0) return `místnost ${r}`;
    let prep = 1;
    if ([4, 18, 31, 35, 38, 41, 45, 47, 49, 53].includes(n)) prep = 2;
    if ([32, 33, 36, 37, 42, 48, 50].includes(n)) prep = 3;
    if ((n >= 6 && n <= 9) || [11, 20, 22, 39].includes(n)) prep = 14;
    return `${text("IPRP", prep, 4)} ${text("IRNM", n, 8)}`;
  };
  const roomName = nameOf(ir);

  // MMRL: is there light?
  const lit = (() => {
    for (const k of [4, 19, 27]) {
      const v = IVEN(k);
      if (IFD(v) === 1 && ((v < 10000 && IFSD(v) === ir) || v > 20000 ||
          (v > 10000 && v < 20000 && IFSD(IRES(IFSD(v))) === ir))) return true;
    }
    if (ir === 10 && IVEN(76) > 1000) return true;
    if (ir === 14 && IVEN(89) > 1000) return true;
    if (IVEN(31) > 21000) return true;
    if (ir > 53) return turn < 300;
    if (IROM(ir) > 20000) return turn < 300;
    return false;
  })();

  const item = (j) => {
    const word = ITEM_WORDS[j - 1];
    return { id: j, word, noun: accusative(word), name: (ITEM_SHOWN[j - 1] || word.toLowerCase()) };
  };

  // MMRI(1) "preskok veci popsanych v popisu mista": items the game leaves out
  // of "Vidíte" while they sit in their starting spot, because the room text
  // hides them (the wedge in the woodpile, the lantern on the hook, maze treasure)
  const tuckedAway = (j, v) =>
    (j === 4 && (v === 493 || v === 1493)) || (j === 5 && v === 1316) ||
    (j === 17 && v === 1141) || (j === 19 && ir === 16) || (j === 29 && v === 1214) ||
    (j === 30 && ir === 25) || (j === 37 && v === 1153) || (j === 39 && v === 1253) ||
    (j === 40 && v === 1151) || (j === 42 && v === 1151) || (j === 46 && v === 1101) ||
    (j === 28 && v === 1184);

  // MMRI(1): portable items lying here (1..53); fixed objects (54..107) placed here.
  // Tucked-away items are not listed; they only become clickable words once
  // some game text actually names them (e.g. after PODÍVAT SE NA DŘEVO).
  const here = [], fixed = [], inventory = [], tucked = [];
  for (let j = 1; j <= 107; j++) {
    const v = IVEN(j);
    if (!ITEM_WORDS[j - 1]) continue;
    if (j <= 53 && v > 20000) { if (j !== 53) inventory.push(item(j)); continue; }
    if (v > 10000 || v <= 0 || IFSD(v) !== ir) continue;
    if (j <= 53 && tuckedAway(j, v)) { tucked.push(item(j)); continue; }
    (j <= 53 ? here : fixed).push(item(j));
  }

  // scenery the verb handlers know in this room although IVEN does not place
  // it (BRÁNA at the gate, DVEŘE, ZÁCLONA, ZEĎ...; wasm/scenery.json). Like the
  // tucked items it is never listed, only clickable where the text names it.
  // ZÁMEK is the chest's lock: wherever the chest is.
  const scenery = (SCENERY[ir] || []).filter((j) => !fixed.some((f) => f.id === j)).map(item);
  const chest = IVEN(35);
  if (chest > 20000 || (chest < 10000 && IFSD(chest) === ir)) scenery.push(item(54));

  // MMSB: "Je zde s Vámi" (mole and wolf stay hidden, like in the game)
  const people = [];
  for (let i = 1; i <= 17; i++) {
    const v = IRES(i);
    if (i === 4 || i === 15 || IFSD(v) !== ir || v === 0) continue;
    if (v > 10000 && i !== 1) continue;
    const word = text("IRSN", i, 4);
    if (word) people.push({ id: i, word, noun: accusative(word), dead: i === 1 });
  }

  // MMSC: exits; indoors without a compass only relative directions work
  const hasCompass = IVEN(6) >= 20000;
  const compassNeeded = ((ir < 54 && ir !== 35) || ir === 98) && !hasCompass;
  const exits = [];
  for (let d = 1; d <= 6; d++) {
    for (let j = d * 60 - 59; j <= d * 60; j++) {
      const v = IXT(j);
      if (IFSD(v) !== ir) continue;
      let cmd = `JÍT ${DIRS[d - 1]}`, label = DIR_SHORT[d - 1];
      if (d <= 4 && compassNeeded) {
        const rel = ((d - facing) % 4 + 4) % 4;      // 0 ahead, 1 right, 2 back, 3 left
        [label, cmd] = [["vpřed", "JÍT VPŘED"], ["vpravo", "JÍT VPRAVO"],
                        ["vzad", "JÍT VZAD"], ["vlevo", "JÍT VLEVO"]][rel];
      }
      exits.push({ dir: d, label, cmd, target: ITFD(v), closed: v > 20000 });
      break;
    }
  }

  // full verb list; slots 4-11 hold this game's secret magic words: skip them
  const verbs = [];
  for (let k = 1; k <= 89; k++) {
    if (k >= 4 && k <= 11) continue;
    const v = text("IVRB", k, 4);
    if (v && !verbs.includes(v)) verbs.push(v);
  }

  // for the map: exits (cardinal) and names of any room
  const exitsOf = (r) => {
    const out = [];
    for (let d = 1; d <= 6; d++)
      for (let j = d * 60 - 59; j <= d * 60; j++) {
        const v = IXT(j);
        if (IFSD(v) === r) { out.push({ dir: d, target: ITFD(v), closed: v > 20000 }); break; }
      }
    return out;
  };

  return {
    room: ir, roomName, lit, turn, facing, compassNeeded, hasCompass, verbs, exitsOf, nameOf,
    mystery: ITST(23),
    here: lit ? here : [], fixed: lit ? fixed : [], tucked: lit ? tucked : [], scenery: lit ? scenery : [],
    people: lit ? people : [],
    inventory, exits,
    raw: { ITST: Array.from({ length: 40 }, (_, i) => ITST(i + 1)) },
  };
}
