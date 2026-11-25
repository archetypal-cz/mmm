// Runs the original MMM engine (Fortran -> f2c -> WebAssembly) with an
// asynchronous console: output arrives as decoded text, input is awaited.
// Saved games (the game's own ZAPSAT/OBNOVIT files) live in /saves in the
// virtual file system and are mirrored to localStorage.
import { decode, encode } from "./cp852.js";

// lang: "puvodni" = the Czech MMM (1982-88), "cz" = its new translation,
//       "en" = Bill Wolpert's original
const ENGINES = {
  puvodni: { load: () => import("../dist/mmm.js"), store: "mmm-saves-v1" },
  en: { load: () => import("../dist/mmm-en.js"), store: "mmm-saves-en-v1" },
  // the new Czech translation: same engine and save format as "puvodni"
  cz: { load: () => import("../dist/mmm-cz.js"), store: "mmm-saves-v1" },
};
let STORE = ENGINES.puvodni.store;
const SAVES = "/saves";

const b64 = {
  enc: (bytes) => btoa(String.fromCharCode(...bytes)),
  dec: (s) => Uint8Array.from(atob(s), (c) => c.charCodeAt(0)),
};
const loadStore = () => {
  try { return JSON.parse(globalThis.localStorage?.getItem(STORE) || "{}"); } catch { return {}; }
};

/**
 * @param {object} io
 * @param {(text: string) => void} io.onOutput   game text (may be a partial line)
 * @param {(mod: object) => Promise<string|null>} io.readLine  next command (null = quit)
 * @param {(status: number) => void} [io.onExit]
 * @param {number} [io.fixedTime]   HHMMSS: reproducible mystery (tests)
 * @param {string} [io.lang]   "puvodni" (Czech, default) or "en" (English original)
 * @returns {Promise<object>} the Emscripten module (for reading game state)
 */
export async function startGame({ onOutput, readLine, onExit, fixedTime, lang = "puvodni" }) {
  const engine = ENGINES[lang] || ENGINES.puvodni;
  STORE = engine.store;
  const createMMM = (await engine.load()).default;
  let pending = [];
  const flush = () => {
    if (pending.length) {
      onOutput(decode(pending));
      pending = [];
    }
  };
  const put = (ch) => {
    if (ch === null || ch === undefined) return;
    pending.push(ch & 255);
    if (ch === 10) flush();
  };
  let mod;
  mod = await createMMM({
    MMM_FIXED_TIME: fixedTime,
    mmmEncode: encode,
    mmmReadLine: async () => {
      flush(); // the prompt " >" has no newline
      syncSaves(mod);
      return readLine(mod);
    },
    preRun: [(m) => {
      m.FS.init(null, put, put);
      m.FS.mkdirTree(SAVES);
      for (const [name, data] of Object.entries(loadStore())) {
        try { m.FS.writeFile(`${SAVES}/${name}`, b64.dec(data)); } catch { /* skip bad entry */ }
      }
      m.FS.chdir(SAVES);
    }],
    print: () => {},
    printErr: () => {},
    onExit: (status) => {
      flush();
      syncSaves(mod);
      onExit?.(status);
    },
  });
  return mod;
}

// ---- saved games ------------------------------------------------------
export function listSaves(mod) {
  return mod.FS.readdir(SAVES).filter((n) => n !== "." && n !== "..").sort();
}

/** Mirror /saves to localStorage (called at every prompt). */
export function syncSaves(mod) {
  if (!mod || !globalThis.localStorage) return;
  const store = {};
  for (const n of listSaves(mod)) store[n] = b64.enc(mod.FS.readFile(`${SAVES}/${n}`));
  try { localStorage.setItem(STORE, JSON.stringify(store)); } catch { /* quota */ }
}

export const readSave = (mod, name) => mod.FS.readFile(`${SAVES}/${name}`);
export function writeSave(mod, name, bytes) { mod.FS.writeFile(`${SAVES}/${name}`, bytes); syncSaves(mod); }
export function deleteSave(mod, name) { mod.FS.unlink(`${SAVES}/${name}`); syncSaves(mod); }

/** Save-file names: the game reads them as text; keep them plain ASCII. */
export const saveName = (s) => s.normalize("NFD").replace(/[^\w-]/g, "").slice(0, 24).toUpperCase();
