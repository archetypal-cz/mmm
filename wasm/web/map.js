// Map of the mansion, revealed as the player explores. Room positions come
// from LAYOUT, precomputed from the game's whole exit table (gen_layout.py),
// so loops close and floors stay apart; only the current floor is drawn.
// Visited rooms are remembered per mystery number in localStorage.
import { LAYOUT, MAZE } from "./layout.js";

const DELTA = { 1: [0, -1], 2: [1, 0], 3: [0, 1], 4: [-1, 0], 5: [1, -1], 6: [-1, 1] };
// spacing works both ways: turned by 90° the vertical spacing must still fit a box width
const W = 128, H = 50, GX = 166, GY = 142;     // box size and grid spacing (px)
const NS = "http://www.w3.org/2000/svg";

export class AutoMap {
  constructor() { this.key = null; this.rooms = {}; }

  /** Switch to the map of a mystery (number from the game). */
  use(mystery) {
    const key = `mmm-map-v1-${mystery}`;
    if (key === this.key) return;
    this.key = key;
    try { this.rooms = JSON.parse(localStorage.getItem(key) || "{}"); } catch { this.rooms = {}; }
    for (const [r, p] of Object.entries(this.rooms)) place(+r, p);   // older saves: fixed layout
  }

  save() {
    if (this.revealAll) return;         // the ?mapa=vse preview must not stick to the real game
    try { localStorage.setItem(this.key, JSON.stringify(this.rooms)); } catch { /* quota */ }
  }

  /** Record the current room (call at every prompt). */
  visit(state) {
    this.use(state.mystery);
    if (this.revealAll) {               // debug/preview (?mapa=vse): the whole precomputed map
      for (const r of Object.keys(LAYOUT))
        if (!this.rooms[r]) this.rooms[r] = place(+r, { lit: true, name: state.nameOf(+r) });
    }
    const r = state.room;
    const lit = !!state.lit;           // a room seen only in the dark has no name yet
    if (this.rooms[r]) {
      const p = this.rooms[r];
      p.lit = p.lit || lit;
      if (p.lit) p.name = state.nameOf(r);
      this.save();
      return;
    }
    if (LAYOUT[r]) {                    // the precomputed map knows where it is
      this.rooms[r] = place(r, { lit, name: lit ? state.nameOf(r) : "" });
      this.save();
      return;
    }
    const taken = new Set(Object.values(this.rooms).map((p) => `${p.x},${p.y}`));
    let pos = null;
    // place next to a visited room that has an exit leading here
    for (const [id, p] of Object.entries(this.rooms)) {
      const e = state.exitsOf(+id).find((x) => x.target === r);
      if (e) { pos = [p.x + DELTA[e.dir][0], p.y + DELTA[e.dir][1]]; break; }
    }
    // or opposite to an exit from here into a visited room
    if (!pos) for (const e of state.exitsOf(r)) {
      const p = this.rooms[e.target];
      if (p) { pos = [p.x - DELTA[e.dir][0], p.y - DELTA[e.dir][1]]; break; }
    }
    // unconnected (magic, falls, start): a new area to the right
    if (!pos) {
      const xs = Object.values(this.rooms).map((p) => p.x);
      pos = [xs.length ? Math.max(...xs) + 3 : 0, 0];
    }
    this.rooms[r] = { ...nearestFree(pos, taken), lit, name: lit ? state.nameOf(r) : "" };
    this.save();
  }

  /**
   * Shortest known way from the current room to `target` through visited
   * rooms and open exits, as game commands. Directions are given the way the
   * game will accept them at each step: relative (vpřed/vpravo/vzad/vlevo)
   * where a compass is needed but not carried, tracking the facing.
   * @returns {{cmd: string, expect: number}[] | null}
   */
  route(state, target) {
    if (state.room === target) return null;
    const hops = this.hops(state, target);
    return hops ? this.commands(state, hops) : null;
  }

  /**
   * Way into an unexplored exit ("?"): walk to `room` (if needed), then take
   * the exit. Null for locked doors or rooms that cannot be reached.
   */
  routeOut(state, room, exit) {
    if (exit.closed) return null;
    const hops = this.hops(state, room);
    if (!hops) return null;
    return this.commands(state, [...hops, { from: room, dir: exit.dir, to: exit.target }]);
  }

  /** BFS through visited rooms and open exits: [{from, dir, to}], [] if already there. */
  hops(state, target) {
    const start = state.room;
    if (start === target) return [];
    if (!this.rooms[target]) return null;
    const prev = new Map([[start, null]]);
    const queue = [start];
    while (queue.length && !prev.has(target)) {
      const r = queue.shift();
      for (const e of state.exitsOf(r)) {
        if (e.closed || !this.rooms[e.target] || prev.has(e.target)) continue;
        prev.set(e.target, { from: r, dir: e.dir });
        queue.push(e.target);
      }
    }
    if (!prev.has(target)) return null;
    const hops = [];
    for (let r = target; prev.get(r); r = prev.get(r).from) hops.unshift({ ...prev.get(r), to: r });
    return hops;
  }

  /** Hops -> game commands, relative where a compass is needed, tracking the facing. */
  commands(state, hops) {
    const DIRS = ["SEVERNĚ", "VÝCHODNĚ", "JIŽNĚ", "ZÁPADNĚ", "NAHORU", "DOLŮ"];
    const REL = ["VPŘED", "VPRAVO", "VZAD", "VLEVO"];
    const needsCompass = (r) => ((r < 54 && r !== 35) || r === 98) && !state.hasCompass;
    let f = state.facing;
    return hops.map(({ from, dir, to }) => {
      let word = DIRS[dir - 1];
      if (dir <= 4 && needsCompass(from)) word = REL[((dir - f) % 4 + 4) % 4];
      if (dir <= 4) f = dir;
      return { cmd: `JÍT ${word}`, expect: to };
    });
  }

  /** Draw into an <svg>; onGo(steps) is called when a reachable room is clicked. */
  render(svg, state, onGo) {
    svg.replaceChildren();
    const floor = this.rooms[state.room]?.z ?? 0;
    this.floor = floor;
    const ids = Object.keys(this.rooms).map(Number).filter((r) => (this.rooms[r].z ?? 0) === floor);
    if (!ids.length) return;
    const P = (r) => ((this.rooms[r]?.z ?? 0) === floor ? this.rooms[r] : undefined);
    // Camera: the current room stays in the middle and the map slides under it.
    // Without a compass the player only knows ahead/left/right/back, so the map
    // turns (smoothly, around the player) so that the facing direction is up;
    // labels counter-rotate and stay level. "Neotáčet" switches this off.
    const cx = (p) => p.x * GX, cy = (p) => p.y * GY;
    const cur = P(state.room) || P(ids[0]);
    const full = !!this.revealAll;                                   // ?mapa=vse: whole floor, scrollable
    const turn = !full && state.compassNeeded && this.rotate !== false;
    const base = turn ? -(state.facing - 1) * 90 : 0;               // 1 S, 2 V, 3 J, 4 Z
    const prevAngle = this.angle ?? base;
    const angle = prevAngle + ((((base - prevAngle) % 360) + 540) % 360 - 180);
    this.angle = angle;
    svg.dataset.up = turn ? "vpřed" : "sever";
    const N = [cx(cur), cy(cur)];
    const host = svg.parentElement;
    const box = host.getBoundingClientRect();
    const aspect = box.width > 0 && box.height > 0 ? box.width / box.height : 1.6;
    if (full) {
      // the whole floor at a readable scale; the map box scrolls
      let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
      for (const r of ids) {
        const p = P(r);
        x0 = Math.min(x0, cx(p) - W); x1 = Math.max(x1, cx(p) + W);
        y0 = Math.min(y0, cy(p) - H - 30); y1 = Math.max(y1, cy(p) + H + 30);
      }
      const vw = x1 - x0, vh = y1 - y0, k = 0.85;
      svg.setAttribute("viewBox", `${x0} ${y0} ${vw} ${vh}`);
      svg.style.width = `${Math.max(vw * k, box.width)}px`; svg.style.height = `${Math.max(vh * k, box.height)}px`;
      requestAnimationFrame(() => {                                   // keep the player in view
        const sc = Math.min(svg.clientWidth / vw, svg.clientHeight / vh);
        const ox = (svg.clientWidth - vw * sc) / 2, oy = (svg.clientHeight - vh * sc) / 2;
        host.scrollTo({ left: ox + (N[0] - x0) * sc - host.clientWidth / 2,
                        top: oy + (N[1] - y0) * sc - host.clientHeight / 2, behavior: "smooth" });
      });
    } else {
      // close up on the room; the view widens just enough that the rooms you can
      // walk to from here stay whole (screen axes, so after the turn), up to a limit
      const reach = 0.96 * Math.max(GX, GY);
      let needW = 0, needH = 0;
      const a90 = ((Math.round(angle / 90) % 4) + 4) % 2 === 1;
      // rooms you can walk to from here, and visited rooms whose exits lead here
      const around = new Set(state.exits.map((e) => e.target));
      for (const r of ids) if (state.exitsOf(r).some((e) => e.target === state.room)) around.add(r);
      for (const r of around) {
        const q = P(r);
        if (!q || r === state.room) continue;
        let dx = Math.abs(cx(q) - N[0]) + W / 2 + 18, dy = Math.abs(cy(q) - N[1]) + H / 2 + 18;
        if (a90) [dx, dy] = [dy, dx];
        needW = Math.max(needW, Math.min(dx, 3.2 * GX)); needH = Math.max(needH, Math.min(dy, 3.2 * GX));
      }
      const halfH = Math.max(reach, reach / aspect, needH, needW / aspect), halfW = halfH * aspect;
      // zoom changes glide (the centre is always the player; the world slides below)
      const from = this.half || [halfW, halfH];
      this.half = [halfW, halfH];
      const setVB = (hw, hh) => svg.setAttribute("viewBox", `${N[0] - hw} ${N[1] - hh} ${2 * hw} ${2 * hh}`);
      cancelAnimationFrame(this.zoomRaf);
      const t0 = performance.now(), dur = 600;
      const step = (now) => {
        const k = Math.min(1, (now - t0) / dur), e = 1 - (1 - k) ** 3;
        setVB(from[0] + (halfW - from[0]) * e, from[1] + (halfH - from[1]) * e);
        if (k < 1) this.zoomRaf = requestAnimationFrame(step);
      };
      if (Math.abs(from[1] - halfH) > 1 && !matchMedia("(prefers-reduced-motion: reduce)").matches) step(t0);
      else setVB(halfW, halfH);
      svg.style.width = ""; svg.style.height = "";
    }

    const world = mk("g", { class: "world" });
    const edges = mk("g", { class: "edges" }), nodes = mk("g", {});
    // mazes: a hatched area under the visited rooms of each maze, with its name
    const defs = mk("defs", {});
    const pat = mk("pattern", { id: "hatch", width: 10, height: 10, patternUnits: "userSpaceOnUse",
      patternTransform: "rotate(45)" });
    pat.append(mk("line", { x1: 0, y1: 0, x2: 0, y2: 10, class: "hatchline" }));
    defs.append(pat);
    const arrow = mk("marker", { id: "oneway", viewBox: "0 0 10 10", refX: 9, refY: 5,
      markerWidth: 16, markerHeight: 16, orient: "auto", markerUnits: "userSpaceOnUse" });
    arrow.append(mk("path", { d: "M0 1 L10 5 L0 9 z", class: "arrowhead" }));
    defs.append(arrow);
    const mazes = mk("g", { class: "mazes" });
    world.append(mazes, edges, nodes);
    svg.append(defs, world);
    const level = [];                                   // elements that must stay upright
    const upright = (el) => { el.classList.add("upright"); level.push(el); return el; };
    const groups = {};
    for (const r of ids) if (MAZE[r]) (groups[MAZE[r]] ||= []).push(r);
    for (const [label, rs] of Object.entries(groups)) {
      // a hatched halo around each maze room; neighbours' halos merge into one shape
      for (const r of rs) {
        mazes.append(mk("rect", { x: cx(P(r)) - W / 2 - 21, y: cy(P(r)) - H / 2 - 22,
          width: W + 42, height: H + 44, rx: 16, class: "maze" }));
      }
      const top = rs.reduce((a, r) => (cy(P(r)) < cy(P(a)) || (cy(P(r)) === cy(P(a)) && cx(P(r)) < cx(P(a))) ? r : a));
      mazes.append(upright(mk("text", { x: cx(P(top)) - W / 2 - 14, y: cy(P(top)) - H / 2 - 8, class: "mazelabel" }, label)));
    }
    const drawn = new Set();
    const pathOf = {};                                  // "a-b" -> SVG path, for the trail
    // Where a passage meets a room box: the side its direction points to.
    const OPP = { 1: 3, 2: 4, 3: 1, 4: 2 };
    const port = (pt, d) => [cx(pt) + DELTA[d][0] * W / 2, cy(pt) + DELTA[d][1] * H / 2];
    const f = (n) => Math.round(n * 10) / 10;
    // The game's passages bend: north from A may enter B from its west side.
    // A passage leaves A on the side of its direction and enters B on the side
    // of B's way back (or opposite to the travel direction if it is one-way),
    // as a cubic curve; consistent passages come out straight.
    const passage = (r, e, p, q) => {
      if (e.target === r) {                             // leads back into the same room: a loop
        const [dx, dy] = DELTA[e.dir], A = port(p, e.dir), L = 70, S = 38;
        return `M${f(A[0] - dy * 8)} ${f(A[1] + dx * 8)} C${f(A[0] + dx * L - dy * S)} ${f(A[1] + dy * L + dx * S)} ` +
               `${f(A[0] + dx * L + dy * S)} ${f(A[1] + dy * L - dx * S)} ${f(A[0] + dy * 8)} ${f(A[1] - dx * 8)}`;
      }
      const back = state.exitsOf(e.target).find((x) => x.target === r && x.dir <= 4);
      const d2 = back ? back.dir : OPP[e.dir];
      const A = port(p, e.dir), B = port(q, d2);
      const k = Math.min(Math.max(Math.hypot(B[0] - A[0], B[1] - A[1]) * 0.45, 28), 130);
      return `M${f(A[0])} ${f(A[1])} C${f(A[0] + DELTA[e.dir][0] * k)} ${f(A[1] + DELTA[e.dir][1] * k)} ` +
             `${f(B[0] + DELTA[d2][0] * k)} ${f(B[1] + DELTA[d2][1] * k)} ${f(B[0])} ${f(B[1])}`;
    };
    const adjacent = new Set(state.exits.filter((e) => !e.closed).map((e) => e.target));

    for (const r of ids) {
      const p = P(r);
      for (const e of state.exitsOf(r)) {
        const q = P(e.target);
        const vertical = e.dir >= 5;
        if (q) {
          const k = e.target === r ? `${r}@${e.dir}` : [r, e.target].sort().join("-");
          if (drawn.has(k)) continue;
          drawn.add(k);
          if (vertical) {                               // stairs within one floor plan: straight, dashed
            edges.append(mk("line", { x1: cx(p), y1: cy(p), x2: cx(q), y2: cy(q),
              class: `edge vertical${e.closed ? " closed" : ""}` }));
            continue;
          }
          const oneway = e.target !== r && !state.exitsOf(e.target).some((x) => x.target === r);
          const d = passage(r, e, p, q);
          pathOf[k] = d;
          const el = mk("path", { d, class: `edge${e.closed ? " closed" : ""}${oneway ? " oneway" : ""}` });
          if (oneway) el.setAttribute("marker-end", "url(#oneway)");
          if (oneway || e.target === r) el.append(mk("title", {}, oneway ? "jednosměrná cesta" : "vede zpátky sem"));
          edges.append(el);
        } else {                                // unexplored: a stub with "?"
          const [dx, dy] = DELTA[e.dir];
          const ex = cx(p) + dx * GX * 0.62, ey = cy(p) + dy * GY * 0.62;
          edges.append(mk("line", { x1: cx(p), y1: cy(p), x2: ex, y2: ey,
            class: `edge stub${vertical ? " vertical" : ""}${e.closed ? " closed" : ""}` }));
          const steps = this.routeOut(state, r, e);
          const q = mk("g", { class: `stubq${steps ? " go" : ""}`,
            transform: `translate(${ex + dx * 10} ${ey + dy * 10})` });
          // a known room on another floor shows as ↑/↓, an unknown one as ?
          const known = this.rooms[e.target];
          const label = known ? (e.dir === 5 ? "↑" : e.dir === 6 ? "↓" : "⇄") : "?";
          const what = known ? (known.name || "tmavá místnost") : "neprozkoumáno";
          q.append(mk("circle", { r: 11 }), upright(mk("text", { y: 5, class: "q" }, label)));
          q.append(mk("title", {}, e.closed ? "zamčeno" : steps
            ? `${what} · ${steps.length === 1 ? "jít tam" : `dojít tam (${steps.length} kroků)`}`
            : what));
          if (steps) q.addEventListener("click", () => onGo(steps));
          edges.append(q);
        }
      }
    }

    for (const r of ids) {
      const p = P(r), here = r === state.room;
      const steps = here ? null : this.route(state, r);
      const cls = steps ? (adjacent.has(r) && steps.length === 1 ? " go" : " far") : "";
      const g = mk("g", { class: `room${here ? " here" : ""}${cls}${p.name ? "" : " dark"}`,
        transform: `translate(${cx(p) - W / 2} ${cy(p) - H / 2})` });
      const body = upright(mk("g", {}));
      body.append(mk("rect", { width: W, height: H, rx: 7 }));
      const [l1, l2] = wrap(p.name || "(tma)", 17);
      body.append(mk("text", { x: W / 2, y: l2 ? 21 : 29 }, l1));
      if (l2) body.append(mk("text", { x: W / 2, y: 37 }, l2));
      const ups = state.exitsOf(r).filter((e) => e.dir >= 5).map((e) => (e.dir === 5 ? "↑" : "↓")).join("");
      if (ups) body.append(mk("text", { x: W - 9, y: 14, class: "updown" }, ups));
      g.append(body, mk("title", {}, (p.name || "tmavá místnost") +
        (steps ? ` · ${steps.length === 1 ? "jít sem" : `dojít sem (${steps.length} kroků)`}` : "")));
      if (steps) g.addEventListener("click", () => onGo(steps));
      nodes.append(g);
    }

    // the "you are here" frame flows from the previous room to the current one
    // (lighting up the corridor) while the camera follows and the map turns
    const last = this.lastRoom !== state.room && P(this.lastRoom) ? P(this.lastRoom) : null;
    const O = last ? [cx(last), cy(last)] : (this.lastCenter && this.lastFloor === floor ? this.lastCenter : N);
    if (last) {
      const d = pathOf[[this.lastRoom, state.room].sort().join("-")];
      edges.append(d ? mk("path", { d, class: "trail" })
                     : mk("line", { x1: O[0], y1: O[1], x2: N[0], y2: N[1], class: "trail" }));
    }
    const hl = mk("rect", { class: "hl", x: -W / 2 - 5, y: -H / 2 - 5, width: W + 10, height: H + 10, rx: 10 });
    nodes.append(hl);
    // start pose = how the previous frame looked: rotated by prevAngle around O
    const rad = (prevAngle * Math.PI) / 180;
    const t = full ? [0, 0] :                          // whole-floor view: the map stays put
              [Math.cos(rad) * (N[0] - O[0]) - Math.sin(rad) * (N[1] - O[1]),
               Math.sin(rad) * (N[0] - O[0]) + Math.cos(rad) * (N[1] - O[1])];
    world.style.transformOrigin = `${N[0]}px ${N[1]}px`;
    const pose = (tx, ty, a, hx, hy) => {
      world.style.transform = `translate(${tx}px, ${ty}px) rotate(${a}deg)`;
      for (const el of level) el.style.transform = `rotate(${-a}deg)`;
      hl.style.transform = `translate(${hx}px, ${hy}px) rotate(${-a}deg)`;
    };
    const moved = Math.abs(t[0]) + Math.abs(t[1]) > 0.5 || prevAngle !== angle || !!last;
    svg.classList.remove("moving");
    pose(t[0], t[1], prevAngle, last ? O[0] : N[0], last ? O[1] : N[1]);
    if (moved) requestAnimationFrame(() => requestAnimationFrame(() => {
      svg.classList.add("moving");
      pose(0, 0, angle, N[0], N[1]);
    }));
    this.lastRoom = state.room;
    this.lastCenter = N;
    this.lastFloor = floor;
  }
}

function place(r, p) {
  const l = LAYOUT[r];
  if (l) { p.x = l[0]; p.y = l[1]; p.z = l[2]; }
  return p;
}

function nearestFree([x, y], taken) {
  for (let rad = 0; rad < 50; rad++)
    for (let dx = -rad; dx <= rad; dx++)
      for (let dy = -rad; dy <= rad; dy++) {
        if (Math.max(Math.abs(dx), Math.abs(dy)) !== rad) continue;
        if (!taken.has(`${x + dx},${y + dy}`)) return { x: x + dx, y: y + dy };
      }
  return { x, y };
}

function wrap(s, n) {
  s = s.trim();
  if (s.length <= n) return [s, ""];
  const cut = s.lastIndexOf(" ", n);
  const i = cut > 3 ? cut : n;
  const rest = s.slice(i).trim();
  return [s.slice(0, i), rest.length > n ? rest.slice(0, n - 1) + "…" : rest];
}

function mk(tag, attrs, text) {
  const e = document.createElementNS(NS, tag);
  for (const [k, v] of Object.entries(attrs)) e.setAttribute(k, v);
  if (text != null) e.textContent = text;
  return e;
}
