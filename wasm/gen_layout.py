#!/usr/bin/env python3
"""Precompute the whole map of the mansion from the game's exit table, so the
web UI only has to *reveal* rooms as the player visits them.

Edges: the initial IXT table (MMM.FOR, DATA IXT0..IXT3) plus every constant
IXT(n)=value the game assigns at run time (opened passages, wells, trapdoors).
IXT(j): direction = (j-1)//60+1 (S V J Z nahoru dolů), from = v%100,
to = (v//100)%100; the ten-thousands digit is the door state.

Floors: breadth-first from the start (room 93) along up/down exits.
Layout per floor: grid positions found by local search that keeps exits
pointing the right way and lets long paths stretch (a road around the walls
closes its loop instead of overlapping the mansion).

Output: web/layout.js and (with --preview) dist/layout-preview.html.
"""
import json
import random
import re
import sys
from collections import defaultdict, deque
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
BUILD = ROOT / "native" / "build"
DELTA = {1: (0, -1), 2: (1, 0), 3: (0, 1), 4: (-1, 0)}
START = 93
MAZES = [("parku s chrli", "bludiště v parku s chrliči"), ("krtčím bludišti", "krtčí bludiště"),
         ("točivém bludišti", "točivé bludiště"), ("chladné chodbě", "chladná chodba")]


# Hand-placed skeleton of the start level, so the map reads like a place:
# the mansion a 3x3 block in the middle, the gate south of it, the park north
# behind the veranda, the roads going round it east and west, the woods west.
# Everything else is laid out around these by the search below.
PINS = {0: {
    16: (0, 0), 17: (1, 0), 18: (2, 0),          # kaple, knihovna, kuchyně
    13: (0, 1), 14: (1, 1), 15: (2, 1),          # tajné schodiště, taneční sál, jídelna
    10: (0, 2), 11: (1, 2), 12: (2, 2),          # obývací pokoj, vstupní hala, herna
    91: (1, 4),                                  # hlavní vchod
    92: (0, 5),                                  # cesta ke vchodu
    93: (0, 6),                                  # hlavní brána (start)
    35: (0, 7),                                  # ohavná cesta
    96: (1, 6), 95: (5, -2),                     # road east, round to the back gate
    94: (-1, 6), 98: (-1, 5), 97: (-2, 6), 99: (-3, 6),   # road west, the woods, the river
    54: (1, -2), 55: (1, -3),                    # větrná veranda, zahradní brána
}}


def stmts(text):
    out, cur = [], None
    for line in text.split("\n"):
        if not line or line[0] in "Cc*!":
            continue
        if len(line) > 5 and line[5] not in " 0" and cur is not None:
            cur += line[6:72]
        else:
            if cur is not None:
                out.append(cur)
            cur = line[6:72]
    if cur:
        out.append(cur)
    return out


def data_values(text, name):
    for s in stmts(text):
        m = re.match(r"\s*DATA\s+" + name + r"\s*/(.*?)/", s, re.I)
        if m:
            vals = []
            for tok in m.group(1).split(","):
                tok = tok.strip()
                if "*" in tok:
                    n, v = tok.split("*")
                    vals += [int(v)] * int(n)
                elif tok:
                    vals.append(int(tok))
            return vals
    raise SystemExit(f"DATA {name} not found")


def text_values(text, name):
    for s in stmts(text):
        m = re.match(r"\s*DATA\s+" + name + r"\s*/(.*)/\s*$", s, re.I)
        if m:
            return re.findall(r"'([^']*)'", m.group(1))
    return []


def load():
    mmm = (BUILD / "MMM.FOR").read_bytes().decode("cp852")
    ixt = sum((data_values(mmm, n) for n in ("IXT0", "IXT1", "IXT2", "IXT3")), [])
    assert len(ixt) == 360
    variants = defaultdict(set)
    for j, v in enumerate(ixt, 1):
        variants[j].add(v)
    for f in BUILD.glob("MMS?.FOR"):
        for m in re.finditer(r"IXT\((\d+)\)\s*=\s*(\d+)", f.read_bytes().decode("cp852"), re.I):
            variants[int(m.group(1))].add(int(m.group(2)))
    edges = set()
    for j, vals in variants.items():
        d = (j - 1) // 60 + 1
        for v in vals:
            a, b = v % 100, (v // 100) % 100
            if a and b and a != b:
                edges.add((a, d, b))
    irom = data_values(mmm, "IROM")
    names = [n for pair in zip(*[iter(text_values(mmm, "CRNM1") + text_values(mmm, "CRNM2"))] * 2)
             for n in ["".join(pair).strip()]]
    room_name = {r: (names[irom[r - 1] % 100 - 1] if 0 < irom[r - 1] % 100 <= len(names) else "")
                 for r in range(1, 100)}
    return edges, room_name


def floors(edges):
    adj = defaultdict(list)
    for a, d, b in edges:
        dz = 1 if d == 5 else -1 if d == 6 else 0
        adj[a].append((b, dz))
        adj[b].append((a, -dz))
    z = {START: 0}
    q = deque([START])
    while q:
        r = q.popleft()
        for s, dz in adj[r]:
            if s not in z:
                z[s] = z[r] + dz
                q.append(s)
    for a, _, b in edges:            # rooms not reachable from the start
        for r in (a, b):
            z.setdefault(r, 0)
    return z


def edge_cost(pa, pb, d):
    dx, dy = pb[0] - pa[0], pb[1] - pa[1]
    ex, ey = DELTA[d]
    along = dx * ex + dy * ey          # progress in the exit's direction
    side = abs(dx * ey - dy * ex)      # sideways drift
    if along <= 0:
        return 12 + 3 * side + 2 * abs(along)
    return (along - 1) * 2.2 + side * 3.0


def layout_floor(rooms, hedges, anchors, seed=1, groups=None, house=(), pins=None, near=None):
    """anchors: room -> (x, y) of its up/down neighbour on an already laid-out floor.
    house: the mansion's rooms on this floor; they stay one block and everything
    outside (roads, park, woods) keeps a gap around it and goes round it."""
    rng = random.Random(seed)
    adj = defaultdict(list)
    for a, d, b in hedges:
        adj[a].append((b, d))
        adj[b].append((a, {1: 3, 2: 4, 3: 1, 4: 2}[d]))
    pos, taken = {}, {}
    pins = {r: c for r, c in (pins or {}).items() if r in rooms}
    for r, c in pins.items():
        pos[r], taken[c] = c, r
    free = [r for r in rooms if r not in pins]

    def free_near(x, y):
        for rad in range(0, 60):
            cells = [(x + dx, y + dy) for dx in range(-rad, rad + 1) for dy in range(-rad, rad + 1)
                     if max(abs(dx), abs(dy)) == rad]
            rng.shuffle(cells)
            for c in cells:
                if c not in taken:
                    return c
    # greedy BFS placement, component by component (largest first)
    remaining = set(free)
    ox = 0
    while remaining:
        comp_start = len(pos)
        # grow from pinned rooms first, then from the rest
        frontier = [r for r in pins if any(s in remaining for s, _ in adj[r])]
        if frontier:
            q = deque(frontier)
        else:
            near = near or {}
            seed_room = START if START in remaining else max(
                remaining, key=lambda r: (r in anchors, any(m in pos for m in near.get(r, ())), len(adj[r])))
            q = deque([seed_room])
            mate = next((m for m in near.get(seed_room, ()) if m in pos), None)
            c = (free_near(*anchors[seed_room]) if seed_room in anchors
                 else free_near(pos[mate][0] - 2, pos[mate][1] - 1) if mate is not None else free_near(ox, 0))
            pos[seed_room], taken[c] = c, seed_room
            remaining.discard(seed_room)
        while q:
            r = q.popleft()
            for s, d in adj[r]:
                if s in remaining:
                    x, y = pos[r]
                    c = free_near(x + DELTA[d][0], y + DELTA[d][1])
                    pos[s], taken[c] = c, s
                    remaining.discard(s)
                    q.append(s)
        if len(pos) > comp_start:
            ox = max(p[0] for p in pos.values()) + 3

    cx = sum(p[0] for p in pos.values()) / len(pos)
    cy = sum(p[1] for p in pos.values()) / len(pos)

    house = [h for h in house if h in rooms]
    outside = [r for r in rooms if r not in house]

    def house_box():
        xs = [pos[h][0] for h in house]; ys = [pos[h][1] for h in house]
        return min(xs) - 1, max(xs) + 1, min(ys) - 1, max(ys) + 1

    def in_box(p, b):
        return b[0] <= p[0] <= b[1] and b[2] <= p[1] <= b[3]

    def cost_of(r):
        x, y = pos[r]
        c = sum(edge_cost(pos[r], pos[s], d) for s, d in adj[r])
        if house:
            b = house_box()
            if r in house:                                      # the block stays compact and
                c += 4 * sum(in_box(pos[o], b) for o in outside)   # nothing outside sits in it
                c += 0.5 * ((b[1] - b[0]) + (b[3] - b[2]))
            elif in_box(pos[r], b):
                c += 8
        c += 0.08 * (abs(x - cx) + abs(y - cy))                 # keep the floor together
        for m in (near or {}).get(r, ()):                       # stairs to a room on this same
            if m in pos:                                        # level: keep the two close
                c += 0.4 * (abs(x - pos[m][0]) + abs(y - pos[m][1]))
        if r in anchors:                                        # stairs line up
            c += 0.6 * (abs(x - anchors[r][0]) + abs(y - anchors[r][1]))
        g = (groups or {}).get(r)
        if g:                                                   # a maze stays in one place
            mates = [pos[m] for m in g if m != r and m in pos]
            if mates:
                mx_ = sum(p[0] for p in mates) / len(mates); my_ = sum(p[1] for p in mates) / len(mates)
                c += 0.9 * (abs(x - mx_) + abs(y - my_))
        return c

    # local search: move a room to a nearby free cell, or swap with a neighbour cell
    temp = 2.0
    for it in range(40000):
        r = rng.choice(free)
        x, y = pos[r]
        c = (x + rng.randint(-2, 2), y + rng.randint(-2, 2))
        if c == pos[r]:
            continue
        other = taken.get(c)
        if other in pins:
            continue
        before = cost_of(r) + (cost_of(other) if other else 0)
        old = pos[r]
        pos[r] = c
        if other:
            pos[other] = old
        after = cost_of(r) + (cost_of(other) if other else 0)
        import math
        if after <= before or rng.random() < math.exp((before - after) / max(temp, 1e-3)):
            del taken[old]
            if other:
                taken[old] = other
            taken[c] = r
        else:
            pos[r] = old
            if other:
                pos[other] = c
        temp *= 0.99985
    total = sum(cost_of(r) for r in rooms)
    return pos, total


def main():
    edges, room_name = load()
    z = floors(edges)
    by_floor = defaultdict(list)
    for r, f in z.items():
        by_floor[f].append(r)
    layout = {}
    report = []
    placed = {}                                   # room -> (x, y) on finished floors
    order = sorted(by_floor, key=lambda f: (abs(f), -f))     # 0, +1, -1, +2, -2 ...
    for f in order:
        rooms = sorted(by_floor[f])
        hedges = [(a, d, b) for a, d, b in edges if d <= 4 and z[a] == f and z[b] == f]
        anchors = {}
        for a, d, b in edges:
            if d >= 5:
                for here, there in ((a, b), (b, a)):
                    if z.get(here) == f and there in placed:
                        anchors.setdefault(here, placed[there])
        best = None
        groups = {}
        for part, label in MAZES:
            members = [r for r in rooms if part in room_name.get(r, "")]
            for r in members:
                groups[r] = members
        # the mansion on this floor: indoor rooms (the game's compass rule: rooms
        # below 54 except the nasty road 35 are inside) joined by plain exits
        indoor = {r for r in rooms if r < 54 and r != 35 and r not in groups}
        hadj = defaultdict(set)
        for a, d, b in hedges:
            if a in indoor and b in indoor:
                hadj[a].add(b); hadj[b].add(a)
        house, seen = [], set()
        for r in sorted(indoor):
            if r in seen:
                continue
            comp, q = [], [r]
            seen.add(r)
            while q:
                x = q.pop(); comp.append(x)
                for y in hadj[x] - seen:
                    seen.add(y); q.append(y)
            if len(comp) > len(house):
                house = comp
        if len(house) < 4:
            house = []
        near = defaultdict(list)                  # up/down exits that stay on this level
        for a, d, b in edges:
            if d >= 5 and z[a] == f and z[b] == f:
                near[a].append(b); near[b].append(a)
        for seed in range(6):
            pos, total = layout_floor(rooms, hedges, anchors, seed, groups, house, PINS.get(f), near)
            if best is None or total < best[1]:
                best = (pos, total)
        pos, total = best
        for r, (x, y) in pos.items():
            placed[r] = (x, y)
            layout[r] = [x, y, f]
        report.append(f"floor {f:+d}: {len(rooms)} rooms, {len(hedges)} exits, cost {total:.0f}")
    mx = min(v[0] for v in layout.values()); my = min(v[1] for v in layout.values())
    for v in layout.values():
        v[0] -= mx; v[1] -= my
    out = "// Generated by wasm/gen_layout.py from the game's exit table. Do not edit.\n"
    out += "// room: [x, y, floor]; floor 0 = start level (+1 up, -1 down)\n"
    out += f"export const LAYOUT = {json.dumps({str(k): v for k, v in sorted(layout.items())})};\n"
    # mazes: rooms that deliberately share a name (and the woods) get a marked area
    maze = {}
    for r, n in room_name.items():
        for part, label in MAZES:
            if part in n and r in layout:
                maze[str(r)] = label
    for r in (97, 98):
        if r in layout:
            maze[str(r)] = "les, kde se bloudí"
    out += f"export const MAZE = {json.dumps(maze, ensure_ascii=False)};\n"
    (ROOT / "wasm" / "web" / "layout.js").write_text(out, encoding="utf-8", newline="\n")
    print("\n".join(report))
    if "--preview" in sys.argv:
        preview(layout, edges, room_name, z)


def preview(layout, edges, room_name, z):
    W, H, GX, GY = 120, 40, 150, 80
    parts = []
    floors_ = sorted(set(v[2] for v in layout.values()), reverse=True)
    oy = 0
    for f in floors_:
        rs = {r: v for r, v in layout.items() if v[2] == f}
        h = (max(v[1] for v in rs.values()) + 1) * GY + 60
        svg = [f'<h2>patro {f:+d}</h2><svg width="{(max(v[0] for v in rs.values()) + 1) * GX + 40}" height="{h}">']
        for a, d, b in edges:
            if d <= 4 and a in rs and b in rs:
                pa, pb = rs[a], rs[b]
                bad = edge_cost(pa, pb, d) >= 12
                svg.append(f'<line x1="{pa[0]*GX+20+W/2}" y1="{pa[1]*GY+20+H/2}" x2="{pb[0]*GX+20+W/2}" '
                           f'y2="{pb[1]*GY+20+H/2}" stroke="{"#c44" if bad else "#5a7"}" stroke-width="2"/>')
        for r, (x, y, _) in rs.items():
            ups = "".join("↑" if d == 5 else "↓" for a, d, b in edges if a == r and d >= 5)
            svg.append(f'<g transform="translate({x*GX+20},{y*GY+20})"><rect width="{W}" height="{H}" rx="6" '
                       f'fill="#142019" stroke="#4a6"/><text x="{W/2}" y="17" fill="#cfe" font-size="11" '
                       f'text-anchor="middle">{r} {ups}</text><text x="{W/2}" y="32" fill="#9b9" font-size="10" '
                       f'text-anchor="middle">{room_name.get(r, "")[:20]}</text></g>')
        svg.append("</svg>")
        parts.append("".join(svg))
        oy += h
    html = ('<!doctype html><meta charset="utf-8"><body style="background:#0d1110;color:#cfe;font-family:monospace">'
            + "".join(parts) + "</body>")
    (ROOT / "wasm" / "dist").mkdir(exist_ok=True)
    (ROOT / "wasm" / "dist" / "layout-preview.html").write_text(html, encoding="utf-8")


if __name__ == "__main__":
    main()
