#!/usr/bin/env python3
"""Smoke-fuzz the English WebAssembly build (node target) with random commands."""
import random, subprocess, sys, os
from pathlib import Path
EXE = ["node", str(Path(__file__).resolve().parent.parent / "dist" / "mmm-en-node.js")]
VERBS = """GO CLIMB ENTER DIG FOLLOW SCORE GET TAKE DROP GIVE LOOK READ FIND LIST LIGHT TURN
HELP OPEN CLOSE SNUFF BLOW EAT RUB BEAM SWEEP CALL MOVE ANSWER FEED ATTACK KILL DRINK
BRIBE QUESTION LISTEN UNLOCK LOCK SHOOT OIL BREAK PLACE SLEEP""".split()
OBJS = """RING KEYS CLOCK LANTERN CROSS COMPASS SHOVEL NOTE CANDLE KNIFE AXE SWORD GUN GATE
DOOR WOODSMAN BUTLER LADY MOLE VAMPIRE COOK MAID CORPSE CURTAINS BOOK BARS ALL""".split()
DIRS = "NORTH EAST SOUTH WEST UP DOWN BACK".split()
def cmd(r):
    x = r.random()
    if x < .45: return f"GO {r.choice(DIRS)}"
    if x < .9: return f"{r.choice(VERBS)} {r.choice(OBJS)}"
    return r.choice(["YES", "NO", "LOOK", "LIST", "SCORE"])
runs = int(sys.argv[1]) if len(sys.argv) > 1 else 50
bad = 0
for seed in range(runs):
    r = random.Random(seed)
    data = ("\n".join([cmd(r) for _ in range(250)] + ["QUIT", "YES"] * 3) + "\n").encode()
    env = dict(os.environ, MMM_FIXED_TIME=f"{r.randrange(24):02d}{r.randrange(60):02d}{r.randrange(60):02d}")
    try:
        p = subprocess.run(EXE, input=data, capture_output=True, timeout=30, env=env)
        out = (p.stdout + p.stderr).decode("latin-1")
        if p.returncode != 0 or "RuntimeError" in out or "error" in out.lower()[-500:]:
            bad += 1; print(f"seed {seed}: exit {p.returncode}", out[-300:].replace("\n", " | "))
    except subprocess.TimeoutExpired:
        bad += 1; print(f"seed {seed}: TIMEOUT")
print(f"{runs - bad}/{runs} sessions clean")
