#!/usr/bin/env python3
"""Differential test: native gfortran build vs. WebAssembly build.

Feeds identical random sessions (same generator as native/fuzz.py) to both,
with MMM_FIXED_TIME pinning the mystery number, and compares the output
byte for byte.  Usage: python3 compare.py [runs] [cmds] [--web] [--cz]
(run where both `native/build/mmm` and `node` are available, e.g. the
emscripten/emsdk container; the native binary is static)."""
import os
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "native"))
import random  # noqa: E402
from fuzz import command  # noqa: E402

CZ = "--cz" in sys.argv          # the new Czech translation build
NATIVE = [str(ROOT / "native" / ("build-cz" if CZ else "build") / "mmm")]
WASM = ["node", str(ROOT / "wasm" / "dist" / ("mmm-cz-node.js" if CZ else "mmm-node.js"))]
WEB = ["node", str(ROOT / "wasm" / "web" / "test-node.mjs")]


def run(cmd, data, env):
    try:
        p = subprocess.run(cmd, input=data, capture_output=True, timeout=30, env=env)
        return p.stdout + p.stderr, p.returncode
    except subprocess.TimeoutExpired as e:
        return (e.stdout or b"") + b"\n<<TIMEOUT>>", -999


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    runs = int(args[0]) if args else 50
    ncmd = int(args[1]) if len(args) > 1 else 200
    bad = 0
    for seed in range(runs):
        rng = random.Random(seed)
        lines = ["Tester"] + [command(rng) for _ in range(ncmd)] + ["KONEC", "ANO"] * 3
        data = ("\n".join(lines) + "\n").encode("cp852")
        env = dict(os.environ, MMM_FIXED_TIME=f"{rng.randrange(24):02d}{rng.randrange(60):02d}{rng.randrange(60):02d}")
        a, ra = run(NATIVE, data, env)
        if "--web" in sys.argv:  # web build via the async console path
            tmp = ROOT / "wasm" / "dist" / "in.txt"
            tmp.write_bytes(data)
            b, rb = run(WEB + [str(tmp), env["MMM_FIXED_TIME"]] + (["cz"] if CZ else []), data, env)
        else:
            b, rb = run(WASM, data, env)
        if a != b or ra != rb:
            bad += 1
            al, bl = a.decode("cp852", "replace").splitlines(), b.decode("cp852", "replace").splitlines()
            i = next((k for k, (x, y) in enumerate(zip(al, bl)) if x != y), min(len(al), len(bl)))
            print(f"seed {seed}: DIFF at output line {i} (exit {ra} vs {rb})")
            print(f"   native: {al[i] if i < len(al) else '<EOF>'!r}")
            print(f"   wasm:   {bl[i] if i < len(bl) else '<EOF>'!r}")
            (ROOT / "wasm" / "dist" / f"diff_{seed}_native.txt").write_bytes(a)
            (ROOT / "wasm" / "dist" / f"diff_{seed}_wasm.txt").write_bytes(b)
    print(f"{runs - bad}/{runs} sessions identical")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
