#!/usr/bin/env python3
"""Smoke-fuzz the native build: run many sessions of random commands and
report runtime errors, crashes and hangs.  Usage: python3 fuzz.py [runs] [cmds]"""
import random
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
EXE = HERE / (sys.argv[3] if len(sys.argv) > 3 else "build") / "mmm"

VERBS = """JÍT ŠPLHAT VSTOUPIT KOPAT SLEDOVAT SKÓRE VZÍT ODLOŽIT DÁT PODÍVAT ČÍST
NAJÍT LIST ROZSVÍTIT OBRÁTIT POMOC OTEVŘÍT ZAVŘÍT UHASIT FOUKAT JÍST HLADIT
PŘEMÍSTIT ZAMETAT ZAVOLAT POHNOUT ZVEDNOUT NAKRMIT NAPADNOUT ZABÍT PÍT ZÁHADA
PODPLATIT ZEPTAT POSLOUCHAT ODEMKNOUT ZAMKNOUT STŘELIT OLEJOVAT ZAZNAMENAT
ROZBÍT POLOŽIT SPÁT ODSUNOUT VOLAT HLEDAT DÍVAT ZATÁHNOUT ZAPÁLIT ZHASNOUT
OTOČIT ROZHLÉDNOUT""".split()
OBJS = """PRSTEN KLÍČE HODINKY LUCERNA KŘÍŽEK BRÁNU DVEŘE LOPATKU LÍSTEK SVÍČKU
NŮŽ SEKERU MEČ JED PISTOLI SIRKY KLACEK BATERIE KLÍČEK SLUHA PANÍ KRTEK
VAMPÍR LESNÍK KUCHAŘ SLUŽKA PÁN MRTVOLA ZÁCLONY KNIHU MŘÍŽE DÝKA VŠE""".split()
DIRS = "SEVERNĚ VÝCHODNĚ JIŽNĚ ZÁPADNĚ NAHORU DOLŮ ZPĚT".split()
PREPS = "v na u k do se".split()


def command(rng):
    r = rng.random()
    if r < 0.45:
        return f"JÍT {rng.choice(DIRS)}"
    if r < 0.85:
        v = rng.choice(VERBS)
        if rng.random() < 0.3:
            return f"{v} {rng.choice(OBJS)} {rng.choice(PREPS)} {rng.choice(OBJS)}"
        return f"{v} {rng.choice(OBJS)}"
    if r < 0.95:
        return rng.choice(["ANO", "NE", "ROZHLÉDNOUT SE", "LIST", "SKÓRE"])
    return rng.choice(VERBS)


def main():
    runs = int(sys.argv[1]) if len(sys.argv) > 1 else 100
    ncmd = int(sys.argv[2]) if len(sys.argv) > 2 else 200
    bad = 0
    for seed in range(runs):
        rng = random.Random(seed)
        lines = ["Fuzzer"] + [command(rng) for _ in range(ncmd)] + ["KONEC", "ANO"] * 3
        data = ("\n".join(lines) + "\n").encode("cp852")
        try:
            p = subprocess.run([str(EXE)], input=data, capture_output=True, timeout=10)
            out = (p.stdout + p.stderr).decode("cp852", "replace")
            ok = p.returncode == 0 and "runtime error" not in out.lower()
            why = f"exit={p.returncode}"
        except subprocess.TimeoutExpired as e:
            out = ((e.stdout or b"") + (e.stderr or b"")).decode("cp852", "replace")
            ok, why = False, "TIMEOUT"
        if not ok:
            bad += 1
            log = HERE / "build" / f"fuzz_{seed}.log"
            log.write_text("\n".join(lines) + "\n=====\n" + out, encoding="utf-8")
            err = [l for l in out.splitlines() if "error" in l.lower() or "at line" in l.lower()]
            print(f"seed {seed}: {why} {' | '.join(err[:2])}")
    print(f"{runs - bad}/{runs} sessions clean")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
