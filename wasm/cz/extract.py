#!/usr/bin/env python3
"""Extract every game message for the new Czech translation.

Czech (native/build after prep.py, CP852):  label F='(...)'  and  label FORMAT(...)
English (original_en/hp1000-fortran):       label FORMAT(...)
Messages are paired by segment file and statement label (the Czech authors
kept the English labels: MMSD 1084 is the amulet in both).

A message is stored as a *template*: the literal text with '\\n' for '/' and
{A8}, {4A2}, {I4} ... for the values the game fills in. Formats without any
literal text (input formats like (40A2)) are skipped.

Output: wasm/cz/messages.json  [{key, file, label, cz, en, new}]
An existing file keeps its "new" translations.
"""
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent.parent
CZ = ROOT / "native" / "build"
EN = ROOT / "original_en" / "hp1000-fortran"
OUT = Path(__file__).resolve().parent / "messages.json"
FILES = ["MMM", "MMSA", "MMSB", "MMSC", "MMSD", "MMSE", "MMSF", "MMSG", "MMSH", "MMSI", "MMSJ", "MMSK"]
EN_FILE = {"MMM": "MMSUBS", "MMSA": "MMSA"}   # MMRI (item list) lives in MMSUBS.F in English


def statements(text):
    """(label, joined statement) for fixed-form source, keeping string spacing."""
    out, cur, label = [], None, None
    for raw in text.split("\n"):
        line = raw.rstrip("\r")
        if line.startswith("\t"):                       # tab format (English sources)
            line = ("      " + line[1:]) if not line[1:2].isdigit() else ("     " + line[1:])
        if not line.strip() or line[0] in "Cc*!":
            continue
        if len(line) > 5 and line[5] not in " 0" and cur is not None:
            cur += line[6:72].ljust(66) if len(line) >= 72 else line[6:72]
        else:
            if cur is not None:
                out.append((label, cur))
            label = line[:5].strip() or None
            cur = line[6:72].ljust(66) if len(line) >= 72 else line[6:72]
    if cur is not None:
        out.append((label, cur))
    return out


def tokenize(fmt):
    """Fortran format items -> template string, or None if there is no text."""
    s, i, parts, has_text = fmt, 0, [], False
    while i < len(s):
        c = s[i]
        if c in "'\"":
            q, j, buf = c, i + 1, []
            while j < len(s):
                if s[j] == q and j + 1 < len(s) and s[j + 1] == q:
                    # '' is an escaped quote; "" in the English source is two strings
                    # split across lines without a comma (see wasm/en/prep_en.py)
                    if q == "'":
                        buf.append(q)
                    j += 2; continue
                if s[j] == q:
                    break
                buf.append(s[j]); j += 1
            parts.append("".join(buf)); has_text = True; i = j + 1
        elif c == "/":
            parts.append("\n"); i += 1
        elif c in ", ()":
            i += 1
        elif c in "$\\":
            parts.append("{$}"); i += 1
        else:
            m = re.match(r"(\d*)H", s[i:]) if re.match(r"\d+H", s[i:]) else None
            if m:                                           # Hollerith nH...
                n = int(m.group(1)); t = s[i + len(m.group(0)): i + len(m.group(0)) + n]
                parts.append(t); has_text = True; i += len(m.group(0)) + n; continue
            m = re.match(r"(\d*)X", s[i:], re.I)
            if m:
                parts.append(" " * int(m.group(1) or 1)); i += len(m.group(0)); continue
            m = re.match(r"\d*[AIFEGLTZ]\d*(?:\.\d+)?", s[i:], re.I)
            if m and m.group(0):
                parts.append("{" + m.group(0).upper() + "}"); i += len(m.group(0)); continue
            i += 1
    return "".join(parts) if has_text else None


def cz_messages(name):
    text = (CZ / f"{name}.FOR").read_bytes().decode("cp852")
    msgs = {}
    for label, st in statements(text):
        m = re.match(r"\s*F\s*=\s*'(.*)'\s*$", st, re.S)
        if m:
            fmt = m.group(1).replace("''", "'")
        else:
            m = re.match(r"\s*FORMAT\s*(\(.*\))\s*$", st, re.S | re.I)
            if not m:
                continue
            fmt = m.group(1)
        if label:
            t = tokenize(fmt)
            if t:
                msgs.setdefault(label, t)
    return msgs


def en_messages(name):
    p = EN / f"{EN_FILE.get(name, name)}.F"
    if not p.exists():
        return {}
    msgs = {}
    for label, st in statements(p.read_text(encoding="latin-1")):
        m = re.match(r"\s*FORMAT\s*(\(.*\))\s*$", st, re.S | re.I)
        if label and m:
            t = tokenize(m.group(1))
            if t:
                msgs.setdefault(label, t)
    return msgs


def main():
    old = {}
    if OUT.exists():
        old = {m["key"]: m.get("new", "") for m in json.loads(OUT.read_text(encoding="utf-8"))}
    rows = []
    for name in FILES:
        cz, en = cz_messages(name), en_messages(name)
        for label, t in cz.items():
            key = f"{name}:{label}"
            e = en.get(label, "")
            if not e and name == "MMSE" and 100 < int(label) < 200:
                e = en.get(str(int(label) - 100), "")      # room texts: Czech label = English + 100
            rows.append({"key": key, "file": name, "label": label, "cz": t,
                         "en": e, "new": old.get(key, "")})
    OUT.write_text(json.dumps(rows, ensure_ascii=False, indent=1), encoding="utf-8")
    paired = sum(1 for r in rows if r["en"])
    print(f"{len(rows)} Czech messages, {paired} paired with English, {len(rows) - paired} Czech-only")
    for name in FILES:
        n = sum(1 for r in rows if r["file"] == name)
        p = sum(1 for r in rows if r["file"] == name and r["en"])
        print(f"  {name}: {n} ({p} paired)")


if __name__ == "__main__":
    main()
