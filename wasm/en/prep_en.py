#!/usr/bin/env python3
"""Prepare Bill Wolpert's English Mystery Mansion (HP 1000 Fortran, as
modified for f2c by Ken Cornetet; original_en/hp1000-fortran) for the
WebAssembly build.  Patched copies go to wasm/en/gen/.

  * Fix: OPEN/CLOSE GATE at room 54 worked the main gate (garden_gate).
  * CALL EXEX(...) is used with 4, 5 or 7 arguments (fine on the HP 1000 and
    on x86, a trap in WebAssembly): pad every call to the full 7 arguments.
"""
import re
from pathlib import Path

HERE = Path(__file__).resolve().parent
SRC = HERE.parent.parent / "original_en" / "hp1000-fortran"
OUT = HERE / "gen"


def pad_exex(line):
    def fix(m):
        args = m.group(2)
        n = 1
        depth = 0
        for ch in args:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
            elif ch == "," and depth == 0:
                n += 1
        return f"{m.group(1)}({args}{',0' * (7 - n)})" if n < 7 else m.group(0)
    return re.sub(r"(CALL\s+exex)\s*\(((?:[^()]|\([^()]*\))*)\)", fix, line, flags=re.I)


def hollerith_value(chars):
    """1HM / 2HAB as an INTEGER*2 in memory byte order (as f2c stores DATA)."""
    b = (chars + "  ")[:2].encode("latin-1")
    v = b[0] | (b[1] << 8)
    return v - 65536 if v >= 32768 else v


def holleriths(line):
    """nH constants in assignments and comparisons -> numbers (f2c refuses
    them outside DATA/FORMAT).  FORMAT statements are left alone."""
    if re.match(r"\s*\d*\s*FORMAT\s*\(", line, re.I):
        return line

    out, pos = [], 0
    pat = re.compile(r"(=\s*|\.(?:EQ|NE)\.\s*)([12])H", re.I)
    while (m := pat.search(line, pos)):
        n = int(m.group(2))
        chars = line[m.end():m.end() + n].ljust(n)   # trailing blanks may be trimmed
        out.append(line[pos:m.start()] + f"{m.group(1)}({hollerith_value(chars)})")
        pos = m.end() + n
    out.append(line[pos:])
    return "".join(out)


def split_strings(lines):
    """A string closed at the end of a line followed by a continuation line
    that opens a new string ("...MANSION," / 3" JUST VISIBLE") is joined by
    f2c into "", an escaped quote, printing MANSION," JUST.  The HP 3000
    source and the C port have a comma there: insert it (at the end of the
    previous line if it fits in 72 columns, else at the start of this one,
    moving any overflow to an extra continuation line)."""
    out, inq, fixed, last_i = [], False, 0, None
    for line in lines:
        is_stmt = line[:1] not in "Cc*!" and not line.startswith("	") and len(line) > 6
        if not is_stmt:
            out.append(line)
            continue
        cont = line[5] not in " 0"
        if not cont:
            inq = False
        prev = out[last_i] if last_i is not None else ""
        extra = None
        if cont and not inq and line[6:72].lstrip().startswith('"') and prev[6:72].rstrip().endswith('"'):
            if len(prev.rstrip()) < 72:
                out[last_i] = prev.rstrip() + ","
            else:
                line = line[:6] + "," + line[6:].rstrip()
                if len(line) > 72:
                    line, extra = line[:72], "     &" + line[72:]
            fixed += 1
        for part in (line, extra):
            if part is None:
                continue
            for ch in part[6:72]:
                if ch == '"':
                    inq = not inq
            out.append(part)
            last_i = len(out) - 1
    return out, fixed


def garden_gate(lines):
    """OPEN/CLOSE GATE at room 54 (garden side of the garden gate) fell through
    to the main-gate code: the garden-gate answers test rooms 95 and 55 only.
    Same fix as native/prep.py patch_garden_gate (OPRAVY.md, section 8)."""
    text = "\n".join(lines)
    text, n = re.subn(r"\(\(IR \.EQ\. ?95\) \.OR\. \(IR \.EQ\. 55\)\)",
                      "(IR.EQ.95.OR.IR.EQ.55.OR.IR.EQ.54)", text)
    if n != 2:
        raise SystemExit(f"garden_gate: expected 2 matches, got {n}")
    out = text.split("\n")
    if any(len(l.rstrip()) > 72 for l in out if not l.startswith("\t") and l[:1] not in "Cc*!"):
        raise SystemExit("garden_gate: line too long")
    return out


def main():
    OUT.mkdir(exist_ok=True)
    count = commas = 0
    for f in sorted(SRC.glob("*.F")):
        text = f.read_text(encoding="latin-1")
        lines = []
        for line in text.split("\n"):
            if line[:1] not in "Cc*!":
                new = holleriths(line)
                count += new != line
                line = new
            if line[:1] not in "Cc*!" and "exex" in line.lower():
                new = pad_exex(line)
                if len(new.rstrip()) > 72 and not line.startswith("\t"):
                    raise SystemExit(f"{f.name}: padded line too long: {new}")
                count += new != line
                line = new
            lines.append(line)
        lines, nfix = split_strings(lines)
        commas += nfix
        if f.name == "MMSF.F":
            lines = garden_gate(lines)
        (OUT / (f.stem.lower() + ".f")).write_text("\n".join(lines), encoding="latin-1")
    (OUT / "common.i").write_bytes((SRC / "COMMON.I").read_bytes())
    print(f"en/gen: {count} lines patched, {commas} split strings joined")


if __name__ == "__main__":
    main()
