#!/usr/bin/env python3
"""Preprocess the MS-Fortran 5.x sources (MMM_LAT, CP852) for modern gfortran.

The original sources are left untouched; patched copies go to native/build/.
Transformations (each one mirrors an MS Fortran behaviour gfortran lacks):

  * $STORAGE:2  -> default INTEGER/LOGICAL are 2 bytes: insert
                   IMPLICIT INTEGER*2 (I-N) into every program unit and turn
                   bare INTEGER / LOGICAL declarations into *2.
  * $INCLUDE:'x' -> INCLUDE 'x';  other $metacommands dropped.
  * Unit 0 is the console in MS Fortran (stderr in gfortran): the game always
    uses LU=0, so READ/WRITE(LU|LX|0, ...) become (*, ...).
  * '\' edit descriptor (no newline) -> '$' (gfortran extension).
  * form='binary' -> access='stream', form='unformatted'.
  * GETTIM (MS runtime) -> provided in compat.f90.
"""
import re
import sys
from pathlib import Path

SRC = Path(__file__).resolve().parent.parent / "MMM_LAT"
OUT = Path(__file__).resolve().parent / "build"

UNIT_RE = re.compile(r"^(?P<head>.*?\b(?:READ|WRITE)\s*\(\s*)(?:LU|LX|0)\s*,", re.I)
HDR_RE = re.compile(
    r"^\s{6}\s*(?:(?:INTEGER|LOGICAL|REAL|CHARACTER)(?:\*\d+)?\s+)?"
    r"(?:PROGRAM|SUBROUTINE|FUNCTION|BLOCK\s*DATA)\b", re.I)


def is_comment(line):
    return line[:1] in ("C", "c", "*", "!") or line.strip() == ""


def is_cont(line):
    return len(line) > 5 and line[5] not in (" ", "0") and not is_comment(line)


def int2_literal(m):
    """'AB' compared with an INTEGER*2 -> its little-endian numeric value."""
    lit = m.group(2).replace("''", "'")
    if len(lit) == 1:
        lit += " "
    b = lit.encode("latin-1")
    v = b[0] | (b[1] << 8)
    if v >= 32768:
        v -= 65536
    return f"{m.group(1)}({v})"


CMP_RE = re.compile(r"(\.(?:EQ|NE)\.\s*)'((?:[^']|''){1,2})'(?!\w)", re.I)


def process(text):
    text = text.replace("\x1a", "")
    # Leftovers from the Kamenicky -> CP852 conversion of MMM_LAT:
    # 0x8F (Kamenicky A-acute, CP852 C-acute) and 0x92 (used for I-acute).
    # They break both display (ZĆPADNĚ) and word matching (MŘĺŽE, KLĺČE).
    text = text.replace("\x8f", "\xb5").replace("\x92", "\xd6")
    lines = text.split("\n")
    out = []
    i = 0
    implicit_at = None
    while i < len(lines):
        line = lines[i].rstrip("\r")
        if line.startswith("$"):
            m = re.match(r"\$include\s*:\s*'([^']+)'", line, re.I)
            if m:
                out.append(f"      INCLUDE '{m.group(1).upper()}'")
            i += 1
            continue
        if not is_comment(line):
            code = line[:72].ljust(72)  # MS Fortran pads to col 72 (matters in continued strings)
            code = CMP_RE.sub(int2_literal, code)
            code = re.sub(r"\bCO=ZNAK\((\d):\d\)", r"CO=ICHAR(ZNAK(\1:\1))+8192", code)
            code = re.sub(r"\bIABS\(", "ABS(", code, flags=re.I)
            code = re.sub(r"\b(MIN|MAX)0\(", r"\1(", code, flags=re.I)
            code = re.sub(r"\bISIGN\(", "SIGN(", code, flags=re.I)
            if re.match(r"\s+IMPLICIT\b", code, re.I):
                code = re.sub(r"INTEGER\s*\(", "INTEGER*2 (", code, flags=re.I)
                if implicit_at is not None:
                    out[implicit_at] = "C     (own IMPLICIT below)"
                    implicit_at = None
            code = UNIT_RE.sub(lambda m: m.group("head") + "*,", code)
            code = re.sub(r"\bREAD\(\*,\*\)\s*(\w+)", r"CALL RDINT(\1)", code, flags=re.I)
            # REIO(2,LU,'text',n): CHARACTER literal into an INTEGER*2 buffer
            code = re.sub(r"\bCALL REIO\((\s*2\s*,[^,']+,\s*')", r"CALL REIOS(\1", code, flags=re.I)
            code = re.sub(r"\\(\s*'?\))", r"$\1", code)
            code = re.sub(r"\\'\)", "$')", code)
            # plain sequential unformatted: valid F77, works with f2c too
            code = re.sub(r"form\s*=\s*'binary'", "form='unformatted'", code, flags=re.I)
            code = re.sub(r"^(\s{6}\s*)(INTEGER|LOGICAL)(\s+)(?!\*)", r"\1\2*2\3", code, flags=re.I)
            # REAL*8 C... arrays are only 8-char text holders EQUIVALENCEd onto
            # INTEGER*2 arrays; as REAL*8 they force 8-byte alignment and shift
            # the COMMON layout in gfortran. CHARACTER*8 keeps byte alignment.
            code = re.sub(r"^(\s{6}\s*)REAL\*8(\s+C)", r"\1CHARACTER*8\2", code, flags=re.I)
            line = code
        out.append(line)
        if not is_comment(line) and HDR_RE.match(line):
            # copy continuation lines of the header, then add IMPLICIT
            while i + 1 < len(lines) and is_cont(lines[i + 1].rstrip("\r")):
                i += 1
                out.append(lines[i].rstrip("\r")[:72])
            implicit_at = len(out)
            out.append("      IMPLICIT INTEGER*2 (I-N)")
        i += 1
    return "\n".join(out) + "\n"


# ---------------------------------------------------------------------------
# Logic fixes for leftovers of the HP-1000 -> MS-DOS port.
#
# On RTE-VI, "CALL SGLOD(NAME,LU,code,...)" loaded the named segment and
# never returned.  The 1988 port turned SGLOD into a dispatcher driven from
# the main loop: segments set p1..p5 and RETURN.  Three call sites were never
# converted; they call the segment recursively and then fall through.  Under
# DOS that "worked" by luck (writable literals, garbage pointers); with
# gfortran it segfaults.  We convert them to the dispatcher convention.
# ---------------------------------------------------------------------------
DARK_RETURN = """\
      IF (IDK.NE.0) THEN
          p1=lu
          p2=1150
          p3=IDK-1
          p4=0
          p5=0
          IDK=0
          RETURN
      ENDIF"""


def patch_mmrl(text):
    """MMRL (light check) -> MMRL(IDARK): 0 = light, 1..3 = dark variant+1."""
    start = text.index("      SUBROUTINE MMRL")
    end = text.index("\n      END", start)
    unit = text[start:end]
    unit = unit.replace("SUBROUTINE MMRL", "SUBROUTINE MMRL(IDARK)", 1)
    unit = re.sub(r"(\n      LU=0)", r"\n      IDARK=0\1", unit, count=1)
    unit = re.sub(r"CALL SGLOD\(NAME,LU,1150,(\d),0,0\)",
                  lambda m: f"GOTO {9910 + int(m.group(1))}", unit)
    unit += "\n      RETURN\n" + "".join(
        f" {9910 + k} IDARK={k + 1}\n      RETURN\n" for k in range(3))
    return text[:start] + unit + text[end:]


def patch_mmrl_callers(text):
    lines = text.split("\n")
    out = []
    for n, line in enumerate(lines):
        if not is_comment(line) and re.search(r"CALL MMRL\s*$", line.rstrip()):
            out.append(line.rstrip() + "(IDK)")
            # emit the check after the (possibly continued) statement
            if not (n + 1 < len(lines) and is_cont(lines[n + 1])):
                out.append(DARK_RETURN)
            continue
        out.append(line)
    return "\n".join(out)


def patch_inline_sglod(text):
    """IF (...) CALL SGLOD(NAME,LU,code) -> IF (...) THEN / p2=code / RETURN."""
    return re.sub(
        r"(\n(?![Cc*])[^\n]*?) CALL SGLOD\(NAME,LU,(\d+)\)[ \t]*(?=\n)",
        r"\1 THEN\n          p2=\2\n          return\n      endif", text)


def patch_record(text):
    """ZAZNAMENAT asked for an HP-1000 printer LU (transcript copy).  Keep the
    prompt, swallow the answer, and keep the transcript unit at 0 (console)."""
    return re.sub(r"\n      CALL RDINT\(IANS\)(\s*\n)(?=[^\n]*\n[^\n]*21011)",
                  r"\n      CALL RDINT(IANS)\n      IANS=0\1", text)


def patch_reio_eof(text):
    """End of input at the command prompt ends the game cleanly."""
    text = text.replace("READ(*,1)(BUF(I),I=1,36)", "READ(*,1,END=99)(BUF(I),I=1,36)", 1)
    return text.replace("13    RETURN", "13    RETURN\n99    STOP", 1)


def patch_exit_bounds(text):
    """GO with a non-direction word can leave IC outside 1..6; the exit search
    then reads IXT far out of bounds (garbage under DOS, SIGSEGV here).
    Treat it as 'no exit that way' (message 1105), which is what DOS printed
    unless the garbage happened to match."""
    return text.replace(
        "      DO 1010 J=IC*60-59,IC*60",
        "      IF (IC.LT.1.OR.IC.GT.6) GOTO 1011\n"
        "      DO 1010 J=IC*60-59,IC*60", 1).replace(
        " 1010 CONTINUE", " 1010 CONTINUE\n 1011 CONTINUE", 1)


def char_assign_to_int(text):
    """X='AB' where X is INTEGER*2 -> X=<numeric value>.  MS Fortran (and
    gfortran -fdec) copy the bytes; f2c rejects it.  Same value either way."""
    chars = set()
    for m in re.finditer(r"^\s+CHARACTER\*\d+\s+(.*)$", text, re.I | re.M):
        chars |= {n.upper() for n in re.findall(r"([A-Za-z]\w*)(?:\([^)]*\))?", m.group(1))}
    pat = re.compile(r"(?<![\w.'])([A-Za-z]\w*)(\s*\([^()=']*\))?(\s*=\s*)'((?:[^']|''){1,2})'(?=\s*$)")

    def repl(m):
        if m.group(1).upper() in chars:
            return m.group(0)
        v = int2_literal(re.match(r"()(.*)", m.group(4))).strip("()")
        return f"{m.group(1)}{m.group(2) or ''}{m.group(3)}{v}"

    return "\n".join(line if is_comment(line) else pat.sub(repl, line.rstrip()).ljust(72)
                     if pat.search(line.rstrip()) else line
                     for line in text.split("\n"))


def wrap72(text):
    """Keep every statement line within columns 1-72 (f2c reads no further):
    longer lines continue on '     &' lines, padded to 72 so that string
    literals split across lines keep their exact spacing."""
    out = []
    for line in text.split("\n"):
        if is_comment(line) or len(line.rstrip()) <= 72:
            out.append(line[:72] if not is_comment(line) else line)
            continue
        line = line.rstrip()
        out.append(line[:72])
        rest = line[72:]
        while rest:
            out.append(("     &" + rest[:66]).ljust(72) if len(rest) > 66 else "     &" + rest)
            rest = rest[66:]
    return "\n".join(out)


def patch_rnd_common(text):
    """REAL*4 RND sits at a 2-byte offset inside /MMBC/; f2c refuses the
    misaligned REAL.  Give it a COMMON block of its own (it is saved and
    restored by name, so nothing else changes)."""
    return text.replace("NAME,RND,NTAG,IWP", "NAME,NTAG,IWP", 1) + \
        "      COMMON /MMRND/ RND\n"


def patch_publisher(text):
    """Title screen publisher line.  History: 'J Z D   A K   S l u s o v i c e'
    (Kamenicky copy), 'F U T U R E   E n g i n e e r i n g ,   s. r. o.'
    (MMM_LAT), and since 2026 the web edition is published by Archetypal.cz."""
    return text.replace("' F U T U R E   E n g i n e e r i n g ,   s. r. o.'",
                        "' A r c h e t y p a l . c z'", 1)


def add_eof_stop(text):
    """Every console READ gets END=99998, with '99998 STOP' before the unit's
    END, so closing stdin ends the game instead of a runtime error."""
    out, used = [], False
    for line in text.split("\n"):
        if not is_comment(line):
            new = re.sub(r"\bREAD\(\*,([^,()]+|'\([^']*\)')\)", r"READ(*,\1,END=99998)", line, flags=re.I)
            used |= new != line
            line = new
            if used and re.match(r"\s{6}END\s*$", line, re.I):
                out.append("99998 STOP")
                used = False
        out.append(line)
    return "\n".join(out)


def patch_garden_gate(text):
    """OTEVŘÍT/ZAVŘÍT BRÁNU at room 54 (garden side of the garden gate, 54<->55)
    before turn 300 skipped the garden-gate answers, which test only rooms
    95 and 55, and fell through to the main-gate code: with a helper or the
    gloves it opened or shut the main gate at the other end of the grounds.
    Same in the 1982 English source (MMSF.F:866-871), so the authors' own
    slip.  Room 54 gets the garden-gate answers like 55."""
    new, n = re.subn(r"\(\(IR \.EQ\. ?95\) \.OR\. \(IR \.EQ\. 55\)\)",
                     "(IR.EQ.95.OR.IR.EQ.55.OR.IR.EQ.54)", text)
    if n != 2:
        raise SystemExit(f"patch_garden_gate: expected 2 matches, got {n}")
    return new


PATCHES = {
    "MMSA.FOR": [patch_publisher],
    "MMCO.FOR": [patch_rnd_common],
    "MMLB.FOR": [patch_reio_eof],
    "MMM.FOR": [patch_mmrl],
    "MMSB.FOR": [patch_mmrl_callers],
    "MMSH.FOR": [patch_mmrl_callers],
    "MMSJ.FOR": [patch_mmrl_callers, patch_record],
    "MMSC.FOR": [patch_inline_sglod, patch_exit_bounds],
    "MMSF.FOR": [patch_inline_sglod, patch_garden_gate],
}


# ---------------------------------------------------------------------------
# New Czech translation (--translation wasm/cz/messages.json): replace message
# format strings by label.  Templates use '\n' for '/', {A8}/{I4}/{$}... for
# edit descriptors; see wasm/cz/extract.py.
# ---------------------------------------------------------------------------
TYPO = str.maketrans({"„": '"', "“": '"', "”": '"', "‚": "'", "‘": "'", "’": "'",
                      "–": "-", "—": "-", " ": " ", "…": "..."})


def to_source_text(s):
    """Unicode -> CP852 bytes held in a latin-1 str (how the sources are read)."""
    return s.translate(TYPO).encode("cp852").decode("latin-1")


def template_to_format(t):
    out, lit = "", ""

    def flush():
        nonlocal out, lit
        if lit:
            out += ("," if out and out[-1] not in "(/" else "") + "'" + lit.replace("'", "''") + "'"
            lit = ""
    for tok in re.split(r"(\{[^}]+\}|\n)", t):
        if tok == "\n":
            flush(); out += "/"
        elif tok.startswith("{") and tok.endswith("}"):
            flush()
            d = "$" if tok == "{$}" else tok[1:-1]
            out += ("," if out and out[-1] not in "(/" else "") + d
        else:
            lit += tok
    flush()
    return "(" + out + ")"


def apply_translation(text, name, table):
    lines = text.split("\n")
    out, i, done = [], 0, 0
    while i < len(lines):
        line = lines[i]
        m = re.match(r"^\s*(\d+)\s+(F\s*=\s*'|FORMAT\s*\()", line[:72], re.I) if not is_comment(line) else None
        key = f"{name}:{m.group(1)}" if m else None
        if key in table:
            label, kind = m.group(1), m.group(2).upper()
            j = i + 1
            while j < len(lines) and is_cont(lines[j]):
                j += 1
            fmt = to_source_text(template_to_format(table[key]))
            stmt = ("F='" + fmt.replace("'", "''") + "'") if "=" in kind else "FORMAT" + fmt
            out.append(f"{label:<5} {stmt}")
            done += 1
            i = j
            continue
        out.append(line)
        i += 1
    text = "\n".join(out)
    if done:   # longer texts: let the format variables hold them
        text = re.sub(r"^(\s+character\*)(\d+)(\s+f\b)", r"\g<1>2000\3", text, flags=re.I | re.M)
    return text, done


def apply_ranks(text, ranks):
    """Replace the DATA CCLS player ranks (MMM.FOR) with right-aligned words."""
    assert len(ranks) == 12 and all(len(r) <= 8 for r in ranks)
    vals = ["'" + to_source_text(r.rjust(8)) + "'" for r in ranks]
    new = ("      DATA CCLS/" + ",".join(vals[:4]) + ",\n"
           "     1" + ",".join(vals[4:8]) + ",\n"
           "     3" + ",".join(vals[8:]) + "/")
    out, n = re.subn(r"^      DATA CCLS/[^\n]*\n[^\n]*\n[^\n]*/[ \t]*$", lambda m: new, text, count=1, flags=re.M)
    if not n:
        raise SystemExit("DATA CCLS not found")
    return out


def main():
    import json
    args = sys.argv[1:]
    out_dir = Path(args[args.index("--out") + 1]) if "--out" in args else OUT
    table = {}
    if "--translation" in args:
        rows = json.loads(Path(args[args.index("--translation") + 1]).read_text(encoding="utf-8"))
        table = {r["key"]: r["new"] for r in rows if r.get("new")}
        rk = Path(args[args.index("--translation") + 1]).with_name("ranks.json")
        ranks = json.loads(rk.read_text(encoding="utf-8"))["ranks"] if rk.exists() else None
    out_dir.mkdir(parents=True, exist_ok=True)
    applied = 0
    for f in sorted(SRC.glob("*.FOR")):
        data = f.read_bytes().decode("latin-1")  # byte-preserving
        text = process(data)
        for fix in PATCHES.get(f.name, []):
            new = fix(text)
            if new == text:
                raise SystemExit(f"patch {fix.__name__} did not apply to {f.name}")
            text = new
        if table:
            text, n = apply_translation(text, f.stem, table)
            applied += n
            if ranks and f.name == "MMM.FOR":
                text = apply_ranks(text, ranks)
        text = add_eof_stop(text)
        text = char_assign_to_int(text)
        text = wrap72(text)
        (out_dir / f.name).write_bytes(text.encode("latin-1"))
    print(f"wrote {len(list(out_dir.glob('*.FOR')))} files to {out_dir}"
          + (f" ({applied}/{len(table)} translated messages)" if table else ""))


if __name__ == "__main__":
    sys.exit(main())
