#!/usr/bin/env python3
"""Check new translations against the original Czech templates.

  python wasm/cz/validate.py [file.json ...]
Each file is either messages.json (uses the "new" field) or a {key: text}
object.  Problems are listed per key; exit status 1 if any.

Rules (what the Czech engine needs):
  * the same placeholders ({A8}, {4A2}, {I4}, {$}, ...) in the same order
  * every printed line starts with a space (Fortran carriage control),
    like the original; a template starting with a blank line keeps it
  * lines at most 72 characters (the original screen width), warn above 70
  * only characters that exist in CP852 (Czech letters yes, „“ … – no)
"""
import json
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
PH = re.compile(r"\{[^}]+\}")


def lines_of(t):
    return t.split("\n")


def check(key, old, new):
    issues = []
    if PH.findall(old) != PH.findall(new):
        issues.append(f"placeholders {PH.findall(old)} -> {PH.findall(new)}")
    if old.startswith("\n") != new.startswith("\n"):
        issues.append("leading blank line differs from the original")
    for ln in lines_of(new):
        shown = PH.sub(lambda m: "X" * width(m.group(0)), ln)
        if ln and not ln.startswith(" ") and not ln.startswith("{"):
            issues.append(f"line does not start with a space: {ln[:40]!r}")
        if len(shown) > 72:
            issues.append(f"line too long ({len(shown)}): {ln[:50]!r}…")
    try:
        new.encode("cp852")
    except UnicodeEncodeError as e:
        issues.append(f"character not in CP852: {new[e.start]!r}")
    return issues


def width(ph):
    m = re.match(r"\{(\d*)([AIF])(\d*)", ph)
    if not m:
        return 0
    n, kind, w = int(m.group(1) or 1), m.group(2), int(m.group(3) or 1)
    return n * w


def main():
    base = {r["key"]: r for r in json.loads((HERE / "messages.json").read_text(encoding="utf-8"))}
    files = sys.argv[1:] or [str(HERE / "messages.json")]
    bad = total = 0
    for f in files:
        data = json.loads(Path(f).read_text(encoding="utf-8"))
        items = {r["key"]: r.get("new", "") for r in data} if isinstance(data, list) else data
        for key, new in items.items():
            if not new:
                continue
            total += 1
            if key not in base:
                print(f"{key}: unknown key"); bad += 1; continue
            for i in check(key, base[key]["cz"], new):
                print(f"{key}: {i}"); bad += 1
    print(f"{total} translations checked, {bad} problems")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
