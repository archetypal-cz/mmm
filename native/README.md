# Native build (modern gfortran)

Builds the original Fortran source (`MMM_LAT/`, CP852) unchanged in meaning
with a modern gfortran. `prep.py` writes patched copies to `build/`, and the
originals are never touched.

```sh
# build (no local gfortran needed)
docker run --rm -v "$PWD/native":/w -w /w gcc:14 ./build.sh
# play in a UTF-8 terminal (Linux / WSL)
python3 native/play.py
# smoke-fuzz: N random sessions x M commands, reports crashes/hangs
docker run --rm -v "$PWD/native":/w -w /w gcc:14 python3 fuzz.py 1000 300
```

## What prep.py changes, and why

MS Fortran 5.x → gfortran compatibility:
- `$STORAGE:2`: `IMPLICIT INTEGER*2 (I-N)` per unit, bare `INTEGER`/`LOGICAL` → `*2`.
- `$INCLUDE` → `INCLUDE`. Ctrl-Z EOF markers stripped.
- Unit 0 = console in MS Fortran (stderr in gfortran): `READ/WRITE(LU|LX|0,…)` → `(*,…)`.
- `'AB'` compared with INTEGER*2 → numeric little-endian constant.
- `CO=ZNAK(n:n)` (char → int2, MS pads with a blank = +8192) → `ICHAR()+8192`.
- `REAL*8 C…` text-holder arrays → `CHARACTER*8` (REAL*8 alignment shifted COMMON).
- `\` edit descriptor → `$`; `form='binary'` → `access='stream'`; `IABS`/`MIN0`/… → generic.
- Lines padded to column 72 (continued string literals keep their spaces).
- `GETTIM` supplied in `compat.f90`.

Data fixes:
- Bytes 0x8F/0x92 left over from the Kamenický→CP852 conversion → Á/Í
  (showed as "ZĆPADNĚ" and broke vocabulary words like MŘÍŽE and KLÍČE).

Logic fixes for bugs in the 1988 DOS port (they worked by luck under DOS
and crash on a modern OS):
- `MMRL` (darkness check) and two inline `CALL SGLOD(NAME,LU,code)` calls
  were never converted from HP "load segment, never return" semantics. They
  recursed into segments and wrote through literal/garbage pointers. They now
  use the dispatcher convention (`p2=code; RETURN`).
- GO with a non-direction word indexed `IXT` far out of bounds → "no exit".
- ZAZNAMENAT (transcript to HP printer LU) swallows the LU answer.
- Non-numeric answers to integer prompts no longer abort, and EOF ends the game cleanly.
