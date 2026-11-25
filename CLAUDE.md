# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**Magical Mystery Mansion (MMM)** - a Czech-language text adventure game originally written in Fortran 77 for HP 1000 RTE-VI (1982–1983), later ported to MS-DOS by Stanislav Matýšek in 1988. Authors: P. Lampa, B. Michal, Stanislav Matýšek.

The game is a detective mystery set in a haunted castle with fantasy and sci-fi elements. The player explores rooms, collects items, solves puzzles, identifies a murderer, and finds treasure - all via text commands in Czech (e.g. `JÍT SEVERNĚ`, `VZÍT`, `PODÍVAT SE NA`).

## Build

The original 1988 build used Microsoft Fortran 5.x (`$STORAGE:2`, `$LARGE`, `$NODEBUG`; linker
response files `005308LK`, `MMM_LAT/006297LK`). That toolchain is proprietary and is **not** in
this public repo; never add it, nor DOS binaries (`*.EXE`, `*.OBJ`).

Today everything builds from `MMM_LAT/` with Docker only:
- `native/` - `prep.py` writes patched copies for gfortran (originals untouched), see `native/README.md`
- `wasm/` - same patched sources → f2c → Emscripten (three engines: `puvodni`, `cz`, `en`), see `wasm/README.md`
- `deploy/Dockerfile` - builds all engines and serves them with nginx; CI pushes `ghcr.io/archetypal-cz/mmm`

Every fix to game logic goes into `native/prep.py` (or the build scripts) and is logged in `OPRAVY.md`.
Never edit the original `.FOR` files.

## Architecture - Segment Structure

The game is organized as a main program + overlay segments, originally loaded dynamically via `SGLOD`. In the MS-DOS port, `SGLOD` (in `MMLB.FOR`) is a dispatch subroutine that calls segments by name.

### Source Files

| File | Segment | Purpose |
|------|---------|---------|
| `MMM.FOR` | Main | Program entry, BLOCK DATA (COMMON initialization: verbs, rooms, items, NPCs, exits, scoring) |
| `MMCO.FOR` | - | COMMON block declaration (included via `$INCLUDE` in every segment) |
| `MMLB.FOR` | - | Library: byte-level string access (`SGET`/`SPUT`), string search (`JSTR`), I/O (`REIO`), segment dispatcher (`SGLOD`), Czech↔ASCII conversion (`CZECH`, `CLCUC`), comparison (`ROVNA`) |
| `MMSA.FOR` | A | Initialization: mystery generation (pick scene, murderer, weapon), random items, magic words |
| `MMSB.FOR` | B | Main game loop: read player input, parse words, time/turn tracking, dispatch to verb handlers |
| `MMSC.FOR` | C | Movement verbs: GO, CLIMB, ENTER, DIG, FOLLOW, magic words |
| `MMSD.FOR` | D | Text output for movement (room descriptions on transition) |
| `MMSE.FOR` | E | Room descriptions (LOOK) |
| `MMSF.FOR` | F | Basic verbs: QUIT, SCORE, GET, DROP, GIVE, LOOK, READ, FIND, LIGHT, OPEN, CLOSE, HELP |
| `MMSG.FOR` | G | Text output for MMSF verbs |
| `MMSH.FOR` | H | Advanced verbs: EAT, RUB, BEAM, SWEEP, CALL, MOVE, ATTACK, KILL, SUSPEND, RESTORE, DRINK, LISTEN |
| `MMSI.FOR` | I | Text output for MMSH verbs |
| `MMSJ.FOR` | J | More verbs: UNLOCK, LOCK, SHOOT, OIL, RECORD, BREAK, PLACE, SLEEP |
| `MMSK.FOR` | K | Text output for MMSJ verbs |

### Key Data Structures (COMMON /MMBC/)

All game state lives in COMMON blocks shared across all segments:

- `IWRD(9,9)` - parsed player input words
- `IVRB(4,89)` - verb dictionary (89 Czech verbs, 4 words each = 8 chars)
- `IRNM(8,66)` - room names (66 rooms)
- `IROM(99)` - room data, encoded as `Z!XX!YY` (Z=curtain state, XX=points, YY=name index)
- `IRSN(4,17)` / `IRES(17)` - NPC names and data (17 NPCs: corpse, lady, butler, mole, vampire, forester, etc.), encoded as `XX/Y/ZZ` (health/strength/room)
- `IVEN(107)` - item data, encoded as `W/X/Y/ZZ` (owner/type/weight/room)
- `IXT(360)` - exit/entrance data (directions, door states)
- `ICLS(4,12)` - player rank classifications (12 levels)
- `ITST(40)` - game state parameters (turn number, current room, flags, etc.)
- `KLICE(15,8)` - key/clue tracking

### Character Encoding

The game uses a custom Czech character encoding (Kamenický brothers / CP895-like). Key routines in `MMLB.FOR`:
- `CZECH()` - converts Czech diacritics to ASCII equivalents for comparison
- `CLCUC()` - Czech-aware uppercase conversion
- `ROVNA()` - Czech-aware string comparison

## Directory Structure

- Root (`/`) - original 1988 sources, Kamenický encoding (history only; builds use `MMM_LAT/`)
- `MMM_LAT/` - later copy in Latin-2 (CP852); the build source
- `original_en/` - English Mystery Mansion reference sources (IF Archive)
- `native/`, `wasm/`, `deploy/` - see Build
- `MMMNAVOD.TXT` - player instructions; `CREDITS.md` authors; `OPRAVY.md` fix log; `TODO.md`

## Publishing

Public repo of the Archetypal nonprofit (archetypal-cz/mmm), live at https://mmm.archetypal.cz.
Commit messages and docs in Czech are fine. Code is MIT, new translation + essay CC BY-SA 4.0,
the game itself has no licence (see `COPYING.md`, `CREDITS.md`).

## Important Notes for Working with This Code

- All strings are packed as `INTEGER*2` arrays (2 bytes per word), accessed character-by-character via `SGET`/`SPUT` - never use direct character operations without these routines.
- Statement functions like `IFSD(I)`, `ITFD(I)`, `ITD(I)`, `IFD(I)` are digit-extraction helpers used to decode the packed numeric data in IROM, IRES, IVEN.
- The segment pattern (verb logic in MMS*.FOR, text output in next letter's MMS*.FOR) is consistent: C→D, E→E, F→G, H→I, J→K.
- Game text is in Czech throughout. Verb matching truncates to first 8 characters.
