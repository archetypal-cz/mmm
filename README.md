# Magical Mystery Mansion

Česká textová detektivka z let 1982-1988, která po skoro čtyřiceti letech běží v prohlížeči.
Původní kód ve Fortranu, přeložený do WebAssembly.

**Hraj na [mmm.archetypal.cz](https://mmm.archetypal.cz)** · esej
[Dvě písmena do jednoho slova](https://mmm.archetypal.cz/esej.html)

> *English summary below.*

Jsi v zámečku, kde se stala vražda. Prozkoumáváš pokoje, sbíráš předměty, luštíš hádanky,
hledáš vraha, zbraň a poklad. To vše příkazy jako `JÍT SEVERNĚ`, `VZÍT LOPATKU` nebo
`PODÍVAT SE NA KRTKA`. Každá hra je jiná záhada: na začátku se náhodně vybere místo činu,
vrah i zbraň.

Na webu jsou tři verze téže hry:

| Přepínač | Co to je |
|---|---|
| `?lang=puvodni` | **Původní čeština** z roku 1988, přesně jak se hrála na PC pod DOSem |
| `?lang=cz` | **Nový český překlad** hlášek (2026), stejný engine, slovník i logika |
| `?lang=en` | **Anglický originál** *Mystery Mansion* Billa Wolperta (1978-1981) |

K tomu klikací panel s předměty, automapa prozkoumaných místností, klasický nebo moderní
styl, uložené hry v prohlížeči a instalace jako aplikace (PWA), která hraje i offline.
Server nic neukládá a nic nesleduje: je to jen pár statických souborů.

Projekt vede spolek [Archetypal](https://archetypal.cz). Hru do DOSu v roce 1988 převedl
Stanislav Matýšek. Webovou podobu z roku 2026 režíroval jeho syn Jaromír „Kerray“ Matýšek,
předseda spolku, a prováděl ji Claude Opus 5.5 (Anthropic).

## Historie po vrstvách

Repozitář je složený tak, aby šlo vrstvy hry číst v historii gitu jednu po druhé
(`git log --reverse`):

1. **Původní zdrojáky** (autorské datum 27. 6. 1988) - Fortran 77 pro MS-DOS v kódování
   bratří Kamenických. Do češtiny hru 1982-1985 převedli a upravili P. Lampa a B. Michal
   na počítači HP 1000 (RTE-VI), pod MS-DOS (Microsoft Fortran 5.x) ji v roce 1988 přenesl
   Stanislav Matýšek.
2. **`MMM_LAT/`** - pozdější kopie téže verze v kódování Latin-2 (CP852). Z ní se staví vše
   další.
3. **`original_en/`** - anglický originál a jeho potomci z IF Archive: Wolpertův Fortran IV
   pro HP 1000, port pro VMS s průchodem na 999 bodů, C port Jamese Garnetta.
4. **`native/`** - build v moderním gfortranu. `prep.py` přepíše kopii zdrojáků pro dnešní
   překladač a opraví chyby portu z roku 1988, které pod DOSem fungovaly náhodou.
   Originály zůstávají nedotčené; všechny zásahy popisuje [`OPRAVY.md`](OPRAVY.md).
5. **`wasm/`** - stejné opravené zdrojáky přes f2c do C a Emscriptenem do WebAssembly.
   Herní smyčka je původní a jen asynchronně „čeká“ na příkaz ze stránky. Nad ní webové
   rozhraní (`wasm/web/`).
6. **`wasm/en/`** - anglický originál stejnou cestou.
7. **`wasm/cz/`** - nový český překlad 756 hlášek (zadání, slovníček, validace).
8. **Esej** `wasm/web/esej.html` - jak se celý zámek vešel do 64 kilobajtů.
9. **`deploy/`** - statický obraz s nginx a GitHub Actions do GHCR.
10. **Dokumentace** - tento soubor, [`CREDITS.md`](CREDITS.md), [`TODO.md`](TODO.md).

Co v repozitáři záměrně není: překladač a linker Microsoft Fortranu, se kterými se hra
v roce 1988 překládala, a zkompilované `MMM.EXE`. Jde o cizí proprietární software. Nejsou
tu ani slepé uličky z jara 2026 (pokus o LFortran a ruční přepis do TypeScriptu). Cesta
přes f2c a Emscripten nakonec dala bajtově shodný výstup s nativním buildem, takže přepis
nebyl potřeba.

## Spuštění u sebe

Nejjednodušeji hotový obraz:

```sh
docker run --rm -p 8080:80 ghcr.io/archetypal-cz/mmm:latest
# http://localhost:8080/
```

Build ze zdrojáků (pár minut, stahuje libf2c z netlib.org):

```sh
docker build -f deploy/Dockerfile -t mmm .
docker run --rm -p 8080:80 mmm
```

nebo `docker compose -f deploy/compose.yml up -d --build`.

Vývoj bez obrazu, jednotlivé enginy a testy (nativní build, fuzz, rozdílový test
nativní × WASM, testy v prohlížeči) popisují [`native/README.md`](native/README.md)
a [`wasm/README.md`](wasm/README.md). Stačí Docker, gfortran ani emsdk instalovat nemusíš.

## Jak přiložit ruku k dílu

- **Chyba ve hře nebo na webu:** založ issue, ideálně s číslem záhady (ukáže se na začátku
  hry) a posledními příkazy.
- **Vzpomínky, fotky, kontakty:** hledáme lidi, kteří MMM hráli na HP 1000 nebo v DOSu,
  a hlavně P. Lampu a B. Michala nebo jejich blízké. Napiš na
  [predseda@archetypal.cz](mailto:predseda@archetypal.cz).
- **Opravy překladu:** `wasm/cz/` má vlastní zadání (`BRIEF.md`) a validátor; pull request
  vítáme.

## Licence a autorská práva

Hra samotná - anglický originál Billa Wolperta i česká verze P. Lampy, B. Michala
a Stanislava Matýška - **není pod žádnou volnou licencí**. Vydáváme ji nekomerčně,
s uvedením všech autorů, a s žádostí o svolení se snažíme autory a jejich dědice zastihnout
(stav v [`CREDITS.md`](CREDITS.md)). Pokud k ní máš práva a nesouhlasíš s tím, napiš na
[predseda@archetypal.cz](mailto:predseda@archetypal.cz) a hru stáhneme.

Co vzniklo pro webové vydání v roce 2026:

| Co | Licence |
|---|---|
| Kód (`native/`, `wasm/` kromě níže uvedeného, `deploy/`, skripty) | [MIT](LICENSE) |
| Nový český překlad (`wasm/cz/messages.json`, `wasm/cz/new/`, `wasm/cz/glossary.md`) a esej (`wasm/web/esej.html`) | [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/deed.cs) |
| `original_en/garnett-19.2/` | BSD, Copyright © 1999-2000 James Garnett (viz `COPYRIGHT` tamtéž) |
| `wasm/en/en_runtime.c` | vychází z `STUBS.C` Kena Corneteta, vydaných jako public domain |

Podrobně v [`COPYING.md`](COPYING.md).

---

## English summary

*Magical Mystery Mansion* is the Czech adaptation (HP 1000, 1982-1985; MS-DOS, 1988) of
Bill Wolpert's mainframe adventure *Mystery Mansion* (1978-1981), ported to MS-DOS by
Stanislav Matýšek. The 2026 web edition was directed by his son Jaromír "Kerray" Matýšek
and carried out by Claude Opus 5.5 (Anthropic). This repository brings
the original Fortran sources to the browser via f2c and Emscripten, with no rewrite: the
1988 game loop runs unchanged in WebAssembly. Three engines: the original Czech, a new
Czech translation, and Wolpert's English original. Play at
[mmm.archetypal.cz](https://mmm.archetypal.cz).

The git history is arranged in layers (original sources dated 1988 → Latin-2 copy →
English reference sources → native gfortran build → WebAssembly → new translation →
deployment). The Microsoft Fortran toolchain and DOS binaries are deliberately left out.

The game itself is published non-commercially with full credits while we try to reach
its authors and their heirs; rights holders who object can write to
predseda@archetypal.cz and we will take it down. Code written in 2026 is MIT; the new
Czech translation and the essay are CC BY-SA 4.0. See [`COPYING.md`](COPYING.md).
