# Credits

> Autoři a zdroje. Podklad pro stránku „O hře“ na webu. Věci, které nejsou ověřené, jsou
> označené **(?)**. Doplnění a opravy vítáme (issue, nebo predseda@archetypal.cz).

## Česká verze: Magical Mystery Mansion (MMM), 1982–1988

| Kdo | Co | Kdy |
|---|---|---|
| **P. Lampa** | překlad a úprava pro HP 1000 (RTE-VI), revize konstrukcí, nové reakce | 10/1982 – 02/1983, další revize do 1985 |
| **B. Michal** | spoluautor české verze pro HP 1000 | 1982 – 1983 |
| **Stanislav Matýšek** | převod pod MS-DOS (Microsoft Fortran), česká verze s diakritikou | 02/1988 (MS-DOS), 06/1988 (česká verze) |

Stopy v původních zdrojácích (`MMM.FOR`, `MMSA.FOR`):
- Historie úprav: 10-10-1982 až 18-02-1983 P. Lampa a B. Michal; 17-06-1983 SGLOD na
  tahání segmentů; 05-07-1983 revize všech konstrukcí; 23-11-1984 oprava RMPAR pro RTE-VI;
  06-09-1985 opravy starších chyb a přidán `URAN`; 04-02-1988 přenos pod MS-DOS (S. Matýšek);
  27-06-1988 česká verze.
- Titulek hry: „Záhada č. N. Revize 17 – 8826 P.L.+B.M.+S.M.“
- Hlavička v úvodu: v Kamenického verzi (kořen repa) **„JZD AK Slušovice“**, ve verzi
  Latin-2 (`MMM_LAT/`) **„FUTURE Engineering, s. r. o.“**
- Revize 17 sedí s revizí anglických zdrojáků „rev 17“, ze kterých vychází i VMS port.
- **`IWORD`** v `MMSB.FOR` je funkce, která v hráčově větě najde slovo i ve skloněném tvaru
  (odtrhne koncovku a zkouší pádové koncovky). Podpis „17-09-1982 MAN … BM“ ukazuje na
  B. Michala. Je to česká přidaná hodnota, anglický originál skloňování nepotřeboval.
  Díky ní hra rozumí „VZÍT LOPATKU“ i „PODÍVAT SE NA KRTKA“.

**Doplnit:** kdo byli P. Lampa a B. Michal (pracoviště, kde stál HP 1000), příběh převodu
do DOSu, fotky, vzpomínky, kde se hra hrála a šířila.

## Webové vydání (2026)

| Kdo | Co |
|---|---|
| **Jaromír „Kerray“ Matýšek** | režie: co a jak hra na webu umí, pravidla nového překladu, esej, kontrola výsledku |
| **Claude Opus 5.5** (Anthropic) | provedení: nativní a WebAssembly build, opravy portu (OPRAVY.md), webové rozhraní, mapa, nový překlad podle zadání, rešerše zdrojů |
| **spolek Archetypal** | vydavatel, provoz [mmm.archetypal.cz](https://mmm.archetypal.cz) |

## Nový český překlad (2026)

Webové vydání nabízí vedle původní české verze i nový překlad hlášek (přepínač „CZ“).
Vychází z anglického originálu a z původní češtiny. Pravidla určil vydavatel (vykání,
bez mužského rodu hráče, věrnost originálu, slovník parseru beze změny). Texty připravili
jazykoví modelové „překladatelé“ podle společného zadání (`wasm/cz/BRIEF.md`), jejich
výstupy prošly automatickou kontrolou (`wasm/cz/validate.py`) a 600 náhodnými hrami.
Engine, slovník i herní logika zůstávají původní.

## Anglický originál: Mystery Mansion, 1978–1981

- **Bill Wolpert**: autor, Naval Undersea Warfare Engineering Station (NUWES), Keyport,
  Washington. Fortran IV pro HP 1000 pod RTE. Úvod hry: „Fantasy, compliments of
  Bill Wolpert – NUWES Keyport, WA.“
- Port na **HP 3000** (MPE): autor neznámý. Zdroják kolovaj na páskách Interex CSL
  (Contributed Software Library); **John Rasins** byl přispěvatel CSL u jedné z raných
  revizí, ne autor.
- **Bob Sorem**: verze adaptovaná z HP 1000 programu **(?)** (zmínka v rešerši, ověřit)
- **James Garnett**: port do C/Unixu, verze 17 až 19.4 (1998–2000), BSD licence.
  Poděkování v jeho THANKS: Jeff Greenberg (pokus o port do BASICu a zdroják Fortran IV),
  Roy Buzdor (druhá kopie zdrojáku a sémantika syscallů HP 1000), Tracy Johnson (přístup
  k HP 3000 s originálem), Mark von Minden a Alia Rayl (testování), Rebecca Zane (opravy
  chyb pamětnice hry).
- **Ken Cornetet**: balíček *mansion-fortran*, „the real deal“, Wolpertův původní Fortran
  pro HP 1000 s minimem úprav (segmenty jako podprogramy, ošetření pořadí bajtů),
  převedený přes f2c, s DOS a Win32 binárkami. Jeho pomocné C rutiny (`STUBS.C`) vydal
  výslovně jako public domain. Na nich stojí anglická verze našeho webu (`wasm/en/`).
- **mmm17**: port rev 17 na VMS, včetně `solution.txt` (průchod na 999 bodů, záhada č. 46).
  Autor **(?)** (doplnit)

### Odkazy

- Wikipedia: [Mystery Mansion (video game)](https://en.wikipedia.org/wiki/Mystery_Mansion_(video_game))
- The Renga in Blue (Jason Dyer): [Mystery Mansion (1978)](https://bluerenga.blog/2011/05/04/adventure-4-mystery-mansion-1978/),
  [Mystery Mansion: The end](https://bluerenga.blog/2011/06/01/mystery-mansion-the-end),
  [všechny články](https://bluerenga.blog/tag/mysterymansion)
- Lysator: [Mainframe adventures](https://lysator.liu.se/adventure/Mainframe_adventures.html)
- MobyGames: [Mystery Mansion](https://www.mobygames.com/game/32469/mystery-mansion/)
- Solution Archive: [Mystery Mansion](https://solutionarchive.com/game/id%2C1514/Mystery+Mansion.html)
- IF Archive: [mansion-19.2.tar.gz](https://ifarchive.org/if-archive/games/source/mansion-19.2.tar.gz)
  (Garnett, C + Fortran rev 17), [mansion-fortran.zip](https://ifarchive.org/if-archive/games/source/mansion-fortran.zip)
  (Wolpertův HP 1000 Fortran), [mmm17.tar.gz](https://ifarchive.org/if-archive/games/vms/mmm17.tar.gz) (VMS port),
  [návod](https://ifarchive.org/if-archive/solutions/Mystery_Mansion.txt)
- Garnettova původní stránka (archiv): http://www.catbelly.com/src/mm/mystery.html

## Licence a svolení autorů

Od 25. 11. 2026 hru vydává spolek Archetypal veřejně, nekomerčně a s uvedením všech
autorů. Výslovné svolení zatím nemáme, proto:

- **Anglický originál** (Wolpertův Fortran, cca 1980) leží na IF Archive **bez jakékoli
  licence**. Hra se desítky let volně šířila (Interex CSL, IF Archive), ale to není svolení.
- **Garnettův copyright (BSD)** platí jen pro jeho přepis do C (`original_en/garnett-19.2/`),
  ne pro hru samotnou. Jeho kód ve webové verzi nepoužíváme, je tu jako reference.
- **Česká verze** má vlastní autory (P. Lampa, B. Michal, Stanislav Matýšek). Za rodinu
  S. Matýška hru vydává jeho syn Jaromír Matýšek; P. Lampu a B. Michala hledáme.

Co děláme:
1. Hledáme autory a dědice (Wolpert; P. Lampa, B. Michal) a prosíme je o svolení.
2. Uvádíme všechny autory a zdroje (tento soubor, web, esej).
3. Kdo k hře má práva a nesouhlasí, napíše na predseda@archetypal.cz a hru stáhneme.

Licence toho, co vzniklo v roce 2026, je v `COPYING.md`.
