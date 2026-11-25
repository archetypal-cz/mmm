# Nápady a otevřené úkoly

## Přepnutí jazyka uprostřed hry (bez restartu)
- **PŮVODNÍ CZ ↔ nový CZ:** stejný engine i stejné rozložení paměti (COMMON /MMBC/),
  takže stačí uložit stav, spustit druhý engine a stav načíst (ZAPSAT/OBNOVIT
  potichu, nebo přímo zkopírovat paměť). Skoro triviální.
- **CZ ↔ EN:** svět i význam dat jsou totožné (IROM, IRES, IVEN, IXT, ITST),
  ale liší se pořadí polí v COMMON, formát uložené hry (EN ukládá IVRBX(32), IROM,
  IRES, IVEN, IXT, ITST), číslo revize (16 vs 17), příznak `'MM'` v ITST(38) a
  kouzelná slova uložená v tabulce sloves. Šlo by to převodníkem uložené hry
  mezi formáty, ale chce to důkladné testování (stejná záhada, stejný tah).

## Mapa
- Ruční doladění rozložení: soubor s úpravami pozic pro gen_layout.py
  (větrná veranda, les a park s chrliči).
- Panel a mapa i pro anglickou verzi (jiné rozložení COMMON, anglický slovník).

## Nový český překlad
- Názvy předmětů v panelu brát z nového překladu (teď z původní CZ).
- Případně upravit i slovník parseru (synonyma), ne jen hlášky.

## Testy a historie
- Přeložit anglický průchod na 999 bodů (vms-mmm17/solution.txt, záhada č. 46)
  do češtiny a přehrát ve všech verzích.
- Podrobné srovnání kódu CZ vs EN do OPRAVY.md (co přinesly revize 1983–85).

## Autoři a svolení
- Najít P. Lampu a B. Michala (pracoviště s HP 1000, kolegové) a Billa Wolperta nebo
  jeho dědice; poprosit o svolení (stav v CREDITS.md).
- Stránka „O hře“ na webu z CREDITS.md.
