# Zadání pro nový český překlad hlášek MMM

Překládáš hlášky české textové hry Magical Mystery Mansion (1982–88) do nové,
moderní češtiny. Engine (logika hry i slovník parseru) zůstává původní, mění se
jen texty, které hra vypisuje.

## Vstup
`wasm/cz/messages.json`: pole záznamů `{key, file, label, cz, en, new}`.
- `cz` je původní český text (šablona), `en` anglický originál Billa Wolperta
  (spárovaný podle čísla řádku; u čistě českých hlášek je prázdný).
- Šablona: `\n` = nový řádek, `{A8}`, `{4A2}`, `{I4}`, `{10A2}`... = hodnoty, které
  hra doplní (slovo hráče, číslo, jméno…), `{$}` = bez odřádkování (za ním píše hráč).

## Výstup
Zapiš JSON objekt `{ "KLÍČ": "nový text", ... }` do souboru, který ti určí zadání
(`wasm/cz/new/<skupina>.json`), pro **všechny** klíče svých souborů. Pak spusť
`python wasm/cz/validate.py wasm/cz/new/<skupina>.json` a oprav všechny nálezy.

## Pravidla stylu (rozhodl vydavatel)
1. **Vykání s malým v:** „vaše výbava“, „jste“, „vám“. Ne „Vaše/Vám“, ne tykání.
2. **Bez mužského rodu hráče:** žádné „spadl jste“, „získal jste“, „byl jste“.
   Přeformuluj: „Pád z pětisetsáhového útesu na skály nepřežijete.“, „Získáváte…“,
   „Doposud máte 47 bodů.“ Rod ostatních postav nevadí.
3. **Věrný smysl anglického originálu, přirozená čeština, humor originálu.**
   Překládej z angličtiny (`en`), starou češtinu (`cz`) ber jako pomoc a zdroj
   českých reálií. Kde stará čeština něco změkčila nebo spletla, drž se
   angličtiny (např. „power, lust and greed“ = „moc, chtíč a chamtivost“;
   „A COMPUTERIZED FANTASY, COMPLIMENTS OF BILL WOLPERT“ = počítačová fantazie,
   kterou vám věnuje Bill Wolpert).
4. **Čistě české hlášky** (prázdné `en`: veršované nadávky, dotaz na jméno,
   skloňovací drobnosti) zachovej co do obsahu, jen učeš češtinu a styl. Rýmy
   v nadávkách nech rýmovat.
5. Krátké, živé věty jako v dobrých textovkách. Žádné anglicismy, žádný úřední
   jazyk. Zachovej trochu dobového kouzla, ale bez kostrbatosti.

## Pravidla, která vyžaduje engine (validate.py je hlídá)
- **Stejné zástupné hodnoty ve stejném pořadí** jako v `cz` (ne jako v `en`, česká
  verze má jinde `{A8}` místo `{4A2}`). Věta musí dávat smysl s jakýmkoli
  doplněným slovem; slovo hráče (`{4A2}`) se nesklání: piš „Slovo {4A2} neznám.“,
  „{4A2} nemáte.“, „Ve výbavě teď máte i {4A2}“.
- **Každý řádek začíná mezerou** jako v originále (tisková konvence Fortranu).
  Prázdné řádky a úvodní prázdný řádek zachovej tam, kde jsou v `cz`.
- **Řádek nejvýš 72 znaků** včetně mezery na začátku (zástupné hodnoty se počítají
  svou šířkou). Dlouhé texty zalom ručně, rozumně po slovech, bez dělení slov.
  Na počtu řádků nezáleží, smí se lišit od originálu.
- Jen znaky z CP852: česká písmena ano; typografické uvozovky „“ ani … ani pomlčky
  – — ne. Používej rovné uvozovky "…" a tři tečky.
- `{$}` nech na konci přesně tam, kde je (za ním hráč píše odpověď).

## Slovník parseru: ZÁVAZNÝ
Hra rozumí jen slovům ze svého slovníku (`wasm/cz/glossary.md`). Předměty, osoby
a směry proto pojmenovávej **slovy ze slovníku** (lucerna, ne svítilna; lopatka,
ne rýč; LESNÍK, ne hajný), ve kterémkoli pádě (hra skloňování pozná). Když hláška
radí, co napsat (ANO/NE, OBNOVIT, ZAZNAMENAT KLÍČE, JÍT SEVERNĚ…), použij přesně
slovo ze slovníku a nech ho VELKÝMI písmeny. Směry v popisech místností piš
VELKÝMI písmeny (SEVERNĚ, VÝCHODNĚ, NAHORU…) jako originál: jsou to nápovědy a
webová verze je dělá klikací.

## Zvláštnosti
- `MMSA:100` je hlavička úvodu, `MMSA:101` jeho text; anglický originál má obojí
  v `MMSA:100`. Řádek `A r c h e t y p a l . c z` nech beze změny.
- `MMM:210`–`MMM:...` (soubor MMM) jsou názvy předmětů v seznamech („Vidíte:“,
  „Vaše výbava obsahuje:“) ve 4. pádě. Zachovej přesnou délku záznamu včetně
  doplňujících mezer na konci (sloupce), max. 19 znaků.
- Popisy místností (MMSE) mají jako `{A8}` dosazovaný směr (např. „Dveře vedoucí
  {A8} jsou jedinou cestou odsud.“), takže věta musí fungovat se slovem typu
  SEVERNĚ.
- Nevymýšlej nové herní informace ani nápovědy, které v originále nejsou.
