# Redakční čtení nového českého překladu (edit.json)

Prošel jsem všech 772 záznamů v `messages.json` (756 přeložených, 16 prázdných).
Zástupné znaky jsem porovnal se zdrojáky Fortranu (`MMM_LAT/*.FOR`), abych věděl,
co se do nich dosazuje. Výsledek: **42 změněných hlášek** v `edit.json`,
`validate.py` hlásí 0 problémů.

Celkově je překlad kvalitní a jednotný: všude je malé „v“, nenašel jsem žádný
mužský rod hráče (žádné „jste byl“, „byste mohl“), slova parseru zůstala zachována.
Změny jsou proto hlavně sjednocení a opravy gramatiky kolem dosazovaných slov.

## Zvolená terminologie

| pojem (EN) | zvolená podoba | poznámka |
|---|---|---|
| vampire | vampír | slovo parseru VAMPÍR; MMSD:262 měl „upír“. Položka v seznamu `upírův prsten` (MMM:210) zůstává, viz níže |
| troll / dwarf / elf | trpaslík / skřet / skřítek | už bylo jednotné |
| hatchet | sekerka | slovo parseru SEKERKA; MMSD:254 měl „sekyrkou“ |
| curtain | záclona | slovo parseru ZÁCLONA; MMSI:1152 měl „závěs“ |
| radio | rádio | MMSI:24012 měl „radio“ (parser pozná obojí, CZECH() diakritiku ignoruje) |
| magic word | magické slovo | MMSI:23360 měl „kouzelných slov“, všude jinde „magické“ |
| screaming demons | vřeštící démoni | podle fragmentů MMSD:286–289; MMSG:10210/10230 měly „vřískající“ |
| gauntlet | (dlouhé) rukavice, množné číslo | podle seznamu „dlouhé rukavice“; MMSG:10116 měl „s rukavicí“ |
| globe | glóbus, po prozkoumání křišťálová koule | parser zná GLÓBUS i KOULE (KOULE -> GLOBUS v MMSB) |
| bludgeon (trpaslíkův) | sukovice | MMSG:5889 měl „kyj“, což je ale i herní předmět KYJ |
| Mystery Mansion v textu | tajemný zámek / tajemné sídlo, malé „t“ | velké jen na patníku (MMSC:1099) a na začátku věty |
| murder weapon | „vražednou zbraní je {4A2}“ | MMSG:5760 měl „vražedná zbraň je“ |
| délky | imperiální jako originál: stopa, palec, míle; útes „500 stop“ | viz sporná místa |
| dungeon | hladomorna (místnost), klíček od vězení (předmět) | obojí jsou názvy ze hry, ponecháno |
| hodnocení | „Vaše hodnocení: {4A2} detektiv.“ | viz níže |

Opakované systémové hlášky jsou teď ve všech segmentech stejné:
„Nechápu, co mi chcete říct.“ (MMSD/G/I/K:320), „Nic se nestalo.“ (MMSD/G/I/K:520),
„Je tu tma.“ (MMSD/G/I/K:1150), „Záclona nepropouští světlo.“ (…:1152),
„Neumím číst myšlenky. Musíte mi říct, co chcete {4A2}“ (MMSG/K:1000).

## Systémové změny

### 1. Hodnocení hráče ({4A2})
Do `{4A2}` se dosazuje `ICLS` z `MMM.FOR`, což je 8 znaků doplněných mezerami:
`žalostný, ubohý, smolař, nováček, slušný, amatér, dobrý, štístko, obratný,
úžasný, mistr, superman`. Jde o přídavná jména a podstatná jména v přívlastku,
takže česky funguje jen pořadí **„{4A2} detektiv“** („žalostný detektiv“,
„mistr detektiv“). Pořadí „detektiv žalostný“ zní jako básnická inverze. Sjednotil
jsem MMSD:90020 na „Vaše hodnocení: {4A2} detektiv.“; MMSF:1010 už to tak měl.
Vedlejší efekt: u kratších hodností vyjdou mezi slovy dvě nebo tři mezery
(„smolař   detektiv“), stejně jako v původní verzi. Pokud to vadí, šlo by to
vyřešit jen ořezáním mezer v enginu nebo úpravou dat ICLS.

### 2. Pády kolem dosazovaných slov
Ve zdrojácích jsem ověřil, odkud se slovo bere. `IWRD` je slovo, jak ho hráč
napsal (v pádě, ve kterém ho napsal, obvykle 4. pád), `IRSN` je jméno postavy
v 1. pádě, `IDTN` je směr.
- MMSI:10040 / 10041 (`IWRD`, „ZABÍT VLKA“): „{4A2} je po smrti“ by dalo „VLKA je
  po smrti“. Nově „Podařilo se vám zabít / zranit {4A2}“, kde je místo pro 4. pád
  a hráč nemá rod.
- MMSK:16040 (`IRSN`, 1. pád): „Střílet na {4A2}“ by dalo „na VLK“. Nově
  „{4A2} není terč, na který by se vyplatilo střílet.“
- MMSG:8015, MMSK:8015 (`IWRD`, „KRMIT VLKA“): „Jídlo pro {4A2}“ / „Má ho v sobě
  {4A2}“ nefungovaly se 4. pádem. Nově „Krmíte {4A2}. Z vašeho jídla nezbyl ani
  drobek.“
- MMSI:8015 (`IRSN`, mohou to být i ženské postavy): „sežral“ mělo mužský rod a
  zvířecí sloveso. Nově „{4A2} spořádá všechno vaše jídlo.“ (přítomný čas, bez rodu).
- MMSI:8005, 8030 (sloveso POHNOUT, hráč může napsat i 7. pád „STOLEM“): slovo jsem
  dal za dvojtečku: „Nic takového nemáte a ani tu není: {4A2}“, „Nic takového
  tu nevidím: {4A2}“.

### 3. Jednotky
Svíčka měla na oltáři (MMSE:216) „{I3} palců“, ale při prohlédnutí (MMSG:5210)
„{I2} cm“, přitom engine dosazuje stejné číslo (v originále obojí INCHES). Sjednotil
jsem na palce. Lano je teď „50 stop“ (místo „15 metrů“), klín „stopu dlouhý“
(místo „30 centimetrů“), což ladí s křížkem „asi stopu dlouhý“ (MMSE:316). Útes byl
napůl „500 stop“ (MMSE:135/195/196) a napůl „pětisetsáhový“ (MMSD:1026, MMSF:5152,
MMSI:23070). Sjednotil jsem na stopy, jak to má originál (500 FOOT).

### 4. Drobnosti
MMSB:268 „a byl pryč“ -> „a zmizel“; MMSC:930 doplněna čárka; MMSE:125 „Pokoj je
velmi dívčí“ -> „ryze dámský“ (VERY FEMININE, dvojice k „ryze pánský“ v MMSE:120);
MMSE:140 „se dobře chodí, zvlášť na druhý konec“ -> „se po ní snadno chodí,
hlavně k druhému konci“; MMSG:5130 „koule je ve skutečnosti … koule“ -> „glóbus je
ve skutečnosti … křišťálová koule“; MMSI:24010 „Napůl slyšíte, ale hlavně cítíte“ ->
„Spíš než slyšíte, vnímáte“; MMSG:5781 nápis na talismanu dán do uvozovek jako
ostatní nápisy; MMSG:3045 „záhadného sídla“ -> „tajemného sídla“; MMSI:9030
„stvořitel této hry“ -> „autor hry“ (jednotně s MMSI:9040; zachován řídicí znak BEL).

## Sporná místa k rozhodnutí autora

1. **Stopy, nebo sáhy?** Příklad v BRIEF.md používá „pětisetsáhový útes“. Sáh
   (asi 1,8 m) dává útes vysoký 900 m, proto jsem zvolil stopy podle originálu. Pro
   návrat k sáhům stačí v MMSD:1026, MMSF:5152 a MMSI:23070 vrátit „pětisetsáhov-“
   a v MMSE:135/195/196 přepsat „500 stop“. Vzdálenost k bráně „{I4} sáhů“ (MMSE:235,
   v originále YARDS) jsem nechal; „yardů“ by byl anglicismus.
2. **Dvojité mezery u hodnosti** („nováček  detektiv“), viz bod 1. Hodnosti jsou
   navíc směsice přídavných a podstatných jmen („štístko detektiv“ zní zvláštně).
   Lepší by byla úprava dat `ICLS` v MMM.FOR (např. jen přídavná jména), to už ale
   není text hlášek.
3. **`upírův prsten`** (MMM:210, seznam předmětů) proti „vampír“ všude jinde.
   Nechal jsem ho, protože je to položka seznamu se zarovnáním a parser chce jen
   PRSTEN. Šlo by „vampýrův prsten“ / „vampírův prsten“ (17 znaků, vejde se).
4. **Urážky rodu mužského** (MMSD:1602–1640: „cizinče“, „syčáku“, „blbečku“, „terénní
   krůto“). Jde o oslovení podstatným jménem, ne o rod hráče, a pocházejí
   z původní české verze. Ponechal jsem je.
5. **MMSG:4050 „Díky. Tady se to hodí.“** proti MMSI/MMSK:4050 „Díky. Tady to bylo
   potřeba.“ Originál je stejný, ale MMSG patří k jinému slovesu (DÁT), takže
   přítomný čas tam sedí. Nechal jsem obě znění.
6. **MMSD:176** „Vrátit se bylo třeba, dokud byl čas.“ je trochu knižní (rodově
   neutrální „Měli jste se vrátit“ nejde). Nechal jsem to.

## Odchylky překladatelů, které jsem posoudil a ponechal

- **MMSG:5042–5048: `{A1}` přesunuté na konec věty** („Má nějaká zranění.{A1}“).
  Je to správně. Tyto WRITE ve Fortranu nepředávají žádnou hodnotu, takže výstup
  skončí na prvním `A1`. Přesun zajistí, že se vypíše celá věta. U 5032–5040, kde se
  `{A1}` skutečně dosazuje (a/mezera), zůstalo uvnitř slova.
- **MMSD:287, MMSD:80024 a fragmenty 286–289, 80025/80026**: mezera přesunutá za
  zástupný znak / na začátek fragmentu. Napojení sedí („stojí 3 vřeštící démoni“,
  „máte prsten.“).
- **MMSE:108/208/308, 116/216/316/416, 125/225/325/425, 184/284/384/484**: tečky a
  napojení fragmentů jsou přeskládané. Prošel jsem všechny kombinace a věty
  navazují.
- **MMSA:100** doplněná věta „Počítačová fantazie, kterou vám věnuje Bill Wolpert.“
  (podle BRIEF). **MMSF:1010** doplněná věta „Pokud vám šlo jen o skóre, napište
  příště SKÓRE.“ (podle EN). **MMSI:2090** doplněná druhá věta o Aladinovi (podle EN).
- **MMSG:9044**: „JÍT SEVERNĚ“ změněno na „JÍT ZÁPADNĚ“ podle EN. Hra začíná na cestě
  vedoucí na východ a na západ, takže je to správně.
- **MMSE:401** „polní lůžko“ místo „dětská postýlka“ (COT), **MMSE:351** „napůl ptáka“,
  **MMSE:352** „kraby“ místo „štírů“, **MMSE:151** trpaslík „by se do východu nevešel“.
  Vše odpovídá originálu (ověřeno v `original_en/garnett-19.2/mansion.c`).
- **MMM:270 „pirátskou pokladnu“, MMM:450 „čarodějné koště“, MMM:540 „mastnou
  sekerku“, MMM:580 „12 zlatých mincí“, MMM:1010 „nic“**: zarovnání sedí (max. 19
  znaků) a parser chce jen POKLADNA / KOŠTĚ / SEKERKA / MINCE.
- **Přímá řeč** „…,“ říká X. Čárka uvnitř uvozovek odpovídá české normě. Ponecháno.

## Hlášky bez překladu (16)

MMSH:9032 a MMSJ:21230, 21236, 21240–21243, 21250–21252, 21261–21264, 21266, 21267
obsahují jen řídicí sekvence terminálu HP (ESC A/D/h/m/J, `&dF`, `&dB`, `&f2…k…L`)
a mezery. Text v nich není, prázdný překlad je správně. MMSJ:21210 začíná sekvencí
ESC J („\x1bJZadejte“). „J“ tam tedy není překlep a nechal jsem ho.

## Rozhodnutí autora (24. 11. 2026)
1. Jednotky: stopy (jako originál), sáhy ne.
2. Hodnosti přepsány na přídavná jména podle anglického originálu (wasm/cz/ranks.json,
   dosazuje prep.py do DATA CCLS jen pro CZ variantu): žalostný, ubohý, smolný, učňovský,
   slušný, laický, dobrý, šťastný, obratný, úžasný, mistrný, dokonalý. Zarovnány doprava,
   takže výplň mezerami je za "hodnocení:" a ne uprostřed "… detektiv".
   MMSF:1010 přeskládána, aby hodnost byla na jednom řádku s "Vaše hodnocení:".
3. "vampírův prsten" místo "upírův prsten" (MMM:210).
4. Nadávky v mužském rodě (oslovení ze staré CZ verze) zůstávají.
