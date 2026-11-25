# Magical Mystery Mansion: deník chyb, oprav a odchylek

*Záznam o tom, co se v roce 2026 našlo ve fortranových zdrojácích MMM při jejich
převodu na dnešní počítače: co v DOS verzi z roku 1988 nefungovalo, proč, a jak je
to opravené.*

## Jak se to zkoumalo

Zdrojáky (`MMM_LAT/*.FOR`, verze s kódováním Latin-2 / CP852) jsme přeložili
dnešním kompilátorem GNU Fortran. Originály zůstaly nedotčené: úpravy dělá skript
`native/prep.py` na kopiích, takže každá změna je dohledatelná a vysvětlená.

Hru pak automaticky hrálo tisíc „hráčů“ s náhodnými příkazy, každý zhruba 300 tahů.
Dnešní operační systém hlídá paměť přísněji než DOS: když program sáhne, kam nemá,
okamžitě ho zastaví a ukáže, na kterém řádku to bylo. Díky tomu vyplavaly chyby,
které v DOSu tiše dělaly neplechu, ale málokdy program shodily. Hledat je v
traktorovém výpisu bylo skoro nemožné.

Na začátek to hlavní: většina chyb nevznikla nedbalostí. Jsou to místa, kde se
setkala dvě různá pojetí programu, segmenty na HP 1000 a podprogramy v DOSu, a
převod tří z nich přehlédl.

---

## 1. Kontrola tmy (MMRL) se místo návratu zanořila do hry

**Kde:** `MMM.FOR`, podprogram `MMRL`, volání `CALL SGLOD(NAME,LU,1150,…)`.

**Pozadí:** Na HP 1000 pod RTE znamenalo `CALL SGLOD(NAME,…)` „nahraj segment
a pokračuj v něm“. Odtud se už nikdy nevracelo. Při převodu do DOSu se z `SGLOD`
stal dispečer: segment nastaví parametry (`p2=kód zprávy`), udělá `RETURN` a hlavní
smyčka zavolá další segment. Takhle přepsaných jsou stovky míst, často se starým
voláním ponechaným v komentáři. V `MMRL` ale volání zůstala aktivní:

```fortran
      IF (IR .GT. 53) CALL SGLOD(NAME,LU,1150,1,0,0)
      ...
      CALL SGLOD(NAME,LU,1150,0,0,0)
```

**Co se dělo v DOSu:** Ve tmě se zavolal textový segment a vypsal „Je zde tma“
(nebo „Slunce již zapadlo“, „Záclony brání světlu“). Pak se ale vrátil zpátky do
`MMRL` a ta pokračovala dalším řádkem. Mezitím už textový segment přepnul `NAME`
na čtecí segment, takže poslední bezpodmínečné `CALL SGLOD(...)` spustilo
**celou herní smyčku (MMSB) znovu, vnořenou do rozpracovaného tahu**. Akce, která
se měla kvůli tmě přerušit (vzít, podívat se, číst…), zůstala viset na zásobníku a
mohla doběhnout později.

Druhý, zákeřnější efekt: textové segmenty si na konci nulují parametry (`p2=0`,
`p3=0`, …). Tady jim ale volající předal **konstanty** `1150, 1, 0, 0`. Fortran
předává parametry odkazem, takže segment zapisoval přímo do paměti, kde ležely
konstanty programu. DOS to dovolil. Pravděpodobný důsledek: konstanta 1150 se
přepsala na 0 a hláška o tmě se příště vůbec nevypsala.

**Oprava:** `MMRL` vrací výsledek (`MMRL(IDARK)`: 0 = světlo, 1–3 = tma a která
hláška). Každé z osmi míst, která ji volají (MMSB, MMSH, MMSJ), při tmě nastaví
`p2=1150`, `p3=varianta` a udělá `RETURN`. Je to přesně tentýž vzor, jaký DOS verze
používá všude jinde, například v MMSC pro stejnou hlášku.

## 2. Amulet v chodbě se svírajícími stěnami

**Kde:** `MMSC.FOR`, řádek ~660:

```fortran
      IF ((IR.GT.35).AND.(IR.LT.40).AND.(IVEN(17).GT.20000)
     1.AND.(ITST(12).LE. 0)) CALL SGLOD(NAME,LU,1084)
```

**Co se dělo:** Stejný druh přehlédnutí jako u bodu 1. Hráč s amuletem v chodbě,
kde se svírají stěny, dostal hlášku „Amulet Vás zachránil od rozmačkání stěnami“ a
segment ho přesunul do místnosti 4 (`ITST(1)=4`). Pak se ale MMSC vrátila a
**pokračovala ve zpracování pohybu**, jako by se nic nestalo. Navíc tu `SGLOD` dostal
jen 3 parametry ze 6. Segment přesto zapsal nuly do `p3`, `p4`, `p5`, tedy na
náhodná místa v paměti, podle toho, co zrovna leželo na zásobníku.

**Oprava:** `IF (…) THEN / p2=1084 / RETURN / ENDIF`.

## 3. „Před Vámi nejsou dveře.“ … a pak se stejně pokračovalo

**Kde:** `MMSF.FOR`, řádek ~1336, sloveso OTEVŘÍT/ZAVŘÍT u dveří:

```fortran
      IF (IC.EQ.4.OR.IC.EQ.13.OR.IC.EQ.22.OR.J.EQ.62.OR.J.EQ.70.OR.J
     1.EQ.133.OR.J.EQ.182.OR.J.EQ.190) CALL SGLOD(NAME,LU,15030)
```

**Co se dělo:** Hláška „Před Vámi nejsou dveře.“ se vypsala, ale program běžel
dál a mohl s „nedveřmi“ dál pracovat. Mezitím už byl přepnutý segment, takže další
hláška šla jinam. A znovu zápis do tří nepředaných parametrů.

**Oprava:** Stejná jako u bodu 2.

## 4. Pohyb s nesmyslným slovem sahal mimo tabulku východů

**Kde:** `MMSC.FOR`, hledání východu:

```fortran
      DO 1010 J=IC*60-59,IC*60
      IF (IFSD(IXT(J)).EQ.IR) GOTO 1015
```

**Co se dělo:** `IC` má být směr 1–6. Tabulka `IXT` má 360 položek, 6 směrů po 60.
Příkaz typu `VSTOUPIT DVEŘE se HODINKY` ale prošel až sem s druhým slovem, které
není směr (kód 92 = dveře). Přes přepočet relativních směrů (vlevo, vpravo…) vyšlo
`IC=83` a program prohledával `IXT(4921)` až `IXT(4980)`, tedy paměť daleko za
koncem tabulky. V DOSu skoro vždy nic neodpovídalo a hráč dostal „Tímto směrem
nemůžete jít“. Jen kdyby se v cizí paměti náhodou trefilo číslo místnosti, hráč by
se teleportoval.

**Oprava:** Je-li `IC` mimo 1–6, rovnou platí „Tímto směrem nemůžete jít“ (hláška 1105).

## 5. Neinicializované parametry hlavního programu

**Kde:** `MMM.FOR`, hlavní program:

```fortran
      p1=0
1     CALL SGLOD(NAME,p1,p2,p3,p4,p5)
```

`p2`–`p5` se nikde nenastavují. MMSA si z `p3` bere číslo jednotky pro „opis
pokecu“ (`ITST(21)=IPR(3)`). V DOSu byla paměť nejspíš vynulovaná, takže to vyšlo.
Na dnešním systému tam bylo smetí a **každá věta se vypsala dvakrát**: jednou na
obrazovku, jednou „do opisu“.

**Oprava:** Lokální proměnné se nulují a jsou statické, stejně jako v MS Fortranu.

## 6. Pokažená písmena v Latin-2 verzi

**Kde:** 26 míst v `MMM_LAT/` (MMM, MMSB, MMSD, MMSE, MMSG, MMSI).

Při převodu z kódování bratří Kamenických do Latin-2 zůstaly dva znaky
nepřevedené: bajt 0x8F (v Kamenických **Á**, v Latin-2 **Ć**) a 0x92 (místo **Í**).
Na obrazovce to dělalo „ZĆPADNĚ“, „HĆK“, „OTEVŘĺT BRĆNU“, „SKŘĺTEK“. Horší je, že
to postihlo i **slovník**: předmět `MŘĺŽE` a seznam programových klíčů
(`ZAZNAMENAT KLĺČE`). Když hráč napsal správně MŘÍŽE, porovnání přes `CZECH()`
nesedělo, protože 0x92 v převodní tabulce není.

V kamenické verzi (kořen repozitáře) chyba není. Vznikla až při pozdějším převodu
kopie.

**Oprava:** 0x8F → Á, 0x92 → Í.

## 7. Drobnosti

- **ZAZNAMENAT** se ptá „zadejte lu #“, tedy na číslo logické jednotky tiskárny na
  HP 1000, kam se měl tisknout opis hry. V DOSu (a dnes) to nemá kam vést. Odpověď
  se teď přečte a zahodí a opis zůstane na obrazovce.
- **Číselné dotazy** (číslo záhady, odpověď u ZAZNAMENAT): odpověď, která nebyla
  číslo, ukončila program chybou runtime knihovny. Teď se bere jako 0.
- **Konec vstupu** ukončí hru čistě, bez chybové hlášky.

## 8. Zahradní branka otevírala hlavní bránu

**Kde:** `MMSF.FOR`, řádek ~1427, sloveso OTEVŘÍT/ZAVŘÍT BRÁNU:

```fortran
      IF (((IR .EQ. 95) .OR. (IR .EQ. 55)) .AND. (IWRD(6,1) .EQ. 32))
      ...
      IF (((IR .EQ.95) .OR. (IR .EQ. 55)) .AND. (IWRD(6,1) .EQ.33))
```

**Pozadí:** Zahradní branka stojí mezi místnostmi 54 a 55. Do 300. tahu je
otevřená a na OTEVŘÍT hra odpoví „již to je“, na ZAVŘÍT „Brána zůstala otevřená“.
Potom se zavře natrvalo. Úvodní kontrola počítá s oběma stranami (`IR.EQ.54.OR.
IR.EQ.55`), jenže obě odpovědi testují jen 95 a 55. Strana 54 chybí.

**Co se dělo:** Hráč stojící u branky na straně 54 propadl do kódu pro **hlavní
bránu** na druhém konci pozemku. Měl-li u sebe rukavice nebo vedle sebe kohokoli
z postav, hlavní brána se na dálku otevřela nebo zavřela. Bez pomocníka hra
odpověděla „Brána je pro Vás samotného příliš těžká.“, i když branka byla dokořán.
Stejné řádky má i anglický originál z roku 1982 (`MMSF.F`, ř. 866–871), takže jde
o přehlédnutí původních autorů, ne o chybu převodu.

**Oprava:** Strana 54 dostává tytéž odpovědi jako strana 55 (`patch_garden_gate`
v `native/prep.py`). Hlavní brána se odtud už ovládat nedá. Totéž je opravené
i v anglické verzi webu (`wasm/en/prep_en.py`).

**Pro zajímavost, dveře:** komentář v `MMM.FOR` u tabulky východů říká „Z = 1/2 –
dveře zamčené/odemčené“. Program to ale všude používá obráceně: 2 = zamčeno,
1 = odemčeno (`MMSJ.FOR`, `MMSC.FOR`). Chyba je jen v komentáři, hra se chová
správně, takže se nic neopravuje.

---

## Co bylo potřeba kvůli dnešnímu překladači (nejsou to chyby)

Tyto úpravy jen převádějí zvyklosti Microsoft Fortranu 5 na GNU Fortran:
`$STORAGE:2` (16bitová celá čísla), `$INCLUDE`, jednotka 0 jako konzole, text
uložený v proměnných `INTEGER*2`, převod znaku na číslo (MS Fortran dopisoval
mezeru, odtud záhadné `-8192` v `SGET`), formátový znak `\`, `form='binary'` a
zarovnání COMMON bloků. Pole `REAL*8` sloužící jen jako schránky na 8 znaků jsou
teď `CHARACTER*8`, protože jinak by dnešní překladač posunul rozložení paměti.
Podrobnosti jsou v `native/README.md`.

Pro verzi do prohlížeče (WebAssembly přes převaděč f2c) přibylo pár dalších věcí.
WebAssembly je ještě přísnější než dnešní procesory, například hlídá, že volaný
podprogram dostane přesně ty parametry, které čeká:
- `CALL REIO(2,LU,' Vidíte:',4)` v `MMRI` předává textovou konstantu tam, kde REIO
  čeká pole `INTEGER*2`. Na x86 to náhodou vyjde nastejno, ve WebAssembly ne.
  Tato volání teď jdou přes malý podprogram `REIOS`.
- Řádky delší než 72 znaků se zalamují, `REAL*4 RND` má vlastní COMMON blok (f2c
  nesnese reálné číslo na lichém zarovnání) a desetinná čísla se tisknou s nulou
  před tečkou („0.7“) jako v MS Fortranu.

Výsledek se ověřuje automaticky: stovky náhodných her musí mít v prohlížečové verzi
výstup do posledního bajtu shodný s nativní verzí.

---

## Vědomé změny ve webovém vydání

- **Hlavička úvodní obrazovky.** Řádek pod titulkem má za sebou celou historii:
  v kamenické kopii zdrojáků stojí `J Z D   A K   S l u š o v i c e`, v Latin-2 kopii
  `F U T U R E   E n g i n e e r i n g ,   s. r. o.` Webové vydání z roku 2026 tam má
  `A r c h e t y p a l . c z`. Původní znění zůstávají ve zdrojácích i v `CREDITS.md`.

## Odchylky od anglického originálu

Porovnání s Wolpertovým anglickým zdrojákem pro HP 1000 (`original_en/hp1000-fortran`)
zatím ukázalo:

- **Svět je stejný.** Data místností (`IROM`), postav (`IRES`), předmětů (`IVEN`) i všech
  360 východů (`IXT`) jsou číslo od čísla totožná. Česká verze nepřidala místnosti.
- **Jedno nové jméno místnosti.** Pole jmen má v české verzi 66 míst místo 56, ale využité
  je jediné nové: č. 57 „kouzelné studni“ pro tři studny (místnosti 78–80). V angličtině
  sdílejí jméno č. 37 „WITCHING WELL“. Zbylých devět míst je prázdných, nejspíš rezerva.
- **Revize 16 → 17** (`ITST(15)`) a příznak `'MM'` v `ITST(38)`.
- **Skloňování.** Česká verze přidala funkci `IWORD` (B. Michal, 1982), která pozná slovo
  i ve skloněném tvaru. Angličtina nic takového nepotřebovala.
- **Úvodní text.** Anglicky „A COMPUTERIZED FANTASY, COMPLIMENTS OF BILL WOLPERT“, česky
  „Vše je fantazie počítače…“. Z „počítačové fantazie, kterou vám věnuje Bill Wolpert“ se
  stala „fantazie počítače“. Anglické „power, lust and greed“ (moc, chtíč a chamtivost)
  se v češtině zcudnělo na „sílu i slabost, lakotu i chamtivost“. A originál má
  v uvítání překlep: „WELCOME TO MYSTERY MANSON“.

*Připravuje se:* podrobné srovnání kódu, tedy co přinesly české revize z let 1983–1985
(„revize všech konstrukcí, přidání nových reakcí“), a přehrání anglického průchodu na
999 bodů (záhada č. 46) v obou verzích.
