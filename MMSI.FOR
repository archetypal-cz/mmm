$storage:2
$large
$nodebug
      subroutine mmsi(p1,p2,p3,p4,p5)
c      ,10-02-1983 MMM    8536E D45 PL&BM <850906.1801>
C     04-02-1988 MS-DOS S.M.
      integer*2 p1,p2,p3,p4,p5
      character*127 f
$INCLUDE:'MMCO.for'
C
C ******* SEGMENT I *******
C
C  Tento segment vypisuje pokecy pro segment MMSH a vraci se
C  zpet do MMSB
C
C *** FUNCTIONS ***
C
      IFSD(I)=I-(I/100)*100
      ITFD(I)=I/100-(I/10000)*100
      ITD(I)=I/100-(I/1000)*10
      IFD(I)=I/1000-(I/10000)*10
C     CALL RMPAR(IPR)
      ipr(1)=p1
      ipr(2)=p2
      ipr(3)=p3
      ipr(4)=p4
      ipr(5)=p5
      LU=IPR(1)
      IRIT=IPR(2)
      IW63=IWRD(6,3)
      p1=lu
      p2=0
      p3=0
      p4=0
      p5=0
C
C * nastaveni smeru *
C
      IFC=ITST(4)/1000
      IDN=1
      IDE=2
      IDS=3
      IDW=4
      IF (IVEN(6).LT.20000)IDN=10-IFC
      IF (IDN.EQ.6)IDN=10
      IF (IVEN(6).LT.20000)IDE=11-IFC
      IF (IVEN(6).LT.20000)IDS=12-IFC
      IF (IDS.EQ.11)IDS=7
      IF (IVEN(6).LT.20000)IDW=13-IFC
      IF (IDW.GT.10)IDW=IDW-4
C
C   *** ukoncujici kecy ***
C
  999 continue
  320 F='(/'' Nen° mi jasnÇ, co se mi pokou®°te ©°ci'')'
      IF (IRIT .EQ. 320) WRITE(LU,f)
C
  520 F='(/'' Nic se nestalo'')'
      IF (IRIT .EQ. 520) WRITE(LU,f)
C
 1000 F='(/'' Zkuste to znovu. Nev°m, na koho m†te spadeno.'')'
      IF (IRIT.EQ.1000) WRITE(LU,f)
C
 1001 F='(/'' Zkuste to znovu. Nev°m, koho se chcete ''4A2)'
      IF (IRIT.EQ.1001) WRITE(LU,f)(IWRD(L,1),L=1,4)
C
 1010 F='(/'' Dàkuji. Chutnalo mi.'')'
      IF (IRIT.EQ.1010) WRITE(LU,f)
C
 1020 F='(/'' Fuj! Co to bylo?!'')'
      IF (IRIT.EQ.1020) WRITE(LU,f)
C
 1040 F='(/'' V po©†dku'')'
      IF (IRIT .EQ. 1040) WRITE(LU,f)
C                            MM-I-1  (LINE 7840)
C 2-I
C
 1065 F='(/'' PROGRAM ERROR AT LINE''I6)'
      IF (IRIT .EQ. 1065) WRITE(LU,f) IPR(3)
C
      IF (IPR(3).EQ.7.OR.IPR(3).EQ.16.OR.IPR(3).EQ.25)IC=IDS
      IF (IPR(3).EQ.5.OR.IPR(3).EQ.14.OR.IPR(3).EQ.23)IC=IDW
      IF (IPR(3).EQ.1.OR.IPR(3).EQ.10.OR.IPR(3).EQ.19)IC=IDN
 1095 F='(/'' Poda©ilo se V†m otev©°t prñchod do tajnÇ chodby, do kterÇ
     1mñëete''/'' vstoupit ''4A2)'
      IF (IRIT .EQ. 1095) WRITE(LU,f)(IDTN(I,IC),I=1,4)
C
 1150 F='(/'' Je zde tma...'')'
      IF (IRIT .EQ. 1150) WRITE(LU,f)
C
 1151 F='(/'' Je po z†padu slunce.'')'
      IF ((IRIT .EQ. 1150) .AND. (IPR(3) .EQ. 1)) WRITE(LU,f)
C
 1152 F='(/'' Zavàsy zatem§uj° m°stnost.'')'
      IF ((IRIT .EQ. 1150) .AND. (IPR(3) .EQ. 2)) WRITE(LU,f)
C
 2010 F='(/'' Nev°m, jak chcete j°st ''4A2)'
      IF (IRIT .EQ. 2010) WRITE (LU,f)(IWRD(L,2),L=1,4)
C
 2020 F='(/'' Nev°m, jak chcete p°t ''4A2)'
      IF (IRIT .EQ. 2020) WRITE(LU,f) (IWRD(L,2),L=1,4)
C
 2030 F='(/1X4A2'' zde nen°.'')'
      IF (IRIT.EQ.2030) WRITE(LU,f)(IRSN(L,IWRD(6,2)),L=1,4)
C
 2035 F='(/'' V°no zde bohuëel nen°.'')'
      IF (IRIT.EQ.2035) WRITE(LU,f)
C
 2036 F='(/'' Jed zde nen°.'')'
      IF (IRIT.EQ.2036) WRITE(LU,f)
C
 2038 F='(/'' Voda zde nen°.'')'
      IF (IRIT.EQ.2038) WRITE(LU,f)
C
 2037 F='(/'' Mus°te si uvàdomit, ëe papyrusovò svitek je tak jemnò, ëe
     1i pouhÇ''/'' fouknut° jej zcela zniá°.'')'
      IF (IRIT.EQ.2037) WRITE(LU,f)
C
 2080 F='(/'' Sluëka V†m dala facku.'')'
      IF (IRIT.EQ.2080) WRITE(LU,f)
C
 2090 F='(/'' Nejste Aladin.'')'
      IF (IRIT.EQ.2090) WRITE(LU,f)
C
 3010 F='(/1X4A2'' nem†te.'')'
      IF (IRIT .EQ. 3010) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
      IF (IRIT.EQ.9040) ITST(34)=NORM(500)-1
C
 3050 F='(/'' Pro p©enos do laborato©e pot©ebujete vys°laá hmoty.'')'
      IF (IRIT.EQ.3050) WRITE(LU,f)
C
C Cu
C
 3060 F='(/'' V nàkolika sekund†ch jste se rozplynul a opàt zmaterializo
     2val''/'' v laborato©i.'')'
      IF (IRIT.EQ.3060) WRITE(LU,f)
C
 3070 F='(/'' Mus°te bòt v laborato©i, abyste se mohl p©enÇst'')'
      IF (IRIT.EQ.3070) WRITE(LU,f)
C
 3080 F='(/'' P©ij°maá hmoty by màl bòt nàkde jinde, abyste se mohl p©en
     1Çst.'')'
      IF (IRIT.EQ.3080) WRITE(LU,f)
C
 3090 F='(/'' V nàkolika sekund†ch jste se rozplynul a opàt zhmotnil''/1
     1XA2,1X,8A2)'
      IF (IRIT.EQ.3090) WRITE(LU,f)IPRP(1,MMPRP(IPR(3))),
     1 (IRNM(L,IFSD(IROM(IPR(3)))),L=1,8)
C
C ???
 3110 F='(/1X4A2'' jië nad†le nen° ve Va®° vòbavà'')'
      IF (IRIT .EQ. 3110) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
C=====C                      MM-I-2  (LINE 7920)
C 3-I C
C=====C
C
C  ???
 4010 F='(/'' Nejprve mus°te vàdàt, kde ''4A2'' je.'')'
      IF (IRIT.EQ.4010) WRITE(LU,f)(IWRD(L,4),L=1,4)
C
 4050 F='(/'' Dob©e. Bylo to tady t©eba.'')'
      IF (IRIT.EQ.4050) WRITE(LU,f)
C
 4055 F='(/'' Nem†te ko®tà ani nàco jinÇho pro zamet†n°.'')'
      IF (IRIT.EQ.4055) WRITE(LU,f)
C
C ???
 5020 F='(/1X4A2'' zde nen°'')'
      IF (IRIT .EQ. 5020) WRITE(LU,f)(IWRD(L,3),L=1,4)
C
 5001 F='(/'' Policie p©ijela a zatkla vraha.'')'
      IF (IRIT.EQ.5001) WRITE(LU,f)
      IF (ITST(24).NE.0.AND.IRIT.EQ.5001)ITST(1)=8
      IF (ITST(24).NE.0.AND.IRIT.EQ.5001.AND.IVEN(2).GT.20000)IVEN(2)=0
      IF (ITST(24).NE.0.AND.IRIT.EQ.5001.AND.IVEN(9).GT.20000)IVEN(9)=0
 5003 F='(/'' Protoëe jste zabil nevinnÇho, policie V†s zav©ela do hlado
     1morny.'')'
      IF (ITST(24).NE.0.AND.IRIT.EQ.5001) WRITE(LU,f)
C
 5002 F='(/'' Dispeáer V†m ©ekl, ëe droëka p©ijede asi za 2 hodiny.'')'
      IF (IRIT.EQ.5002) WRITE(LU,f)
C
 5007 f='(/'' Mus°te mi ©°ct, s kòm chcete mluvit.'')'
      if (irit .eq. 5007) write(lu,f)
C
 5009 F='(/'' Telefon je st†le obsazen.'')'
      IF (IRIT.EQ.5009) WRITE(LU,f)
C
 6005 F='(/'' Asi tucet indi†nñ V†s zas†hlo nàkolika jedovatòmi ®°py a z
     1em©el jste''/'' bolestivou smrt°.'')'
      IF (IRIT.EQ.6005) WRITE(LU,f)
C
      IF (IRIT.EQ.6005)NAME(2)='SD'
      IF (IRIT.EQ.6005) then
c      CALL SGLOD(NAME,LU,6005)
          p2=6005
          return
      endif
 6010 F='(/'' Odkryl jste nàkolik zlatòch minc°.'')'
      IF (IRIT.EQ.6010) WRITE(LU,f)
C
 6015 F='(/'' Po pohybu stolem se otev©ely tajnÇ dve©e a odkryly dlouhÇ
     1rukavice.'')'
      IF (IRIT.EQ.6015) WRITE(LU,f)
C
      IF (IRIT.EQ.6015)IVEN(48)=9115
      IF (IRIT.EQ.6030) WRITE(LU,6030)
 6030 FORMAT(/
     1' Pod hadrami je podivn† kresba kompasu se st©elkou ohnutou do'/
     2' U a hodin s dvàma minutovòmi ruáiákami; z nichë jedna stoj°'/
     3' na 10 a druh† na 12.')
C
      IF (IRIT.EQ.6040) WRITE(LU,6040)
 6040 FORMAT(/' Pod hadrami je podivn† kresba kompasu se st©elkou',
     1' ukazuj°c°'/' na sever a hodin, kterÇ maji pouze minutovou',
     2' ruáiáku na 12.')
C
 6041 F='(/'' Nemñëete pohnout ani jedn°m kamenem.'')'
      IF (IRIT.EQ.6041) WRITE(LU,f)
C
      IF (IRIT.EQ.6042) WRITE(LU,6042)
 6042 FORMAT(/
     1' V touze po z°sk†n° vàt®° ko©isti se V†m poda©ilo pohnout'/
     2' jedn°m z balvanñ nespr†vnym smàrem a ten se odkut†lel a'/
     3' zavalil jedinò vòchod.')
      IF (IRIT.EQ.6042)IXT(319)=0
C
      IF (IRIT.EQ.6043) WRITE(LU,6043)
 6043 FORMAT(/' S velkou n†mahou se V†m poda©ilo o kousek pohnout',
     1'jedn°m balvanem a'/' v jednom z kr†terñ pod n°m jste na®el'
     2' s†áek perel.')
C
 6044 F='(/'' D°ky rukavici se V†m poda©ilo o kousek pohnout balvanem, a
     1le nic''/'' pod n°m nen°.'')'
      IF (IRIT.EQ.6044) WRITE(LU,f)
C
C                            MM-I-3  (LINE 8000)
C 4-I
C
 7010 F='(/'' Hlas v telefonu ©ekl:'')'
      IF (IRIT .EQ. 7010) WRITE(LU,f)
C
      IF (IRIT .EQ. 7010) IVEN(42)=1000
 7015 F='('' Proá neodloë°te l°stek? Uë ho nebudete pot©ebovat.'')'
      IF (IRIT .EQ. 7010.AND.IPR(3).EQ.1) WRITE(LU,f)
C
 7020 F='('' Proá se nepod°v†te na nàkterÇ vàci, kterÇ m†te u sebe?'')'
      IF (IRIT.EQ.7010.AND.IPR(3).EQ.2) WRITE(LU,f)
C
 7025 F='('' Kdybych byl V†mi, hl°dal bych vraha a zavolal policii!'')'
      IF (IRIT.EQ.7010.AND.IPR(3).EQ.3) WRITE(LU,f)
C
 7030 F='('' Neë se dostanete d†le, bylo by asi rozumnÇ vr†tit se k hlav
     1n° br†nà a''/'' vz°t si lucernu.'')'
      IF (IRIT.EQ.7010.AND.IPR(3).EQ.4) WRITE(LU,f)
C
 7080 F='('' a zavàsil.'')'
      IF (IRIT .EQ. 7010) WRITE(LU,f)
C
 7090 F='(/'' Nikdo neodpov°d†.'')'
      IF (IRIT .EQ. 7090) WRITE(LU,f)
C
 7050 F='('' nic'')'
      IF (IRIT .EQ. 7050) WRITE(LU,f)
C
 8005 F='(/1X4A2'' pohnout nemñëete, nikde zde nen°.'')'
      IF (IRIT .EQ. 8005) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 8006 F='(/'' Nem†te ë†dnÇ j°dlo.'')'
      IF (IRIT.EQ.8006) WRITE(LU,f)
C
 8010 F='(/'' Nev°m, co oáek†v†te, ëe se stane'')'
      IF (IRIT .EQ. 8010 ) WRITE(LU,f)
C
 8015 F='(/1X4A2'' V†m snàdl v®echno j°dlo'')'
      IF (IRIT .EQ. 8015) WRITE(LU,f)(IRSN(L,IWRD(6,2)),L=1,4)
C
 8020 F='(/'' uë je'')'
      IF (IRIT .EQ. 8020) WRITE(LU,f)
C
 8030 F='(/1X4A2'' posunout nemñëete, nen° zde'')'
      IF (IRIT .EQ. 8030) WRITE(LU,f)(IWRD(L,3),L=1,4)
C
 8031 F='(/1X4A2'' zde nen°'')'
      IF (IRIT .EQ. 8031) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 8032 F='(/'' Bez telefonu se nikam nedovol†te.'')'
      IF (IRIT.EQ.8032) WRITE(LU,f)
C
 8035 F='(/'' Baterie v lucerne je vybit†'')'
      IF (IRIT .EQ. 8035) WRITE(LU,f)
C
 9027 F='(/'' Nem†te dostateánò poáet bodñ'')'
      IF (IRIT.EQ.9027) WRITE(LU,f)
C
 9030 F='(/'' Podvodn°ku!! Nejste povolan† osoba.'')'
      IF (IRIT.EQ.9030) WRITE(LU,f)
C
 9040 F='(/'' Neotravujte mne, kdyë nejste autor.'')'
      IF (IRIT.EQ.9040) WRITE(LU,f)
C
10020 F='(/'' Je®tà jste nep©i®el na to, ëe to takto nejde?'')'
      IF (IRIT.EQ.10020) WRITE(LU,f)
C
10030 F='(/'' Byl jste zabit v boji'')'
      IF (IRIT.EQ.10030) WRITE(LU,f)
C
      IF (IRIT.EQ.10030)NAME(2)='SD'
      IF (IRIT.EQ.10030) then
c      CALL SGLOD(NAME,LU,10030)
          p2=10030
          return
      endif
10031 F='(/'' Byl jste zranàn v boji.'')'
      IF (IRIT.EQ.10031) WRITE(LU,f)
C                            MM-I-4  (LINE 8080)
C 5-I
C
10035 F='(/'' Dve©e jsou zav©enÇ'')'
      IF (IRIT.EQ.10035) WRITE(LU,f)
C
10040 F='(/'' Zabil jste ''4A2)'
      IF (IRIT.EQ.10040) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
10041 F='(/'' Zranil jste ''4A2)'
      IF (IRIT.EQ.10041) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
10045 F='(/'' Dve©e jsou otev©enÇ'')'
      IF (IRIT.EQ.10045) WRITE(LU,f)
C
10046 F='(/'' Dve©e jsou zav©enÇ'')'
      IF (IRIT.EQ.10046) WRITE(LU,f)
C
10060 F='(/'' V†® protivn°k uprchl bàhem ruán° vòmàny n†zorñ'')'
      IF (IRIT.EQ.10060) WRITE(LU,f)
C
      IF (IRIT.EQ.10011)  THEN
10011 F='(/'' Pr†và jste vamp°rovi zarazil kñl do srdce. Vamp°r v mëiku'
     1'/'' zest†rl o tis°c let a promànil se v prach.'')'
         WRITE(LU,f)
         IRES(5)=0
         IVEN(1)=105
         IVEN(29)=0
c        IF (LU.NE.ITST(21))ITST(2)=ITST(2)+75
         ITST(2)=ITST(2)+75
      ENDIF
C
      IF (IRIT .EQ. 11040) WRITE(LU,11040) ITST(15)
11040 FORMAT(
     1/' Souáasn† situace hry je uloëena a bude vyhovovat pro'
     1/' revizi 'I3'. Nyni mñëete ukonáit hru nebo d†le pokra-'
     2/' áovat. Budete-li cht°t pokraáovat v uloëenÇ h©e, za-'
     3/' dejte OBNOVIT')
C
      IF (IRIT.EQ.11075) WRITE(LU,11075)
11075 FORMAT(/' Data nejsou uloëena.'/
     +' ChybnÇ jmÇno souboru, nedostatek prostoru nebo jin† chyba.')
C
12040 F='(/'' Nyn° jste ve stejnÇ situaci jako p©i uloëen° hry.'')'
      IF ((IRIT .EQ. 12040) .AND. (ITST(15) .EQ. 17)) WRITE(LU,f)
C
12050 F='(/'' Tato data jsou pro jinou revizi'')'
      IF ((ITST(15) .NE. 17) .AND. (IRIT .EQ. 12040)) WRITE(LU,f)
C
12055 F='(/'' Aë budete hotov, zru®te soubor. '')'
      IF (IRIT.EQ.11040.OR.IRIT.EQ.12040) WRITE(LU,f)
C
14010 F='(/'' Mñëete nastavit á°slo z†hady prvn°m p©°kazem po zah†jen° h
     1ry. Nyn° jste v z†hadà á°slo''I4)'
      IF (IRIT.EQ.14010) WRITE(LU,f) ITST(23)
C
14110 F='(/'' To je jmÇno hry.'')'
      IF (IRIT.EQ.14110) WRITE(LU,f)
C
15110 F='(/'' V†® kl°á nepasuje do z†mku'')'
      IF (IRIT.EQ.15110) WRITE(LU,f)
C
15200 F='(/1X4A2'' se nezaj°m† o nic z toho, co m†te'')'
      IF (IRIT.EQ.15200) WRITE(LU,f)(IRSN(L,IWRD(6,2)),L=1,4)
C
15400 F='(/1X4A2'' V†m stejnà nic ne©ekne '')'
      IF (IRIT.EQ.15400) WRITE(LU,f)(IRSN(L,IWRD(6,2)),L=1,4)
C
15500 F='(/1X4A2'' si vzal pokladnu a potÇ V†m ©ekl, ëe vraëdu sp†chal '
     1'4A2/1XA2,1X,8A2'', a ëe vraëednòm n†strojem je''4A2)'
      IF (IRIT.EQ.15500.AND. ITST(8).NE.0) WRITE(LU,f)(IRSN(L,IWRD
     1(6,2)),L=1,4),(IRSN(L,ITST(9)),L=1,4),
     2 IPRP(1,MMPRP(ITST(8))),(IRNM(L,IFSD(IROM(ITST(8)))),
     3 L=1,8),(IWP(L),L=1,4)
C
15510 F='(/1X4A2'' si vzal od V†s pokladnici a po malÇm zav†h†n° odpovàd
     1àl,''/'' ëe mñëete odej°t podkrovn° svàtnic°.'')'
      IF (IRIT.EQ.15500.AND.ITST(8).EQ.0) WRITE(LU,f)(IRSN(L,IWRD(6,
     12)),L=1,4)
C                            MM-I-5  (LINE 8160)
C 6-I
16035 F='(/'' Rozbil jste z†mek na kousky'')'
      IF (IRIT.EQ.16035) WRITE(LU,f)
C
16045 F='(/'' Zabil jste sk©eta a ten zmizel v oblaku dusivÇho áernÇho d
     1òmu'')'
      IF (IRIT.EQ.16035)IVEN(35)=IVEN(35)+1000
      IF (IRIT.EQ.16045) WRITE(LU,f)
      IF (IRIT.EQ.16045)IRES(13)=IRES(13)+100
C
20010 F='(/'' Pot©ebujete olejniáku nebo nàco podobnÇho'')'
      IF (IRIT .EQ. 20010) WRITE(LU,f)
C
20020 F='(/'' Olejniáka je pr†zdn†'')'
      IF (IRIT .EQ. 20020) WRITE(LU,f)
C
20030 F='(/'' NebuÉte hloupò'')'
      IF (IRIT .EQ. 20030) WRITE(LU,f)
C
*      IF (IRIT.EQ.21020) WRITE(LU,21020)ITST(21)
*21020 FORMAT(/' Tento a vsechny nasledujici radky budou zaznamenavany',
*     1' na lu 'I3)
C
22010 F='(/'' Zde nemñëete rozb°t z†mek truhly'')'
      IF (IRIT.EQ.22010) WRITE(LU,f)
C
22020 F='(/'' Nem†te nic, á°m by ®el rozb°t z†mek truhly'')'
      IF (IRIT.EQ.22020) WRITE(LU,f)
C
23010 F='(/'' Vamp°r V†s ochromil uhranáivòm pohledem, zvolna k V†m kr†á
     1° a ©°k†,''/'' ëe se jmenuje hrabà DRACULA'')'
      IF (IRIT.EQ.23010) WRITE(LU,f)
C
23015 F='(/'' Odpornò slizkò tvor k V†m pr†và kr†á° a chrocht†'')'
      IF (IRIT.EQ.23015) WRITE(LU,f)
C
23020 F='(/'' Ö†belskò malò tvor si V†s prohl°ë° a checht† se.'')'
      IF (IRIT.EQ.23020) WRITE(LU,f)
C
23030 F='(/'' ûik†, ëe s V†mi pñjde k hlavn° br†nà a pomñëe V†m ''/'' ji
     1 otev©°t, jestliëe o to stoj°te.'')'
      IF (IRIT.EQ.23030) WRITE(LU,f)
C
23040 F='(/'' û°k†, ëe br†na je tàëk† a pomñëe V†m ji otev©°t, jestliëe'
     1'/'' ©eknete: OTEVûãT BRèNU'')'
      IF (IRIT.EQ.23040) WRITE(LU,f)
C
23050 F='(/'' û°k†, ëe vëdy, kdyë je zde s p©°telem, zav©e br†nu'')'
      IF (IRIT.EQ.23050) WRITE(LU,f)
C
23060 F='(/'' û°k†, ëe mñëete pouë°t mapy, kter† je v chaloupce.'')'
      IF (IRIT.EQ.23060) WRITE(LU,f)
C
23070 F='(/'' û°k†, ëe m†te d†vat pozor na 500 stop vysokò skaln° £tes n
     1a''/'' vòchodn° stranà s°dla.'')'
      IF (IRIT.EQ.23070) WRITE(LU,f)
C
23080 F='(/'' û°k†, ëe se boj° j°t do chaloupky.'')'
      IF (IRIT.EQ.23080) WRITE(LU,f)
C
23090 F='(/'' û°k†, ëe v° o nàáem lep®°m neë bloudit v lese'')'
      IF (IRIT.EQ.23090) WRITE(LU,f)
C
C
C                            MM-I-6  (LINE 8240)
C 7-I
C
23100 F='(/'' û°k†, ëe nesm° j°t do zahrady.'')'
      IF (IRIT.EQ.23100) WRITE(LU,f)
C
23110 F='(/'' û°k†, ëe hlavn° vchod je nebezpeánò.'')'
      IF (IRIT.EQ.23110) WRITE(LU,f)
C
      IC1=' '
      IF (IWRD(6,2).EQ.2.OR.IWRD(6,2).EQ.8) IC1='a'
23120 F='(/1X4A2'' zaá°n† bòt otr†ven''A1'' Va®imi dotazy'')'
      IF (IRIT.EQ.23120) WRITE(LU,f)(IRSN(L,IWRD(6,2)),L=1,4),IC1
C
23140 F='(/'' Va®e posledn° ot†zka narazila na hluchÇ u®i.'')'
      IF (IRIT.EQ.23140) WRITE(LU,f)
C
23145 F='(/1X4A2'' zaá°n† bòt opravdu znechucen''A1'' Va®imi dotazy.'')'
      IF (IRIT.EQ.23145) WRITE(LU,f)(IRSN(L,IWRD(6,2)),L=1,4),IC1
C
23150 F='(/1X4A2'' odpovàdàl''A1'': o vraëdà nev°m nic.'')'
      IF (IRIT.EQ.23150) WRITE(LU,f)(IRSN(L,IWRD(6,2)),L=1,4),IC1
C
23170 F='(/'' Na Va®i ot†zku odpovàdàl''A1'' se zav†h†n°m:'')'
      IF (IRIT.GE.23170.AND.IRIT.LE.23190) WRITE(LU,f) IC1
C
23175 F='('' Mysl°m, ëe vrah je ''4A2)'
      IF (IRIT.EQ.23170) WRITE(LU,f)(IRSN(L,ITST(9)),L=1,4)
C
23180 F='('' Mysl°m, ëe vraëda byla sp†ch†na ''A2,1X,8A2)'
      IF (IRIT.EQ.23180) WRITE(LU,f)IPRP(1,MMPRP(ITST(8))),
     1 (IRNM(L,IFSD(IROM(ITST(8)))),L=1,8)
C
23190 F='('' Mysl°m, ëe vraëednou zbran° je ''4A2)'
      IF (IRIT.EQ.23190) WRITE(LU,f)(IWP(L),L=1,4)
C
23210 F='(/'' û°k†, ëe se mu nechce odej°t ze s°dla.'')'
      IF (IRIT.EQ.23210) WRITE(LU,f)
C
23220 F='(/'' û°k†, ëe existuje vstup do tajnÇ laborato©e z velkÇ loënic
     1e.'')'
      IF (IRIT.EQ.23220) WRITE(LU,f)
C
23230 F='(/'' û°k†, ëe tajn† chodba spojuje v®echna t©i podlaë° s°dla.''
     1)'
      IF (IRIT.EQ.23230) WRITE(LU,f)
C
23240 F='(/'' û°k†, ëe sluëka um° bòt velmi p©†telsk†.'')'
      IF (IRIT.EQ.23240) WRITE(LU,f)
C
23270 F='(/'' û°k†, ëe se boj° j°t pod hlavn° podlaë° s°dla'')'
      IF (IRIT.EQ.23270) WRITE(LU,f)
C
23280 F='(/'' û°k†, ëe neopust° s°dlo ani zahradu'')'
      IF (IRIT.EQ.23280) WRITE(LU,f)
C
23290 F='(/'' û°k†, ëe um° p©eá°st n†pis na truhle.'')'
      IF (IRIT.EQ. 23290) WRITE(LU,f)
C
23300 F='(/'' û°k†, ëe sk©°tek schov†v† vàci ve studni.'')'
      IF (IRIT.EQ.23300) WRITE(LU,f)
C
23330 F='(/'' û°k†, ëe na konci chladnÇ chodby je pokladna'')'
      IF (IRIT.EQ.23330) WRITE(LU,f)
C
23340 F='(/'' û°k†, ëe mñëete odl†kat vlka potravou.'')'
      IF (IRIT.EQ.23340) WRITE(LU,f)
C                            MM-I-7  (LINE 8320)
C 8-I
C
23350 F='(/'' û°k†, ëe mñëete stàny zastavit, kdyë u nich nech†te nàjakò
     1 p©edmàt.'')'
      IF (IRIT.EQ.23350) WRITE(LU,f)
C
23360 F='(/'' û°k†, ëe mñëete p©eá°st svitek pomoc° magickòch slov'')'
      IF (IRIT.EQ.23360) WRITE(LU,f)
C
24001 F='(/'' Sly®°te tlachy opice.'')'
      IF (IRIT.EQ.24001) WRITE(LU,f)
C
24002 F='(/'' Sly®°te hukot ohnà v krbu.'')'
      IF (IRIT.EQ.24002) WRITE(LU,f)
C
24003 F='(/'' Zespodu k V†m dolÇhaj° podivnÇ zvuky.'')'
      IF (IRIT.EQ.24003) WRITE(LU,f)
C
24004 F='(/'' Sly®°te sk©°pot podlahy p©i kaëdÇm kroku.'')'
      IF (IRIT.EQ.24004.OR.IRIT.EQ.24013.OR.IRIT.EQ.24022)
     1 WRITE(LU,f)
C
24005 F='(/'' Je zde mrtvÇ ticho.'')'
      IF (IRIT.EQ.24005) WRITE(LU,f)
C
24006 F='(/'' Sly®°te, jak nàkdo chod° v pokoji nad V†mi.'')'
      IF (IRIT.EQ.24006) WRITE(LU,f)
C
24007 F='(/'' Sly®°te jen hluk, kterò zpñsobujete n†razy na vàci.'')'
      IF (IRIT.EQ.24007) WRITE(LU,f)
C
24008 F='(/'' Sly®°te pi®tàn° nàkolika krys pob°haj°c°ch okolo.'')'
      IF (IRIT.EQ.24008) WRITE(LU,f)
C
24009 F='(/'' Sly®°te, jak nàjakÇ zv°©e ®kr†be a chce se dostat dovnit©.
     1'')'
      IF (IRIT.EQ.24009) WRITE(LU,f)
C
24010 F='(/'' Mñëete m°t sebelep®° u®i, ale stejnà nic neusly®°te.'')'
      IF (IRIT.EQ.24010) WRITE(LU,f)
C
24011 F='(/'' Mñëete sly®et slabou ozvànu vysoko nad V†mi.'')'
      IF (IRIT.EQ.24011) WRITE(LU,f)
C
24012 F='(/'' Sly®°te hraj°c° radio.'')'
      IF (IRIT.EQ.24012.AND.IFD(IVEN(75)).EQ.1) WRITE(LU,f)
C
24212 F='(/'' Sly®°te nàco, co zni jako hrom v blizkòch hor†ch'')'
      IF (IRIT.EQ.24012.AND.IFD(IVEN(75)).NE.1) WRITE(LU,f)
C
24014 F='(/'' Sly®°te hukot ohnà v krbu.'')'
      IF (IRIT.EQ.24014.AND.IFD(IVEN(89)).EQ.1) WRITE(LU,f)
C
24214 F='(/'' Sly®°te jen ozvànu svòch krokñ.'')'
      IF (IRIT.EQ.24014.AND.IFD(IVEN(89)).NE.1) WRITE(LU,f)
C
24015 F='(/'' Sly®°te, jak nàkdo pod podlahou dàl† hluk.'')'
      IF (IRIT.EQ.24015) WRITE(LU,f)
C
24016 F='(/'' Sly®°te tichou varhann° hudbu.'')'
      IF (IRIT.EQ.24016) WRITE(LU,f)
C
24017 F='(/'' Sly®°te slabò zvuk varhan.'')'
      IF (IRIT.EQ.24017) WRITE(LU,f)
C
24018 F='(/'' Nesly®°te vñbec nic.'')'
      IF (IRIT.EQ.24018) WRITE(LU,f)
C                            MM-I-8  (LINE 8400)
C 9-I
C
24019 F='(/'' Sly®°te, jak V†m tluáe srdce.'')'
      IF (IRIT.EQ.24019) WRITE(LU,f)
C
24020 F='(/'' Sly®°te, jak venku fouk† v°tr ''4A2)'
      IF (IRIT.EQ.24020) WRITE(LU,f)(IDTN(L,IDS),L=1,4)
      IF (IRIT.EQ.24024) WRITE(LU,f)(IDTN(L,IDE),L=1,4)
      IF (IRIT.EQ.24025) WRITE(LU,f)(IDTN(L,IDN),L=1,4)
C
24021 F='(/'' Sly®°te, jak V†m v°tr hv°zd† kolem u®°.'')'
      IF (IRIT.EQ.24021) WRITE(LU,f)
C
24023 F='(/'' Sly®°te, jak nad V†mi rachot° ©etàzy.'')'
      IF (IRIT.EQ.24023.OR.IRIT.EQ.24026) WRITE(LU,f)
C
24027 F='(/'' Sly®°te vlastn° dech.'')'
      IF (IRIT.EQ.24027) WRITE(LU,f)
C
24028 F='(/'' Sly®°te ®est rñznòch ozvàn kaëdÇho zvuku, kterò vyd†te.'')
     1'
      IF (IRIT.EQ.24028) WRITE(LU,f)
C
24029 F='(/'' Sly®°te pàt rñznòch ozvàn kaëdÇho zvuku, kterò vyd†v†te.''
     1)'
      IF (IRIT.EQ.24029) WRITE(LU,f)
C
24030 F='(/'' Sly®°te áty©i rñznÇ ozvàny kaëdÇho zvuku, ktery vyd†v†te.'
     1')'
      IF (IRIT.EQ.24030) WRITE(LU,f)
C
24031 F='(/'' Sly®°te t©i rñznÇ ozvàny kaëdÇho zvuku, kterò vyd†v†te.'')
     1'
      IF (IRIT.EQ.24031) WRITE(LU,f)
C
24032 F='(/'' Sly®°te dvà rñznÇ ozvàny kaëdÇho zvuku, kterò vyd†v†te.'')
     1)'
      IF (IRIT.EQ.24032) WRITE(LU,f)
C
24033 F='(/'' Sly®°te ozvànu kaëdÇho zvuku, kterò vyd†v†te.'')'
      IF (IRIT.EQ.24033.OR.IRIT.EQ.24034) WRITE(LU,f)
C
24036 F='(/'' Sly®°te, jak se stàny p©ibliëuj°.'')'
      IF (IRIT.GT.24035.AND.IRIT.LT.24041) WRITE(LU,f)
C
24041 F='(/'' Sly®°te vodu, proud°c° pod V†mi.'')'
      IF (IRIT.EQ.24041) WRITE(LU,f)
C
24042 F='(/'' Sly®°te, jak nàkdo poblië kope.'')'
      IF (IRIT.GT.24041.AND.IRIT.LT.24054.AND.IRES(4).NE.0)
     1 WRITE(LU,f)
C
24043 F='(/'' Sly®°te sk©°pàn°, jak se tunel uzav°r†'')'
      IF (IRIT.GT.24041.AND.IRIT.LT.24054.AND.IRES(4).EQ.0)
     1 WRITE(LU,f)
C
24088 F='(/'' Sly®°te slabò zvuk stroje.'')'
      IF (IRIT.EQ.24088) WRITE(LU,f)
C
24100 F='(/'' Nesly®°te nic zaj°mavÇho.'')'
      IF (IRIT.EQ.24100) WRITE(LU,f)
C
24105 F='(''  '')'
      IF (IRIT.EQ.24105) WRITE(LU,f)
C
24108 F='('' Sly®°te tikat hodinky.'')'
      IF (IPR(3).EQ.24108) WRITE(LU,f)
C
24110 F='('' Sly®°te, jak nàco poblië dàla hluk.'')'
      IF (IPR(3).EQ.24110) WRITE(LU,f)
C                            MM-I-9  (LINE 8480)
C 10-I
C
24120 F='('' Sly®°te, jak nàkdo pobl°ë dàl† hluk.'')'
      IF (IPR(3).EQ.24120) WRITE(LU,f)
C
24130 F='('' Sly®°te, jak nàkdo ®ept†.'')'
      IF (IPR(3).EQ.24130) WRITE(LU,f)
C
24140 F='('' Sly®°te, jak nàkdo mluv°.'')'
      IF (IPR(3).EQ.24140) WRITE(LU,f)
C
24150 F='('' Sly®°te, ëe se lesn°k a lovec smàj° a ©°kaj° nàco o zav©en°
     1 br†ny.'')'
      IF (IPR(3).EQ.24150) WRITE(LU,f)
C
24160 F='('' Sly®°te, ëe p†n a sluha ©°kaj° nàco jako vraëda a '',4A2)'
      IF (IPR(3).EQ.24160) WRITE(LU,f)(IRSN(L,ITST(9)),L=1,4)
C
24162 F='('' Sly®°te, jak p†n a sluha si ©°kaj° nàco jako:''/'' "stalo s
     1e to ''A2,1X,8A2''"'')'
      IF (IPR(3).EQ.24162) WRITE(LU,f)IPRP(1,MMPRP(ITST(8))),
     1 (IRNM(L,IFSD(IROM(ITST(8)))),L=1,8)
C
24164 F='('' Sly®°te, jak p†n ©°k† sluhovi nàco jako vraëda a '',4A2)'
      IF (IPR(3).EQ.24164) WRITE(LU,f)(IWP(L),L=1,4)
C
24170 F='('' Z rozhovoru kucha©e a sada©e jste zaslechl mimo jinÇ slova
     1 ''/'' vraëda a ''4A2)'
      IF (IPR(3).EQ.24170) WRITE(LU,f)(IRSN(L,ITST(9)),L=1,4)
C
24172 F='('' Sly®°te, jak si kucha© se sada©em ©°kaj° cosi''/''  o vraëd
     1à ''A2,1X,8A2)'
      IF (IPR(3).EQ.24172) WRITE(LU,f)IPRP(1,MMPRP(ITST(8))),
     1 (IRNM(L,IFSD(IROM(ITST(8)))),L=1,8)
C
24174 F='('' Sly®°te, jak kucha© ©°k† sada©ovi nàco jako vraëda a ''4A2)
     1'
      IF (IPR(3).EQ.24174) WRITE(LU,f)(IWP(L),L=1,4)
C
24180 F='('' Z rozhovoru pan° a sluëky jste zaslechl mimo jinÇ slova''/'
     1' vraëda a ''4A2)'
      IF (IPR(3).EQ.24180) WRITE(LU,f)(IRSN(L,ITST(9)),L=1,4)
C
24182 F='('' Z rozhovoru pan° a sluëky jste zaslechl nàco jako m°sto''/'
     1' vraëdy a ''A2,1X,8A2)'
      IF (IPR(3).EQ.24182) WRITE(LU,f)IPRP(1,MMPRP(ITST(8))),
     1 (IRNM(L,IFSD(IROM(ITST(8)))),L=1,8)
C
24184 F='('' Sly®°te, jak pan° ©°k† sluëce nàco jako vraëda a ''4A2)'
      IF (IPR(3).EQ.24184) WRITE(LU,f)(IWP(L),L=1,4)
C
      IF (LU.NE.IPR(1).OR.ITST(21).EQ.0) GOTO 9999
      LU=ITST(21)
      GOTO 999
 9999 NAME(2)='SB'
      IF (IRIT.EQ.12040)NAME(2)='SC'
      IF (IRIT.EQ.12040)IWRD(6,1)=25
c      CALL SGLOD(NAME,LU)
      return
      END
