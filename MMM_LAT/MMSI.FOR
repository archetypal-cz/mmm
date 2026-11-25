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
  320 F='(/'' Nen° mi jasnÇ, co se mi pokouÁ°te ˝°ci'')'
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
 1010 F='(/'' Dÿkuji. Chutnalo mi.'')'
      IF (IRIT.EQ.1010) WRITE(LU,f)
C
 1020 F='(/'' Fuj! Co to bylo?!'')'
      IF (IRIT.EQ.1020) WRITE(LU,f)
C
 1040 F='(/'' V po˝†dku'')'
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
 1095 F='(/'' Poda˝ilo se V†m otev˝°t prÖchod do tajnÇ chodby, do kterÇ
     1mÖßete''/'' vstoupit ''4A2)'
      IF (IRIT .EQ. 1095) WRITE(LU,f)(IDTN(I,IC),I=1,4)
C
 1150 F='(/'' Je zde tma...'')'
      IF (IRIT .EQ. 1150) WRITE(LU,f)
C
 1151 F='(/'' Je po z†padu slunce.'')'
      IF ((IRIT .EQ. 1150) .AND. (IPR(3) .EQ. 1)) WRITE(LU,f)
C
 1152 F='(/'' Zavÿsy zatemÂuj° m°stnost.'')'
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
 2035 F='(/'' V°no zde bohußel nen°.'')'
      IF (IRIT.EQ.2035) WRITE(LU,f)
C
 2036 F='(/'' Jed zde nen°.'')'
      IF (IRIT.EQ.2036) WRITE(LU,f)
C
 2038 F='(/'' Voda zde nen°.'')'
      IF (IRIT.EQ.2038) WRITE(LU,f)
C
 2037 F='(/'' Mus°te si uvÿdomit, ße papyrusovÏ svitek je tak jemnÏ, ße
     1i pouhÇ''/'' fouknut° jej zcela zniü°.'')'
      IF (IRIT.EQ.2037) WRITE(LU,f)
C
 2080 F='(/'' Slußka V†m dala facku.'')'
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
 3050 F='(/'' Pro p˝enos do laborato˝e pot˝ebujete vys°laü hmoty.'')'
      IF (IRIT.EQ.3050) WRITE(LU,f)
C
C Cu
C
 3060 F='(/'' V nÿkolika sekund†ch jste se rozplynul a opÿt zmaterializo
     2val''/'' v laborato˝i.'')'
      IF (IRIT.EQ.3060) WRITE(LU,f)
C
 3070 F='(/'' Mus°te bÏt v laborato˝i, abyste se mohl p˝enÇst'')'
      IF (IRIT.EQ.3070) WRITE(LU,f)
C
 3080 F='(/'' P˝ij°maü hmoty by mÿl bÏt nÿkde jinde, abyste se mohl p˝en
     1Çst.'')'
      IF (IRIT.EQ.3080) WRITE(LU,f)
C
 3090 F='(/'' V nÿkolika sekund†ch jste se rozplynul a opÿt zhmotnil''/1
     1XA2,1X,8A2)'
      IF (IRIT.EQ.3090) WRITE(LU,f)IPRP(1,MMPRP(IPR(3))),
     1 (IRNM(L,IFSD(IROM(IPR(3)))),L=1,8)
C
C ???
 3110 F='(/1X4A2'' jiß nad†le nen° ve VaÁ° vÏbavÿ'')'
      IF (IRIT .EQ. 3110) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
C=====C                      MM-I-2  (LINE 7920)
C 3-I C
C=====C
C
C  ???
 4010 F='(/'' Nejprve mus°te vÿdÿt, kde ''4A2'' je.'')'
      IF (IRIT.EQ.4010) WRITE(LU,f)(IWRD(L,4),L=1,4)
C
 4050 F='(/'' Dob˝e. Bylo to tady t˝eba.'')'
      IF (IRIT.EQ.4050) WRITE(LU,f)
C
 4055 F='(/'' Nem†te koÁtÿ ani nÿco jinÇho pro zamet†n°.'')'
      IF (IRIT.EQ.4055) WRITE(LU,f)
C
C ???
 5020 F='(/1X4A2'' zde nen°'')'
      IF (IRIT .EQ. 5020) WRITE(LU,f)(IWRD(L,3),L=1,4)
C
 5001 F='(/'' Policie p˝ijela a zatkla vraha.'')'
      IF (IRIT.EQ.5001) WRITE(LU,f)
      IF (ITST(24).NE.0.AND.IRIT.EQ.5001)ITST(1)=8
      IF (ITST(24).NE.0.AND.IRIT.EQ.5001.AND.IVEN(2).GT.20000)IVEN(2)=0
      IF (ITST(24).NE.0.AND.IRIT.EQ.5001.AND.IVEN(9).GT.20000)IVEN(9)=0
 5003 F='(/'' Protoße jste zabil nevinnÇho, policie V†s zav˝ela do hlado
     1morny.'')'
      IF (ITST(24).NE.0.AND.IRIT.EQ.5001) WRITE(LU,f)
C
 5002 F='(/'' Dispeüer V†m ˝ekl, ße droßka p˝ijede asi za 2 hodiny.'')'
      IF (IRIT.EQ.5002) WRITE(LU,f)
C
 5007 f='(/'' Mus°te mi ˝°ct, s kÏm chcete mluvit.'')'
      if (irit .eq. 5007) write(lu,f)
C
 5009 F='(/'' Telefon je st†le obsazen.'')'
      IF (IRIT.EQ.5009) WRITE(LU,f)
C
 6005 F='(/'' Asi tucet indi†nÖ V†s zas†hlo nÿkolika jedovatÏmi Á°py a z
     1em˝el jste''/'' bolestivou smrt°.'')'
      IF (IRIT.EQ.6005) WRITE(LU,f)
C
      IF (IRIT.EQ.6005)NAME(2)='SD'
      IF (IRIT.EQ.6005) then
c      CALL SGLOD(NAME,LU,6005)
          p2=6005
          return
      endif
 6010 F='(/'' Odkryl jste nÿkolik zlatÏch minc°.'')'
      IF (IRIT.EQ.6010) WRITE(LU,f)
C
 6015 F='(/'' Po pohybu stolem se otev˝ely tajnÇ dve˝e a odkryly dlouhÇ
     1rukavice.'')'
      IF (IRIT.EQ.6015) WRITE(LU,f)
C
      IF (IRIT.EQ.6015)IVEN(48)=9115
      IF (IRIT.EQ.6030) WRITE(LU,6030)
 6030 FORMAT(/
     1' Pod hadrami je podivn† kresba kompasu se st˝elkou ohnutou do'/
     2' U a hodin s dvÿma minutovÏmi ruüiükami; z nichß jedna stoj°'/
     3' na 10 a druh† na 12.')
C
      IF (IRIT.EQ.6040) WRITE(LU,6040)
 6040 FORMAT(/' Pod hadrami je podivn† kresba kompasu se st˝elkou',
     1' ukazuj°c°'/' na sever a hodin, kterÇ maji pouze minutovou',
     2' ruüiüku na 12.')
C
 6041 F='(/'' NemÖßete pohnout ani jedn°m kamenem.'')'
      IF (IRIT.EQ.6041) WRITE(LU,f)
C
      IF (IRIT.EQ.6042) WRITE(LU,6042)
 6042 FORMAT(/
     1' V touze po z°sk†n° vÿtÁ° ko˝isti se V†m poda˝ilo pohnout'/
     2' jedn°m z balvanÖ nespr†vnym smÿrem a ten se odkut†lel a'/
     3' zavalil jedinÏ vÏchod.')
      IF (IRIT.EQ.6042)IXT(319)=0
C
      IF (IRIT.EQ.6043) WRITE(LU,6043)
 6043 FORMAT(/' S velkou n†mahou se V†m poda˝ilo o kousek pohnout',
     1'jedn°m balvanem a'/' v jednom z kr†terÖ pod n°m jste naÁel'
     2' s†üek perel.')
C
 6044 F='(/'' D°ky rukavici se V†m poda˝ilo o kousek pohnout balvanem, a
     1le nic''/'' pod n°m nen°.'')'
      IF (IRIT.EQ.6044) WRITE(LU,f)
C
C                            MM-I-3  (LINE 8000)
C 4-I
C
 7010 F='(/'' Hlas v telefonu ˝ekl:'')'
      IF (IRIT .EQ. 7010) WRITE(LU,f)
C
      IF (IRIT .EQ. 7010) IVEN(42)=1000
 7015 F='('' Proü neodloß°te l°stek? Uß ho nebudete pot˝ebovat.'')'
      IF (IRIT .EQ. 7010.AND.IPR(3).EQ.1) WRITE(LU,f)
C
 7020 F='('' Proü se nepod°v†te na nÿkterÇ vÿci, kterÇ m†te u sebe?'')'
      IF (IRIT.EQ.7010.AND.IPR(3).EQ.2) WRITE(LU,f)
C
 7025 F='('' Kdybych byl V†mi, hl°dal bych vraha a zavolal policii!'')'
      IF (IRIT.EQ.7010.AND.IPR(3).EQ.3) WRITE(LU,f)
C
 7030 F='('' Neß se dostanete d†le, bylo by asi rozumnÇ vr†tit se k hlav
     1n° br†nÿ a''/'' vz°t si lucernu.'')'
      IF (IRIT.EQ.7010.AND.IPR(3).EQ.4) WRITE(LU,f)
C
 7080 F='('' a zavÿsil.'')'
      IF (IRIT .EQ. 7010) WRITE(LU,f)
C
 7090 F='(/'' Nikdo neodpov°d†.'')'
      IF (IRIT .EQ. 7090) WRITE(LU,f)
C
 7050 F='('' nic'')'
      IF (IRIT .EQ. 7050) WRITE(LU,f)
C
 8005 F='(/1X4A2'' pohnout nemÖßete, nikde zde nen°.'')'
      IF (IRIT .EQ. 8005) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 8006 F='(/'' Nem†te ß†dnÇ j°dlo.'')'
      IF (IRIT.EQ.8006) WRITE(LU,f)
C
 8010 F='(/'' Nev°m, co oüek†v†te, ße se stane'')'
      IF (IRIT .EQ. 8010 ) WRITE(LU,f)
C
 8015 F='(/1X4A2'' V†m snÿdl vÁechno j°dlo'')'
      IF (IRIT .EQ. 8015) WRITE(LU,f)(IRSN(L,IWRD(6,2)),L=1,4)
C
 8020 F='(/'' uß je'')'
      IF (IRIT .EQ. 8020) WRITE(LU,f)
C
 8030 F='(/1X4A2'' posunout nemÖßete, nen° zde'')'
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
 9027 F='(/'' Nem†te dostateünÏ poüet bodÖ'')'
      IF (IRIT.EQ.9027) WRITE(LU,f)
C
 9030 F='(/'' Podvodn°ku!! Nejste povolan† osoba.'')'
      IF (IRIT.EQ.9030) WRITE(LU,f)
C
 9040 F='(/'' Neotravujte mne, kdyß nejste autor.'')'
      IF (IRIT.EQ.9040) WRITE(LU,f)
C
10020 F='(/'' JeÁtÿ jste nep˝iÁel na to, ße to takto nejde?'')'
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
10031 F='(/'' Byl jste zranÿn v boji.'')'
      IF (IRIT.EQ.10031) WRITE(LU,f)
C                            MM-I-4  (LINE 8080)
C 5-I
C
10035 F='(/'' Dve˝e jsou zav˝enÇ'')'
      IF (IRIT.EQ.10035) WRITE(LU,f)
C
10040 F='(/'' Zabil jste ''4A2)'
      IF (IRIT.EQ.10040) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
10041 F='(/'' Zranil jste ''4A2)'
      IF (IRIT.EQ.10041) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
10045 F='(/'' Dve˝e jsou otev˝enÇ'')'
      IF (IRIT.EQ.10045) WRITE(LU,f)
C
10046 F='(/'' Dve˝e jsou zav˝enÇ'')'
      IF (IRIT.EQ.10046) WRITE(LU,f)
C
10060 F='(/'' V†Á protivn°k uprchl bÿhem ruün° vÏmÿny n†zorÖ'')'
      IF (IRIT.EQ.10060) WRITE(LU,f)
C
      IF (IRIT.EQ.10011)  THEN
10011 F='(/'' Pr†vÿ jste vamp°rovi zarazil kÖl do srdce. Vamp°r v mßiku'
     1'/'' zest†rl o tis°c let a promÿnil se v prach.'')'
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
     1/' Souüasn† situace hry je uloßena a bude vyhovovat pro'
     1/' revizi 'I3'. Nyni mÖßete ukonüit hru nebo d†le pokra-'
     2/' üovat. Budete-li cht°t pokraüovat v uloßenÇ h˝e, za-'
     3/' dejte OBNOVIT')
C
      IF (IRIT.EQ.11075) WRITE(LU,11075)
11075 FORMAT(/' Data nejsou uloßena.'/
     +' ChybnÇ jmÇno souboru, nedostatek prostoru nebo jin† chyba.')
C
12040 F='(/'' Nyn° jste ve stejnÇ situaci jako p˝i uloßen° hry.'')'
      IF ((IRIT .EQ. 12040) .AND. (ITST(15) .EQ. 17)) WRITE(LU,f)
C
12050 F='(/'' Tato data jsou pro jinou revizi'')'
      IF ((ITST(15) .NE. 17) .AND. (IRIT .EQ. 12040)) WRITE(LU,f)
C
12055 F='(/'' Aß budete hotov, zruÁte soubor. '')'
      IF (IRIT.EQ.11040.OR.IRIT.EQ.12040) WRITE(LU,f)
C
14010 F='(/'' MÖßete nastavit ü°slo z†hady prvn°m p˝°kazem po zah†jen° h
     1ry. Nyn° jste v z†hadÿ ü°slo''I4)'
      IF (IRIT.EQ.14010) WRITE(LU,f) ITST(23)
C
14110 F='(/'' To je jmÇno hry.'')'
      IF (IRIT.EQ.14110) WRITE(LU,f)
C
15110 F='(/'' V†Á kl°ü nepasuje do z†mku'')'
      IF (IRIT.EQ.15110) WRITE(LU,f)
C
15200 F='(/1X4A2'' se nezaj°m† o nic z toho, co m†te'')'
      IF (IRIT.EQ.15200) WRITE(LU,f)(IRSN(L,IWRD(6,2)),L=1,4)
C
15400 F='(/1X4A2'' V†m stejnÿ nic ne˝ekne '')'
      IF (IRIT.EQ.15400) WRITE(LU,f)(IRSN(L,IWRD(6,2)),L=1,4)
C
15500 F='(/1X4A2'' si vzal pokladnu a potÇ V†m ˝ekl, ße vraßdu sp†chal '
     1'4A2/1XA2,1X,8A2'', a ße vraßednÏm n†strojem je''4A2)'
      IF (IRIT.EQ.15500.AND. ITST(8).NE.0) WRITE(LU,f)(IRSN(L,IWRD
     1(6,2)),L=1,4),(IRSN(L,ITST(9)),L=1,4),
     2 IPRP(1,MMPRP(ITST(8))),(IRNM(L,IFSD(IROM(ITST(8)))),
     3 L=1,8),(IWP(L),L=1,4)
C
15510 F='(/1X4A2'' si vzal od V†s pokladnici a po malÇm zav†h†n° odpovÿd
     1ÿl,''/'' ße mÖßete odej°t podkrovn° svÿtnic°.'')'
      IF (IRIT.EQ.15500.AND.ITST(8).EQ.0) WRITE(LU,f)(IRSN(L,IWRD(6,
     12)),L=1,4)
C                            MM-I-5  (LINE 8160)
C 6-I
16035 F='(/'' Rozbil jste z†mek na kousky'')'
      IF (IRIT.EQ.16035) WRITE(LU,f)
C
16045 F='(/'' Zabil jste sk˝eta a ten zmizel v oblaku dusivÇho üernÇho d
     1Ïmu'')'
      IF (IRIT.EQ.16035)IVEN(35)=IVEN(35)+1000
      IF (IRIT.EQ.16045) WRITE(LU,f)
      IF (IRIT.EQ.16045)IRES(13)=IRES(13)+100
C
20010 F='(/'' Pot˝ebujete olejniüku nebo nÿco podobnÇho'')'
      IF (IRIT .EQ. 20010) WRITE(LU,f)
C
20020 F='(/'' Olejniüka je pr†zdn†'')'
      IF (IRIT .EQ. 20020) WRITE(LU,f)
C
20030 F='(/'' Nebu‘te hloupÏ'')'
      IF (IRIT .EQ. 20030) WRITE(LU,f)
C
*      IF (IRIT.EQ.21020) WRITE(LU,21020)ITST(21)
*21020 FORMAT(/' Tento a vsechny nasledujici radky budou zaznamenavany',
*     1' na lu 'I3)
C
22010 F='(/'' Zde nemÖßete rozb°t z†mek truhly'')'
      IF (IRIT.EQ.22010) WRITE(LU,f)
C
22020 F='(/'' Nem†te nic, ü°m by Áel rozb°t z†mek truhly'')'
      IF (IRIT.EQ.22020) WRITE(LU,f)
C
23010 F='(/'' Vamp°r V†s ochromil uhranüivÏm pohledem, zvolna k V†m kr†ü
     1° a ˝°k†,''/'' ße se jmenuje hrabÿ DRACULA'')'
      IF (IRIT.EQ.23010) WRITE(LU,f)
C
23015 F='(/'' OdpornÏ slizkÏ tvor k V†m pr†vÿ kr†ü° a chrocht†'')'
      IF (IRIT.EQ.23015) WRITE(LU,f)
C
23020 F='(/'' “†belskÏ malÏ tvor si V†s prohl°ß° a checht† se.'')'
      IF (IRIT.EQ.23020) WRITE(LU,f)
C
23030 F='(/'' ¸ik†, ße s V†mi pÖjde k hlavn° br†nÿ a pomÖße V†m ''/'' ji
     1 otev˝°t, jestliße o to stoj°te.'')'
      IF (IRIT.EQ.23030) WRITE(LU,f)
C
23040 F='(/'' ¸°k†, ße br†na je tÿßk† a pomÖße V†m ji otev˝°t, jestliße'
     1'/'' ˝eknete: OTEV¸íT BRèNU'')'
      IF (IRIT.EQ.23040) WRITE(LU,f)
C
23050 F='(/'' ¸°k†, ße vßdy, kdyß je zde s p˝°telem, zav˝e br†nu'')'
      IF (IRIT.EQ.23050) WRITE(LU,f)
C
23060 F='(/'' ¸°k†, ße mÖßete pouß°t mapy, kter† je v chaloupce.'')'
      IF (IRIT.EQ.23060) WRITE(LU,f)
C
23070 F='(/'' ¸°k†, ße m†te d†vat pozor na 500 stop vysokÏ skaln° £tes n
     1a''/'' vÏchodn° stranÿ s°dla.'')'
      IF (IRIT.EQ.23070) WRITE(LU,f)
C
23080 F='(/'' ¸°k†, ße se boj° j°t do chaloupky.'')'
      IF (IRIT.EQ.23080) WRITE(LU,f)
C
23090 F='(/'' ¸°k†, ße v° o nÿüem lepÁ°m neß bloudit v lese'')'
      IF (IRIT.EQ.23090) WRITE(LU,f)
C
C
C                            MM-I-6  (LINE 8240)
C 7-I
C
23100 F='(/'' ¸°k†, ße nesm° j°t do zahrady.'')'
      IF (IRIT.EQ.23100) WRITE(LU,f)
C
23110 F='(/'' ¸°k†, ße hlavn° vchod je nebezpeünÏ.'')'
      IF (IRIT.EQ.23110) WRITE(LU,f)
C
      IC1=' '
      IF (IWRD(6,2).EQ.2.OR.IWRD(6,2).EQ.8) IC1='a'
23120 F='(/1X4A2'' zaü°n† bÏt otr†ven''A1'' VaÁimi dotazy'')'
      IF (IRIT.EQ.23120) WRITE(LU,f)(IRSN(L,IWRD(6,2)),L=1,4),IC1
C
23140 F='(/'' VaÁe posledn° ot†zka narazila na hluchÇ uÁi.'')'
      IF (IRIT.EQ.23140) WRITE(LU,f)
C
23145 F='(/1X4A2'' zaü°n† bÏt opravdu znechucen''A1'' VaÁimi dotazy.'')'
      IF (IRIT.EQ.23145) WRITE(LU,f)(IRSN(L,IWRD(6,2)),L=1,4),IC1
C
23150 F='(/1X4A2'' odpovÿdÿl''A1'': o vraßdÿ nev°m nic.'')'
      IF (IRIT.EQ.23150) WRITE(LU,f)(IRSN(L,IWRD(6,2)),L=1,4),IC1
C
23170 F='(/'' Na VaÁi ot†zku odpovÿdÿl''A1'' se zav†h†n°m:'')'
      IF (IRIT.GE.23170.AND.IRIT.LE.23190) WRITE(LU,f) IC1
C
23175 F='('' Mysl°m, ße vrah je ''4A2)'
      IF (IRIT.EQ.23170) WRITE(LU,f)(IRSN(L,ITST(9)),L=1,4)
C
23180 F='('' Mysl°m, ße vraßda byla sp†ch†na ''A2,1X,8A2)'
      IF (IRIT.EQ.23180) WRITE(LU,f)IPRP(1,MMPRP(ITST(8))),
     1 (IRNM(L,IFSD(IROM(ITST(8)))),L=1,8)
C
23190 F='('' Mysl°m, ße vraßednou zbran° je ''4A2)'
      IF (IRIT.EQ.23190) WRITE(LU,f)(IWP(L),L=1,4)
C
23210 F='(/'' ¸°k†, ße se mu nechce odej°t ze s°dla.'')'
      IF (IRIT.EQ.23210) WRITE(LU,f)
C
23220 F='(/'' ¸°k†, ße existuje vstup do tajnÇ laborato˝e z velkÇ loßnic
     1e.'')'
      IF (IRIT.EQ.23220) WRITE(LU,f)
C
23230 F='(/'' ¸°k†, ße tajn† chodba spojuje vÁechna t˝i podlaß° s°dla.''
     1)'
      IF (IRIT.EQ.23230) WRITE(LU,f)
C
23240 F='(/'' ¸°k†, ße slußka um° bÏt velmi p˝†telsk†.'')'
      IF (IRIT.EQ.23240) WRITE(LU,f)
C
23270 F='(/'' ¸°k†, ße se boj° j°t pod hlavn° podlaß° s°dla'')'
      IF (IRIT.EQ.23270) WRITE(LU,f)
C
23280 F='(/'' ¸°k†, ße neopust° s°dlo ani zahradu'')'
      IF (IRIT.EQ.23280) WRITE(LU,f)
C
23290 F='(/'' ¸°k†, ße um° p˝eü°st n†pis na truhle.'')'
      IF (IRIT.EQ. 23290) WRITE(LU,f)
C
23300 F='(/'' ¸°k†, ße sk˝°tek schov†v† vÿci ve studni.'')'
      IF (IRIT.EQ.23300) WRITE(LU,f)
C
23330 F='(/'' ¸°k†, ße na konci chladnÇ chodby je pokladna'')'
      IF (IRIT.EQ.23330) WRITE(LU,f)
C
23340 F='(/'' ¸°k†, ße mÖßete odl†kat vlka potravou.'')'
      IF (IRIT.EQ.23340) WRITE(LU,f)
C                            MM-I-7  (LINE 8320)
C 8-I
C
23350 F='(/'' ¸°k†, ße mÖßete stÿny zastavit, kdyß u nich nech†te nÿjakÏ
     1 p˝edmÿt.'')'
      IF (IRIT.EQ.23350) WRITE(LU,f)
C
23360 F='(/'' ¸°k†, ße mÖßete p˝eü°st svitek pomoc° magickÏch slov'')'
      IF (IRIT.EQ.23360) WRITE(LU,f)
C
24001 F='(/'' SlyÁ°te tlachy opice.'')'
      IF (IRIT.EQ.24001) WRITE(LU,f)
C
24002 F='(/'' SlyÁ°te hukot ohnÿ v krbu.'')'
      IF (IRIT.EQ.24002) WRITE(LU,f)
C
24003 F='(/'' Zespodu k V†m dolÇhaj° podivnÇ zvuky.'')'
      IF (IRIT.EQ.24003) WRITE(LU,f)
C
24004 F='(/'' SlyÁ°te sk˝°pot podlahy p˝i kaßdÇm kroku.'')'
      IF (IRIT.EQ.24004.OR.IRIT.EQ.24013.OR.IRIT.EQ.24022)
     1 WRITE(LU,f)
C
24005 F='(/'' Je zde mrtvÇ ticho.'')'
      IF (IRIT.EQ.24005) WRITE(LU,f)
C
24006 F='(/'' SlyÁ°te, jak nÿkdo chod° v pokoji nad V†mi.'')'
      IF (IRIT.EQ.24006) WRITE(LU,f)
C
24007 F='(/'' SlyÁ°te jen hluk, kterÏ zpÖsobujete n†razy na vÿci.'')'
      IF (IRIT.EQ.24007) WRITE(LU,f)
C
24008 F='(/'' SlyÁ°te piÁtÿn° nÿkolika krys pob°haj°c°ch okolo.'')'
      IF (IRIT.EQ.24008) WRITE(LU,f)
C
24009 F='(/'' SlyÁ°te, jak nÿjakÇ zv°˝e Ákr†be a chce se dostat dovnit˝.
     1'')'
      IF (IRIT.EQ.24009) WRITE(LU,f)
C
24010 F='(/'' MÖßete m°t sebelepÁ° uÁi, ale stejnÿ nic neuslyÁ°te.'')'
      IF (IRIT.EQ.24010) WRITE(LU,f)
C
24011 F='(/'' MÖßete slyÁet slabou ozvÿnu vysoko nad V†mi.'')'
      IF (IRIT.EQ.24011) WRITE(LU,f)
C
24012 F='(/'' SlyÁ°te hraj°c° radio.'')'
      IF (IRIT.EQ.24012.AND.IFD(IVEN(75)).EQ.1) WRITE(LU,f)
C
24212 F='(/'' SlyÁ°te nÿco, co zni jako hrom v blizkÏch hor†ch'')'
      IF (IRIT.EQ.24012.AND.IFD(IVEN(75)).NE.1) WRITE(LU,f)
C
24014 F='(/'' SlyÁ°te hukot ohnÿ v krbu.'')'
      IF (IRIT.EQ.24014.AND.IFD(IVEN(89)).EQ.1) WRITE(LU,f)
C
24214 F='(/'' SlyÁ°te jen ozvÿnu svÏch krokÖ.'')'
      IF (IRIT.EQ.24014.AND.IFD(IVEN(89)).NE.1) WRITE(LU,f)
C
24015 F='(/'' SlyÁ°te, jak nÿkdo pod podlahou dÿl† hluk.'')'
      IF (IRIT.EQ.24015) WRITE(LU,f)
C
24016 F='(/'' SlyÁ°te tichou varhann° hudbu.'')'
      IF (IRIT.EQ.24016) WRITE(LU,f)
C
24017 F='(/'' SlyÁ°te slabÏ zvuk varhan.'')'
      IF (IRIT.EQ.24017) WRITE(LU,f)
C
24018 F='(/'' NeslyÁ°te vÖbec nic.'')'
      IF (IRIT.EQ.24018) WRITE(LU,f)
C                            MM-I-8  (LINE 8400)
C 9-I
C
24019 F='(/'' SlyÁ°te, jak V†m tluüe srdce.'')'
      IF (IRIT.EQ.24019) WRITE(LU,f)
C
24020 F='(/'' SlyÁ°te, jak venku fouk† v°tr ''4A2)'
      IF (IRIT.EQ.24020) WRITE(LU,f)(IDTN(L,IDS),L=1,4)
      IF (IRIT.EQ.24024) WRITE(LU,f)(IDTN(L,IDE),L=1,4)
      IF (IRIT.EQ.24025) WRITE(LU,f)(IDTN(L,IDN),L=1,4)
C
24021 F='(/'' SlyÁ°te, jak V†m v°tr hv°zd† kolem uÁ°.'')'
      IF (IRIT.EQ.24021) WRITE(LU,f)
C
24023 F='(/'' SlyÁ°te, jak nad V†mi rachot° ˝etÿzy.'')'
      IF (IRIT.EQ.24023.OR.IRIT.EQ.24026) WRITE(LU,f)
C
24027 F='(/'' SlyÁ°te vlastn° dech.'')'
      IF (IRIT.EQ.24027) WRITE(LU,f)
C
24028 F='(/'' SlyÁ°te Áest rÖznÏch ozvÿn kaßdÇho zvuku, kterÏ vyd†te.'')
     1'
      IF (IRIT.EQ.24028) WRITE(LU,f)
C
24029 F='(/'' SlyÁ°te pÿt rÖznÏch ozvÿn kaßdÇho zvuku, kterÏ vyd†v†te.''
     1)'
      IF (IRIT.EQ.24029) WRITE(LU,f)
C
24030 F='(/'' SlyÁ°te üty˝i rÖznÇ ozvÿny kaßdÇho zvuku, ktery vyd†v†te.'
     1')'
      IF (IRIT.EQ.24030) WRITE(LU,f)
C
24031 F='(/'' SlyÁ°te t˝i rÖznÇ ozvÿny kaßdÇho zvuku, kterÏ vyd†v†te.'')
     1'
      IF (IRIT.EQ.24031) WRITE(LU,f)
C
24032 F='(/'' SlyÁ°te dvÿ rÖznÇ ozvÿny kaßdÇho zvuku, kterÏ vyd†v†te.'')
     1)'
      IF (IRIT.EQ.24032) WRITE(LU,f)
C
24033 F='(/'' SlyÁ°te ozvÿnu kaßdÇho zvuku, kterÏ vyd†v†te.'')'
      IF (IRIT.EQ.24033.OR.IRIT.EQ.24034) WRITE(LU,f)
C
24036 F='(/'' SlyÁ°te, jak se stÿny p˝iblißuj°.'')'
      IF (IRIT.GT.24035.AND.IRIT.LT.24041) WRITE(LU,f)
C
24041 F='(/'' SlyÁ°te vodu, proud°c° pod V†mi.'')'
      IF (IRIT.EQ.24041) WRITE(LU,f)
C
24042 F='(/'' SlyÁ°te, jak nÿkdo pobliß kope.'')'
      IF (IRIT.GT.24041.AND.IRIT.LT.24054.AND.IRES(4).NE.0)
     1 WRITE(LU,f)
C
24043 F='(/'' SlyÁ°te sk˝°pÿn°, jak se tunel uzav°r†'')'
      IF (IRIT.GT.24041.AND.IRIT.LT.24054.AND.IRES(4).EQ.0)
     1 WRITE(LU,f)
C
24088 F='(/'' SlyÁ°te slabÏ zvuk stroje.'')'
      IF (IRIT.EQ.24088) WRITE(LU,f)
C
24100 F='(/'' NeslyÁ°te nic zaj°mavÇho.'')'
      IF (IRIT.EQ.24100) WRITE(LU,f)
C
24105 F='(''  '')'
      IF (IRIT.EQ.24105) WRITE(LU,f)
C
24108 F='('' SlyÁ°te tikat hodinky.'')'
      IF (IPR(3).EQ.24108) WRITE(LU,f)
C
24110 F='('' SlyÁ°te, jak nÿco pobliß dÿla hluk.'')'
      IF (IPR(3).EQ.24110) WRITE(LU,f)
C                            MM-I-9  (LINE 8480)
C 10-I
C
24120 F='('' SlyÁ°te, jak nÿkdo pobl°ß dÿl† hluk.'')'
      IF (IPR(3).EQ.24120) WRITE(LU,f)
C
24130 F='('' SlyÁ°te, jak nÿkdo Áept†.'')'
      IF (IPR(3).EQ.24130) WRITE(LU,f)
C
24140 F='('' SlyÁ°te, jak nÿkdo mluv°.'')'
      IF (IPR(3).EQ.24140) WRITE(LU,f)
C
24150 F='('' SlyÁ°te, ße se lesn°k a lovec smÿj° a ˝°kaj° nÿco o zav˝en°
     1 br†ny.'')'
      IF (IPR(3).EQ.24150) WRITE(LU,f)
C
24160 F='('' SlyÁ°te, ße p†n a sluha ˝°kaj° nÿco jako vraßda a '',4A2)'
      IF (IPR(3).EQ.24160) WRITE(LU,f)(IRSN(L,ITST(9)),L=1,4)
C
24162 F='('' SlyÁ°te, jak p†n a sluha si ˝°kaj° nÿco jako:''/'' "stalo s
     1e to ''A2,1X,8A2''"'')'
      IF (IPR(3).EQ.24162) WRITE(LU,f)IPRP(1,MMPRP(ITST(8))),
     1 (IRNM(L,IFSD(IROM(ITST(8)))),L=1,8)
C
24164 F='('' SlyÁ°te, jak p†n ˝°k† sluhovi nÿco jako vraßda a '',4A2)'
      IF (IPR(3).EQ.24164) WRITE(LU,f)(IWP(L),L=1,4)
C
24170 F='('' Z rozhovoru kucha˝e a sada˝e jste zaslechl mimo jinÇ slova
     1 ''/'' vraßda a ''4A2)'
      IF (IPR(3).EQ.24170) WRITE(LU,f)(IRSN(L,ITST(9)),L=1,4)
C
24172 F='('' SlyÁ°te, jak si kucha˝ se sada˝em ˝°kaj° cosi''/''  o vraßd
     1ÿ ''A2,1X,8A2)'
      IF (IPR(3).EQ.24172) WRITE(LU,f)IPRP(1,MMPRP(ITST(8))),
     1 (IRNM(L,IFSD(IROM(ITST(8)))),L=1,8)
C
24174 F='('' SlyÁ°te, jak kucha˝ ˝°k† sada˝ovi nÿco jako vraßda a ''4A2)
     1'
      IF (IPR(3).EQ.24174) WRITE(LU,f)(IWP(L),L=1,4)
C
24180 F='('' Z rozhovoru pan° a slußky jste zaslechl mimo jinÇ slova''/'
     1' vraßda a ''4A2)'
      IF (IPR(3).EQ.24180) WRITE(LU,f)(IRSN(L,ITST(9)),L=1,4)
C
24182 F='('' Z rozhovoru pan° a slußky jste zaslechl nÿco jako m°sto''/'
     1' vraßdy a ''A2,1X,8A2)'
      IF (IPR(3).EQ.24182) WRITE(LU,f)IPRP(1,MMPRP(ITST(8))),
     1 (IRNM(L,IFSD(IROM(ITST(8)))),L=1,8)
C
24184 F='('' SlyÁ°te, jak pan° ˝°k† slußce nÿco jako vraßda a ''4A2)'
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