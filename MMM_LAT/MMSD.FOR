$storage:2
$large
$nodebug
      subroutine mmsd(p1,p2,p3,p4,p5)
c     10-02-1983 MMM    8536E D45 PL&BM <850920.1730>
C     04-02-1988 MS-DOS S.M.
C
C ******* SEGMENT D *******
C
C  tato cast vypise zpravu a vrati se do cteciho segmentu MMSB
C  nebo legalne ukonci program.
C
$INCLUDE:'MMCO.for'
      character*127 f
      DIMENSION ITIM(5),IDATA(19,32),ICOM(64,30),ICOMM(37),
     1IVAL(52),IRTN(5)
      INTEGER POM(10),p1,p2,p3,p4,p5
C
C *** FUNCTIONS ***
C
      IFSD(I)=I-(I/100)*100
      ITD(I)=I/100-(I/1000)*10
C
C *** nastaveni promennych ***
C
*     CALL RMPAR(IPR)
      ipr(1)=p1
      ipr(2)=p2
      ipr(3)=p3
      ipr(4)=p4
      ipr(5)=p5
      LU=IPR(1)
      LX=ITST(21)
      IRIT=IPR(2)
      p1=lu
      p2=0
      p3=0
      p4=0
      p5=0
      DATA IVAL/3,2,1,2,5,2,10,2,2,1,1,1,1,1,1,1,4,4,1,2,1,1,10,1,1,
     110,1,1,1,4,1,1,1,1,10,10,10,10,10,10,10,10,10,2,5,1,1,10,5,1,0,0/
      DATA IRTN/0,0,0,0,0/
C
C *** konec hry ***
C
      IF (IRIT.EQ.11111) GOTO 99990
C
C *** hrac nekoho zabil ***
C
      IF (ITST(9).NE.2.AND.IRES(2).GE.10000)ITST(24)=1
      IF (ITST(9).NE.6.AND.IRES(6).GE.10000)ITST(24)=1
      IF (ITST(9).NE.7.AND.IRES(7).GE.10000)ITST(24)=1
      IF (ITST(9).NE.8.AND.IRES(8).GE.10000)ITST(24)=1
      IF (ITST(9).NE.9.AND.IRES(9).GE.10000)ITST(24)=1
      IF (ITST(9).NE.12.AND.IRES(12).GE.10000)ITST(24)=1
      IF (ITST(9).NE.14.AND.IRES(14).GE.10000)ITST(24)=1
C
C *** ztrata bodu od neskodneho vampira ***
C
      IF (IRES(5).EQ.0)IRES(5)=100
      IF (ITD(IRES(5)).NE.1.AND.ITST(5).GT.300)ITST(3)=ITST(3)-75
      IF (ITD(IRES(5)).NE.1.AND.ITST(5).GT.300)IRES(5)=IRES(5)+100
C
C *** sebevrazda ***
C
      IF (IRIT.EQ.6005) GOTO 80000
C                            MM-D-1  (LINE 3200)
C
C 2-D
C
C   *** ukoncujici zpravy ***
C
C ??
C
  999 continue
  110 F='(/'' Jste neohrabanì çaçek! Zakopl jste a ztratil vçe.'')'
      IF (IRIT.EQ.110) WRITE(LU,f)
C
  150 F='(/'' Slovo je pý¡liç dlouh‚'')'
      IF (IRIT.EQ.150) WRITE(LU,f)
C
  170 F='(/'' Pý¡liç mnoho slov'')'
      IF (IRIT.EQ.170) WRITE(LU,f)
C
  160 F='(/'' M…j vstupn¡ slovn¡k neobsahuje slovo '',4A2)'
      IF (IRIT.EQ.160) WRITE(LU,f) (IWRD(L,IPR(3)),L=1,4)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  VYBRANE A MENE VYBRANE NADAVKY
C
      IF (IRIT.NE.165) GOTO 1600
      GOTO (1601,1602,1603,1604,1605,1606,1607,1608,1609,1610),NORM(10)
C
1601  F='(/'' Pozor na jazyk!'')'
      WRITE(LU,f)
      GOTO 1600
C
1602  F='(/'' Moc si trouf ç, cizinŸe !'')'
      WRITE(LU,f)
      GOTO 1600
C
1603  F='(/'' Douf m, §e jsi t¡m nemyslel mne, mohlo by tØ to pýij¡t dra
     +ho !'')'
      WRITE(LU,f)
      GOTO 1600
C
1604  F='(/''     syŸ ku !''/)'
      WRITE(LU,f)
      GOTO 1600
C
1605  F='(/'' Zkus se alespoå chvilku chovat jako ŸlovØk !'')'
      WRITE(LU,f)
      GOTO 1600
C
1606  F='(/'' BlbŸe tupŸe omezenŸe,''/'' kr vo savŸe vykulenŸe,''/'' zý¡
     +cenino  balvane,''/'' koz¡ bobku, havrane !'')'
      WRITE(LU,f)
      GOTO 1600
C
C
C
1607  F='(/'' Chceç do ýepy ?'')'
      WRITE(LU,f)
      GOTO 1600
C
1608  F='(/''       blbeŸku!'')'
      WRITE(LU,f)
      GOTO 1600
C
1609  F='(/''    ..........!!!!(cenz…rov no)'')'
      WRITE(LU,f)
      GOTO 1600
C
1610  WRITE(LU,1640)
1640  FORMAT(/' Ty jedna terenn¡ kr…to, ty si mysliç, §e si m…§eç'/
     +        ' vçechno dovolit ?'/
     +        ' Tak to tedy ne ! '/
     +        ' Mezi n mi dvØma je tady velikej rozd¡l !'/
     +        ' Na rozd¡l od tebe MYSL’M ! ')
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
1600  F='(/'' Neodpov¡d m na § dn‚ ot zky. Pouze reaguji na Vaçe akce'')
     1'
      IF (IRIT.EQ.166) WRITE(LU,f)
C
  176 F='(/'' Nemohl jste d‚le udr§et dech a utonul jste.''/'' MØl jste
     1se vr tit dokud byl Ÿas.'')'
      IF (IRIT.EQ.176) WRITE(LU,f)
      IF (IRIT.EQ.176) GOTO 80000
C
  180 F='(/'' Jako prvni slovo oŸek v m sloveso'')'
      IF (IRIT.EQ.180) WRITE(LU,f)
C
      IF (IRIT.NE.185) GOTO 188
  185 FORMAT(/' Slyç¡te exploze a hlubok‚ dunØn¡, jako by se nØkter‚',
     1' z chodeb'/' pod z mkem a zahradou propadly.',
     2' Jste tØ§ce otýesen a vçude'/' kolem V s je zk za. '
     3'C¡t¡te kouý.')
      WRITE(LU,185)
C
      DO 186 J=301,360
  186 IXT(J)=0
      IXT(173)=0
      IXT(26)=0
      IXT(198)=0
      IXT(137)=0
      IXT(46)=0
      IXT(237)=0
      IXT(197)=0
  187 CONTINUE
C
  188 F='(/'' Byl jste uduçen kouýem a Vaçe tØlo je pomalu konzumov no p
     1lameny,''/'' celì tajemnì z mek miz¡ v kouýi.'')'
      IF (IRIT.EQ.189) WRITE(LU,f)

      IF (IRIT.EQ.189) GOTO 90020
C
 1017 F='(/'' Nem…§ete se prot hnout malìmi dveými.'')'
      IF (IRIT.EQ.1017) WRITE(LU,f)
C
 1027 F='(/'' Vstoupil jste do tunelu, kterì je a§ po Vaçi hlavu zaplnØn
     :ì''/'' rychle proud¡c¡ vodou.'')'
      IF (IRIT.EQ.1027) WRITE(LU,f)
C
 1093 F='(/'' Pr vØ jste zakopl o klubko jedovatìch had… a mnoho z nich
     :V s uçtklo a plaz¡ se po Vaçem mrtv‚m tØle.'')'
      IF (IRIT.EQ.1093) WRITE(LU,f)
      IF (IRIT.EQ.1093) GOTO 80000
C
C                            MM-D-2  (LINE 3280)
C
C 3-D
C
C
C  *** uplne ukonceni po 6. hod, u brany ***
C
      IF (IRIT.NE.191) GOTO 1199
  191 FORMAT(/' Pýekonal jste vçechny n strahy osudu a dok zal jste'/
     :' nemo§n‚, pýe§il jste zk zu Tajemn‚ho z mku, kterì se pýed'/
     :' V mi nyn¡ mØn¡ v popel.')
      WRITE(LU,191)
      IC=0
      DO 1190 I=1,52
      IF (IVEN(I).GT.20000)IC=IC+IVAL(I)
 1190 CONTINUE
C
C
C
 1191 F='(/'' Vaçe sk¢re zahrnuje ''I3'' bod… za vØci, kter‚ m te s sebo
     1u.'')'
      WRITE(LU,f)IC
      IF (LU.NE.ITST(21))ITST(2)=ITST(2)+IC
      IF (IXT(52).EQ.0.AND.LU.NE.ITST(21))  ITST(2)=ITST(2)+10
 1192 F='(/'' Vaçe sk¢re tak‚ obsahuje 10 bod…, proto§e dro§ka na V s Ÿe
     1k .'')'
      IF (IXT(52).EQ.0) WRITE(LU,f)
C
C  *** pokud se propadne propadlistem, ceka na nej drozka ***
C
      IF (ITST(2).GT.999)ITST(2)=999
 1193 F='(/'' TeÔ mus¡te j¡t do mØsta pØknØ po svìch.'')'
      IF (IXT(52).NE.0) WRITE(LU,f)
      GOTO 90020
C
1199  continue
  190 F='(/'' V jedn‚ vØtØ nemohu rozliçit v¡ce ne§ jedno sloveso'')'
      IF (IRIT.EQ.190) WRITE(LU,f)
C
  192 F='(/'' Nech v m to na V s, s m jste se dostal do takov‚ kaçe, §e
     :V s z n¡ nemohu dostat.'')'
      IF (IRIT.EQ.192) WRITE(LU,f)
      IF (IRIT.EQ.192) GOTO 90020
C
  213 F='(/'' Lehkì v nek V m sfoukl sv¡Ÿku.'')'
      IF (IRIT.EQ.213) WRITE(LU,f)
      IF (IRIT.EQ.213)IVEN(19)=20200
C
C ??
C
  244 F='(/'' Vìbuch bomby V s roztrhal na kousky'')'
      IF (IRIT.EQ.244) WRITE(LU,f)
C
      IF (IRIT.EQ.244) GOTO 80000
  251 F='(/'' Vrah mØl strach, §e zavol te policii a proto V s''/'' pro
     :jistotu zabil.'')'
      IF (IRIT.EQ.251) WRITE(LU,f)
C
      IF (IRIT.EQ.251) GOTO 80000
  254 F='(/'' Skýet po V s hodil sekerou a rozsekl V m hlavu na''/'' dvØ
     : poloviny.'')'
      IF (IRIT.EQ.254) WRITE(LU,f)
C
      IF (IRIT.EQ.254) GOTO 80000
  262 F='(/'' Up¡r se do V s zakousl a vys l V m vçechnu krev.'')'
      IF (IRIT.EQ.262) WRITE(LU,f)
      IF (IRIT.EQ.262) GOTO 80000
C
  285 F='(/'' Vlkodlak na V s za£toŸil a rozs pal V s na kousky.'')'
      IF (IRIT.EQ.285) WRITE(LU,f)
      IF (IRIT.EQ.285) GOTO 80000
C
      IF (IRIT.EQ.287) THEN
  287    F='(/'' V cestØ V m br n¡ ''I2''''\)'
         WRITE(LU,f) IPR(3)
  286    F='(''výeçt¡c¡ d‚mon.'')'
         IF (IPR(3) .EQ. 1) WRITE(LU,f)
  288    F='(''výeçt¡c¡ d‚moni.'')'
         IF (IPR(3) .GE. 2 .AND. IPR(3) .LE. 4) WRITE(LU,f)
  289    F='(''výeçt¡c¡ch d‚mon….'')'
         IF (IPR(3) .GT. 4) WRITE(LU,f)
      endif
C
C                            MM-D-3  (LINE 3360)
C
C 4-D
C
  290 F='(/'' Naprosto nic se nestalo!'')'
      IF (IRIT.EQ.290) WRITE(LU,f)
C
  320 F='(/'' Nen¡ mi jasn‚, co se mi pokouç¡te sdØlit'')'
      IF (IRIT.EQ.320) WRITE(LU,f)
C
  340 F='(/'' ZeÔ je pý¡liç vysok '')'
      IF (IRIT.EQ.340) WRITE(LU,f)
C
  350 F='(/'' Nikde v okoli § dnou zeÔ nevid¡m'')'
      IF (IRIT.EQ.350) WRITE(LU,f)
C
  360 F='(/'' Musel byste bìt v lese, abyste mohl çplhat na strom'')'
      IF (IRIT.EQ.360) WRITE(LU,f)
C
C
  370 FORMAT(/' PravdØpodobnØ byste spadl ze stromu.'/
     :' Takto jsem pýiçel ji§ o mnoho hr Ÿ…, a proto V m nemohu'/
     :' dovolit çplhat na jakìkoliv strom')
      IF (IRIT.EQ.370) WRITE(LU,370)
C
C ??
C
  380 FORMAT(/' Sadaý V s chytil, dovlekl V s na verandu a ýekl V m,'/
     :' abyste radØji odeçel, jestli chcete i nad le jednat'/
     :' takovìm zp…sobem')
      IF (IRIT.EQ.380) WRITE(LU,380)
C
  385 F='(/'' Br na je pokryta r…znìmi druhy bodc… a ostnatìch dr t… a n
     :ikdy''/'' byste ji nepýekonal §iv a zdr v.'')'
      IF (IRIT.EQ.385) WRITE(LU,f)
C
  390 F='(/'' Nejste u br ny.'')'
      IF (IRIT.EQ.390) WRITE(LU,f)
C
  410 F='(/'' Nepýedpokl d m, §e byste zde chtØl nØkam vstupovat'')'
      IF (IRIT.EQ.410) WRITE(LU,f)
C
  510 F='(/'' N hle V s zahalil kouý a kdy§ se rozptìlil, zjistil jste,
     :''/'' §e jste na verandØ'')'
      IF (IRIT.EQ.510) WRITE(LU,f)
C
  520 F='(/'' Nic se nestalo'')'
      IF (IRIT.EQ.520) WRITE(LU,f)
C
  610 F='(/'' N hle V s zahalil kouý a kdy§ se rozptìlil, zjistil jste,
     :§e jste v lese'')'
      IF (IRIT.EQ.610) WRITE(LU,f)
C
  710 F='(/'' N hle V s zahalil kouý a kdy§ se rozptìlil, zjistil jste,
     :§e jste na mostØ'')'
      IF (IRIT.EQ.710) WRITE(LU,f)
C
      IF (IRIT.EQ.810) IC=MMPRP(ITST(8))
  810 FORMAT(/' Svitek se magicky rozvinul na stole a Ÿtete, §e '4A2/
     :' je vrah, vra§da byla sp ch na 'A2' '8A2' a'/
     :' smrt¡c¡ zbran¡ je '4A2)
      IF (IRIT.EQ.810) WRITE(LU,810)(IRSN(L,ITST(9)),L=1,4),IPRP(1,IC),
     : (IRNM(L,IFSD(IROM(ITST(8)))),L=1,8),(IWP(L),L=1,4)
C
  910 F='(/'' Zde nem…§ete kopat.'')'
      IF (IRIT.EQ.910) WRITE(LU,f)
C
  920 F='(/'' Bez lopatky se daleko nedostanete.'')'
      IF (IRIT.EQ.920) WRITE(LU,f)
C
  960 F='(/'' Myslete rychleji. ¬ek m na odpovØÔ.'')'
      IF (IRIT.EQ.960) WRITE(LU,f)
C
  963 F='(/'' ZaŸ¡n m bìt z toho Ÿek n¡ unavenì.'')'
      IF (IRIT.EQ.963) WRITE(LU,f)
C
  966 F='(/'' RadØji nØco napiçte. U§ nebudu dlouho Ÿekat.'')'
      IF (IRIT.EQ.966) WRITE(LU,f)
C
  970 F='(/'' Konec! Ja V s varoval.@@'')'
      IF (IRIT.EQ.970) WRITE(LU,f)
C
      IF (IRIT.EQ.970) GOTO 90020
  980 F='(/1X4A2'' zde nen¡.'')'
      IF (IRIT.EQ.980) WRITE(LU,f)(IRSN(L,IWRD(6,2)),L=1,4)
C
C
C                            MM-D-4  (LINE 3440)
C 5-D
C
 1002 F='(/'' ZapomnØl jsem, kde jste'')'
      IF (IRIT.EQ.1002) WRITE(LU,f)
C
 1005 F='(/'' Potýebujete kompas, abyste urŸil svØtov‚ strany'')'
      IF (IRIT.EQ.1005) WRITE(LU,f)
C
 1021 F='(/'' Dveýe jsou zamcen‚'')'
      IF (IRIT.EQ.1021) WRITE(LU,f)
C
 1023 F='(/'' Br na je zavýen '')'
      IF (IRIT.EQ.1023) WRITE(LU,f)
C
 1026 F='(/'' Pr vØ jste spadl z 500 s h… vysok‚ho £tesu na sk ly dole.'
     :')'
      IF (IRIT.EQ.1026) WRITE(LU,f)
C
      IF (IRIT.EQ.1026) GOTO 80000
      IF (IRIT.EQ.1032) GOTO 90060
 1040 F='(/'' Dobr .'')'
      IF (IRIT.EQ.1040) WRITE(LU,f)
C
 1050 F='(/'' Nem…§ete j¡t ''4A2)'
      IF (IRIT.EQ.1050) WRITE(LU,f) (IWRD(L,2),L=1,4)
C
 1065 F='(/'' Programov  chyba na ý dku ''I6)'
      IF (IRIT.EQ.1065) WRITE(LU,f) IPR(3)
C
 1084 F='(/'' Amulet V s zachr nil od rozmaŸk n¡ stØnami'')'
      IF (IRIT.EQ.1084) WRITE(LU,f)
C
      IF (IRIT.EQ.1084) ITST(1)=4
 1086 F='(/'' Sv¡raj¡c¡ se stØny chodby V m zablokovaly vìchody''/'' a j
     :ste v pasti'')'
      IF (IRIT.EQ.1086) WRITE(LU,f)
C
      IF (IRIT.EQ.1086) IXT(314)=0
      IF (IRIT.EQ.1086) IXT(140)=0
 1088 F='(/'' Sv¡raj¡c¡ se stØny V s rozdrtily'')'
      IF (IRIT.EQ.1088) WRITE(LU,f)
C
      IF (IRIT.EQ.1088) GOTO 80000
 1096 F='(/'' Vlk na V s za£toŸil a zat¡m je zanepr zdnØn po§¡r n¡m''/''
     :Vaç¡ potravy.'')'
      IF (IRIT.EQ.1096) WRITE(LU,f)
C
      IF (IRIT.EQ.1096) IVEN(24)=0
      IF (IRIT.EQ.1096) IRES(15)=ITST(1)
 1097 F='(/'' Vlk na V s zbØsile za£toŸil a se§ral V s.'')'
      IF (IRIT.EQ.1097) WRITE(LU,f)
C
      IF (IRIT.EQ.1097) GOTO 80000
 1105 F='(/'' T¡mto smØrem nem…§ete j¡t.'')'
      IF (IRIT.EQ.1105) WRITE(LU,f)
C
 1145 FORMAT(/' Gratuluji! Vyýeçil jste z hadu. M te '
     14A2', co§ je'/' smrt¡c¡ zbraå, na m¡stØ Ÿinu je s V mi '4A2
     2'- vrah')
      IF (IRIT.EQ.1145) WRITE(LU,1145)(IWP(L),L=1,4),
     1(IRSN(L,ITST(9)),L=1,4)
      IF (IRIT.EQ.1145.AND.ITST(21).NE.LU)ITST(2)=ITST(2)+200
      IF (IRIT.EQ.1145) ITST(8)=0
C
 1150 F='(/'' Je zde tma'')'
      IF (IRIT.EQ.1150) WRITE(LU,f)
C
 1151 F='(/'' Slunce ji§ zapadlo'')'
      IF ((IRIT.EQ.1150) .AND. (IPR(3).EQ.1)) WRITE(LU,f)
C
 1152 F='(/'' Z clony br n¡ svØtlu'')'
      IF ((IRIT.EQ.1150) .AND. (IPR(3).EQ.2)) WRITE(LU,f)
C
      IF (IRIT.EQ.10030) GOTO 80000
C
13010 F='(/'' Jed rychle zap…sobil a zemýel jste v hroznìch bolestech.''
     :)'
      IF (IRIT.EQ.13010) WRITE(LU,f)
C
      IF (IRIT.EQ.13010) GOTO 80000
C
C                            MM-D-5  (LINE 3520)
C
C 6-D
C
      IF (ITST(21).EQ.LU.OR.ITST(21).EQ.0) GOTO 9999
      LU=ITST(21)
      GOTO 999
 9999 NAME(2)='SB'
c      CALL SGLOD(NAME,LU)
      p2=lu
      return
C
C *** prevteleni ***
C
C ** ma to zadarmo **
C
80000 IF (ITST(21).EQ.0.OR.ITST(21).EQ.LU) GOTO 80001
      LU=ITST(21)
      GOTO 999
80001 LU=IPR(1)
      IC=0
      IF (IVEN(1) .GT. 20000) IC=1
      IF (IVEN(18) .GT. 20000) IC=18
      IF (IC .NE. 0) IVEN(IC)=0
80005 IF ((ITST(2) .LT. 100) .AND. (IC.EQ.0)) GOTO 90000
80010 F='(/'' Chcete se pýevtØlit?'')'
      WRITE(LU,f)
C
80022 F='(/'' Bude V s to st t 100 bod….''/'' ?''\)'
      IF (IC.EQ.0) WRITE(LU,f)
C
80024 F='(/'' Nebude V s to st t ani bod, proto§e m te ''\)'
      IF (IC .NE. 0) WRITE(LU,f)
C
80025 F='(''prsten.''//'' ?''\)'
      IF (IC.EQ.1) WRITE(LU,f)
C
80026 F='(''talisman.''//'' ?''\)'
      IF (IC.EQ.18) WRITE(LU,f)
C
80030 F='(A2)'
*     IF (ITST(21).NE.LU)READ(LU,f) IANS
      read(lu,f)ians
      IF (IANS.EQ.'NE') GOTO 90020
      IF (IANS.EQ.'ne') GOTO 90020
      IF (IANS.EQ.'AN') GOTO 80050
      IF (IANS.EQ.'an') GOTO 80050
C
80040 F='(/'' Co? Napiçte ANO nebo NE'')'
      WRITE(LU,f)
      GOTO 80010
C
C *** lucernu zpet na branu ***
C
80050 IF (IVEN(4).GT.20000)IVEN(4)=IVEN(4)-20000+193
      IF (IVEN(4).EQ.493.AND.ITST(5).GT.295)IVEN(4)=1493
C
C *** ostatni veci nechat na miste ***
C
      DO 80060 J=1,52
      IF (IVEN(J) .GT. 20000) IVEN(J)=IVEN(J)-20000+ITST(1)
80060 CONTINUE
C
C *** rytire k zadni brane ***
C
      IF (IRES(16).NE.0)IRES(16)=IRES(16)-IFSD(IRES(16))+95
C
C *** hrace k hlavni brane ***
C
      ITST(1)=93
C
C *** ubrat 100 bodu, nema-li to zadarmo ***
C
      IF (IC.EQ.0.AND.LU.NE.ITST(21)) ITST(2)=ITST(2)-100
      IF (IC.EQ.0) ITST(3)=ITST(3)-100
80070 F='(/'' Dobr . Dostal jsem V s z kaçe, pod¡vejte se, kde jste'')'
      WRITE(LU,f)
      IF (ITST(21).EQ.LU.OR.ITST(21).EQ.0) GOTO 89999
      LU=ITST(21)
      GOTO 999
89999 NAME(2)='SB'
c      CALL SGLOD(NAME,LU)
      p2=lu
      return
C
C                            MM-D-6  (LINE 3600)
C 7-D
C
C *** odepsani ***
C
90000 F='(/'' Nem te dostatek bod… na pýevtØlen¡!''//)'
      WRITE(LU,f)
90020 F='(/'' Z¡skal jste ''I4'' bod…, jste hodnocen jako ''4A2/'' detek
     1tiv.'')'
      WRITE(LU,f) ITST(2),(ICLS(L,1+ITST(2)/85),L=1,4)
      CALL cas(10,ITIM)
      ITR=60*ITIM(4)+ITIM(3)-60*(ITST(19)/100)-IFSD(ITST(19))
      IF (ITR.LT.0)ITR=ITR+1440
      TG=ITST(5)/25.
      IEF=200*ITR/ITST(5)
      IF (IEF.GT.99)IEF=99
90032 F='(/'' Hr l jste ''I4'' minut re ln‚ho Ÿasu, co§ je ''F5.1'' hodi
     :n hry, ''/I3'' % zrychlen¡ casu.'')'
      WRITE(LU,f)ITR,TG,IEF
C
90035 F='(/'' Pý¡çtØ v¡ce çtØst¡.'')'
      IF (IRIT.NE.192.AND.ITST(2).LT.700) WRITE(LU,f)
C
90036 F='('' Douf m, §e si brzy znovu zahrajete.'')'
      IF (IRIT.EQ.192) WRITE(LU,f)
90060 F='(/'' Na shledanou '',10A2/)'
      WRITE(LU,f)NTAG
      stop
C
C  MA SE ZAPISOVAT
C
99990 IF (ITST(38) .NE. 'MM') GOTO 99999
C
C  VYTVORENI DAT
C
c     CALL EXEC(11,ITIM,IYEAR)
      IDATA(1,1)=ITIM(5)
      IDATA(2,1)=ITST(19)
      IDATA(3,1)=100*ITIM(4)+ITIM(3)
      IDATA(4,1)=ITST(5)
      IDATA(5,1)=ITST(2)
      IDATA(6,1)=ITST(3)
      IDATA(7,1)=ITST(1)
      IDATA(8,1)=ITST(28)
      IDATA(9,1)=NTAG(1)
      IDATA(10,1)=NTAG(2)
      IDATA(11,1)=NTAG(3)
      IDATA(12,1)=NTAG(4)
      IDATA(13,1)=NTAG(5)
      IDATA(14,1)=NTAG(6)
      IDATA(15,1)=NTAG(7)
      IDATA(16,1)=NTAG(8)
      IDATA(17,1)=NTAG(9)
      IDATA(18,1)=NTAG(10)
      IDATA(19,1)=IYEAR
c     CALL EXEC(100000B+24,6HSCORE ,2HMM,2HMM,2HMM,2HMM,2HMM,
c    :          IDATA,19,*99999)
C
C  NENI, NEVADI
C
99999 return
      end
