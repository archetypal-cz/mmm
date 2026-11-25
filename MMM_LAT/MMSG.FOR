$storage:2
$large
$nodebug
      subroutine mmsg(p1,p2,p3,p4,p5)
c      ,10-02-1983 MMM    8536E D45 PL&BM <850906.1733>
C     04-02-1988 MS-DOS S.M.
      integer*2 p1,p2,p3,p4,p5
      character*127 f
$INCLUDE:'MMCO.FOR'
C
C ******* SEGMENT G *******
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
C * nastaveni smeru*
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
C                            MM-G-1  (LINE 6000)
C
C
C 2-G
C
C
C  *** CLOSING COMMENTS ***
C
  999 continue
  320 F='(/'' Nen¡ mi zcela jasn‚, co se mi pokouç¡te sdØlit'')'
      IF (IRIT .EQ. 320) WRITE(LU,f)
  520 F='(/'' Nic se nestalo'')'
      IF (IRIT .EQ. 520) WRITE(LU,f)
C
 1000 F='(/'' Mo§n  nejsem pozornìm Ÿten ýem, ale mus¡te mi ý¡ct, co chc
     1ete ''4A2)'
      IF (IRIT.EQ.1000) WRITE(LU,f)(IWRD(L,1),L=1,4)
C
 1040 F='(/'' V poý dku'')'
      IF (IRIT .EQ. 1040) WRITE(LU,f)
C
 1065 F='(/'' Programov  chyba na ý dku ''I6)'
      IF (IRIT .EQ. 1065) WRITE(LU,f) IPR(3)
      IF (IPR(3).EQ.7.OR.IPR(3).EQ.16.OR.IPR(3).EQ.25)IC=IDS
      IF (IPR(3).EQ.5.OR.IPR(3).EQ.14.OR.IPR(3).EQ.23)IC=IDW
      IF (IPR(3).EQ.1.OR.IPR(3).EQ.10.OR.IPR(3).EQ.19)IC=IDN
 1095 F='(/'' Vaçe poŸ¡n n¡ otevýelo tajn‚ dveýe do chodby, kterìmi m…§e
     1te''/'' vej¡t ''4A2)'
      IF (IRIT .EQ. 1095) WRITE(LU,f)(IDTN(I,IC),I=1,4)
C
 1096 F='(/'' Rakev je pr zdn  a v¡ko se zav¡r .'')'
      IF (IRIT.EQ.1095.AND.ITST(1).EQ.5.AND.ITST(5).GT.50)
     1  WRITE(LU,f)
      IF (IRIT.EQ.1095.AND.ITST(1).EQ.5.AND.IRES(5).EQ.300)IRES(5)=5
 1097 F='(/'' Vamp¡r je v rakvi a probouz¡ se.'')'
      IF (IRIT.EQ.1095.AND.ITST(1).EQ.5.AND.ITST(5).LE.50
     1.AND.IRES(5).EQ.5) WRITE(LU,f)
C
 1150 F='(/'' Je zde tma'')'
      IF (IRIT .EQ. 1150) WRITE(LU,f)
C
 1151 F='(/'' Je po z padu slunce'')'
      IF ((IRIT .EQ. 1150) .AND. (IPR(3) .EQ. 1)) WRITE(LU,f)
C
 1152 F='(/'' Zata§en‚ z clony blokuj¡ svØtlo'')'
      IF ((IRIT .EQ. 1150) .AND. (IPR(3) .EQ. 2)) WRITE(LU,f)
C
 2010 F='(/'' Nem…§ete ''2A2'' ''4A2)'
      IF (IRIT .EQ. 2010) WRITE (LU,f)(IWRD(L,1),L=1,2),
     1(IWRD(L,2),L=1,4)
C
 2011 F='(/'' Nem…§ete ''2A2'' ''4A2)'
      IF (IRIT.EQ.2011) WRITE(LU,f)(IWRD(L,1),L=1,2),
     1(IWRD(L,2),L=1,4)
C
 2020 F='(/1X4A2'' ji§ m te'')'
      IF (IRIT .EQ. 2020) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 2025 F='(/'' Toto ''4A2'' nem .'')'
      IF (IRIT .EQ. 2025) WRITE(LU,f)(IRSN(L,IWRD(6,4)),L=1,4)
C
 2030 F='(/'' Nikde nevid¡m ''4A2)'
      IF (IRIT .EQ. 2030) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 2035 F='(/'' Dýevo je naskl d no do velk‚ vìçky. Kl¡n nem…§ete vyt hnou
     1t.'')'
      IF (IRIT.EQ.2035) WRITE(LU,f)
C
 2037 F='(/'' Jemnì svitek papyru se rozpadl, pot‚ co jste se jej dotkl.
     1'')'
      IF (IRIT.EQ.2037) WRITE(LU,f)
C
C                            MM-G-2  (LINE 6080)
C
C
C 3-G
C
 2038 F='(/'' VØdØl jsem, §e se o to pokus¡te. Pr vØ jste rozfoukal kous
     1ky''/'' papyru po cel‚ knihovnØ.'')'
      IF (IRIT.EQ.2038) WRITE(LU,f)
      IF (IRIT.EQ.2037.OR.IRIT.EQ.2038)IVEN(88)=1017
C
 2040 F='(/'' Nem…§ete un‚st ''4A2)'
      IF (IRIT .EQ. 2040) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 2041 F='(/'' Soçka zlat‚ho bìka a zlat‚ tyŸky byly vratce poskl d ny a
     1spadly.'')'
      IF (IRIT.EQ.2041) WRITE(LU,f)
C
 2042 F='(/'' Dìka hlasitØ a protivnØ zaský¡pØla, kdy§ jste ji tahal z d
     1esky.'')'
      IF (IRIT.EQ.2042) WRITE(LU,f)
C
 2043 F='('' Hluk probudil trpasl¡ka a on V s utloukl k smrti.'')'
      IF (IRIT.EQ.2041.OR.IRIT.EQ.2042) WRITE(LU,f)
C
      IF (IRIT.EQ.2041.OR.IRIT.EQ.2042)NAME(2)='SD'
      IF (NAME(2).EQ.'SD') then
c      CALL SGLOD(NAME,LU,10030)
          p2=10030
          return
      endif
 2060 F='(/'' Nejdý¡ve mus¡te nØco odlo§it'')'
      IF (IRIT .EQ. 2060) WRITE(LU,f)
C
 2070 F='(/'' Vaçe vìbava obsahuje ''4A2)'
      IF (IRIT .EQ. 2070) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 2110 F='(/'' DoporuŸuji V m, abyste se osprchoval v koupelnØ.'')'
      IF (IRIT.EQ.2110) WRITE(LU,f)
C
 2120 F='(/'' Nejdý¡v byste si mØl odlo§it.'')'
      IF (IRIT.EQ.2120) WRITE(LU,f)
C
 2140 F='(/'' Zvlhly by V m çaty.'')'
      IF (IRIT.EQ.2140) WRITE(LU,f)
C
 2150 F='(/'' Jste obŸerstven a tak‚ tolik nezap ch te.'')'
      IF (IRIT.EQ.2150) WRITE(LU,f)
C
 3010 F='(/'' '',4A2'' nem te.'')'
      IF (IRIT .EQ. 3010) WRITE(LU,f) (IWRD(L,iwrd(7,1)),L=1,4)
C
C nevolame
 3012 F='(/'' Tohle nem te.'')'
      IF (IRIT.EQ.3012) WRITE(LU,f)
C
 3015 F='(/'' Lesn¡ nymfa vybØhla z lesa, ukradla V m ''4A2'' a zase ''/
     1'' zmizela v lese.'')'
      IF (IRIT.EQ.3015) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 3030 F='(/'' Slyçel jste bouchnut¡, jak to dopadlo na dno studny.'')'
      IF (IRIT .EQ. 3030) WRITE(LU,f)
C
 3040 F='(/'' Slyçel jste çplouchnut¡, jak to dopadlo do vody ve studni.
     1'')'
      IF (IRIT .EQ. 3040) WRITE(LU,f)
C
      IF (IRIT.EQ.3045) WRITE(LU,3045)
 3045 FORMAT(/' Kýiçœ lov  koule se roztý¡çtila na tis¡c kousk…, ',
     1'pr vØ se promØåuj¡c¡ch'/' v obl Ÿky dìmu, ve kterìch se V m ',
     2'objevuj¡ smØj¡c¡ se obyvatel‚'/' z hadn‚ho s¡dla.')
C
C                            MM-G-3  (LINE 6160)
C
C
C 4-G
C
 3110 F='(/'' Odlo§il jste ''4A2)'
      IF (IRIT .EQ. 3110) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 3210 F='(/'' To bych nedØlal. Nymfa si to vçechno vezme.'')'
      IF (IRIT.EQ.3210) WRITE(LU,f)
C
 4010 F='(/1X4A2'' zde nen¡. Mus¡te ho nejdý¡v naj¡t.'')'
      IF (IRIT .EQ. 4010) WRITE(LU,f)(IRSN(L,IWRD(6,3)),L=1,4)
C
 4030 F='(/'' Nem…§ete to d vat ''4A2''. Neunese to.'')'
      IF (IRIT .EQ. 4030) WRITE(LU,f)(IWRD(L,3),L=1,4)
C
      IC1=' '
      IF (IWRD(6,3).EQ. 2 .OR. IWRD(6,3).EQ.8) IC1='a'
 4040 F='(/1X4A2'' si to vzal''A1''.'')'
      IF ((IRIT .EQ. 3110) .AND. (IPR(3).EQ. 1)) WRITE(LU,f)
     1(IRSN(L,IWRD(6,3)),L=1,4),IC1
C
 4050 F='(/'' D¡ky. Je to tady potýeba.'')'
      IF (IRIT.EQ.4050) WRITE(LU,f)
C
 5010 F='(/'' M…§ete se d¡vat pouze na pýedmØty a obyvatele'')'
      IF (IRIT .EQ. 5010) WRITE(LU,f)
C
 5020 F='(/1X4A2'' zde nen¡.'')'
      IF (IRIT .EQ. 5020) WRITE(LU,f)(IRSN(L,IWRD(6,3)),L=1,4)
C
      IF (IRIT.NE.5070) GOTO 5100
 5030 F='(''  a ne§ije. '')'
      IF (IRIT.EQ.5070.AND.IRES(IW63).GT.10000.AND.
     : IW63.NE.1) WRITE(LU,f)
C
      I1=' '
      IF (IW63.EQ.1.OR.IW63.EQ.2.OR.IW63.EQ.8) I1='a'
 5032 F='(/''  je nØkolikr t zranØn''A1'' a asi brzy umýe.'')'
      IF (IRES(IW63)/1000.EQ.9) WRITE(LU,f) I1
C
 5034 F='(/''  je tØ§ce zranØn''A1'' a asi zemýe.'')'
      IF (IRES(IW63)/1000.EQ.8) WRITE(LU,f) I1
C
 5036 F='(/''  je zranØn''A1'' a utekl''A1''.'')'
      IF (IRES(IW63)/1000.EQ.7) WRITE(LU,f) I1,I1
C
 5038 F='(/'' a upadl''A1'' do bezvØdom¡.'')'
      IF (IRES(IW63)/1000.EQ.6) WRITE(LU,f) I1
C
 5040 F='(/'' byl''A1'' zasa§en''A1'' do nohy.'')'
      IF (IRES(IW63)/1000.EQ.5) WRITE(LU,f) I1,I1
C
 5042 F='(/'' a je zranØn''A1''.'')'
      IF (IRES(IW63)/1000.EQ.4) WRITE(LU,f)
C
 5044 F='(/'' a je zranØn''A1'', ale m…§e se uzdravit.'')'
      IF (IRES(IW63)/1000.EQ.3) WRITE(LU,f)
C
 5046 F='(/'' je zranØn''A1'', ale brzy bude v poý dku.'')'
      IF (IRES(IW63)/1000.EQ.2) WRITE(LU,f)
C
 5048 F='(/'' je zranØn''A1'' a pohybuje se pomalu.'')'
      IF (IRES(IW63)/1000.EQ.1) WRITE(LU,f)
C
C   DIVAT SE NA OBYVATELE
C
      IF (IRIT .NE. 5070) GOTO 5100
      IF (IRES(IW63) .GT. 1000 .AND. IW63 .NE. 1) GOTO 5100
C
C 5-G  LADY
C
 5072 FORMAT(/' Je to starç¡ çt¡hl  §ena obleŸena do dlouhìch, Ÿernìch ç
     1at….')
      IF (IW63.EQ.2) WRITE(LU,5072)
C
C  BUTLER
C
 5073 FORMAT(/' Je to hezkì mladì mu§ elegantnØ odØnì.')
      IF (IW63.EQ.3) WRITE(LU,5073)
C
C  MOLE
C
 5074 F='(/'' Je to obrovsk  hnusn  vØc, pomal  a slep .'')'
      IF (IW63.EQ.4) WRITE(LU,f)
C
C  VAMPIRE
C
 5075 F='(/'' Je bledì, m  dlouhou Ÿernou k pi a tØ§ce dìch  pýes''/'' d
     1louh‚ çpiŸ ky.'')'
      IF (IW63.EQ.5) WRITE(LU,f)
C
C  WOODSMAN
C
 5076 F='(/'' Je to pý telsky vyhl¡§ej¡c¡ drsnì chlap¡k.'')'
      IF (IW63.EQ.6) WRITE(LU,f)
C
C  GARDENER
C
 5077 F='(/'' Je to malì mu§ se çpinavìmi çaty i rukama.'')'
      IF (IW63.EQ.7) WRITE(LU,f)
C
C  MAID
C
 5078 FORMAT(/' Je to mlad  sexy d¡vka s dlouhìmi svØtlìmi vlasy.')
      IF (IW63.EQ.8) WRITE(LU,5078)
C
C  COOK
C
 5079 F='(/'' Je obleŸen v b¡l‚m a m  na pl çti krvavou skvrnu.'')'
      IF (IW63.EQ.9) WRITE(LU,f)
C
C  WEREWOLF
C
 5080 FORMAT(/' Je to chlupatì netvor. Jeho oŸi se d¡vaj¡ pý¡mo na V s.'
     1)
      IF (IW63.EQ.10) WRITE(LU,5080)
C
C  ELF - skritek
C
 5081 F='(/'' Je to malì chlap¡k se smØçnìmi çaty a potmØçilìma oŸima.''
     1)'
      IF (IW63.EQ.11) WRITE(LU,f)
C
C  HUNTER
C
 5082 F='(/'' Je to zdravØ vypadaj¡c¡ chlap¡k s plnovousem.'')'
      IF (IW63.EQ.12) WRITE(LU,f)
C
C  DWARF - skret
C
 5083 F='(/'' Je to mrzce vypadaj¡c¡ mråous se s laj¡c¡ma oŸima.'')'
      IF (IW63.EQ.13) WRITE(LU,f)
C
C  MASTER
C
 5084 F='(/'' Je to starì gentleman s çedivìmi vlasy a tlustìmi brìlemi.
     1'')'
      IF (IW63.EQ.14) WRITE(LU,f)
C
C  WOLF
C
 5085 F='(/'' Je to velk‚, hladovØ vyhl¡§ej¡c¡ zv¡ýe.'')'
      IF (IW63.EQ.15) WRITE(LU,f)
C
C  WARRIOR
C
 5086 F='(/'' Je to velkì svalnatì chlap s nen vist¡ v oŸ¡ch.'')'
      IF (IW63.EQ.16) WRITE(LU,f)
C
C  CORPSE
C
 5090 F='(/'' Ze vzhledu mrtvoly lze usoudit, §e obØœ nen¡ moc dlouho mr
     1tv .'')'
      IF (IW63.EQ.1) WRITE(LU,f)
C
 5091 F='(/'' Pa§e obØti jsou rozta§eny jako kdyby sem byla dovleŸena.''
     :/'' Jej¡ koçile je roztr§en  a zkrvaven .'')'
      IF (IW63.EQ.1.AND.ITST(10).LT.13) WRITE(LU,f)
C
 5092 F='(/'' Je cel  zkroucen  a ruce spoŸ¡vaj¡ na §aludku.'')'
      IF (IW63.EQ.1.AND.ITST(10).EQ.13) WRITE(LU,f)
C
 5093 F='(/'' Je cel  ztluŸen  a m  prora§enou hlavu.'')'
      IF (IW63.EQ.1.AND.ITST(10).EQ.14) WRITE(LU,f)
C
 5095 F='(/'' Vçiml jste si pohledu pln‚ho obav.'')'
      IF (IW63.EQ.ITST(9)) WRITE(LU,f)
C
C
C
 5100 CONTINUE
C
 5110 F='(/'' '',4A2'' nem te.'')'
      IF (IRIT.EQ.5110) WRITE(LU,f) (IWRD(L,iwrd(7,1)),L=1,4)
C
      IF (IRIT.EQ.5115) WRITE(LU,5115)
 5115 FORMAT(/' Je to tØ§k  §elezn  br na ve vysok‚ cihlov‚ zdi',
     >' obklopuj¡c¡'/' s¡dlo. N pis ý¡k :'//15X'Vstup na vlastn¡',
     >' nebezpeŸ¡'//' Je pokryt  r…znìmi druhy bodc… a ostn….')
C
 5120 F='(/'' Br na je silnØ zrezavØl  a vis¡ nap…l otevýen .'')'
      IF (IRIT.EQ.5120) WRITE(LU,f)
C
 5125 F='(/'' Je to "l¡tac¡" br na urŸen  ke snadn‚mu vstupu.'')'
      IF (IRIT.EQ.5125) WRITE(LU,f)
C
      IF (IRIT .EQ. 5130) WRITE(LU,5130)
 5130 FORMAT(/
     1' Pýi podrobn‚ prohl¡dce jste objevil, §e koule je ve'/
     2' skuteŸnosti z pr çenou kýiçœalovu kouli s  magickou'/
     3' silou, ji§ m…§ete vyu§¡t pýi hled n¡ obyvatel s¡dla')
C
 5140 F='(/'' Lano je asi 50 stop dlouh‚ a m  h k na konci.'')'
      IF (IRIT.EQ.5140) WRITE(LU,f)
C
 5170 F='(/'' Na mapØ jsou pouze vzd len‚ lesy a cesty.'')'
      IF (IRIT .EQ. 5170) WRITE(LU,f)
C
 5210 F='(/'' Sv¡Ÿka je asi ''I2'' cm dlouh '')'
      IF (IRIT .EQ. 5210) WRITE(LU,f) IW63
C
 5310 F='(/'' Pochodeå je zap len '')'
      IF (IRIT .EQ. 5310.AND.IVEN(27).GT.21000) WRITE(LU,f)
C
 5311 F='(/'' Pochodeå cel  vyhoýela'')'
      IF (IRIT.EQ.5310.AND.IVEN(27).LT.21000) WRITE(LU,f)
C
 5410 F='(/'' V pistoli zbylo ''I2'' naboj''A1)'
      l='e'
      IF (itst(11) .eq. 1) l=' '
      IF (itst(11) .gt. 4) l='…'
      IF (IRIT .EQ. 5410) WRITE(LU,f) ITST(11),l
C
 5510 F='(/'' Je pr vØ ''I3'':''I2)'
      IF (IRIT .EQ. 5510) WRITE(LU,f)IPR(3),IPR(4)
C
 5610 F='(/'' Kniha obsahuje neŸitelnì text, z nØho§ jsem pýeŸetl ''/''
     :jen slovo ''4A2)'
      IF (IRIT .EQ. 5610) WRITE(LU,f)(IVRB(L,ITST(16)),L=1,4)
C
 5710 F='(/'' Jste natoŸen ''4A2)'
      IF (IRIT .EQ. 5710) WRITE(LU,f)(IDTN(L,IPR(3)),L=1,4)
C
 5720 F='(/'' Ampule je pr zdn , ale je z n¡ c¡tit jed.'')'
      IF (IRIT .EQ. 5720) WRITE(LU,f)
C
 5730 F='(/'' Ampule je pln  jedu'')'
      IF (IRIT .EQ. 5730) WRITE(LU,f)
C
 5740 F='(/'' Je zde krev'')'
      IF (IRIT .EQ. 5740) WRITE(LU,f)
C
      IF (IRIT.EQ.5750.AND.LU.NE.ITST(21)) IVEN(22)=IVEN(22)+1000
 5750 F='(/'' L¡stek ý¡k :'')'
      IF (IRIT .EQ. 5750) WRITE(LU,f)
C
 5751 F='('' M…§u V m pomoci otevý¡t br nu'')'
      IF ((IRIT .EQ. 5750) .AND. (ITST(5) .LE. 50)) WRITE(LU,f)
C
 5752 F='('' M…§ete j¡t libovolìm smØrem, kdy§ nap¡çete pouze prvn¡ p¡sm
     1eno'')'
      IF ((IRIT .EQ. 5750) .AND. (ITST(5) .GT. 50) .AND. (ITST(5)
     1.LE. 100)) WRITE(LU,f)
C
 5753 F='('' Jestli§e najdete vra§ednou zbraå a podaý¡ se V m dostat''/'
     1' vraha zpØt na m¡sto Ÿinu s V mi, z¡sk te hodnØ bod….'')'
      IF (IRIT.EQ.5750.AND.ITST(5).GT.100.AND.ITST(5).LE.150.AND.
     1ITST(8).NE.0) WRITE(LU,f)
C
 5754 F='(/'' Gratuluji V m, vyýeçil jste vra§du velmi rychle.'')'
      IF (IRIT.EQ.5750.AND.ITST(5).GT.100.AND.ITST(5).LE.150.AND.
     1ITST(8).EQ.0) WRITE(LU,f)
C
 5755 F='('' D vejte pozor na vamp¡ra. Jestli§e se V m ho podaý¡ zlikvid
     1ovat,''/'' z¡sk te body'')'
      IF (IRIT.EQ.5750.AND.ITST(5).GT.150.AND.ITST(5).LE.200.AND.
     1IRES(5).NE.0) WRITE(LU,f)
C
      IF (IRIT.EQ.5750.AND.ITST(5).GT.150.AND.ITST(5).LE.200.AND.
     1IRES(5).EQ.0) WRITE(LU,5756)(IRSN(L,ITST(39)),L=1,4)
 5756 FORMAT(/
     1' Minulì mØs¡c zapomØl jeden hr Ÿ z clony rozta§en‚ a najednou'/
     1' s hr…zou spatýil, jak se pýi p…lnoŸn¡m mØs¡Ÿn¡m svØtle '4A2/
     2' pýemØåuje na vlkodlaka.')
C
 5757 F='('' Mus¡te j¡t dol… do temn‚ chodby a tam naj¡t baterii do luce
     1rny.'')'
      IF ((IRIT .EQ. 5750) .AND. (ITST(5) .GT. 200) .AND. (ITST(5)
     1.LE. 250)) WRITE(LU,f)
C
 5758 F='('' Rada: mØs¡c je dnes v £plåku a nØkde zde je vlkodlak.''/''
     1Zabijte ho a z¡sk te body.'')'
      IF ((IRIT .EQ. 5750) .AND. (ITST(5) .GT. 250) .AND. (ITST(5)
     1.LE. 300)) WRITE(LU,f)
C
      IC=NORM(3)-1
      IC1=MMPRP(ITST(8))
 5759 F='('' V pý¡padØ, §e jste to neuhodl, zloŸin byl sp ch n ''/1X,A2,
     11X,8A2)'
      IF (IRIT.EQ.5750.AND.IC.EQ.0.AND.ITST(5).GT.300.AND.ITST(5).LE.
     1350.AND.ITST(8).NE.0) WRITE(LU,f) IPRP(1,IC1),
     :  (IRNM(L,IFSD(IROM(ITST(8)))),L=1,8)
C
 5760 F='('' V pý¡padØ, §e jste to neuhodl, vra§ednou zbran¡ je '',4A2)'
      IF (IRIT.EQ.5750.AND.IC.EQ.1.AND.ITST(5).GT.300.AND.ITST(5)
     1.LE.350.AND.ITST(8).NE.0) WRITE(LU,f)(IWP(L),L=1,4)
C
 5761 F='('' V pý¡padØ, §e jste to neuhodl, vrah je ''4A2)'
      IF (IRIT.EQ.5750.AND.IC.EQ.2.AND.ITST(5).GT.300.AND.ITST(5).
     1LE.350.AND.ITST(8).NE.0) WRITE(LU,f)(IRSN(L,ITST(9)),L=1,4)
C
 5762 F='(/'' Po uzavýen¡ zahrad magick  slova nefunguj¡.'')'
      IF (IRIT.EQ.5750.AND.ITST(5).GT.300.AND.ITST(5).LE.350.AND.
     1ITST(8).EQ.0) WRITE(LU,f)
C
 5763 F='('' M…§ete z¡skat body, kdy§ nØco uslyç¡te'')'
      IF ((IRIT .EQ. 5750) .AND. (ITST(5) .GT. 350) .AND. (ITST(5)
     1.LE. 400)) WRITE(LU,f)
C
C                            MM-G-7  (LINE 6480)
C
C
C 8-G
C
 5764 F='('' NØkdo ulo§il do chodeb pod s¡dlo spoustu dynamitu.'')'
      IF ((IRIT .EQ. 5750) .AND.(ITST(5) .GT. 400) .AND.(ITST(5)
     1.LE. 450)) WRITE(LU,f)
C
 5765 F='('' Kdybych byl V mi, tak bych odtud rychle zmizel.'')'
      IF ((IRIT .EQ. 5750) .AND. (ITST(5) .GT. 450)) WRITE(LU,f)
C
 5766 F='('' Podpis: pý¡tel'')'
      IF (IRIT .EQ. 5750) WRITE(LU,f)
C
 5775 F='(/'' L¡stek ý¡k  tot‚§ co pýedt¡m.'')'
      IF (IRIT .EQ. 5775) WRITE(LU,f)
C
 5780 F='(/'' Kulka je star  a matn .'')'
      IF (IRIT .EQ. 5780) WRITE(LU,f)
C
 5781 F='(/'' Talisman V m pom…§e pýi reinkarnaci v pý¡padØ £mrt¡.'')'
      IF (IRIT .EQ. 5781) WRITE(LU,f)
C
 5782 F='(/'' Truhla je velmi star  a je na n¡ tajemnì n pis'')'
      IF (IRIT.EQ.5782) WRITE(LU,f)
C
 5783 F='(/'' Visac¡ z mek uzamyk  smotan‚ výeteno spleten‚ho dr tu.'')'
      IF ((IRIT.EQ.5782.OR.IRIT.EQ.5783).AND.IFD(IVEN(35)).EQ.0)
     1 WRITE(LU,f)
C
 5784 F='(/'' Tato truhla je kl¡Ÿem k ýeçen¡ z hady, Ÿte vedle V s slu§k
     1a.'')'
      IF (IRIT.EQ.5782.AND.IFSD(IRES(8)).EQ.ITST(1).AND.
     1IRES(8).LT.10000) WRITE(LU,f)
C
 5785 F='(/'' Neotv¡rejte tuto truhlu v odemŸen‚ m¡stnosti, Ÿte za V mi
     1p n.'')'
      IF (IRIT.EQ.5782.AND.IFSD(IRES(14)).EQ.ITST(1).AND.
     1IRES(14).LT.10000) WRITE(LU,f)
C
 5786 F='(/'' Tuto truhlu zavýete co nejrychleji, Ÿte pýed V mi pan¡.'')
     1'
      IF (IRIT.EQ.5782.AND.IFSD(IRES(2)).EQ.ITST(1).AND.
     1IRES(2).LT.10000) WRITE(LU,f)
C
 5810 F='(/'' M…§ete vidØt sv…j obraz a vØci v pokoji za V mi.'')'
      IF (IRIT.EQ.5810) WRITE(LU,f)
C
 5820 F='(/'' V zrcadle je tak‚ vedle Vas vidØt ''4A2)'
      IF (IRIT.EQ.5810.AND.IPR(3).GT.0) WRITE(LU,f)(IRSN(L,IPR(3)),L=
     11,4)
C
      IF (IRIT.EQ.5810.AND.IPR(3).EQ.2) WRITE(LU,5072)
      IF (IRIT.EQ.5810.AND.IPR(3).EQ.3) WRITE(LU,5073)
      IF (IRIT.EQ.5810.AND.IPR(3).EQ.8) WRITE(LU,5078)
      IF (IRIT.EQ.5810.AND.IPR(3).EQ.10) WRITE(LU,5080)
 5830 F='(/'' Amulet je velkì, podivnì, brouçenì drahokam s osmi plocham
     1i,''/'' z nich§ Ÿtyýi jsou çesti£heln¡kov‚.'')'
      IF (IRIT.EQ.5830) WRITE(LU,f)
C
C                            MM-G-8  (LINE 6560)
C 9-G
 5860 F='(/'' Z soba dýeva se skl d  z nØkolika velkìch kl d zakl¡nØnìch
     1''/'' malìm kl¡nem o stØnu u krbu.'')'
      IF (IRIT.EQ.5860) WRITE(LU,f)
C
 5861 F='('' Kl dy nahoýe vypadaj¡, jako kdyby byly pr vØ pýilo§eny.'')'
      IF (IRIT.EQ.5860.AND.ITST(5).GT.50) WRITE(LU,f)
C
 5870 F='(/'' Kl¡n je asi stopu dlouhì zahrocenì kus dýeva a pou§¡v  se
     1k zajiçtØn¡''/'' kl d, aby se nekut lely.'')'
      IF (IRIT.EQ.5870) WRITE(LU,f)
C
 5880 F='(/'' Na tabulce je naps no:''/'' Nez…st vejte zde dlouho.''/''
     1Prkna jsou uvolnØn  a mohl byste se propadnout dolu.'')'
      IF (IRIT.EQ.5880) WRITE(LU,f)
C
 5885 F='(/'' Svitek se sroloval a nem…§ete pýeŸ¡st n pis.'')'
      IF (IRIT.EQ.5885) WRITE(LU,f)
C
 5886 F='(/'' D¡v te se na kresbu jak‚hosi p kov‚ho mechanismu um¡stØn‚h
     1o v''/'' troj£heln¡kov‚ dutinØ ve zdi.'')'
      IF (IRIT.EQ.5886) WRITE(LU,f)
C
 5888 F='(/'' Trpasl¡k je obrovsk  çeredn  vØc neklidnØ sp¡c¡ na hromadØ
     1 kost¡.'')'
      IF (IRIT.EQ.5888.OR.IRIT.EQ.5890) WRITE(LU,f)
C
 5889 F='('' V jedn‚ ruce m  velkì klacek a v druh‚ perlovì n hrdeln¡k.'
     1')'
      IF (IRIT.EQ.5888.AND.IVEN(36).EQ.1151) WRITE(LU,f)
C
 5890 F='('' V jedn‚ ruce m  palc t a v druh‚ stý¡brnou kulku.'')'
      IF (IRIT.EQ.5890.AND.IVEN(39).EQ.1253) WRITE(LU,f)
C
 5891 F='(/'' Je to pestrì pt k s krou§kem na noze, kterì ý¡k : "Um¡m hl
     1edat vØci."'')'
      IF (IRIT.EQ.5891) WRITE(LU,f)
C
 5892 F='(/'' Dlouh‚ rukavice jsou mØkk‚ kovov‚ rukavice, kter‚ ''/'' V 
     1m zýejmØ dod vaj¡ zvl çtn¡ s¡lu.'')'
      IF (IRIT.EQ.5892) WRITE(LU,f)
C
      IF (IRIT.EQ.5893) WRITE(LU,5893)
 5893 FORMAT(/
     1' Stoly jsou v 8 ýad ch a do nØkterìch jsou vyýez ny figurky.'/
     2' Dva nejbli§ç¡ jsou vyrobeny z drah‚ho dýeva a jsou do nich'/
     3' vyýez ny figurky kr le a kr lovny.')
C
 5910 F='(/'' Nev¡m, co chcete vidØt.'')'
      IF (IRIT .EQ. 5910) WRITE(LU,f)

C
 6005 F='(/1X4A2'' nemus¡te hledat, stoj¡ pýece pýed V mi.'')'
      IF (IRIT.EQ.6005) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 6010 F='(/'' Potýeboval byste kýiçœ lovou kouli, abyste naçel nØkoho,''
     1/'' kdo nen¡ s V mi.'')'
      IF (IRIT .EQ. 6010) WRITE (LU,f)
C
      IC=MMPRP(IFSD(IRES(IWRD(6,2))))
 6020 F='(/'' Pohledem do kýiçœ lov‚ koule jste  zjistil, §e '',4A2/'' j
     1e ''A2'' ''8A2)'
      IF (IRIT .EQ. 6020) WRITE (LU,f) (IRSN(L,IWRD(6,2)),L=1,4),
     1IPRP(1,IC),(IRNM(L,IPR(3)),L=1,8)
C
 6030 F='(/'' Dosud nem te prostýedky pro hled n¡ vØc¡, kter‚ nejsou pob
     1l¡§.'')'
      IF (IRIT.EQ.6030) WRITE(LU,f)
C
 6040 F='(/'' Papouçek na nØkolik minut odletØl a vr til se se slovy:'')
     1'
      IF (IRIT.EQ.6040.OR.IRIT.EQ.6035) WRITE(LU,f)
C
      IF (IRIT.EQ.6040) IC1=MMPRP(IFSD(IVEN(IWRD(6,2))))
 6042 F='('' BAAAAAAKKK: ''A2'' ''8A2)'
      IF (IRIT.EQ.6040.AND.IVEN(IWRD(6,2)).LT.10000) WRITE(LU,f)
     :  IPRP(1,IC1),(IRNM(L,IFSD(IROM(IFSD(IVEN(IWRD(6,2)))))),L=1,8)
C
 6044 F='('' BAAAAAAKKK: je mi l¡to!'')'
      IF (IRIT.EQ.6035) WRITE(LU,f)
C
 6045 F='('' BAAAAAAKKK: '',4A2)'
      IF (IRIT.EQ.6040.AND.IVEN(IWRD(6,2)).GT.10000)
     1 WRITE(LU,f)(IRSN(L,IFSD(IVEN(IWRD(6,2)))),L=1,4)
C
 7050 F='('' nic'')'
      IF (IRIT .EQ. 7050) WRITE(LU,f)
C
 8010 F='(/'' Nev¡m, co jste oŸek val, §e se stane'')'
      IF (IRIT .EQ. 8010 ) WRITE(LU,f)
C
C ??
C
 8015 F='(/1X,4A2'' snØdl Vaçe j¡dlo.'')'
      IF (IRIT .EQ. 8015) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 8020 F='(/'' ji§ to je'')'
      IF (IRIT .EQ. 8020) WRITE(LU,f)
C
 8030 F='(/1X,4A2'' nevid¡m. Vy snad ano?'')'
      IF (IRIT.EQ.8030) WRITE(LU,f)(IWRD(L,3),L=1,4)
C
 8031 F='(/1X,4A2'' nevid¡m. Vy snad ano?'')'
      IF (IRIT.EQ.8031) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 8035 F='(/'' Doçla V m baterie v lucernØ.'')'
      IF (IRIT .EQ. 8035) WRITE(LU,f)
C
      IC1=' '
      IF (ITST(9) .EQ. 2 .OR. ITST(9) .EQ. 8) IC1='a'
 8040 F='(/'' Pot‚, co se radio zahý lo, jste slyçel zpr vu, §e '',4A2/'
     :' z tajemn‚ho s¡dla je hled n''A1'' polici¡ pro vra§du.'')'
      IF (IRIT .EQ. 8040) WRITE(LU,f)(IRSN(L,ITST(9)),L=1,4),IC1
C
 8045 F='(/'' Po zahý t¡ radio zaŸalo hr t n ladovou hudbu.'')'
      IF (IRIT.EQ.8045) WRITE(LU,f)
C
C                            MM-G-10  (LINE 6720)
C
C
C 11-G
C
 8050 F='(/'' Potýebujete sirku nebo nØco podobn‚ho.'')'
      IF (IRIT .EQ. 8050) WRITE(LU,f)
C
 8060 F='(/'' Pochodeå dohoýela.'')'
      IF (IRIT .EQ. 8060) WRITE(LU,f)
C
 9027 F='(/'' Nem te dostatek bod….'')'
      IF (IRIT.EQ.9027) WRITE(LU,f)
C
 9040 F='(/'' U§ to v¡ckr t nezkouçejte.'')'
      IF (IRIT.EQ.9040) WRITE(LU,f)
C
 9041 F='(/'' SkonŸete hru, dokud jeçtØ §ijete!'')'
      IF (IRIT.EQ.9041) WRITE(LU,f)
C
 9042 F='(/'' StejnØ se to nikdy nedozv¡te vçechno'')'
      IF (IRIT.EQ.9042) WRITE(LU,f)
C
 9043 F='(/'' ¬tØte pozornØ, co ý¡k m.'')'
      IF (IRIT.EQ.9043) WRITE(LU,f)
C
 9044 F='(/'' Dobrì zp…sob, jak zaŸ¡t, je "J’T SEVERN·" nebo "J’T VíCHOD
     1N·"'')'
      IF (IRIT.EQ.9044) WRITE(LU,f)
C
 9045 F='(/'' Pohybujte se a dý¡ve Ÿi pozdØji z lesa vyjdete.'')'
      IF (IRIT.EQ.9045) WRITE(LU,f)
C
 9046 F='(/'' Zkuste magick‚ slovo'')'
      IF (IRIT.EQ.9046) WRITE(LU,f)
C
 9047 F='(/'' Zkuste si pýin‚st §idli nebo zavý¡t dveýe.'')'
      IF (IRIT.EQ.9047) WRITE(LU,f)
C
 9048 F='(/'' Dokud nezn te magick‚ slovo, nic pro V s nemohu udØlat.'')
     1'
      IF (IRIT.EQ.9048) WRITE(LU,f)
C
 9049 F='(/'' Nem…§ete nic dØlat, pokud nezn te magick‚ slovo nebo''/''
     1nem te lopatku.'')'
      IF (IRIT.EQ.9049) WRITE(LU,f)
C
 9050 F='(/'' Nemohu pro V s nic udØlat, pokud nezn te magick‚ slovo neb
     1o''/'' nenaolejujete z vØs §ebý¡ku.'')'
      IF (IRIT.EQ.9050) WRITE(LU,f)
C
 9051 F='(/'' Zkuste odkl dat vØci pro z¡sk n¡ orientace.'')'
      IF (IRIT.EQ.9051) WRITE(LU,f)
C
10020 F='(/'' JeçtØ jste neodhalil, jak to nedØlat.'')'
      IF (IRIT.EQ.10020) WRITE(LU,f)
C
10035 F='(/'' Dveýe jsou zamŸen‚.'')'
      IF (IRIT.EQ.10035) WRITE(LU,f)
C
10045 F='(/'' Dveýe jsou otevýen‚.'')'
      IF (IRIT.EQ.10045) WRITE(LU,f)
C
10046 F='(/'' Dveýe jsou zavren‚.'')'
      IF (IRIT.EQ.10046) WRITE(LU,f)
C
10010 F='(/'' Z vØsy zde nejsou.'')'
      IF (IRIT .EQ. 10010) WRITE(LU,f)
C
10085 F='(/'' Nachytal jste vamp¡ra na denn¡m svØtle a on se rozplynul.'
     1')'
      IF (IRIT.EQ.10085) WRITE(LU,f)
C
C                            MM-G-11  (LINE 6800)
C
C
C 12-G
C
10105 F='(/'' Zahradn¡ br na je nyn¡ trvale zavýen .'')'
      IF (IRIT.EQ.10105) WRITE(LU,f)
C
10110 F='(/'' Br na z…stala otevýen .'')'
      IF (IRIT .EQ. 10110) WRITE(LU,f)
C
10115 F='(/1X,4A2'' V m pomohl silnym çœouchnut¡m pohnout s branou.'')'
      IF (IRIT.EQ.10115) WRITE(LU,f)(IRSN(L,IPR(3)),L=1,4)
C
10116 F='(/'' Shrnovac¡ rukavice V m umo§åuje snadno pohybovat branou.''
     1)'
      IF (IRIT.EQ.10116) WRITE(LU,f)
C
10130 F='(/'' Br na je pro V s samotn‚ho pý¡liç tØ§k .'')'
      IF (IRIT .EQ. 10130) WRITE(LU,f)
C
      IF (IRIT.EQ.10210) WRITE(LU,10210)
10210 FORMAT(/
     1' Houfy výeçt¡c¡ch d‚mon… vyletØly z truhly, kde byly po'/
     2' staleti vØznØny, a rozl‚tly se do vçech konŸin, aby V s'/
     2' tr pily a zdr§ovaly pýi ka§d‚m kroku.')
C
      IF (IRIT.EQ.10230) IC1=MMPRP(ITST(8))
      IF (IRIT.EQ.10230) WRITE(LU,10230) IPRP(1,IC1),
     :   (IRNM(L,IFSD(IROM(ITST(8)))),L=1,8)
10230 FORMAT(/
     1' Tis¡ce výeçt¡c¡ch d‚mon… vyletØly z truhly a hledaly cestu'/
     2' ven ze zamŸen‚ho pokoje. Kdy§ cestu nenaçly, vr tily se do'/
     3' truhly mumlaj¡ce cosi o vra§dØ 'A2' '8A2)
C
10910 F='(/1X,4A2'' nelze otevý¡t.'')'
      IF (IRIT .EQ. 10910) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
      IF (IRIT.EQ.12140) WRITE(LU,12140)
12140 FORMAT(/' Podlaha se nap…l zasunula do stØny. Pýi pohledu ',
     1'dol… vid¡te hlubokou'/' çachtu se spir lovitìm schodiçtØm ',
     2'vysekanìm do zdi'/' vedouc¡ do hlubok‚ temnoty.')
C
15020 F='(/'' Dveýe, pýed kterìmi stoj¡te, nemaj¡ z mek.'')'
      IF (IRIT .EQ. 15020) WRITE(LU,f)
C
15030 F='(/'' Pýed V mi nejsou dveýe.'')'
      IF (IRIT .EQ. 15030) WRITE(LU,f)
C
15040 F='(/'' Potýebujete kl¡Ÿ.'')'
      IF (IRIT .EQ. 15040) WRITE(LU,f)
C
15110 F='(/'' V ç kl¡Ÿ se nehod¡ do z mku.'')'
      IF (IRIT.EQ.15110) WRITE(LU,f)
C
20030 F='(/'' NebuÔte hloupì.'')'
      IF (IRIT .EQ. 20030) WRITE(LU,f)
C
      IF (LU.NE.IPR(1).OR.ITST(21).EQ.0) GOTO 9999
      LU=ITST(21)
      GOTO 999
 9999 NAME(2)='SB'
c      CALL SGLOD(NAME,LU)
      return
      END
