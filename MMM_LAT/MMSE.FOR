$storage:2
$large
$nodebug
      subroutine mmse(p1,p2,p3,p4,p5)
c      ,06-09-1985 MMM    8536E D45 PL&BM <850920.1536>
C     04-02-1988 MS-DOS S.M.
C
C *** definice common ***
C
$INCLUDE:'MMCO.for'
      integer*2 p1,p2,p3,p4,p5
      REAL*8 CDTN(11),CWP
      character*72 f
      EQUIVALENCE (IDTN,CDTN),(IWP,CWP)
C
C ******* SEGMENT E *******
C
C  Segment popisuje vzhled mistnosti  na prikaz 'rozhlednout' nebo
C  pri prvnim vstupu do mistnosti. Vraci se zpet do MMSB.
C
C     CALL RMPAR(IPR(1))
C
      ipr(1)=p1
      ipr(2)=p2
      ipr(3)=p3
      ipr(4)=p4
      ipr(5)=p5
      LU=IPR(1)
      IR=IPR(2)
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
      IF (IVEN(6).LT.20000) IDN=10-IFC
      IF (IDN.EQ.6) IDN=10
      IF (IVEN(6).LT.20000) IDE=11-IFC
      IF (IVEN(6).LT.20000) IDS=12-IFC
      IF (IDS.EQ.11) IDS=7
      IF (IVEN(6).LT.20000) IDW=13-IFC
      IF (IDW.GT.10) IDW=IDW-4
C
C *** po zhrouceni ***
C
  999 IF (ITST(5) .GT. 450.AND.IR.LT.84) THEN
 1000 FORMAT(
     :/' VÁechno je v ruin†ch po vÏbuchu a zhroucen°. VÁude je kou˝,'/
     : ' kterÏ st†le houstne. Kolem V†s neustale pad† drobnÏ prach a'/
     : ' obüas i nÿjakÇ temnÇ p˝edmÿty.')
         GOTO 2000
      ENDIF
      GOTO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     : 21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
     : 41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
     : 61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
     : 81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99),IR
C
C  BUTLER'S ROOM
C
    1 WRITE(LU,401)
      IF (IVEN(46).NE.1101) THEN
  101    F='(''  Na zdi je  pr†zdnÏ HèK  pro KLí¨EK  od vÿzen°.'')'
         WRITE(LU,f)
      ELSE
  201    F='(''  Na protÿjÁ° zdi vis° na HèKU KLí¨EK od vÿzen°.'')'
         WRITE(LU,f)
      ENDIF
  301 F='('' Jestliße nenajdete jinou cestu, mÖßete j°t '',A8)'
      WRITE(LU,f) CDTN(IDE)
      GOTO 2000
  401 FORMAT(/
     :' M°stnost je vybavena pouze dÿtskou postÏlkou, stoliükou'/
     :' a lavic°, na n°ß je trs ban†nÖ. Je zde opice, p˝iv†zan†'/
     :' v rohu. '\)
C
C  FURNACE ROOM
C
   2  WRITE(LU,102) CDTN(IDE),CDTN(IDN)
 102  FORMAT(/
     :' V kotelnÿ je teplo, na zemi leß° kousky uhl°, vÁude'/
     :' jsou saze, v nichß jsou z˝etelnÇ stopy vedouc° p˝es'/
     :' kotelnu 'A8' od V†s a takÇ 'A8)
      GOTO 2000
C
C  DARK PIT
C
   3  WRITE(LU,103) CDTN(IDW)
  103 FORMAT(/
     :' Je zde schodiÁtÿ vedouc° NAHORU a nÿkolik oÁlapanÏch'/
     :' schodÖ vedouc°ch  DOLﬁ  do temnoty, odkud lze slyÁet'/
     :' podivnÇ zvuky. Jsou zde takÇ dve˝e vedouc° 'A8)
      GOTO 2000
C
C  LOWER SECRET PASSAGE
C
   4  WRITE(LU,104)
  104 FORMAT(/
     :' Je zde  strmÇ  schodiÁtÿ  vedouc°  vzhÖru  alespoÂ do'/
     :' dvou dalÁ°ch pater. MÖßete j°t kamkoliv, jen ne dolÖ.')
      GOTO 2000
C
C  CREEPY CRYPT
C
   5  WRITE(LU,105) CDTN(IDS)
  105 FORMAT(/
     :' Je zde hodnÿ rakv°, obsahuj°c°ch pozÖstatky bÏvalÏch majitelÖ'/
     :' z†mku. VÁechny rakve kromÿ jedinÇ vypadaj° neporuÁenÇ a pevnÿ'/
     :' uzav˝enÇ. Pokud neum°te otev˝°t vchod do tajnÇ chodby, mus°te'/
     :' j°t 'A8', abyste se dostal ven.')
      GOTO 2000
C
C  WINE CELLAR
C
   6  WRITE(LU,106) CDTN(IDN)
  106 FORMAT(/
     :' Police jsou zcela prazdnÇ, pravdÿpodobnÿ doraßenÇ p˝edchoz°mi'/
     :' hr†üi. JedinÇ, co zbylo, je pr†zdnÏ sud a rozbitÇ l†hve. Dve˝e'/
     :' vedouc° 'A8' jsou jedinou cestou odsud.')
      GOTO 2000
C
C  STORAGE ROOM
C
    7 WRITE(LU,107) CDTN(IDE)
      IF (IXT(320).NE.0) THEN
         WRITE(LU,207)
      ELSE
         WRITE(LU,307)
      ENDIF
  107 FORMAT(/
     :' M°stnost je zane˝†dÿn†  nepot˝ebnÏmi odpadky. Je zde podivnÿ'/
     :' vypadaj°c° koÁ. MÖßete nahoru po oÁlapanÇm d˝evÿnÇm ßeb˝°ku,'/
     :' nebo 'A8' tÿßkÏmi  d˝evÿnÏmi  dve˝mi s malÏm okÇnkem, na'/
     :' kterÇm jsou ßeleznÇ m˝°ße. '\)
  207 FORMAT                     ('U zdi vid°te p†r schodÖ vedouc°ch'/
     :' dolÖ, do temnÏch hlubin.')
  307 FORMAT                     ('U zdi vid°te zavalenou ü†st scho-'/
     :' diÁtÿ, kterÇ vedlo do podzem°.')
      GOTO 2000
C
C  DAMP DUNGEON
C
   8  WRITE(LU,108) CDTN(IDW)
      IF (IXT(184).EQ.0) THEN
         WRITE(LU,208)
      ELSE
         WRITE(LU,308)
      ENDIF
  108 FORMAT(/
     :' PozÖstatky koster dvou p˝edchoz°ch  hr†üÖ vis° v okovech'/
     :' na protÿjÁ° zdi. Mezi nimi jsou jedny prazdnÇ. Je slyÁet'/
     :' piÁtÿn° nÿkolika  pob°haj°c°ch krys. JedinÏ vÏchod jsou '/
     :' jsou dve˝e 'A8', kterÇ pr†vÿ nÿkdo zav°r†'\)
  208 FORMAT(' a zamyk†.')
  308 FORMAT('.')
      GOTO 2000
C
C                            MM-E-2  (LINE 3920)
C
C 3-E
C
C  FOOD CELLAR
C
   9  WRITE(LU,109) CDTN(IDS)
  109 FORMAT(/
     :' Vypad† to zde, jako by se krysy pustily do vÿtÁiny potra-'/
     :' vin. MÖßete j°t 'A8' dve˝mi oznaüenÏmi  VINNÌ SKLEP'/
     :' nebo NAHORU po ßeb˝°ku, kterÏ vede do kuchynÿ.')
      GOTO 2000
C
C  LIVING ROOM
C
   10 WRITE(LU,110) CDTN(IDE)
  110 FORMAT(/
     :' Je to velikÏ a pohodlnÏ  pokoj s velkou olejovou lampou'/
     :' na podstavci v rohu. Na stÿnÿ vis° velkÏ obraz. Zd† se,'/
     :' ße 'A8' je jedinÏ vÏchod odsud.')
      GOTO 2000
C
C  ENTRANCE HALL
C
   11 WRITE(LU,111)
  111 FORMAT(/
     :' Na stÿnÿ u p˝edn°ch dve˝° vis° starÏ telefon s klikou.'/
     :' VelkÏm oknem nad n°m lze  vidÿt  pozemek p˝ed  s°dlem.'/
     :' Jsou zde ütvery dve˝e, takße mÖßete j°t  VP¸ED,  VZAD,'/
     :' VPRAVO i VLEVO.')
      GOTO 2000
C
C  GAME ROOM
C
   12 WRITE(LU,112) CDTN(IDW)
  112 FORMAT(/
     :' Pokoj je na spadnut°,  jsou zde zbytky rÖznÏch starÏch'/
     :' her, h†zec° Áipky, velkÏ kuleün°k,  Áachovnice s roze-'/
     :' hranou parti°, nÿkolik bal°ükÖ karet a sta˝iükÇ radio.'/
     :' V rohu m°stnosti je schodiÁtÿ vedouc° NAHORU  a  DOLﬁ,'/
     :' dalÁ°m vÏchodem jsou dve˝e vedouc° 'A8)
      GOTO 2000
C
C  MIDDLE SECRET PASSAGE
C
   13 WRITE(LU,113) CDTN(IDW)
  113 FORMAT(/
     :' Je zde schodiÁtÿ, vedouc° nahoru a dolÖ do ostatn°ch'/
     :' poschod°. MÖßete j°t kam chcete, mimo ',A8)
      GOTO 2000
C
C  BALL ROOM
C
   14 WRITE(LU,114) CDTN(IDW)
  114 FORMAT(/
     :' M°stnost je obrovsk† a pr†zdn†. NejtiÁÁ° zvuk se vrac°'/
     :' p˝es celÏ s†l s ozvÿnou. Jedinou zaj°mavou vÿc° je zde'/
     :' krb se z†sobou d˝eva. Je zde schodiÁtÿ  vedouc° NAHORU'/
     :' a dve˝e na kaßdÇ stranÿ pokoje, jen ne 'A8)
      GOTO 2000
C
C  DINING HALL
C
   15 WRITE(LU,115) CDTN(IDN),CDTN(IDW)
  115 FORMAT(/
     :' Mus°te proj°t kolem ˝ady stolÖ, abyste se dostal kamkoliv.'/
     :' Na polic°ch vysoko na zdech stoj° stovky d˝evÿnÏch indi†nÖ'/
     :' s nataßenÏmi luky.   Jsou zde dvoje dve˝e,  kterÏmi mÖßete'/
     :' j°t 'A8' nebo 'A8)
      GOTO 2000
C
C  CHARMING CHAPEL
C
   16 IC=9-ITST(6)/3600
      WRITE(LU,116)
      IF (IVEN(19).EQ.1216) WRITE(LU,216)IC
      IF (IVEN(5).EQ.1316) THEN
         WRITE(LU,316)
      ELSE
         WRITE(LU,416)
      ENDIF
      WRITE(LU,516) CDTN(IDE)
  116 FORMAT(/
     :' Vp˝edu je p˝ekr†snÏ olt†˝ s trochou v°na'\)
  216 FORMAT(                                  ' a b°lou'/
     :' jasnÿ ho˝°c° sv°ükou, dlouhou 'I3' palcÖ'\)
  316 FORMAT                                  ('. Na zdi'/
     :' za olt†˝em vis° asi stopu dlouhÏ st˝°brnÏ k˝°ßek.')
  416 FORMAT                                  ('. Na zdi'/
     :' jsou vidÿt obrysy k˝°ße, kterÏ tam obvykle visel.')
  516 FORMAT(
     :' V rohu kaple jsou schody vedouci DOLﬁ do temnoty'/
     :' a dve˝e 'A8', kde lze vidÿt ü†st knihovny.')
      GOTO 2000
C
C  LIBRARY
C
   17 IF (IVEN(88).NE.1017) THEN
         WRITE(LU,117)
      ELSE
         WRITE(LU,217)
      ENDIF
      WRITE(LU,317) CDTN(IDW),CDTN(IDN),CDTN(IDS)
  117 FORMAT(/
     :' Stoj°te pobl°ß malÇho podstavce, na nÿmß je starÏ'/
     :' papyrusovÏ svitek. '\)
  217 FORMAT(/
     :' Stoj°te pobl°ß podstavce, na nÿmß jsou rozdrobenÇ'/
     :' zbytky papyru.   '\)
  317 FORMAT             ('PodÇl vÁech stÿn jsou ˝ady po-'/
     :' lic se starÏmi knihami. MÖßete proj°t 'A8/
     :' otev˝enÏmi dve˝mi, skrz nÿß lze vidÿt ü†st kaple,'/
     :' a 'A8' nebo 'A8' jednÿmi ze dvou malÏch dve˝°.')
      GOTO 2000
C
C  KITCHEN
C
   18 WRITE(LU,118) CDTN(IDS)
  118 FORMAT(/
     :' Je to velk† star† kuchynÿ,  navrßen† pro p˝°pravu  j°del'/
     :' pro velkÏ poüet lid°. Je zde ßeb˝°k vedouc° DOLﬁ a dve˝e,'/
     :' kterÏmi mÖßete vyj°t ',A8)
      GOTO 2000
C
C  BROOM CLOSET
C
   19 WRITE(LU,119) CDTN(IDE)
  119 FORMAT(/
     :' Je to malÏ z†chodek bez moßnosti pohybu. Zd† se, ße 'A8
     :' jsou jedinÇ dve˝e ven. Stÿny, podlaha a dokonce i strop'/
     :' vypadaj° jako kdyby mÿly tajnÏ vÏchod.')
      GOTO 2000
C
C  MASTER BEDROOM
C
   20 WRITE(LU,120) CDTN(IDW),CDTN(IDN),CDTN(IDS)
  120 FORMAT(/
     :' Pokoj je typicky p†nskÏ. Na zd°ch je nÿkolik velkÏch'/
     :' zv°˝ec°ch hlav spoleünÿ s dalÁ°mi trofejemi.  MÖßete'/
     :' j°t 'A8' na malÏ z†chod, nebo vyj°t 'A8' do'/
     :' haly, nebo po poß†rn°m ßeb˝°ku 'A8)
      GOTO 2000
C
C  BELL TOWER
C
   21 WRITE(LU,121) CDTN(IDN),CDTN(IDE)
  121 FORMAT(/
     :' Uprost˝ed vÿße je velkÏ zvon s propletenÏm  provazem,'/
     :' upevnÿnÏm  na  srdci  zvonu,  na  kterÏ  nedos†hnete.'/
     :' MÖßete j°t 'A8' dlouhou  klesajic°  chodbou  nebo'/
     :' DOLﬁ toüitÏm schodiÁtÿm, nebo 'A8' z velkÇho okna'/
     :' vyhl°ßej°c°ho na oce†n pod n°m.')
      GOTO 2000
C
C  UPPER SECRET PASSAGE
C
   22 WRITE(LU,122) CDTN(IDW)
  122 FORMAT(/' Jste na schodiÁti vedouc°m dolÖ do nißÁ°ch poschod°.'/
     :' MÖßete j°t vÁemi smÿry mimo NAHORU a 'A8)
      GOTO 2000
C
C  SOUTH HALLWAY
C
   23 WRITE(LU,123) CDTN(IDN)
  123 FORMAT(/
     :' Je to velmi dlouh† hala s malÏm okÇnkem na vzd†lenÿjÁ°m kon-'/
     :' ci, na podlaze je hodnÿ smet°. MÖßete j°t libovolnÏm smÿrem,'/
     :' mimo NAHORU. K druhÇmu konci haly se dostanete, kdyß pÖjdete'/
     :' ',A8)
      GOTO 2000
C
C  LARGE BEDROOM
C
   24 WRITE(LU,124) CDTN(IDN),CDTN(IDW),CDTN(IDS)
  124 FORMAT(/
     :' Je to ßensk† loßnice, s Áic°m  strojem, velkÏm zrcadlem'/
     :' krejüovskou pannou a dalÁ°mi p˝edmÿty. MÖßete j°t 'A8/
     :' na z†chod, nebo opustit pokoj 'A8' nebo 'A8)
      GOTO 2000
C
C  MAID'S ROOM
C
   25 WRITE(LU,125)
      IF (IVEN(30).NE.325) THEN
         WRITE(LU,225)
      ELSE
         WRITE(LU,325)
      ENDIF
      WRITE(LU,425) CDTN(IDE)
  125 FORMAT(/
     :' M°stnost je hezky upraven†, na oknech vis° ozdobnÇ z†clony.'/
     :' P˝°mo p˝ed V†mi je mÿkkÇ pohodlnÇ lÖßko s nebesy'\)
  225 FORMAT                                          ('.  JedinÏ')
  325 FORMAT                                          ('  a kouli'/
     :' uprost˝ed. JedinÏ'\)
  425 FORMAT(' vÏchod je 'A8)
      GOTO 2000
C
C  NORTH HALLWAY
C
   26 WRITE(LU,126) CDTN(IDS),CDTN(IDW),CDTN(IDE)
  126 FORMAT(/
     :' Je zde malÇ okÇnko, ale jestliße  pÖjdete 'A8' na'/
     :' druhou stranu haly, nebudete vidÿt. MÖßete j°t dve˝mi'/
     :' ',A8' nebo 'A8)
      GOTO 2000
C
C  CLOSET FOR LABORATORY
C
   88 CONTINUE
   27 WRITE(LU,127) CDTN(IDS)
  127 FORMAT(/
     :' Tento z†chod je velikÏ a bez zvl†Átnost°, mimo hromadu'/
     :' hader v jednom rohu a malÏch  dve˝° ve stÿnÿ  vlevo od'/
     :' vchodu. Nevid°te ß†dnÏ jinÏ vÏchod mimo dve˝e 'A8)
      GOTO 2000
C
C  TWISTY MAZE
C
   28 CONTINUE
   29 CONTINUE
   30 CONTINUE
   31 CONTINUE
   32 CONTINUE
   33 CONTINUE
   34 CONTINUE
      WRITE(LU,128)
  128 FORMAT(/
     :' Jste v toüivÇm bludiÁti s malÏmi chodbami. Chodby jsou'/
     :' nizkÇ a mus°te  se  shrbit, abyste si neotloukl hlavu.'/
     :' Je z˝ejmÇ, ße nÿkdo zde str†vil hodnÿ üasu.')
      GOTO 2000
C
C  HIDEOUS HIGHWAY
C
   35 IF (ITST(17).LT.5) THEN
         IC=ITST(17)*200
         WRITE(LU,135)
         WRITE(LU,235) IC
      ELSEIF (ITST(17).LT.10) THEN
         WRITE(LU,135)
         WRITE(LU,335)
      ELSE
         WRITE(LU,435)
      ENDIF
  135 FORMAT(/
     :' Cesta vede p˝°mo na jih, aß za obzor, mezi hustÏmi lesy'/
     :' na z†padÿ a 500 s†hÖ vysokÏm p˝°krÏm £tesem na vÏchodÿ.')
  235 FORMAT(' Br†na z†mku je asi 'I4' s†hÖ na sever.')
  335 FORMAT(' Br†nu vid°te na severu.')
  435 FORMAT(' Cesta vede na jih do nedohledna.')
      GOTO 2000
C
C  CORRIDORS
C
   36 WRITE(LU,136)
  136 FORMAT(/
     :' Stÿny a podlaha jsou vlhkÇ a pokrytÇ slizkÏm liÁejn°kem,'/
     :' kterÏ üin° chodbu kluzkou. Lev† stÿna chodby se pohybuje'/
     :' k V†m, VPRAVO jsou dve˝e. Chodbou fouk† chladnÏ v†nek.')
      GOTO 2000
C
C 2
C
   37 WRITE(LU,137)
  137 FORMAT(/
     :' JedinÇ, co mÖßete vidÿt, jsou slizkÇ zdi a podlaha. Chod-'/
     :' ba rychle kles† do hlubokÇ temnoty, ale na konci je vidÿt'/
     :' temnÏ otvor. Je klid, kromÿ obüasnÇho zvuku padaj°c°ch'/
     :' kapek vody.')
      GOTO 2000
C
C 3
C
   38 WRITE(LU,138)
  138 FORMAT(/
     :' Mus°te bÏt alespoÂ v polovinÿ  cesty dolÖ chodbou, stÿß°'/
     :' vid°te ßeb˝°k na doln°m konci  a £zkÏ prÖchod na opaünÇm'/
     :' konci.')
      GOTO 2000
C
C 4
C
   39 WRITE(LU,139)
  139 FORMAT(/
     :' Jste na doln°m  konci  chodby.  ¶eb˝°k  vede nahoru a dolÖ.'/
     :' P˝°üky ßeb˝°ku jsou pokryty nÿjakou odpornÿ slizkou hmotou.')
      GOTO 2000
C
C  SHORT CUT
C
   40 WRITE(LU,140)
      IF (IXT(140).NE.0) THEN
         WRITE(LU,240) CDTN(IDS)
      ELSE
         WRITE(LU,340)
      ENDIF
  140 FORMAT(/
     :' Jste v dlouhÇm prÖchodu  rovnobÿßnÇm  s  chodbou. Zem je zde'/
     :' such† a üist†, takße mÖßete snadno proj°t aß na druhÏ konec.')
  240 FORMAT(
     :' MÖßete seÁplhat DOLﬁ po ßeb˝°ku na druhÇm konci  nebo mÖßete'/
     :' j°t dve˝mi 'A8)
  340 FORMAT(' Stÿny chodby se sev˝ely a zablokovaly vÁechny vÏchody.')
      GOTO 2000
C
C  TRICKY TREASURY
C
   41 WRITE(LU,141)
  141 FORMAT(/
     :' Jste ve velkÇ pokladnici. Zd† se, ße zde kdysi bylo mnoho'/
     :' cennÏch  p˝edmÿtÖ. VÁiml  jste si malÇho otvoru ve st˝edu'/
     :' n†stÿnnÇ  malby. Je  zde  uzkÏ  ßeb˝°k  vedouc°  vzhÖru.')
C
      IF (IXT(320).EQ.0) WRITE(LU,241)
  241 FORMAT(' Jsou zde schody vytesanÇ po stran†ch obrovskÇ Áachty',
     :' vedouc° DOLﬁ.')
      GOTO 2000
C
C  MOLE MAZE
C
   42 CONTINUE
   43 CONTINUE
   44 CONTINUE
   45 CONTINUE
   46 CONTINUE
   47 CONTINUE
   48 CONTINUE
   49 CONTINUE
   50 CONTINUE

      IF (IXT(320).NE.0) THEN
         WRITE(LU,142)
      ELSE
  142 FORMAT(/
     :' Jste v bludiÁti s velkÏmi chodbami. NÿkterÇ z nich se zdaj°'/
     :' bÏt üerstvÿ  vykopanÇ. Jsou zde vÁude velkÇ  podivnÇ otisky'/
     :' nohou a tu a tam se povaluj° kosti.')
C
C  TREASURE TREK
C
         WRITE(LU,242)
      ENDIF
  242 FORMAT(/
     :' Jste ve sklepen°ch  pod s°dlem. Vysekat  vÁechny  schody a'/
     :' chodby do sk†ly dalo asi mnoho pr†ce. VÁude se tu povaluj°'/
     :' uvolnÿnÇ balvany.')
      GOTO 2000
C
C  MOLE'S VAULT
C
   51 IF (IXT(320) .NE. 0) THEN
         WRITE(LU,142)
         GOTO 2000
      ELSE
        GOTO 251
      ENDIF
C
C
C
   52 IF (IXT(320) .NE. 0) THEN
         WRITE(LU,152) CWP
  152 FORMAT(/
     :' Povaluje se tu znaünÇ mnoßstv° kost°. NÿkterÇ z nich jsou'/
     :' z˝ejmÿ lidskÇ. Na zdi vid°te krv° napsanou zpr†vu. ¨tete:'/
     :/'           Vraßednou zbran° je 'A8'!')
         GOTO 2000
      ELSE
         GOTO 251
      ENDIF
C
C  MOLE CAVE
C
   53 IF (IXT(320) .NE. 0) THEN
         WRITE(LU,153)
  153 FORMAT(/' Tato jeskynÿ je nad s°dlem obrovskeho krtka. ',
     1'Dokud zde budete,'/' bude st†le mÿnit svÇ tunely a pokouÁet ',
     2'se V†s zm†st.'/' Vedou odsud cesty NAHORU a DOLﬁ.')
         GOTO 2000
      ELSE
         GOTO 251
      ENDIF
C
C  TROLL TRAP
C
  251 WRITE(LU,252)
  252 FORMAT(/
     :' M°stnost byla kdysi d†vno  vysek†na do velmi pevnÇ sk†ly.')
      IF (IR .EQ. 51 .OR. IR .EQ. 53) WRITE(LU,151)
  151 FORMAT(
     :' V rohu tvrdÿ sp° trpasl°k a je z˝ejmÿ dostateünÿ velkÏ na'/
     :' to, aby zcela zaplnil  jedinÏ vÏchod  uprost˝ed m°stnosti.')
      IF (IR .EQ. 51) THEN
         IF (IVEN(42) .EQ. 1151) WRITE(LU,551)
  551 FORMAT(
     :' V jednÇ ruce m† rub°novÏ  KROU¶EK a v druhÇ ruce m† velkÏ'/
     :' klacek.')
         IF (IVEN(40) .EQ. 1151) WRITE(LU,351)
  351 FORMAT(
     :' Vedle trpasl°ka je ßiv† socha p˝ekr†snÇ nahÇ ßeny, tiÁe a'/
     :' svÖdnÿ zp°v† a l†k†  V†s  nÿkolika  DIAMANTY v nataßenÏch'/
     :' dlan°ch.')
         WRITE(LU,451)
  451 FORMAT(
     :' Naproti  trpasl°kovi je zlat†  FIGURíNA  bÏka na  hromadÿ'/
     :' tyü°.')
      ELSEIF (IR .EQ. 53) THEN
         IF (IVEN(39).EQ.1253) WRITE(LU,253)
  253 FORMAT(
     :' V jednÇ  ruce m† st˝°brnou ¨íÊI a ve druhÇ velkÏ klacek.')
         IF (IVEN(37).EQ.1153) WRITE(LU,353)
  353 FORMAT(
     :' Vedle trpasl°ka je kamennÏ bÖßek s velkÏm SMARAGDEM m°sto'/
     :' oka.')
         WRITE(LU,453)
  453 FORMAT(
     :' Naproti trpasl°kovi je kamenn† obÿtn° deska, na n° leß°'/
     :' DÌKA.')
      ELSE
C
C  DEATH'S DEN
C
         WRITE(LU,352)
         IF (IXT(319).NE.0) WRITE(LU,452)
  352 FORMAT(' Je zde pr†zdno mimo nÿkolika bludnÏch balvanÖ a ',
     2'nÿkolika koster'/' pokrytÏch Át°ry. Podlaha je nerovn†, ',
     3'pln† vÏmolÖ.')
  452 FORMAT(' Jedinou cestou odsud je tunel DOLﬁ, kterÏ je ',
     1'uprost˝ed doupÿte.')
      ENDIF
      GOTO 2000
C
C  VEXING VERANDA
C
   54 WRITE(LU,154)
  154 FORMAT(/' Kolem V†s je zahrada s vysokÏmi ßivÏmi ploty.',
     1' Hlavn° pÿÁinka'/' vede SEVERN∑ a po stran†ch jsou jednosmÿrnÇ',
     2' br†ny. Na ceduli je'/' naps†no, ße se pro VaÁi bezpeünost',
     3' br†ny po z†padu slunce zav°raj°.')
      GOTO 2000
C
C  GARDEN GATE
C
   55 WRITE(LU,155)
  155 FORMAT(/' Tabulka ˝°k†: Z†kaz trh†n° kvÿtÖ, koup†n° a vstupu ',
     1'na travnatÇ plochy.'/' MÖßete j°t JI¶N∑ na verandu nebo ',
     2'VÌCHODN∑ üi ZèPADN∑.')
      GOTO 2000
C
C  GARDEN EXIT
C
   56 CONTINUE
   57 WRITE(LU,156)
  156 FORMAT(/' Tabulka ˝°k†: Douf†me, ße se V†m zde l°bilo. MÖßete ',
     1'j°t SEVERN∑'/' do zahrady a nebo na verandu.')
      GOTO 2000
C
C  GARGOYLE GARDEN
C
   58 CONTINUE
   59 CONTINUE
   60 CONTINUE
   61 CONTINUE
   62 CONTINUE
   63 CONTINUE
      WRITE(LU,158)
  158 FORMAT(/
     :' VysokÇ ßivÇ ploty V†m br†n° v rozhledu, ale st†le vid°te'/
     :' smÿrem jißnÿ st˝echu s°dla.  Vedou odsud pÿÁiny nÿkolika'/
     :' smÿry.')
      GOTO 2000
C
C  GARDEN SOUTH OF MEADOW
C
   64 CONTINUE
   65 WRITE(LU,164)
      WRITE(LU,264)
  164 FORMAT(/' Jste na jißn°m konci travnatÇ louky.')
      GOTO 277
C
C  GARDEN WEST OF MEADOW
C
   66 WRITE(LU,166)
  166 FORMAT(/
     :' Jste na z†padn°m konci travnatÇ louky. Z†padnÿ od V†s je'/
     :' vysok† ze‘ s°dla, j°t mÖßete vÁemi ostatn°mi smÿry.')
      GOTO 277
C
C  GARDEN EAST OF MEADOW
C
   67 WRITE(LU,167)
  167 FORMAT(/
     :' Jste na vÏchodn°m konci travnatÇ louky. Na vÏchod od V†s'/
     :' je vysok† ze‘ s°dla, j°t mÖßete pÿÁinkami ve vÁech osta-'/
     :' n°ch smÿrech.')
      GOTO 277
C
C  GARDEN WEST OF POND
C
   68 WRITE(LU,168) (IVRB(L,7),L=1,3)
      WRITE(LU,264)
  168 FORMAT(/
     :' Jste na severn°m konci travnatÇ louky a na z†padn°m b˝ehu'/
     :' velkÇho prot†hlÇho rybn°ka, t†hnouc°ho se na vÏchod. P˝es'/
     2' rybn°k vede most, na kterÇm ütete slovo '3A2)
      GOTO 277
C
C  GARDEN EAST OF POND
C
   69 WRITE(LU,169) (IVRB(L,11),L=1,3)
      WRITE(LU,264)
  169 FORMAT(/
     :' Jste na severn°m konci travnatÇ louky a na vÏchodn°m'/
     :' b˝ehu dlouhÇho prot†hlÇho rybn°ka,  t†hnouc°ho se na'/
     :' z†pad. P˝es rybn°k vede most, na nÿmß ütete '3A2)
      GOTO 277
C
C  GARDEN NORTH OF POND
C
   70 WRITE(LU,170)
  170 FORMAT(/
     :' Jste na severn°m b˝ehu rybn°ka. Severnÿ od V†s je'/
     :' vysok† ze‘.')
      GOTO 2000
C
C  GRASSY MEADOW
C
   71 CONTINUE
   72 WRITE(LU,171)
      WRITE(LU,264)
  171 FORMAT(/' Tr†va je vysok† a v okol° je mnoho k˝ov°.')
  264 FORMAT(' MÖßete j°t libovolnÏm smÿrem.')
      GOTO 2000
C
C  FOUL FOUNTAIN
C
   73 CONTINUE
   74 WRITE(LU,173)
  173 FORMAT(/
     :' Font†na z˝ejmÿ nebyla mnoho let v provozu, je vyschl† a'/
     :' zasypan† list°m. MÖßete j°t vÁemi smÿry, kromÿ SEVERN∑.')
      GOTO 2000
C
C  CENTER WELL
C
   75 WRITE(LU,175)
  175 FORMAT(/' Studna je hlubok† a tmav†. Cesty vedou VYCHODN∑ a',
     1' ZèPADN∑.')
      GOTO 2000
C
C  WELL EAST OF MEADOW
C
   76 WRITE(LU,176)
  176 FORMAT(/
     :' Jste na vÏchodn°m konci travnatÇ louky pobl°ß temnÇ studny.'/
     :' J°t mÖßete SEVERN∑ a ZèPADN∑.')
      Goto 277
C
C  WELL WEST OF MEADOW
C
   77 WRITE(LU,177)
  177 FORMAT(/
     :' Jste na z†padn°m konci travnatÇ louky pobl°ß temnÇ studny.'/
     :' J°t mÖßete SEVERN∑ a VÌCHODN∑.')
c
c  VLK
c
  277 IF (IRES(15).LT.10000) WRITE(LU,377)
  377 FORMAT(' P˝i pohledu do d†li vid°te, ße se k V†m nÿco ',
     1'p˝iblißuje.')
      GOTO 2000
C
C  WET WELL
C
   78 CONTINUE
   80 WRITE(LU,178)
  178 FORMAT(/' Zdi jsou kluzkÇ a nemÖßete se odsud dostat.')
      GOTO 2000
C
C DRY WELL
C
   79 WRITE(LU,179)
  179 FORMAT(/' Je zde sucho. Zdi jsou z nerovnÏch kamenÖ a mÖßete '/
     1'se snadno vyÁplhat NAHORU.')
      GOTO 2000
C
C  WEST BRIDGE
C
   81 WRITE(LU,181) (IVRB(L,10),L=1,3)
  181 FORMAT(/
     :' Jste na mostÿ p˝es rybn°k.   Kdyß se naklon°te p˝es okraj'/
     1' mostu, ütete slovo '3A2' odr†ßej°c° se ve vodn° hladinÿ.'/
     2' MÖßete j°t JI¶N∑ nebo SEVERN∑.')
      GOTO 2000
C
C  EAST BRIDGE
C
   82 WRITE(LU,181) (IVRB(L,6),L=1,3)
      GOTO 2000
C
C  POLLUTED POND
C
   83 WRITE(LU,183) (IVRB(L,8),L=1,3),(IVRB(L,4),L=1,3)
  183 FORMAT(/
     :' Jste po p†s ve vodÿ. VÏchodnÿ a z†padnÿ od V†s jsou mosty'/
     :' p˝es rybn°k,  ütete na nich slova  '3A2 ' a '3A2 '. B˝ehy'/
     2' rybn°ka jsou strmÇ a nemÖßete se dostat ven z vody.')
      GOTO 2000
C
C   COTTAGE
C
   84 WRITE (LU,184)
  184 FORMAT(/
     :' Stoj°te uprost˝ed malÇ chaloupky na n†horn° ploÁinÿ po stranÿ'/
     :' kaÂonu. Kdyß se rozhlÇdnete, vid°te '\)
      IF (IVEN(28).EQ.1184) WRITE(LU,284)
  284 FORMAT                             ('malou cestovn° mapu lehce'/
     :' upevnÿnou na stÿnÿ.  '\)
      IF (IVEN(28).NE.1184) WRITE(LU,384)
  384 FORMAT                             ('zpr†vu na zdi, kde visela'/
     :' mapa: Jestliße zabloud°te v lese, dostanete se ven, kdyß na-'/
     :' p°Áete JíT BEZHLAV∑. '\)
      WRITE(LU,484)
  484 FORMAT               ('Od zadn°ch dve˝° vede pÿÁina smÿ˝uj°c°'/
     3' d†le po h˝ebenu. Je zde takÇ stezka vedouc° dolÖ k potoku v'/
     4' lese.')
      GOTO 2000
C
C  FIRE ESCAPES
C
   86 IF (IXT(265) .ne. 0) THEN
         IXT(265)=0
         ITST(14)=86
         WRITE(LU,186)
      ENDIF
  186 FORMAT(/
     :' ¶eb˝°k se rozpadl, kdyß jste se Áplhal nahoru a nemÖßete'/
     :' se dostat zpÿt do podkrovi.')
   85 WRITE(LU,185)
  185 FORMAT(/
     :' Je zde poß†rn° ßeb˝°k zhotovenÏ z rezivÇ oceli.')
      If (IXT(322) .NE. 0) THEN
         WRITE(LU,285)
  285 FORMAT(' Po poß†rn°m ßeb˝°ku mÖßete slÇzt DOLﬁ.')
      ELSE
         WRITE(LU,385)
  385 FORMAT(
     :' Z†vÿsy poß†rn°ho ßeb˝°ku jsou vÁak tak zrezivÿlÇ, ße s n°m'/
     :' nejde pohnout.')
      ENDIF
      GOTO 2000
C
C  AWKWARD ATTIC
C
   87 WRITE(LU,187)
  187 FORMAT(/
     :' Jste v zapr†ÁenÇm pokoji s pavuüinami. Mus°te se skrüit,'/
     :' abyste si neotloukl hlavu.')
      IF (ITST(5).LT.300) WRITE(LU,287)
  287 FORMAT(
     :' MalÏ krb slabÿ osvÿtluje okol° tak, ße trochu vid°te.')
      IF (IXT(265).NE.0) WRITE(LU,387)
  387 FORMAT(
     :' Je zde napÖl rozpadlÏ ßeb˝°k vedouc° vzhÖru k poß†rn°mu'/
     :' ßeb˝°ku.')
      GOTO 2000
C
C  LABORATORY
C
   89 WRITE(LU,189)
  189 FORMAT(/
     :' Je to laborato˝ z˝ejmÿ Á°lenÇho vÿdce. Je zde mnoho p˝°-'/
     1' strojÖ, kterÇ vÁechny bruü° a jsou p˝ipraveny k poußit°.'/
     2' Je zde jak†si telefonn° budka,  kter† vypad† jako kabina'/
     3' pro transport hmoty.  Z velkÏch oken je vÏhled na okol°.'/
     4' Dve˝e jsou JI¶N∑ a v podlaze.')
      GOTO 2000
C
C  BATTY BATHROOM
C
   90 WRITE(LU,190) CDTN(IDW)
  190 FORMAT(/
     :' Je zde obvyklÇ za˝°zen°; zvl†Átÿ pak velmi kr†sn† sprcha.'/
     :' Naproti vchodu jsou ve zdi malÇ dve˝e a dalÁ° jsou z˝ejmÿ'/
     :' ve stropÿ. Z malÇho okna naho˝e je vidÿt hluboko dolÖ.'/
     :' Zd† se, ße jedin† cesta odsud je 'A8)
      GOTO 2000
C
C  FRONT PORCH
C
   91 WRITE(LU,191)
  191 FORMAT(/
     :' Stoj°te na prkennÇ podlaze vchodu do s°dla. Na dve˝°ch vis°'/
     :' mal† plaketa s nÿjakÏm n†pisem. MÖßete j°t bu‘ SEVERN∑ do'/
     :' s°dla nebo ZèPADN∑ cestou ke vchodu.')
      GOTO 2000
C
C  DREARY DRIVEWAY
C
   92 WRITE(LU,192)
  192 FORMAT(/
     :' Jste na dlouhÇ cestÿ, kter† vede vÏchodnÿ od br†ny s°dla.'/
     1' Tabule u cesty ˝°k†: zastav se, rozhlÇdni se a poslouchej.'/
     :' Ze‘ je jißnÿ, na severu je vidÿt les. J°t mÖßete VÌCHODN∑'/
     :' nebo ZèPADN∑.')
      GOTO 2000
C
C  MAIN GATE
C
   93 WRITE(LU,193)
  193 FORMAT(/' Jste jißnÿ od tÿßkÇ ßeleznÇ br†ny. N†pis ˝°k†:',
     1//'      Vstup na vlastn° nebezpeü°.'/)
      IF (IVEN(4).EQ.493) WRITE(LU,293)
  293 FORMAT(' Pod n†pisem vis° na br†nÿ vyleÁtÿn† mosazn† lucerna.')
      IF (IVEN(4).EQ.1493) WRITE(LU,393)
  393 FORMAT(' Vedle n†pisu na kovovÇm h†ku vis° jasnÿ sv°t°c° ',
     1'lucerna.')
      IF (IVEN(4).NE.493.AND.IVEN(4).NE.1493) WRITE(LU,493)
  493 FORMAT(' Na br†nÿ vedle n†pisu je pr†zdnÏ kovovÏ h†k.')
      WRITE(LU,593)
  593 FORMAT(
     :' VÌCHODN∑ i ZèPADN∑ vede cesta zahÏbaj°c° severnÿ podÇl zdi'/
     :' ütvrt m°le odsud.  Jste na konci silnice, kter† vede JI¶N∑'/
     :' do velkÇho mÿsta.')
      GOTO 2000
C
C  WEST ROADWAY
C
   94 WRITE(LU,194)
  194 FORMAT(/
     :' Jste vnÿ z†padn° zdi s°dla. Cesta zde konü°, ale ze‘ pokra-'/
     :' üuje SEVERN∑ do hustÏch lesÖ. Les je takÇ  ZAPADN∑  od ce-'/
     2' sty, ale je ü†steünÿ prosek†n. JI¶N∑, asi ütvrt m°le odsud,'/
     3' zat†ü° cesta VÌCHODN∑.')
      GOTO 2000
C
C  BACK GATE
C
   95 WRITE(LU,195)
  195 FORMAT(/
     :' Jste na cestÿ u zadn° br†ny s°dla. Panty jsou zcela'/
     1' prorezavÿlÇ a br†na vis° napÖl otev˝en†. Cesta vede'/
     2' branou JI¶N∑ mezi dvÿma ˝adami ßivÇho plotu. SEVER-'/
     3' N∑ od cesty je asi 500 stop vysokÏ sr†znÏ £tes, ty-'/
     4' ü°c° se vysoko nad mo˝em.  Cesta zde konü°, ale ze‘'/
     5' pokraüuje ZèPADN∑ do hustÇho lesa.')
      GOTO 2000
C
C  EAST ROADWAY
C
   96 WRITE(LU,196)
  196 FORMAT(/
     :' Jste p˝ed vÏchodn° zd° s°dla. Ze‘ vede JI¶N∑ kolem s°dla,'/
     1' podÇl n° vede cesta, kter† pak zahÏb† ZèPADN∑. Cesta pok-'/
     2' raüuje takÇ SEVERN∑, kde rovnÿß zat†ü° ZèPADN∑, asi ütvrt'/
     3' m°le odsud. VÌCHODN∑ od zdi se tyü° tÇmÿ˝ 500 stop vysokÏ'/
     4' p˝°krÏ £tes.')
      GOTO 2000
C
C  FOREST
C
   97 WRITE(LU,197)
  197 FORMAT(/
     :' Jste v ˝°dkÇm lese. Nejsou zde ß†dnÇ pÿÁiny, mÖßete j°t'/
     :' libovolnÏm smÿrem.')
      GOTO 2000
C
C  DENSE WOODS
C
   98 WRITE(LU,198)
  198 FORMAT(/
     :' Jste v hustÇm lese. NemÖßete p˝esnÿ urüit svÿtovÇ strany,'/
     :' takße mÖßete chodit st†le dokoleüka, aniß byste se dostal'/
     :' ven.')
      GOTO 2000
C
C  STREAM
C
   99 WRITE(LU,199)
  199 FORMAT(/
     :' Byst˝ina teüe ze severu na jih v hlubokÇm kaÂonu. Jsou zde'/
     :' schody vyÁlapanÇ do sk†ly, kterÇ vedou  DOLﬁ  do jeskynÿ a'/
     2' NAHORU po stÿnÿ kaÂonu do chaloupky postavenÇ na malÇ plo-'/
     3' Áince.')
 2000 IF (LU.NE.IPR(1).OR.ITST(21).EQ.0) GOTO 3000
      LU=ITST(21)
      GOTO 999
 3000 NAME(2)='SB'
c     CALL SGLOD(NAME,IPR(1))
      p1=ipr(1)
      return
      END
