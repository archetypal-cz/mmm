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
     :/' V®echno je v ruin†ch po vòbuchu a zhroucen°. V®ude je kou©,'/
     : ' kterò st†le houstne. Kolem V†s neustale pad† drobnò prach a'/
     : ' obáas i nàjakÇ temnÇ p©edmàty.')
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
  101    F='(''  Na zdi je  pr†zdnò HèK  pro KLãÄEK  od vàzen°.'')'
         WRITE(LU,f)
      ELSE
  201    F='(''  Na protàj®° zdi vis° na HèKU KLãÄEK od vàzen°.'')'
         WRITE(LU,f)
      ENDIF
  301 F='('' Jestliëe nenajdete jinou cestu, mñëete j°t '',A8)'
      WRITE(LU,f) CDTN(IDE)
      GOTO 2000
  401 FORMAT(/
     :' M°stnost je vybavena pouze dàtskou postòlkou, stoliákou'/
     :' a lavic°, na n°ë je trs ban†nñ. Je zde opice, p©iv†zan†'/
     :' v rohu. '\)
C
C  FURNACE ROOM
C
   2  WRITE(LU,102) CDTN(IDE),CDTN(IDN)
 102  FORMAT(/
     :' V kotelnà je teplo, na zemi leë° kousky uhl°, v®ude'/
     :' jsou saze, v nichë jsou z©etelnÇ stopy vedouc° p©es'/
     :' kotelnu 'A8' od V†s a takÇ 'A8)
      GOTO 2000
C
C  DARK PIT
C
   3  WRITE(LU,103) CDTN(IDW)
  103 FORMAT(/
     :' Je zde schodi®tà vedouc° NAHORU a nàkolik o®lapanòch'/
     :' schodñ vedouc°ch  DOL¶  do temnoty, odkud lze sly®et'/
     :' podivnÇ zvuky. Jsou zde takÇ dve©e vedouc° 'A8)
      GOTO 2000
C
C  LOWER SECRET PASSAGE
C
   4  WRITE(LU,104)
  104 FORMAT(/
     :' Je zde  strmÇ  schodi®tà  vedouc°  vzhñru  alespo§ do'/
     :' dvou dal®°ch pater. Mñëete j°t kamkoliv, jen ne dolñ.')
      GOTO 2000
C
C  CREEPY CRYPT
C
   5  WRITE(LU,105) CDTN(IDS)
  105 FORMAT(/
     :' Je zde hodnà rakv°, obsahuj°c°ch pozñstatky bòvalòch majitelñ'/
     :' z†mku. V®echny rakve kromà jedinÇ vypadaj° neporu®enÇ a pevnà'/
     :' uzav©enÇ. Pokud neum°te otev©°t vchod do tajnÇ chodby, mus°te'/
     :' j°t 'A8', abyste se dostal ven.')
      GOTO 2000
C
C  WINE CELLAR
C
   6  WRITE(LU,106) CDTN(IDN)
  106 FORMAT(/
     :' Police jsou zcela prazdnÇ, pravdàpodobnà doraëenÇ p©edchoz°mi'/
     :' hr†ái. JedinÇ, co zbylo, je pr†zdnò sud a rozbitÇ l†hve. Dve©e'/
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
     :' M°stnost je zane©†dàn†  nepot©ebnòmi odpadky. Je zde podivnà'/
     :' vypadaj°c° ko®. Mñëete nahoru po o®lapanÇm d©evànÇm ëeb©°ku,'/
     :' nebo 'A8' tàëkòmi  d©evànòmi  dve©mi s malòm okÇnkem, na'/
     :' kterÇm jsou ëeleznÇ m©°ëe. '\)
  207 FORMAT                     ('U zdi vid°te p†r schodñ vedouc°ch'/
     :' dolñ, do temnòch hlubin.')
  307 FORMAT                     ('U zdi vid°te zavalenou á†st scho-'/
     :' di®tà, kterÇ vedlo do podzem°.')
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
     :' Pozñstatky koster dvou p©edchoz°ch  hr†áñ vis° v okovech'/
     :' na protàj®° zdi. Mezi nimi jsou jedny prazdnÇ. Je sly®et'/
     :' pi®tàn° nàkolika  pob°haj°c°ch krys. Jedinò vòchod jsou '/
     :' jsou dve©e 'A8', kterÇ pr†và nàkdo zav°r†'\)
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
     :' Vypad† to zde, jako by se krysy pustily do vàt®iny potra-'/
     :' vin. Mñëete j°t 'A8' dve©mi oznaáenòmi  VINNù SKLEP'/
     :' nebo NAHORU po ëeb©°ku, kterò vede do kuchynà.')
      GOTO 2000
C
C  LIVING ROOM
C
   10 WRITE(LU,110) CDTN(IDE)
  110 FORMAT(/
     :' Je to velikò a pohodlnò  pokoj s velkou olejovou lampou'/
     :' na podstavci v rohu. Na stànà vis° velkò obraz. Zd† se,'/
     :' ëe 'A8' je jedinò vòchod odsud.')
      GOTO 2000
C
C  ENTRANCE HALL
C
   11 WRITE(LU,111)
  111 FORMAT(/
     :' Na stànà u p©edn°ch dve©° vis° starò telefon s klikou.'/
     :' Velkòm oknem nad n°m lze  vidàt  pozemek p©ed  s°dlem.'/
     :' Jsou zde átvery dve©e, takëe mñëete j°t  VPûED,  VZAD,'/
     :' VPRAVO i VLEVO.')
      GOTO 2000
C
C  GAME ROOM
C
   12 WRITE(LU,112) CDTN(IDW)
  112 FORMAT(/
     :' Pokoj je na spadnut°,  jsou zde zbytky rñznòch staròch'/
     :' her, h†zec° ®ipky, velkò kuleán°k,  ®achovnice s roze-'/
     :' hranou parti°, nàkolik bal°ákñ karet a sta©iákÇ radio.'/
     :' V rohu m°stnosti je schodi®tà vedouc° NAHORU  a  DOL¶,'/
     :' dal®°m vòchodem jsou dve©e vedouc° 'A8)
      GOTO 2000
C
C  MIDDLE SECRET PASSAGE
C
   13 WRITE(LU,113) CDTN(IDW)
  113 FORMAT(/
     :' Je zde schodi®tà, vedouc° nahoru a dolñ do ostatn°ch'/
     :' poschod°. Mñëete j°t kam chcete, mimo ',A8)
      GOTO 2000
C
C  BALL ROOM
C
   14 WRITE(LU,114) CDTN(IDW)
  114 FORMAT(/
     :' M°stnost je obrovsk† a pr†zdn†. Nejti®®° zvuk se vrac°'/
     :' p©es celò s†l s ozvànou. Jedinou zaj°mavou vàc° je zde'/
     :' krb se z†sobou d©eva. Je zde schodi®tà  vedouc° NAHORU'/
     :' a dve©e na kaëdÇ stranà pokoje, jen ne 'A8)
      GOTO 2000
C
C  DINING HALL
C
   15 WRITE(LU,115) CDTN(IDN),CDTN(IDW)
  115 FORMAT(/
     :' Mus°te proj°t kolem ©ady stolñ, abyste se dostal kamkoliv.'/
     :' Na polic°ch vysoko na zdech stoj° stovky d©evànòch indi†nñ'/
     :' s nataëenòmi luky.   Jsou zde dvoje dve©e,  kteròmi mñëete'/
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
     :' Vp©edu je p©ekr†snò olt†© s trochou v°na'\)
  216 FORMAT(                                  ' a b°lou'/
     :' jasnà ho©°c° sv°ákou, dlouhou 'I3' palcñ'\)
  316 FORMAT                                  ('. Na zdi'/
     :' za olt†©em vis° asi stopu dlouhò st©°brnò k©°ëek.')
  416 FORMAT                                  ('. Na zdi'/
     :' jsou vidàt obrysy k©°ëe, kterò tam obvykle visel.')
  516 FORMAT(
     :' V rohu kaple jsou schody vedouci DOL¶ do temnoty'/
     :' a dve©e 'A8', kde lze vidàt á†st knihovny.')
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
     :' Stoj°te pobl°ë malÇho podstavce, na nàmë je starò'/
     :' papyrusovò svitek. '\)
  217 FORMAT(/
     :' Stoj°te pobl°ë podstavce, na nàmë jsou rozdrobenÇ'/
     :' zbytky papyru.   '\)
  317 FORMAT             ('PodÇl v®ech stàn jsou ©ady po-'/
     :' lic se staròmi knihami. Mñëete proj°t 'A8/
     :' otev©enòmi dve©mi, skrz nàë lze vidàt á†st kaple,'/
     :' a 'A8' nebo 'A8' jednàmi ze dvou malòch dve©°.')
      GOTO 2000
C
C  KITCHEN
C
   18 WRITE(LU,118) CDTN(IDS)
  118 FORMAT(/
     :' Je to velk† star† kuchynà,  navrëen† pro p©°pravu  j°del'/
     :' pro velkò poáet lid°. Je zde ëeb©°k vedouc° DOL¶ a dve©e,'/
     :' kteròmi mñëete vyj°t ',A8)
      GOTO 2000
C
C  BROOM CLOSET
C
   19 WRITE(LU,119) CDTN(IDE)
  119 FORMAT(/
     :' Je to malò z†chodek bez moënosti pohybu. Zd† se, ëe 'A8
     :' jsou jedinÇ dve©e ven. Stàny, podlaha a dokonce i strop'/
     :' vypadaj° jako kdyby màly tajnò vòchod.')
      GOTO 2000
C
C  MASTER BEDROOM
C
   20 WRITE(LU,120) CDTN(IDW),CDTN(IDN),CDTN(IDS)
  120 FORMAT(/
     :' Pokoj je typicky p†nskò. Na zd°ch je nàkolik velkòch'/
     :' zv°©ec°ch hlav spoleánà s dal®°mi trofejemi.  Mñëete'/
     :' j°t 'A8' na malò z†chod, nebo vyj°t 'A8' do'/
     :' haly, nebo po poë†rn°m ëeb©°ku 'A8)
      GOTO 2000
C
C  BELL TOWER
C
   21 WRITE(LU,121) CDTN(IDN),CDTN(IDE)
  121 FORMAT(/
     :' Uprost©ed vàëe je velkò zvon s propletenòm  provazem,'/
     :' upevnànòm  na  srdci  zvonu,  na  kterò  nedos†hnete.'/
     :' Mñëete j°t 'A8' dlouhou  klesajic°  chodbou  nebo'/
     :' DOL¶ toáitòm schodi®tàm, nebo 'A8' z velkÇho okna'/
     :' vyhl°ëej°c°ho na oce†n pod n°m.')
      GOTO 2000
C
C  UPPER SECRET PASSAGE
C
   22 WRITE(LU,122) CDTN(IDW)
  122 FORMAT(/' Jste na schodi®ti vedouc°m dolñ do nië®°ch poschod°.'/
     :' Mñëete j°t v®emi smàry mimo NAHORU a 'A8)
      GOTO 2000
C
C  SOUTH HALLWAY
C
   23 WRITE(LU,123) CDTN(IDN)
  123 FORMAT(/
     :' Je to velmi dlouh† hala s malòm okÇnkem na vzd†lenàj®°m kon-'/
     :' ci, na podlaze je hodnà smet°. Mñëete j°t libovolnòm smàrem,'/
     :' mimo NAHORU. K druhÇmu konci haly se dostanete, kdyë pñjdete'/
     :' ',A8)
      GOTO 2000
C
C  LARGE BEDROOM
C
   24 WRITE(LU,124) CDTN(IDN),CDTN(IDW),CDTN(IDS)
  124 FORMAT(/
     :' Je to ëensk† loënice, s ®ic°m  strojem, velkòm zrcadlem'/
     :' krejáovskou pannou a dal®°mi p©edmàty. Mñëete j°t 'A8/
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
     :' P©°mo p©ed V†mi je màkkÇ pohodlnÇ lñëko s nebesy'\)
  225 FORMAT                                          ('.  Jedinò')
  325 FORMAT                                          ('  a kouli'/
     :' uprost©ed. Jedinò'\)
  425 FORMAT(' vòchod je 'A8)
      GOTO 2000
C
C  NORTH HALLWAY
C
   26 WRITE(LU,126) CDTN(IDS),CDTN(IDW),CDTN(IDE)
  126 FORMAT(/
     :' Je zde malÇ okÇnko, ale jestliëe  pñjdete 'A8' na'/
     :' druhou stranu haly, nebudete vidàt. Mñëete j°t dve©mi'/
     :' ',A8' nebo 'A8)
      GOTO 2000
C
C  CLOSET FOR LABORATORY
C
   88 CONTINUE
   27 WRITE(LU,127) CDTN(IDS)
  127 FORMAT(/
     :' Tento z†chod je velikò a bez zvl†®tnost°, mimo hromadu'/
     :' hader v jednom rohu a malòch  dve©° ve stànà  vlevo od'/
     :' vchodu. Nevid°te ë†dnò jinò vòchod mimo dve©e 'A8)
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
     :' Jste v toáivÇm bludi®ti s malòmi chodbami. Chodby jsou'/
     :' nizkÇ a mus°te  se  shrbit, abyste si neotloukl hlavu.'/
     :' Je z©ejmÇ, ëe nàkdo zde str†vil hodnà áasu.')
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
     :' Cesta vede p©°mo na jih, aë za obzor, mezi hustòmi lesy'/
     :' na z†padà a 500 s†hñ vysokòm p©°kròm £tesem na vòchodà.')
  235 FORMAT(' Br†na z†mku je asi 'I4' s†hñ na sever.')
  335 FORMAT(' Br†nu vid°te na severu.')
  435 FORMAT(' Cesta vede na jih do nedohledna.')
      GOTO 2000
C
C  CORRIDORS
C
   36 WRITE(LU,136)
  136 FORMAT(/
     :' Stàny a podlaha jsou vlhkÇ a pokrytÇ slizkòm li®ejn°kem,'/
     :' kterò áin° chodbu kluzkou. Lev† stàna chodby se pohybuje'/
     :' k V†m, VPRAVO jsou dve©e. Chodbou fouk† chladnò v†nek.')
      GOTO 2000
C
C 2
C
   37 WRITE(LU,137)
  137 FORMAT(/
     :' JedinÇ, co mñëete vidàt, jsou slizkÇ zdi a podlaha. Chod-'/
     :' ba rychle kles† do hlubokÇ temnoty, ale na konci je vidàt'/
     :' temnò otvor. Je klid, kromà obáasnÇho zvuku padaj°c°ch'/
     :' kapek vody.')
      GOTO 2000
C
C 3
C
   38 WRITE(LU,138)
  138 FORMAT(/
     :' Mus°te bòt alespo§ v polovinà  cesty dolñ chodbou, stàë°'/
     :' vid°te ëeb©°k na doln°m konci  a £zkò prñchod na opaánÇm'/
     :' konci.')
      GOTO 2000
C
C 4
C
   39 WRITE(LU,139)
  139 FORMAT(/
     :' Jste na doln°m  konci  chodby.  íeb©°k  vede nahoru a dolñ.'/
     :' P©°áky ëeb©°ku jsou pokryty nàjakou odpornà slizkou hmotou.')
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
     :' Jste v dlouhÇm prñchodu  rovnobàënÇm  s  chodbou. Zem je zde'/
     :' such† a áist†, takëe mñëete snadno proj°t aë na druhò konec.')
  240 FORMAT(
     :' Mñëete se®plhat DOL¶ po ëeb©°ku na druhÇm konci  nebo mñëete'/
     :' j°t dve©mi 'A8)
  340 FORMAT(' Stàny chodby se sev©ely a zablokovaly v®echny vòchody.')
      GOTO 2000
C
C  TRICKY TREASURY
C
   41 WRITE(LU,141)
  141 FORMAT(/
     :' Jste ve velkÇ pokladnici. Zd† se, ëe zde kdysi bylo mnoho'/
     :' cennòch  p©edmàtñ. V®iml  jste si malÇho otvoru ve st©edu'/
     :' n†stànnÇ  malby. Je  zde  uzkò  ëeb©°k  vedouc°  vzhñru.')
C
      IF (IXT(320).EQ.0) WRITE(LU,241)
  241 FORMAT(' Jsou zde schody vytesanÇ po stran†ch obrovskÇ ®achty',
     :' vedouc° DOL¶.')
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
     :' Jste v bludi®ti s velkòmi chodbami. NàkterÇ z nich se zdaj°'/
     :' bòt áerstvà  vykopanÇ. Jsou zde v®ude velkÇ  podivnÇ otisky'/
     :' nohou a tu a tam se povaluj° kosti.')
C
C  TREASURE TREK
C
         WRITE(LU,242)
      ENDIF
  242 FORMAT(/
     :' Jste ve sklepen°ch  pod s°dlem. Vysekat  v®echny  schody a'/
     :' chodby do sk†ly dalo asi mnoho pr†ce. V®ude se tu povaluj°'/
     :' uvolnànÇ balvany.')
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
     :' Povaluje se tu znaánÇ mnoëstv° kost°. NàkterÇ z nich jsou'/
     :' z©ejmà lidskÇ. Na zdi vid°te krv° napsanou zpr†vu. Ätete:'/
     :/'           Vraëednou zbran° je 'A8'!')
         GOTO 2000
      ELSE
         GOTO 251
      ENDIF
C
C  MOLE CAVE
C
   53 IF (IXT(320) .NE. 0) THEN
         WRITE(LU,153)
  153 FORMAT(/' Tato jeskynà je nad s°dlem obrovskeho krtka. ',
     1'Dokud zde budete,'/' bude st†le mànit svÇ tunely a pokou®et ',
     2'se V†s zm†st.'/' Vedou odsud cesty NAHORU a DOL¶.')
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
     :' V rohu tvrdà sp° trpasl°k a je z©ejmà dostateánà velkò na'/
     :' to, aby zcela zaplnil  jedinò vòchod  uprost©ed m°stnosti.')
      IF (IR .EQ. 51) THEN
         IF (IVEN(42) .EQ. 1151) WRITE(LU,551)
  551 FORMAT(
     :' V jednÇ ruce m† rub°novò  KROUíEK a v druhÇ ruce m† velkò'/
     :' klacek.')
         IF (IVEN(40) .EQ. 1151) WRITE(LU,351)
  351 FORMAT(
     :' Vedle trpasl°ka je ëiv† socha p©ekr†snÇ nahÇ ëeny, ti®e a'/
     :' svñdnà zp°v† a l†k†  V†s  nàkolika  DIAMANTY v nataëenòch'/
     :' dlan°ch.')
         WRITE(LU,451)
  451 FORMAT(
     :' Naproti  trpasl°kovi je zlat†  FIGURãNA  bòka na  hromadà'/
     :' tyá°.')
      ELSEIF (IR .EQ. 53) THEN
         IF (IVEN(39).EQ.1253) WRITE(LU,253)
  253 FORMAT(
     :' V jednÇ  ruce m† st©°brnou ÄãõI a ve druhÇ velkò klacek.')
         IF (IVEN(37).EQ.1153) WRITE(LU,353)
  353 FORMAT(
     :' Vedle trpasl°ka je kamennò bñëek s velkòm SMARAGDEM m°sto'/
     :' oka.')
         WRITE(LU,453)
  453 FORMAT(
     :' Naproti trpasl°kovi je kamenn† obàtn° deska, na n° leë°'/
     :' DùKA.')
      ELSE
C
C  DEATH'S DEN
C
         WRITE(LU,352)
         IF (IXT(319).NE.0) WRITE(LU,452)
  352 FORMAT(' Je zde pr†zdno mimo nàkolika bludnòch balvanñ a ',
     2'nàkolika koster'/' pokrytòch ®t°ry. Podlaha je nerovn†, ',
     3'pln† vòmolñ.')
  452 FORMAT(' Jedinou cestou odsud je tunel DOL¶, kterò je ',
     1'uprost©ed doupàte.')
      ENDIF
      GOTO 2000
C
C  VEXING VERANDA
C
   54 WRITE(LU,154)
  154 FORMAT(/' Kolem V†s je zahrada s vysokòmi ëivòmi ploty.',
     1' Hlavn° pà®inka'/' vede SEVERNâ a po stran†ch jsou jednosmàrnÇ',
     2' br†ny. Na ceduli je'/' naps†no, ëe se pro Va®i bezpeánost',
     3' br†ny po z†padu slunce zav°raj°.')
      GOTO 2000
C
C  GARDEN GATE
C
   55 WRITE(LU,155)
  155 FORMAT(/' Tabulka ©°k†: Z†kaz trh†n° kvàtñ, koup†n° a vstupu ',
     1'na travnatÇ plochy.'/' Mñëete j°t JIíNâ na verandu nebo ',
     2'VùCHODNâ ái ZèPADNâ.')
      GOTO 2000
C
C  GARDEN EXIT
C
   56 CONTINUE
   57 WRITE(LU,156)
  156 FORMAT(/' Tabulka ©°k†: Douf†me, ëe se V†m zde l°bilo. Mñëete ',
     1'j°t SEVERNâ'/' do zahrady a nebo na verandu.')
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
     :' VysokÇ ëivÇ ploty V†m br†n° v rozhledu, ale st†le vid°te'/
     :' smàrem jiënà st©echu s°dla.  Vedou odsud pà®iny nàkolika'/
     :' smàry.')
      GOTO 2000
C
C  GARDEN SOUTH OF MEADOW
C
   64 CONTINUE
   65 WRITE(LU,164)
      WRITE(LU,264)
  164 FORMAT(/' Jste na jiën°m konci travnatÇ louky.')
      GOTO 277
C
C  GARDEN WEST OF MEADOW
C
   66 WRITE(LU,166)
  166 FORMAT(/
     :' Jste na z†padn°m konci travnatÇ louky. Z†padnà od V†s je'/
     :' vysok† zeÉ s°dla, j°t mñëete v®emi ostatn°mi smàry.')
      GOTO 277
C
C  GARDEN EAST OF MEADOW
C
   67 WRITE(LU,167)
  167 FORMAT(/
     :' Jste na vòchodn°m konci travnatÇ louky. Na vòchod od V†s'/
     :' je vysok† zeÉ s°dla, j°t mñëete pà®inkami ve v®ech osta-'/
     :' n°ch smàrech.')
      GOTO 277
C
C  GARDEN WEST OF POND
C
   68 WRITE(LU,168) (IVRB(L,7),L=1,3)
      WRITE(LU,264)
  168 FORMAT(/
     :' Jste na severn°m konci travnatÇ louky a na z†padn°m b©ehu'/
     :' velkÇho prot†hlÇho rybn°ka, t†hnouc°ho se na vòchod. P©es'/
     2' rybn°k vede most, na kterÇm átete slovo '3A2)
      GOTO 277
C
C  GARDEN EAST OF POND
C
   69 WRITE(LU,169) (IVRB(L,11),L=1,3)
      WRITE(LU,264)
  169 FORMAT(/
     :' Jste na severn°m konci travnatÇ louky a na vòchodn°m'/
     :' b©ehu dlouhÇho prot†hlÇho rybn°ka,  t†hnouc°ho se na'/
     :' z†pad. P©es rybn°k vede most, na nàmë átete '3A2)
      GOTO 277
C
C  GARDEN NORTH OF POND
C
   70 WRITE(LU,170)
  170 FORMAT(/
     :' Jste na severn°m b©ehu rybn°ka. Severnà od V†s je'/
     :' vysok† zeÉ.')
      GOTO 2000
C
C  GRASSY MEADOW
C
   71 CONTINUE
   72 WRITE(LU,171)
      WRITE(LU,264)
  171 FORMAT(/' Tr†va je vysok† a v okol° je mnoho k©ov°.')
  264 FORMAT(' Mñëete j°t libovolnòm smàrem.')
      GOTO 2000
C
C  FOUL FOUNTAIN
C
   73 CONTINUE
   74 WRITE(LU,173)
  173 FORMAT(/
     :' Font†na z©ejmà nebyla mnoho let v provozu, je vyschl† a'/
     :' zasypan† list°m. Mñëete j°t v®emi smàry, kromà SEVERNâ.')
      GOTO 2000
C
C  CENTER WELL
C
   75 WRITE(LU,175)
  175 FORMAT(/' Studna je hlubok† a tmav†. Cesty vedou VYCHODNâ a',
     1' ZèPADNâ.')
      GOTO 2000
C
C  WELL EAST OF MEADOW
C
   76 WRITE(LU,176)
  176 FORMAT(/
     :' Jste na vòchodn°m konci travnatÇ louky pobl°ë temnÇ studny.'/
     :' J°t mñëete SEVERNâ a ZèPADNâ.')
      Goto 277
C
C  WELL WEST OF MEADOW
C
   77 WRITE(LU,177)
  177 FORMAT(/
     :' Jste na z†padn°m konci travnatÇ louky pobl°ë temnÇ studny.'/
     :' J°t mñëete SEVERNâ a VùCHODNâ.')
c
c  VLK
c
  277 IF (IRES(15).LT.10000) WRITE(LU,377)
  377 FORMAT(' P©i pohledu do d†li vid°te, ëe se k V†m nàco ',
     1'p©ibliëuje.')
      GOTO 2000
C
C  WET WELL
C
   78 CONTINUE
   80 WRITE(LU,178)
  178 FORMAT(/' Zdi jsou kluzkÇ a nemñëete se odsud dostat.')
      GOTO 2000
C
C DRY WELL
C
   79 WRITE(LU,179)
  179 FORMAT(/' Je zde sucho. Zdi jsou z nerovnòch kamenñ a mñëete '/
     1'se snadno vy®plhat NAHORU.')
      GOTO 2000
C
C  WEST BRIDGE
C
   81 WRITE(LU,181) (IVRB(L,10),L=1,3)
  181 FORMAT(/
     :' Jste na mostà p©es rybn°k.   Kdyë se naklon°te p©es okraj'/
     1' mostu, átete slovo '3A2' odr†ëej°c° se ve vodn° hladinà.'/
     2' Mñëete j°t JIíNâ nebo SEVERNâ.')
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
     :' Jste po p†s ve vodà. Vòchodnà a z†padnà od V†s jsou mosty'/
     :' p©es rybn°k,  átete na nich slova  '3A2 ' a '3A2 '. B©ehy'/
     2' rybn°ka jsou strmÇ a nemñëete se dostat ven z vody.')
      GOTO 2000
C
C   COTTAGE
C
   84 WRITE (LU,184)
  184 FORMAT(/
     :' Stoj°te uprost©ed malÇ chaloupky na n†horn° plo®inà po stranà'/
     :' ka§onu. Kdyë se rozhlÇdnete, vid°te '\)
      IF (IVEN(28).EQ.1184) WRITE(LU,284)
  284 FORMAT                             ('malou cestovn° mapu lehce'/
     :' upevnànou na stànà.  '\)
      IF (IVEN(28).NE.1184) WRITE(LU,384)
  384 FORMAT                             ('zpr†vu na zdi, kde visela'/
     :' mapa: Jestliëe zabloud°te v lese, dostanete se ven, kdyë na-'/
     :' p°®ete JãT BEZHLAVâ. '\)
      WRITE(LU,484)
  484 FORMAT               ('Od zadn°ch dve©° vede pà®ina smà©uj°c°'/
     3' d†le po h©ebenu. Je zde takÇ stezka vedouc° dolñ k potoku v'/
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
     :' íeb©°k se rozpadl, kdyë jste se ®plhal nahoru a nemñëete'/
     :' se dostat zpàt do podkrovi.')
   85 WRITE(LU,185)
  185 FORMAT(/
     :' Je zde poë†rn° ëeb©°k zhotovenò z rezivÇ oceli.')
      If (IXT(322) .NE. 0) THEN
         WRITE(LU,285)
  285 FORMAT(' Po poë†rn°m ëeb©°ku mñëete slÇzt DOL¶.')
      ELSE
         WRITE(LU,385)
  385 FORMAT(
     :' Z†vàsy poë†rn°ho ëeb©°ku jsou v®ak tak zrezivàlÇ, ëe s n°m'/
     :' nejde pohnout.')
      ENDIF
      GOTO 2000
C
C  AWKWARD ATTIC
C
   87 WRITE(LU,187)
  187 FORMAT(/
     :' Jste v zapr†®enÇm pokoji s pavuáinami. Mus°te se skráit,'/
     :' abyste si neotloukl hlavu.')
      IF (ITST(5).LT.300) WRITE(LU,287)
  287 FORMAT(
     :' Malò krb slabà osvàtluje okol° tak, ëe trochu vid°te.')
      IF (IXT(265).NE.0) WRITE(LU,387)
  387 FORMAT(
     :' Je zde napñl rozpadlò ëeb©°k vedouc° vzhñru k poë†rn°mu'/
     :' ëeb©°ku.')
      GOTO 2000
C
C  LABORATORY
C
   89 WRITE(LU,189)
  189 FORMAT(/
     :' Je to laborato© z©ejmà ®°lenÇho vàdce. Je zde mnoho p©°-'/
     1' strojñ, kterÇ v®echny bruá° a jsou p©ipraveny k pouëit°.'/
     2' Je zde jak†si telefonn° budka,  kter† vypad† jako kabina'/
     3' pro transport hmoty.  Z velkòch oken je vòhled na okol°.'/
     4' Dve©e jsou JIíNâ a v podlaze.')
      GOTO 2000
C
C  BATTY BATHROOM
C
   90 WRITE(LU,190) CDTN(IDW)
  190 FORMAT(/
     :' Je zde obvyklÇ za©°zen°; zvl†®tà pak velmi kr†sn† sprcha.'/
     :' Naproti vchodu jsou ve zdi malÇ dve©e a dal®° jsou z©ejmà'/
     :' ve stropà. Z malÇho okna naho©e je vidàt hluboko dolñ.'/
     :' Zd† se, ëe jedin† cesta odsud je 'A8)
      GOTO 2000
C
C  FRONT PORCH
C
   91 WRITE(LU,191)
  191 FORMAT(/
     :' Stoj°te na prkennÇ podlaze vchodu do s°dla. Na dve©°ch vis°'/
     :' mal† plaketa s nàjakòm n†pisem. Mñëete j°t buÉ SEVERNâ do'/
     :' s°dla nebo ZèPADNâ cestou ke vchodu.')
      GOTO 2000
C
C  DREARY DRIVEWAY
C
   92 WRITE(LU,192)
  192 FORMAT(/
     :' Jste na dlouhÇ cestà, kter† vede vòchodnà od br†ny s°dla.'/
     1' Tabule u cesty ©°k†: zastav se, rozhlÇdni se a poslouchej.'/
     :' ZeÉ je jiënà, na severu je vidàt les. J°t mñëete VùCHODNâ'/
     :' nebo ZèPADNâ.')
      GOTO 2000
C
C  MAIN GATE
C
   93 WRITE(LU,193)
  193 FORMAT(/' Jste jiënà od tàëkÇ ëeleznÇ br†ny. N†pis ©°k†:',
     1//'      Vstup na vlastn° nebezpeá°.'/)
      IF (IVEN(4).EQ.493) WRITE(LU,293)
  293 FORMAT(' Pod n†pisem vis° na br†nà vyle®tàn† mosazn† lucerna.')
      IF (IVEN(4).EQ.1493) WRITE(LU,393)
  393 FORMAT(' Vedle n†pisu na kovovÇm h†ku vis° jasnà sv°t°c° ',
     1'lucerna.')
      IF (IVEN(4).NE.493.AND.IVEN(4).NE.1493) WRITE(LU,493)
  493 FORMAT(' Na br†nà vedle n†pisu je pr†zdnò kovovò h†k.')
      WRITE(LU,593)
  593 FORMAT(
     :' VùCHODNâ i ZèPADNâ vede cesta zahòbaj°c° severnà podÇl zdi'/
     :' átvrt m°le odsud.  Jste na konci silnice, kter† vede JIíNâ'/
     :' do velkÇho màsta.')
      GOTO 2000
C
C  WEST ROADWAY
C
   94 WRITE(LU,194)
  194 FORMAT(/
     :' Jste vnà z†padn° zdi s°dla. Cesta zde koná°, ale zeÉ pokra-'/
     :' áuje SEVERNâ do hustòch lesñ. Les je takÇ  ZAPADNâ  od ce-'/
     2' sty, ale je á†steánà prosek†n. JIíNâ, asi átvrt m°le odsud,'/
     3' zat†á° cesta VùCHODNâ.')
      GOTO 2000
C
C  BACK GATE
C
   95 WRITE(LU,195)
  195 FORMAT(/
     :' Jste na cestà u zadn° br†ny s°dla. Panty jsou zcela'/
     1' prorezavàlÇ a br†na vis° napñl otev©en†. Cesta vede'/
     2' branou JIíNâ mezi dvàma ©adami ëivÇho plotu. SEVER-'/
     3' Nâ od cesty je asi 500 stop vysokò sr†znò £tes, ty-'/
     4' á°c° se vysoko nad mo©em.  Cesta zde koná°, ale zeÉ'/
     5' pokraáuje ZèPADNâ do hustÇho lesa.')
      GOTO 2000
C
C  EAST ROADWAY
C
   96 WRITE(LU,196)
  196 FORMAT(/
     :' Jste p©ed vòchodn° zd° s°dla. ZeÉ vede JIíNâ kolem s°dla,'/
     1' podÇl n° vede cesta, kter† pak zahòb† ZèPADNâ. Cesta pok-'/
     2' raáuje takÇ SEVERNâ, kde rovnàë zat†á° ZèPADNâ, asi átvrt'/
     3' m°le odsud. VùCHODNâ od zdi se tyá° tÇmà© 500 stop vysokò'/
     4' p©°krò £tes.')
      GOTO 2000
C
C  FOREST
C
   97 WRITE(LU,197)
  197 FORMAT(/
     :' Jste v ©°dkÇm lese. Nejsou zde ë†dnÇ pà®iny, mñëete j°t'/
     :' libovolnòm smàrem.')
      GOTO 2000
C
C  DENSE WOODS
C
   98 WRITE(LU,198)
  198 FORMAT(/
     :' Jste v hustÇm lese. Nemñëete p©esnà uráit svàtovÇ strany,'/
     :' takëe mñëete chodit st†le dokoleáka, anië byste se dostal'/
     :' ven.')
      GOTO 2000
C
C  STREAM
C
   99 WRITE(LU,199)
  199 FORMAT(/
     :' Byst©ina teáe ze severu na jih v hlubokÇm ka§onu. Jsou zde'/
     :' schody vy®lapanÇ do sk†ly, kterÇ vedou  DOL¶  do jeskynà a'/
     2' NAHORU po stànà ka§onu do chaloupky postavenÇ na malÇ plo-'/
     3' ®ince.')
 2000 IF (LU.NE.IPR(1).OR.ITST(21).EQ.0) GOTO 3000
      LU=ITST(21)
      GOTO 999
 3000 NAME(2)='SB'
c     CALL SGLOD(NAME,IPR(1))
      p1=ipr(1)
      return
      END
