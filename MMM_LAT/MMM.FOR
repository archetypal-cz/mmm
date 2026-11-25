$STORAGE:2
$large
$nodebug

      PROGRAM MMM
C     04-02-1988 MMM  8826E PL & BM & SM
C
C     UPRAVY:
C        10-10-1982 - 18-02-1983 P.Lampa, B.Michal
C        17-06-1983 - SGLOD na tahani segmentu
C        05-07-1983 - revize vsech konstrukci, pridani novych reakci
C        23-11-1984 - OPRAVA RMPAR PRO RTE-VI
C        06-09-1985 - OPRAVY STARSICH CHYB
C        06-09-1985 - predelano sglod, pridan uran
C        04-02-1988 - prenos pod MS-DOS S.MatÏÁek
C        27-06-1988 - üesk† verze
C
C ******* Hlavni program obsahuje COMMON a nekolik procedur *******
C
C
      integer*2 p1,p2,p3,p4,p5
$INCLUDE:'MMCO.FOR'
C     CALL RMPAR(IPR)         ! parametry - lu terminalu
      RND=2.
C
C     IWRD  Ulozeni zadanych slov
C     IVRB  Slovesa
C     IPRP  Predlozky
C     IDTN  Smery
C     IRNM  Jmena pokoju
C     IROM  Data pokoju (cislo, osvetleni, apod.)
C     IRSN  Jmena obyvatelu
C     IRES  Data obyvatelu (cislo pokoje apod.)
C     IVEN  Data veci pouzivanych ve hre
C     IXT   Data vychodu a vchodu (odkud kam a stav dveri)
C     ICLS  Klasifikace hracu
C     ITST  Parametry a testovana data (cislo tahu, pokoj,...)
C     NAME  Jmeno volaneho segmentu
C     IPR   Data z RMPAR
C     RND   Nahodne cislo
C     NTAG  Jmeno hrace
C     IWP   Jmeno vrazedne zbrane
C
C ****** INDEX ******
C
C  -MMBC - strana M-2 az M-7   BLOCK COMMON
C
C  -MMRI - strana M-8 az M-10  Vypis veci
C
C  -MMRL - strana M-11         Kontrola svetla
C
C
C  -MMSA-  strana A-1 az A-5   Inicializacni segment
C
C  -MMSB-  strana B-1 az B-11  cteni,kontola casu
C
C  -MMSC-  strana C-1 az C-12  zpracovani sloves typu GO
C                              GO,CLIMB,ENTER,MAGIC,WORDS,DIG,FOLLOW
C
C  -MMSD-  strana D-1 az D-8   Vypisy pro MMSC
C
C  -MMSE-  strana E-1 az E-11  Popis pokoju
C
C  -MMSF-  strana F-1 az F-16  prvni slovesa
C                              QUIT,STOP,SCORE,GET,TAKE,DROP,GIVE,LOOK,
C                              READ,FIND,LIST,LIGHT,TURN,HELP,OPEN,
C                              LOSE,SNUFF,BLOWC
C
C  -MMSG-  strana G-1 az G-12  vypisy pro MMSF
C
C  -MMSH-  strana H-1 az H-11  vyssi slovesa
C                              EAT,RUB,BEAM,SWEEP,CALL,MOVE,ANSWER,FEED
C                              DISPLAY,ATTACK,KILL,SUSPEND,RESTORE,DRINK
C                              MYSTERY,QUESTION,LISTEN
C
C  -MMSI-  strana I-1 az I-10  vypisy pro MMSH
C
C  -MMSJ-  strana J-1 az J-7   dalsi slovesa
C                              UNLOCK,LOCK,SHOOT,OIL,RECORD,BREAK,PLACE,SLEEP
C
C  -MMSK-  strana K-1 az K-4   vypisy pro MMSJ
C
C  -MMSL-  strana L-1 az L-6   debug segment
C
C ***   Vyvolani MMSA - zacatek hry ***
C
      p1=0
1     CALL SGLOD(NAME,p1,p2,p3,p4,p5)
      if(p1.eq.0) goto 1
      stop
      END
C
C
      BLOCK DATA MMBCDT
C     ,10-02-1983 MMM    8536E D45 PL&BM <850920.1755>
C
C     *** complex promenne pro ulozeni 8 znaku ***
C
      INTEGER*2 IXT0(100),IXT1(100),IXT2(100),IXT3(60)
      REAL*8 CVRB(89),CCLS(12),CRNM(2,66),CRSN(17),CPRP(17),CDTN(11),
     +       CRNM1(2,50),CRNM2(2,16)
C
$INCLUDE:'MMCO.FOR'
C
      EQUIVALENCE (CVRB,IVRB),(CCLS,ICLS),(CRNM,IRNM),(CRSN,IRSN),
     1(CPRP,IPRP),(CDTN,IDTN),
     2(CRNM,CRNM1),(CRNM(1,51),CRNM2),
     3(IXT(1),IXT0),(IXT(101),IXT1),(IXT(201),IXT2),(IXT(301),IXT3)
C
C
C=====C                       MM-2
C M-3 C
C=====C
C
C    *** MMSC SLOVESA --
C
C                 GO         CLIMB      ENTER
      DATA CVRB/'J÷T     ','ÊPLHAT  ','VSTOUPIT','XIMOW   ','XIWOM   ',
     :'MOWIX   ','WOMIX   ','WIMOX   ','WOXIM   ','XIMOW   ','MOXIW   ',
     :'KOPAT   ','SLEDOVAT','        ','        ','        ','        ',
     :'KON¨IT  ','KONEC   ','SK‡RE   ','VZ÷T    ','SPRCHOVA','ODLO¶IT ',
     :'DµT     ','POD÷VAT ','¨÷ST    ','NAJ÷T   ','LIST    ','ROZSV÷TI',
     :'OBRµTIT ','POMOC   ','OTEV¸÷T ','ZAV¸÷T  ','UHASIT  ','FOUKAT  ',
     :'J÷ST    ','HLADIT  ','P¸EM÷STI','ZAMETAT ','ZAVOLAT ','POHNOUT ',
     :'ZVEDNOUT','NAKRMIT ','DISPLAY ','NAPADNOU','ZAB÷T   ','ZAPSAT  ',
     :'OBNOVIT ','P÷T     ','ZµHADA  ','PODPLATI','ZEPTAT  ','POSLOUCH',
     :'ODEMKNOU','ZAMKNOUT','ST¸ELIT ','OLEJOVAT','ZAZNAMEN','ROZB÷T  ',
     :'POLO¶IT ','SPµT    ','ULO¶IT  ','ODSUNOUT','VOLAT   ','TELEFONO',
     :'HLEDAT  ','P÷T     ','D÷VAT   ','UST¸ELIT','ZATµHNOU','ZAPµLIT ',
     :'ZHASNOUT','OTO¨IT  ','ZATO¨IT ','FOUKNOUT','T¸÷T    ','POV∑SIT ',
     :'ROZHRNOU','ODPOV∑D∑','KRMIT   ','ZAÈTO¨IT','ZAST¸ELI','NAOLEJOV',
     :'UM÷STIT ','ROZHLêDN','B∑¶ET   ','UT÷KAT  ','ZAPNOUT ','VYPNOUT '/
C
C=====C                       MM-3  (LINE 240)
C M-4 C
C=====C
C
C    *** Jmena pokoju
C
C                 Butlers               Furnace
      DATA CRNM1/'pokoji  ','sluhy   ','kotelnÿ ','        ',
     2'temnÇ ch','odbÿ    ','tajnÇm s','chodiÁti','hroznÇ h','robce   ',
     5'vinnÇm s','klepÿ   ','spiß°rnÿ','        ','vlhkÇ hl','adomornÿ',
     8'skladu j','°dla    ','obÏvac°m',' pokoji ','vstupn° ','hale    ',
     2'hernÿ   ','        ','taneün°m',' s†le   ','j°delnÿ ','        ',
     5'kouzelnÇ',' kapli  ','knihovnÿ','        ','kuchyni ','        ',
     8'z†chodÿ ','        ','p†nskÇ l','oßnici  ','zvonici ','        ',
     2'temnÇ ha','le      ','velkÇ lo','ßnici   ','pokoji s','lußky   ',
     5'toüivÇm ','bludiÁti','chladnÇ ','chodbÿ  ','prÖchodu','        ',
     8'pokladni','ci      ','krtü°m b','ludiÁti ','krtü° hr','obce    ',
     2'krtü° je','skyni   ','vÿtrnÇ v','erandÿ  ','zahradn°',' br†ny  ',
     5'vÏchodu ','z parku ','parku s ','chrliüi ','travnatÇ',' louce  ',
     8'zarostlÇ',' font†ny','kouzelnÇ',' studny ','poÁkozen','Çm mostÿ',
     2'ÁpinavÇm',' rybn°ku','£tulnÇ c','haloupce','poß†rn°m',' ßeb˝°ku',
     5'nezn†mÇ ','˝eky    ','podkrov°','        ','laborato','˝i      ',
     8'ohavnÇ c','estÿ    ','hlavn°m ','vchodu  ','cestÿ ke',' vchodu ',
     2'hlavn° b','r†ny    ','cestÿ   ','        ','zadn° br','†ny     '/
      DATA CRNM2/
     5'hustem l','ese     ','lese    ','        ','cestÿ k ','pokladu ',
     6'doupÿti ','smrti   ','trpasliü','° lÇüce ','koupelnÿ','        ',
     6'kouzelnÇ',' studni ','        ','        ','        ','        ',
     6'        ','        ','        ','        ','        ','        ',
     6'        ','        ','        ','        ','        ','        ',
     6'        ','        '/
C
C
C
C=====C                            MM-4
C 5-M C
C=====C
C
C     IROM = Z!XX!YY
C
C     Z = 0!1!2 ne!zatazene!otevrene zavesy; XX=body; YY=cislo jmena
C
C   prirazeni mist 1
C
      DATA IROM/5201,5202,5203,5804,5205,5206,5207,5208,5209,15210,
     125211,15212,5804,5213,15214,5215,15816,15217,5218,15219,25220,
     25804,5221,15222,15223,15221,5218,5824,5024,5024,5124,5024,5124,
     35024,25245,5225,5125,3025,5225,5826,8027,5828,5028,5028,5028,
     45128,5028,5028,5028,5028,5028,8029,5830,5231,5232,5233,5233,
     55034,5034,5034,5034,5034,5034,5234,5234,5234,5234,5234,5234,
     65234,6535,6535,5236,5236,5237,5237,5237,57,8057,57,5238,5238,
     839,6540,5241,8041,6543,6518,8044,5156,5246,6547,5248,5249,
     95250,5249,5152,5151,5842/
C
C
C
C
C  OBYVATELE      CORPSE     LADY       BUTLER     MOLE       VAMPIRE
C
      DATA CRSN/'MRTVOLA ','PAN÷    ','SLUHA   ','KRTEK   ','VAMP÷R  ',
     3'LESN÷K  ','SADA¸   ','SLU¶KA  ','KUCHA¸  ','VLKODLAK','SK¸÷TEK ',
     5'LOVEC   ','SK¸ET   ','PµN     ','VLK     ','RYT÷¸   ','        '/
C
C
C  IRES = zdravi/sila/pokoj = XX/Y/ZZ
C
C pozice obyvatel  1
C
      DATA IRES/19,424,911,43,300,894,763,525,718,0,
     1870,996,330,920,71,0,0/
C
C
C
C
C
C  *** predlozky --  IN      ON         BY         AT         TO
C
      DATA CPRP/'v       ','na      ','u       ','u       ','k       ',
     2'p˝es    ','z       ','ven     ','si      ','pro     ','!!!!!!  ',
     3'se      ','s       ','ve      ','do      ','od      ','za      '/
C
C
C
C
C
C
C  ***  Smery --  NORTH      EAST       SOUTH      WEST       UP
C
      DATA CDTN/'SEVERN∑ ','VÌCHODN∑','JI¶N∑   ','ZµPADN∑ ','NAHORU  ',
     2'DOLﬁ    ','VZAD    ','VLEVO   ','VP¸ED   ','VPRAVO  ','ZP∑T    '/
C
C
C
C
C  data veci
C
C  IVEN = majitel/druh/vaha/pokoj = W/X/Y/ZZ
C
C     majitel = 0 nikdo, 1 obyvatel, 2 hrac
C     druh =
C     vaha = 0 - 9
C
C
      DATA IVEN/0,121,220,493,1316,126,441,233,217,5218,9396,7312,
     1125,5308,295,520,152,105,1216,114,217,193,789,309,319,789,0,
     21184,1214,312,118,394,202,17213,625,0,0,0,0,
     40,0,0,0,0,14416,1101,0,0,10216,14316,0,0,20100,0,0,0,0,0,0,
     516,0,0,0,24,41,41,14,27,7,5,0,
     510,11,0,12,10,0,0,0,0,0,0,52,43,93,0,0,17,14,
     691,0,0,0,15,15,15,15,15,0,51,51,51,53,0,0,0,0/
C
C
C=====C                       MM-5  (LINE 400)
C 6-M C
C=====C
C
C    spoje  IXT = Z!XX!YY
C
C    Z = 0/1/2 - dvere ne/zamcene/odemcene; XX = kam; YY = ve kterem
C
C na sever
C
      DATA IXT0/401,502,704,20906,1310,1411,9562,1613,11714,1815,
     12219,22320,2421,2522,2623,22724,328,3429,2085,24036,
     22831,4342,4645,4647,4849,15417,5554,5856,5100,4600,
     20,0,6157,6458,7359,7460,6561,7164,7265,6866,
     26967,6268,6369,6871,6972,95,9335,8176,8277,6281,
     26382,1191,9792,29293,9894,9596,00,9899,9897,9798,
     320201,302,504,20807,1110,1211,1413,1514,22019,1716,
     42322,22423,22625,35,2829,2930,2931,436,3637,3738,
     53839,5342,4443,4644,4445,4446,4548,4350,4200,4700,
     529026,0,8890,0,6055,5456,5958,5559,6160,7062/
      DATA IXT1/6963,7364,6765,7166,8368,6769,6370,7671,6772,7573,
     56574,7475,7277,9192,9693,21,9695,96,9497,9898,
     6104,205,407,20609,1013,1114,6295,1316,11417,1518,
     71922,22023,2124,2225,2326,22427,8520,3228,3229,3640,
     83033,4344,4847,5150,4251,11754,5455,5658,4300,4800,
     80,0,5761,8162,8263,5864,6165,6466,6567,7168,
     87269,8370,6471,6572,5973,6074,3593,3535,7681,7782,
     82488,8889,9111,0,0,9394,9799,9396,9897,9898,
     920102,203,405,20708,1011,1112,1314,1415,21920,1617,
     12223,22324,22526,2928,3330,2932,9766,3604,3736,3837/
      DATA IXT2/3938,5043,4845,5247,4948,5149,4950,4951,4400,4900,
     222690,9088,0,5955,5457,5859,5560,6061,6862,7063,
     26664,7465,7267,6668,8369,6270,6671,7772,6473,7574,
     27375,7176,9291,29392,9493,9794,9895,9835,9997,9898,
     31203,1304,1607,1809,2112,2213,2314,3128,3129,2832,
     40,4039,3941,4544,4746,4748,5051,4752,4500,5000,
     50,0,0,753,8687,00,8499,9884,2934,0,
     600,0,9933,3839,3738,3637,3130,0,0,0,
     78990,0,0,0,0,0,0,0,0,0,
     80,0,0,0,0,0,0,0,0,0/
      DATA IXT3/312,413,716,918,1221,1322,1423,2803,3028,3029,
     72830,3031,4139,3940,5142,4243,4546,5049,4253,5307,
     89984,00,00,0,0,00,00,00,5100,5100,
     90,1987,3399,3938,3837,3736,0,0,0,0,
     100,00,00,00,00,00,00,0,0,0,
     89089,0,0,0,0,0,0,0,0,0/
C
C
C
C
C=====C                       MM-6
C 7-M C
C=====C
C
C kategorie hracu PITIFUL    POOR       UNLUCKY    NOVICE
C
      DATA CCLS/'ßalostnÏ','ubohÏ   ','smola˝  ','nov†üek ',
     1'sluÁnÏ  ','amatÇr  ','dobrÏ   ','Át°stko ',
     3'obratnÏ ','£ßasnÏ  ','mistr   ','superman'/
C
C
C
C               IR ISC ISCP IFC ITN IBAT1 IBAT2 MRM MIN MWP
C
      DATA ITST/93, 45,999, 1000, 0,   0,    0,  0,  3,  0,
     :         6,    0,   0, 93, 17,     0,     0,  0,    0,   0,
     :     0,     0,     0,    0,   1,    0,   0,    0,   29,   12,
     :       0,   0,   0,   0,    0,   0,   0,  'MM',       0,    0/
C
      DATA NAME/'MM','SA','  '/
      DATA NTAG/'V†','ße','nÏ',' n','†v','Át','ÿv','n°','ku','! '/
      DATA KLICE
     :/'RO','ZH','Lê','DN','OU','T ','SE',8*'  ',
     : 'PO','SL','OU','CH','AT',10*'  ',
     : 'SK','‡R','E ',12*'  ',
     : 'PO','MO','C ',12*'  ',
     : 'KO','NE','C ',12*'  ',
     : 'KO','PA','T ',12*'  ',
     : 'ZA','PS','AT',12*'  ',
     : 'ZA','ZN','AM','EN','AT',' K','Lí','¨E',7*'  '/
      DATA IDELKY,IDENTK /8*9,'1 ','2 ','3 ','4 ','5 ',
     : '6 ','7 ','8 '/
      END
C
C
C
C
C=====C                       MM-7
C 8-M C
C=====C
C
      SUBROUTINE MMRI(IV,IRS)
C    :,10-02-1983 MMM    8536E D45 PL&BM <850920.1755>
      character*14 f110
      character*10 f111
      character*26 f
$INCLUDE:'MMCO.FOR'
C
C *** FUNKCE ***
C
      IFSD(I)=I-(I/100)*100
      IFD(I)=I/1000-(I/10000)*10
C
C ******* SUBROUTINE I *******
C
C *** PROMENNE ***
C
      LU=IPR(1)
      IR=ITST(1)
      LX=ITST(21)
C
   50 IC=0
      DO 1000 J=1,53
C
C     IV = 1 - seznam veci na miste ITST(1)
C
      IF(IV.EQ.1.AND.(IVEN(J).GT.10000.OR.IVEN(J)-100*(IVEN(J)/100)
     1.NE.IR))GOTO 1000
C
C     IV = 2 - vybava
C
      IF(IV.EQ.2.AND.IVEN(J).LT.20000)GOTO 1000
C
C     IV = 4 - popis veci IRS
C
      IF(IV.EQ.4.AND.IRS.NE.J)GOTO 1000
C
C     IV = 3 - vse, co ma osoba IRS
C
      IF(IV.EQ.3.AND.(IVEN(J).LT.10000.OR.IVEN(J).GT.20000.OR.
     1IFSD(IVEN(J)).NE.IRS))GOTO 1000
C
C     IV = 5 - debug, vypis vseho po trech na radek
C
C
C
C *** preskok veci popsanych v popisu mista ***
C
C
C     lucerna na haku
C
      IF(IV.EQ.1.AND.J.EQ.4.AND.(IVEN(4).EQ.493.OR.IVEN(4).EQ.1493))
     1GOTO 1000
C
C     kriz na zdi
C
      IF(IV.EQ.1.AND.J.EQ.5.AND.IVEN(5).EQ.1316)GOTO 1000
C
C     amulet ve sluji
C
      IF(IV.EQ.1.AND.J.EQ.17.AND.IVEN(17).EQ.1141)GOTO 1000
C
C     svicka v kapli
C
      IF(IV.EQ.1.AND.J.EQ.19.AND.IR.EQ.16)GOTO 1000
C
C     klin v herne
C
      IF(IV.EQ.1.AND.J.EQ.29.AND.IVEN(29).EQ.1214)GOTO 1000
C
C     globus v knihovne
C
      IF(IV.EQ.1.AND.J.EQ.30.AND.IR.EQ.25)GOTO 1000
C
C     smaragd v bludisti
C
      IF(IV.EQ.1.AND.J.EQ.37.AND.IVEN(37).EQ.1153)GOTO 1000
C
C     cise v bludisti
C
      IF(IV.EQ.1.AND.J.EQ.39.AND.IVEN(39).EQ.1253)GOTO 1000
C
C     diamant v bludisti
C
      IF(IV.EQ.1.AND.J.EQ.40.AND.IVEN(40).EQ.1151)GOTO 1000
C
C     krouzek v bludisti
C
      IF(IV.EQ.1.AND.J.EQ.42.AND.IVEN(42).EQ.1151)GOTO 1000
C
C     klicek od vezeni
C
      IF(IV.EQ.1.AND.J.EQ.46.AND.IVEN(46).EQ.1101)GOTO 1000
C
C     mapa v chaloupce
C
      IF(IV.EQ.1.AND.J.EQ.28.AND.IVEN(28).EQ.1184)GOTO 1000
C
C     *** IV=1, popis mista
C
      IF(IV.EQ.1.AND.IC.EQ.0) THEN
         CALL REIO(2,LU,'  ',1)
         CALL REIO(2,LU,' Vid°te:',4)
      ENDIF
C
C     *** IV=2, ve vybave nevypisujem saty
C
  110 F110='(I3,I6,'' ''\)'
  111 F111='(I6,'' ''\)'
      IF(IV.EQ.2.AND.J.EQ.53)GOTO 1000
      IF(IV.EQ.5.AND.(J-1)/3.-INT((J-1)/3.).EQ.0)WRITE(LU,f110)J,IVEN(J)
      IF(IV.EQ.5.AND.(J-1)/3.-INT((J-1)/3.).NE.0)WRITE(LU,f111)IVEN(J)
C
  210 F='('' up°rÖv prsten     ''\)'
      IF(J.EQ.1)WRITE(LU,f)

C
  220 F='('' svazek kl°üÖ      ''\)'
      IF(J.EQ.2)WRITE(LU,f)
C
  230 F='('' hodinky           ''\)'
      IF(J.EQ.3)WRITE(LU,f)
C
  240 F='('' bateriovou lucernu''\)'
      IF(J.EQ.4)WRITE(LU,f)
C
  250 F='('' st˝°brnÏ k˝°ßek   ''\)'
      IF(J.EQ.5)WRITE(LU,f)
C
  260 F='('' kapesn° kompas    ''\)'
      IF(J.EQ.6)WRITE(LU,f)
C
  270 F='('' pokladnu          ''\)'
      IF(J.EQ.7)WRITE(LU,f)
C
  280 F='('' novou baterii     ''\)'
      IF(J.EQ.8)WRITE(LU,f)
C
  290 F='('' pistoli           ''\)'
      IF(J.EQ.9)WRITE(LU,f)
C
C
C=====C                       MM-8  (LINE 640)
C 9-M C
C=====C
C
  300 F='('' ˝eznickÏ nÖß      ''\)'
      IF(J.EQ.10)WRITE(LU,f)
C
  310 F='('' oboustrannou sekeru''\)'
      IF(J.EQ.11)WRITE(LU,f)
C
  320 F='('' meü               ''\)'
      IF(J.EQ.12)WRITE(LU,f)
C
  330 F='('' ampuli jedu       ''\)'
      IF(J.EQ.13.AND.ITST(10).NE.13)WRITE(LU,f)
C
  335 F='('' pr†zdnou ampuli   ''\)'
      IF(J.EQ.13.AND.ITST(10).EQ.13)WRITE(LU,f)
C
  340 F='('' kyj               ''\)'
      IF(J.EQ.14)WRITE(LU,f)
C
  350 F='('' c°vku lana        ''\)'
      IF(J.EQ.15)WRITE(LU,f)
C
  360 F='('' d˝evÿnou ßidli    ''\)'
      IF(J.EQ.16)WRITE(LU,f)
C
  370 F='('' amulet            ''\)'
      IF(J.EQ.17)WRITE(LU,f)
C
  380 F='('' starÏ talisman    ''\)'
      IF(J.EQ.18)WRITE(LU,f)
C
  390 F='('' sv°t°c° sv°üku    ''\)'
      IF(J.EQ.19.AND.IFD(IVEN(19)).EQ.1)WRITE(LU,f)
C
  395 F='('' sv°üku            ''\)'
      IF(J.EQ.19.AND.IFD(IVEN(19)).NE.1)WRITE(LU,f)
C
  400 F='('' st˝°brnou kulku   ''\)'
      IF(J.EQ.20)WRITE(LU,f)
C
  410 F='('' knihu             ''\)'
      IF(J.EQ.21)WRITE(LU,f)
C
  420 F='('' l°stek            ''\)'
      IF(J.EQ.22)WRITE(LU,f)
C
  430 F='('' vys°laü hmoty     ''\)'
      IF(J.EQ.23)WRITE(LU,f)
C
  440 F='('' trochu j°dla      ''\)'
      IF(J.EQ.24)WRITE(LU,f)
C
  450 F='('' d˝evÿnÇ koÁtÿ     ''\)'
      IF(J.EQ.25)WRITE(LU,f)
C
  460 F='('' p˝ij°maü hmoty    ''\)'
      IF(J.EQ.26)WRITE(LU,f)
C
  470 F='('' ho˝°c° pochodeÂ   ''\)'
      IF(J.EQ.27.AND.IFD(IVEN(27)).EQ.1)WRITE(LU,f)
C
  475 F='('' doho˝elou pochodeÂ''\)'
      IF(J.EQ.27.AND.IFD(IVEN(27)).NE.1)WRITE(LU,f)
C
  480 F='('' cestovn° mapu     ''\)'
      IF(J.EQ.28)WRITE(LU,f)
C
  490 F='('' d˝evÿnÏ kl°n      ''\)'
      IF(J.EQ.29)WRITE(LU,f)
C
  500 F='('' zapr†Áenou kouli  ''\)'
      IF(J.EQ.30)WRITE(LU,f)
C
  510 F='('' kuchyÂskÇ z†palky ''\)'
      IF(J.EQ.31)WRITE(LU,f)
C
  520 F='('' rezavou lopatku   ''\)'
      IF(J.EQ.32)WRITE(LU,f)
C
C======C                      MM-9  (LINE 720)
C 10-M C
C======C
C
  530 F='('' malou olejniüku   ''\)'
      IF(J.EQ.33)WRITE(LU,f)
C
  540 F='('' mastnou sekyrku   ''\)'
      IF(J.EQ.34)WRITE(LU,f)
C
  550 F='('' z†hadnou truhlu   ''\)'
      IF(J.EQ.35)WRITE(LU,f)
C
  560 F='('' s†üek perel       ''\)'
      IF(J.EQ.36)WRITE(LU,f)
C
  570 F='('' velkÏ smaragd     ''\)'
      IF(J.EQ.37)WRITE(LU,f)
C
  580 F='('' tucet minc°       ''\)'
      IF(J.EQ.38)WRITE(LU,f)
C
  590 F='('' st˝°brnou ü°Ái    ''\)'
      IF(J.EQ.39)WRITE(LU,f)
C
  600 F='('' nÿkolik diamantÖ  ''\)'
      IF(J.EQ.40)WRITE(LU,f)
C
  610 F='('' ryz° klenoty      ''\)'
      IF(J.EQ.41)WRITE(LU,f)
C
  620 F='('' rub°novÏ kroußek  ''\)'
      IF(J.EQ.42)WRITE(LU,f)
C
  630 F='('' klenotn° korunu   ''\)'
      IF(J.EQ.43)WRITE(LU,f)
C
  640 F='('' h†zec° Áipky      ''\)'
      IF(J.EQ.44)WRITE(LU,f)
C
  650 F='('' ocelovÇ brnÿn°    ''\)'
      IF(J.EQ.45)WRITE(LU,f)
C
  660 F='('' kl°üek od vÿzen°  ''\)'
      IF(J.EQ.46)WRITE(LU,f)
C
  670 F='('' starou baterii    ''\)'
      IF(J.EQ.47)WRITE(LU,f)
C
  680 F='('' dlouhÇ rukavice   ''\)'
      IF(J.EQ.48)WRITE(LU,f)
C
  690 F='('' barevnÇho papouÁka''\)'
      IF(J.EQ.49)WRITE(LU,f)
C
  700 F='('' hrozivÏ palc†t    ''\)'
      IF(J.EQ.50)WRITE(LU,f)
C
  710 F='('' mezery            ''\)'
      IF(J.EQ.51)WRITE(LU,f)
C
  720 F='('' mezery            ''\)'
      IF(J.EQ.52)WRITE(LU,f)
C
  730 F='('' svoje Áaty        ''\)'
      IF(J.EQ.53)WRITE(LU,f)
C
  900 F='('' '')'
      IF((IV.NE.5.OR.J/3.-INT(J/3.).EQ..0).AND.LU.NE.6)WRITE(LU,f)
      IC=1
 1000 CONTINUE
 1010 F='('' pr†zdno'')'
      IF((IV.EQ.2.OR.IV.EQ.3).AND.IC.EQ.0)WRITE(LU,f)

      IF(LX.EQ.0.OR.LU.EQ.LX)RETURN
      LU=LX
      GOTO 50
C
      END
C
C
C
      SUBROUTINE MMRL
C     ,10-02-1983 MMM    8536E D45 PL&BM <850920.1755>
$INCLUDE:'MMCO.FOR'
C
C *** FUNCTION
C
      IFSD(I)=I-(I/100)*100
      IFD(I)=I/1000-(I/10000)*10
C
C ******* SUBROUTINE L *******
C
C
C *** nastaveni promennych ***
C
C     LU=IPR(1)
      LU=0
      IR=ITST(1)
C
C
C *** zjistuje svetelne podminky ***
C
C ** je zde svetelny zdroj **
C
C
C * lucerna, svicka nebo pochoden na miste nebo u osoby *
C
      DO 1153 I=1,3
C
      IF(I.EQ.1)K=4
      IF(I.EQ.2)K=19
      IF(I.EQ.3)K=27
C
C     sviti a je zde nebo ji ma hrac nebo osoba, ktera je zde
C
      IF(IFD(IVEN(K)).EQ.1.AND.((IVEN(K).LT.10000.AND.IFSD(IVEN(K))
     1.EQ.IR).OR.IVEN(K).GT.20000.OR.(IVEN(K).GT.10000.AND.IVEN(K)
     2.LT.20000.AND.IFSD(IRES(IFSD(IVEN(K)))).EQ.IR))) RETURN
C
 1153 CONTINUE
C
C * olejova lampa v obyvacim pokoji *
C
      IF ((IR .EQ. 10) .AND. (IVEN(76) .GT. 1000)) RETURN
C
C * krb *
C
      IF ((IR .EQ. 14) .AND. (IVEN(89) .GT. 1000)) RETURN
C
C * sirky *
C
      IF (IVEN(31) .GT. 21000) RETURN
C
C ** je venku a je den **
C
      IF((IR .GT. 53) .AND. (ITST(5) .LT. 300)) RETURN
      IF (IR .GT. 53) CALL SGLOD(NAME,LU,1150,1,0,0)
C
C ** jsou rozhrnute zaclony **
C
      IF ((IROM(IR) .GT. 20000) .AND. (ITST(5) .LT. 300)) RETURN
      IF (IROM(IR) .GT. 20000) CALL SGLOD(NAME,LU,1150,1,0,0)
      IF((ITST(5) .LT. 300) .AND. (IROM(IR) .GT. 10000))
     1CALL SGLOD(NAME,LU,1150,2,0,0)
C       ! zaclony rozhrnout
      CALL SGLOD(NAME,LU,1150,0,0,0)
C
C
C
      END
      INTEGER*2 FUNCTION MMPRP(IR)
C     ,18-02-1983 MMM    8536E D45 PL&BM <850920.1755>
C
$INCLUDE:'MMCO.FOR'
C
C
      IC=1
C       ! predlozka 'v'
      IIR=IROM(IR)
      IIR=IIR-(IIR/100)*100
      IF (IIR.EQ.4.OR.
     1    IIR.EQ.18.OR.IIR.EQ.31.OR.IIR.EQ.35.OR.
     1    IIR.EQ.38.OR.IIR.EQ.41.OR.IIR.EQ.45.OR.
     2    IIR.EQ.47.OR.IIR.EQ.49.OR.
     3    IIR.EQ.53) IC=2
C         ! predlozka 'na'
      IF (IIR.EQ.32.OR.IIR.EQ.33.OR.IIR.EQ.36.OR.
     1    IIR.EQ.37.OR.IIR.EQ.42.OR.IIR.EQ.48.OR.
     2    IIR.EQ.50) IC=3
C      !'u'
      IF (IIR.GE.6.AND.IIR.LE.9.OR.IIR.EQ.11.OR.
     1    IIR.EQ.20.OR.IIR.EQ.22.OR.IIR.EQ.39) IC=14
C      ! 've'
      MMPRP=IC
      RETURN
      END
*
*
      LOGICAL*2 FUNCTION PROB(PRV)
C    :,06-09-1985 MMM    8536E D45 PL <850920.1755>
      REAL*4 URAN,PRV
      PROB=(URAN() .LT. PRV)
      END
*
*
      INTEGER*2 FUNCTION NORM(KOLIK)
C    :,06-09-1985 MMM    8536E D45 PL <850920.1755>
      REAL*4 URAN
      INTEGER*2 KOLIK,pom
      pom=INT(URAN( )*KOLIK+0.5)
      if(pom.lt.1) pom=1
      if(pom.gt.kolik) pom=kolik
      norm=pom
      END
*
*
      subroutine sseed(im)
      real*4 base
      common /random/base
      base=im
      return
      end
*
*
      real*4 function uran ( )
      integer*2 x(4)
      real*4 base,org
      real*8 pom
      common /random/base
      equivalence (pom,x(1))
      org=base
      pom=base*1220703125
      if(x(3).lt.0) x(3)=-(x(3)+1)
      base=x(3)/32768.0
      if(base.lt.0.0) base=0.0
      if(base.gt.1.0) base=1.0
      uran=base
      return
      end