$storage:2
$large
$nodebug
C
      subroutine mmsk(p1,p2,p3,p4,p5)
c      ,10-02-1983 MMM    8536E D45 PL&BM <850906.1746>
C     04-02-1988 MS-DOS S.M.
      integer*2 p1,p2,p3,p4,p5
      character*127 f
$INCLUDE:'MMCO.for'
C
C ******* SEGMENT K *******
C
C  Tento segment pise zpravy pro segment MMSJ a vraci se do MMSB
C
C *** funkce ***
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
C                            MM-K-1  (LINE 9200)
C 2-K
C
C *** ukoncujici pokecy ***
C
  999 continue
  320 F='(/'' Nen¡ mi jasn‚, co se pokou¨¡te ©¡ct.'')'
      IF (IRIT .EQ. 320) WRITE(LU,f)
C
  520 F='(/'' Nic se nedˆje.'')'
      IF (IRIT .EQ. 520) WRITE(LU,f)
C
 1000 F='(/'' Nejsem ‡ten © my¨lenek, mus¡te ©¡ct, co chcete '',4a2)'
      IF (IRIT.EQ.1000) WRITE(LU,f)(IWRD(L,1),L=1,4)
C
 1003 F='(/'' Mus¡te mi ©¡ct, koho chcete zast©elit.'')'
      IF (IRIT .EQ. 1003) WRITE(LU,f)
C
 1040 F='(/'' V po© dku'')'
      IF (IRIT .EQ. 1040) WRITE(LU,f)
C
 1065 F='(/'' PROGRAM ERROR AT LINE''I6)'
      IF (IRIT .EQ. 1065) WRITE(LU,f) IPR(3)
C
      IF (IPR(3).EQ.7.OR.IPR(3).EQ.16.OR.IPR(3).EQ.25)IC=IDS
      IF (IPR(3).EQ.5.OR.IPR(3).EQ.14.OR.IPR(3).EQ.23)IC=IDW
      IF (IPR(3).EQ.1.OR.IPR(3).EQ.10.OR.IPR(3).EQ.19)IC=IDN
 1095 F='(/'' Otev©el se tajn˜ vchod, do kter‚ho m–‘ete vej¡t ''4A2)'
      IF (IRIT .EQ. 1095) WRITE(LU,f)(IDTN(I,IC),I=1,4)
C
 1150 F='(/'' Je zde tma'')'
      IF (IRIT.EQ.1150) WRITE(LU,f)
C
 1151 F='(/'' Slunce ji‘ zapadlo'')'
      IF ((IRIT.EQ.1150) .AND. (IPR(3).EQ.1)) WRITE(LU,f)
C
 1152 F='(/'' Z clony br n¡ svˆtlu'')'
      IF ((IRIT.EQ.1150) .AND. (IPR(3).EQ.2)) WRITE(LU,f)
C
 2010 F='(/'' Nem–‘ete zamykat truhlu.'')'
      IF (IRIT .EQ. 2010) WRITE (LU,f)
C
C ???
 2020 F='(/''    ''4A2'' ji‘ m te.'')'
      IF (IRIT .EQ. 2020) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 2025 F='(/''    ''4A2'' toto nem–‘e udˆlat.'')'
      IF (IRIT .EQ. 2025) WRITE(LU,f)(IWRD(L,4),L=1,4)
C
 2030 F='(/''    ''4A2'' nikde nevid¡m.'')'
      IF (IRIT .EQ. 2030) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 3010 F='(/''    ''4A2'' nem te.'')'
      IF (IRIT .EQ. 3010) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 3110 F='(/''    ''4A2'' ji‘ nebudete m¡t.'')'
      IF (IRIT .EQ. 3110) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
C ???
 4010 F='(/''    ''4A2'' mus¡ b˜t nejprve V mi nalezen.'')'
      IF (IRIT .EQ. 4010) WRITE(LU,f)(IWRD(L,4),L=1,4)
C
 4050 F='(/'' D¡ky. Pot©ebovalo to tady.'')'
      IF (IRIT.EQ.4050) WRITE(LU,f)
C
 5020 F='(/''    ''4A2'' zde nen¡.'')'
      IF (IRIT .EQ. 5020) WRITE(LU,f)(IWRD(L,3),L=1,4)
C
 6010 F='(/'' Nemus¡te d vat pozor.'')'
      IF (IRIT.EQ.6010) WRITE(LU,f)
C
 6020 F='(/'' Podlaha pod V mi se slabˆ pohnula, jako by amulet nˆco akt
     1ivoval.'')'
      IF (IRIT.EQ.6020) WRITE(LU,f)
C
 7010 F='(/'' Asi jste ponˆkud perverzn¡.'')'
      IF (IRIT.EQ.7010) WRITE(LU,f)
C
 7020 F='(/''    ''4A2'' ©¡k : "Zde na to nen¡ vhodn‚ m¡sto."'')'
      IF (IRIT.EQ.7020) WRITE(LU,f)(IRSN(L,IWRD(6,3)),L=1,4)
C
 7021 F='(/''    ''4A2'' ©¡k : "A‘ se osprchujete."'')'
      IF (IRIT.EQ.7021) WRITE(LU,f)(IRSN(L,IWRD(6,3)),L=1,4)
C
 7022 F='(/'' Co kdyby nˆkdo p©i¨el, ©¡k  ''4A2)'
      IF (IRIT.EQ.7022) WRITE(LU,f)(IRSN(L,IWRD(6,3)),L=1,4)
C
 7023 F='(/''    ''4A2'' ©¡k : Nechcete si udˆlat trochu pohodl¡!?'')'
      IF (IRIT.EQ.7023) WRITE(LU,f)(IRSN(L,IWRD(6,3)),L=1,4)
C
 7024 F='(/'' Je zde svˆtlo, ©¡k  ''4A2)'
      IF (IRIT.EQ.7024) WRITE(LU,f)(IRSN(L,IWRD(6,3)),L=1,4)
C
 7030 F='(/''    ''4A2'' ©¡k : Ne teƒ. Nˆkdo tady je.'')'
      IF (IRIT.EQ.7030) WRITE(LU,f)(IRSN(L,IWRD(6,3)),L=1,4)
C
      IF (IRIT.EQ.7040) WRITE(LU,7040)(IRSN(L,IWRD(6,3)),L=1,4),
     1 (IRSN(L,IWRD(6,3)),L=1,4)
C
 7040 FORMAT(/' Za‡¡n te se c¡tit vzru¨en, kdy‘ vid¡te, jak se ',
     1 4A2' svl‚k ; pak si V s'/' dychtivˆ odvle‡e do postele. ',
     2' V ¨nivˆ se mazl¡te a @.@.@.@.@.@.@.@.'///' O hodin',
     3'u pozdˆji, po mnoha vzru¨uj¡c¡ch z ‘itc¡ch, vid¡te, jak se '4A2/
     4' obl‚k  a vdˆ‡nˆ se na V s usm¡v .')
C
C                            MM-K-2  (LINE 9280)
C 3-K
C
 7050 F='('' Nic'')'
      IF (IRIT .EQ. 7050) WRITE(LU,f)
C
 7510 F='(/'' Nem te tady pohodl¡.'')'
      IF (IRIT.EQ.7510) WRITE(LU,f)
C
 7530 F='(/'' Nˆkdo v m¡stnosti V s bud¡.'')'
      IF (IRIT.EQ.7530) WRITE(LU,f)
C
 7540 F='(/'' Hodinku jste si zd©¡ml a vstal jste osvˆ‘en.'')'
      IF (IRIT.EQ.7540) WRITE(LU,f)
C
 8010 F='(/'' Nev¡m, co o‡ek v te, ‘e se stane.'')'
      IF (IRIT .EQ. 8010 ) WRITE(LU,f)
C
C ???
 8015 F='(/''    ''4A2'' V m snˆdl j¡dlo.'')'
      IF (IRIT .EQ. 8015) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 8020 F='(/'' U‘ je.'')'
      IF (IRIT .EQ. 8020) WRITE(LU,f)
C
 8030 F='(/''    ''4A2'' zde nevid¡m.'')'
      IF (IRIT .EQ. 8030) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
 8031 F='(/''    ''4A2'' zde nen¡.'')'
      IF (IRIT .EQ. 8031) WRITE(LU,f)(IRSN(L,IWRD(6,3)),L=1,4)
C
15030 F='(/'' Naproti V m nejsou dve©e.'')'
      IF (IRIT .EQ. 15030) WRITE(LU,f)
15020 F='(/'' Dve©e proti V m nemaj¡ z mek.'')'
      IF (IRIT.EQ.15020) WRITE(LU,f)
C
C
15040 F='(/'' Pot©ebujete kl¡‡.'')'
      IF (IRIT .EQ. 15040) WRITE(LU,f)
C
15041 F='(/'' Pot©ebujete kl¡‡ek od vˆzen¡.'')'
      IF (IRIT.EQ.15041) WRITE(LU,f)
C
15110 F='(/'' Kl¡‡e nepasuj¡ do z mku.'')'
      IF (IRIT.EQ.15110) WRITE(LU,f)
C
16010 F='(/'' Pot©ebujete pistoli, abyste mohl st©¡let.'')'
      IF (IRIT .EQ. 16010) WRITE(LU,f)
C
16020 F='(/'' Do¨ly V m kulky.'')'
      IF (IRIT .EQ. 16020) WRITE(LU,f)
C
16030 F='(/'' Netrefil jste se.'')'
      IF (IRIT .EQ. 16030) WRITE(LU,f)
C
16031 F='(/'' Zranil jste ''4A2)'
      IF (IRIT.EQ.16031) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
16035 F='(/'' Rozml til jste z mek na kousky.'')'
      IF (IRIT.EQ.16035) WRITE(LU,f)
      IF (IRIT.EQ.16035)IVEN(35)=IVEN(35)+1000
16040 F='(/'' Jen jste vypl˜tval n boj. ''4A2'' nen¡ vhodn˜ ter‡.'')'
      IF (IRIT .EQ. 16040) WRITE(LU,f)(IRSN(L,IWRD(6,2)),L=1,4)
C
16045 F='(/'' Zabil jste sk©eta, kter˜ zmizel v oblaku dusiv‚ho ‡ern‚ho
     1d˜mu.'')'
      IF (IRIT.EQ.16045) WRITE(LU,f)
C
16046 F='(/'' Kulka se odrazila od brnˆn¡ ryt¡©e.'')'
      IF (IRIT.EQ.16046) WRITE(LU,f)
C
      IF (IRIT.EQ.16045)IRES(13)=IRES(13)+100
16050 F='(/'' Strefil jste se a zabil jste ''4A2)'
      IF (IRIT .EQ. 16050) WRITE(LU,f)(IWRD(L,2),L=1,4)
C
20010 F='(/'' Pot©ebujete olejni‡ku nebo nˆco podobn‚ho.'')'
      IF (IRIT .EQ. 20010) WRITE(LU,f)
C
20020 F='(/'' Olejni‡ka je pr zdn .'')'
      IF (IRIT .EQ. 20020) WRITE(LU,f)
C                            MM-K-3  (LINE 9360)
C 4-K
C
20030 F='(/'' Nebuƒte hloup˜.'')'
      IF (IRIT .EQ. 20030) WRITE(LU,f)
C
*      IF (IRIT.EQ.21020) WRITE(LU,21020)ITST(21)
*21020 FORMAT(/' Tento radek a kazdy dalsi bude zaznamenan na LU #'I3)
C
22010 F='(/'' Zde nejde rozb¡t z mek truhly.'')'
      IF (IRIT.EQ.22010) WRITE(LU,f)
C
22020 F='(/'' Nem te nic, ‡¡m byste rozbil z mek.'')'
      IF (IRIT.EQ.22020) WRITE(LU,f)
C
      IF (LU.NE.IPR(1).OR.ITST(21).EQ.0) GOTO 9999
      LU=ITST(21)
      GOTO 999
 9999 NAME(2)='SB'
c      CALL SGLOD(NAME,LU)
      return
      END
