$storage:2
$large
$nodebug
      subroutine mmsc(p1,p2,p3,p4,p5)
c      ,10-02-1983 MMM    8536E D45 PL&BM <850920.1827>
C     04-02-1988 MS-DOS S.M.
C
C ******* SEGMENT C *******
C
C      Tento segment zpracovava slovesa typu jit. Normalne je ukoncen
C      segmentem MMSD.
C
      integer*2 p1,p2,p3,p4,p5
      character*127 f
      LOGICAL FOLLOW
$INCLUDE:'MMCO.for'
      DIMENSION MSG(45),IMZ(3),MSGR(45),ITIM(5)
C
C *** FUNCTIONS ***
C
      IFSD(I)=I-(I/100)*100
      ITFD(I)=I/100-(I/10000)*100
      ITD(I)=I/100-(I/1000)*10
      IFD(I)=I/1000-(I/10000)*10
C
C *** nastaveni promennych ***
C
      ipr(1)=p1
      ipr(2)=p2
      ipr(3)=p3
      ipr(4)=p4
      ipr(5)=p5
      LU=IPR(1)
      LX=ITST(21)
      IR=ITST(1)
      p1=lu
      p2=0
      p3=0
      p4=0
      p5=0
C
C ****** INDEX ******
C
C        SUBJECT                                                PAGE
C        A. GO TYPE VERBS                                       C-2
C           1. CLIMB                                            C-2
C           2. ENTER AND MAGIC WORDS                            C-3
C           3. DIG AND FOLLOW                                   C-4
C           4. GO                                               C-5
C        B. ROOM DEPENDENT ITEMS                                C-6
C           1. FALL NOTE AND WOODS                              C-6
C           2. MOLE MAZE AND CORRIDOR                           C-7
C           3. MEADOW, PASSAGE ENTRANCES,HIGHWAY, AND DEMONS    C-8
C           4. MOVE INHABITANTS AND MYSTERY SOLUTION            C-9
C           5. LIGHT,SCORE, AND ROOM                            C-10
C           6. WHAT, WHO, WAY, AND DESCRIPTION                  C-11
C                            MM-C-1  (LINE 2240)
C 2-C
C
C ****** GO TYPE VERBS ROUTINE ******
C
C ***** nastaveni ukoncujiciho segmentu *******
C
      NAME(2)='SD'
C
C ** magicka slova po 18. hod nefunguji **
C
      IF (ITST(5).GT.300.AND.IWRD(6,1).GT.3.AND.IWRD(6,1).LT.12) then
c     1 CALL SGLOD(NAME,LU,290)
          p2=290
          return
      endif
C
C *** podivat ****
C
      IF (IWRD(7,1).EQ.1.AND.IWRD(6,1).EQ.25) GOTO 1142
C
C *** ostatni slovesa ***
C
      GOTO(1000,300,400,600,500,700,600,600,500,700,600,900,970,950)
     1IWRD(6,1)
C
C **** splhani ****
C
C *** splhani nahoru ***
C
  300 IF (IWRD(7,1).NE.1) GOTO 310
      IWRD(7,1)=2
      IWRD(5,2)=2
      IWRD(6,2)=5
      GOTO 1000
C
C *** vyjmuti predlozek ***
C
  310 IF (IWRD(7,1).EQ.2) GOTO 330
      IF ((IWRD(7,1).GT.3).OR.(IWRD(5,2).NE.5).OR.
     1(IWRD(6,2).NE.2)) then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C  splhat na -> splhat
      IWRD(5,2)=IWRD(5,3)
      IWRD(6,2)=IWRD(6,3)
      IWRD(7,1)=IWRD(7,1)-1
C
C*** splhani na zed ***
C
  330 IF (IWRD(5,2).NE.3) then
c         CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
      IF (IVEN(15).LT.20000.OR.IWRD(6,2).NE.77.OR.(IR.NE.93.AND.
     1IR.NE.94.AND.IR.NE.95.AND.IR.NE.96.AND.IR.NE.70.AND.IR.NE.66.AND.
     2IR.NE.67.AND.IR.NE.92)) GOTO 335
      IRB=ITST(1)
      ITST(14)=IRB
      IF (ITST(1).EQ.93)IR=92
      IF (ITST(1).EQ.94)IR=66
      IF (ITST(1).EQ.95)IR=70
      IF (ITST(1).EQ.96)IR=67
      IF (ITST(1).EQ.67)IR=96
      IF (ITST(1).EQ.70)IR=95
      IF (ITST(1).EQ.66)IR=94
      IF (ITST(1).EQ.92)IR=93
      ITST(1)=IR
      GOTO 1141
  335 IF (IWRD(6,2).EQ.77.AND.(IR.EQ.66.OR.IR.EQ.67.OR.IR.EQ.70.OR.
     1IR.EQ.92.OR.IR.EQ.93.OR.IR.EQ.94.OR.IR.EQ.95.OR.IR.EQ.96)) then
c     2 CALL SGLOD(NAME,LU,340)
          p2=340
          return
      endif
      IF (IWRD(6,2).EQ.77) then
c      CALL SGLOD(NAME,LU,350)
          p2=350
          return
      endif
C
C *** Splhani na strom ***
C
      IF ((IWRD(6,2).EQ.78).AND.(IR.NE.97).AND.
     1(IR.NE.98)) then
c         CALL SGLOD(NAME,LU,360)
          p2=360
          return
      endif
      IF (IWRD(6,2).EQ.78) then
c      CALL SGLOD(NAME,LU,370)
          p2=370
          return
      endif
C
C *** Splhani na branu ***
C
      IF (IWRD(6,2).EQ.93.AND.IR.NE.93) then
c      CALL SGLOD(NAME,LU,390)
          p2=390
          return
      endif
      IF (IWRD(6,2).EQ.93) then
c      CALL SGLOD(NAME,LU,385)
          p2=385
          return
      endif
c     CALL SGLOD(NAME,LU,320)
      p2=320
      return
C
C
C=====C                      MM-C-2  (LINE 2320)
C 3-C C
C=====C
C
C
C **** ENTER - vstoupit ***
C
  400 IF (IWRD(7,1).GT.1.AND.IWRD(5,3).NE.3.AND.IWRD(6,3).NE.93.AND.
     1  IWRD(5,2).NE.5.AND.IWRD(6,2).NE.15) then
c     2  CALL SGLOD(NAME,LU,320)   ! VSTOUPIT DO BRANY
          p2=320
          return
      endif
      IWRD(7,1)=2
      IWRD(6,1)=1
      IWRD(5,2)=2
      IF (IR.EQ.93.OR.IR.EQ.91)IWRD(6,2)=1
      IF (IR.EQ.95)IWRD(6,2)=3
      IF (IWRD(6,2).NE.0) GOTO 1000
c     CALL SGLOD(NAME,LU,410)
      p2=410
      return
C
C **** MAGICKA SLOVA ****
C
C *** JESTLI JE V ZAHRADE, TAK PRESUN NA VERANDU ***
C
  500 IF (IR.EQ.17) GOTO 800
      IF ((IR.LT.55).OR.(IR.GT.83)) then
c      CALL SGLOD(NAME,LU,520)
          p2=520
          return
      endif
      ITST(1)=54
      IR=54
      WRITE(LU,720)
      GOTO 1141
C
C *** PRESUN DO LESU ODJINUD ***
C
  600 IF (IR.EQ.98) then
c  CALL SGLOD(NAME,LU,520)
           p2=520
           return
      endif
      ITST(1)=98
      IR=98
      WRITE(LU,720)
      GOTO 1141
C
C  *** presun na most ***
C
  700 IF (IR.EQ.17) GOTO 800
      IF ((IR.LT.54).OR.(IR.GT.83)) then
c      CALL SGLOD(NAME,LU,520)
          p2=520
          return
      endif
      IF (IWRD(6,1).EQ.6)IR=81
      IF (IWRD(6,1).EQ.10)IR=82
      ITST(1)=IR
      WRITE(LU,720)
  720 FORMAT(/' Jste zahalen oblakem kou©e, kter˜ rychle miz¡')
      GOTO 1141
C
C ** SCROLL - svitek **
C
  800 IF ((IWRD(6,1).EQ.5.AND.IVEN(88).NE.17).OR.(IWRD(6,1).EQ.6.AND.
     1IVEN(88).NE.117).OR.(IWRD(6,1).EQ.9.AND.IVEN(88).NE.217).OR.
     2(IWRD(6,1).EQ.10.AND.IVEN(88).NE.317)) then
c     3  CALL SGLOD(NAME,LU,520)
          p2=520
          return
      endif
      IF (IVEN(88).EQ.1017) then
c         CALL SGLOD(NAME,LU,520)
          p2=520
          return
      endif
      IVEN(88)=IVEN(88)+100
      IF (IVEN(88).EQ.417) then
c      CALL SGLOD(NAME,LU,810)
          p2=810
          return
      endif
c     CALL SGLOD(NAME,LU,1040)
      p2=1040
      return
C
C=====C                      MM-C-3  (LINE 2400)
C 4-C C
C=====C
C
C *** DIG - kopat ***
C
  900 IF (IWRD(7,1).NE.1) then
c   CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C ** lze zde kopat? **
C
      IF (IR.LT.28.OR.(IR.GT.80.AND.IR.LT.97)) then
c     1  CALL SGLOD(NAME,LU,910)
          p2=910
          return
      endif
C
C ** ma lopatku? **
C
      IF (IVEN(32).LT.20000) then
c      CALL SGLOD(NAME,LU,920)
          p2=920
          return
      endif
C
C ** kopani **
C
      IF (IR.LT.62.OR.(IR.GT.80.AND.IR.LT.97).OR.(IR.GT.65.AND.IR.LT.78)
     :.OR. PROB(0.667)) then
c     CALL SGLOD(NAME,LU,1040)
          p2=1040
          return
      endif
      IF (IR .EQ. 99) THEN
         IR=28
      ELSEIF (IR .EQ. 98) THEN
         IR=33
      ELSEIF (IR .EQ. 97) THEN
         IR=34
      ELSEIF (IR .EQ. 62) THEN
         IR=43
         IXT(341)=4362
      ELSEIF (IR.EQ.63) THEN
         IR=45
         IXT(342)=4563
      ELSEIF (IR.EQ.64) THEN
         IR=48
         IXT(343)=4864
      ELSEIF (IR.EQ.65) THEN
         IR=50
         IXT(344)=5065
      ELSEIF (IR.EQ.78) THEN
         IR=29
         IXT(345)=2978
      ELSEIF (IR.EQ.79) THEN
         IR=33
         IXT(346)=3379
      ELSEIF (IR.EQ.80) THEN
         IR=29
         IXT(347)=2980
      ENDIF
      ITST(1)=IR
      ITST(14)=IR
c       !neni cesty zpet
  930 F='(/'' P©i kop n¡ jste se propadl do podzemn¡ chodby.'')'
      WRITE(LU,f)
      GOTO 1141
C
C ****** nic se nedeje ******
C
  950 ITST(22)=ITST(22)+1
      IF (ITST(38).NE.'MM') NAME(2)='SB'
      IF (ITST(38).NE.'MM') then
c      CALL SGLOD(NAME,LU)
          return
      endif
      IF (ITST(22).EQ.1) then
c      CALL SGLOD(NAME,LU,960)
          p2=960
          return
      endif
      IF (ITST(22).EQ.2) then
c      CALL SGLOD(NAME,LU,963)
          p2=963
          return
      endif
      IF (ITST(22).EQ.3) then
c      CALL SGLOD(NAME,LU,966)
          p2=966
          return
      endif
c     CALL SGLOD(NAME,LU,970)
      p2=970
      return
C
C *** FOLLOW - nasleduj **
C
  970 IF (IWRD(5,2).NE.4.OR.IWRD(7,1).NE.2) then
c     1 CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
      IF (IFSD(IRES(IWRD(6,2))).NE.IR) then
c      CALL SGLOD(NAME,LU,980)
          p2=980
          return
      endif
      J=IWRD(6,2)
      FOLLOW=.TRUE.
      GOTO 1105
C
C
C                            MM-C-4  (LINE 2480)
C=====C
C 5-C C
C=====C
C
C ***** GO - jit *****
C
 1000 IF (IWRD(7,1).GT.2) then
c         CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
      IC=IWRD(6,2)
      IF (IWRD(7,1).EQ.1)IC=9
C *** GO CRAZY IN WOODS ROUTINE ***
      IRX=0
      IF (IWRD(6,2).EQ.110.AND.(IR.EQ.97.OR.IR.EQ.98))IRX=84
      IF (IRX.EQ.84)IR=84
      IF (IRX.EQ.84)ITST(1)=84
      IF (IRX.EQ.84) GOTO 1025
      IF ((IWRD(7,1).EQ.2).AND.(IWRD(5,2).NE.2)) then
c     1 CALL SGLOD(NAME,LU,1050)
          p2=1050
          return
      endif
C
C *** jit zpet ***
C
      IF ((IC.EQ.11).AND.(ITST(5)-1.NE.ITST(4)-(ITST(4)/1000)
     1*1000.OR.IR.EQ.97.OR.IR.EQ.98)) then
c     CALL SGLOD(NAME,LU,1002)
          p2=1002
          return
      endif
      IF (IC.EQ.11)ITST(1)=ITST(14)
      IF (IC.EQ.11)ITST(14)=IR
      IF (IC.EQ.11)IR=ITST(1)
      IF (IC.EQ.11) GOTO 1025
C
C *** kompas ***
C
      IF (IC.LT.5.AND.((IR.LT.54..AND.IR.NE.35).OR.IR.EQ.98)
     1.AND.(IVEN(6).LT.20000)) then
c     CALL SGLOD(NAME,LU,1005)
          p2=1005
          return
      endif
      IF (IC.LT.7) GOTO 1007
      IC=IC-9+ITST(4)/1000
      IF (IC.GT.4)IC=IC-4
      IF (IC.LT.1)IC=IC+4
 1007 IFC=ITST(4)/1000
      IF (IC.LT.5)ITST(4)=ITST(4)-(ITST(4)/1000)*1000+IC*1000
C
C *** hledani vychodu ***
C
      DO 1010 J=IC*60-59,IC*60
      IF (IFSD(IXT(J)).EQ.IR) GOTO 1015
 1010 CONTINUE
c     CALL SGLOD(NAME,LU,1105)
      p2=1105
      return
C
C *** male dvere ***
C
 1015 IF (J.NE.93.AND.J.NE.212.AND.J.NE.281.AND.J.NE.351) GOTO 1020
      IF (IR.LT.88) GOTO 1020
      DO 1016 K=1,53
      IF (IVEN(K).GT.20000.AND.(J.EQ.281.OR.ITD(IVEN(K)).GT.1)) then
c     1 CALL SGLOD(NAME,LU,1017)
          p2=1017
          return
      endif
 1016 CONTINUE
C
C *** dvere jsou zavrene ***
C
 1020 IF ((IR.LT.55.OR.IR.EQ.90).AND.(IXT(J).GT.20000)) then
c     1 CALL SGLOD(NAME,LU,1021)
          p2=1021
          return
      endif
C
C *** hlavni vchod ***
C
      IX=0
      IF (IR.EQ.91.AND.ITST(5)-1.NE.ITST(13))IX=99
      IF (IX.EQ.99)ITST(1)=2
      IF (IX.EQ.99)IR=2
 1023 F='(/'' Propadl jste se propadli¨tˆm.'')'
      IF (IX.EQ.99) WRITE(LU,f)
      IF (IX.EQ.99) GOTO 1025
C
C                            MM-C-5  (LINE 2560)
C=====C
C 6-C C
C=====C
C
C *** ZEBRIK V CHODBE ***
C
      IF (PROB(0.5) .OR. IR .NE. 39) GOTO 1024
      IF (PROB(0.75)) IR=41
      ITST(1)=IR
 1022 F='(/'' Uklouzl jste na ‘eb©¡ku.'')'
      WRITE(LU,f)
      GOTO 1025
C
C *** brana je zavrena ***
C
 1024 IF ((IR.GT.54).AND.(IXT(J).GT.20000)) then
c         CALL SGLOD(NAME,LU,1023)
          p2=1023
          return
      endif
      IRB=ITST(1)
      ITST(14)=IRB
      ITST(1)=ITFD(IXT(J))
      IR=ITST(1)
 1025 ITST(4)=(ITST(4)/1000)*1000+ITST(5)
C
C                            MM-C-6  (LINE 2640)
C=====C
C 7-C C
C=====C
C
C
C **** po pohybu veci zavisle na pokoji ****
C
C *** pad z utesu ***
C
      IF (IR.EQ.0) then
c      CALL SGLOD(NAME,LU,1026)
          p2=1026
          return
      endif
C
C *** zaplneni chodby vodou ***
C
      IF (IR.EQ.46.AND.IXT(320).EQ.0) then
c      CALL SGLOD(NAME,LU,1027)
          p2=1027
          return
      endif
C
C *** listek ***
C
      IF (IVEN(22).EQ.0.AND.ITST(5)-(ITST(5)/50)*50.LT.10.AND.IR
     1.NE.35)IVEN(22)=100+IR
      IF ((IVEN(22).NE.0).AND.(IVEN(22).LT.20000).AND.
     1(ITST(5)-(ITST(5)/50)*50.GT.40).AND.(IFSD(IVEN(22)).NE.
     2IR))IVEN(22)=0
C
C *** jestlize jsou u vchodu myslivec i lovec, zavrou branu ***
C
      IF (IRES(6).EQ.893.AND.IRES(12).EQ.993)IXT(54)=29293
      IF (IRES(6).EQ.892.AND.IRES(12).EQ.992)IXT(54)=29293
      IF (IXT(54).EQ.29293)IXT(234)=29392
C
C *** bloudeni v lesich ***
C
      IF ((IR.NE.97.AND.IR.NE.98).OR.IWRD(6,1).EQ.25) GOTO 1045
      DO 1030 I=1,2
      R=URAN()
      DO 1030 J=1,INT(R*10+10)
      R1=URAN()
c     IC=IXT(60*INT(R1*4)+58+I)
      iii=int(r1*4)
      ic=ixt(60*iii+58+i)
      IF (IR.EQ.98.AND.ITST(17).GT.1.AND.IC.EQ.9798)IC=3598
      IF (IR.EQ.98.AND.ITST(17).LT.2.AND.IC.EQ.3598)IC=9798
      R=URAN()
c     IXT(60*INT(R1*4)+58+I)=IXT(60*INT(R*4)+58+I)
      iii=int(r1*4)
      iiii=int(r*4)
      ixt(60*iii+58+i)=ixt(60*iiii+58+i)
c     IXT(60*INT(R*4)+58+I)=IC
      iii=int(r*4)
      ixt(60*iii+58+i)=ic
 1030 CONTINUE
C
C ** pokud v lese predmet nevezmem, muze zmizet ***
C
      DO 1040 J=1,35
      IF ((IVEN(J).GE. 10000).OR.((IFSD(IVEN(J)).NE.IR).AND.
     1(IFSD(IVEN(J)).NE.0)).OR.IVEN(J).EQ.0) GOTO 1040
      IF (IFSD(IVEN(J)).EQ.IR)IVEN(J)=IVEN(J)-IR
      IF (IFSD(IVEN(J)).GT.0) GOTO 1040
      IF (PROB(0.9)) GOTO 1040
      IVEN(J)=IVEN(J)+IR
 1040 CONTINUE
C
C ** vytvoreni vychodu z lesa **
C
      IF (IXT(59).EQ.9497.AND.IXT(179).EQ.9897)IXT(59)=9597
      IF (IXT(119).EQ.9297.OR.IXT(119).EQ.9597)IXT(119)=9497
      IF (IXT(179).EQ.9497.AND.IXT(59).EQ.9997)IXT(179)=9297
      IF (IXT(239).EQ.9297.OR.IXT(239).EQ.9597)IXT(239)=9497
C
C ** ztracene veci muze prinest lesni vila - pouze v lese **
C
      IF (PROB(0.8)) GOTO 1045
      DO 1042 J=NORM(35),35
      IF (J.EQ.22) GOTO 1042
      IF (IVEN(J).GT.10000.OR.IFSD(IVEN(J)).NE.0.OR.IVEN(J).EQ.0)
     1 GOTO 1042
 1041 F='(/'' Lesn¡ nymfa p©ibˆhla a p©inesla ''\)'
      WRITE(LU,f)
      CALL MMRI(4,J)
      IVEN(J)=IVEN(J)+IR
      GOTO 1045
 1042 CONTINUE
C
C                            MM-C-7  (LINE 2720)
C
C=====C
C C-8 C
C=====C
C
C *** krtci bludiste ***
C
C ** jestlize je hrac v bludisti a krtek nazivu **
C
 1045 IC=1
      IF (IR.LT.42.OR.IR.GT.53.OR.IRES(4).GE.10000
     1.OR.IFSD(IRES(4)).EQ.52) GOTO 1080
C
C ** zmena pobytu krtka ***
C
C * zmena vychodu *
C
      IMZ(1)=0
      IMZ(2)=0
      IMZ(3)=0
      IC=0
      IX=NORM(3)
      DO 1050 I=1,360
         IF (IFSD(IXT(I)).NE.IFSD(IRES(4)).OR.ITFD(IXT(I)).LT.42.OR.
     1       ITFD(IXT(I)).GT.51) GOTO 1050
         IC=IC+1
         IMZ(IC)=I
         IF (IC.EQ.IX) GOTO 1055
 1050 CONTINUE
      GOTO 1080
C
C
C
 1055 IX=NORM(360)
      DO 1060 J=IX,360
         IF (IXT(J).LT.4100.OR.IFSD(IXT(J)).NE.0) GOTO 1060
         DO 10550 K=1,3
            IF ((IMZ(K)-1)/60 .EQ. (J-1)/60) GOTO 1060
10550    continue
         IXX=IXT(J)
         IXT(J)=IXT(I)
         IXT(I)=IXX
         GOTO 1075
 1060 CONTINUE
C
C * druhy cyklus *
C
      DO 1070 J=1,IX
         IF (IXT(J).LT.4100.OR.IFSD(IXT(J)).NE.0) GOTO 1070
         DO 10700 K=1,3
            IF ((IMZ(K)-1)/60 .EQ. (J-1)/60) GOTO 1070
10700    continue
         IXX=IXT(J)
         IXT(J)=IXT(I)
         IXT(I)=IXX
         GOTO 1075
 1070 CONTINUE
      GOTO 1080
 1075 IF (IC.LT.2 .OR. PROB(0.7)) GOTO 1080
      DO 1076 I=1,360
        IF (IFSD(IXT(I)).EQ.ITFD(IXT(J)) .AND.
     1     ITFD(IXT(I)).EQ.IFSD(IXT(J))) IXT(I)=100*(IXT(I)/100)
 1076 CONTINUE
      IXT(J)=100*(IXT(J)/100)
C
C *** zpracovani chodby ***
C
 1080 IF ((IR.GT.35).AND.(IR.LT.41))ITST(12)=ITST(12)-ITST(5)+ITST(13)
      IF ((IR.GT.35).AND.(IR.LT.41).AND.(ITST(12).GT.0)) THEN
 1082    F='(/'' Stˆny chodby se sv¡raj¡.'')'
         WRITE(LU,f)
      ENDIF
      IF ((IR.GT.35).AND.(IR.LT.40).AND.(IVEN(17).GT.20000)
     1.AND.(ITST(12).LE. 0)) CALL SGLOD(NAME,LU,1084)
      IF ((IR.EQ.40).AND.(ITST(12).LE.0)) then
c     1    CALL SGLOD(NAME,LU,1086)
          p2=1086
          return
      endif
      IF ((IR.GT.35).AND.(IR.LT.40).AND.(ITST(12).LE. 0)) then
c     1    CALL SGLOD(NAME,LU,1088)
          p2=1088
          return
      endif
      IF ((IR.EQ.4).OR.(IR.EQ.41)) THEN
 1089    F='(/'' Stˆny chodby se otev©ely.'')'
         WRITE(LU,f)
      ENDIF
      IF ((IR.EQ.4).OR.(IR.EQ.41)) ITST(12)=4
      IF (IR.EQ.4)IXT(140)=3640
      IF (IR.EQ.4)IXT(314)=3940
C
C                            MM-C-8  (LINE 2800)
C=====C
C 9-C C
C=====C
C
C   *** zavrena zahrada (PO 18.00) ***
C
      IF (ITST(5).LE.300.OR.IR.LT.55.OR.IR.GT.82) GOTO 1097
      IF (PROB(0.3)) THEN
 1091    F='(/'' Prosmekl se kolem V s had.'')'
         WRITE(LU,f)
      ELSEIF (PROB(0.15)) THEN
 1092    F='(/'' Had, kter˜ se kolem plazil, V s pr vˆ u¨tkl.'')'
         WRITE(LU,f)
      ELSEIF (PROB(0.05)) THEN
c         CALL SGLOD(NAME,LU,1093)
          p2=1093
          return
      ENDIF
C
C *** vlk na travnate louce ***
C
 1097 IF ((IR.EQ.71.OR.IR.EQ.72).AND.IRES(15).LT.10000) THEN
 1098    F='(/'' Bˆ‘¡ k V m vlk.'')'
         WRITE(LU,f)
      ENDIF
      IF ((IR.EQ.71.OR.IR.EQ.72).AND.IVEN(24).LT.20000.AND.IRES(15).LT.
     :10000) then
c     CALL SGLOD(NAME,LU,1097)
          p2=1097
          return
      endif
      IF ((IR.EQ.71.OR.IR.EQ.72).AND.IRES(15).LT.10000) then
c     : CALL SGLOD(NAME,LU,1096)
          p2=1096
          return
      endif
C
C *** uzavreni tajnych vchodu ***
C
      IF (IR.NE.1)IXT(1)=0
      IF (IR.NE.5)IXT(183)=0
      IF (IR.NE.7)IXT(123)=0
      IF (IR.NE.10)IXT(5)=0
      IF (IR.NE.14)IXT(187)=0
      IF (IR.NE.16)IXT(128)=0
      IF (IR.NE.19)IXT(11)=0
      IF (IR.NE.23)IXT(191)=0
      IF (IR.NE.25)IXT(134)=0
C
C *** jizni cesta ***
C
      IF (IR.EQ.93) ITST(17)=0
      IF (IR.EQ.35.AND.IWRD(6,2).EQ.3) ITST(17)=ITST(17)+1
      IF (IR.EQ.35.AND.IWRD(6,2).EQ.1) ITST(17)=ITST(17)-1
      IF (IR.EQ.35.AND.(IFC+IWRD(6,2).EQ.10.OR.IFC
     1+IWRD(6,2).EQ.14)) ITST(17)=ITST(17)-1
      IF (IR.EQ.35.AND.(IFC+IWRD(6,2).EQ.8.OR.IFC
     1+IWRD(6,2).EQ.12)) ITST(17)=ITST(17)+1
      IF (ITST(17).EQ.1) IXT(47)=9335
      IF (ITST(17).EQ.2) IXT(47)=3535
      IC=100-ITST(17)/10
      ID=ITST(17)/10
      IF (IR.EQ.35.AND.ITST(17)/10.-ITST(17)/10.EQ..0.AND.ITST(5)
     1.LT.300) THEN
1099  F='(/'' Pr vˆ jste minul patn¡k, na kter‚m je naps no:''/'' Velk‚
     :mˆsto:  ''I3/'' Tajemn˜ z mek:''I3)'
          WRITE(LU,f)IC,ID
      ENDIF
C
C *** vyleti demoni ***
C
      IF (IFD(IVEN(35)).EQ.2.AND.ITD(IVEN(35)).EQ.6)NAME(2)='SG'
      IF (NAME(2).EQ.'SG')IVEN(35)=IVEN(35)-100
      IF (NAME(2).EQ.'SG') then
c      CALL SGLOD(NAME,LU,10210)
          p2=10210
          return
      endif
C
C *** presun obyvatel ***
C
 1100 FOLLOW=.FALSE.
      j=1
1101  j=j+1
      if(j.gt.16) goto 1141
         IF (J.EQ.15) GOTO 1140
C
C *** neni nebo mrtvola ***
C
         IF (IFSD(IRES(J)).EQ.0.OR.IRES(J).GT.10000) GOTO 1140
C
C *** je na stejnem miste ***
C
         IF (IFSD(IRES(J)).EQ.IRB) GOTO 1120
C
C ** presun obyvatel, jsou-li sami **
C
 1105    II=(NORM(6)-1)*60
         DO 1110 K=II+1,II+60
            IF (IFSD(IXT(K)).NE.IFSD(IRES(J))) GOTO 1110
            IF (IXT(K).GT.20000) GOTO 1140
            IRC=IFSD(IRES(J))
C *** presun obyvatele ***
            IRES(J)=(IRES(J)/100)*100+ITFD(IXT(K))
            GOTO 1130
 1110    CONTINUE
         GOTO 1140
C
C                            MM-C-9  (LINE 2880)
C======C
C 10-C C
C======C
C
C
C ** presun obyvatel s hracem **
C
 1120    IRES(J)=IRES(J)-IRB+IR
         IRC=IRB
C
C ** muze se tam presunout? **
C
 1130    IC=IRES(J)-(IRES(J)/100)*100
C
C * sluha a pan *
C
         IF ((J.EQ.3.OR.J.EQ.14).AND.(IC.GT.27.AND.(ITST(5).LT.400.OR.
     1   IFSD(IRES(J)).NE.IR).OR.IC.EQ.0)) IRES(J)=100*(IRES(J)/100)+IRC
C
C * krtek *
C
         IF (J.EQ.4.AND.(IC.GT.51.OR.IC.LT.43))IRES(4)=100*
     1    (IRES(4)/100)+IRC
C
C * vampir *
C
         IF (J.EQ.5.AND.(IC.EQ.16.OR.(IROM(IC).GE.20000.AND.
     1     ITST(5).LT.300).OR.IC.GT.27.OR.IC.EQ.21))
     2     IRES(5)=100*(IRES(5)/100)+IRC
C
C * lesnik a lovec *
C
         IF ((J.EQ.6.OR.J.EQ.12).AND.(IC.LT.92.OR.IC.EQ.98))IRES(J)=
     1       100*(IRES(J)/100)+IRC
C
C * sluzka a pani *
C
         IF ((J.EQ.8.OR.J.EQ.2).AND.(IC.LT.9.OR.IC.GT.77))IRES(J)=
     1       100*(IRES(J)/100)+IRC
C
C * kuchar a sadar *
C
       IF ((J.EQ.9.OR.J.EQ.7).AND.((IC.GT.18.AND.IC.LT.54).OR.IC.GT.77))
     1    IRES(J)=100*(IRES(J)/100)+IRC
C
C * vlkodlak *
C
       IF (J.EQ.10.AND.(IC.EQ.98.OR.IC.EQ.78.OR.IC.EQ.80.OR.IC.EQ.88.OR.
     1      IC.EQ.83.OR.IC.EQ.0))IRES(10)=100*(IRES(10)/100)+IRC
C
C * skret *
C
         IF (J.EQ.13.AND.(IC.LT.29.OR.IC.GT.31))IRES(13)=
     1      100*(IRES(13)/100)+IRC
C
C * skritek *
C
         IF (J.EQ.11.AND.(IC.LT.55.OR.IC.GT.77)) IRES(11)=
     1      100*(IRES(11)/100)+IRC
C
C * rytir *
C
         IF (J.EQ.16.AND.(IC.LT.93.OR.IC.GT.96)) IRES(16)=
     1      100*(IRES(16)/100)+IRC

 1140    IF (FOLLOW) GOTO 1141
c  !VYSKOK Z CYKLU
      goto 1101
 1141 ITST(13)=ITST(5)
C
C *** follow - jde za obyvatelem ***
C
      IF (IWRD(6,1).EQ.13) IR=IFSD(IRES(IWRD(6,2)))
      ITST(1)=IR
C
C *** reseni zahady ***
C
      IF (IR.EQ.ITST(8).AND.IFSD(IRES(ITST(9))).EQ.IR.AND.
     1  (IVEN(ITST(10)).GT.20000.OR.
     2  (IFSD(IVEN(ITST(10))).EQ.IR.AND.IVEN(ITST(10)).LT.10000)))
     3    then
c     CALL SGLOD(NAME,LU,1145)
          p2=1145
          return
      endif
C
C                            MM-C-10 (LINE 2960)
C======C
C 11-C C
C======C
C
C *** je zde svetlo? ***
C
C ** je zde svetelny zdroj? **
C
C   je zde lucerna, svice nebo pochoden nebo ji ma hrac
C   nebo obyvatel
C
 1142 DO 1150 I=1,3
      IF (I.EQ.1)K=4
      IF (I.EQ.2)K=19
      IF (I.EQ.3)K=27
      IF (IFD(IVEN(K)).EQ.1.AND.((IVEN(K).LT.10000.AND.IFSD(IVEN(K))
     1.EQ.IR).OR.IVEN(K).GT.20000.OR.(IVEN(K).GT.10000.AND.IVEN(K)
     2.LT.20000.AND.IFSD(IRES(IFSD(IVEN(K)))).EQ.IR))) GOTO 1190
 1150 CONTINUE
C
C * lampa *
C
      IF ((IR.EQ.10).AND.(IVEN(76).GT.1000)) GOTO 1190
C
C * krb *
C
      IF ((IR.EQ.14).AND.(IVEN(89).GT.1000)) GOTO 1190
C
C * zapalky *
C
      IF (IVEN(31).GT.21000) GOTO 1190
C
C ** je venku a je den **
C
      IF ((IR.GT.53).AND.(ITST(5).LT.300)) GOTO 1190
      IF (IR.GT.53) then
c      CALL SGLOD(NAME,LU,1150,1)
          p2=1150
          p3=1
          return
      endif
C
C ** jsou vytazene zavesy **
C
      IF ((IROM(IR).GT.20000).AND.(ITST(5).LT.300)) GOTO 1190
      IF (IROM(IR).GT.20000) then
c      CALL SGLOD(NAME,LU,1150,1)
          p2=1150
          p3=1
          return
      endif
      IF ((ITST(5).LT.300).AND.(IROM(IR).GT.10000)) then
c     1 CALL SGLOD(NAME,LU,1150,2)
          p2=1150
          p3=2
          return
      endif
c     CALL SGLOD(NAME,LU,1150)
      p2=1150
      return
C
C *** vypocet skore ***
C
 1190 ISC=ITFD(IROM(IR))
      ITST(2)=ITST(2)+ISC-50
      IROM(IR)=IROM(IR)-(ISC*100)+5000
      IF (ISC.LT.50)ITST(3)=ITST(3)-50+ISC
C
C *** tisk pokoje ***
C
      IIR=IFSD(IROM(IR))
      IC=MMPRP(IR)
c           ! VYBER SPRAVNOU PREDLOZKU
 1200 F='(/'' Jste ''A2'' ''8A2)'
      WRITE(LU,f)IPRP(1,IC),(IRNM(L,IIR),L=1,8)
C
C * sfouknuti svice *
C
      IF (IVEN(19).GT.21000.AND.(IR.EQ.21.OR.IR.EQ.36.OR.IR.EQ.54
     1.OR.IR.EQ.92)) then
c     CALL SGLOD(NAME,LU,213)
          p2=213
          return
      endif
C
C                            MM-C-11 (LINE 3040)
C======C
C 12-C C
C======C
C  *** micto cinu ***
C
 1300 F='(/'' Je zde krev...'')'
      IF ((ITST(8).EQ.IR).AND.(ITST(10).GT.8).AND.(ITST(10)
     1.LT.13)) WRITE(LU,f)
C
C *** co je zde ***
C
      CALL MMRI(1,0)
C
C *** kdo je zde ***
C
      IC=0
      DO 1510 J=1,17
      IF (IR.NE.IFSD(IRES(J))) GOTO 1510
      IF (IC.EQ.0) WRITE(LU,1500)(IRSN(L,J),L=1,4)
      IF (IC.EQ.1) WRITE(LU,1505)(IRSN(L,J),L=1,4)
 1500 FORMAT(/' Je zde s V mi: '4A2)
 1505 FORMAT(16X,4A2)
      IC=1
 1510 CONTINUE
C
C *** kam muze jit ***
C
      IF (ITST(5).LT.450.AND.(IR.LT.28.OR.IR.GT.64.OR.(IR.GT.34.AND.
     1IR.LT.42).OR.(IR.GT.52.AND.IR.LT.58))) GOTO 1620
      J2=NORM(240)
 1585 DO 1610 J=J2,360
      IF (IFSD(IXT(J)).NE.IR) GOTO 1610
      J1=(J-1)/60+1
C
C *** nezna svetove strany ***
C
      IF ((IR.GT.53.AND.IR.NE.98).OR.IVEN(6).GT.20000.OR.IR.EQ.35.
     1.OR.(J1.GT.4)) GOTO 1590
      J1=9+J1-ITST(4)/1000
      IF (J1.GT.10)J1=J1-4
      IF (J1.LT.7)J1=J1+4
 1590 WRITE(LU,1600)(IDTN(L,J1),L=1,4)
 1600 FORMAT(/' M–‘ete j¡t ',4A2)
      GOTO 1620
 1610 CONTINUE
      IF (J2.EQ.1) GOTO 1620
      J2=1
      GOTO 1585
 1620 CONTINUE
C
C *** poprve se vypisuje dlouhy popis ***
C
      IF (ISC.NE.50.OR.IWRD(6,1).EQ.25)NAME(2)='SE'
      IF (NAME(2).EQ.'SE') then
c      CALL SGLOD(NAME,LU,IR)
          p2=ir
          return
      endif
      NAME(2)='SB'
c     CALL SGLOD(NAME,LU)
      return
      END
