C
C                            MM-B-11  (LINE 2160)
C 1-C
      subroutine MMSC
C
C ******* SEGMENT C *******
C
C      THIS IS THE GO TYPE VERB SEGMENT THAT IS CALLED BY MMSB. IT
C      NORMALLY COMPLETES BY CALLING MMSD WITH CLOSING COMMENTS.
C
      include 'common.i'

      DIMENSION MSG(45),IMZ(3),MSGR(45),ITIM(5)
C
C *** FUNCTIONS ***
C
      IFSD(I)=I-(I/100)*100
      ITFD(I)=I/100-(I/10000)*100
      ITD(I)=I/100-(I/1000)*10
      IFD(I)=I/1000-(I/10000)*10
      RN(R)=R*7.7-INT(R*7.7)
C
C *** SET VARIABLES ***
C
      LU=IPR(1)
      LX=ITST(21)
      IR=ITST(1)
      R=RND
C
C ****** INDEX ******
C
C        SUBJECT                                                PAGE
C
C        A. GO TYPE VERBS                                       C-2
C
C           1. CLIMB                                            C-2
C           2. ENTER AND MAGIC WORDS                            C-3
C           3. DIG AND FOLLOW                                   C-4
C           4. GO                                               C-5
C
C        B. ROOM DEPENDENT ITEMS                                C-6
C
C           1. FALL NOTE AND WOODS                              C-6
C           2. MOLE MAZE AND CORRIDOR                           C-7
C           3. MEADOW, PASSAGE ENTRANCES,HIGHWAY, AND DEMONS    C-8
C           4. MOVE INHABITANTS AND MYSTERY SOLUTION            C-9
C           5. LIGHT,SCORE, AND ROOM                            C-10
C           6. WHAT, WHO, WAY, AND DESCRIPTION                  C-11
C
C                            MM-C-1  (LINE 2240)
C 2-C
C ****** GO TYPE VERBS ROUTINE ******
C
C ***** SET UP FOR SEGMENT D AND SELECT VERB *******
C
      NAME(2)=is_sd
C
C ** MAGIC WORD AND GARDEN CLOSED **
C
      IF(ITST(5).GT.300.AND.IWRD(6,1).GT.3.AND.IWRD(6,1).LT.12)
     1CALL exex(8,NAME,LU,290,0,0,0)
      IF(IWRD(7,1).EQ.1.AND.IWRD(6,1).EQ.25)GOTO 1142
      GOTO(1000,300,400,600,500,700,600,600,500,700,600,900,970,950)
     1IWRD(6,1)
C
C **** CLIMB ROUTINE ****
C
C *** IF CLIMB MEANS GO UP ***
C
  300 IF(IWRD(7,1).NE.1)GOTO 310
      IWRD(7,1)=2
      IWRD(5,2)=2
      IWRD(6,2)=5
      GOTO 1000
C
C *** CHECK FOR AND REMOVE PREPOSITION ***
C
  310 IF(IWRD(7,1).EQ.2)GOTO 330
      IF((IWRD(7,1).GT.3).OR.(IWRD(5,2).NE.5).OR.
     1(IWRD(6,2).NE.5))CALL exex(8,NAME,LU,320,0,0,0)
      IWRD(5,2)=IWRD(5,3)
      IWRD(6,2)=IWRD(6,3)
C
C*** CHECK AND RESPOND TO WALL ***
C
  330 IF(IWRD(5,2).NE.3)CALL exex(8,NAME,LU,320,0,0,0)
      IF(IVEN(15).LT.20000.OR.IWRD(6,2).NE.77.OR.(IR.NE.93.AND.
     1IR.NE.94.AND.IR.NE.95.AND.IR.NE.96.AND.IR.NE.70.AND.IR.NE.66.AND.
     2IR.NE.67.AND.IR.NE.92))GOTO 335
      IRB=ITST(1)
      ITST(14)=IRB
      IF(ITST(1).EQ.93)IR=92
      IF(ITST(1).EQ.94)IR=66
      IF(ITST(1).EQ.95)IR=70
      IF(ITST(1).EQ.96)IR=67
      IF(ITST(1).EQ.67)IR=96
      IF(ITST(1).EQ.70)IR=95
      IF(ITST(1).EQ.66)IR=94
      IF(ITST(1).EQ.92)IR=93
      ITST(1)=IR
      GOTO 1141
  335 IF(IWRD(6,2).EQ.77.AND.(IR.EQ.66.OR.IR.EQ.67.OR.IR.EQ.70.OR.
     1IR.EQ.92.OR.IR.EQ.93.OR.IR.EQ.94.OR.IR.EQ.95.OR.IR.EQ.96))
     2CALL exex(8,NAME,LU,340,0,0,0)
      IF(IWRD(6,2).EQ.77)CALL exex(8,NAME,LU,350,0,0,0)
C
C *** CHECK AND RESPOND TO TREE ***
C
      IF((IWRD(6,2).EQ.78).AND.(IR.NE.97).AND.
     1(IR.NE.98))CALL exex(8,NAME,LU,360,0,0,0)
      IF(IWRD(6,2).EQ.78)CALL exex(8,NAME,LU,370,0,0,0)
C
C *** CHECK AND RESPOND TO GATE ***
C
      IF(IWRD(6,2).EQ.93.AND.IR.NE.93)CALL exex(8,NAME,LU,390,0,0,0)
      IF(IWRD(6,2).EQ.93)CALL exex(8,NAME,LU,385,0,0,0)
      CALL exex(8,NAME,LU,320,0,0,0)
C
C                            MM-C-2  (LINE 2320)
C 3-C
C **** ENTER ROUTINE ****
C
  400 IF(IWRD(7,1).GT.1.AND.IWRD(5,2).NE.3.AND.IWRD(6,2).NE.93)
     1CALL exex(8,NAME,LU,320,0,0,0)
      IWRD(7,1)=2
      IWRD(6,1)=1
      IWRD(5,2)=2
      IF(IR.EQ.93.OR.IR.EQ.91)IWRD(6,2)=1
      IF(IR.EQ.95)IWRD(6,2)=3
      IF(IWRD(6,2).NE.0)GOTO 1000
      CALL exex(8,NAME,LU,410,0,0,0)
C
C **** MAGIC WORD ROUTINE ****
C
C *** MOVE TO VERANDA IF IN GARDEN ***
C
  500 IF(IR.EQ.17)GOTO 800
      IF((IR.LT.55).OR.(IR.GT.83))CALL exex(8,NAME,LU,520,0,0,0)
      ITST(1)=54
      IR=54
      write(6,720)
      IF(LX.NE.0)WRITE(LX,720)
      GOTO 1141
C
C *** MOVE TO WOODS FROM ANYWHERE ***
C
  600 IF(IR.EQ.98)CALL exex(8,NAME,LU,520,0,0,0)
      ITST(1)=98
      IR=98
      write(6,720)
      IF(LX.NE.0)WRITE(LX,720)
      GOTO 1141
C
C  *** MOVE TO BRIDGE ***
C
  700 IF(IR.EQ.17)GOTO 800
      IF((IR.LT.54).OR.(IR.GT.83))CALL exex(8,NAME,LU,520,0,0,0)
      IF(IWRD(6,1).EQ.6)IR=81
      IF(IWRD(6,1).EQ.10)IR=82
      ITST(1)=IR
      write(6,720)
      IF(LX.NE.0)WRITE(LX,720)
  720 FORMAT(/" YOU ARE ENVELOPED IN A CLOUD OF SMOKE THAT QUICKLY ",
     1"DISAPPEARS")
      GOTO 1141
C
C ** SCROLL ROUTINE **
C
  800 IF((IWRD(6,1).EQ.5.AND.IVEN(88).NE.17).OR.(IWRD(6,1).EQ.6.AND.
     1IVEN(88).NE.117).OR.(IWRD(6,1).EQ.9.AND.IVEN(88).NE.217).OR.
     2(IWRD(6,1).EQ.10.AND.IVEN(88).NE.317))
     3 CALL exex(8,NAME,LU,520,0,0,0)
      IF(IVEN(88).EQ.1017)CALL exex(8,NAME,LU,520,0,0,0)
      IVEN(88)=IVEN(88)+100
      IF(IVEN(88).EQ.417)CALL exex(8,NAME,LU,810,0,0,0)
      CALL exex(8,NAME,LU,1040,0,0,0)
C
C                            MM-C-3  (LINE 2400)
C 4-C
C *** DIG ROUTINE ***
C
  900 IF(IWRD(7,1).NE.1)CALL exex(8,NAME,LU,320,0,0,0)
C
C ** CAN HE DIG HERE **
C
      IF(IR.LT.28.OR.(IR.GT.80.AND.IR.LT.97))
     1 CALL exex(8,NAME,LU,910,0,0,0)
C
C ** DOES HE HAVE A SHOVEL **
C
      IF(IVEN(32).LT.20000)CALL exex(8,NAME,LU,920,0,0,0)
C
C ** DIG **
C
      R=RN(R)
      IF(IR.LT.62.OR.(IR.GT.80.AND.IR.LT.97).OR.(IR.GT.65.AND.IR.LT.78)
     1.OR.R.LT..667)CALL exex(8,NAME,LU,1040,0,0,0)
      IF(IR.EQ.99)IR=28
      IF(IR.EQ.98)IR=33
      IF(IR.EQ.97)IR=34
      IF(IR.EQ.62)IR=43
      IF(IR.EQ.62)IXT(341)=4362
      IF(IR.EQ.63)IR=45
      IF(IR.EQ.63)IXT(342)=4563
      IF(IR.EQ.64)IR=48
      IF(IR.EQ.64)IXT(343)=4864
      IF(IR.EQ.65)IR=50
      IF(IR.EQ.65)IXT(344)=5065
      IF(IR.EQ.78)IR=29
      IF(IR.EQ.78)IXT(345)=2978
      IF(IR.EQ.79)IR=33
      IF(IR.EQ.79)IXT(346)=3379
      IF(IR.EQ.80)IR=29
      IF(IR.EQ.80)IXT(347)=2980
      ITST(1)=IR
      ITST(14)=IR
      write(6,930)
      IF(LX.NE.0)WRITE(LX,930)
  930 FORMAT(/" YOU DUG AND FELL INTO AN UNDERGROUND TUNNEL")
      GOTO 1141
C
C ****** NO RESPONSE ROUTINE ******
C
      data it_mm/2hMM/

  950 ITST(22)=ITST(22)+1
      IF(ITST(38).NE.it_mm) NAME(2)=is_sb
      IF(ITST(38).NE.it_mm) CALL exex(8,NAME,0,0,0,0,0)
      IF(ITST(22).EQ.1)CALL exex(8,NAME,LU,960,0,0,0)
      IF(ITST(22).EQ.2)CALL exex(8,NAME,LU,963,0,0,0)
      IF(ITST(22).EQ.3)CALL exex(8,NAME,LU,966,0,0,0)
      CALL exex(8,NAME,LU,970,0,0,0)
C
C *** FOLLOW ROUTINE ***
C
  970 IF (IWRD(5,2).NE.4.OR.IWRD(7,1).NE.2)
     1 CALL exex(8,NAME,LU,320,0,0,0)
      IF(IFSD(IRES(IWRD(6,2))).NE.IR)CALL exex(8,NAME,LU,980,0,0,0)
      GOTO 1105
C
C                            MM-C-4  (LINE 2480)
C 5-C
C ***** GO ROUTINE *****
C
 1000 IF(IWRD(7,1).GT.2)CALL exex(8,NAME,LU,320,0,0,0)
      IC=IWRD(6,2)
      IF(IWRD(7,1).EQ.1)IC=9
C
C *** GO CRAZY IN WOODS ROUTINE ***
C
      IRX=0
      IF(IWRD(6,2).EQ.110.AND.(IR.EQ.97.OR.IR.EQ.98))IRX=84
      IF(IRX.EQ.84)IR=84
      IF(IRX.EQ.84)ITST(1)=84
      IF(IRX.EQ.84)GOTO 1025
      IF((IWRD(7,1).EQ.2).AND.(IWRD(5,2).NE.2))
     1CALL exex(8,NAME,LU,1050,0,0,0)
C
C *** GO BACK ROUTINE ***
C
      IF((IC.EQ.11).AND.(ITST(5)-1.NE.ITST(4)-(ITST(4)/1000)
     1*1000.OR.IR.EQ.97.OR.IR.EQ.98))CALL exex(8,NAME,LU,1002,0,0,0)
      IF(IC.EQ.11)ITST(1)=ITST(14)
      IF(IC.EQ.11)ITST(14)=IR
      IF(IC.EQ.11)IR=ITST(1)
      IF(IC.EQ.11)GOTO 1025
C
C *** COMPASS ROUTINE ***
C
      IF(IC.LT.5.AND.((IR.LT.54..AND.IR.NE.35).OR.IR.EQ.98)
     1.AND.(IVEN(6).LT.20000))CALL exex(8,NAME,LU,1005,0,0,0)
      IF(IC.LT.7)GOTO 1007
      IC=IC-9+ITST(4)/1000
      IF(IC.GT.4)IC=IC-4
      IF(IC.LT.1)IC=IC+4
 1007 IFC=ITST(4)/1000
      IF(IC.LT.5)ITST(4)=ITST(4)-(ITST(4)/1000)*1000+IC*1000
C
C *** FIND EXIT ***
C
      DO 1010 J=IC*60-59,IC*60
      IF(IFSD(IXT(J)).EQ.IR)GOTO 1015
 1010 CONTINUE
      CALL exex(8,NAME,LU,1105,0,0,0)
C
C *** SMALL DOOR ***
C
 1015 IF(J.NE.93.AND.J.NE.212.AND.J.NE.281.AND.J.NE.351)GOTO 1020
      IF(IR.LT.88)GOTO 1020
      DO 1016 K=1,53
      IF(IVEN(K).GT.20000.AND.(J.EQ.281.OR.ITD(IVEN(K)).GT.1))
     1CALL exex(8,NAME,LU,1017,0,0,0)
 1016 CONTINUE
C
C *** IS THE DOOR LOCKED ***
C
 1020 IF((IR.LT.55.OR.IR.EQ.90).AND.(IXT(J).GT.20000))
     1CALL exex(8,NAME,LU,1021,0,0,0)
C
C *** ON PORCH ROUTINE ***
C
      IX=0
      IF(IR.EQ.91.AND.ITST(5)-1.NE.ITST(13))IX=99
      IF(IX.EQ.99)ITST(1)=2
      IF(IX.EQ.99)IR=2
      IF(IX.EQ.99)write(6,1023)
      IF(IX.EQ.99.AND.LX.NE.0)WRITE(LX,1023)
 1023 FORMAT(/" YOU JUST FELL THROUGH THE LOOSE BOARDS.")
      IF(IX.EQ.99)GOTO 1025
C
C                            MM-C-5  (LINE 2560)
C 6-C
C *** LADDER IN CORRIDOR ***
C
      R=RN(R)
      IF(R.GT..5.OR.IR.NE.39)GOTO 1024
      IF(R.GT..25)IR=41
      ITST(1)=IR
      write(6,1022)
      IF(LX.NE.0)WRITE(LX,1022)
 1022 FORMAT(/" YOU SLIPPED ON THE LADDER.")
      GOTO 1025
C
C *** IS GATE CLOSED ***
C
 1024 IF((IR.GT.54).AND.(IXT(J).GT.20000))
     1 CALL exex(8,NAME,LU,1023,0,0,0)
      IRB=ITST(1)
      ITST(14)=IRB
      ITST(1)=ITFD(IXT(J))
      IR=ITST(1)
 1025 ITST(4)=(ITST(4)/1000)*1000+ITST(5)
C
C                            MM-C-6  (LINE 2640)
C 7-C
C **** ROOM DEPENDENT ITEMS AFTER MOVE ****
C
C *** FALL ROUTINE ***
C
      IF(IR.EQ.0)CALL exex(8,NAME,LU,1026,0,0,0)
C
C *** WATER ROUTINE ***
C
      IF(IR.EQ.46.AND.IXT(320).EQ.0)CALL exex(8,NAME,LU,1027,0,0,0)
C
C *** NOTE ROUTINE ***
C
      IF(IVEN(22).EQ.0.AND.ITST(5)-(ITST(5)/50)*50.LT.10.AND.IR
     1.NE.35)IVEN(22)=100+IR
      IF((IVEN(22).NE.0).AND.(IVEN(22).LT.20000).AND.
     1(ITST(5)-(ITST(5)/50)*50.GT.40).AND.(IFSD(IVEN(22)).NE.
     2IR))IVEN(22)=0
C
C *** IN THE WOODS ROUTINE ***
C
      IF((IR.NE.97.AND.IR.NE.98).OR.IWRD(6,1).EQ.25)GOTO 1045
      DO 1030 I=1,2
      R=RN(R)
      DO 1030 J=1,INT(R*10+10)
      R1=RN(R)
      IC=IXT(60*INT(R1*4)+58+I)
      IF(IR.EQ.98.AND.ITST(17).GT.1.AND.IC.EQ.9798)IC=3598
      IF(IR.EQ.98.AND.ITST(17).LT.2.AND.IC.EQ.3598)IC=9798
      R=RN(R1)
      IXT(60*INT(R1*4)+58+I)=IXT(60*INT(R*4)+58+I)
      IXT(60*INT(R*4)+58+I)=IC
 1030 CONTINUE
C
C ** MOVE ITEMS **
C
      DO 1040 J=1,35
      IF((IVEN(J).GE. 10000).OR.((IFSD(IVEN(J)).NE.IR).AND.
     1(IFSD(IVEN(J)).NE.0)).OR.IVEN(J).EQ.0)GOTO 1040
      IF(IFSD(IVEN(J)).EQ.IR)IVEN(J)=IVEN(J)-IR
      IF(IFSD(IVEN(J)).GT.0)GOTO 1040
      R=RN(R)
      IF(R.GT..1)GOTO 1040
      IVEN(J)=IVEN(J)+IR
 1040 CONTINUE
C
C ** SET UP EXITS **
C
      IF(IXT(59).EQ.9497.AND.IXT(179).EQ.9897)IXT(59)=9597
      IF(IXT(119).EQ.9297.OR.IXT(119).EQ.9597)IXT(119)=9497
      IF(IXT(179).EQ.9497.AND.IXT(59).EQ.9997)IXT(179)=9297
      IF(IXT(239).EQ.9297.OR.IXT(239).EQ.9597)IXT(239)=9497
C
C *** CLOSE FRONT GATE IF WOODSMAN AND HUNTER ARE THERE ***
C
      IF(IRES(6).EQ.893.AND.IRES(12).EQ.993)IXT(54)=29293
      IF(IRES(6).EQ.892.AND.IRES(12).EQ.992)IXT(54)=29293
      IF(IXT(54).EQ.29293)IXT(234)=29392
C
C ** NYMPH ROUTINE **
C
      R=RN(R)
      IF(R.GT..2)GOTO 1045
      R=RN(R)
      DO 1042 J=INT(R*35+1),35
      IF(J.EQ.22)GOTO 1042
      IF(IVEN(J).GT.10000.OR.IFSD(IVEN(J)).NE.0.OR.IVEN(J).EQ.0)
     1GOTO 1042
      write(6,1041)
      IF(LX.NE.0)WRITE(LX,1041)
 1041 FORMAT(/" A WOOD NYMPH RAN BY CARRYING",$)
      CALL MMRI(4,J)
      GOTO 1045
 1042 CONTINUE
C
C                            MM-C-7  (LINE 2720)
C 8-C *** MOLE MAZE ROUTINE ***
C
C ** IS PLAYER IN MAZE AND MOLE ALIVE **
C
 1045 IC=1
      IF(IR.LT.42.OR.IR.GT.53.OR.IRES(4).GE.10000
     1.OR.IFSD(IRES(4)).EQ.52)GOTO 1080
C
C ** CHANGE ROOM MOLE IS IN ***
C
C * CHANGE AN EXIT *
C
      IMZ(1)=0
      IMZ(2)=0
      IMZ(3)=0
      IC=0
      RND=RND*7.7-INT(RND*7.7)
      IX=3*RND+1
      DO 1050 I=1,360
      IF(IFSD(IXT(I)).NE.IFSD(IRES(4)).OR.ITFD(IXT(I)).LT.42.OR.
     1ITFD(IXT(I)).GT.51)GOTO 1050
      IC=IC+1
      IMZ(IC)=I
      IF(IC.EQ.IX)GOTO 1055
 1050 CONTINUE
      GOTO 1080
 1055 RND=RND*7.7-INT(RND*7.7)
      IX=360*RND+1
      DO 1060 J=IX,360
      IF(IXT(J).LT.4100.OR.IFSD(IXT(J)).NE.0)GOTO 1060
      IF((IMZ(1)-1)/60.EQ.(J-1)/60.OR.(IMZ(2)-1)/60.EQ.(J-1)/60.OR.
     1(IMZ(3)-1)/60.EQ.(J-1)/60)GOTO 1060
      IXX=IXT(J)
      IXT(J)=IXT(I)
      IXT(I)=IXX
      GOTO 1075
 1060 CONTINUE
C
C * SECOND LOOP *
C
      DO 1070 J=1,IX
      IF(IXT(J).LT.4100.OR.IFSD(IXT(J)).NE.0)GOTO 1070
      DO 1070 K=1,3
      IF((IMZ(1)-1)/60.EQ.(J-1)/60.OR.(IMZ(2)-1)/60.EQ.(J-1)/60.OR.
     1(IMZ(3)-1)/60.EQ.(J-1)/60) GOTO 1070
      IXX=IXT(J)
      IXT(J)=IXT(I)
      IXT(I)=IXX
      GOTO 1075
 1070 CONTINUE
      GOTO 1080
 1075 RND=RND*7.7-INT(RND*7.7)
      IF(IC.LT.2.OR.RND.GT..3)GOTO 1080
      DO 1076 I=1,360
      IF(IFSD(IXT(I)).EQ.ITFD(IXT(J)).AND.ITFD(IXT(I)).EQ.IFSD(IXT(J)))
     1IXT(I)=100*(IXT(I)/100)
 1076 CONTINUE
      IXT(J)=100*(IXT(J)/100)
C
C *** CORRIDOR ROUTINE ***
C
 1080 IF((IR.GT.35).AND.(IR.LT.41))ITST(12)=ITST(12)-ITST(5)
     1+ITST(13)
      IF((IR.GT.35).AND.(IR.LT.41).AND.(ITST(12).GT.0))
     1write(6,1082)
      IF(LX.NE.0.AND.IR.GT.35.AND.IR.LT.41.AND.ITST(12).GT.0)
     1WRITE(LX,1082)
 1082 FORMAT(/" THE CORRIDOR WALLS ARE CLOSING IN")
      IF((IR.GT.35).AND.(IR.LT.40).AND.(IVEN(17).GT.20000)
     1.AND.(ITST(12).LE. 0))CALL exex(8,NAME,LU,1084,0,0,0)
      IF((IR.EQ.40).AND.(ITST(12).LE.0))CALL exex(8,NAME,LU,1086,0,0,0)
      IF((IR.GT.35).AND.(IR.LT.40).AND.(ITST(12).LE. 0))
     1CALL exex(8,NAME,LU,1088,0,0,0)
      IF((IR.EQ.4).OR.(IR.EQ.41))write(6,1089)
      IF(LX.NE.0.AND.(IR.EQ.4.OR.IR.EQ.41))WRITE(LX,1089)
 1089 FORMAT(/" THE CORRIDOR WALLS HAVE OPENED")
      IF((IR.EQ.4).OR.(IR.EQ.41))ITST(12)=4
      IF(IR.EQ.4)IXT(140)=3640
      IF(IR.EQ.4)IXT(314)=3940
C                            MM-C-8  (LINE 2800)
C 9-C
C   *** GARDEN CLOSED ROUTINE ***
C
      IF(ITST(5).LE.300.OR.IR.LT.55.OR.IR.GT.82)GOTO 1097
      R=RN(R)
      IF(R.GT..5.AND.R.LT..8)write(6,1091)
      IF(R.GT..8.AND.R.LT..95)write(6,1092)
      IF(R.GT..95)CALL exex(8,NAME,LU,1093,0,0,0)
 1091 FORMAT(/" YOU JUST NOTICED A SNAKE SLITHER BY.")
 1092 FORMAT(/" YOU WERE JUST BITTEN BY A SNAKE THAT RACED BY.")
C
C *** GRASSY MEADOW WOLF ROUTINE ***
C
 1097 IF((IR.EQ.71.OR.IR.EQ.72).AND.IRES(15).LT.10000)write(6,1098)
      IF(LX.NE.0.AND.(IR.EQ.71.OR.IR.EQ.72).AND.IRES(15).LT.10000)
     1WRITE(LX,1098)
      IF((IR.EQ.71.OR.IR.EQ.72).AND.IVEN(24).LT.20000.AND.IRES(15).LT.
     110000)CALL exex(8,NAME,LU,1097,0,0,0)
      IF((IR.EQ.71.OR.IR.EQ.72).AND.IRES(15).LT.10000
     1)CALL exex(8,NAME,LU,1096,0,0,0)
 1098 FORMAT(/" A WOLF IS RUNNING TOWARDS YOU")
C
C *** CLOSE SECRET PASSAGE ENTRANCES ***
C
      IF(IR.NE.1)IXT(1)=0
      IF(IR.NE.5)IXT(183)=0
      IF(IR.NE.7)IXT(123)=0
      IF(IR.NE.10)IXT(5)=0
      IF(IR.NE.14)IXT(187)=0
      IF(IR.NE.16)IXT(128)=0
      IF(IR.NE.19)IXT(11)=0
      IF(IR.NE.23)IXT(191)=0
      IF(IR.NE.25)IXT(134)=0
C
C *** HIGHWAY ***
C
      IF(IR.EQ.93)ITST(17)=0
      IF(IR.EQ.35.AND.IWRD(6,2).EQ.3)ITST(17)=ITST(17)+1
      IF(IR.EQ.35.AND.IWRD(6,2).EQ.1)ITST(17)=ITST(17)-1
      IF(IR.EQ.35.AND.(IFC+IWRD(6,2).EQ.10.OR.IFC
     1+IWRD(6,2).EQ.14))ITST(17)=ITST(17)-1
      IF(IR.EQ.35.AND.(IFC+IWRD(6,2).EQ.8.OR.IFC
     1+IWRD(6,2).EQ.12))ITST(17)=ITST(17)+1
      IF(ITST(17).EQ.1)IXT(47)=9335
      IF(ITST(17).EQ.2)IXT(47)=3535
      IC=100-ITST(17)/10
      ID=ITST(17)/10
      IF(IR.EQ.35.AND.ITST(17)/10.-ITST(17)/10.EQ..0.AND.ITST(5)
     1.LT.300)write(6,1099)IC,ID
      IF(IR.EQ.35.AND.ITST(17)/10.-ITST(17)/10.EQ..0.AND.ITST(5)
     1.LT.300.AND.LX.NE.0)WRITE(LX,1099)IC,ID
 1099 FORMAT(/" YOU PASSED A MILEAGE SIGN THAT READ: BIG CITY"I3,
     1"   MYSTERY MANSION"I3)
C
C *** DEMON ESCAPE ***
C
      IF(IFD(IVEN(35)).EQ.2.AND.ITD(IVEN(35)).EQ.6)NAME(2)=is_sg

      data it_sg/2hSG/

      IF(NAME(2).EQ.it_sg)IVEN(35)=IVEN(35)-100
      IF(NAME(2).EQ.it_sg)CALL exex(8,NAME,LU,10210,0,0,0)
C
C *** MOVE INHABITANTS ***
C
 1100 DO 1140 J=2,16
      IF(J.EQ.15)GOTO 1140
      IF(IFSD(IRES(J)).EQ.0.OR.IRES(J).GT.10000)GOTO 1140
      IF(IFSD(IRES(J)).EQ.IRB)GOTO 1120
C
C ** MOVE INHABITANT IF ALONE **
C
 1105 R=RN(R)
      DO 1110 K=INT(R*6)*60+1,INT(R*6)*60+60
      IF(IFSD(IXT(K)).NE.IFSD(IRES(J)))GOTO 1110
      IF(IXT(K).GT.20000)GOTO 1140
      IRC=IFSD(IRES(J))
      IRES(J)=(IRES(J)/100)*100+ITFD(IXT(K))
      GOTO 1130
 1110 CONTINUE
      GOTO 1140
C
C                            MM-C-9  (LINE 2880)
C 10-C
C ** MOVE INHABITANT WITH PLAYER **
C
 1120 IRES(J)=IRES(J)-IRB+IR
      IRC=IRB
C
C ** CAN INHABITANT MOVE THERE **
C
 1130 IC=IRES(J)-(IRES(J)/100)*100
C
C * BUTLER AND MASTER *
C
      IF((J.EQ.3.OR.J.EQ.14).AND.(IC.GT.26.AND.(ITST(5).LT.400.OR.IFSD
     1(J).NE.IR).OR.IC.EQ.0))IRES(J)=100*(IRES(J)/100)+IRC
C * MOLE *
C
      IF(J.EQ.4.AND.(IC.GT.51.OR.IC.LT.43))IRES(4)=100*
     1(IRES(4)/100)+IRC
C * VAMPIRE *
C
      IF(J.EQ.5.AND.(IC.EQ.16.OR.(IROM(IC).GE.20000.AND.ITST(5).LT.300)
     1.OR.IC.GT.27.OR.IC.EQ.21))IRES(5)=100*(IRES(5)/100)+IRC
C * WOODSMAN AND HUNTER *
C
      IF((J.EQ.6.OR.J.EQ.12).AND.(IC.LT.92.OR.IC.EQ.98))IRES(J)=
     1100*(IRES(J)/100)+IRC
C * MAID AND LADY *
C
      IF((J.EQ.8.OR.J.EQ.2).AND.(IC.LT.9.OR.IC.GT.77))IRES(J)=
     1100*(IRES(J)/100)+IRC
C * COOK AND GARDENER *
C
      IF((J.EQ.9.OR.J.EQ.7).AND.((IC.GT.18.AND.IC.LT.54).OR.IC.GT.77))
     1IRES(J)=100*(IRES(J)/100)+IRC
C * WEREWOLF *
C
      IF(J.EQ.10.AND.(IC.EQ.98.OR.IC.EQ.78.OR.IC.EQ.80.OR.IC.EQ.88.OR.
     1IC.EQ.83.OR.IC.EQ.0))IRES(10)=100*(IRES(10)/100)+IRC
C * DWARF *
C
      IF(J.EQ.13.AND.(IC.LT.29.OR.IC.GT.31))IRES(13)=
     1100*(IRES(13)/100)+IRC
C * ELF *
C
      IF(J.EQ.11.AND.(IC.LT.55.OR.IC.GT.77))IRES(11)=
     1100*(IRES(11)/100)+IRC
C * WARRIOR *
C
      IF(J.EQ.16.AND.(IC.LT.93.OR.IC.GT.96))IRES(16)=
     1100*(IRES(16)/100)+IRC
 1140 CONTINUE
 1141 CONTINUE
      ITST(13)=ITST(5)
      IF(IWRD(6,1).EQ.13)IR=IFSD(IRES(IWRD(6,2)))
      ITST(1)=IR
C
C *** MYSTERY SOLUTION ***
C
      IF(IR.EQ.ITST(8).AND.IFSD(IRES(ITST(9))).EQ.IR.AND.(IVEN(ITST(10))
     1.GT.20000.OR.(IFSD(IVEN(ITST(10))).EQ.IR.AND.IVEN(ITST(10)).LT.
     A10000)))CALL exex(8,NAME,LU,1145,0,0,0)
C
C                            MM-C-10 (LINE 2960)
C 11-C
C *** IS IT LIGHT HERE ***
C
C ** IS THERE A LIGHT SOURCE **
C
C * LANTERN, CANDLE OR TORCH ON AND HERE OR WITH PLAYER OR INHABITANT *
C
 1142 DO 1150 I=1,3
      IF(I.EQ.1)K=4
      IF(I.EQ.2)K=19
      IF(I.EQ.3)K=27
      IF(IFD(IVEN(K)).EQ.1.AND.((IVEN(K).LT.10000.AND.IFSD(IVEN(K))
     1.EQ.IR).OR.IVEN(K).GT.20000.OR.(IVEN(K).GT.10000.AND.IVEN(K)
     2.LT.20000.AND.IFSD(IRES(IFSD(IVEN(K)))).EQ.IR))) GOTO 1190
C
 1150 CONTINUE
C
C * LAMP *
C
      IF((IR.EQ.10).AND.(IVEN(45).GT.1000))GOTO 1190
C
C * FIREPLACE *
C
      IF((IR.EQ.14).AND.(IVEN(89).GT.1000))GOTO 1190
C
C * MATCH *
C
      IF(IVEN(31).GT.21000)GOTO 1190
C
C ** IS HE OUTSIDE AND DAYTIME **
C
      IF((IR.GT.53).AND.(ITST(5).LT.300))GOTO 1190
      IF(IR.GT.53)CALL exex(8,NAME,LU,1150,1,0,0)
C
C ** IS THERE AN OPEN CURTAIN **
C
      IF((IROM(IR).GT.20000).AND.(ITST(5).LT.300))GOTO 1190
      IF(IROM(IR).GT.20000)CALL exex(8,NAME,LU,1150,1,0,0)
      IF((ITST(5).LT.300).AND.(IROM(IR).GT.10000))
     1CALL exex(8,NAME,LU,1150,2,0,0)
      CALL exex(8,NAME,LU,1150,0,0,0)
C
C *** COMPUTE SCORE ***
C
 1190 ISC=ITFD(IROM(IR))
      ITST(2)=ITST(2)+ISC-50
      IROM(IR)=IROM(IR)-(ISC*100)+5000
      IF(ISC.LT.50)ITST(3)=ITST(3)-50+ISC
C *** PRINT WHAT ROOM HE IS IN ***
C
      IC=1
      IF(IR.EQ.35.OR.IR.EQ.54.OR.IR.EQ.71.OR.IR.EQ.72.OR.
     1IR.EQ.81.OR.IR.EQ.82.OR.IR.EQ.85.OR.IR.EQ.94.OR.IR.EQ.96.OR.
     2(IR.EQ.86).OR.(IR.EQ.92).OR.(IR.EQ.91))IC=2
      IF(((IR.GE.73).AND.(IR.LE.77)).OR.(IR.EQ.99))IC=3
      IF(((IR.GE.55).AND.(IR.LE.57)).OR.(IR.EQ.93).OR.
     1(IR.EQ.95))IC=4
      write(6,1200)IPRP(1,IC),(IRNM(L,IFSD(IROM(IR))),L=1,8)
      IF(LX.NE.0)WRITE(LX,1200)IPRP(1,IC),(IRNM(L,IFSD(IROM(IR)))
     1,L=1,8)
 1200 FORMAT(/" YOU ARE "A2" THE "8A2)
C
C * BLOW OUT CANDLE *
C
      IF(IVEN(19).GT.21000.AND.(IR.EQ.21.OR.IR.EQ.36.OR.IR.EQ.54
     1.OR.IR.EQ.92))CALL exex(8,NAME,LU,213,0,0,0)
C
C                            MM-C-11 (LINE 3040)
C 12-C
C *** MURDER ROOM ***
C
      IF((ITST(8).EQ.IR).AND.(ITST(10).GT.8).AND.(ITST(10)
     1.LT.13))write(6,1300)
      IF((ITST(8).EQ.IR).AND.(ITST(10).GT.8).AND.(ITST(10)
     1.LT.13).AND.LX.NE.0)WRITE(LX,1300)
 1300 FORMAT(/" THERE IS BLOOD HERE")
C
C *** TELL WHAT IS THERE ***
C
      CALL MMRI(1,0)
C
C *** TELL WHO IS THERE ***
C
      IC=0
      DO 1510 J=1,17
      IF(IR.NE.IFSD(IRES(J)))GOTO 1510
      IF(IC.EQ.0)write(6,1500)(IRSN(L,J),L=1,4)
      IF(LX.NE.0.AND.IC.EQ.0)WRITE(LX,1500)(IRSN(L,J),L=1,4)
      IF(IC.EQ.1)write(6,1505)(IRSN(L,J),L=1,4)
      IF(LX.NE.0.AND.IC.EQ.1)WRITE(LX,1505)(IRSN(L,J),L=1,4)
 1500 FORMAT(/" YOU ARE HERE WITH: "4A2)
 1505 FORMAT(20X,4A2)
      IC=1
 1510 CONTINUE
C
C *** TELL A WAY HE CAN GO ***
C
      IF(ITST(5).LT.450.AND.(IR.LT.28.OR.IR.GT.64.OR.(IR.GT.34.AND.
     1IR.LT.42).OR.(IR.GT.52.AND.IR.LT.58)))GOTO 1620
      J2=INT(R*240+1)
 1585 DO 1610 J=J2,360
      IF(IFSD(IXT(J)).NE.IR)GOTO 1610
      J1=(J-1)/60+1
C
C *** IF NOT CARDINAL ***
C
      IF((IR.GT.53.AND.IR.NE.98).OR.IVEN(6).GT.20000.OR.IR.EQ.35.
     1.OR.(J1.GT.4))GOTO 1590
      J1=9+J1-ITST(4)/1000
      IF(J1.GT.10)J1=J1-4
      IF(J1.LT.7)J1=J1+4
 1590 write(6,1600)(IDTN(L,J1),L=1,4)
      IF(LX.NE.0)WRITE(LX,1600)(IDTN(L,J1),L=1,4)
 1600 FORMAT(/" THERE IS A WAY TO GO ",4A2)
      GOTO 1620
 1610 CONTINUE
      IF(J2.EQ.1)GOTO 1620
      J2=1
      GOTO 1585
 1620 CONTINUE
C
C*** GIVE LONG DESCRIPTION IF LOOKING OR FIRST TIME THERE ***
C
      IF(ISC.NE.50.OR.IWRD(6,1).EQ.25)NAME(2)=is_se

      data it_se/2hSE/

      IF(NAME(2).EQ.it_se)CALL exex(8,NAME,LU,IR,0,0,0)
      NAME(2)=is_sb
      CALL exex(8,NAME,0,0,0,0,0)
C
      END

