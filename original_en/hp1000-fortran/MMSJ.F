C
C                            MM-I-10  (LINE 8560)
C 1-J
      subroutine MMSJ
C
C ******* SEGMENT J *******
C
      include 'common.i'
C
      DIMENSION KEYM(9)
C
C ***** THIS SEGMENT IS CALLED BY SEGMENT B TO PERFORM THE FUNCTION
C       OF THE HIGHER NUMBERED VERBS USED. THIS SEGMENT NORMALLY
C       COMPLETES BY CALLING SEGMENT K TO WRITE AN APPROPRIATE MESSAGE.
C
C *** FUNCTIONS ***
C
C * FIRST & SECOND DIGITS, THIRD & FOURTH DIGITS, THIRD DIGIT,
C   FOURTH DIGIT AND RANDOM NUMBER
C
      IFSD(I)=I-(I/100)*100
      ITFD(I)=I/100-(I/10000)*100
      ITD(I)=I/100-(I/1000)*10
      IFD(I)=I/1000-(I/10000)*10
      RN(R)=R*7.7-INT(R*7.7)
C
C *** SET UP VARIABLES ***
C
      NAME(2)=is_sk
      LU=IPR(1)
      LX=ITST(21)
      IR=ITST(1)
      R=RND
C
C ***** INDEX OF VERB ROUTINES *****
C
C       ROUTINE               PAGE
C
C       UNLOCK AND LOCK      2
C
C       SHOOT                3
C
C       OIL                  4
C
C       RECORD               5
C
C       BREAK                6
C
C       PLACE                6
C
C       SLEEP                7
C
C                            MM-J-1  (LINE 8640)
C 2-J
C
C ***** SELECT THE RIGHT VERB ROUTINE *****
C
      GOTO(1000,1000,2000,3000,4000,5000,6000,7000)IWRD(6,1)-53
C
C ****** UNLOCK AND LOCK ROUTINE ******
C
C *** CHECK FORMAT ***
C
 1000 IF(IWRD(7,1).EQ.1)CALL exex(8,NAME,LU,1000,0,0,0)
      IF(IWRD(7,1).NE.2.OR.IWRD(5,2).NE.3)
     1CALL exex(8,NAME,LU,1000,0,0,0)
C
C *** DOOR ***
C
      IF(IWRD(6,2).NE.92) GOTO 15100
      IF((IR.GT.27.AND.IR.NE.36.AND.IR.NE.54.AND.IR.NE.91.AND.IR.NE.
     190).OR.IR.EQ.3.OR.IR.EQ.4.OR.IR.EQ.13.OR.IR.EQ.16.OR.IR.EQ.21
     2.OR.IR.EQ.22)CALL exex(8,NAME,LU,2030,0,0,0)
      DO 15010 J=1,240
      IF(IFSD(IXT(J)) .NE. IR) GOTO 15010
      IF((J-1)/60+1 .NE. ITST(4)/1000) GOTO 15010
      IC=ITFD(IXT(J))
      IF(IC.EQ.4.OR.IC.EQ.13.OR.IC.EQ.22.OR.J.EQ.62.OR.J.EQ.70.OR.J
     1.EQ.133.OR.J.EQ.182.OR.J.EQ.190)CALL exex(8,NAME,LU,15030,0,0,0)
      IF(IXT(J) .LT. 10000) CALL exex(8,NAME,LU,15020,0,0,0)
      IF(((IXT(J) .LT. 20000) .AND. (IWRD(6,1) .EQ. 54)) .OR.
     1((IXT(J) .GT. 20000) .AND. (IWRD(6,1) .EQ. 55)))
     2CALL exex(8,NAME,LU,8020,0,0,0)
      IF(IVEN(46).LT.20000.AND.J.EQ.64)CALL exex(8,NAME,LU,15041,0,0,0)
      IF(IVEN(2) .LT. 20000.AND.J.NE.64)
     1 CALL exex(8,NAME,LU,15040,0,0,0)
      IF(IWRD(6,1) .EQ. 54) IXT(J)=IXT(J)-10000
      IF(IWRD(6,1) .EQ. 55) IXT(J)=IXT(J)+10000
      IF((J.LT.121).AND.(IWRD(6,1).EQ.54))IXT(J+120)=IXT(J+120)-10000
      IF((J.GT.120).AND.(IWRD(6,1).EQ.54))IXT(J-120)=IXT(J-120)-10000
      IF((J.LT.121).AND.(IWRD(6,1).EQ.55))IXT(J+120)=IXT(J+120)+10000
      IF((J.GT.120).AND.(IWRD(6,1).EQ.55))IXT(J-120)=IXT(J-120)+10000
      CALL exex(8,NAME,LU,1040,0,0,0)
15010 CONTINUE
      CALL exex(8,NAME,LU,15030,0,0,0)
C
C *** CHEST ***
C
15100 IF(IWRD(6,2).NE.54.AND.IWRD(6,2).NE.35) GOTO 15200
      IF(IVEN(35).LT.20000.AND.IVEN(35).NE.600+IR)
     1 CALL exex(8,NAME,LU,8030,0,0,0)
C
C ** LOCK CHEST **
C
      IF(IWRD(6,1).EQ.55.AND.IFD(IVEN(35)).EQ.0)
     1CALL exex(8,NAME,LU,8020,0,0,0)
      IF(IWRD(6,1).EQ.55)CALL exex(8,NAME,LU,2010,0,0,0)
C
C ** UNLOCK CHEST **
C
      IF(IVEN(2).LT.20000)CALL exex(8,NAME,LU,15040,0,0,0)
      CALL exex(8,NAME,LU,15110,0,0,0)
C
15200 CONTINUE
C
15900 CALL exex(8,NAME,LU,8010,0,0,0)
C
C                            MM-J-2  (LINE 8720)
C 3-J
C
C ****** SHOOT ROUTINE ******
C
 2000 IF(IWRD(7,1).EQ.1)CALL exex(8,NAME,LU,1000,0,0,0)
      IF(IWRD(7,1).NE.2.OR.(IWRD(5,2).NE.4.AND.IWRD(5,2).NE.3).OR.
     1(IWRD(5,2).EQ.3.AND.IWRD(6,2).NE.54))
     2 CALL exex(8,NAME,LU,320,0,0,0)
C
C *** DOES HE HAVE A GUN ***
C
      IF (IVEN(9) .LT. 20000) CALL exex(8,NAME,LU,16010,0,0,0)
C
C *** IS IT LIGHT HERE ***
C
      CALL MMRL
C
C *** IS INHABITANT OR PADLOCK HERE ***
C
16019 IF(IFSD(IRES(IWRD(6,2))).NE.IR.AND.IWRD(5,2).NE.3)
     1CALL exex(8,NAME,LU,2030,0,0,0)
      IF(IWRD(5,2).EQ.3.AND.IFD(IVEN(35)).NE.0)
     1 CALL exex(8,NAME,LU,2030,0,0,0)
C
C *** DOES HE HAVE BULLETS ***
C
      IC=0
      IF(IVEN(20).GT.20000)IC=1
      IF(IWRD(6,2).EQ.10)GOTO 16020
      IF((ITST(11) .EQ. 0) .AND. (IVEN(20) .LT. 20000))
     1CALL exex(8,NAME,LU,16020,0,0,0)
      IF((ITST(11).EQ.0.OR.IWRD(6,2).EQ.10).AND.IVEN(20).GT.20000)
     1IVEN(20)=0
      IF(((ITST(11).GT.0).AND.(IWRD(6,2).NE.10)) .OR. ((IWRD(6,2)
     1.EQ.10).AND.(IVEN(20).LT.20000)))ITST(11)=ITST(11)-1
C
C *** MISS AND WOUND ROUTINE ***
C
      R=RN(R)
      IF(R .LT. .2) CALL exex(8,NAME,LU,16030,0,0,0)
      IF(R.LT..4.AND.IRES(IWRD(6,2))/1000.EQ.0)IRES(IWRD(6,2))=
     1IRES(IWRD(6,2))+5000
      IF(R.LT..4.AND.IRES(IWRD(6,2))/1000.EQ.0)
     1CALL exex(8,NAME,LU,16031,0,0,0)
C
C *** PADLOCK ***
C
      IF(IWRD(5,2).EQ.3)CALL exex(8,NAME,LU,16035,0,0,0)
C
C *** CAN INHABITANT BE KILLED ***
C
      IF((IWRD(6,2) .EQ. 1) .OR. (IWRD(6,2)
     1.EQ. 5) .OR. ((IWRD(6,2) .EQ. 10) .AND. (IC .EQ. 0))
     2.OR.(IRES(IWRD(6,2)).GT.10000))
     1 CALL exex(8,NAME,LU,16040,0,0,0)
C
C *** DWARF ROUTINE ***
C
      IF(IWRD(6,2).EQ.13.AND.IR.NE.33) IRES(13)=533
      IF(IWRD(6,2).EQ.13.AND.IR.EQ.33) IRES(13)=534
      IF(IWRD(6,2).EQ.13)CALL exex(8,NAME,LU,16045,0,0,0)
C
C *** WEREWOLF ***
C
16020 IF(IWRD(6,2).EQ.10)IRES(ITST(39))=10000+IR
      IF(IWRD(6,2).EQ.10)ITST(2)=ITST(2)+75
C
C *** WARRIOR ***
C
      IF(IWRD(6,2).EQ.16.AND.R.LT..8.AND.IVEN(45).EQ.14416.AND.
     1IRES(16)/1000.LT.4)CALL exex(8,NAME,LU,16046,0,0,0)
C
C *** KILL INHABITANT ***
C
      IRES(IWRD(6,2))=10000+IR
      IF(IWRD(6,2).EQ.10)IRES(10)=0
      CALL exex(8,NAME,LU,16050,0,0,0)
C
C                            MM-J-3  (LINE 8800)
C 4-J
C
C ****** OIL ROUTINE ******
C
C *** CHECK FORMAT ***
C
 3000 IF((IWRD(7,1).NE.2).OR.(IWRD(5,2).NE.3))
     1 CALL exex(8,NAME,LU,320,0,0,0)
C
C *** IS IT LIGHT ***
C
      CALL MMRL
C
C *** DOES HE HAVE THE OILCAN ***
C
      IF(IVEN(33) .LT. 20000) CALL exex(8,NAME,LU,20010,0,0,0)
C
C *** DOES CAN HAVE OIL ***
C
      IF(IVEN(33) .GT. 22000) CALL exex(8,NAME,LU,20020,0,0,0)
C
C *** DOES HE HAVE ITEM ***
C
      IF((IWRD(6,2) .LT. 54) .AND. (IVEN(IWRD(6,2)) .LT. 20000))
     1CALL exex(8,NAME,LU,3010,0,0,0)
C
C *** IS HE NEAR IT ***
C
      IF((IWRD(6,2) .GT. 47) .AND. (IFSD(IVEN(IWRD(6,2))) .NE. IR)
     1.AND. (IFSD(IVEN(IWRD(6,2))) .NE. 0))
     2 CALL exex(8,NAME,LU,2030,0,0,0)
      IF((IWRD(6,2) .GT. 45) .AND. (IWRD(6,2) .LT. 51))
     1CALL exex(8,NAME,LU,20030,0,0,0)
      IF((IWRD(6,2) .EQ. 91) .AND. (IROM(IR) .LT. 10000))
     1CALL exex(8,NAME,LU,2030,0,0,0)
      IF((IWRD(6,2) .EQ. 92) .OR. (IWRD(6,2) .EQ. 93))
     1 CALL exex(8,NAME,LU,8020,0,0,0)
C
C * HINGE *
C
      IF((IWRD(6,2) .EQ. 74) .AND. (IR .EQ. 85)) IXT(322)=9185
      IF((IWRD(6,2) .EQ. 74) .AND. (IR .EQ. 86)) IXT(322)=8586
C
      IVEN(74)=IVEN(74)+1000
      CALL exex(8,NAME,LU,1040,0,0,0)
C
C                            MM-J-4  (LINE 8880)
C 5-J
C  ****** RECORD ROUTINE ******
C
 4000 IF(IWRD(7,1).GT.1)GOTO 21200
      write(6,21010)
21010 FORMAT(/" ENSURE UNIT IS READY TO RECORD AND ENTER THE LU #: ",$)
      read(5,*)IANS
      IF(LX.NE.0)WRITE(LX,21011)IANS
21011 FORMAT(/" ENSURE UNIT IS READY TO RECORD AND ENTER THE LU #: "I3)
      ITST(21)=IANS
      CALL exex(8,NAME,LU,21020,0,0,0)
C
C *** KEY ***
C
21200 IF(IWRD(7,1).EQ.3.AND.IWRD(5,2).EQ.5.AND.(IWRD(6,2).EQ.7.OR.
     1IWRD(6,2).EQ.2).AND.IWRD(6,3).EQ.105)GOTO 22000
      IF(IWRD(7,1).EQ.3.AND.IWRD(5,2).EQ.5.AND.IWRD(6,2).EQ.2.AND.
     1IWRD(6,3).EQ.104)ITST(21)=6
      IF(IWRD(7,1).EQ.3.AND.IWRD(5,2).EQ.5.AND.IWRD(6,2).EQ.2.AND.
     1IWRD(6,3).EQ.104)CALL exex(8,NAME,LU,21020,0,0,0)
      IF(IWRD(7,1).NE.2.OR.IWRD(5,2).NE.3.OR.(IWRD(6,2).NE.2.AND.
     1IWRD(6,2).NE.46))CALL exex(8,NAME,LU,320,0,0,0)
21205 write(6,21210)
21210 FORMAT(/" ENTER 0 TO END, 9 TO CLEAR OR KEY # AND MESSAGE",$)
      read(5,21220)IANS,KEYM
21220 FORMAT(10A2)
      IANS=IANS/256-6*8
      IF(IANS.EQ.0)CALL exex(8,NAME,LU,1040,0,0,0)
      IF(IANS.EQ.9)GOTO 21260
      IF(IANS.LT.1.OR.IANS.GT.8)GOTO 21205
      write(6,21230)
21230 FORMAT(" ")
      DO 21245 J=1,8
      IF(J.NE.IANS)write(6,21236)
21236 FORMAT(" ")
      IF(IANS.EQ.J.AND.J.LT.4)write(6,21240)KEYM
21240 FORMAT(" ",9A2," ")
      IF(IANS.EQ.J.AND.J.GT.4.AND.J.LT.8)write(6,21241)KEYM
21241 FORMAT(" ",9A2," ")
      IF(IANS.EQ.J.AND.J.EQ.4)write(6,21242)KEYM
21242 FORMAT(" ",9A2," ")
      IF(IANS.EQ.J.AND.J.EQ.8)write(6,21243)KEYM
21243 FORMAT(" ",9A2," ")
21245 CONTINUE
      write(6,21250)
21250 FORMAT(" ")
      data it_bb/2h  /
      IANX=it_bb  
      write(6,21251)IANX,KEYM
21251 FORMAT(" "A2" "9A2)
      write(6,21252)
21252 FORMAT(" ")
      GOTO 21205
21260 write(6,21261)
21261 FORMAT(" ")
      DO 21265 I=1,11
      IF(I.LT.5)write(6,21262)
      IF(I.GT.4.AND.I.LT.9)write(6,21263)
21262 FORMAT(" ")
21263 FORMAT(" ")
      IF(I.GT.8)write(6,21264)
21264 FORMAT("                    ")
21265 CONTINUE
      write(6,21266)
21266 FORMAT("                   ")
      DO 21268 I=1,8
      IANX=it_bb  
      IANY=it_bb  
      write(6,21267)IANX,IANY
21267 FORMAT(" "A2" ")
21268 CONTINUE
      GOTO 21205
C
C                            MM-J-5  (LINE 8960)
C 6-J
C
C *** CASSETTE RECORD ***
C
22000 write(6,22010)
22010 FORMAT(/" ENTER 0 TO ABORT OR LU OF TAPE UNIT."//"?",$)
      read(5,*)LA
      IF(IWRD(6,2).EQ.7)ITST(36)=LA
      IF(IWRD(6,2).EQ.7)ITST(37)=0
      IF(IWRD(6,2).EQ.2)ITST(37)=LA
      IF(IWRD(6,2).EQ.2)ITST(36)=0
      CALL exex(8,NAME,LU,1040,0,0,0)
C
C ****** BREAK ROUTINE ******
C
 5000 IF(IWRD(7,1).NE.2.OR.IWRD(5,2).NE.3.OR.IWRD(6,2).NE.54)
     1CALL exex(8,NAME,LU,320,0,0,0)
C
C *** IS IT LIGHT ***
C
      CALL MMRL
C
C *** IS IT ALREADY BROKEN ***
C
      IF(IFD(IVEN(35)).EQ.1)CALL exex(8,NAME,LU,8020,0,0,0)
C
C *** ARE CONDITIONS RIGHT ***
C
      IF(IFSD(IVEN(35)).NE.IR.OR.IVEN(35).GT.10000)
     1CALL exex(8,NAME,LU,22010,0,0,0)
C
      IF(IVEN(11).LT.20000.AND.IVEN(12).LT.20000.AND.IVEN(14).LT.
     120000.AND.IVEN(32).LT.20000.AND.IVEN(34).LT.20000)
     2CALL exex(8,NAME,LU,22020,0,0,0)
      CALL exex(8,NAME,LU,16035,0,0,0)
C
C ******* PLACE ROUTINE *******
C
 6000 IF(IWRD(7,1).NE.4.OR.IWRD(5,2).NE.3.OR.IWRD(5,3).NE.5.OR.
     1IWRD(5,4).NE.3)CALL exex(8,NAME,LU,320,0,0,0)
C
C *** AMULET ***
C
      IF(IWRD(6,2).NE.17.OR.IWRD(6,3).NE.1.OR.IWRD(6,4).NE.66)GOTO 6100
      IF(IVEN(17).LT.20000)CALL exex(8,NAME,LU,3010,0,0,0)
      IF(IR.NE.41)CALL exex(8,NAME,LU,6010,0,0,0)
      IVEN(17)=1141
      CALL exex(8,NAME,LU,6020,0,0,0)
C
C *** KEY ***
C
 6100 IF(IWRD(6,2).NE.46.OR.IWRD(6,3).NE.2.OR.IWRD(6,4).NE.85)GOTO 6200
      IF(IVEN(46).LT.20000)CALL exex(8,NAME,LU,3010,0,0,0)
      IF(IR.NE.1)CALL exex(8,NAME,LU,6010,0,0,0)
      IVEN(46)=1101
      CALL exex(8,NAME,LU,1040,0,0,0)
C
C *** CROSS ***
C
 6200 IF(IWRD(6,2).NE.5.OR.IWRD(6,3).NE.2.OR.IWRD(6,4).NE.77)GOTO 6300
      IF(IVEN(5).LT.20000)CALL exex(8,NAME,LU,3010,0,0,0)
      IF(IR.NE.16)CALL exex(8,NAME,LU,6010,0,0,0)
      IVEN(5)=1316
      CALL exex(8,NAME,LU,1040,0,0,0)
C
C *** LANTERN ***
C
 6300 IF(IWRD(6,2).NE.4.OR.IWRD(6,3).NE.2.OR.IWRD(6,4).NE.85)GOTO 6900
      IF(IVEN(4).LT.20000)CALL exex(8,NAME,LU,3010,0,0,0)
      IF(IR.NE.93)CALL exex(8,NAME,LU,6010,0,0,0)
      IF(IFD(IVEN(4)).EQ.1)IVEN(4)=1493
      IF(IFD(IVEN(4)).NE.1)IVEN(4)=493
      CALL exex(8,NAME,LU,1040,0,0,0)
C
 6900 CALL exex(8,NAME,LU,320,0,0,0)
C
C                            MM-J-6  (LINE 9040)
C 7-J
C
C ****** SLEEP ROUTINE ******
C
 7000 IF(IWRD(7,1).EQ.1)GOTO 7500
C
C **** WITH SOMEONE ****
C
      IF(IWRD(7,1).NE.3.OR.IWRD(5,2).NE.5.OR.IWRD(6,2).NE.13.OR.
     1IWRD(5,3).NE.4)CALL exex(8,NAME,LU,320,0,0,0)
C
C *** IS THE INHABITANT HERE ***
C
      IF(IFSD(IRES(IWRD(6,3))).NE.IR)
     1 CALLexex(8,NAME,LU,8031,0,0,0)
C
C *** IS IT PERVERTED ***
C
      IF(IWRD(6,3).EQ.1.OR.IWRD(6,3).EQ.15.OR.IWRD(6,3).EQ.4)
     1CALL exex(8,NAME,LU,7010,0,0,0)
C
C *** IS IT THE RIGHT PLACE ***
C
      IF(IR.NE.1.AND.IR.NE.20.AND.IR.NE.24.AND.IR.NE.25)
     1CALL exex(8,NAME,LU,7020,0,0,0)
C
C *** IS THE INHABITANT IN THE MOOD ***
C
      IF(ITST(32).NE.1.OR.(IR.EQ.1.AND.IXT(61).LT.20000).OR.
     1(IR.EQ.20.AND.(IXT(12).LT.20000.OR.IROM(20).GT.20000)).OR.
     2(IR.EQ.25.AND.(IXT(73).LT.20000.OR.IROM(25).GT.20000)).OR.
     3IR.EQ.24)CALL exex(8,NAME,LU,7030,0,0,0)
      DO 7035 I=1,53
      IF(IVEN(I).GE.20000)CALL exex(8,NAME,LU,7030,0,0,0)
 7035 CONTINUE
      DO 7050 I=1,17
      IF(IFSD(IRES(I)).EQ.IR.AND.IWRD(6,3).NE.I)
     1CALL exex(8,NAME,LU,7030,0,0,0)
 7050 CONTINUE
C
C *** DO IT ***
C
      IF(IWRD(6,3).EQ.3.OR.IWRD(6,3).EQ.7.OR.IWRD(6,3).EQ.14)ITST(32)=2
      IF(IWRD(6,3).EQ.2.OR.IWRD(6,3).EQ.8.OR.IWRD(6,3).EQ.9)ITST(32)=3
      ITST(5)=ITST(5)+25
      CALL exex(8,NAME,LU,7040,0,0,0)
C
C **** BY SELF ****
C
C *** IS HE COMFORTABLE ***
C
 7500 IF (IR.NE.1.AND.IR.NE.10.AND.IR.NE.20.AND.IR.NE.24.AND.IR.
     1NE.25)CALL exex(8,NAME,LU,7510,0,0,0)
C
C *** IS SOMEONE NEARBY ***
C
      DO 7520 I=2,17
      IF(IFSD(IRES(I)).EQ.IR)CALL exex(8,NAME,LU,7530,0,0,0)
 7520 CONTINUE
C
C *** SLEEP ***
C
      ITST(5)=ITST(5)+25
      CALL exex(8,NAME,LU,7540,0,0,0)
C
      END

