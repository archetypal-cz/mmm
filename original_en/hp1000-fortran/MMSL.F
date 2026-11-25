C
C                            MM-K-4  (LINE 9440))
C 1-L
      subroutine MMSL
C
C ******* SEGMENT L *******
C
C ***** THIS SEGMENT IS DESIGNED MOSTLY AS A DEBUG ROUTINE BUT CAN
C       OBVIOUSLY BE USED TO CHEAT.
C
C ****** DISPLAY IS ENTERED BY ENTERING YES XX, WHERE XX = 99-CODE #
C
      include 'common.i'
C
      DIMENSION IDCB(144),NAMF(3),ITIM(5),IDTL(18,32),NAMT(3),
     1ICOM(64,30),IRM(8)
C
C *** FUNCTIONS ***
C
C * FIRST & SECOND, THIRD & FOURTH , THIRD AND FOURTH DIGITS
C
      IFSD(I)=I-(I/100)*100
      ITFD(I)=I/100-(I/10000)*100
      ITD(I)=I/100-(I/1000)*10
      IFD(I)=I/1000-(I/10000)*10
C
C *** SET UP VARIABLES ***
C
      NAME(2)=is_sb
      LU=IPR(1)
      LX=ITST(21)
      IR=ITST(1)
      DATA NAMT/2HMM,2HTL,2HDA/
      IF(ITST(25).EQ.-1)GOTO 9805
      ITST(25)=0
C
C ****** DISPLAY ROUTINE ******
C
      write(6,1000)
 1000 FORMAT(" ")
 9035 write(6,9040)
      IF(LX.NE.0)WRITE(LX,9040)
 9040 FORMAT(/" 0   DONE           1   ITEMS          2   ROOMS 1-33"/
     1" 3   ROOMS 34-66    4   ROOMS 67-99    5   EXITS NORTH"/
     2" 6   EXITS EAST     7   EXITS SOUTH    8   EXITS WEST "/
     3" 9   EXITS UP       10  EXITS DOWN     11  INHABITANTS"/
     4" 12  VERBS          13  TEST DATA      14  GAME DATA"/
     5" 15  ROOM DSCRPTN   16  WORDS          17  NOT USED")
      write(6,9041)
 9041 FORMAT(/" ENTER NUMBER FROM ABOVE: ",$)
      read(5,*) IANS
      IF(LX.NE.0)WRITE(LX,9042)IANS
 9042 FORMAT(/" ENTER NUMBER FROM ABOVE:"I3)
      IF(IANS.EQ.0)CALL exex(8,NAME,0,0,0,0,0)
C
C                            MM-L-1  (LINE 9520)
C 2-L
C
C ** INHABITANTS **
C
 9045 IF (IANS .NE. 11) GOTO 9080
      write(6,9050)
      IF(LX.NE.0)WRITE(LX,9050)
 9050 FORMAT(/ "  #  ", "WHO      DATA  ",
     1                  "WHO      DATA  ",
     1                  "WHO      DATA  "/)
      DO 9070 J=1,5
      J1=3*J-2
      write(6,9060)J1,(IRSN(L,J1),L=1,4),IRES(J1),(IRSN(L,
     13*J-1),L=1,4),IRES(3*J-1),(IRSN(L,3*J),L=1,4),IRES(3*J)
      IF(LX.NE.0)WRITE(LX,9060)J1,(IRSN(L,J1),L=1,4),IRES(J1),(IRSN(L,
     13*J-1),L=1,4),IRES(3*J-1),(IRSN(L,3*J),L=1,4),IRES(3*J)
 9060 FORMAT(X,I2,2X,4A2,I5,2X,4A2,I5,2X,4A2,I5)
 9070 CONTINUE
      IX=16
      write(6,9071)IX,(IRSN(L,16),L=1,4),IRES(16),(IRSN(L,17),L=1,4),
     1IRES(17)
      IF(LX.NE.0)WRITE(LX,9071)IX,(IRSN(L,16),L=1,4),IRES(16),
     1(IRSN(L,17),L=1,4),IRES(17)
 9071 FORMAT(X,I2,2X,4A2,I5,2X,4A2,I5)
      write(6,9075)
      IF(LX.NE.0)WRITE(LX,9075)
 9075 FORMAT(/" ENTER 0 IF NO CHANGE OR INHABITANTS, NEW DATA"//" ?",$)
      read(5,*) INH,NDAT
      IF(INH.EQ.0) GOTO 9035
      IRES(INH)=NDAT
C
C ** ITEMS **
C
 9080 IF (IANS .NE. 1) GOTO 9120
      write(6,9081)
 9081 FORMAT(/"  #  DATA  NAME"15X"DATA  NAME"15X"DATA  NAME"/)
      CALL MMRI(5,0)
      write(6,9115)
 9115 FORMAT(//" ENTER 0 IF NO CHANGE OR ITEM, NEW DATA"//"?",$)
      read(5,*) ITM,NDAT
      IF (ITM.EQ.0) GOTO 9035
      IVEN(ITM)=NDAT
      GOTO 9080
C
C                            MM-L-2  (LINE 9600)
C 3-L
C
C ** ROOMS **
C
 9120 IF (IANS .GT. 4) GOTO 9250
      write(6,9130)
      IF(LX.NE.0)WRITE(LX,9130)
 9130 FORMAT(/,"  #  ", "ROOM NAME          DATA  ",
     1                  "ROOM NAME          DATA  ",
     1                  "ROOM NAME          DATA  ",/)
      DO 9150 J=11*(IANS-2)+1,11*(IANS-1)
      J1=3*J-2
      write(6,9140) J1,(IRNM(L,IROM(J1)-(IROM(J1)/100)*100),
     1L=1,8),IROM(3*J-2),(IRNM(L,IROM(3*J-1)-(IROM(3*J-1)/100)*100),
     2L=1,8),IROM(3*J-1),(IRNM(L,IROM(3*J)-(IROM(3*J)/100)*100),
     3L=1,8),IROM(3*J)
      IF(LX.NE.0)WRITE(LX,9140) J1,(IRNM(L,IROM(J1)-(IROM(J1)/100)*100
     1),L=1,8),IROM(3*J-2),(IRNM(L,IROM(3*J-1)-(IROM(3*J-1)/100)*100),
     2L=1,8),IROM(3*J-1),(IRNM(L,IROM(3*J)-(IROM(3*J)/100)*100),
     3L=1,8),IROM(3*J)
 9140 FORMAT(X,I2,2X,8A2,I7,2X,8A2,I7,2X,8A2,I7)
 9150 CONTINUE
      write(6,9160)
      IF(LX.NE.0)WRITE(LX,9160)
 9160 FORMAT(/" ENTER 0 IF NO CHANGE OR ROOM #, NEW DATA"//" ?",$)
      read(5,*) IRMN,NDAT
      IF (IRMN.EQ.0) GOTO 9170
      IROM(IRMN)=NDAT
      GOTO 9120
 9170 write(6,9180)
      IF(LX.NE.0)WRITE(LX,9180)
 9180 FORMAT(/" DISPLAY ROOM EXITS?  ENTER ROOM #"//" ?",$)
      read(5,*)IRMN
      IF(IRMN .EQ. 0) GOTO 9035
      write(6,9190) IRMN,(IRNM(L,IFSD(IROM(IRMN))),L=1,8)
      IF(LX.NE.0)WRITE(LX,9190) IRMN,(IRNM(L,IFSD(IROM(IRMN))),L=1,8)
 9190 FORMAT(/" EXITS FOR"I3,2X,8A2/)
      DO 9210 J=1,360
      IF(IXT(J)-(IXT(J)/100)*100 .EQ. IRMN) write(6,9200)(IDTN(L,
     1(J+59)/60),L=1,4),(IRNM(L,IROM(IXT(J)/100-(IXT(J)/10000)*100)-
     2(IROM(IXT(J)/100-(IXT(J)/10000)*100)/100)*100),
     3L=1,8),J,IXT(J)
      IF(IXT(J)-(IXT(J)/100)*100.EQ.IRMN.AND.LX.NE.0) WRITE(LX,9200)
     1(IDTN(L,(J+59)/60),L=1,4),(IRNM(L,IROM(IXT(J)/100-(IXT(J)/10000)
     2*100)-(IROM(IXT(J)/100-(IXT(J)/10000)*100)/100)*100),
     2L=1,8),J,IXT(J)
 9200 FORMAT(X,4A2" TO "8A2,I4,I7)
 9210 CONTINUE
      write(6,9220)
      IF(LX.NE.0)WRITE(LX,9220)
 9220 FORMAT(/" ENTRANCES"/)
      DO 9240 J=1,360
      IF(IXT(J)/100-(IXT(J)/10000)*100 .EQ. IRMN) write(6,9230)
     1(IDTN(L,(J+59)/60),L=1,4),(IRNM(L,IROM(IXT(J)-(IXT(J)/100)*
     2100)-(IROM(IXT(J)-(IXT(J)/100)*100)/100)*100),
     2L=1,8),J,IXT(J)
      IF(ITFD(IXT(J)).EQ.IRMN.AND.LX.NE.0) WRITE(LX,9230)
     1(IDTN(L,(J+59)/60),L=1,4),(IRNM(L,IROM(IXT(J)-(IXT(J)/100)*
     1100)-(IROM(IXT(J)-(IXT(J)/100)*100)/100)*100),
     2L=1,8),J,IXT(J)
 9230 FORMAT(X,4A2" FROM "8A2,I4,I7)
 9240 CONTINUE
      GOTO 9170
C
C                            MM-L-3  (LINE 9680)
C 4-L
C
C ** EXITS **
C
 9250 IF(IANS .GT. 10) GOTO 9310
      write(6,9260)(IDTN(L,IANS-4),L=1,4)
      IF(LX.NE.0)WRITE(LX,9260)(IDTN(L,IANS-4),L=1,4)
 9260 FORMAT(/" EXITS "4A2//" ROOM #  ","  EXIT   ",
     1                                  "  EXIT   ",
     1                                  "  EXIT   ",
     1                                  "  EXIT   ",
     1                                  "  EXIT   ",
     1                                  "  EXIT   "/)
      DO 9280 J=1,10
      IC=(IANS-5)*60+6*J-5
      write(6,9270)IC,IXT(IC),IXT(IC+1),IXT(IC+2),
     1IXT(IC+3),IXT(IC+4),IXT(IC+5)
      IF(LX.NE.0)WRITE(LX,9270)IC,IXT(IC),IXT(IC+1),IXT(IC+2),
     1IXT(IC+3),IXT(IC+4),IXT(IC+5)
 9270 FORMAT(X,I3,2X,6I9)
 9280 CONTINUE
      write(6,9290)
 9290 FORMAT(/" ENTER 0 IF NO CHANGE OR EXIT #, NEW DATA "//" ?",$)
      read(5,*) IXIT,NDAT
      IF(LX.NE.0)WRITE(LX,9291)IXIT,NDAT
 9291 FORMAT(/" ENTER 0 IF NO CHANGE OR EXIT #, NEW DATA "//" ?"I4","I6)
      IF (IXIT.EQ.0) GOTO 9035
      IXT(IXIT)=NDAT
      GOTO 9250
C
C ** VERBS **
C
 9310 IF (IANS .NE. 12) GOTO 9340
      write(6,9320)
      IF(LX.NE.0)WRITE(LX,9320)
 9320 FORMAT(/" VERBS:"/)
      DO 9335 J=1,18
      IC=5*J-4
      write(6,9330) IC,(IVRB(L,IC),L=1,4),(IVRB(L,5*J-3),L=1,4),
     1(IVRB(L,5*J-2),L=1,4),(IVRB(L,5*J-1),L=1,4),(IVRB(L,5*J),L=1,4)
      IF(LX.NE.0)WRITE(LX,9330)IC,(IVRB(L,IC),L=1,4),(IVRB(L,5*J-3)
     1,L=1,4),(IVRB(L,5*J-2),L=1,4),(IVRB(L,5*J-1),L=1,4),(IVRB(L,5*J)
     2,L=1,4)
 9330 FORMAT(X,I2,3X,4A2,3X,4A2,3X,4A2,3X,4A2,3X,4A2)
 9335 CONTINUE
      GOTO 9035
C
C ** TEST DATA **
C
 9340 IF (IANS .NE. 13) GOTO 9500
      write(6,9350) (ITST(L),L=1,10),RND,(ITST(L),L=11,20),(ITST(L),
     1L=21,30),(ITST(L),L=31,40)
      IF(LX.NE.0)WRITE(LX,9350) (ITST(L),L=1,10),RND,(ITST(L),L=11,20),
     1(ITST(L),L=21,30),(ITST(L),L=31,40)
 9350 FORMAT(/"    ROOM SCORE  POSS  FACE  TURN  BAT1  BAT2 MROOM  ",
     1"MWHO  MWEP   RND"//X,10I6,3X,F7.6//"  SHOTS  ICOR  ITNB  IRB  ",
     2"IREV  IBOOK IHWAY  RTIM  STIM  REVC"//X,10I6//"   IRLU IRESP  ",
     3"MYST  KILL  IDSP  ITND  ITNL ICODE  IDS1  IDS2"//X,10I6//"   ",
     4"HEAL  TIDE  DSET  DCHK DBUGR  IALU  IOLU   SYS  WERE"//X,10I6)
      write(6,9400)
 9400 FORMAT(/" ENTER 0 IF NO CHANGE OR TEST #, NEW DATA"//" ?",$)
      read(5,*) ITS,NDAT
      IF(LX.NE.0)WRITE(LX,9401)ITS,NDAT
 9401 FORMAT(/" ENTER 0 IF NO CHANGE OR TEST #, NEW DATA"//" ?"I3","I6)
      IF(ITS.EQ.0) GOTO 9035
      ITST(ITS)=NDAT
      GOTO 9340
C
C                            MM-L-4  (LINE 9760))
C 5-L
C
C ** GAME DATA **
C
 9500 IF(IANS.NE.14)GOTO 9800

      data it_mm/2hMM/

      goto 9035	

c      IF(ITST(38).NE.it_mm)write(6,9505)
c      IF(ITST(38).NE.it_mm)GOTO 9035
c 9505 FORMAT(/"CARTRIDGE MM NOT SET UP.")
c      CALL OPEN(IDCB,IERR,NAMT,0,-44,2HMM)
c      IF(IERR.LT.0)GOTO 9530
c      CALL READF(IDCB,IERR,IDTL,576)
c      IF(IERR.LT.0)GOTO 9530
c      CALL READF(IDCB,IERR,ICOM,1920)
c      IF(IERR.LT.0)GOTO 9530
c      CALL CLOSE(IDCB)
c      write(6,9510)
c 9510 FORMAT(/" LU? ",$)
c      read(5,*)IANS
c      IF(LX.NE.0)WRITE(LX,9501)IANS
c 9501 FORMAT(/" LU?"I3)
c      IF(IANS.EQ.0)GOTO 9035
c      WRITE(IANS,9520)IDTL
c 9520 FORMAT(//4X"MYSTERY MANSION DATA"//4X"DAY START  STOP  TURN",
c     1" SCORE    OF  ROOM  CODE  NAME"//33(" "8I6,2X,10A2/))
c      WRITE(IANS,9526)
c 9526 FORMAT(/"  TAG   COMMENTS:"/)
c      DO 9527 I=1,30
c      IF(ICOM(1,I).EQ.2H  )GOTO 9529
c 9527 WRITE(IANS,9528)(ICOM(J,I),J=1,37)
c 9528 FORMAT(I6,2X,36A2)
c 9529 IF(IANS.EQ.6.OR.IANS.EQ.15)WRITE(IANS,9525)
c 9525 FORMAT("1"/"1")
c      GOTO 9035
c 9530 write(6,9540)IERR
c      IF(LX.NE.0)WRITE(LX,9540)IERR
c 9540 FORMAT(/" DISC ERROR"I6)
c      CALL CLOSE(IDCB)
c      GOTO 9035
C
C                            MM-L-5  (LINE 9840)
C 6-L
C
C *** ROOM DESCRIPTION ***
C
 9800 IF(IANS.NE.15)GOTO 9860
 9805 write(6,9810)
 9810 FORMAT(/" ENTER 0 TO END OR ROOM?  ",$)
      read(5,9820)IRM
 9820 FORMAT(8A2)
      IRMX=(IRM(1)/256-6*8)
      IRMY=IRM(1)-256*(IRM(1)/256)-6*8
      IF(IRMX.EQ.0.AND.(IRMY.LT.1.OR.IRMY.GT.9))ITST(25)=0
      IF(IRMX.EQ.0.AND.(IRMY.LT.1.OR.IRMY.GT.9))GOTO 9035
      NAME(2)=is_se
      ITST(25)=-1
      IF(IRMX.GT.0.AND.IRMX.LT.10.AND.(IRMY.LT.0.OR.IRMY.GT.9))
     1IRMZ=IRMX
      IF(IRMY.GE.0.AND.IRMY.LE.9)IRMZ=10*IRMX+IRMY
      IF(IRMZ.GT.0.AND.IRMZ.LT.100)write(6,9825)(IRNM(L,IFSD(IROM(
     >IRMZ))),L=1,8)
 9825 FORMAT(/1X,8A2)
      IF(IRMZ.GT.0.AND.IRMZ.LT.100)
     1CALL exex(8,NAME,LU,IFSD(IROM(IRMZ)),0,0,0)
      IX=0
      DO 9840 I=1,99
      IC=0
      DO 9830 J=1,8
      IF(IRNM(J,IFSD(IROM(I))).EQ.IRM(J))IC=IC+1
 9830 CONTINUE
      IF(IC.NE.8)GOTO 9840
      IF(IX.EQ.1)write(6,9835)
 9835 FORMAT(/" CHOICES:")
      IF(IX.GT.0)write(6,9836)IL
 9836 FORMAT(9X,I2)
      IX=IX+1
      IL=I
 9840 CONTINUE
      IF(IX.GT.1)write(6,9836)IL
      IF(IX.EQ.1)CALL exex(8,NAME,LU,IL,0,0,0)
      IF(IX.EQ.0)write(6,9850)
 9850 FORMAT(/" ROOM NOT FOUND. CHECK SPELLING OR # < 100.")
      NAME(2)=is_sb
      ITST(25)=0
      GOTO 9805
C
C *** WORDS ***
C
 9860 IF(IANS.NE.16)GOTO 9035
      write(6,9865)(IVEN(L),L=54,107)
 9865 FORMAT(/"  54   PADLOCK   POLICE OPERATOR  FIREMEN     TAXI",
     1"    BLANK    WINE   POISON"/4X,8I9//"  62    WATER   MONKEY",
     2"   MIRROR  DRAWING   CAVITY     WOOD     RAGS    CRATE"/
     34X,8I9//"  70   COFFIN   POINTS  PICTURE    PHONE ",
     4"    HINGE    RADIO     LAMP     WALL"/4X,8I9//
     5"  78     TREE    TROLL   STATUE FIGURINE     IDOL",
     6"    BLANK    ROCKS     HOOK"/4X,8I9//"  86      NEW",
     7"      OLD   SCROLL     FIRE   PLAQUE  CURTAIN",
     8"     DOOR     GATE"/4X,8I9//"  94    QUEEN     KING",
     9"  INDIANS    TABLE   TABLES   SHOWER FIGURINE",
     1"     BULL"/4X,8I9//" 102     BARS   DAGGER    BLANK    BLANK",
     2"    BLANK    BLANK"/4X,6I9" ",$)
      write(6,9115)
      read(5,*)ITM,NDAT
      IF(ITM.EQ.0)GOTO 9035
      IVEN(ITM)=NDAT
      GOTO 9860
C
      END

