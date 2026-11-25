C
C                            MM-G-12  (LINE 6880)
C 1-H
      subroutine MMSH
C
C ******* SEGMENT H *******
C
C ******* THIS SEGMENT IS CALLED BY SEGMENT B TO PERFORM THE FUNCTION
C         OF THE MIDDLE NUMBER VERB USED. THIS SEGMENT NORMALLY
C         COMPLETES BY CALLING SEGMENT I TO WRITE AN APPRORIATE MESSAGE.
C
      include 'common.i'

      DIMENSION NAMF(3),ITIM(5),IDTL(8,16),NAMT(3),
     1ICOM(64,15),IVRBX(4,8),KEYM(9)

      character*128 cfile

      equivalence (cfile, NAMF)
C
C *** FUNCTIONS ***
C
C * FIRST & SECOND DIGIT, THRID & FOURTH DIGIT, THIRD DIGIT AND FOURTH
C   DIGIT
C
      IFSD(I)=I-(I/100)*100
      ITFD(I)=I/100-(I/10000)*100
      ITD(I)=I/100-(I/1000)*10
      IFD(I)=I/1000-(I/10000)*10
      RN(R)=R*7.7-INT(R*7.7)
C
C *** SET UP VARIABLES ***
C
      NAME(2)=is_si
      LU=IPR(1)
      LX=ITST(21)
      LA=ITST(36)
      LO=ITST(37)
      IR=ITST(1)
      DATA NAMT/2HMM,2HTL,2HDA/
      DATA KEYM/2H  ,2H  ,2H  ,2H  ,2H  ,2H  ,2H  ,2H  ,2H  /
      IW71=IWRD(7,1)
      IW52=IWRD(5,2)
      IW62=IWRD(6,2)
      R=RND
C
C ***** INDEX OF VERB ROUTINES *****
C
C       ROUTINE               PAGE
C
C       EAT, RUB              2
C
C       BEAM, SWEEP, CALL     3
C
C       MOVE                  4
C
C       ANSWER, FEED, DISPLAY 5
C
C       ATTACK AND KILL       6
C
C       SUSPEND, RESTORE      7
C
C       DRINK, MYSTERY        8
C
C       BRIBE                 9
C
C       QUESTION             10
C
C       LISTEN               11
C
C                            MM-H-1  (LINE 6960)
C 2-H
C
C ****** SELECT THE RIGHT VERB ******
C
      GOTO(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,
     A10000,11000,12000,13000,14000,15000,16000,17000)IWRD(6,1)-35
C
C ****** EAT ROUTINE ******
C
 1000 IF(IW71.NE.1.AND.(IW52.NE.3.OR.IW62.GT.35))
     1CALL exex(8,NAME,LU,320,0,0,0)
      IF(IW71.EQ.1)IW62=24
C
C *** DOES HE HAVE IT ***
C
      IF(IVEN(IW62).LT.20000)CALL exex(8,NAME,LU,3010,0,0,0)
C
C *** CAN HE EAT IT ***
C
      IF(IW62.NE.22.AND.IW62.NE.24.AND.IW62.NE.28.AND.
     1IW62.NE.29.AND.IW62.NE.31)CALL exex(8,NAME,LU,2010,0,0,0)
C
C *** EAT IT ***
C
      IVEN(IW62)=0
      IF(IW62.EQ.24)CALL exex(8,NAME,LU,1010,0,0,0)
      CALL exex(8,NAME,LU,1020,0,0,0)
C
C ****** RUB ROUTINE ******
C
 2000 IF((IW52.NE.3.AND.IW52.NE.4).OR.(IW52.EQ.4.AND.
     1IW62.NE.8).OR.(IW52.EQ.3.AND.IVEN(IW62).EQ.0))
     2CALL exex(8,NAME,LU,320,0,0,0)
C
C *** IS IT LIGHT ***
C
      CALL MMRL
C
C *** SCROLL ***
C
      IF(IWRD(6,2).EQ.88.AND.IR.EQ.17)
     1 CALL exex(8,NAME,LU,2037,0,0,0)
C
C *** MAID ***
C
      IF((IW52.EQ.4.AND.IFSD(IRES(8)).NE.IR).OR.(IW52.EQ.3
     1.AND.IW62.GT.35.AND.IVEN(IW62).NE.IR))
     2 CALL exex(8,NAME,LU,2030,0,0,0)
      IF(IW52.EQ.4.AND.IR.NE.25)CALL exex(8,NAME,LU,2080,0,0,0)
      IF(IW52.EQ.4)IXT(134)=2225
      IF(IW52.EQ.4)CALL exex(8,NAME,LU,1095,IR,0,0)
C
C *** ITEMS ***
C
      IF(IW62.LT.36.AND.IVEN(IW62).LT.20000)
     1CALL exex(8,NAME,LU,3010,0,0,0)
      IF(IW62.NE.45)CALL exex(8,NAME,LU,520,0,0,0)
      CALL exex(8,NAME,LU,2090,0,0,0)
C
C                            MM-H-2  (LINE 7040)
C 3-H
C
C ****** BEAM ROUTINE ******
C
 3000 IF(IW71.NE.2.OR.IW52.NE.2.OR.(IW62.NE.5.AND.
     1IW62.NE.6))CALL exex(8,NAME,LU,320,0,0,0)
C
C *** BEAM UP ***
C
      IF(IW62.EQ.5.AND.IVEN(23).LT.20000)
     1 CALL exex(8,NAME,LU,3050,0,0,0)
      IF(IW62.EQ.5)ITST(1)=89
      IF(IW62.EQ.5)CALL exex(8,NAME,LU,3060,0,0,0)
C
C *** BEAM DOWN ***
C
      IF(IR.NE.89)CALL exex(8,NAME,LU,3070,0,0,0)
      IC=IFSD(IVEN(26))
      IF(IVEN(26).EQ.789.OR.IVEN(26).GT.20000.OR.(IVEN(26).LT.20000
     1.AND.IVEN(26).GT.10000.AND.IFSD(IRES(IC)).EQ.89))
     2CALL exex(8,NAME,LU,3080,0,0,0)
      IF(IVEN(26).GT.10000)IC=IFSD(IRES(IC))
      IF(IC.EQ.0)CALL exex(8,NAME,LU,520,0,0,0)
      ITST(1)=IC
      CALL exex(8,NAME,LU,3090,IC,0,0)
C
C ****** SWEEP ROUTINE ******
C
 4000 IF(IW71.NE.1)CALL exex(8,NAME,LU,320,0,0,0)
      IF(IVEN(25).LT.20000)CALL exex(8,NAME,LU,4055,0,0,0)
C
C *** IS IT LIGHT ***
C
      CALL MMRL
      IF(IR.NE.23)CALL exex(8,NAME,LU,4050,0,0,0)
      IXT(191)=2223
      CALL exex(8,NAME,LU,1095,IR,0,0)
C
C ****** CALL ROUTINE ******
C
C *** CHECK FORMAT ***
C
 5000 IF(IW52.NE.3.OR.IW62.LT.55.OR.IW62.GT.58)
     1CALL exex(8,NAME,LU,320,0,0,0)
C
C *** CAN HE CALL ***
C
      IF(IR.NE.11) CALL exex(8,NAME,LU,8032,0,0,0)
C
C *** POLICE ***
C
      IC=0
      IF(IW62.EQ.55.AND.ITST(8).EQ.0.AND.IXT(54).LT.20000
     1.AND.IFSD(IRES(ITST(9))).EQ.IR)IC=1
      IF(IC.EQ.1) IRES(ITST(9))=0
      IF(IC.EQ.1)ITST(2)=ITST(2)+50
      IF(IC.EQ.1) CALL exex(8,NAME,LU,5001,0,0,0)
C
C *** TAXI ***
C
      IF(IW62.EQ.58.AND.IXT(52).NE.0.AND.ITST(5).GT.400)IXT(52)=0
      IF(IXT(52).EQ.0.AND.IW62.EQ.58)CALL exex(8,NAME,LU,5002,0,0,0)
C
C *** NO RESPONSE ***
C
      CALL exex(8,NAME,LU,5009,0,0,0)
C
C                            MM-H-3  (LINE 7120)
C 4-H
C    ****** MOVE ROUTINE ******
C
C *** CHECK FORMAT ***
C
 6000 IF(IW71.NE.2.OR.IW52.NE.3.OR.IVEN(IW62).EQ.0)
     1CALL exex(8,NAME,LU,320,0,0,0)
C
C *** IS IT LIGHT ***
C
      CALL MMRL
C
C *** TABLES ***
C
      IF((IW62.EQ.94.OR.IW62.EQ.97.OR.IW62.EQ.98).AND.IR.EQ.15)
     1CALL exex(8,NAME,LU,6005,0,0,0)
      IF(IW62.EQ.95.AND.IR.EQ.15.AND.IVEN(48).EQ.0)
     1CALL exex(8,NAME,LU,6015,0,0,0)
C
C *** SCROLL ***
C
      IF(IWRD(6,2).EQ.88.AND.IR.EQ.17)CALL exex(8,NAME,LU,2037,0,0,0)
C
C *** RAGS ***
C
      IF(IW62.EQ.68.AND.IR.EQ.27)CALL exex(8,NAME,LU,6030,0,0,0)
      IF(IW62.EQ.68.AND.IR.EQ.88)CALL exex(8,NAME,LU,6040,0,0,0)
C
C *** ROCKS ***
C
      IF(IW62.EQ.84.AND.(IR.LT.42.OR.IR.GT.50))
     1 CALL exex(8,NAME,LU,8031,0,0,0)
      IF(IW62.EQ.84.AND.IXT(320).NE.0)CALL exex(8,NAME,LU,1040,0,0,0)
      R=RN(R)
      IF(IW62.EQ.84.AND.R.LT..333.AND.IR.GT.42.AND.IR.LT.47)IVEN(38)=
     1100+IR
      IF(IW62.EQ.84.AND.R.LT..333.AND.IR.GT.42.AND.IR.LT.47)
     1CALL exex(8,NAME,LU,6010,0,0,0)
      IF(IW62.EQ.84)CALL exex(8,NAME,LU,1040,0,0,0)
C
C *** BOULDERS ***
C
      IF(IW62.EQ.83.AND.(IXT(320).NE.0.OR.IR.NE.52))
     1CALL exex(8,NAME,LU,8031,0,0,0)
      IF(IW62.EQ.83.AND.IVEN(48).LT.20000)
     1CALL exex(8,NAME,LU,6041,0,0,0)
      IF(IW62.EQ.83.AND.IVEN(36).NE.0.AND.IXT(319).EQ.4952)
     1CALL exex(8,NAME,LU,6042,0,0,0)
      IF(IW62.EQ.83.AND.R.GT..67.AND.IXT(319).NE.0)IVEN(36)=152
      IF(IW62.EQ.83.AND.R.GT..67.AND.IXT(319).NE.0)
     1CALL exex(8,NAME,LU,6043,0,0,0)
      IF(IW62.EQ.83)CALL exex(8,NAME,LU,6044,0,0,0)
C
C *** IS HE NEAR IT ***
C
      IF(IVEN(IW62).NE.IR.AND.IW62.GT.47)
     1CALL exex(8,NAME,LU,8031,0,0,0)
C
C *** DOES HE HAVE IT ***
C
      IF(IW62.LT.54.AND.IVEN(IW62).LT.20000)
     1CALL exex(8,NAME,LU,8005,0,0,0)
C
C *** WOOD ***
C
      IF(IW62.EQ.67)IXT(187)=1314
      IF(IW62.EQ.67)CALL exex(8,NAME,LU,1095,IR,0,0)
C
      CALL exex(8,NAME,LU,8010,0,0,0)
C
C                            MM-H-4  (LINE 7200)
C 5-H
C  ****** ANSWER ROUTINE ******
C
 7000 IF(IW52 .NE. 3.OR.IW62.NE.73.OR.IW71.NE.2)
     1CALL exex(8,NAME,LU,320,0,0,0)
C
C *** IS IT LIGHT ***
C
      CALL MMRL
C
C *** SAY MESSAGE ***
C
      IF(IR .NE. 11) CALL exex(8,NAME,LU,8031,0,0,0)
      IC=ITD(IVEN(73))
      IF(IC.NE.0)IVEN(73)=1000
      IF(IC.NE.0)CALL exex(8,NAME,LU,7010,IC,0,0)
      CALL exex(8,NAME,LU,7090,0,0,0)
C
C ****** FEED ROUTINE ******
 8000 IF(IW71.NE.2.OR.(IW52.NE.4.AND.IW62.NE.63))
     1 CALL exex(8,NAME,LU,320,0,0,0)
C
C *** IS IT LIGHT ***
C
      CALL MMRL
C
C *** FEED MONKEY ***
C
      IF(IR.NE.1.AND.IW62.EQ.63)CALL exex(8,NAME,LU,2030,0,0,0)
      IF((IR .EQ. 1) .AND. (IW62 .EQ. 63)) IXT(1)=401
      IF((IR.EQ.1).AND.(IW62.EQ.63)) CALL exex(8,NAME,LU,1095,IR,0,0)
C
C *** DOES HE HAVE FOOD ***
C
      IF(IVEN(24).LT.20000)CALL exex(8,NAME,LU,8006,0,0,0)
C
C *** IS INHABITANT HERE ***
C
      IF(IFSD(IRES(IW62)) .NE. IR) CALL exex(8,NAME,LU,2030,0,0,0)
      IVEN(24)=309
      CALL exex(8,NAME,LU,8015,0,0,0)
C
C ****** DISPLAY ROUTINE ******
C
C *** CHECK FOR DEBUGGER IN CASE ANYBODY SHOULD TRY TO USE IT ***
C
 9000 IF (IW71 .GT. 1) CALL exex(8,NAME,LU,320,0,0,0)
      IF(ITST(30).EQ.0)NAME(2)=is_sl
      IF(ITST(30).EQ.0)CALL exex(8,NAME,0,0,0,0,0)
      R=RN(R)
      IC=INT(R*57)+10
      write(6,9010) IC
 9010 FORMAT(/X,I2" ARE YOU THE CREATOR?"//" ?",$)
      read(5,9020)IANS,IAN,ID
C
C *** ANSWER IN THE FORM 'YES XX' WHERE XX =(99-IC)
C
C *** ONCE YOU ENTER DISPLAY SET ITST(30)=0 FOR EASY ACCESS.
C
      IF(LX.NE.0)WRITE(LX,9011)IC,IANS
 9011 FORMAT(I3," ARE YOU THE CREATOR?"//" ?",I3)
 9020 FORMAT(2A2,I2)
c      write(6,9032)
c 9032 FORMAT("A            DDDDDDDDDDDDAAAA")

      data it_ye/2hYE/
      data it_no/2hNO/

      IF(IANS.EQ.it_no)CALL exex(8,NAME,LU,9040,0,0,0)
      IF(IANS.NE.it_ye.OR.99-IC.NE.ID)CALL exex(8,NAME,LU,9030,0,0,0)
      NAME(2)=is_sl
      CALL exex(8,NAME,0,0,0,0,0)
C
C                            MM-H-5  (LINE 7280)
C 6-H
C
C ****** ATTACK AND KILL ROUTINE ******
C
10000 IF(IWRD(7,1).EQ.1)CALL exex(8,NAME,LU,1000,0,0,0)
      IF(IW71.NE.2.OR.IW52.NE.4)CALL exex(8,NAME,LU,320,0,0,0)
C
C *** IS IT LIGHT ***
C
      CALL MMRL
C
C *** IS INHABITANT HERE AND ALIVE ***
C
      IC=IW62
      IF(IFSD(IRES(IC)).NE.IR)CALL exex(8,NAME,LU,2030,0,0,0)
      IF(IRES(IC).GT.10000)CALL exex(8,NAME,LU,20030,0,0,0)
C
C **** CAN INHABITANT BE KILLED ****
C
C *** VAMPIRE ***
C
      IF(IC.EQ.5.AND.IR.EQ.5.AND.IRES(5).EQ.5.AND.IVEN(29).GE.20000.
     1AND.(IVEN(11).GT.20000.OR.IVEN(14).GT.20000).AND.ITST(5).LT.50
     2.AND.IXT(183).EQ.405) CALL exex(8,NAME,LU,10011,0,0,0)
C
      IF(IC.EQ.5.OR.IC.EQ.10)CALL exex(8,NAME,LU,10020,0,0,0)
C
C *** WOLF OR WEREWOLF ***
C
      IF(IC.EQ.15.OR.IC.EQ.10)CALL exex(8,NAME,LU,10030,0,0,0)
C
C *** FIGHT ***
C
      IP=IRES(IC)/1000-ITST(31)
      DO 10010 I=1,89
      IF(I.NE.10.AND.I.NE.11.AND.I.NE.12.AND.I.NE.14.AND.I.NE.34.AND.
     1I.NE.45.AND.I.NE.48.AND.I.NE.49)GOTO 10010
      IF(IVEN(I).GT.20000)IP=IP+IFD(IVEN(I))
      IF(IFSD(IVEN(I)).EQ.IC.AND.IVEN(I).GT.10000.AND.IVEN(I).LT.
     120000) IP=IP-IFD(IVEN(J))
10010 CONTINUE
      R=RN(R)
      IP=IP+5*R-2
      IF(IP.EQ.6)IP=8
      IF(IP.EQ.7)IP=9
      IF(IP.EQ.-6)IP=-8
      IF(IP.EQ.-7)IP=-9
      IF(IP.GT.9)IP=10
      IF(IP.LT.-9)IP=-10
      IF(IP.LT.0)ITST(31)=-1*IP
      IF(IP.GT.0)IRES(IC)=IRES(IC)-1000*(IRES(IC)/1000)+1000*IP
      IF(IABS(IP).LT.2.AND.IC.NE.16)GOTO 10050
      IF(IP.LT.-9)CALL exex(8,NAME,LU,10030,0,0,0)
      IF(IP.GT.9.AND.IC.EQ.13.AND.IR.EQ.33)IRES(13)=934
      IF(IP.GT.9.AND.IC.EQ.13.AND.IR.NE.33)IRES(13)=933
      IF(IP.GT.9.AND.IC.EQ.13)CALL exex(8,NAME,LU,16045,0,0,0)
      IF(IP.GT.9)CALL exex(8,NAME,LU,10040,0,0,0)
      IF(IP.GT.0)CALL exex(8,NAME,LU,10041,0,0,0)
      CALL exex(8,NAME,LU,10031,0,0,0)
C
C *** ESCAPE ***
C
10050 DO 10070 J=1,360
      IF(IFSD(IXT(J)).NE.IR.OR.IXT(J).GT.20000)GOTO 10070
      IRES(IC)=IRES(IC)-IR+ITFD(IXT(J))
      CALL exex(8,NAME,LU,10060,0,0,0)
10070 CONTINUE
      R=RN(R)
      IF(R.LT..3)CALL exex(8,NAME,LU,10030,0,0,0)
      IRES(IC)=10000+IR
      CALL exex(8,NAME,LU,10040,0,0,0)
C
C                            MM-H-6  (LINE 7360)
C 7-H
C *** THE REVISION IS CHANGED WHENEVER OLD SUSPENDED GAMES WILL
C     NOT WORK CORRECTLY OR WHENEVER THE GAME MIGHT NOT APPEAR TO BE
C     THE SAME AS BEFORE. OTHERWISE ONLY THE DATE IS CHANGED.
C
C ****** SUSPEND ROUTINE ******
C
11000 IF(IW71 .GT. 1) CALL exex(8,NAME,LU,320,0,0,0)
      IC=-1

11050 write(*, '("Enter name to save to: ",$)')
      read(*,'(a)') cfile

	  CALL OPEN(IDCB,IERR,NAMF,1)
      IF(IERR .LT. 0) CALL exex(8,NAME,LU,11075,IERR)
      DO 11080 I=1,8
      DO 11080 J=1,4
11080 IVRBX(J,I)=IVRB(J,3+I)
      CALL WRITF(IDCB,IERR,IVRBX,32)
      IF(IERR.LT.0)CALL exex(8,NAME,LU,11075,IERR)
      CALL WRITF(IDCB,IERR,IROM,99)
      CALL WRITF(IDCB,IERR,IRES,17)
      CALL WRITF(IDCB,IERR,IVEN,107)
      CALL WRITF(IDCB,IERR,IXT,360)
      CALL WRITF(IDCB,IERR,ITST,40)
      CALL CLOSE(IDCB)
      CALL exex(8,NAME,LU,11040,NAMF(1),NAMF(2),NAMF(3))
C
C ****** RESTORE ROUTINE ******
C
12000 IF(IW71 .GT. 1) CALL exex(8,NAME,LU,320)
      IC=-1

12050 write(*,'("Enter file saved under: ",$)')
      read(*,'(a)') cfile

	  CALL OPEN(IDCB,IERR,NAMF,0)
      IF(IERR .LT. 0) CALL exex(8,NAME,LU,11075,IERR)
      CALL READF(IDCB,IERR,IVRBX,32)
      IF(IERR.LT.0)CALL exex(8,NAME,LU,11075,IERR)
      DO 12080 I=1,8
      DO 12080 J=1,4
12080 IVRB(J,3+I)=IVRBX(J,I)
      IXX=ITST(19)
      CALL READF(IDCB,IERR,IROM,99)
      CALL READF(IDCB,IERR,IRES,17)
      CALL READF(IDCB,IERR,IVEN,107)
      CALL READF(IDCB,IERR,IXT,360)
      CALL READF(IDCB,IERR,ITST,40)
      CALL CLOSE(IDCB)




      CALL EXEC(11,ITIM)
      ITST(19)=IXX
      ITST(18)=ITIM(4)*1200+ITIM(3)*20+ITIM(2)/3
      ITST(20)=ITST(20)+1
      CALL exex(8,NAME,LU,12040,NAMF(1),NAMF(2),NAMF(3))
C
C                            MM-H-7  (LINE 7440)
C 8-H
C
C ****** DRINK ROUTINE ******
C
13000 IF(IW71.NE.1.AND.IW62.NE.13.AND.IW62.NE.60.AND.IW62.NE.62)
     1CALL exex(8,NAME,LU,320,0,0,0)
      IF(IW71.GT.2)CALL exex(8,NAME,LU,320,0,0,0)
      IF(IW71.EQ.1)IW62=62
C
C *** WINE ***
C
      IF(IW62.EQ.60.AND.IR.NE.16)CALL exex(8,NAME,LU,2030,0,0,0)
      IF(IW62.EQ.60)IXT(128)=1316
      IF(IW62.EQ.60)CALL exex(8,NAME,LU,1095,IR,0,0)
C
C *** POISON ***
C
      IF(IW62.EQ.13.AND.(IVEN(13).LT.20000.OR.ITST(10).EQ.13))
     1CALL exex(8,NAME,LU,2030,0,0,0)
      IF(IW62.EQ.13)NAME(2)=is_sd
      IF(IW62.EQ.13)CALL exex(8,NAME,LU,13010,0,0,0)
C
C *** WATER ***
C
      IF(IW62.EQ.62.AND.(IR.LT.68.OR.IR.GT.83).AND.IR.NE.99)
     1CALL exex(8,NAME,LU,2030,0,0,0)
      IF(IW62.EQ.62)CALL exex(8,NAME,LU,1010,0,0,0)
      CALL exex(8,NAME,LU,320,0,0,0)
C
C ****** MYSTERY CHANGE ******
C
14000 IF(IW71.GT.1)GOTO 14100
C
C *** IS IT THE FIRST TURN ***
C
      IF(ITST(5).GT.2)CALL exex(8,NAME,LU,14010,0,0,0)
C
C *** GET THE MYSTERY NUMBER ***
C
14020 write(6,14030)
14030 FORMAT(/"MYSTERY #? ",$)
      IF(LA.EQ.0)read(5,*)IC
      IF(LA.NE.0)READ(LA,14032)IC
      IF(LO.NE.0)WRITE(LO,14032)IC
      IF(LX.NE.0)WRITE(LX,14031)IC
14031 FORMAT(/" MYSTERY #? "I4)
14032 FORMAT(I6)
      IF(IC.LT.1.OR.IC.GT.999)GOTO 14020
      RND=.001*IC
      write(6,14035)IC
      IF(LX.NE.0)WRITE(LX,14035)IC
14035 FORMAT(/" THIS GAME IS NOW SET UP FOR MYSTERY #"I4)
      ITST(23)=IC
      NAME(2)=is_sa
      CALL exex(8,NAME,0,0,0,0,0)
C
C *** MYSTERY MANSION ***
C
14100 IF(IWRD(7,1).GT.2.OR.IWRD(6,2).NE.107)
     1 CALL exex(8,NAME,LU,320,0,0,0)
      CALL exex(8,NAME,LU,14110,0,0,0)
C
C                            MM-H-8  (LINE 7520)
C 9-H
C
C ****** BRIBE ROUTINE ******
C
C *** CHECK FORMAT ***
C
15000 IF(IW71.NE.2.OR.IW52.NE.4)CALL exex(8,NAME,LU,320,0,0,0)
C
C *** IS THE PLAYER WITH THE INHABITANT ***
C
      IF(IFSD(IRES(IW62)).NE.IR) CALL exex(8,NAME,LU,2030,0,0,0)
C
C *** DOES THE PLAYER HAVE THE TREASURE ***
C
      IF(IVEN(7).LT.20000)CALL exex(8,NAME,LU,15200,0,0,0)
C
C *** IS THE INHABITANT ALIVE ***
C
      IF(IRES(IW62).GT.10000)CALL exex(8,NAME,LU,15400,0,0,0)
C
C *** DOES THE INHABITANT KNOW ANYTHING ***
C
      IC=0
      IG=IW62
      IM=ITST(9)
      IR=ITST(8)
      IF(IM.EQ.2.AND.IR.GT.53.AND.(IG.EQ.7.OR.IG.EQ.8.OR.IG.EQ.9))IC=1
      IF(IM.EQ.2.AND.IR.LT.28.AND.(IG.EQ.3.OR.IG.EQ.8.OR.IG.EQ.14))IC=1
      IF(IM.EQ.3.AND.IR.GT.18.AND.(IG.EQ.2.OR.IG.EQ.8.OR.IG.EQ.14))IC=1
      IF(IM.EQ.3.AND.IR.LT.10.AND.(IG.EQ.7.OR.IG.EQ.9.OR.IG.EQ.14))IC=1
      IF(IM.EQ.7.AND.IR.GT.53.AND.(IG.EQ.2.OR.IG.EQ.8.OR.IG.EQ.9))IC=1
      IF(IM.EQ.7.AND.IR.LT.10.AND.(IG.EQ.3.OR.IG.EQ.9.OR.IG.EQ.14))IC=1
      IF(IM.EQ.8.AND.IR.GT.53.AND.(IG.EQ.2.OR.IG.EQ.7.OR.IG.EQ.9))IC=1
      IF(IM.EQ.8.AND.IR.LT.28.AND.(IG.EQ.2.OR.IG.EQ.3.OR.IG.EQ.14))IC=1
      IF(IM.EQ.9.AND.IR.GT.53.AND.(IG.EQ.2.OR.IG.EQ.7.OR.IG.EQ.8))IC=1
      IF(IM.EQ.9.AND.IR.LT.10.AND.(IG.EQ.3.OR.IG.EQ.7.OR.IG.EQ.14))IC=1
      IF(IM.EQ.14.AND.IR.GT.9.AND.(IG.EQ.2.OR.IG.EQ.3.OR.IG.EQ.8))IC=1
      IF(IM.EQ.14.AND.IR.LT.10.AND.(IG.EQ.3.OR.IG.EQ.7.OR.IG.EQ.9))IC=1
      IF(IC.EQ.0)CALL exex(8,NAME,LU,15400,0,0,0)
      IVEN(7)=0
      IRES(IG)=0
      CALL exex(8,NAME,LU,15500,0,0,0)
C
C                            MM-H-9  (LINE 7600)
C 10-H
C ****** QUESTION ROUTINE ******
C
16000 IF(IWRD(7,1).EQ.1) CALL exex(8,NAME,LU,1000,0,0,0)
      IF(IWRD(7,1).NE.2.OR.IWRD(5,2).NE.4)
     1 CALL exex(8,NAME,LU,320,0,0,0)
C
C *** IS HE WITH INHABITANT ***
C
      IF(IFSD(IRES(IWRD(6,2))).NE.IR)CALL exex(8,NAME,LU,2030,0,0,0)
C
C *** IS INHABITANT ALIVE AND VOCAL ***
C
      IF(IRES(IWRD(6,2)).GE.1000.OR.IWRD(6,2).EQ.4.OR.IWRD(6,2).EQ.15)
     1CALL exex(8,NAME,LU,20030,0,0,0)
C
C *** IRRITATE ***
C
      R=RN(R)
      IF(ITST(5)-2.GT.ITST(13).AND.R.LT..5)
     1 CALL exex(8,NAME,LU,23120,0,0,0)
      IF(ITST(5)-4.GT.ITST(13))CALL exex(8,NAME,LU,23145,0,0,0)
      IF(ITST(5)-2.GT.ITST(13))CALL exex(8,NAME,LU,23140,0,0,0)
C
C *** VAMPIRE ***
C
      IF(IWRD(6,2).EQ.5)CALL exex(8,NAME,LU,23010,0,0,0)
C
C *** WEREWOLF ***
C
      IF(IWRD(6,2).EQ.10)CALL exex(8,NAME,LU,23015,0,0,0)
C
C *** ELF AND DWARF ***
C
      IF(IWRD(6,2).EQ.11.OR.IWRD(6,2).EQ.13)
     1 CALL exex(8,NAME,LU,23020,0,0,0)
C
C *** WOODSMAN AND HUNTER ***
C
      IF(IWRD(6,2).NE.6.AND.IWRD(6,2).NE.12)GOTO 23130
      IF(IXT(54).GT.20000.AND.IR.NE.93.AND.R.LT..3)
     1CALL exex(8,NAME,LU,23030,0,0,0)
      IF(IXT(54).GT.20000.AND.IR.EQ.93.AND.RDN.LT..3)
     1CALL exex(8,NAME,LU,23040,0,0,0)
      IF(IXT(54).LT.20000.AND.R.LT..3)CALL exex(8,NAME,LU,23050,0,0,0)
      IF(R.LT..4)CALL exex(8,NAME,LU,23060,0,0,0)
      IF(R.LT..5)CALL exex(8,NAME,LU,23070,0,0,0)
      IF(R.LT..58)CALL exex(8,NAME,LU,23080,0,0,0)
      IF(R.LT..66)CALL exex(8,NAME,LU,23090,0,0,0)
      IF(R.LT..74)CALL exex(8,NAME,LU,23100,0,0,0)
      IF(R.LT..82)CALL exex(8,NAME,LU,23110,0,0,0)
      CALL exex(8,NAME,LU,23120,0,0,0)
C
C *** MURDERER ***
C
23130 IF(IWRD(6,2).NE.ITST(9))GOTO 23160
      IF(R.LT..33)CALL exex(8,NAME,LU,23120,0,0,0)
      IF(R.LT..67)CALL exex(8,NAME,LU,23140,0,0,0)
      CALL exex(8,NAME,LU,23150,0,0,0)
C
C *** MASTER AND BUTLER ***
C
23160 IF(IWRD(6,2).NE.3.AND.IWRD(6,2).NE.14)GOTO 23250
      IF(R.LT..3)GOTO 23200
      IF(ITST(8).LT.28.AND.R.LT..15)CALL exex(8,NAME,LU,23170,0,0,0)
      IF(ITST(8).LT.28)CALL exex(8,NAME,LU,23180,0,0,0)
      IF(IVEN(ITST(10)).LT.10000.AND.IFSD(IVEN(ITST(10))).LT.28)
     1CALL exex(8,NAME,LU,23190,0,0,0)
      CALL exex(8,NAME,LU,23150,0,0,0)
23200 IF(R.LT..4)CALL exex(8,NAME,LU,23210,0,0,0)
      IF(R.LT..5)CALL exex(8,NAME,LU,23220,0,0,0)
      IF(R.LT..6)CALL exex(8,NAME,LU,23230,0,0,0)
      IF(R.LT..7)CALL exex(8,NAME,LU,23240,0,0,0)
      IF(R.LT..8)CALL exex(8,NAME,LU,23140,0,0,0)
      CALL exex(8,NAME,LU,23120,0,0,0)
C
C                            MM-H-10  (LINE 7680)
C 11-H
C
C *** LADY AND MAID ***
C
23250 IF(IWRD(6,2).NE.8.AND.IWRD(6,2).NE.2)GOTO 23310
      IF(R.LT..3)GOTO 23260
      IF(ITST(8).GT.9.AND.R.LT..15)CALL exex(8,NAME,LU,23170,0,0,0)
      IF(ITST(8).GT.9)CALL exex(8,NAME,LU,23180,0,0,0)
      IF(IVEN(ITST(10)).LT.10000.AND.IFSD(IVEN(ITST(10))).GT.9)
     1CALL exex(8,NAME,LU,23190,0,0,0)
      CALL exex(8,NAME,LU,23150,0,0,0)
23260 IF(R.LT..4)CALL exex(8,NAME,LU,23270,0,0,0)
      IF(R.LT..5)CALL exex(8,NAME,LU,23280,0,0,0)
      IF(R.LT..6)CALL exex(8,NAME,LU,23290,0,0,0)
      IF(R.LT..7)CALL exex(8,NAME,LU,23300,0,0,0)
      IF(R.LT..8)CALL exex(8,NAME,LU,23140,0,0,0)
      CALL exex(8,NAME,LU,23150,0,0,0)
C
C *** COOK AND GARDENER ***
C
23310 IF(R.LT..3)GOTO 23320
      IF(ITST(8).LT.10.OR.ITST(8).GT.27.AND.R.LT..15)
     1CALL exex(8,NAME,LU,23170,0,0,0)
      IF(ITST(8).LT.10.OR.ITST(8).GT.27)
     1 CALL exex(8,NAME,LU,23180,0,0,0)
      IF(IVEN(ITST(10)).LT.10000.AND.IFSD(IVEN(ITST(10))).GT.9)
     1CALL exex(8,NAME,LU,23190,0,0,0)
      CALL exex(8,NAME,LU,23150,0,0,0)
23320 IF(R.LT..4)CALL exex(8,NAME,LU,23330,0,0,0)
      IF(R.LT..5)CALL exex(8,NAME,LU,23340,0,0,0)
      IF(R.LT..6)CALL exex(8,NAME,LU,23350,0,0,0)
      IF(R.LT..7)CALL exex(8,NAME,LU,23360,0,0,0)
      IF(R.LT..8)CALL exex(8,NAME,LU,23140,0,0,0)
      CALL exex(8,NAME,LU,23150,0,0,0)
C
C ****** LISTEN ROUTINE ******
C
17000 IF(IWRD(7,1).NE.1)CALL exex(8,NAME,LU,320,0,0,0)
      IX=0
      IC=0
      ID=24000+IR
      IF(IR.GT.53)ID=24100
      IF(IVEN(3).GT.20000.OR.IVEN(3).EQ.200+IR.OR.(IVEN(3).GT.10000.
     1AND.IFSD(IVEN(3)).EQ.IR))IX=24108
      DO 24010 I=2,15
      IF(IRES(I).GT.10000.OR.IFSD(IRES(I)).NE.IR)GOTO 24010
      IX=24110
      IF(I.NE.4.AND.I.NE.10.AND.I.NE.15)IC=IC+1
      IF(IC.EQ.1)IX=24120
      IF(IC.LT.2)GOTO 24010
      IF(IC.EQ.2)IX=24130
      IF(IC.EQ.3)IX=24140
24010 CONTINUE
      IF(IRES(6)+IRES(12).LT.10000.AND.IFSD(IRES(6)).EQ.IR.AND.
     1IFSD(IRES(12)).EQ.IR)IX=24150
      IF(IRES(3)+IRES(14).LT.10000.AND.IFSD(IRES(3)).EQ.IR.AND.
     1IFSD(IRES(14)).EQ.IR.AND.ITST(9).NE.3.AND.ITST(9).NE.14.
     2AND.ITST(8).LT.28)IX=24160
      RNX=77.7*IR/253.-INT(77.7*IR/253.)
      IF(IX.EQ.24160.AND.RNX.GT..8)IX=24164
      IF(IX.EQ.24160.AND.RNX.GT..6)IX=24162
      IF(IRES(7)+IRES(9).LT.10000.AND.IFSD(IRES(7)).EQ.IR.AND.
     1IFSD(IRES(9)).EQ.IR.AND.ITST(9).NE.7.AND.ITST(9).NE.9.AND.
     2(ITST(8).LT.10.OR.ITST(8).GT.27))IX=24170
      IF(IX.EQ.24170.AND.RNX.GT..8)IX=24174
      IF(IX.EQ.24170.AND.RNX.GT..6)IX=24172
      IF(IRES(2)+IRES(8).LT.10000.AND.IFSD(IRES(2)).EQ.IR.AND.
     1IFSD(IRES(8)).EQ.IR.AND.ITST(9).NE.2.AND.ITST(9).NE.8.AND.
     2ITST(8).GT.27)IX=24180
      IF(IX.EQ.24180.AND.RNX.GT..8)IX=24184
      IF(IX.EQ.24180.AND.RNX.GT..6)IX=24182
      IF(ID.EQ.24100.AND.IX.NE.0)ID=24105
      CALL exex(8,NAME,LU,ID,IX,0,0)
C
      END

