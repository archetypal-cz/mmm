$storage:2
$large
$nodebug
      subroutine mmsh(p1,p2,p3,p4,p5)
c      ,10-02-1983 MMM    8536E D45 PL&BM <850920.1834>
C     04-02-1988 MS-DOS S.M.
C
C ******* SEGMENT H *******
C
C ******* Tento segment provadi stredni slovesa. Normalne se
C         ukoncuje segmentem MMSI. *************
C
$INCLUDE:'MMCO.for'
      integer*2 p1,p2,p3,p4,p5
      character*64 namf
      DIMENSION ITIM(5),IDTL(8,16),NAMT(3),
     1ICOM(64,15),IVRBX(4,8),KEYM(9)
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
C
C *** nastaveni promennych ***
C
      NAME(2)='SI'
      ipr(1)=p1
      ipr(2)=p2
      ipr(3)=p3
      ipr(4)=p4
      ipr(5)=p5
      LU=IPR(1)
      LX=ITST(21)
      LA=ITST(36)
      LO=ITST(37)
      IR=ITST(1)
      p1=lu
      p2=0
      p3=0
      p4=0
      p5=0
      DATA NAMT/'MM','TL','DA'/
      DATA KEYM/'  ','  ','  ','  ','  ','  ','  ','  ','  '/
      IW71=IWRD(7,1)
      IW52=IWRD(5,2)
      IW62=IWRD(6,2)
C
C ***** INDEX OF VERB ROUTINES *****
C       ROUTINE               PAGE
C       EAT, RUB              2
C       BEAM, SWEEP, CALL     3
C       MOVE                  4
C       ANSWER, FEED, DISPLAY 5
C       ATTACK AND KILL       6
C       SUSPEND, RESTORE      7
C       DRINK, MYSTERY        8
C       BRIBE                 9
C       QUESTION             10
C       LISTEN               11
C
C                            MM-H-1  (LINE 6960)
C 2-H
C ****** SELECT THE RIGHT VERB ******
C
      GOTO(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,
     A10000,11000,12000,13000,14000,15000,16000,17000)IWRD(6,1)-35
C
C ****** EAT ROUTINE ******
C
 1000 IF (IW71.NE.1.AND.(IW52.NE.3.OR.IW62.GT.35)) then
c     1 CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
      IF (IW71.EQ.1)IW62=24
      IF (IW71.EQ.1)IWRD(1,2)='JI'
      IF (IW71.EQ.1)IWRD(2,2)='DL'
      IF (IW71.EQ.1)IWRD(3,2)='O '
C
C *** ma ho ***
C
      IF (IVEN(IW62).LT.20000) then
c      CALL SGLOD(NAME,LU,3010)
          p2=3010
          return
      endif
C
C *** da se to jist ***
C
C *** listek, jidlo, mapa, klin, zapalky
C
      IF (IW62.NE.22.AND.IW62.NE.24.AND.IW62.NE.28.AND.
     1IW62.NE.29.AND.IW62.NE.31) then
c     CALL SGLOD(NAME,LU,2010)
          p2=2010
          return
      endif
C
C *** spapa to ***
C
      IVEN(IW62)=0
      IF (IW62.EQ.24) then
c      CALL SGLOD(NAME,LU,1010)
          p2=1010
          return
      endif
c     CALL SGLOD(NAME,LU,1020)
      p2=1020
      return
C
C ****** rub ******
C
 2000 IF ((IW52.NE.3.AND.IW52.NE.4).OR.(IW52.EQ.4.AND.
     1IW62.NE.8).OR.(IW52.EQ.3.AND.IVEN(IW62).EQ.0)) then
c     2 CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** je svetlo ***
C
      CALL MMRL
C
C *** svitek ***
C
      IF (IWRD(6,2).EQ.88.AND.IR.EQ.17) then
c      CALL SGLOD(NAME,LU,2037)
          p2=2037
          return
      endif
C
C *** sluzka ***
C
      IF ((IW52.EQ.4.AND.IFSD(IRES(8)).NE.IR).OR.(IW52.EQ.3
     1.AND.IW62.GT.35.AND.IVEN(IW62).NE.IR)) then
c     2 CALL SGLOD(NAME,LU,2030)
          p2=2030
          return
      endif
      IF (IW52.EQ.4.AND.IR.NE.25) then
c      CALL SGLOD(NAME,LU,2080)
          p2=2080
          return
      endif
      IF (IW52.EQ.4)IXT(134)=2225
      IF (IW52.EQ.4) then
c      CALL SGLOD(NAME,LU,1095,IR)
          p2=1095
          p3=ir
          return
      endif
C
C *** veci ***
C
      IF (IW62.LT.36.AND.IVEN(IW62).LT.20000) then
c     1 CALL SGLOD(NAME,LU,3010)
          p2=3010
          return
      endif
      IF (IW62.NE.45) then
c      CALL SGLOD(NAME,LU,520)
          p2=520
          return
      endif
c     CALL SGLOD(NAME,LU,2090)
      p2=2090
      return
C
C                            MM-H-2  (LINE 7040)
C 3-H
C ****** BEAM ROUTINE ******
C
C
C **** premistit se ***
C
 3000 IF (IW71.NE.1) then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** BEAM UP *** mimo laborator - premisteni do laboratore ***
C
      IF (ir .ne. 89 .and. IVEN(23).LT.20000) then
c     1   CALL SGLOD(NAME,LU,3050) ! potrebuje vysilac
          p2=3050
          return
      endif
      IF (ir .ne. 89) ITST(1)=89
      IF (ir .ne. 89) then
c      CALL SGLOD(NAME,LU,3060)
          p2=3060
          return
      endif
C
C *** BEAM DOWN *** premisteni zpet ***
C
C     IF (IR.NE.89) then
c     CALL SGLOD(NAME,LU,3070) ! musi byt v lab.
c         p2=3070
c         return
c     endif
      IC=IFSD(IVEN(26))
      IF (IVEN(26).EQ.789.OR.IVEN(26).GT.20000.OR.(IVEN(26).LT.20000
     1.AND.IVEN(26).GT.10000.AND.IFSD(IRES(IC)).EQ.89)) then
c     2 CALL SGLOD(NAME,LU,3080)  ! musi byt nekde jinde
          p2=3080
          return
      endif
      IF (IVEN(26).GT.10000)IC=IFSD(IRES(IC))
      IF (IC.EQ.0) then
c      CALL SGLOD(NAME,LU,520)
          p2=520
          return
      endif
      ITST(1)=IC
c     CALL SGLOD(NAME,LU,3090,IC)
      p2=3090
      p3=ic
      return
C
C ****** SWEEP ******
C
 4000 IF (IW71.NE.1) then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
      IF (IVEN(25).LT.20000) then
c      CALL SGLOD(NAME,LU,4055)
          p2=4055
          return
      endif
C
C *** je tu svetlo ***
C
      CALL MMRL
      IF (IR.NE.23) then
c      CALL SGLOD(NAME,LU,4050)
          p2=4050
          return
      endif
C
C *** zametani v chodbe otevre tajne dvere ***
C
      IXT(191)=2223
c     CALL SGLOD(NAME,LU,1095,IR)
      p2=1095
      p3=ir
      return
C
C ****** CALL ROUTINE ******
C
C *** format ***
C
 5000 IF (iw71.gt.2 .and. iwrd(5,3).eq.5 .and. iwrd(6,2).eq.10) then
         do 5002 i=1,6
 5002    iwrd(i,2)=iwrd(i,3)
         iw71=2
      endif
C
C *** muze volat ***
C
      IF (IR.NE.11) then
c      CALL SGLOD(NAME,LU,8032)
          p2=8032
          return
      endif
      if (iw71 .eq. 1) then
c      call SGLOD(name,lu,5007)
          p2=5007
          return
      endif
      IF (IW52.NE.3.OR.IW62.LT.55.OR.IW62.GT.58) then
c     1 CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** policie ***
C
      IC=0
      IF (IW62.EQ.55.AND.ITST(8).EQ.0.AND.IXT(54).LT.20000
     1.AND.IFSD(IRES(ITST(9))).EQ.IR) IC=1
      IF (IC.EQ.1) IRES(ITST(9))=0
      IF (IC.EQ.1) ITST(2)=ITST(2)+50
      IF (IC.EQ.1) then
c      CALL SGLOD(NAME,LU,5001)
          p2=5001
          return
      endif
C
C *** drozka ***
C
      IF (IW62.EQ.58.AND.IXT(52).NE.0.AND.ITST(5).GT.400) IXT(52)=0
      IF (IXT(52).EQ.0.AND.IW62.EQ.58) then
c      CALL SGLOD(NAME,LU,5002)
          p2=5002
          return
      endif
C
C *** nic se nedeje ***
C
c     CALL SGLOD(NAME,LU,5009)
      p2=5009
      return
C
C
C
C                            MM-H-3  (LINE 7120)
C 4-H
C
C    ****** MOVE ROUTINE ******
C
C *** kontrola formatu ***
C
 6000 IF (IW71.NE.2.OR.IW52.NE.3.OR.IVEN(IW62).EQ.0) then
c     1 CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** je svetlo ***
C
      CALL MMRL
C
C *** stoly *** kralovna, stul, stoly
C
      IF ((IW62.EQ.94.OR.IW62.EQ.97.OR.IW62.EQ.98).AND.IR.EQ.15) then
c     1 CALL SGLOD(NAME,LU,6005)
          p2=6005
          return
      endif
C
C *** kral ***
C
      IF (IW62.EQ.95.AND.IR.EQ.15.AND.IVEN(48).EQ.0) then
c     1 CALL SGLOD(NAME,LU,6015)
           p2=6015
           return
      endif
C
C *** svitek ***
C
      IF (IWRD(6,2).EQ.88.AND.IR.EQ.17) then
c      CALL SGLOD(NAME,LU,2037)
          p2=2037
          return
      endif
C
C *** hadry ***
C
      IF (IW62.EQ.68.AND.IR.EQ.27) then
c      CALL SGLOD(NAME,LU,6030)
           p2=6030
           return
      endif
      IF (IW62.EQ.68.AND.IR.EQ.88) then
c      CALL SGLOD(NAME,LU,6040)
           p2=6040
           return
       endif
C
C *** skaly, kameny ***
C
      IF (IW62.EQ.84.AND.(IR.LT.42.OR.IR.GT.50)) then
c     1 CALL SGLOD(NAME,LU,8030)
           p2=8030
           return
      endif
      IF (IW62.EQ.84.AND.IXT(320).NE.0) then
c      CALL SGLOD(NAME,LU,1040)
          p2=1040
          return
      endif
      IF (IW62.EQ.84.AND.PROB(0.333).AND.IR.GT.42.AND.IR.LT.47) THEN
         IVEN(38)=100+IR
c        CALL SGLOD(NAME,LU,6010)
         p2=6010
         return
      ENDIF
      IF (IW62.EQ.84) then
c      CALL SGLOD(NAME,LU,1040)
          p2=1040
          return
      endif
C
C *** balvany ***
C
      IF (IW62.EQ.83.AND.(IXT(320).NE.0.OR.IR.NE.52)) then
c     1 CALL SGLOD(NAME,LU,8030)
           p2=8030
           return
      endif
      IF (IW62.EQ.83.AND.IVEN(48).LT.20000) then
c     1 CALL SGLOD(NAME,LU,6041)
           p2=6041
           return
      endif
      IF (IW62.EQ.83.AND.IVEN(36).NE.0.AND.IXT(319).EQ.4952) then
c     1 CALL SGLOD(NAME,LU,6042)
          p2=6042
          return
      endif
      IF (IW62.EQ.83.AND.PROB(0.33).AND.IXT(319).NE.0) THEN
          IVEN(36)=152
c         CALL SGLOD(NAME,LU,6043)
          p2=6043
          return
      ENDIF
      IF (IW62.EQ.83) then
c      CALL SGLOD(NAME,LU,6044)
          p2=6044
          return
      endif
C
C *** je pobliz ***
C
      IF (IVEN(IW62).NE.IR.AND.IW62.GT.47) then
c     1 CALL SGLOD(NAME,LU,8030)
          p2=8030
          return
      endif
C
C *** ma to ***
C
      IF (IW62.LT.54.AND.IVEN(IW62).LT.20000) then
c     1 CALL SGLOD(NAME,LU,8005)
          p2=8005
          return
      endif
C
C *** drevo ***
C
      IF (IW62.EQ.67) IXT(187)=1314
      IF (IW62.EQ.67) then
c      CALL SGLOD(NAME,LU,1095,IR)
          p2=1095
          p3=ir
          return
      endif
c     CALL SGLOD(NAME,LU,8010)
      p2=8010
      return
C                            MM-H-4  (LINE 7200)
C 5-H
C  ****** ANSWER ROUTINE ******
C
C  *** zvednout telefon ***
C
 7000 IF (IW52 .NE. 3.OR.IW62.NE.73.OR.IW71.NE.2) then
c     1 CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** je svetlo ***
C
      CALL MMRL
C
C *** zprava ***
C
      IF (IR .NE. 11) then
c      CALL SGLOD(NAME,LU,8031)
          p2=8031
          return
      endif
      IC=ITD(IVEN(73))
      IF (IC.NE.0)IVEN(73)=1000
      IF (IC.NE.0) then
c      CALL SGLOD(NAME,LU,7010,IC)
          p2=7010
          p3=ic
          return
      endif
c     CALL SGLOD(NAME,LU,7090)
      p2=7090
      return
C
C ****** FEED ROUTINE ******
C
C
C *** nakrmit obyvatele nebo opici ***
C
 8000 IF (IW71.NE.2.OR.(IW52.NE.4.AND.IW62.NE.63)) then
c     1 call SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** je svetlo ***
C
      CALL MMRL
C
C *** nakrmit opici ***
C
      IF (IR.NE.1.AND.IW62.EQ.63) then
c      CALL SGLOD(NAME,LU,2030)
           p2=2030
           return
      endif
      IF ((IR .EQ. 1) .AND. (IW62 .EQ. 63)) IXT(1)=401
      IF ((IR.EQ.1).AND.(IW62.EQ.63)) then
c      CALL SGLOD(NAME,LU,1095,IR)
          p2=1095
          p3=ir
          return
      endif
C
C *** ma jidlo ***
C
      IF (IVEN(24).LT.20000) then
c      CALL SGLOD(NAME,LU,8006)
          p2=8006
          return
      endif
C
C *** je zde obyvatel ***
C
      IF (IFSD(IRES(IW62)) .NE. IR) then
c      CALL SGLOD(NAME,LU,2030)
          p2=2030
          return
      endif
      IVEN(24)=309
c         !nove jidlo ve spizirne
c     CALL SGLOD(NAME,LU,8015)
      p2=8015
      return
C
C ****** DISPLAY ROUTINE ******
C
C *** kontrola hesla ***
C
 9000 IF (IW71 .GT. 1) then
c      CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
      IF (ITST(30).EQ.0) NAME(2)='SL'
      IF (ITST(30).EQ.0) then
c      CALL SGLOD(NAME,LU)
          return
      endif
      IC=NORM(57)+10
      WRITE(LU,9010) IC
 9010 FORMAT(/' ',I2' Zn ç heslo?'//' ?'\)
      READ(LU,9020)IANS,IAN,ID
C
C *** Po zadani hesla lze nastavit ITST(30)=0 pro snadny pristup.
C
      IF (LX.NE.0) WRITE(LX,9011)IC,IANS
 9011 FORMAT(I3,' Zn ç heslo?'//' ?',I3)
 9020 FORMAT(2A2,I3)
      WRITE(LU,9032)
 9032 FORMAT('A            DDDDDDDDDDDDAAAA')
      IF (IANS.EQ.'NE') then
c      CALL SGLOD(NAME,LU,9040)
          p2=9040
          return
      endif
      IF (IANS.NE.'AN'.OR.IC-ITST(28).NE.ID+23.OR.ITST(40).NE.IAN) then
c     1 CALL SGLOD(NAME,LU,9030)
          p2=9030
          return
      endif
      NAME(2)='SL'
c     CALL SGLOD(NAME,LU)
      return
C                            MM-H-5  (LINE 7280)
C 6-H
C ****** ATTACK AND KILL ROUTINE ******
C      ZAUTOCIT NA NEKOHO -> NAPADNOUT NEKOHO
C
10000 IF (IWRD(5,2).NE.5.OR.IWRD(6,2).NE.2) GOTO 10005
      IW71=IW71-1
      IW52=IWRD(5,3)
      IW62=IWRD(6,3)
      IWRD(6,2)=IW62
      IWRD(5,2)=IW52
C
10005 IF (IWRD(7,1).EQ.1) then
c     CALL SGLOD(NAME,LU,1000)
           p2=1000
           return
      endif
      IF (IW71.NE.2.OR.IW52.NE.4) then
c      CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** je svetlo ***
C
      CALL MMRL
C
C *** je zde a nazivu ***
C
      IC=IW62
      IF (IFSD(IRES(IC)).NE.IR) then
c      CALL SGLOD(NAME,LU,2030)
          p2=2030
          return
      endif
      IF (IRES(IC).GT.10000) then
c     CALL SGLOD(NAME,LU,20030)
          p2=20030
          return
      endif
C
C **** muze byt zabit ****
C
C *** vampir ***
C
      IF (IC.EQ.5.AND.IR.EQ.5.AND.IRES(5).EQ.5.AND.IVEN(29).GE.20000.
     1AND.(IVEN(11).GT.20000.OR.IVEN(14).GT.20000).AND.ITST(5).LT.50
     2.AND.IXT(183).EQ.405) then
c     CALL SGLOD(NAME,LU,10011)
          p2=10011
          return
      endif
      IF (IC.EQ.5) then
c      CALL SGLOD(NAME,LU,10020)
          p2=10020
          return
      endif
C
C *** WOLF OR WEREWOLF ***
C
      IF (IC.EQ.15.OR.IC.EQ.10) then
c      CALL SGLOD(NAME,LU,10030)
          p2=10030
          return
      endif
C
C *** boj ***
C
      IP=IRES(IC)/1000-ITST(31)
      DO 10010 I=1,89
      IF (I.NE.10.AND.I.NE.11.AND.I.NE.12.AND.I.NE.14.AND.I.NE.34.AND.
     1I.NE.45.AND.I.NE.48.AND.I.NE.50) GOTO 10010
      IF (IVEN(I).GT.20000)IP=IP+IFD(IVEN(I))
      IF (IFSD(IVEN(I)).EQ.IC.AND.IVEN(I).GT.10000.AND.IVEN(I).LT.
     120000) IP=IP-IFD(IVEN(J))
10010 CONTINUE
      IP=IP+NORM(5)-3
      IF (IP.EQ.6)IP=8
      IF (IP.EQ.7)IP=9
      IF (IP.EQ.-6)IP=-8
      IF (IP.EQ.-7)IP=-9
      IF (IP.GT.9)IP=10
      IF (IP.LT.-9)IP=-10
      IF (IP.LT.0)ITST(31)=-1*IP
      IF (IP.GT.0)IRES(IC)=IRES(IC)-1000*(IRES(IC)/1000)+1000*IP
      IF (IABS(IP).LT.2.AND.IC.NE.16) GOTO 10050
      IF (IP.LT.-9) then
c      CALL SGLOD(NAME,LU,10030)
          p2=10030
          return
      endif
      IF (IP.GT.9.AND.IC.EQ.13.AND.IR.EQ.33)IRES(13)=934
      IF (IP.GT.9.AND.IC.EQ.13.AND.IR.NE.33)IRES(13)=933
      IF (IP.GT.9.AND.IC.EQ.13) then
c      CALL SGLOD(NAME,LU,16045)
          p2=16045
          return
      endif
      IF (IP.GT.9) then
c      CALL SGLOD(NAME,LU,10040)
          p2=10040
          return
      endif
      IF (IP.GT.0) then
c      CALL SGLOD(NAME,LU,10041)
          p2=10041
          return
      endif
c     CALL SGLOD(NAME,LU,10031)
      p2=10031
      return
C
C *** utek ***
C
10050 DO 10070 J=1,360
      IF (IFSD(IXT(J)).NE.IR.OR.IXT(J).GT.20000) GOTO 10070
      IRES(IC)=IRES(IC)-IR+ITFD(IXT(J))
c     CALL SGLOD(NAME,LU,10060)
      p2=10060
      return
10070 CONTINUE
      IF (PROB(0.3)) then
c      CALL SGLOD(NAME,LU,10030)
          p2=10030
          return
      endif
      IRES(IC)=10000+IR
c      CALL SGLOD(NAME,LU,10040)
      p2=10040
      return
C
C                            MM-H-6  (LINE 7360)
C 7-H
C
C *** Revize se meni, kdykoliv dojde k podstatnym zmenam v hre.
C     Hra uchovana s jinym cislem revize nebude pracovat.
C
C ****** SUSPEND ROUTINE ******
C
11000 IF (IW71 .GT. 1) then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
*      IC=0
*11010 WRITE(LU,11020)
*11020 FORMAT(/' Napiste lu kazety (zaporne) nebo 1 pro ukonceni'//'?'\)
*      READ(LU,*) IC
*      IF (LX.NE.0) WRITE(LX,11021)IC
*11021 FORMAT(/' Napiste lu kazety nebo 1 pro ukonceni'//' ?'I3)
*      IF (IC .EQ. 1) then
*c      CALL SGLOD(NAME,LU,1040)
*          p2=1040
*          return
*      endif
11050 WRITE(LU,11060)
11060 FORMAT(/' Jm‚no souboru pro ulo§en¡ hry :'\)
      READ(LU,'(a64)')namf
*      IF (LX.NE.0) WRITE(LX,11061)(NAMF(I),I=2,3)
*11061 FORMAT(/' Zadejte ctyrznakove jmeno pro ulozeni.'//' ?'2A2)
*11070 FORMAT(2A2)
*      NAMF(1)='MM'
c     CALL OPEN(IDCB,IERR,NAMF,0,0,IC)
c      IF (IERR .EQ. -6) CALL CREAT(IDCB,IERR,NAMF,12,3,0,IC)
c      IF (IERR .LT. 0) CALL SGLOD(NAME,LU,11075,IERR)
      open(10,file=namf,status='new',form='binary',err=11081)
      DO 11080 I=1,8
      DO 11080 J=1,4
11080 IVRBX(J,I)=IVRB(J,3+I)
c      CALL WRITF(IDCB,IERR,IVRBX,32)
      write(10,err=11081)ivrbx
c      IF (IERR.LT.0) CALL SGLOD(NAME,LU,11075,IERR)
c      CALL WRITF(IDCB,IERR,IROM,99)
      write(10,err=11081)irom
c      CALL WRITF(IDCB,IERR,IRES,17)
      write(10,err=11081)ires
c      CALL WRITF(IDCB,IERR,IVEN,107)
      write(10,err=11081)iven
c      CALL WRITF(IDCB,IERR,IXT,360)
      write(10,err=11081)ixt
c      CALL WRITF(IDCB,IERR,ITST,40)
      write(10,err=11081)itst
      write(10,err=11081)iwp
c      CALL CLOSE(IDCB)
      close(10)
c      CALL SGLOD(NAME,LU,11040,NAMF(1),NAMF(2),NAMF(3))
      p2=11040
      return
11081 p2=11075
      return
C
C ****** RESTORE ROUTINE ******
C
12000 IF (IW71 .GT. 1) then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
*      IC=0
*12010 WRITE(LU,12020)
*12020 FORMAT(/' Zadejte lu kazety (zaporne) nebo 1 pro ukonceni'//'?'\)
*      READ(LU,*) IC
*      IF (LX.NE.0) WRITE(LX,12021)IC
*12021 FORMAT(/' Zadejte lu kazety nebo 1 pro ukonceni'//' ?'I3)
*      IF (IC .EQ. 1) then
*c      CALL SGLOD(NAME,LU,1040)
*          p2=1040
*          return
*      endif
12050 WRITE(LU,12060)
12060 FORMAT(/' Jm‚no souboru s ulo§enou hrou :'\)
      READ(LU,'(a64)')namf
*      IF (LX.NE.0) WRITE(LX,12061)(NAMF(I),I=2,3)
*12061 FORMAT(/' Zadejte ctyrznakove jmeno ulozenenho souboru.'//' ?'
*     12A2)
*      NAMF(1)='MM'
c      CALL OPEN(IDCB,IERR,NAMF,0,0,IC)
      open(10,file=namf,status='old',form='binary',err=12081)
c      IF (IERR .LT. 0) CALL SGLOD(NAME,LU,11075,IERR)
c      CALL READF(IDCB,IERR,IVRBX,32)
      read(10,err=12081)ivrbx
c      IF (IERR.LT.0) CALL SGLOD(NAME,LU,11075,IERR)
      DO 12080 I=1,8
      DO 12080 J=1,4
12080 IVRB(J,3+I)=IVRBX(J,I)
      IXX=ITST(19)
c      CALL READF(IDCB,IERR,IROM,99)
      read(10,err=12081)irom
c      CALL READF(IDCB,IERR,IRES,17)
      read(10,err=12081)ires
c      CALL READF(IDCB,IERR,IVEN,107)
      read(10,err=12081)iven
c      CALL READF(IDCB,IERR,IXT,360)
      read(10,err=12081)ixt
c      CALL READF(IDCB,IERR,ITST,40)
      read(10,err=12081)itst
      read(10,err=12081)iwp
c      CALL CLOSE(IDCB)
      close(10)
      CALL cas(10,ITIM)
      ITST(19)=IXX
      ITST(18)=ITIM(4)*1200+ITIM(3)*20+ITIM(2)/3
      ITST(20)=ITST(20)+1
c      CALL SGLOD(NAME,LU,12040,NAMF(1),NAMF(2),NAMF(3))
      p2=12040
      return
12081 p2=11075
      return
C
C                            MM-H-7  (LINE 7440)
C 8-H
C ****** DRINK ROUTINE ******
C
C
C *** drink
C
13000 IF (IW71.GT.1 .AND. IW52.NE. 3) then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
      IF (IW71.GT.2) then
c      CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
      IF (IW71.EQ.1) THEN
         IWRD(5,2)=3
         IWRD(6,2)=62
         IW62=62
      ENDIF
      IF (IW62.NE.13 .AND. IW62.NE.60 .AND. IW62.NE.62) then
c     1 CALL SGLOD(NAME,LU,2020)
          p2=2020
          return
      endif
C
C *** WINE ***
C
      IF (IW62.EQ.60.AND.IR.NE.16) then
c      CALL SGLOD(NAME,LU,2035)
           p2=2035
           return
      endif
      IF (IW62.EQ.60)IXT(128)=1316
      IF (IW62.EQ.60) then
c      CALL SGLOD(NAME,LU,1095,IR)
          p2=1095
          p3=ir
          return
      endif
C
C *** jed ***
C
      IF (IW62.EQ.13.AND.(IVEN(13).LT.20000.OR.ITST(10).EQ.13)) then
c     1 CALL SGLOD(NAME,LU,2036)
          p2=2036
          return
      endif
      IF (IW62.EQ.13)NAME(2)='SD'
      IF (IW62.EQ.13) then
c      CALL SGLOD(NAME,LU,13010)
          p2=13010
          return
      endif
C
C *** voda ***
C
      IF (IW62.EQ.62.AND.(IR.LT.68.OR.IR.GT.83).AND.IR.NE.99) then
c     1 CALL SGLOD(NAME,LU,2038)
          p2=2038
          return
      endif
      IF (IW62.EQ.62) then
c      CALL SGLOD(NAME,LU,1010)
          p2=1010
          return
      endif
c      CALL SGLOD(NAME,LU,320)
      p2=320
      return
C
C ****** MYSTERY CHANGE ******
C
14000 IF (IW71.GT.1) GOTO 14100
C
C *** pouze pri prvnim tahu ***
C
      IF (ITST(5).GT.2) then
c      CALL SGLOD(NAME,LU,14010)
          p2=14010
          return
      endif
C
C *** cislo zahady ***
C
14020 WRITE(LU,14030)
14030 FORMAT(/' ¬¡slo z hady? '\)
      IF (LA.EQ.0)READ(LU,*)IC
      IF (LA.NE.0)READ(LA,14032)IC
      IF (LO.NE.0) WRITE(LO,14032)IC
      IF (LX.NE.0) WRITE(LX,14031)IC
14031 FORMAT(/' ¬¡slo z hady? 'I4)
14032 FORMAT(I6)
      IF (IC.LT.1.OR.IC.GT.999) GOTO 14020
      RND=.001*IC
      WRITE(LU,14035)IC
      IF (LX.NE.0) WRITE(LX,14035)IC
14035 FORMAT(/' Hra je nyn¡ nastavena na z hadu Ÿ¡s.'I4)
      ITST(23)=IC
      NAME(2)='SA'
c      CALL SGLOD(NAME,LU,0,ITST(21),ITST(38))
      p2=0
      p3=itst(21)
      p4=itst(38)
      return
C
C *** MYSTERY MANSION ***
C
14100 IF (IWRD(7,1).GT.2.OR.IWRD(6,2).NE.107) then
c     1   CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
c      CALL SGLOD(NAME,LU,14110)
      p2=14110
      return
C
C                            MM-H-8  (LINE 7520)
C 9-H
C ****** BRIBE ROUTINE ******
C
C
C *** podplatit nekoho ***
C
15000 IF (IW71.NE.2.OR.IW52.NE.4) then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** je u hrace ***
C
      IF (IFSD(IRES(IW62)).NE.IR) then
c      CALL SGLOD(NAME,LU,2030)
          p2=2030
          return
      endif
C
C *** ma hrac pokladnici ***
C
      IF (IVEN(7).LT.20000) then
c      CALL SGLOD(NAME,LU,15200)
          p2=15200
          return
      endif
C
C *** je obyvatel nazivu ***
C
      IF (IRES(IW62).GT.10000) then
c      CALL SGLOD(NAME,LU,15400)
          p2=15400
          return
      endif
C
C *** vi vubec neco ***
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
      IF(IC.EQ.0) then
c      CALL SGLOD(NAME,LU,15400)
          p2=15400
          return
      endif
      IVEN(7)=0
      IRES(IG)=0
c      CALL SGLOD(NAME,LU,15500)
      p2=15500
      return
C
C                            MM-H-9  (LINE 7600)
C 10-H
C ****** QUESTION ROUTINE ******
C
16000 IF (IWRD(7,1).EQ.1) then
c     CALL SGLOD(NAME,LU,1001)
          p2=1001
          return
      endif
      IF (IWRD(7,1).NE.2.OR.IWRD(5,2).NE.4) then
c     1  CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** je s obyvatelem ***
C
      IF (IFSD(IRES(IWRD(6,2))).NE.IR) then
c      CALL SGLOD(NAME,LU,2030)
          p2=2030
          return
      endif
C
C *** je nazivu a umi mluvit ***
C
      IF (IRES(IWRD(6,2)).GE.1000.OR.IWRD(6,2).EQ.4.OR.IWRD(6,2).EQ.15)
     1 then
c     CALL SGLOD(NAME,LU,20030)
          p2=20030
          return
      endif
C
C *** protiva ***
C
      IF(ITST(5)-2 .GT. ITST(13) .AND. PROB(0.5)) then
c     1  CALL SGLOD(NAME,LU,23120)
          p2=23120
          return
      endif
      IF (ITST(5)-4.GT.ITST(13)) then
c      CALL SGLOD(NAME,LU,23145)
          p2=23145
          return
      endif
      IF (ITST(5)-2.GT.ITST(13)) then
c      CALL SGLOD(NAME,LU,23140)
          p2=23140
          return
      endif
C
C *** vampir ***
C
      IF (IWRD(6,2).EQ.5) then
c      CALL SGLOD(NAME,LU,23010)
          p2=23010
          return
      endif
C
C *** vlokdlak ***
C
      IF (IWRD(6,2).EQ.10) then
c      CALL SGLOD(NAME,LU,23015)
          p2=23015
          return
      endif
C
C *** skritek a skret ***
C
      IF (IWRD(6,2).EQ.11.OR.IWRD(6,2).EQ.13) then
c     1    CALL SGLOD(NAME,LU,23020)
          p2=23020
          return
      endif
C
C *** lesnik a lovec ***
C
      R=URAN()
      IF (IWRD(6,2).NE.6.AND.IWRD(6,2).NE.12) GOTO 23130
      IF (IXT(54).GT.20000.AND.IR.NE.93.AND.R.LT..3) then
c     : CALL SGLOD(NAME,LU,23030)
          p2=23030
          return
      endif
      IF (IXT(54).GT.20000.AND.IR.EQ.93.AND.R.LT..3) then
c     : CALL SGLOD(NAME,LU,23040)
           p2=23040
           return
      endif
      IF (IXT(54).LT.20000.AND.R.LT..3) then
c      CALL SGLOD(NAME,LU,23050)
          p2=23050
          return
      endif
      IF (R.LT..4) then
c      CALL SGLOD(NAME,LU,23060)
          p2=23060
          return
      endif
      IF (R.LT..5) then
c      CALL SGLOD(NAME,LU,23070)
          p2=23070
          return
      endif
      IF (R.LT..58) then
c      CALL SGLOD(NAME,LU,23080)
          p2=23080
          return
      endif
      IF (R.LT..66) then
c      CALL SGLOD(NAME,LU,23090)
          p2=23090
          return
      endif
      IF (R.LT..74) then
c      CALL SGLOD(NAME,LU,23100)
          p2=23100
          return
      endif
      IF (R.LT..82) then
c      CALL SGLOD(NAME,LU,23110)
          p2=23110
          return
      endif
c      CALL SGLOD(NAME,LU,23120)
      p2=23120
      return
C
C *** vrah ***
C
23130 IF (IWRD(6,2).NE.ITST(9)) GOTO 23160
      IF (R.LT..33) then
c      CALL SGLOD(NAME,LU,23120)
          p2=23120
          return
      endif
      IF (R.LT..67) then
c      CALL SGLOD(NAME,LU,23140)
          p2=23140
          return
      endif
c      CALL SGLOD(NAME,LU,23150)
      p2=23150
      return
C
C *** pan a sluha ***
C
23160 IF (IWRD(6,2).NE.3.AND.IWRD(6,2).NE.14) GOTO 23250
      IF (R.GT..3) GOTO 23200
      IF (ITST(8).LT.28.AND.R.LT..15) then
c      CALL SGLOD(NAME,LU,23170)
          p2=23170
          return
      endif
      IF (ITST(8).LT.28) then
c      CALL SGLOD(NAME,LU,23180)
          p2=23180
          return
      endif
      IF (IVEN(ITST(10)).LT.10000.AND.IFSD(IVEN(ITST(10))).LT.28) then
c     1 CALL SGLOD(NAME,LU,23190)
          p2=23190
          return
      endif
c      CALL SGLOD(NAME,LU,23150)
      p2=23150
      return
23200 IF (R.LT..4) then
c     CALL SGLOD(NAME,LU,23210)
          p2=23210
          return
      endif
      IF (R.LT..5) then
c      CALL SGLOD(NAME,LU,23220)
          p2=23220
          return
      endif
      IF (R.LT..6) then
c      CALL SGLOD(NAME,LU,23230)
          p2=23230
          return
      endif
      IF (R.LT..7) then
c      CALL SGLOD(NAME,LU,23240)
          p2=23240
          return
      endif
      IF (R.LT..8) then
c      CALL SGLOD(NAME,LU,23140)
          p2=23140
          return
      endif
c      CALL SGLOD(NAME,LU,23120)
      p2=23120
      return
C
C *** pani a sluzka ***
C
23250 IF (IWRD(6,2).NE.8.AND.IWRD(6,2).NE.2) GOTO 23310
      IF (R.GT..3) GOTO 23260
      IF (ITST(8).GT.9.AND.R.LT..15) then
c      CALL SGLOD(NAME,LU,23170)
          p2=23170
          return
      endif
      IF (ITST(8).GT.9) then
c      CALL SGLOD(NAME,LU,23180)
          p2=23180
          return
      endif
      IF (IVEN(ITST(10)).LT.10000.AND.IFSD(IVEN(ITST(10))).GT.9) then
c     1 CALL SGLOD(NAME,LU,23190)
          p2=23190
          return
      endif
c      CALL SGLOD(NAME,LU,23150)
      p2=23150
      return
23260 IF (R.LT..4) then
c     CALL SGLOD(NAME,LU,23270)
          p2=23270
          return
      endif
      IF (R.LT..5) then
c      CALL SGLOD(NAME,LU,23280)
          p2=23280
          return
      endif
      IF (R.LT..6) then
c      CALL SGLOD(NAME,LU,23290)
           p2=23290
           return
      endif
      IF (R.LT..7) then
c       CALL SGLOD(NAME,LU,23300)
           p2=23300
           return
      endif
      IF (R.LT..8) then
c      CALL SGLOD(NAME,LU,23140)
          p2=23140
          return
      endif
c      CALL SGLOD(NAME,LU,23150)
      p2=23150
      return
C
C *** sadar a kuchar ***
C
23310 IF (R.GT..3) GOTO 23320
      IF (ITST(8).LT.10.OR.ITST(8).GT.27.AND.R.LT..15) then
c     1 CALL SGLOD(NAME,LU,23170)
          p2=23170
          return
      endif
      IF (ITST(8).LT.10.OR.ITST(8).GT.27) then
c     1 CALL SGLOD(NAME,LU,23180)
          p2=23180
          return
      endif
      IF (IVEN(ITST(10)).LT.10000.AND.IFSD(IVEN(ITST(10))).GT.9) then
c     1 CALL SGLOD(NAME,LU,23190)
          p2=23190
          return
      endif
c      CALL SGLOD(NAME,LU,23150)
      p2=23150
      return
23320 IF (R.LT..4) then
c     CALL SGLOD(NAME,LU,23330)
           p2=23330
           return
      endif
      IF (R.LT..5) then
c      CALL SGLOD(NAME,LU,23340)
          p2=23340
          return
      endif
      IF (R.LT..6) then
c      CALL SGLOD(NAME,LU,23350)
           p2=23350
           return
      endif
      IF (R.LT..7) then
c      CALL SGLOD(NAME,LU,23360)
          p2=23360
          return
      endif
      IF (R.LT..8) then
c      CALL SGLOD(NAME,LU,23140)
          p2=23140
          return
      endif
c      CALL SGLOD(NAME,LU,23150)
      p2=23150
      return
C
C ****** LISTEN ROUTINE ******
C
C *** poslouchat ***
C
17000 IF (IWRD(7,1).NE.1) then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
      IX=0
      IC=0
      ID=24000+IR
      IF (IR.GT.53)ID=24100
      IF (IVEN(3).GT.20000.OR.IVEN(3).EQ.200+IR.OR.(IVEN(3).GT.10000.
     1AND.IFSD(IVEN(3)).EQ.IR))IX=24108
      DO 24010 I=2,15
      IF (IRES(I).GT.10000.OR.IFSD(IRES(I)).NE.IR) GOTO 24010
      IX=24110
      IF (I.NE.4.AND.I.NE.10.AND.I.NE.15)IC=IC+1
      IF (IC.EQ.1)IX=24120
      IF (IC.EQ.2)IX=24130
      IF (IC.EQ.3)IX=24140
24010 CONTINUE
      IF (IRES(6)+IRES(12).LT.10000.AND.IFSD(IRES(6)).EQ.IR.AND.
     1IFSD(IRES(12)).EQ.IR)IX=24150
      IF (IRES(3)+IRES(14).LT.10000.AND.IFSD(IRES(3)).EQ.IR.AND.
     1IFSD(IRES(14)).EQ.IR.AND.ITST(9).NE.3.AND.ITST(9).NE.14.
     2AND.ITST(8).LT.28)IX=24160
      RNX=77.7*IR/253.-INT(77.7*IR/253.)
      IF (IX.EQ.24160.AND.RNX.GT..8)IX=24164
      IF (IX.EQ.24160.AND.RNX.GT..6)IX=24162
      IF (IRES(7)+IRES(9).LT.10000.AND.IFSD(IRES(7)).EQ.IR.AND.
     1IFSD(IRES(9)).EQ.IR.AND.ITST(9).NE.7.AND.ITST(9).NE.9.AND.
     2(ITST(8).LT.10.OR.ITST(8).GT.27))IX=24170
      IF (IX.EQ.24170.AND.RNX.GT..8)IX=24174
      IF (IX.EQ.24170.AND.RNX.GT..6)IX=24172
      IF (IRES(2)+IRES(8).LT.10000.AND.IFSD(IRES(2)).EQ.IR.AND.
     1IFSD(IRES(8)).EQ.IR.AND.ITST(9).NE.2.AND.ITST(9).NE.8.AND.
     2ITST(8).GT.27)IX=24180
      IF (IX.EQ.24180.AND.RNX.GT..8)IX=24184
      IF (IX.EQ.24180.AND.RNX.GT..6)IX=24182
      IF (ID.EQ.24100.AND.IX.NE.0)ID=24105
c      CALL SGLOD(NAME,LU,ID,IX)
      p2=id
      p3=ix
      return
      END
