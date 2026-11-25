$storage:2
$large
$nodebug
      subroutine mmsj(p1,p2,p3,p4,p5)
c      ,10-02-1983 MMM    8536E D45 PL&BM <850906.1803>
C     04-02-1988 MS-DOS S.M.
C
C ******* SEGMENT J *******
C
$INCLUDE:'MMCO.for'
      integer*2 p1,p2,p3,p4,p5
      DIMENSION KEYM(9),LKEY(30)
      EQUIVALENCE (KEYM(1),LKEY(1))
C
C ***** Tento segment je volan segmentem mmsb, provadi funkce vyssich
C       sloves. Normalne je ukoncen segmentem MMSK
C
C *** funkce ***
C
C * prvni & druha cislice, treti & ctvrta cislice, treti cislice,
C   ctvrta cislice a nahodne cislof
C
      IFSD(I)=I-(I/100)*100
      ITFD(I)=I/100-(I/10000)*100
      ITD(I)=I/100-(I/1000)*10
      IFD(I)=I/1000-(I/10000)*10
C
C *** nastaveni promennych ***
C
      NAME(2)='SK'
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
C ***** index *****
C
C       ROUTINE               PAGE
C
C       UNLOCK AND LOCK      2
C       SHOOT                3
C       OIL                  4
C       RECORD               5
C       BREAK                6
C       PLACE                6
C       SLEEP                7
C                            MM-J-1  (LINE 8640)
C 2-J
C ***** vyber procedury *****
C
      GOTO(1000,1000,2000,3000,4000,5000,6000,7000)IWRD(6,1)-53
C
C ****** UNLOCK AND LOCK ******
C
C *** kontrola formatu ***
C
 1000 IF (IWRD(7,1).EQ.1) then
c      CALL SGLOD(NAME,LU,1000)
          p2=1000
          return
      endif
      IF (IWRD(7,1).NE.2.OR.IWRD(5,2).NE.3) then
c     1 CALL SGLOD(NAME,LU,1000)
          p2=1000
          return
      endif
C
C *** dvere ***
C
      IF (IWRD(6,2).NE.92) GOTO 15100
      IF ((IR.GT.27.AND.IR.NE.36.AND.IR.NE.54.AND.IR.NE.91.AND.IR.NE.
     190).OR.IR.EQ.3.OR.IR.EQ.4.OR.IR.EQ.13.OR.IR.EQ.16.OR.IR.EQ.21
     2.OR.IR.EQ.22) then
c     CALL SGLOD(NAME,LU,2030)
          p2=2030
          return
      endif
C
C
C
      DO 15010 J=1,240
      IF (IFSD(IXT(J)) .NE. IR) GOTO 15010
      IF ((J-1)/60+1 .NE. ITST(4)/1000) GOTO 15010
      IC=ITFD(IXT(J))
C
C     *** neni obracen ke dverim *** (tajne pruchody )
C
      IF (IC.EQ.4.OR.IC.EQ.13.OR.IC.EQ.22.OR.J.EQ.62.OR.J.EQ.70.OR.J
     1.EQ.133.OR.J.EQ.182.OR.J.EQ.190) then
c     CALL SGLOD(NAME,LU,15030)
          p2=15030
          return
      endif
C
C     *** nemaji zamek ***
C
      IF (IXT(J) .LT. 10000) then
c      CALL SGLOD(NAME,LU,15020)
          p2=15020
          return
      endif
C
C     *** je odemceno ***
C
      IF (((IXT(J) .LT. 20000) .AND. (IWRD(6,1) .EQ. 54)) .OR.
     1((IXT(J) .GT. 20000) .AND. (IWRD(6,1) .EQ. 55))) then
c     2 CALL SGLOD(NAME,LU,8020)
          p2=8020
          return
      endif
C
C     *** nema klicek od vezeni ***
C
      IF (IVEN(46).LT.20000.AND.J.EQ.64) then
c      CALL SGLOD(NAME,LU,15041)
          p2=15041
          return
      endif
C
C     *** nema klice ***
C
      IF (IVEN(2) .LT. 20000.AND.J.NE.64) then
c     1  CALL SGLOD(NAME,LU,15040)
          p2=15040
          return
      endif
C
C     odemceni, zamceni
C
      IF (IWRD(6,1) .EQ. 54) IXT(J)=IXT(J)-10000
      IF (IWRD(6,1) .EQ. 55) IXT(J)=IXT(J)+10000
      IF ((J.LT.121).AND.(IWRD(6,1).EQ.54))IXT(J+120)=IXT(J+120)-10000
      IF ((J.GT.120).AND.(IWRD(6,1).EQ.54))IXT(J-120)=IXT(J-120)-10000
      IF ((J.LT.121).AND.(IWRD(6,1).EQ.55))IXT(J+120)=IXT(J+120)+10000
      IF ((J.GT.120).AND.(IWRD(6,1).EQ.55))IXT(J-120)=IXT(J-120)+10000
c      CALL SGLOD(NAME,LU,1040)
      p2=1040
      return
15010 CONTINUE
c      CALL SGLOD(NAME,LU,15030)
      p2=15030
      return
C
C *** truhla ***
C
15100 IF (IWRD(6,2).NE.54.AND.IWRD(6,2).NE.35) GOTO 15200
      IF (IVEN(35).LT.20000.AND.IVEN(35).NE.600+IR) then
c     1   CALL SGLOD(NAME,LU,8030)
          p2=8030
          return
      endif
C
C ** zamknout truhlu **
C
      IF (IWRD(6,1).EQ.55.AND.IFD(IVEN(35)).EQ.0) then
c     1 CALL SGLOD(NAME,LU,8020)
          p2=8020
          return
      endif
      IF (IWRD(6,1).EQ.55) then
c      CALL SGLOD(NAME,LU,2010)
          p2=2010
          return
      endif
C
C ** odemknout truhlu **
C
      IF (IVEN(2).LT.20000) then
c      CALL SGLOD(NAME,LU,15040)
           p2=15040
           return
      endif
c      CALL SGLOD(NAME,LU,15110)
      p2=15110
      return
15200 CONTINUE
c15900 CALL SGLOD(NAME,LU,8010)
15900 p2=8010
      return
C
C
C                            MM-J-2  (LINE 8720)
C 3-J
C
C ****** SHOOT ******
C
 2000 IF (IWRD(5,2).NE.5) GOTO 2005
c     ! NENI PREDLOZKA
      IF (IWRD(6,2).NE.2.AND.IWRD(6,2).NE.15) GOTO 2005
c       ! NENI 'NA','DO'
      IWRD(7,1)=IWRD(7,1)-1
      IWRD(6,2)=IWRD(6,3)
c           ! predlozku vypustit
      IWRD(5,2)=IWRD(5,3)
C
C     *** koho chce zastrelit ***
C
 2005 IF (IWRD(7,1).EQ.1) then
c     CALL SGLOD(NAME,LU,1003)
          p2=1003
          return
      endif
C
C     *** zastrelit obyvatele nebo zamek ***
C
      IF (IWRD(7,1).NE.2.OR.(IWRD(5,2).NE.4.AND.IWRD(5,2).NE.3).OR.
     1  (IWRD(5,2).EQ.3.AND.IWRD(6,2).NE.54)) then
c     2  CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** ma pistoli ***
C
      IF (IVEN(9) .LT. 20000) then
c      CALL SGLOD(NAME,LU,16010)
          p2=16010
          return
      endif
C
C *** je svetlo ***
C
      CALL MMRL
C
C *** je cil zde ***
C
16019 IF (IFSD(IRES(IWRD(6,2))).NE.IR.AND.IWRD(5,2).NE.3) then
c     1 CALL SGLOD(NAME,LU,2030)
          p2=2030
          return
      endif
      IF (IWRD(5,2).EQ.3.AND.IFD(IVEN(35)).NE.0) then
c     1 CALL SGLOD(NAME,LU,2030)
          p2=2030
          return
      endif
C
C *** ma kulku? ***
C
      IC=0
      IF (IVEN(20).GT.20000)IC=1
C
C     *** vlkodlak ***
C
      IF (IWRD(6,2).EQ.10 .and. IC .eq. 1) GOTO 16020
C
C     *** kulky nejsou ***
C
      IF ((ITST(11) .EQ. 0) .AND. (IVEN(20) .LT. 20000)) then
c     1 CALL SGLOD(NAME,LU,16020)
          p2=16020
          return
      endif
      IF ((ITST(11).EQ.0.OR.IWRD(6,2).EQ.10).AND.IVEN(20).GT.20000)
     1IVEN(20)=0
      IF (((ITST(11).GT.0).AND.(IWRD(6,2).NE.10)) .OR. ((IWRD(6,2)
     1.EQ.10).AND.(IVEN(20).LT.20000)))ITST(11)=ITST(11)-1
C
C *** vedle nebo zraneni ***
C
      IF (PROB(0.2)) then
c      CALL SGLOD(NAME,LU,16030)
           p2=16030
           return
      endif
      IF (PROB(0.4) .AND. IRES(IWRD(6,2))/1000 .EQ. 0) THEN
         IRES(IWRD(6,2))=IRES(IWRD(6,2))+5000
c         CALL SGLOD(NAME,LU,16031)
         p2=16031
         return
      ENDIF
C
C *** zamek ***
C
      IF (IWRD(5,2).EQ.3) then
c      CALL SGLOD(NAME,LU,16035)
          p2=16035
          return
      endif
C
C *** muze byt obyvatel zabit? ***
C
      IF ((IWRD(6,2) .EQ. 1) .OR. (IWRD(6,2).EQ. 5) .OR.
     1 ((IWRD(6,2) .EQ. 10) .AND. (IC .EQ. 0))
     2.OR.(IRES(IWRD(6,2)).GT.10000)) then
c     CALL SGLOD(NAME,LU,16040)
          p2=16040
          return
      endif
C
C *** skret ***
C
      IF (IWRD(6,2).EQ.13.AND.IR.NE.33) IRES(13)=533
      IF (IWRD(6,2).EQ.13.AND.IR.EQ.33) IRES(13)=534
      IF (IWRD(6,2).EQ.13) then
c      CALL SGLOD(NAME,LU,16045)
          p2=16045
          return
      endif
C
C *** vlkodlak ***
C
16020 IF (IWRD(6,2).EQ.10)IRES(ITST(39))=10000+IR
      IF (IWRD(6,2).EQ.10)ITST(2)=ITST(2)+75
C
C *** rytir ***
C
      IF (IWRD(6,2).EQ.16.AND.PROB(0.8).AND.IVEN(45).EQ.14416.AND.
     1IRES(16)/1000.LT.4) then
c     CALL SGLOD(NAME,LU,16046)
          p2=16046
          return
      endif
C
C *** zabiti obyvatele ***
C
      IRES(IWRD(6,2))=10000+IR
      IF (IWRD(6,2).EQ.10)IRES(10)=0
c      CALL SGLOD(NAME,LU,16050)
      p2=16050
      return
C
C                            MM-J-3  (LINE 8800)
C 4-J
C
C ****** OIL ******
C
C *** syntax ***
C
 3000 IF ((IWRD(7,1).NE.2).OR.(IWRD(5,2).NE.3)) then
c     1 CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** je svetlo ***
C
      CALL MMRL
C
C *** ma olejnicku ***
C
      IF (IVEN(33) .LT. 20000) then
c      CALL SGLOD(NAME,LU,20010)
          p2=20010
          return
      endif
C
C *** ma olej ***
C
      IF (IVEN(33) .GT. 22000) then
c      CALL SGLOD(NAME,LU,20020)
          p2=20020
          return
      endif
C
C *** ma predmet, ktery chce olejovat ***
C
      IF ((IWRD(6,2) .LT. 54) .AND. (IVEN(IWRD(6,2)) .LT. 20000)) then
c     1 CALL SGLOD(NAME,LU,3010)
          p2=3010
          return
      endif
C
C *** je u neho ***
C
      IF ((IWRD(6,2) .GT. 47) .AND. (IFSD(IVEN(IWRD(6,2))) .NE. IR)
     1.AND. (IFSD(IVEN(IWRD(6,2))) .NE. 0)) then
c     2 CALL SGLOD(NAME,LU,2030)
          p2=2030
          return
      endif
C
C     *** brneni, klicek, baterie, rukavice, papousek, palcat
C
      IF ((IWRD(6,2) .GT. 45) .AND. (IWRD(6,2) .LT. 51)) then
c     1 CALL SGLOD(NAME,LU,20030) ! nebudte hloupy
          p2=20030
          return
      endif
C
C     *** olejovat zaclony ***
C
      IF ((IWRD(6,2) .EQ. 91) .AND. (IROM(IR) .LT. 10000)) then
c     1 CALL SGLOD(NAME,LU,2030)
          p2=2030
          return
      endif
C
C     *** olejovat dvere -> uz jsou ***
C
      IF ((IWRD(6,2) .EQ. 92) .OR. (IWRD(6,2) .EQ. 93)) then
c     1 CALL SGLOD(NAME,LU,8020)
          p2=8020
          return
      endif
C
C * panty na pozarnim rebriku *
C
      IF ((IWRD(6,2) .EQ. 74) .AND. (IR .EQ. 85)) IXT(322)=9185
      IF ((IWRD(6,2) .EQ. 74) .AND. (IR .EQ. 86)) IXT(322)=8586
      IVEN(74)=IVEN(74)+1000
c      CALL SGLOD(NAME,LU,1040)
      p2=1040
      return
C
C                            MM-J-4  (LINE 8880)
C 5-J
C
C  ****** RECORD ******
C
 4000 IF (IWRD(7,1).GT.1) GOTO 21200
      WRITE(LU,21010)
21010 FORMAT(/' PýesvØdŸete se, zda je jednotka pýipravena a',
     1 ' zadejte lu #: '\)
      READ(LU,*)IANS
      IF (LX.NE.0) WRITE(LX,21011)IANS
21011 FORMAT(/' PýesvØdŸete se, zda je jednotka pýipravena a',
     1 ' zadejte lu #: 'I3)
      ITST(21)=IANS
c      CALL SGLOD(NAME,LU,21020)
      p2=21020
      return
C
C *** KEY ***
C
C     *** z na kazetu ***
C
21200 IF (IWRD(7,1).EQ.3.AND.IWRD(5,2).EQ.5.AND.(IWRD(6,2).EQ.7.OR.
     1IWRD(6,2).EQ.2).AND.IWRD(6,3).EQ.105) GOTO 22000
C
C     *** na tiskarnu ***
C
      IF (IWRD(7,1).EQ.3.AND.IWRD(5,2).EQ.5.AND.IWRD(6,2).EQ.2.AND.
     1IWRD(6,3).EQ.104) ITST(21)=6
c      ! tiskarna lu#6
      IF (IWRD(7,1).EQ.3.AND.IWRD(5,2).EQ.5.AND.IWRD(6,2).EQ.2.AND.
     1IWRD(6,3).EQ.104) then
c     CALL SGLOD(NAME,LU,21020)
          p2=21020
          return
      endif
C
C     *** zaznamenat klice ***
C
      IF (IWRD(7,1).NE.2.OR.IWRD(5,2).NE.3.OR.(IWRD(6,2).NE.2.AND.
     1IWRD(6,2).NE.2)) then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C     WRITE(LU,21202)
C21202 FORMAT(' Programove klice? (ANO/NE) '\)
C     READ(LU,21203)IANS
C21203 FORMAT(A2)
      ians='an'
      IF (IANS.EQ.'AN' .or. ians .eq. 'an') GOTO 21280
c       ! PROGRAMOVE
21205 WRITE(LU,21210)
21210 FORMAT(/' JZadejte: 0 .. konec, 9 .. vymaz nebo cislo klice',
     1/' a text: '\)
      READ(LU,21220)IANS,KEYM
21220 FORMAT(10A2)
      IANS=ians-IANS/256*256-48
      IF (IANS.EQ.0) then
c      CALL SGLOD(NAME,LU,1040)
          p2=1040
          return
      endif
      IF (IANS.EQ.9) GOTO 21260
      IF (IANS.LT.1.OR.IANS.GT.8) GOTO 21205
      WRITE(LU,21230)
21230 FORMAT('hmh'\)
      DO 21245 J=1,8
      IF (J.NE.IANS) WRITE(LU,21236)
21236 FORMAT('CCCCCCCCCCCCCCCCCCCC'\)
      IF (IANS.EQ.J.AND.J.LT.4) WRITE(LU,21240)KEYM
21240 FORMAT('&dF ',9A2,'&d@ ')
      IF (IANS.EQ.J.AND.J.GT.4.AND.J.LT.8) WRITE(LU,21241)KEYM
21241 FORMAT('&dB ',9A2,'&d@ ')
      IF (IANS.EQ.J.AND.J.EQ.4) WRITE(LU,21242)KEYM
21242 FORMAT('&dF ',9A2,'&d@')
      IF (IANS.EQ.J.AND.J.EQ.8) WRITE(LU,21243)KEYM
21243 FORMAT('&dB ',9A2,'&d@')
21245 CONTINUE
      WRITE(LU,21250)
21250 FORMAT('Bl')
      IANX='a0'
      ianx=ianx+IANS
      WRITE(LU,21251)IANX,KEYM
21251 FORMAT('&f2'A2'k18L'9A2)
      WRITE(LU,21252)
21252 FORMAT('hA')
      GOTO 21205
21260 WRITE(LU,21261)
21261 FORMAT('mhJ'\)
      DO 21265 I=1,11
      IF (I.LT.5) WRITE(LU,21262)
      IF (I.GT.4.AND.I.LT.9) WRITE(LU,21263)
21262 FORMAT('&dF                   &d@ '\)
21263 FORMAT('&dB                   &d@ '\)
      IF (I.GT.8) WRITE(LU,21264)
21264 FORMAT('                    '\)
21265 CONTINUE
      WRITE(LU,21266)
21266 FORMAT('                   ')
      DO 21268 I=1,8
      IANX='a0'
      ianx=ianx+I
      IANY='o'
      iany=iany+I
      WRITE(LU,21267)IANX,IANY
21267 FORMAT('&f2'A2'k 2L'A2)
21268 CONTINUE
      GOTO 21205
C
C  ** PROGRAMOVE KLICE **
C
21280 WRITE(LU,21282)
21282 FORMAT(' Zadejte 0..konec, 9..vìpis nebo Ÿ¡slo(1..8),text',
     : /'?'\)
      READ(LU,21284)IANS,LKEY
21284 FORMAT(31A2)
      IANS=ians-IANS/256*256-48
      IF (IANS.EQ.0) then
c      CALL SGLOD(NAME,LU,1040)
          p2=1040
          return
      endif
      IF (IANS.LT.0.OR.IANS.GT.9) GOTO 21280
      IF (IANS.NE.9) GOTO 21285
c     CALL REIO(2,LU,'Programove klice: ',9)
      write(*,'(''Programov‚ kl¡Ÿe: '')')
      LKEY(2)='  '
      DO 21287 II=1,8
      LKEY(1)=KCVT(II)
c      CALL REIO(2,LU+2000B,LKEY,2)
      call reio(2,lu,lkey,2)
      CALL REIO(2,LU,KLICE(1,II),ABS(IDELKY(II)))
21287 CONTINUE
      GOTO 21280
C
21285 DO 21286 II=15,1,-1
      KLICE(II,IANS)=LKEY(II)
      IF (LKEY(II).NE.'  ') IDELKY(IANS)=15-II
21286 CONTINUE
      GOTO 21280
C
C                            MM-J-5  (LINE 8960)
C 6-J
C
C *** zaznamenat na kazetu ***
C
22000 WRITE(LU,22010)
22010 FORMAT(/' Zadej 0 pro ukonŸen¡ nebo LU p skov‚ jednotky'//'?'\)
      READ(LU,*)LA
      IF (IWRD(6,2).EQ.7)ITST(36)=LA
      IF (IWRD(6,2).EQ.7)ITST(37)=0
      IF (IWRD(6,2).EQ.2)ITST(37)=LA
      IF (IWRD(6,2).EQ.2)ITST(36)=0
c      CALL SGLOD(NAME,LU,1040)
      p2=1040
      return
C
C
C
C
C ****** BREAK ******
C
C
C     ** rozbit zamek **
C
 5000 IF (IWRD(7,1).NE.2.OR.IWRD(5,2).NE.3.OR.IWRD(6,2).NE.54) then
c     1 CALL SGLOD(NAME,LU,320)
           p2=320
           return
       endif
C
C *** je svetlo ***
C
      CALL MMRL
C
C *** je uz rozbit ***
C
      IF (IFD(IVEN(35)).EQ.1) then
c      CALL SGLOD(NAME,LU,8020)
          p2=8020
          return
      endif
C
C *** je vse v poradku ***
C
      IF (IFSD(IVEN(35)).NE.IR.OR.IVEN(35).GT.10000) then
c     1 CALL SGLOD(NAME,LU,22010)  ! truhla zde
          p2=22010
          return
      endif
C
C     sekera, mec, kyj, lopatka, sekerka
C
      IF (IVEN(11).LT.20000.AND.IVEN(12).LT.20000.AND.IVEN(14).LT.
     120000.AND.IVEN(32).LT.20000.AND.IVEN(34).LT.20000) then
c     2 CALL SGLOD(NAME,LU,22020)
          p2=22020
          return
      endif
c      CALL SGLOD(NAME,LU,16035)
      p2=16035
      return
C
C ******* PLACE ROUTINE *******
C
C
C     *** polozit neco predl kam
C
 6000 IF (IWRD(7,1).NE.4.OR.IWRD(5,2).NE.3.OR.IWRD(5,3).NE.5.OR.
     1IWRD(5,4).NE.3) then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** amulet ***
C
C     polozit amulet do otvoru
C
      IF (IWRD(6,2).NE.17.OR.IWRD(6,3).NE.15.OR.IWRD(6,4).NE.66)
     1 GOTO 6100
C ** nema ho **
      IF (IVEN(17).LT.20000) then
c      CALL SGLOD(NAME,LU,3010)
          p2=3010
          return
      endif
C ** neni tam **
      IF (IR.NE.41) then
c      CALL SGLOD(NAME,LU,6010)
          p2=6010
          return
      endif
      IVEN(17)=1141
c              ! zmena stavu amuletu
c      CALL SGLOD(NAME,LU,6020)
      p2=6020
      return
C
C *** klic ***
C
C
C     *** klicek na hak ***
C
 6100 IF (IWRD(6,2).NE.46.OR.IWRD(6,3).NE.2.OR.IWRD(6,4).NE.85)
     1  GOTO 6200
C ** nema ho **
      IF (IVEN(46).LT.20000) then
c      CALL SGLOD(NAME,LU,3010)
          p2=3010
          return
      endif
C ** neni tam **
      IF (IR.NE.1) then
c      CALL SGLOD(NAME,LU,6010)
          p2=6010
          return
      endif
      IVEN(46)=1101
c      CALL SGLOD(NAME,LU,1040)
      p2=1040
      return
C
C *** krizek ***
C
C     *** krizek na zed ***
C
 6200 IF (IWRD(6,2).NE.5.OR.IWRD(6,3).NE.2.OR.IWRD(6,4).NE.77) GOTO 6300
      IF (IVEN(5).LT.20000) then
c      CALL SGLOD(NAME,LU,3010)
          p2=3010
          return
      endif
      IF (IR.NE.16) then
c      CALL SGLOD(NAME,LU,6010)
          p2=6010
          return
      endif
      IVEN(5)=1316
c      CALL SGLOD(NAME,LU,1040)
      p2=1040
      return
C
C *** lucerna ***
C
C
C     *** lucerna na hak ***
C
 6300 IF (IWRD(6,2).NE.4.OR.IWRD(6,3).NE.2.OR.IWRD(6,4).NE.85) GOTO 6900
      IF (IVEN(4).LT.20000) then
c      CALL SGLOD(NAME,LU,3010)
          p2=3010
          return
      endif
      IF (IR.NE.93) then
c      CALL SGLOD(NAME,LU,6010)
          p2=6010
          return
      endif
      IF (IFD(IVEN(4)).EQ.1)IVEN(4)=1493
      IF (IFD(IVEN(4)).NE.1)IVEN(4)=493
c      CALL SGLOD(NAME,LU,1040)
      p2=1040
      return
c 6900 CALL SGLOD(NAME,LU,320)
 6900 p2=320
      return
C
C                            MM-J-6  (LINE 9040)
C 7-J
C
C ****** SLEEP ******
C
 7000 IF (IWRD(7,1).EQ.1) GOTO 7500
C
C **** s nekym ****
C
C
C     *** spat s obyvatelem ***
C
      IF (IWRD(7,1).NE.3.OR.IWRD(5,2).NE.5.OR.(IWRD(6,2).NE.13.and.
     1IWRD(6,2).NE.12).OR.IWRD(5,3).NE.4) then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** je zde ***
C
      IF (IFSD(IRES(IWRD(6,3))).NE.IR) then
c      CALLSGLOD(NAME,LU,8031)
          p2=8031
          return
      endif
C
C *** je perverzni? ***
C
C     mrtvola, vlk, krtek
C
      IF (IWRD(6,3).EQ.1.OR.IWRD(6,3).EQ.15.OR.IWRD(6,3).EQ.4) then
c     1 CALL SGLOD(NAME,LU,7010)
         p2=7010
         return
      endif
C
C *** je zde pohodli ***
C
C     pouze pokoj sluhy, panska loznice, velka loznice, pokoj sluzky
C
      IF (IR.NE.1.AND.IR.NE.20.AND.IR.NE.24.AND.IR.NE.25) then
c     1 CALL SGLOD(NAME,LU,7020)
          p2=7020
          return
      endif
C
C *** je vse pripraveno ***
C
C     osprchovany
C
      IF (ITST(32).NE.1) then
c      CALL SGLOD(NAME,LU,7021)
          p2=7021
          return
      endif
C
C     zavrene dvere
C
      IF ((IR .EQ.1 .AND. IXT(61) .LT. 20000) .OR.
     1 (IR .EQ. 20 .AND. IXT(12) .LT. 20000) .OR.
     2 (IR .EQ. 25 .AND. IXT(73) .LT. 20000) .OR.
     3 IR .EQ. 24) then
c     CALL SGLOD(NAME,LU,7022)
          p2=7022
          return
      endif
C
C     neni svetlo
C
      IF ((IR.EQ.20 .AND. IROM(20).GT.20000) .OR.
     1 (IR.EQ.25 .AND. IROM(25).GT.20000)) then
c     2  CALL SGLOD(NAME,LU,7024)
           p2=7024
           return
       endif
C
C     nic u sebe
C
      DO 7035 I=1,53
      IF (IVEN(I).GE.20000) then
c      CALL SGLOD(NAME,LU,7023)
          p2=7023
          return
      endif
 7035 CONTINUE
C
C     nikdo jiny uvnitr
C
      DO 7050 I=1,17
      IF (IFSD(IRES(I)).EQ.IR.AND.IWRD(6,3).NE.I) then
c     1 CALL SGLOD(NAME,LU,7030)
          p2=7030
          return
      endif
 7050 CONTINUE
C
C *** jdem na to ***
C
C     spi se sluhou, sadarem, panem, kucharem
C
      IF(IWRD(6,3).EQ.3.OR.IWRD(6,3).EQ.7.OR.IWRD(6,3).EQ.14
     1 .OR. IWRD(6,3).EQ. 9) ITST(32)=2
C
C     spi s pani, sluzkou
C
      IF (IWRD(6,3).EQ.2.OR.IWRD(6,3).EQ.8) ITST(32)=3
      ITST(5)=ITST(5)+25
c        ! v kazdem pripade mu to zabere dost casu
c      CALL SGLOD(NAME,LU,7040)
      p2=7040
      return
C
C **** spi sam ****
C
C *** ma pohodli ***
C
 7500 IF (IR.NE.1.AND.IR.NE.10.AND.IR.NE.20.AND.IR.NE.24.AND.IR.
     1NE.25) then
c      CALL SGLOD(NAME,LU,7510)
          p2=7510
          return
      endif
C
C *** je nekdo zde ***
C
      DO 7520 I=2,17
      IF (IFSD(IRES(I)).EQ.IR) then
c      CALL SGLOD(NAME,LU,7530)
          p2=7530
          return
      endif
 7520 CONTINUE
C
C *** vyspi se ***
C
      ITST(5)=ITST(5)+25
c      CALL SGLOD(NAME,LU,7540)
      p2=7540
      return
      END
