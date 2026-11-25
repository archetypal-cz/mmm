$storage:2
$large
$nodebug
      subroutine mmsf(p1,p2,p3,p4,p5)
c      ,10-02-1983 MMM    8536E D45 PL&BM <850920.1810>
C     04-02-1988 MS-DOS S.M.
C
C ******* SEGMENT F *******
C
$INCLUDE:'MMCO.for'
      integer*2 p1,p2,p3,p4,p5
      DIMENSION ITIM(5)
C
C ***** Tento segment je volan segmentem MMSB, provadi nizsi slovesa
C       Normalne je ukoncen segmentem MMSG.
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
C
C *** SET UP VARIABLES ***
C
      ipr(1)=p1
      ipr(2)=p2
      ipr(3)=p3
      ipr(4)=p4
      ipr(5)=p5
      NAME(2)='SG'
      LU=IPR(1)
      LX=ITST(21)
      IR=ITST(1)
      p1=lu
      p2=0
      p3=0
      p4=0
      p5=0
C
C ***** INDEX OF VERB ROUTINES *****
C       ROUTINE               PAGE
C       QUIT, STOP AND SCORE  2
C       GET AND TAKE          3
C       DROP                  4
C       GIVE                  6
C       LOOK AND READ         6
C       FIND                  9
C       LIST                  9
C       LIGHT                10
C       HELP                 11
C       OPEN AND CLOSE       12
C       EXTINGUISH           14
C       TURN                 15
C                            MM-F-1  (LINE 4720)
C 2-F
C
C ***** SELECT THE RIGHT VERB ROUTINE *****
C
      GOTO(1000,2000,2000,3000,4000,5000,5000,6000,7000,8000,
     18000,9000,10000,10000,11000,11000,12000)IWRD(6,1)-19
C
C ****** QUIT, STOP AND SCORE ROUTINE ******
C
C *** SCORE 2 POINTS ROUTINE ***
C
 1000 IF (ITST(5)-(ITST(5)/25)*25.EQ.13.AND.IWRD(6,1).EQ.20.AND.
     :    IWRD(5,12).EQ.3.AND.IWRD(6,2).EQ.71.AND.ITST(5).LT.450) THEN
         ITST(2)=ITST(2)+2
c        CALL SGLOD(NAME,LU,1040)
         p2=1040
         return
      ENDIF
      IF (IWRD(6,1).EQ.20.AND.IWRD(5,2).EQ.3.AND.IWRD(6,2).EQ.71)
     1 then
c     CALL SGLOD(NAME,LU,520)
          p2=520
          return
      endif
      IF (IWRD(7,1) .GT. 1) then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
      ISC=ITST(2)
      ISCP=ITST(3)
      ISCL=(ISC/85+1)*85-ISC
      NAME(2)='SB'
      WRITE(LU,1005)ISC,ISCP
 1005 FORMAT(/' Doposud jste z¡skal 'I4' bod…. ',
     1'St le jeçtØ m…§ete z¡skat 'I4' bod…')
      IF (IWRD(6,1).EQ.20) then
c     CALL SGLOD(NAME,LU)
         return
      endif
      WRITE(LU,1010)(ICLS(L,ISC/85+1),L=1,4),ISCL
 1010 FORMAT(' z p…vodn¡ch 999, jestli§e ovçem budete pokraŸovat.',
     1' T¡m jste se um¡-'/' stil jako '4A2' detektiv. Na lepç¡',
     3' um¡stØn¡ by jste potýeboval'/' jeçtØ 'I3' bodu.'/)
      CALL cas(10,ITIM)
      ITR=60*ITIM(4)+ITIM(3)-60*(ITST(19)/100)-IFSD(ITST(19))
      IF (ITR.LT.0)ITR=ITR+1440
      TG=ITST(5)/25.
      IEF=200*ITR/ITST(5)
      IF (IEF.GT.99)IEF=99
      WRITE(LU,1011) ITR,TG
c      !,IEF
 1011 FORMAT(/' Hr l jste 'I4' min. re ln‚ho Ÿasu, co§ odpov¡d  '
     1         F5.1' hod. hry.')
      NAME(2)='SG'
      WRITE(LU,1020)
 1020 FORMAT(/' Chcete opravdu ji§ skonŸit?'//' ?'\)
 1015 READ(LU,1030) IANS
 1021 FORMAT(/' Chcete opravdu ji§ skonŸit?'//' ? ',A2)
 1030 FORMAT(A2)
      IF (IANS .EQ. 'NE') then
c     CALL SGLOD(NAME,LU,1040)
          p2=1040
          return
      endif
      IF (IANS .EQ. 'ne') then
c     CALL SGLOD(NAME,LU,1040)
          p2=1040
          return
      endif
      IF (IANS .EQ. 'AN' .or. ians .eq. 'an') then
         NAME(2)='SD'
c        CALL SGLOD(NAME,LU,1032)
         p2=1032
         return
      endif
      WRITE(LU,1035)
 1035 FORMAT(/' Pros¡m?  OdpovØzte ANO nebo NE.'//' ?'\)
      GOTO 1015
C
C                            MM-F-2  (LINE 4800)
C 3-F
C
C   ****** GET AND TAKE ROUTINE ******
C
C *** kontrola formatu ***
C
 2000 IF (IWRD(7,1).EQ.1) then
c     CALL SGLOD(NAME,LU,1000)
           p2=1000
           return
      endif
      IF (IWRD(7,1).GT.2.AND.(IWRD(5,3).NE.5.OR.IWRD(6,3).NE.16))then
C
C *** vzit neco nekomu
C
         do 2003 i=1,6
 2003    iwrd(i,4)=iwrd(i,3)
         iwrd(5,3)=5
c                   ! vlozime od
         iwrd(6,3)=16
         iwrd(7,1)=iwrd(7,1)+1
      endif
C
C *** vzit nekomu neco
C
      if (iwrd(7,1) .eq. 4 .and. iwrd(5,2) .eq. 4 .and.
     1  iwrd(5,4) .eq. 3) then
         do 2004 i=1,6
         ii=iwrd(i,4)
         iwrd(i,4)=iwrd(i,2)
 2004    iwrd(i,2)=ii
      endif
C
C *** vzit neco od nekoho
C
      if ((IWRD(5,2).NE.3 .OR. IWRD(5,4).NE.4) .and. iwrd(7,1) .gt. 2)
     1    then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
      IF (IWRD(5,2) .NE. 3) then
c     CALL SGLOD(NAME,LU,2011)
          p2=2011
          return
      endif
C
C *** vzit sprchu ***
C
      IF (IWRD(6,2).EQ.99.AND.IWRD(6,1).EQ.22) GOTO 2100
C
C * ma to *
C
 2019 IF (IVEN(IWRD(6,2)) .GT. 20000) then
c     CALL SGLOD(NAME,LU,2020)
          p2=2020
          return
      endif
C
C * jestlize to bere nekomu ma to dotycna osoba? *
C
      IF ((IWRD(7,1) .GT. 2) .AND. (IFSD(IVEN(IWRD(6,2)))
     1.NE. IWRD(6,4)) .AND. (IVEN(IWRD(6,2)) .LT. 10000)) then
c    2 CALL SGLOD(NAME,LU,2025)
          p2=2025
          return
      endif
C
C * je to zde *
C
      IF (IVEN(IWRD(6,2)).EQ.0.AND.IWRD(6,2).GT.53) then
c    1 CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C  *** je to zde ***
C
      IF (((IFSD(IVEN(IWRD(6,2))) .NE. IR) .AND.
     1(IVEN(IWRD(6,2)) .LT. 10000)) .OR. ((IVEN(IWRD(6,2)) .GE. 10000)
     2.AND. (IRES(IVEN(IWRD(6,2))-(IVEN(IWRD(6,2))/100)*100)
     3-(IRES(IVEN(IWRD(6,2))-(IVEN(IWRD(6,2))/100)*100)/100)*100
     4  .NE. IR)).OR.(IVEN(IWRD(6,2)).GT.10000.AND.IWRD(6,1).EQ.22))
     5 then
c    CALL SGLOD(NAME,LU,2030)
         p2=2030
         return
      endif
C
C * da se s tim hybat *
C
      IF (IWRD(6,2).EQ.88) then
c     CALL SGLOD(NAME,LU,2037)
c      !svitek
          p2=2037
          return
      endif
      IF (IWRD(6,2).GT.99.AND.IWRD(6,2).LT.103) then
c      !pevne veci
c     1  CALL SGLOD(NAME,LU,2041)
          p2=2041
          return
      endif
      IF (IWRD(6,2).EQ.103) then
c      CALL SGLOD(NAME,LU,2042) !dyka
          p2=2042
          return
      endif
      IF (IWRD(6,2).EQ.29.AND.ITST(5).GT.50.AND.IVEN(29).EQ.1214)
     1 then
c     CALL SGLOD(NAME,LU,2035) !klin
          p2=2035
          return
      endif
      IF (IWRD(6,2).GT.53) then
c     CALL SGLOD(NAME,LU,2040)
          p2=2040
          return
      endif
C
C * unese to *
C
      IC=ITD(IVEN(IWRD(6,2)))
      DO 2050 J=1,53
      IF (IVEN(J) .LT. 20000) GOTO 2050
      IC=IC+ITD(IVEN(J))
 2050 CONTINUE
      IF (IC .GT. 16.AND.IVEN(48).LT.20000) then
c     1 CALL SGLOD(NAME,LU,2060)
          p2=2060
          return
      endif
      IF (IC .GT. 24) then
c     CALL SGLOD(NAME,LU,2060)
          p2=2060
          return
      endif
C
C * bodovane veci - CROSS, AMULET, WEDGE, KEY, EMERALD, GOBLET,
C                   DIAMONDS, NECKLACE, LANTERN, MAP *
C
      K=IWRD(6,2)
      IF (K.NE.5.AND.K.NE.17.AND.K.NE.29.AND.K.NE.36.AND.K.NE.37.AND.K.
     1NE.39.AND.K.NE.40.AND.K.NE.42.AND.K.NE.46.AND.K.NE.4.AND.K.NE.28)
     2 GOTO 2052
      IF (IFD(IVEN(K)).EQ.1.AND.K.NE.4)IVEN(K)=IVEN(K)-1000
      IF (ITD(IVEN(4)).EQ.4.AND.K.EQ.4)IVEN(4)=IVEN(4)-100
C
C * slovo v knize *
C
 2052 IF (IWRD(6,2) .EQ. 21) ITST(16)=NORM(89)
      IF (IWRD(6,2).EQ.21.AND.IVRB(1,ITST(16)).EQ.'  ') GOTO 2052
C
C * ulozeni do vybavy *
C
      IVEN(IWRD(6,2))=(IVEN(IWRD(6,2))/100)*100-(IVEN(IWRD(6,2))
     1/10000)*10000+20000
C
C *** vzit od nekoho ***
C
      IF (ITST(5).LT.100.OR.ITST(5).GT.400) GOTO 2095
      DO 2090 J=2,14
         IF ((IRES(J) .GT. 10000) .OR. (ITD(IRES(J)) .EQ. 0) .OR.
     1   (IFSD(IRES(J)) .EQ. 0)) GOTO 2090
C
C  *** hledani veci ***
C
         DO 2085 K=1,53
            IF ((IVEN(K).GT.10000).OR.(IFSD(IRES(J)).NE.IFSD(IVEN(K))))
     1         GOTO 2085
            IF (ITST(5) .LT. 250) THEN
               IF (PROB(0.8)) GOTO 2085
            ELSE
               IF (PROB(0.9)) GOTO 2085
            ENDIF
C
C *** unese toho vice ***
C
            IC=ITD(IVEN(K))
            DO 2080 L=1,53
               IF ((IVEN(L) .LT. 10000) .OR. (IVEN(L) .GT. 20000) .OR.
     1           (IFSD(IVEN(L)) .NE. J)) GOTO 2080
            IC=IC+ITD(IVEN(L))
 2080       CONTINUE
            IF (PROB(0.7)) GOTO 2085
            IF (IC .LE. ITD(IRES(J))) IVEN(K)=10000+(IVEN(K)/100)*100+J
            IC1=' '
            IF (J.EQ.1.OR.J.EQ.2.OR.J.EQ.8) IC1='a'
            IF (IC.LE.ITD(IRES(J)).AND.IFSD(IRES(J)).EQ.IR)
     :          WRITE(LU,2081) (IRSN(I,J),I=1,4),IC1
 2081 FORMAT(/' VidØl jste, jak '4A2'  vzal'A1' '\)
            IF (IC.LE.ITD(IRES(J)).AND.IFSD(IRES(J)).EQ.IR)
     :          CALL MMRI(4,K)
 2085    continue
 2090 continue
c2095 CALL SGLOD(NAME,LU,2070)
 2095 p2=2070
      return
C
C **** vzit sprchu ****
C
C
C *** nema nic u sebe ****
C
 2100 IF (IR.NE.90) then
c     CALL SGLOD(NAME,LU,2110)
          p2=2110
          return
      endif
      DO 2130 I=1,52
      IF (IVEN(I).GT.20000) then
c     CALL SGLOD(NAME,LU,2120)
          p2=2120
          return
      endif
 2130 CONTINUE
      IF (IVEN(53).GT.20000) then
c     CALL SGLOD(NAME,LU,2140)
          p2=2140
          return
      endif
C
C *** sprchujem se ***
C
      ITST(32)=1
c     CALL SGLOD(NAME,LU,2150)
      p2=2150
      return
C
C ****** odlozit ******
C
C
C * kontrola formatu *
C
 3000 IF (IWRD(7,1).EQ.1) then
c     CALL SGLOD(NAME,LU,1000)
          p2=1000
          return
      endif
C
C *** odlozit vybavu ****
C
      IF (IWRD(7,1).EQ.2.AND.IWRD(5,2).EQ.3.AND.IWRD(6,2).EQ.132)
     1 GOTO 3200
      IF (IWRD(7,1).GT.2.OR.IWRD(5,2).NE.3) then
c    1  CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C * ma to *
C
      IF (IVEN(IWRD(6,2)) .LT. 20000) then
c     CALL SGLOD(NAME,LU,3010)
          p2=3010
          return
      endif
C
C * je na dalnici *
C
      IF (IR.EQ.35) IVEN(IWRD(6,2))=ITD(IVEN(IWRD(6,2)))+98
      IF (IR.EQ.35) then
c     CALL SGLOD(NAME,LU,3015)
          p2=3015
          return
      endif
C
C * je u studny *
C
      IF ((IR .LT. 75) .OR. (IR .GT. 77)) GOTO 3100
      DO 3020 J=326,328
      IF (IFSD(IXT(J)) .NE. IR) GOTO 3020
      IVEN(IWRD(6,2))=IVEN(IWRD(6,2))-20000+IXT(J)/100
      IF (IXT(J)/100 .EQ. 79) then
c      CALL SGLOD(NAME,LU,3030)
          p2=3030
          return
      endif
      IF (IXT(J)/100 .NE. 79) then
c      CALL SGLOD(NAME,LU,3040)
          p2=3040
          return
      endif
 3020 CONTINUE
C
C *** rozbiti koule ***
C
 3100 IF (IWRD(6,2).EQ.30.AND.IR.NE.25)IVEN(30)=0
      IF (IWRD(6,2).EQ.30.AND.IR.NE.25) then
c      CALL SGLOD(NAME,LU,3045)
          p2=3045
          return
      endif
C
C                            MM-F-4  (LINE 4960)
C
C 5-F
C
C
C * vyjmuti veci z vybavy *
C
      IVEN(IWRD(6,2))=IVEN(IWRD(6,2))-20000+IR
C
C *** obyvatel si odklada ***
C
      DO 3120 J=2,14
C
C *** muze ***
C
         IF ((ITD(IRES(J)) .EQ. 0) .OR. (IRES(J) .GT. 10000)) GOTO 3120
C
C *** ma vubec neco ***
C
         IF (ITST(5) .LT. 250) THEN
            IF (PROB(0.9)) GOTO 3120
         ELSE
            IF (PROB(0.8)) GOTO 3120
         ENDIF
         DO 3110 K=1,53
            IF ((IVEN(K) .LT. 10000) .OR. (IVEN(K) .GT. 20000) .OR.
     :         (IFSD(IVEN(K)) .NE. J)) GOTO 3110
            IF (PROB(0.7)) GOTO 3110
            IVEN(K)=IVEN(K)-10000-J+IFSD(IRES(J))
            IC1=' '
            IF (J.EQ.1.OR.J.EQ.2.OR.J.EQ.8) IC1='a'
            IF (IFSD(IRES(J)).EQ.IR)WRITE(LU,3115)(IRSN(L,J),L=1,4),IC1
 3115 FORMAT(/' VidØl jste, jak '4A2' odlo§il'A1' '\)
            IF (IFSD(IRES(J)).EQ.IR) CALL MMRI(4,K)
 3110    continue
 3120 continue
c3130 CALL SGLOD(NAME,LU,3110)
 3130 p2=3110
      return
C
C ***odlozeni vybavy ***
C
C * na dalnici *
C
 3200 IF (IR.EQ.35) then
c     CALL SGLOD(NAME,LU,3210)
          p2=3210
          return
      endif
C
C * provedem to *
C
      IC=0
      IX=0
      DO 3220 I=1,52
      IF (I.EQ.30.AND.IVEN(I).GT.20000)IX=1
      IF (IVEN(I).LT.20000) GOTO 3220
      IVEN(I)=IVEN(I)-20000+IR
      IC=1
 3220 CONTINUE
      IF (IC.EQ.0) then
c      CALL SGLOD(NAME,LU,520)
          p2=520
          return
      endif
      IF (IX.EQ.1.AND.IR.NE.25)IVEN(30)=0
      IF (IX.EQ.1.AND.IR.NE.25) then
c      CALL SGLOD(NAME,LU,3045)
          p2=3045
          return
      endif
      IF (IX.EQ.1)IVEN(30)=1325
c     CALL SGLOD(NAME,LU,1040)
      p2=1040
      return
C
C                            MM-F-5  (LINE 5040)
C
C
C 6-F
C
C
C  ****** GIVE ******
C
C
C * kontrola formatu *
C
 4000 IF (IWRD(7,1).EQ.1) then
c     CALL SGLOD(NAME,LU,1000)
          p2=1000
          return
      endif
C
C   dat neco nekomu
C
      IF (IWRD(7,1).NE.3.OR.IWRD(5,2).NE.3.OR.IWRD(5,3).NE.4) then
c    1 CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C * ma to *
C
      IF (IVEN(IWRD(6,2)).LT.20000) then
c      CALL SGLOD(NAME,LU,3010)
          p2=3010
          return
      endif
C
C * je zde obyvatel
C
      IF (IFSD(IRES(IWRD(6,3))).NE.IR) then
c      CALL SGLOD(NAME,LU,4010)
          p2=4010
          return
      endif
C
C * unese to
C
      IC=ITD(IVEN(IWRD(6,2)))
      DO 4020 J=1,53
      IF ((IVEN(J)/10000 .NE. 1) .OR. (IWRD(6,3) .NE. IFSD(IVEN(J))))
     1  GOTO 4020
      IC=IC+ITD(IVEN(J))
 4020 CONTINUE
      IF (IC .GT. ITFD(IRES(IWRD(6,3)))) then
c      CALL SGLOD(NAME,LU,4030)
          p2=4030
          return
      endif
C
C * prenos veci *
C
      IVEN(IWRD(6,2))=IVEN(IWRD(6,2))-10000+IWRD(6,3)
c     CALL SGLOD(NAME,LU,3110,1)
      p2=3110
      p3=1
      return
C
C  ****** LOOK READ ******
C
 5000 IF (IWRD(7,1).EQ.1) then
c     CALL SGLOD(NAME,LU,1000)
          p2=1000
          return
      endif
C
C                   LOOK               PREDL                 DO
      IF (IWRD(6,1).EQ.25.AND.IWRD(5,2).EQ.5.AND.IWRD(6,2).EQ.15.AND.
     1IWRD(6,3).EQ.64)IWRD(6,2)=2
C
C
C            READ                         VEC
      IF (IWRD(6,1).EQ.26.AND.(IWRD(5,2).NE.3.OR.(IWRD(6,2).NE.3.AND.
     1IWRD(6,2).NE.6.AND.IWRD(6,2).NE.18.AND.IWRD(6,2).NE.21.AND.
     2IWRD(6,2).NE.22.AND.IWRD(6,2).NE.28.AND.IWRD(6,2).NE.29.AND.
     3IWRD(6,2).NE.90.AND.IWRD(6,2).NE.88))) then
c    4  CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C                      READ
      IF (IWRD(6,1) .EQ. 26) IWRD(5,3)=IWRD(5,2)
      IF (IWRD(6,1) .EQ. 26) IWRD(6,3)=IWRD(6,2)
      IF (IWRD(6,1) .EQ. 26) IWRD(5,2)=5
c                !PREDL
      IF (IWRD(6,1) .EQ. 26) IWRD(6,2)=2
c                ! NA
C
C                       PREDL                    NA
      IF ((IWRD(5,2) .NE. 5) .OR. (IWRD(6,2) .NE. 2)) then
c     1 CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C                        OBYVATEL                  VEC
      IF ((IWRD(5,3) .NE. 4) .AND. (IWRD(5,3) .NE. 3)) then
c    1 CALL SGLOD(NAME,LU,5010)
          p2=5010
          return
      endif
C
C 6  * divani na obyvatele *
C
      IF (IWRD(5,3).EQ.3) GOTO 5100
C
C * je zde *
C
 5019 IF (IFSD(IRES(IWRD(6,3))) .NE. IR) then
c     1 CALL SGLOD(NAME,LU,5020)
          p2=5020
          return
      endif
C
C * na koho se diva *
C
      IC=0
      WRITE(LU,5030) (IWRD(L,3),L=1,4)
 5030 FORMAT(/' D¡v te se na '4A2', jen§ m  u sebe:')
      CALL MMRI(3,IWRD(6,3))
c     CALL SGLOD(NAME,LU,5070)
      p2=5070
      return
C
C                            MM-F-6  (LINE 5120)
C
C
C 7-F
C
C
C * divat se na vec *
C
 5100 IF (IVEN(IWRD(6,3)).LT.20000.AND.IWRD(6,3).LT.54) then
c     1 CALL SGLOD(NAME,LU,5110)
          p2=5110
          return
      endif
      IF (IXT(320).EQ.0.AND.(IR.EQ.51.OR.IR.EQ.53))IVEN(79)=IR
C
C *** specialni pripady ***
C
C
C * brana *
C
      IF (IWRD(6,3).EQ.93.AND.(IR.EQ.93.OR.IR.EQ.92)) then
c     > CALL SGLOD(NAME,LU,5115)
          p2=5115
          return
      endif
      IF (IWRD(6,3).EQ.93.AND.(IR.EQ.95.OR.IR.EQ.62)) then
c     > CALL SGLOD(NAME,LU,5120)
          p2=5120
          return
      endif
      IF (IWRD(6,3).EQ.93.AND.(IR.EQ.54.OR.IR.EQ.55)) then
c     > CALL SGLOD(NAME,LU,5125)
          p2=5125
          return
      endif
      IF (IFSD(IVEN(IWRD(6,3))).NE.IR.AND.IWRD(6,3).GT.53) then
c     1 CALL SGLOD(NAME,LU,8030)
          p2=8030
          return
      endif
C
C * koule *
C
      IF (IWRD(6,3) .EQ. 30) then
c      CALL SGLOD(NAME,LU,5130)
          p2=5130
          return
      endif
C
C * lano *
C
      IF (IWRD(6,3).EQ.15) then
c      CALL SGLOD(NAME,LU,5140)
          p2=5140
          return
       endif
C
C * mapa *
C
      IF (IWRD(6,3) .NE. 28) GOTO 5200
      IC=0
      ID=0
      DO 5160 J=1,360
      IF (IFSD(IXT(J)) .NE. IR) GOTO 5160
      IF (ITFD(IXT(J)).LT.92.AND.ITFD(IXT(J)).NE.35.AND.
     1ITFD(IXT(J)).NE.0) GOTO 5160
      IC=(J-1)/60+1
      IF (ID .EQ. 0) THEN
         WRITE(LU,5150)
      ENDIF
 5150 FORMAT(/' Z mapy jste zjistil, §e:')
C
      ID=1
      IC1=MMPRP(ITFD(IXT(J)))
      IF (ITFD(IXT(J)).NE.0) WRITE(LU,5151)(IDTN(L,IC),L=1,4),
     :IPRP(1,IC1),(IRNM(L,IFSD(IROM(ITFD(IXT(J))))),L=1,8)
 5151 FORMAT(' ',4A2' .. se octnete 'A2' '8A2)
C
      IF (ITFD(IXT(J)).EQ.0) WRITE(LU,5152)(IDTN(L,IC),L=1,4)
 5152 FORMAT(' ',4A2' .. spadnete z 500 s h… vysok‚ho skaln¡ho £tesu')
 5160 CONTINUE
C
      IF (ID .EQ. 0) then
c      CALL SGLOD(NAME,LU,5170)
          p2=5170
          return
      endif
c     CALL SGLOD(NAME,LU)
      return
C
C * svicka *
C
 5200 IF (IWRD(6,3) .NE. 19) GOTO 5300
      IWRD(6,3)=9-ITST(6)/3600
c     CALL SGLOD(NAME,LU,5210)
      p2=5210
      return
C
C * pochoden *
C
 5300 IF (IWRD(6,3).EQ.27) then
c      CALL SGLOD(NAME,LU,5310)
          p2=5310
          return
      endif
C
C * pistole *
C
      IF (IWRD(6,3) .EQ. 9) then
c      CALL SGLOD(NAME,LU,5410)
          p2=5410
          return
      endif
C
C 7  * hodinky *
C
 5500 IF (IWRD(6,3) .NE. 3) GOTO 5600
      IHR=6+ITST(5)/25
      IF (IHR .GT. 12) IHR=IHR-12
      IF (IHR .GT. 12) IHR=IHR-12
      MIN=INT((ITST(5)-(ITST(5)/25)*25)*2.4)
c     CALL SGLOD(NAME,LU,5510,IHR,MIN)
      p2=5510
      p3=ihr
      p4=min
      return
C
C                            MM-F-7  (LINE 5200)
C
C
C 8-F
C
C
C * kniha a kompas *
C
 5600 IF (IWRD(6,3) .EQ. 21) then
c     CALL SGLOD(NAME,LU,5610)
          p2=5610
          return
      endif
      IF (IWRD(6,3) .EQ. 6) then
c     1 CALL SGLOD(NAME,LU,5710,ITST(4)/1000)
          p2=5710
          p3=itst(4)/1000
          return
      endif
C
C * jed *
C
      IF ((IWRD(6,3) .EQ. 13) .AND. (ITST(10) .EQ. 13)) then
c     1 CALL SGLOD(NAME,LU,5720)
          p2=5720
          return
      endif
      IF (IWRD(6,3) .EQ. 13) then
c     CALL SGLOD(NAME,LU,5730)
          p2=5730
          return
      endif
C
C * krvava zbran *
C
      IF ((IWRD(6,3) .GT. 9) .AND. (IWRD(6,3) .LT. 13) .AND. (IWRD(6,3)
     1.EQ. ITST(10))) then
c     CALL SGLOD(NAME,LU,5740)
          p2=5740
          return
      endif
C
C * listek *
C
      IF (IWRD(6,3).EQ.22.AND.IVEN(22).LT.21000) then
c     1  CALL SGLOD(NAME,LU,5750)
          p2=5750
          return
      endif
      IF (IWRD(6,3).EQ.22.AND.IVEN(22).GT.21000) then
c     1  CALL SGLOD(NAME,LU,5775)
          p2=5775
          return
      endif
C
C * kulka, talisman a truhla *
C
      IF (IWRD(6,3) .EQ. 20) then
c     CALL SGLOD(NAME,LU,5780)
          p2=5780
          return
      endif
      IF (IWRD(6,3) .EQ. 18) then
c      CALL SGLOD(NAME,LU,5781)
          p2=5781
          return
      endif
      IF (IWRD(6,3).EQ.35) then
c     CALL SGLOD(NAME,LU,5782)
          p2=5782
          return
      endif
C
C * zrcadlo *
C
      IF (IWRD(6,3).NE.64) GOTO 5820
      IC=0
      DO 5800 I=2,14
      IF (I.EQ.5) GOTO 5800
      IF (IFSD(IRES(I)).EQ.IR)IC=I
 5800 CONTINUE
c     CALL SGLOD(NAME,LU,5810,IC)
      p2=5810
      p3=ic
      return
C
C * amulet, drevo a klin *
C
 5820 IF (IWRD(6,3).EQ.17) then
c     CALL SGLOD(NAME,LU,5830)
          p2=5830
          return
      endif
      IF (IWRD(6,3).EQ.67) then
c      CALL SGLOD(NAME,LU,5860)
          p2=5860
          return
      endif
      IF (IWRD(6,3).EQ.29) then
c      CALL SGLOD(NAME,LU,5870)
          p2=5870
          return
      endif
C
C * plaketa *
C
      IF (IWRD(6,3).EQ.90) then
c      CALL SGLOD(NAME,LU,5880)
          p2=5880
          return
      endif
C
C * svitek *
C
      IF (IWRD(6,3).EQ.88.AND.IVEN(88).EQ.1017) then
c     1   CALL SGLOD(NAME,LU,2037)
          p2=2037
          return
      endif
      IF (IWRD(6,3).EQ.88) then
c      CALL SGLOD(NAME,LU,5885)
          p2=5885
          return
      endif
C
C * kresba nebo otvor *
C
      IF (IWRD(6,3).EQ.65.OR.IWRD(6,3).EQ.66) then
c     1 CALL SGLOD(NAME,LU,5886)
          p2=5886
          return
      endif
C
C * troll *
C
      IF (IXT(320) .EQ. 0 .AND. IWRD(6,3) .EQ. 79) THEN
         IF (IR.EQ.51) then
c         CALL SGLOD(NAME,LU,5888)
             p2=5888
             return
         endif
         IF (IR.EQ.53) then
c         CALL SGLOD(NAME,LU,5890)
             p2=5890
             return
         endif
      ENDIF
C
C * papousek *
C
      IF (IWRD(6,3).EQ.49) then
c      CALL SGLOD(NAME,LU,5891)
          p2=5891
          return
      endif
C
C * rukavice *
C
      IF (IWRD(6,3).EQ.48) then
c      CALL SGLOD(NAME,LU,5892)
          p2=5892
          return
      endif
C
C * stoly *
C
      IF (IWRD(6,3).EQ.97.OR.IWRD(6,3).EQ.98) then
c     1  CALL SGLOD(NAME,LU,5893)
          p2=5893
          return
      endif
C
C * neni na co se divat *
C
c5900 CALL SGLOD(NAME,LU,5910)
 5900 p2=5910
      return
C
C                            MM-F-8  (LINE 5280)
C
C 9-F
C
C ****** FIND ROUTINE ******
C
 6000 IF (IWRD(5,2).EQ.3) GOTO 6100
      IF (IWRD(7,1).EQ.1) then
c      CALL SGLOD(NAME,LU,1000)
          p2=1000
          return
      endif
C
C *** obyvatele ***
C
      IF (IWRD(5,2).NE.4.OR.IWRD(7,1).NE.2) then
c     1    CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** je pobliz ***
C
      IF (IFSD(IRES(IWRD(6,2))).EQ.IR) then
c      CALL SGLOD(NAME,LU,6005)
          p2=6005
          return
      endif
C
C *** ma hrac globus ***
C
      IF (IVEN(30) .LT. 20000) then
c      CALL SGLOD(NAME,LU,6010)
          p2=6010
          return
      endif
C
C *** nalezeni ***
C
      IC=IFSD(IRES(IWRD(6,2)))
      IF (IC .EQ. 0) then
c      CALL SGLOD(NAME,LU,520)
          p2=520
          return
      endif
      IC=IFSD(IROM(IC))
c     CALL SGLOD(NAME,LU,6020,IC)
      p2=6020
      p3=ic
      return
C
C **** veci ****
C
 6100 IF (IWRD(7,1).NE.2) then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** ma ji hrac ***
C
      IF (IVEN(IWRD(6,2)).GT.20000) then
c      CALL SGLOD(NAME,LU,2020)
          p2=2020
          return
      endif
C
C *** je to pobliz ***
C
      IF (IVEN(IWRD(6,2)).GT.10000.AND.IFSD(IVEN(IWRD(6,2))).EQ.IR)
     1 then
c     CALL SGLOD(NAME,LU,6005)
          p2=6005
          return
      endif
C
C *** ma hrac papouska ***
C
      IF (IVEN(49).LT.20000) then
c      CALL SGLOD(NAME,LU,6030)
          p2=6030
          return
      endif
      IF (IFSD(IVEN(IWRD(6,2))).EQ.0) then
c      CALL SGLOD(NAME,LU,6035)
          p2=6035
          return
      endif
c     CALL SGLOD(NAME,LU,6040)
      p2=6040
      return
C
C ****** LIST ROUTINE ******
C
 7000 IF (IWRD(7,1).NE.1.AND.(IWRD(7,1).NE.2.OR.IWRD(5,2).NE.3.OR.
     1IWRD(6,2).NE.132)) then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** vypis vybavy ***
C
      WRITE(LU,7010)
 7010 FORMAT(/' Vaçe vìbava obsahuje:')
      CALL MMRI(2,0)
c     CALL SGLOD(NAME,LU)
      return
C
C                            MM-F-9  (LINE 5360)
C
C
C 10-F
C
C
C ****** LIGHT ROUTINE ******
C
C
C  zde se zpracovava zapnout a rozsvitit
C
 8000 continue
C
C   obratit se zpracuje jinde
C
      IF (IWRD(6,1) .EQ.30) goto 11000
      IF (IWRD(7,1) .ne. 2) then
c      CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
      IF (iwrd(5,2) .ne. 3) then
c      call SGLOD(name,lu,8010)
          p2=8010
          return
      endif
C
C **** ma to hrac ****
C
      IF ((IWRD(6,2) .LT. 54) .AND. (IVEN(IWRD(6,2)) .LT. 20000))
     1 then
c     CALL SGLOD(NAME,LU,3010)
          p2=3010
          return
      endif
C
C **** je to zde ****
C
      IF ((IWRD(6,2) .GT. 47) .AND. (IVEN(IWRD(6,2))-(IVEN(IWRD(6,2))/
     1100)*100 .NE. IR)) then
c     CALL SGLOD(NAME,LU,8031)
          p2=8031
          return
      endif
C
C **** muze byt rozsviceno nebo zapnuto ****
C
C     radio, lucerna, svicka, pochoden, zapalky, lampa, ohen
C
C
      IF ((IWRD(6,2) .NE. 4) .AND. (IWRD(6,2) .NE. 19) .AND.
     1(IWRD(6,2) .NE. 26) .AND. (IWRD(6,2) .NE. 27) .AND.
     2(IWRD(6,2) .NE. 31) .AND. (IWRD(6,2) .NE. 75) .AND.
     3(IWRD(6,2) .NE. 76) .AND. (IWRD(6,2) .NE. 89)) then
c     4 CALL SGLOD(NAME,LU,8010)
          p2=8010
          return
      endif
C
C **** uz je nebo sviti ****
C
      IF (IVEN(IWRD(6,2))/1000-(IVEN(IWRD(6,2))/10000)*10 .EQ. 1)
     1 then
c     CALL SGLOD(NAME,LU,8020)
          p2=8020
          return
      endif
      IF (IWRD(6,2).EQ.27) then
c      CALL SGLOD(NAME,LU,8060)
          p2=8060
          return
      endif
C
C *** je treba plamen ***
C
      IF (IWRD(6,2).NE.19.AND.IWRD(6,2).NE.76.AND.IWRD(6,2).NE.89)
     1 GOTO 8010
C
C ** ma ho **
C
      IF (IVEN(31).LT.20000.AND.IVEN(19).LT.21000.AND.(IR.NE.10.OR.
     1IVEN(76).LT.1000).AND.(IR.NE.14.OR.IVEN(89).LT.1000)) then
c     2 CALL SGLOD(NAME,LU,8050)
          p2=8050
          return
      endif
C
C ** bylo pouzito zapalky **
C
      IF (((IWRD(6,2).EQ.76.OR.IWRD(6,2).EQ.89).AND.IVEN(19).LT.21000)
     1.OR.(IWRD(6,2).EQ.19.AND.(IR.NE.10.OR.IVEN(76).LT.1000).AND.
     2(IR.NE.14.OR.IVEN(89).LT.1000))) IVEN(31)=21100
C
C **** dosla baterie ****
C
 8010 IF ((IWRD(6,2) .EQ. 4) .AND. (ITST(6)-(ITST(6)/1000)*1000.GT.
     :  119) .AND. (IVEN(8) .LT. 20000)) then
c     CALL SGLOD(NAME,LU,8035)
          p2=8035
          return
      endif
C
      IF ((IWRD(6,2) .EQ. 4) .AND. (ITST(6)-(ITST(6)/1000)*1000 .GT.
     1119)) ITST(6)=ITST(6)-2
C
C **** zapnuti nebo rozsviceni ***
C
      IVEN(IWRD(6,2))=IVEN(IWRD(6,2))+1000
C
C **** radio ****
C
      IF (IWRD(6,2).EQ.75.AND.ITST(8).NE.0) then
c     1   CALL SGLOD(NAME,LU,8040)
          p2=8040
          return
      endif
      IF (IWRD(6,2).EQ.75.AND.ITST(8).EQ.0) then
c     1   CALL SGLOD(NAME,LU,8045)
          p2=8045
          return
      endif
      IF (ITFD(IROM(IR)).EQ.50) then
c      CALL SGLOD(NAME,LU,1040)
          p2=1040
          return
      endif
      IWRD(7,1)=1
      IWRD(6,1)=25
      NAME(2)='SC'
c     CALL SGLOD(NAME,LU)
      return
C
C                            MM-F-10  (LINE 5440)
C
C
C 11-F
C
C
C ****** HELP ROUTINE ******
C
 9000 IF (IWRD(7,1).GT.1) then
c CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
      IC=NORM(4)+9039
C
C *** u hlavni brany
C
      IF (IR.EQ.93.AND.ITST(14).EQ.93)IC=9044
C
C *** v lese ***
C
      IF (IR.EQ.98.OR.IR.EQ.97)IC=9045
C
C *** v rybniku ***
C
      IF (IR.EQ.83)IC=9046
C
C *** na wc ***
C
      IF (IR.EQ.19)IC=9047
C
C *** v zkratce ***
C
      IF (IR.EQ.40.AND.IXT(141).EQ.0)IC=9048
C
C *** v mokre studni ***
C
      IF (IR.EQ.78.OR.IR.EQ.80)IC=9049
C
C *** na pozarnim rebriku ***
C
      IF (IR.EQ.86)IC=9050
C
C *** v bludisti ***
C
      IF ((IR.LT.35.AND.IR.GT.27).OR.(IR.LT.52.AND.IR.GT.41))IC=9051
C
C *** ve vezeni ***
C
      IF (IR.EQ.8.AND.IXT(84).EQ.0)IC=9048
      IF (IC.GT.9043) THEN
         IRAD=10
      ELSE
         IRAD=20
      ENDIF
      WRITE(LU,9010) IRAD
 9010 FORMAT(/' Rada stoj¡ jedna 'I2' bod…. Chcete ji?'//' ?'\)
 9020 READ(LU,9025)IANS
 9025 FORMAT(A2)
      IF (IANS.EQ.'NE') then
c      CALL SGLOD(NAME,LU,1040)
          p2=1040
          return
      endif
      IF (IANS.EQ.'ne') then
c      CALL SGLOD(NAME,LU,1040)
          p2=1040
          return
      endif
      IF (IANS.NE.'AN' .and. ians .ne. 'an') WRITE(LU,1035)
      IF (IANS.NE.'AN' .and. ians .ne. 'an') GOTO 9020
      IF (ITST(2).LT.IRAD) then
c      CALL SGLOD(NAME,LU,9027)
          p2=9027
          return
      endif
      ITST(2)=ITST(2)-IRAD
      ITST(3)=ITST(3)-IRAD
c     CALL SGLOD(NAME,LU,IC)
      p2=ic
      return
C
C                            MM-F-11  (LINE 5520)
C
C
C 12-F
C
C
C ****** OPEN AND CLOSE ROUTINE ******
C
10000 IF (IWRD(7,1).EQ.1) then
c     CALL SGLOD(NAME,LU,1000)
          p2=1000
          return
      endif
      IF (IWRD(7,1).NE.2.OR.IWRD(5,2).NE.3) then
c     1  CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** tajne pruchody ***
C
C
C *** zavrit dvere na wc ***
C
      IF ((IR .EQ. 19) .AND. (IWRD(6,1) .EQ.33) .AND.(IWRD(6,2)
     1.EQ. 92)) then
        IXT(11)=2219
c       call SGLOD(name,lu,1095,ir)
        p2=1095
        p3=ir
        return
      endif
      IF (IR.EQ.5.AND.IWRD(6,1).EQ.32.AND.IWRD(6,2).EQ.70) then
        IXT(183)=405
c       call SGLOD(NAME,LU,1095,IR)
        p2=1095
        p3=ir
        return
      endif
      IF (IR.EQ.7.AND.IWRD(6,1).EQ.32.AND.IWRD(6,2).EQ.69) then
        IXT(123)=407
c       CALL SGLOD(NAME,LU,1095,IR)
        p2=1095
        p3=ir
        return
      endif
C
C **** svitek ****
C
      IF (IWRD(6,2).EQ.88.AND.IR.EQ.17) then
c      CALL SGLOD(NAME,LU,2037)
          p2=2037
          return
      endif
C
C **** dvere ****
C
      IF (IWRD(6,2).NE.92) GOTO 10050
      IF ((IR.GT.27.AND.IR.NE.91.AND.IR.NE.54.AND.IR.NE.36.AND.IR.NE.
     190).OR.IR.EQ.3.OR.IR.EQ.4.OR.IR.EQ.13.OR.IR.EQ.16.OR.IR.EQ.21
     2.OR.IR.EQ.22) then
c     CALL SGLOD(NAME,LU,2030)
           p2=2030
           return
      endif
      DO 10030 J=1,240
      IF (IFSD(IXT(J)).NE.IR) GOTO 10030
      IF ((J-1)/60+1.NE.ITST(4)/1000) GOTO 10030
      IC=ITFD(IXT(J))
      IF (IC.EQ.4.OR.IC.EQ.13.OR.IC.EQ.22.OR.J.EQ.62.OR.J.EQ.70.OR.J
     1.EQ.133.OR.J.EQ.182.OR.J.EQ.190) CALL SGLOD(NAME,LU,15030)
      IF (IXT(J).GT.20000.AND.IWRD(6,1).EQ.32) then
c     1   CALL SGLOD(NAME,LU,10035)
          p2=10035
          return
      endif
      IF (IWRD(6,1).EQ.32) then
c      CALL SGLOD(NAME,LU,10045)
          p2=10045
          return
      endif
c      CALL SGLOD(NAME,LU,10046)
      p2=10046
      return
10030 CONTINUE
c     CALL SGLOD(NAME,LU,15030)
      p2=15030
      return
C
C **** zaclony ****
C
10050 IF (IWRD(6,2) .NE. 91) GOTO 10100
      IF ((IR .GT. 26) .OR. (IR .EQ. 21) .OR. (IROM(IR) .LT. 10000))
     1 then
c     CALL SGLOD(NAME,LU,10010)
          p2=10010
          return
      endif
      IF (((IROM(IR) .GT. 20000) .AND. (IWRD(6,1) .EQ. 32)) .OR.
     1((IROM(IR) .LT. 20000) .AND. (IWRD(6,1) .EQ.33))) then
c     2 CALL SGLOD(NAME,LU,8020)
          p2=8020
          return
      endif
      IF (IWRD(6,1) .EQ. 32) IROM(IR)=IROM(IR)+10000
      IF (IWRD(6,1) .EQ. 33) IROM(IR)=IROM(IR)-10000
C
C *** zabiti vampira ***
C
      IF ((IWRD(6,1) .NE. 32) .OR. (IFSD(IRES(5)) .NE. IR) .OR.
     1(ITST(1) .GT. 300)) GOTO 10090
      DO 10080 J=1,360
      IF (IFSD(IXT(J)) .NE. IR) GOTO 10080
      IF (IXT(J) .GT. 20000) GOTO 10080
      IF (ITFD(IXT(J)).EQ.16.OR.ITFD(IXT(J)).GT.27) GOTO 10080
      IF (IROM(ITFD(IXT(J))) .GT. 20000) GOTO 10080
      IRES(5)=IRES(5)-IR+ITFD(IXT(J))
      GOTO 10090
10080 CONTINUE
      IRES(5)=0
      IVEN(1)=100+IR
      ITST(2)=ITST(2)+75
c     CALL SGLOD(NAME,LU,10085)
      p2=10085
      return
10090 IF (ITFD(IROM(IR)).EQ.50) GOTO 10095
      IWRD(7,1)=1
      IWRD(6,1)=25
      NAME(2)='SC'
c     CALL SGLOD(NAME,LU)
      return
c10095 CALL SGLOD(NAME,LU,1040)
10095 p2=1040
      return
C
C                            MM-F-12  (LINE 5600)
C
C
C 13-F
C
C
C **** brana ****
C
10100 IF (IWRD(6,2) .NE. 93) GOTO 10200
      IF (IR.NE.93.AND.IR.NE.92.AND.IR.NE.95.AND.IR.NE.54.AND.IR.NE.55)
     1 then
c     CALL SGLOD(NAME,LU,2030)
          p2=2030
          return
      endif
C
C *** zadni nebo zahradni ***
C
      IF ((IXT(27).EQ.0).AND.(IR.EQ.54.OR.IR.EQ.55)) then
c     1 CALL SGLOD(NAME,LU,10105)
          p2=10105
          return
      endif
      IF (((IR .EQ. 95) .OR. (IR .EQ. 55)) .AND. (IWRD(6,1) .EQ. 32))
     1 then
c     CALL SGLOD(NAME,LU,8020)
          p2=8020
          return
      endif
      IF (((IR .EQ.95) .OR. (IR .EQ. 55)) .AND. (IWRD(6,1) .EQ.33))
     1 then
c     CALL SGLOD(NAME,LU,10110)
          p2=10110
          return
      endif
C
C *** hlavni brana ***
C
      IF (((IWRD(6,1) .EQ.33) .AND. (IXT(54) .GT. 20000)) .OR.
     1((IWRD(6,1) .EQ. 32) .AND. (IXT(54) .LT. 20000))) then
c     2 CALL SGLOD(NAME,LU,8020)
          p2=8020
          return
      endif
C
C ** je zde nekdo, kdo mu pomuze **
C
      DO 10120 J=1,14
      IF ((IFSD(IRES(J)).NE.IR.OR.IRES(J).GT.10000).AND.
     1IVEN(48).LT.20000) GOTO 10120
      IF (IWRD(6,1) .EQ. 32) IXT(54)=19293
      IF (IWRD(6,1) .EQ. 32) IXT(234)=19392
      IF (IWRD(6,1) .EQ.33) IXT(54)=29293
      IF (IWRD(6,1) .EQ.33) IXT(234)=29392
      IF (J.GT.1) then
c      CALL SGLOD(NAME,LU,10115,J)
          p2=10115
          p3=j
          return
      endif
c     CALL SGLOD(NAME,LU,10116)
      p2=10116
      return
10120 CONTINUE
c     CALL SGLOD(NAME,LU,10130)
      p2=10130
      return
C
C **** ma to ****
C
10200 IF (IWRD(6,2).LT.54.AND.IVEN(IWRD(6,2)).LT.20000) then
c     1 CALL SGLOD(NAME,LU,3010)
          p2=3010
          return
      endif
C
C *** truhla **
C
      IF (IWRD(6,2).NE.35) GOTO 10300
C
C ** je to uz **
C
      IF ((IWRD(6,1).EQ.32.AND.IFD(IVEN(35)).EQ.2).OR.(IWRD(6,1).EQ.33
     1.AND.IFD(IVEN(35)).LT.2)) then
c     CALL SGLOD(NAME,LU,8020)
          p2=8020
          return
      endif
C
C ** zavreti **
C
      IF (IWRD(6,1).EQ.33)IVEN(35)=IVEN(35)-1000
      IF (IWRD(6,1).EQ.33) then
c      CALL SGLOD(NAME,LU,1040)
          p2=1040
          return
      endif
C
C ** otevreti **
C
      IF (IFD(IVEN(35)).EQ.0) then
c      CALL SGLOD(NAME,LU,5783)
          p2=5783
          return
      endif
      IF (ITD(IVEN(35)).EQ.5)IVEN(35)=IVEN(35)+1000
      IF (ITD(IVEN(35)).EQ.5) then
c      CALL SGLOD(NAME,LU,1040)
          p2=1040
          return
      endif
      IVEN(35)=IVEN(35)+1000
      DO 10220 J=1,360
      IF (IFSD(IXT(J)).NE.IR.OR.IXT(J).GT.20000) GOTO 10220
      IVEN(35)=IVEN(35)-100
c     CALL SGLOD(NAME,LU,10210)
      p2=10210
      return
10220 CONTINUE
c     CALL SGLOD(NAME,LU,10230)
      p2=10230
      return
10300 CONTINUE
c10900 CALL SGLOD(NAME,LU,10910)
10900 p2=10910
      return
C
C Cu
C
C
C                            MM-F-13  (LINE 5680)
C
C 14-F
C
C ****** uhasit, foukat, turn off *****
C
11000 IF (IWRD(7,1).EQ.1) then
c     CALL SGLOD(NAME,LU,1000)
          p2=1000
          return
      endif
C
C *** tajne vchody ***
C
C *** v obyvacim pokoji otocit obrazem ***
C
      if (iwrd(6,1) .eq. 30) then
         IF ((iwrd(5,2) .eq. 3) .and. (IR .EQ. 10) .AND.
     1      (IWRD(6,2) .EQ. 72) .AND. (IWRD(7,1) .EQ. 2)) then
            IXT(5)=1310
c           CALL SGLOD(NAME,LU,1095,IR)
            p2=1095
            p3=ir
            return
         endif
C
C *** otocit amuletem ***
C
         IF (IWRD(5,2).EQ.3.AND.IWRD(6,2).EQ.17.AND.IWRD(7,1).EQ.2)
     1   GOTO 12100
C
C *** otocit smer je jinde ***
C
         IF (IWRD(5,2) .eq. 2) goto 12000
      endif
C
C *** svitek ***
C                     foukat               na                     do
      IF (IWRD(6,1).EQ.35.AND.(((IWRD(6,2).EQ.2 .or. iwrd(6,2).eq. 15)
     9  .AND.IWRD(5,2).EQ.5.AND.
     1 IWRD(6,3).EQ.88.AND.IWRD(5,3).EQ.3.AND.IWRD(7,1).EQ.3).OR.
     2(IWRD(7,1).EQ.2.AND.IWRD(6,2).EQ.88.AND.IWRD(5,2).EQ.3))) then
c     3 CALL SGLOD(NAME,LU,2038) !foukat na svitek
         p2=2038
         return
      endif
C
C *** kontrola formatu ***
C
C     uhasit neco
C
      IF ((IWRD(7,1) .ne. 2) .AND. (IWRD(6,1) .eq. 34)) then
c     1 CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C     foukat na neco
C
      IF ((IWRD(5,2) .EQ. 5) .AND.
     1(iwrd(5,2) .NE. 2)) then
c     2 CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
      IF (IWRD(7,1) .GT. 3) then
c      CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** vyjmuti predlozky ***
C
      IF (IWRD(5,2) .EQ. 5) IWRD(6,2)=IWRD(6,3)
      IF (IWRD(5,2) .EQ. 5) IWRD(1,2)=IWRD(1,3)
      IF (IWRD(5,2) .EQ. 5) IWRD(2,2)=IWRD(2,3)
      IF (IWRD(5,2) .EQ. 5) IWRD(3,2)=IWRD(3,3)
      IF (IWRD(5,2) .EQ. 5) IWRD(4,2)=IWRD(4,3)
      IF (IWRD(5,2) .EQ. 5) IWRD(5,2)=IWRD(5,3)
      IC=IWRD(6,2)
C
C *** ma to ***
C
      IF ((IC.LT.36).AND.(IVEN(IC).LT.20000)) then
c     1   CALL SGLOD(NAME,LU,3010)
          p2=3010
          return
      endif
C
C *** je to zde ***
C
      IF ((IC .GT. 35) .AND. (IFSD(IVEN(IC)) .NE. IR)) then
c     1 CALL SGLOD(NAME,LU,8031)
          p2=8031
          return
      endif
C
C *** muze se to zlikvidovat ***
C
      IF ((IC .NE. 4) .AND. (IC .NE. 19) .AND. (IC .NE. 26) .AND.
     1(IC .NE. 27) .AND. (IC .NE. 31) .AND. (IC .NE. 44) .AND.
     2(IC .NE. 45)) then
c     CALL SGLOD(NAME,LU,8010)
          p2=8010
          return
      endif
C
C *** je to vypnuto nebo pryc ***
C
      IF (IVEN(IC)/1000-(IVEN(IC)/10000)*10 .EQ. 0) then
c     1 CALL SGLOD(NAME,LU,8020)
          p2=8020
          return
      endif
C
C *** vypnem to ***
C
      IVEN(IC)=IVEN(IC)-1000
c     CALL SGLOD(NAME,LU,1040)
      p2=1040
      return
C
C                            MM-F-14  (LINE 5760)
C
C
C 15-F
C
C
C ****** TURN ROUTINE ******  zatocit na smer, zatocit smer
C
C *** kontrola formatu ***
C
12000 IC=IWRD(6,2)
      IF ((IWRD(7,1) .NE. 2) .OR. (IWRD(5,2) .NE. 2) .OR. (IC .EQ. 5)
     1.OR. (IC .EQ. 6) .OR. (IC .GT. 10)) then
c     CALL SGLOD(NAME,LU,320)
          p2=320
          return
      endif
C
C *** otoceni absolutni ***
C
      IF ((IC .LT. 5) .AND. (IVEN(6) .LT. 20000) .AND. (IR .LT. 54))
     1 then
c     CALL SGLOD(NAME,LU,1005)
          p2=1005
          return
      endif
      IF (IC .LT. 5) ITST(4)=ITST(4)-(ITST(4)/1000)*1000 +IC*1000
      IF (IC .LT. 5) then
c      CALL SGLOD(NAME,LU,1040)
          p2=1040
          return
      endif
C
C *** relativni ***
C
      ITST(4)=ITST(4)+1000*(IC-9)
      IF (ITST(4).LT.1000)ITST(4)=ITST(4)+4000
      IF (ITST(4).GT.5000)ITST(4)=ITST(4)-4000
c     CALL SGLOD(NAME,LU,1040)
      p2=1040
      return
C
C *** amulet ***
C
12100 IF (IVEN(17).GT.20000.OR.(IFSD(IVEN(17)).EQ.IR.AND.IFD(IVEN(17))
     1.NE.1.AND.IVEN(17).LT.10000)) then
c     CALL SGLOD(NAME,LU,1040)
          p2=1040
          return
      endif
      IF ((IFSD(IVEN(17)).NE.IR.AND.IVEN(17).LT.10000).OR.
     1IVEN(17).GT.10000) then
c     CALL SGLOD(NAME,LU,8031)
          p2=8031
          return
      endif
C
C ** stare vychody znicit **
C
      DO 12110 I=1,360
      IF ((ITFD(IXT(I)).GT.41.AND.ITFD(IXT(I)).LT.54).OR.(IFSD(IXT(I))
     1.GT.41.AND.IFSD(IXT(I)).LT.54))IXT(I)=0
12110 CONTINUE
C
C ** nova jmena pokoju **
C
      IROM(42)=5153
      DO 12120 I=43,50
12120 IROM(I)=5053
      IROM(51)=6555
      IROM(52)=6554
      IROM(53)=6555
C
C ** veci z krtci nory pryc **
C
      DO 12130 I=1,53
      IF (IVEN(I).LT.10000.AND.IFSD(IVEN(I)).GT.41.AND.IFSD(IVEN(I))
     1.LT.54) IVEN(I)=0
12130 CONTINUE
C
C ** nove veci na cestu **
C
      IVEN(40)=1151
      IVEN(42)=1151
      IVEN(37)=1153
      IVEN(39)=1253
      IRES(4)=10000
C
C                            MM-F-15  (LINE 5840)
C
C
C 16-F
C
C
C ** nove spoje **
C
      ITST(32)=ITST(5)
      IXT(22)=4243
      IXT(23)=4244
      IXT(82)=4442
      IXT(83)=4445
      IXT(84)=4544
      IXT(85)=4750
      IXT(86)=4948
      IXT(87)=5049
      IXT(142)=4344
      IXT(143)=4443
      IXT(202)=4342
      IXT(203)=4345
      IXT(204)=4543
      IXT(205)=5047
      IXT(254)=4142
      IXT(255)=4547
      IXT(256)=5148
      IXT(257)=5249
      IXT(258)=5350
      IXT(315)=4241
      IXT(316)=4648
      IXT(317)=4647
      IXT(318)=4745
      IXT(351)=4849
      IXT(352)=4851
      IXT(353)=4950
      IXT(319)=4952
      IXT(354)=5053
c     CALL SGLOD(NAME,LU,12140)
      p2=12140
      return
      NAME(2)='SB'
c     CALL SGLOD(NAME,LU)
      return
      END
