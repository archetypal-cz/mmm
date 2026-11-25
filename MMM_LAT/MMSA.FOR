$STORAGE:2
$large
$nodebug
C
C     UPRAVY:
C        10-10-1982 P.Lampa, B.Michal
C        05-07-1983 P.Lampa v. MMM
C        04-02-1988 S.Matìçek MS-DOS
C
      SUBROUTINE MMSA(P1,P2,P3,P4,P5)
C      ,10-02-1983 MMM    8536E D45 PL&BM <850920.1457>
$INCLUDE:'MMCO.FOR'
C
C ******* SEGMENT A *******
C
C      Tento segment inicializuje hru generovanim zahady, magickych
C      slov, a dalsich. Segment je volan pouze jednou (pouze pri
C      zmene zahady pri prvnim tahu je mozno volat znovu)
C
      INTEGER*2 P1,P2,P3,P4,P5
      DIMENSION ITIM(5),NCHK(75)
      IPR(1)=P1
      IPR(2)=P2
      IPR(3)=P3
      IPR(4)=P4
      IPR(5)=P5
      IERR=0
      LU=IPR(1)
C                             !zadat, lu vstupu
      ITST(21)=IPR(3)
C                       !lu pro opis pokecu
      LX=IPR(3)
      IF (IPR(4).NE.0)ITST(38)=IPR(4)
C       !potlaceni souborovych operaci
      IF (RND.NE.2.) GOTO 1000
C
C ****** INDEX ******                                    PAGE
C
C      A. SET UP MYSTERY                                 A-3
C         1. PICK SCENE
C         2. PICK MURDERER
C         3. PICK WEAPON
C      B. SET UP RANDOM ITEMS                            A-4
C         1. WELLS
C         2. BATTERY
C         3. MOLE
C      C. GENERATE MAGIC WORDS                           A-4
C
C ***** Nastaveni nahodnych promennych ****
C
      CALL cas(10,ITIM)
      RND=(ITIM(2)*ITIM(3)+10)/3600.
C        !seed
      IM=1000*RND
C                           ! cislo zahady
C
C     pri nastaveni cisla zahady bude jednoznacne
C     urcen scenar hry
C
      ITST(23)=IM
C                           !poznamenat cislo hry
      CALL SSEED(IM)
      ITST(30)=ITST(30)+3*ITST(29)
C          !!???
C
C *** EXISTUJE SOUBOR MMMC ? ***
C
c     IF (ITST(38).NE.'MM') GOTO 10
C     CALL OPEN(IDCB,IERR,6HMMTLAN,1,-44)
c     IF (IERR.LT.0) THEN
c         WRITE(LU,2)
c   2     FORMAT(/' Pri posledni navsteve se bohuzel povedlo jednomu'
c    1 ' nejmenovanemu'/' programatoru zcela znicit tajemny zamek'
c    2 ' i s jeho blizkym okolim.'/' Doporucuji Vam obratit se na'
c    3 ' SRPP (sluzbu restaurace programovych'/' her), jejiz pobocka'
c    4 ' zajiste existuje i ve Vasem'/' vypocetnim stredisku.')
c         GOTO 9999
c      ENDIF
C
C                            MM-A-1
C=====C
C 2-A C
C=====C
C
C *** Kontrola hraciho casu a hesla ***
C
C     WRITE(LU,3)
C   3 FORMAT('A'30X/)
      IC=100*ITIM(4)+ITIM(3)
C       !cas spusteni
C
C **** kontrola jmena ****
C
 7777 NTAG(1)='  '
      WRITE(LU,7)
    7 FORMAT(/' Vaçe jm‚no? '\)
      READ(LU,8)NTAG
    8 FORMAT(10A2)
C     CALL CLCUC(NTAG,20)
      IF (NTAG(1) .NE. '  ') GOTO 20
      WRITE(LU,11)
   11 FORMAT(' Dokud mi neýeknete sv‚ jm‚no, nebudu se s V mi bavit.')
      GOTO 7777
C
C * HLEDANI JMENA *
C
c 20  ID=0
c 22  do 21 i=1,75
c 21  nchk(i)='  '
C     CALL READF(IDCB,IERR,NCHK,75,LEN)
c     goto 28
c     IF (IERR.LT.0) GOTO 9
c     IF (LEN.LT.0) GOTO 27
c     ID=ID+1
c     DO 24 I=1,10
c     IF (NCHK(I).NE.NTAG(I)) GOTO 22
c  24 CONTINUE
c     goto 28
C
C   pridani jmena
C
c  27 do 2700 i=1,10
c2700     nchk(i)=ntag(i)
c     do 2701 i=11,75
c2701     nchk(i)='  '
C     call writf(idcb,ierr,nchk,75)
c      if (ierr .lt. 0) goto 9
C
C     vypise se uvitaci proslov
C
c28    WRITE(LU,12) (NCHK(L),L=12,42)
c     IF (NCHK(43).NE.'  ') WRITE(LU,13)(NCHK(L),L=43,74)
c  12 FORMAT(/32A2)
C     ! vypne se mem lock PRO HP
c  13 FORMAT(' ',32A2)
C
C * podle jmena se nastavi jiste promenne *
C
c     ITST(28)=ID
20    call sget(ntag,1,l)
      itst(28)=l-64
C                   ! cislo v seznamu
c     ITST(40)=NCHK(11)
      itst(40)='  '
C             ! heslo
      GOTO 30
C
C ** jmeno neni v seznamu **
C
    9 WRITE(LU,25)
   25 FORMAT(' Vase jmeno neni v seznamu. Lituji.')
      GOTO 9999
C
C ** zadani v poradku **
C
C  30 CALL CLOSE(IDCB)
30    NAME(2)='SB'
      NAME(3)='  '
C
C **** Vstupni informace ****
C
   10 WRITE(LU,100) NTAG,IM,ITST(15)
      write(lu,101)
C
  100 FORMAT(/' Nazdar '10A2/' V¡t m V s v tajemn‚m z mku. ',
     :'Z hada Ÿ.'I4'. Revize 'I3' - 8826 P.L.+B.M.+S.M. '//' ',5x,
     :' F U T U R E   E n g i n e e r i n g ,   s. r. o.'//)
  101 format(
     :'      Vçe je fantazie poŸ¡taŸe, zalo§en  na faktech, povØstech ',
     :'a vìmyslech'/' z minulosti, pý¡tomnosti a o budoucnosti. Sc‚n ý',
     :' byl vymyçlen tak, aby V s'/' st le udr§oval v napØt¡. Uniknete',
     :' na p r chvil z okoln¡ho svØta a za§ijete'/' zklam n¡ i triumf,',
     :' nadØji i zoufalstv¡, s¡lu i slabost, lakotu i chamtivost.'/
     :'      Vyu§¡vejte toho co vid¡te, slyç¡te a c¡t¡te; mnØ pouze ',
     :'ý¡kejte, co'/' chcete dØlat, nebo co si mysl¡te, §e m te dØlat.',
     :' Sna§te se objevit co nejv¡ce'/' skuteŸnost¡ a fakt…, ka§dì V ç',
     :' krok je sledov n a bodov n.'/' ',5x,'Vaçim c¡lem je z¡skat ',
     :'maxim ln¡ poŸet mo§nìch bod….'//
     :'     Jste pýed tØ§kou §eleznou branou,  kter  je zýejmØ jedi-'/
     :' nou cestou vedouc¡ za vysokou cihlovou zeÔ, obklopuj¡c¡ sta-'/
     :' rì z mek, jen§ je vidØt branou. Rozedn¡v  se a nad travnatì-'/
     :' mi plochami v okol¡ z mku stoj¡ n¡zk  ý¡dk  mlha.  Na jih od'/
     :' V s zaŸ¡n  silnice vedouc¡ ji§nØ a§ k obzoru.  V d li vid¡te'/
     :' dro§ku, kter  V s sem pýivezla, a nyn¡ odj¡§d¡ zpØt.')
C
C=====C                      MM-A-2  (LINE 1040)
C 3-A C
C=====C
C
C *** cas posledniho cteni a cas zacatku hry ***
C
      ITST(18)=ITIM(4)*1200+ITIM(3)*20+ITIM(2)/3
      ITST(19)=100*ITIM(4)+ITIM(3)
C
C ***** Vytvoreni zahady *****
C
C *** Vyber pokoje ***
C
 1000 IC=NORM(77)
C
C     vrazdit se muze ve sklepnich prostorach, tajnem shodisti,
C     loznici, horni hale, zvonici a v parku, cili 1..9,19..27,54..77
C
      IF (((IC .GT. 9) .AND. (IC .LT. 19)) .OR. ((IC .GT. 27)
     1.AND. (IC .LT. 54))) GOTO 1000
      ITST(8)=IC
C
C *** Vyber vraha ***
C
C
C     vrahem muze byt
C        v parku - pani, sluzka, kuchar, sadar
C        v mistnostech - sluha, sluzka, pan, pani
C        ve sklepe - sluha, sadar, kuchar, pan
C
      ID=NORM(4)-1
      IF (ID .EQ. 0) IE=7
      IF (ID .EQ. 1) IE=8
      IF (ID .EQ. 2) IE=9
      IF (ID .EQ. 3) IE=2
      IF ((IC .LT. 28) .AND. (ID .EQ. 0)) IE=3
      IF ((IC .LT. 28) .AND. (ID .EQ. 1)) IE=8
      IF ((IC .LT. 28) .AND. (ID .EQ. 2)) IE=14
      IF ((IC .LT. 28) .AND. (ID .EQ. 3)) IE=2
      IF ((IC .LT. 10) .AND. (ID .EQ. 0)) IE=3
      IF ((IC .LT. 10) .AND. (ID .EQ. 1)) IE=7
      IF ((IC .LT. 10) .AND. (ID .EQ. 2)) IE=9
      IF ((IC .LT. 10) .AND. (ID .EQ. 3)) IE=14
      ITST(9)=IE
C
C *** Vyber zbrane a mista jeji ukryti ***
C     vrah = sluha - v sklepe nebo mistnostech
C            sadar - v sklepe nebo parku
C            sluzka - v mistnostech nebo parku
C            kuchar - v sklepe nebo parku
C            pan - v sklepe nebo mistnostech
C            pani - v mistnostech nebo parku
C
      IF=NORM(2)-1
      IF ((IE .EQ. 3) .AND. (IF .EQ. 0)) IG=NORM(9)
      IF ((IE .EQ. 3) .AND. (IF .EQ. 1)) IG=NORM(9)+18
      IF ((IE .EQ. 7) .AND. (IF .EQ. 0)) IG=NORM(9)
      IF ((IE .EQ. 7) .AND. (IF .EQ. 1)) IG=NORM(24)+53
      IF ((IE .EQ. 8) .AND. (IF .EQ. 0)) IG=NORM(9)+18
      IF ((IE .EQ. 8) .AND. (IF .EQ. 1)) IG=NORM(24)+53
      IF ((IE .EQ. 9) .AND. (IF .EQ. 0)) IG=NORM(9)
      IF ((IE .EQ. 9) .AND. (IF .EQ. 1)) IG=NORM(24)+53
      IF ((IE .EQ. 14) .AND. (IF .EQ. 0)) IG=NORM(9)
      IF ((IE .EQ. 14) .AND. (IF .EQ. 1)) IG=NORM(9)+18
      IF ((IE .EQ. 2) .AND. (IF .EQ. 0)) IG=NORM(9)+18
      IF ((IE .EQ. 2) .AND. (IF .EQ. 1)) IG=NORM(24)+53
C
C
C     *** zbrani je pistole, nuz, sekera, mec, jed, kyj ***
C
      ixx=NORM(6)+8
      IVEN(ixx)=(IVEN(ixx)/100)*100+IG
      ITST(10)=ixx
      IF (IXX .EQ. 9) ITST(11)=5
C         ! naboju jen 5
C
C *** Vyber pokoje pro mrtvolu ***
C
C
C     pokud je zbrani jed nebo kyj, zustane na miste cinu
C
      IF (ITST(10) .GT. 12) then
         IRES(1)=10000+IC
      else
         IF=NORM(2)-1
         IF ((IE .EQ. 3) .AND. (IF .EQ. 0)) IG=NORM(9)
         IF ((IE .EQ. 3) .AND. (IF .EQ. 1)) IG=NORM(9)+18
         IF ((IE .EQ. 7) .AND. (IF .EQ. 0)) IG=NORM(9)
         IF ((IE .EQ. 7) .AND. (IF .EQ. 1)) IG=NORM(24)+53
         IF ((IE .EQ. 8) .AND. (IF .EQ. 0)) IG=NORM(9)+18
         IF ((IE .EQ. 8) .AND. (IF .EQ. 1)) IG=NORM(24)+53
         IF ((IE .EQ. 9) .AND. (IF .EQ. 0)) IG=NORM(9)
         IF ((IE .EQ. 9) .AND. (IF .EQ. 1)) IG=NORM(24)+53
         IF ((IE .EQ. 14) .AND. (IF .EQ. 0)) IG=NORM(9)
         IF ((IE .EQ. 14) .AND. (IF .EQ. 1)) IG=NORM(9)+18
         IF ((IE .EQ. 2) .AND. (IF .EQ. 0)) IG=NORM(9)+18
         IF ((IE .EQ. 2) .AND. (IF .EQ. 1)) IG=NORM(24)+53
         IRES(1)=10000+IG
      endif
C
C=====C                      MM-A-3
C 4-A C
C=====C
C
C ***** Vytvoreni studni *****
C
 2000 IC=NORM(3)+77
      IXT(326)=75+100*IC
      IF (IC .EQ. 79) IXT(266)=7579
      IC=IC+1
      IF (IC .EQ. 81) IC=78
      IXT(327)=76+100*IC
      IF (IC .EQ. 79) IXT(266)=7679
      IC=IC+1
      IF (IC .EQ. 81) IC=78
      IXT(328)=77+100*IC
      IF (IC .EQ. 79) IXT(266)=7779
C
C ***** Nova baterie - nahodne v tocivem bludisti *****
C
      IVEN(8)=200+NORM(6)+28
C
C ***** Krtek - nahodne v krtcim bludisti *****
C
      IRES(4)=NORM(9)+42
C
C ***** Magicka slova *****
C
C *** Nastaveni zamenitelnych pismen ***
C
      IWRD(1,1)=' M'
      IWRD(2,1)=' I'
      IWRD(3,1)=' W'
      IWRD(4,1)=' O'
      IWRD(5,1)=' X'
      IWRD(1,2)=' M'
      IWRD(2,2)=' I'
      IWRD(3,2)=' W'
      IWRD(4,2)=' O'
      IWRD(5,2)=' H'
C
C *** Nahodne propojeni pismen ***
C
      DO 3300 I=1,2
      DO 3300 J=1,NORM(10)+4
      IF (PROB(0.5)) GOTO 3100
      IC=IWRD(1,I)
      IWRD(1,I)=IWRD(3,I)
      IWRD(3,I)=IC
 3100 IF (PROB(0.5)) GOTO 3200
      IC=IWRD(2,I)
      IWRD(2,I)=IWRD(4,I)
      IWRD(4,I)=IC
 3200 IF (PROB(0.5)) GOTO 3300
      IC=IWRD(5,1)
      IWRD(5,1)=IWRD(5,2)
      IWRD(5,2)=IC
 3300 CONTINUE
      IF (PROB(0.5)) GOTO 3400
      IC=IWRD(1,1)
      IWRD(1,1)=IWRD(5,1)
      IWRD(5,1)=IC
 3400 IF (PROB(0.5)) GOTO 3500
      IC=IWRD(1,2)
      IWRD(1,2)=IWRD(5,2)
      IWRD(5,2)=IC
 3500 CONTINUE
C
C=====C                      MM-A-4
C 5-A C
C=====C
C
C *** Vytvoreni slov z pismen ***
C
C * 1=NORMAL 2=REVERSE 3=UPSIDE DOWN 4=REVERSE AND UPSIDE DOWN *
C
      DO 3700 I=1,2
      IVRB(1,4*I)=IWRD(2,I)-32+(IWRD(1,I)-32)/256
      IVRB(2,4*I)=IWRD(4,I)-32+(IWRD(3,I)-32)/256
      IVRB(3,4*I)=IWRD(5,I)/256+32*256
      IVRB(1,4*I+1)=IWRD(4,I)-32+(IWRD(5,I)-32)/256
      IVRB(2,4*I+1)=IWRD(2,I)-32+(IWRD(3,I)-32)/256
      IVRB(3,4*I+1)=IWRD(1,I)/256+32*256
      IF ((IWRD(1,I) .NE. ' W') .AND. (IWRD(1,I) .NE. ' M')) GOTO 3600
      IVRB(1,4*I+2)=IWRD(2,I)-32+(IWRD(3,I)-32)/256
      IVRB(2,4*I+2)=IWRD(4,I)-32+(IWRD(1,I)-32)/256
      IVRB(3,4*I+2)=IWRD(5,I)/256+32*256
      IVRB(1,4*I+3)=IWRD(4,I)-32+(IWRD(5,I)-32)/256
      IVRB(2,4*I+3)=IWRD(2,I)-32+(IWRD(1,I)-32)/256
      IVRB(3,4*I+3)=IWRD(3,I)/256+32*256
      GOTO 3700
 3600 IVRB(1,4*I+2)=IWRD(2,I)-32+(IWRD(1,I)-32)/256
      IVRB(2,4*I+2)=IWRD(4,I)-32+(IWRD(5,I)-32)/256
      IVRB(3,4*I+2)=IWRD(3,I)/256+32*256
      IVRB(1,4*I+3)=IWRD(4,I)-32+(IWRD(3,I)-32)/256
      IVRB(2,4*I+3)=IWRD(2,I)-32+(IWRD(5,I)-32)/256
      IVRB(3,4*I+3)=IWRD(1,I)/256+32*256
 3700 CONTINUE
C
C *** Jmeno zbrane ***
C
      IWP(3)='  '
      IWP(4)='  '
      IF (ITST(10).NE.9) GOTO 3710
      IWP(1)='PI'
      IWP(2)='ST'
      IWP(3)='OL'
      IWP(4)='E '
3710  IF (ITST(10).NE.10) GOTO 3720
      IWP(1)='NÞ'
      IWP(2)='¦ '
3720  IF (ITST(10).NE.11) GOTO 3730
      IWP(1)='SE'
      IWP(2)='KE'
      IWP(3)='RA'
3730  IF (ITST(10).NE.12) GOTO 3740
      IWP(1)='ME'
      IWP(2)='¬ '
3740  IF (ITST(10).NE.13) GOTO 3750
      IWP(1)='JE'
      IWP(2)='D '
3750  IF (ITST(10).NE.14) GOTO 3800
      IWP(1)='KY'
      IWP(2)='J '
C
C *** Vyber vlkodlaka ***
C
C     vldkodlak muze byt pouze pani, sluha, sluzka, kuchar, pan, sadar
C
3800  IC=NORM(6)+1
      IF (IC.EQ.4) IC=8
      IF (IC.EQ.5) IC=9
      IF (IC.EQ.6) IC=14
      IF (IC.EQ.ITST(9)) GOTO 3800
      ITST(39)=IC
C
C *** Poznacime hru v kontrolnim souboru ***
C
C     IF (ITST(38).eq.'MM') then
C         CALL OPEN(IDCB,IERR,6HMMCF  )
C        if (ierr .eq. -6) then
C           call creat(idcb,ierr,6HMMCF  ,1,3)
C           if (ierr .gt. 0) call writf(idcb,ierr,1,1) !inicializace
C        else
C           CALL READF(IDCB,IERR,IFILX,1)
C           IFILX=IFILX+1
C           call rwndf(IDCB,IERR)
C           CALL WRITF(IDCB,IERR,IFILX,1)
C        endif
C        if (ierr .lt. 0) goto 9999
C        CALL CLOSE(IDCB,IERR)
C     endif
      NAME(1)='MM'
      NAME(2)='SB'
      NAME(3)='  '
C      CALL MESSS(20HOF,MMSA,8                   ,10)
C     CALL SGLOD(NAME,LU,0,0,0,0)
      P1=LU
      P2=0
      P3=0
      P4=0
      P5=0
      RETURN
C
C     ukonceni hry
C
 9999 if (ierr .lt. 0) write(lu,99990) ierr
99990 format(' FMP-chyba ',I4)
C     Call Close(Idcb)
      NAME(1)='MM'
      NAME(2)='SD'
      NAME(3)='  '
C     CALL SGLOD(NAME,LU,11111,0,0,0)
      P1=LU
      P2=11111
      P3=0
      P4=0
      P5=0
      RETURN
      END
