$STORAGE:2
$large
$nodebug
      SUBROUTINE MMSB(P1,P2,P3,P4,P5)
C     ,10-02-1983 MMM    8536E D45 PL&BM <850920.1754>
C     04-02-1988 MS-DOS S.M.
C
C ******* SEGMENT B *******
C
C      Tento segment zacina kazdou novou sekvenci. Ukoncuje se
C      volanim patricneho segmentu.
C
$INCLUDE:'MMCO.FOR'
      integer*2 p1,p2,p3,p4,p5,predloz(4),isluzce(4)
      INTEGER ITEM(4,149)
      LOGICAL*2 FLAG,ROVNA
      real*8 CTEM1(100),ctem2(49)
      character*72 f
      DIMENSION MSG(72),IMZ(2,3),MSGR(72),ITIM(5),IMSGR(73),IA(2)
      EQUIVALENCE (CTEM1,ITEM),(MSGR(1),IMSGR(2)),(RR,IA),(IA(2),LEN),
     +            (ctem2,item(1,101))
      DATA IMSGR/' >', 72*'  '/, MSG/72*'  '/,
     +     isluzce/'SL','U’','CE','  '/
C
C *** funkce ***
C
      IFSD(I)=I-100*INT(I/100.)
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
      NAME(2)='SD'
C
C *** navrat do debugu pri vypisu ***
C
      IF (ITST(25).EQ.-1) NAME(2)='SL'
      IF (ITST(25).EQ.-1) then
c         CALL SGLOD(NAME,LU)
          p1=lu
          p2=0
          p3=0
          p4=0
          p5=0
          return
      endif
C
C ****** INDEX ******
C
C                        Cinnost                              strana
C        A. cteni odpovedi                                      B-2
C        B. nalezeni slov v odpovedi                            B-3
C        C. nastaveni casove zavislych veci                     B-7
C           1. zvuky                                            B-7
C           2. zdroje svetla                                    B-7
C           3. vchody a vychody                                 B-9
C        D. veci zavisle na obyvatelich                         B-10
C           1. vrah, skret, bojovnik, vamipir a skritek         B-10
C           2. vlkodlak, demoni a krtek                         B-11
C
C=====C                      MM-B-1  (LINE 1360)
C 2-B C
C=====C
C
C  Prenositelne veci
C
C   1             RING       KEYS        CLOCK     LANTERN    CROSS
      DATA CTEM1/'PRSTEN  ','KL‹€E   ','HODINKY ','LUCERNA ','Kž‹’EK  ',
     1'KOMPAS  ','POKLADNA','BATERIE ','PISTOLE ','N¦’     ','SEKERA  ',
     3'ME€     ','AMPULE  ','KYJ     ','LANO    ','’IDLE   ','AMULET  ',
     5'TALISMAN','SV‹€KA  ','KULKA   ','KNIHA   ','L‹STEK  ','VYS‹LA€ ',
     7'J‹DLO   ','KO›T‰   ','PžIJ‹MA€','POCHODE¥','MAPA    ','KL‹N    ',
     1'GL•BUS  ','ZPALKY ','LOPATKA ','OLEJNI€K','SEKERKA ','TRUHLA  ',
     3'PERLY   ','SMARAGD ','MINCE   ','€‹›E    ','DIAMANTY','KLENOTY ',
     6'KROU’EK ','KORUNA  ','›IPKY   ','BRN‰N‹  ','KL‹€EK  ','BATERIE ',
     8'RUKAVICE','PAPOU›EK','PALCT  ','        ','        ','›ATY    ',
     1'ZMEK   ','POLICIE ','SPOJAž  ','HASI€   ','DRO’KA  ','        ',
     3'V‹NO    ','JED     ','VODA    ','OPICE   ','ZRCADLO ','KRESBA  ',
     5'OTVOR   ','DžEVO   ','HADRY   ','KO›     ','RAKEV   ','BODY    ',
     6'OBRAZ   ','TELEFON ','PANTY   ','RADIO   ','LAMPA   ','ZE…     ',
     7'STROM   ','TRPASL‹K','SOCHA   ','FIGUR‹NA','MODLA   ','BALVANY ',
     8'KAMEN‹  ','HK     ','NOV    ','STAR   ','SVITEK  ','OHE¥    ',
     9'PLAKETA ','ZCLONA ','DVEžE   ','BRNA   ','KRLOVNA','KRL    ',
     1'INDINI ','ST¦L    ','STOLY   ','SPRCHA  ','FIGUR‹NA'/
      data ctem2/'BK     ',
     2'Mž‹’E   ','DKA    ','TISKRNA','KAZETA  ','STž‹BRN','S‹DLO   ',
     3'A       ','ZV‰S   ','BEZHLAV‰','BALVAN  ','KOULE   ','        ',
     4'        ','        ','        ','        ','        ','        ',
     5'        ','        ','        ','        ','        ','        ',
     6'        ','        ','        ','        ','        ','        ',
     7'VBAVA  ','        ','        ','        ','        ','        ',
     8'KDE     ','CO      ','KDO     ','JAK     ','PRO€    ','KDY     ',
     7'BLBE€KU ','TROUBO  ','KRVO   ','SAKRA   ','OSLE    ','VOLE    '/
C
C=====C                           MM-B-2  (LINE 1440)
C 3-B C
C=====C
C
C   ****** TEST NA SPOJKU 'A' ******
C
      IF (IWRD(6,IWRD(7,1)+1).NE.108) GOTO 5
      DO 4 I=1,6
    4 IWRD(I,IWRD(7,1))=IWRD(I,IWRD(7,1)+2)
      IWRD(6,IWRD(7,1)+1)=0
      Inum=1
c        !????
      GOTO 296
C
C
C ****** CTENI VSTUPU ******
C
C **** NULOVANI POLE SLOV ****
C
    5 DO 10 I=1,9
      DO 9 J=1,4
       IWRD(J,I)='  '
    9  CONTINUE
      DO 10 J=5,9
       IWRD(J,I)=0
   10  CONTINUE
C
C     vstup a vystup
C
      LU=IPR(1)
      LX=ITST(21)
c     IF (IFBRK(IDMY)) then
c        LA=0
c        ITST(36)=0
c        LO=0
c        ITST(37)=0
c     else
         LA=ITST(36)
         LO=ITST(37)
c     endif
C
C **** cteme prikaz ****
C
      if (LA.eq.0) then
*        call reio(2,lu,'>\',1)
         write(*,'('' >''\)')
c        rr=reio(1,LU+400B,msgr,36)
         call reio(1,lu,msgr,len)
      else
c        rr=reio(1,la+400B,msgr,36)
         call reio(1,la,msgr,len)
      endif
C
C     *** programove klice ***
C
      if (len .eq. 1) then
C
C     *** vypis klicu ***
C
         IF (MSGR(1).eq.'9 ') then
c           CALL REIO(2,LU,'Programove klice: ',9)
            write(*,'('' Programov‚ kl¡‡e: '')')
            DO 226 II=1,8
c           call REIO(2,LU+2000B,identk(ii),1)
            write(*,'(''  '',a2)')identk(ii)
c           call REIO(2,LU,KLICE(1,II),IDELKY(II))
            iiii=idelky(ii)
            write(*,'('' '',10a2)')(klice(iii,ii),iii=1,iiii)
226         CONTINUE
            GOTO 5
c                       ! cteme znovu
         endif
C
C     *** hledani klice ***
C
227      DO 229 I=1,8
         IF (MSGR(1).EQ.IDENTK(I)) GOTO 228
229      CONTINUE
         goto 232
c            ! neni
C
C     *** presune se do vstupniho radku ***
C
228      CALL SMOVE(KLICE(1,I),1,2*IDELKY(I),MSGR,1)
         LEN=IDELKY(I)
c        if (la.eq.0) call REIO(2,LU,MSGR,LEN)
c          !opis prikazu
         if (la.eq.0) write(*,'('' '',10a2)')(msgr(iii),iii=1,len)
232   endif
      if (len .lt. 1) then
          msgr(1)='  '
          len=1
      endif
c     IF (LA.NE.0) call reio(2,LU,imsgr,LEN+1)
c       !pokud neni z terminalu
      if (la.ne.0) write(*,'('' '',39a2)')(imsgr(iii),iii=1,len+1)
c     IF (LO.NE.0) call reio(2,LO,msgr,LEN)
c          !zaznam na kazetu
      if (lo.ne.0) write(*,'('' '',39a2)')(msgr(iii),iii=1,len)
c     IF (LX.NE.0) call reio(2,LX,imsgr,LEN+1)
c       !listing
      if (lx.ne.0) write(*,'('' '',39a2)')(imsgr(iii),iii=1,len+1)
C
C     *** komentar ***
C
      IF (MSGR(1).EQ.'**') GOTO 5
C
C  *** prekodovani na velka ***
C
      call clcuc(msgr,2*len)
C
C     *** suspend ***
C
      IF (MSGR(1).NE.'SS'.OR.LEN.NE.1) GOTO 77
c        R0=REIO(2,LU,6HPause!,3)
c        CALL EXEC(7)
         pause
      GOTO 5
C
C *** DEKODOVANI SLOV ZE VSTUPNIHO RADKU ***
C
77    IC=0
      I=0
C
C ** dalsi slovo **
C
25    I=I+1
C
C     hodne slov
C
      IF (I.EQ.10.AND.IC.LT.2*LEN) then
c         call SGLOD(name,lu,170)
          p1=lu
          p2=170
          p3=0
          p4=0
          p5=0
          return
      endif
      IF (I.EQ.10) GOTO 80
c         !konec radku a 10 slov
      J=0
28    J=J+1
C
C * dalsi znak *
C
   29 IC=IC+1
      IF (IC.GT.LEN*2) GOTO 80
      CALL SGET(MSGR,IC,ICHAR)
C
C * IGNORUJ UVODNI MEZERY *
C
      IF (ICHAR.EQ.32.AND.J.EQ.1) GOTO 29
C
C * konec slova *
C
      IF (ICHAR.EQ.32) GOTO 25
      IF (J.GT.8) IWRD(9,I)=1
c        !poznacime si ze je dlouhe
      IF (J.LE.8) CALL SPUT(IWRD(1,I),J,ICHAR)
      GOTO 28
C
C
C=====C                      MM-B-3  (LINE 1520)
C 4-B C
C=====C
C
C *** veta je rozhazene na slova ****
C
C
C *** zaznamename pocet slov ***
C
   80 INUM=I
      IF (J.EQ.1.AND.IWRD(1,I).EQ.'  ') INUM=INUM-1
C
C *** hledani ve slovniku ***
C
      DO 170 I=1,INUM
C
C ** HLEDEJ SLOVESA **
C
      DO 110 K=1,89
      IF (JSCOM(IWRD(1,I),1,8,IVRB(1,K),1,IC).NE.0) GOTO 110
      IWRD(5,I)=1
      GOTO 169
  110 CONTINUE
C
C ** HLEDEJ SMERY **
C
      DO 120 K=1,11
      IF (JSCOM(IWRD(1,I),1,8,IDTN(1,K),1,IC).NE.0) GOTO 120
      IWRD(5,I)=2
      GOTO 169
  120 CONTINUE
C
C ** HLEDEJ PREDMETY **
C
      DO 130 K=1,137
      IF (ROVNA(IWRD(1,108),ITEM(1,108))) GOTO 127
      IF (IWORD(IWRD(1,I),1,8,ITEM(1,K),8).LE.0) GOTO 130
*     if(jscom(iwrd(1,i),1,8,item(1,k),1,ic).ne.0) goto 130
127   IWRD(5,I)=3
      GOTO 169
  130 CONTINUE
C
C ** HLEDEJ PRISLOVCE **
C
      DO 135 K=138,149
      IF (JSCOM(IWRD(1,I),1,8,ITEM(1,K),1,IC).NE.0) GOTO 135
      IWRD(5,I)=3
      GOTO 169
135   CONTINUE
C
C ** HLEDEJ OBYVATELE **
C * NAJDI SLOVO 'SLUZCE' *
C
      IF (JSCOM(IWRD(1,I),1,8,isluzce,1,IC).NE.0) GOTO 136
      IWRD(5,I)=4
      K=8
      GOTO 169
C
C * OSTATNI OBYVATELE *
C
136   DO 140 K=1,17
      IF (IWORD(IWRD(1,I),1,8,IRSN(1,K),8).LE.0) GOTO 140
*     if(jscom(iwrd(1,i),1,8,irsn(1,k),1,ic).ne.0) goto 140
      IWRD(5,I)=4
      GOTO 169
  140 CONTINUE
      IF (IWRD(9,I).NE.0) then
c         call SGLOD(name,lu,150) !dlouhe slovo
          p1=lu
          p2=150
          p3=0
          p4=0
          p5=0
          return
      endif
C
C ** NAJDI ZKRATKU SMERU **
C
      IF (I.GT.1) GOTO 155
      IC=IWRD(1,1)
      ID=0
      IF (IC.EQ.'S ')ID=1
      IF (IC.EQ.'V ')ID=2
      IF (IC.EQ.'J ')ID=3
      IF (IC.EQ.'Z ')ID=4
      IF (IC.EQ.'N ')ID=5
      IF (IC.EQ.'D ')ID=6
      IF (IC.EQ.'B ')ID=7
      IF (IC.EQ.'L ')ID=8
      IF (IC.EQ.'F ')ID=9
      IF (IC.EQ.'P ')ID=10
      IF (ID.NE.0)IWRD(5,1)=1
      IF (ID.NE.0)IWRD(5,2)=2
      IF (ID.NE.0)INUM=2
      IF (ID.NE.0)IWRD(6,1)=1
      IF (ID.NE.0)IWRD(6,2)=ID
      IF (ID.NE.0) GOTO 171
C
C ** HLEDEJ PREDLOZKU **
C
155   DO 150 K=1,17
c     IC=0
c     DO 1550 J=1,4
c       IF (ROVNA((IWRD(J,I) .OR. '  ' ),IPRP(J,K))) IC=IC+1
c       IF (IC .EQ. 4) IWRD(5,I)=5
c       IF (IC .EQ. 4) GOTO 169
c1550  continue
      call smove(iprp(1,k),1,8,predloz,1)
      call clcuc(predloz,8)
      if(jscom(iwrd(1,i),1,8,predloz,1,ic).ne.0) goto 150
      iwrd(5,i)=5
      goto 169
  150 CONTINUE
C
C * NEZNAME SLOVO *
C
c     CALL SGLOD(NAME,LU,160,I)
      p1=lu
      p2=160
      p3=i
      p4=0
      p5=0
      return
C
C **** ZKONTROLUJ KONSTRUKCI VETY ****
C
  169 CONTINUE
      IWRD(6,I)=K
C
C *** TEST NA NADAVKU ***
C
      IF (K.GT.143) then
c         CALL SGLOD(NAME,LU,165)
          p1=lu
          p2=165
          p3=0
          p4=0
          p5=0
          return
      endif
C
C *** TEST NA OTAZKU ***
C
      IF (K.GT.137.AND.K.LT.144) then
c         CALL SGLOD(NAME,LU,166)
          p1=lu
          p2=166
          p3=0
          p4=0
          p5=0
          return
      endif
  170 CONTINUE
C
C
C *** hledani synonym ***
C
C * vybava -> list
C
      IX=0
      if (IWRD(5,1).EQ.3.AND.IWRD(6,1).EQ.132.AND.INUM.EQ.1) then
         IWRD(5,1)=1
         IWRD(6,1)=28
      endif
C
C *** TEST NA MNOZSTVI SLOVES ***
C
C   PRVNI MUSI BYT SLOVESO
C
      IF (IWRD(5,1).NE.1) then
c         CALL SGLOD(NAME,LU,180)
          p1=lu
          p2=180
          p3=0
          p4=0
          p5=0
          return
      endif
C
C                   KONEC                KONEC
      IF (IWRD(6,1).EQ.19.AND.IWRD(5,2) .EQ. 1 .AND.
     1  IWRD(6,2).EQ.19) GOTO 9999
C
C   vic sloves jen u podplatit
C
      IF ((IWRD(5,2).EQ.1.OR.IWRD(5,3).EQ.1.OR.IWRD(5,4).EQ.1.OR.
     1IWRD(5,5).EQ.1).AND.IWRD(6,1).NE.51) then
c         CALL SGLOD(NAME,LU,190)
          p1=lu
          p2=190
          p3=0
          p4=0
          p5=0
          return
      endif
C
C                            MM-B-5  (LINE 1680)
C
C 6-B
C
C *** odstraneni pridavnych jemn a spojek ***
C
      IF (IWRD(6,1).NE.1) GOTO 178
c        ! jine sloveso nez JIT
C
C                         PREDL                   S
      IF (.NOT.(IWRD(5,2).EQ.5.AND.(IWRD(6,2).EQ.13.OR.
     1   IWRD(6,2).EQ.17))) GOTO 178
C
C *** jit s/za --> NASLEDOVAT
C
      IWRD(6,1)=13
      DO 179 I=1,9
  179 IWRD(I,2)=IWRD(I,3)
      INUM=INUM-1
C
C  *** hlavni cast ***
C
  178 DO 176 I=2,8
C
C     ODSTRANIT 'SI' A 'SE' (NENI-LI PO SLOVESU 'SPAT')
C
C                   PREDL
      IF (IWRD(5,I).EQ.5.AND.
     1  ((IWRD(6,I).EQ.9.OR.IWRD(6,I).EQ.12).AND.IWRD(6,1).NE.61))
     2  GOTO172
C         ZAVES -> ZACLONA
      IF (IWRD(5,I).EQ.3.AND.IWRD(6,I).EQ.109) IWRD(6,I)=91
C         KOULE -> GLOBUS
      IF (IWRD(5,I).EQ.3.AND.IWRD(6,I).EQ.112) IWRD(6,I)=30
C
C
C                   SMER                  PICK                TO
C     IF (IWRD(5,I).EQ.2.AND.IWRD(6,I-1).EQ.74.AND.IWRD(6,I).EQ.5)
C    1IWRD(6,I-1)=21
C                    SMER                 GET                TO
C     IF (IWRD(5,I).EQ.2.AND.IWRD(6,I-1).EQ.21.AND.IWRD(6,I).EQ.5)
C    1 GOTO 172
C
C
C                   SMER                  PUT                OVER
C     IF (IWRD(5,I).EQ.2.AND.IWRD(6,I-1).EQ.73.AND.IWRD(6,I).EQ.6)
C    1IWRD(6,I-1)=23
C                   SMER                  DROP                OVER
C     IF (IWRD(5,I).EQ.2.AND.IWRD(6,I-1).EQ.23.AND.IWRD(6,I).EQ.6)
C    1 GOTO 172
C
C
C                   PREDMET            PREDMET
      IF (IWRD(5,I).NE.3.OR.IWRD(5,I+1).NE.3) GOTO 176
C
C                   NOVA                BATERIE
      IF (IWRD(6,I).EQ.86.AND.IWRD(6,I+1).EQ.8) GOTO 172
C
C                  BATERIOVA             LUCERNA
C     IF (IWRD(6,I).EQ.8.AND.IWRD(6,I+1).EQ.4) GOTO 172
C
C                   STRIBRNA              KULKA
      IF (IWRD(6,I).EQ.106.AND.IWRD(6,I+1).EQ.20) GOTO 172
C
C
C                    STRIBRNA              KRIZEK
C     IF (IWRD(6,I).EQ.106.AND.IWRD(6,I+1).EQ.5) GOTO 172
C
C
C                   STRIBRNA                CISE
      IF (IWRD(6,I).EQ.106.AND.IWRD(6,I+1).EQ.39) GOTO 172
C
C
C                    STARA               BATERIE
      IF (IWRD(6,I).EQ.87.AND.IWRD(6,I+1).EQ.8)IWRD(6,I+1)=47
      IF (IWRD(6,I).EQ.87.AND.IWRD(6,I+1).EQ.47) GOTO 172
      GOTO 176
  172 INUM=INUM-1
      iminus=0
      DO 175 J=I,8
      DO 175 K=1,6
      IWRD(K,J)=IWRD(K,J+1)
      IF (J.EQ.8)IWRD(K,J+1)='  '
      IF (J.EQ.8.AND.K.GT.4)IWRD(K,J+1)=0
c     I=I-1
      iminus=iminus+1
  175 CONTINUE
  176 CONTINUE
      i=i-iminus
C
C *** ZPRACOVANI SYNONYM ***
C
C                   PREDMET             JED         AMPULE
      IF (IWRD(5,2).EQ.3.AND.IWRD(6,2).EQ.61)IWRD(6,2)=13
C
C     *** SPRCHOVAT => DAT SI SPRCHU ***
C                     SPRCHOVAT
      IF (IWRD(6,1).NE.22) GOTO 177
c      ! NENI 'SPRCHOVAT'
      IWRD(6,2)=99
c      ! -> VZIT SPRCHU
      IWRD(5,2)=3
      INUM=2
C
177   IF (IWRD(6,1).EQ.72) IWRD(6,1)=34
c  ! ZHASNOUT
      IF (IWRD(6,1).EQ.73) IWRD(6,1)=30
c        ! OTOCIT
      IF (IWRD(6,1).EQ.69) IWRD(6,1)=56
c        ! USTRELIT
      IF (IWRD(6,1).EQ.74) IWRD(6,1)=30
c        ! ZATOCIT
      IF (IWRD(6,1).EQ.75) IWRD(6,1)=35
c        ! FOUKNOUT
      IF (IWRD(6,1).EQ.76) IWRD(6,1)=37
c        ! TRIT
      IF (IWRD(6,1).EQ.77) IWRD(6,1)=60
c        ! POVESIT
      IF (IWRD(6,1).EQ.78) IWRD(6,1)=32
c        ! OTEVRIT
      IF (IWRD(6,1).EQ.79) IWRD(6,1)=42
c        ! ODPOVEDET
      IF (IWRD(6,1).EQ.80) IWRD(6,1)=43
c        ! NAKRMIT
      IF (IWRD(6,1).EQ.81) IWRD(6,1)=45
c        ! ZAUTOCIT
      IF (IWRD(6,1).EQ.82) IWRD(6,1)=56
c        ! ZASTRELIT
      IF (IWRD(6,1).EQ.83) IWRD(6,1)=57
c        ! NAOLEJOVAT
      IF (IWRD(6,1).EQ.84) IWRD(6,1)=60
c        ! POLOZIT
      IF (IWRD(6,1).EQ.85) IWRD(6,1)=25
c        ! ROZHLEDNOUT
      IF (IWRD(6,1).EQ.86) IWRD(6,1)=1
c         ! BEZET
      IF (IWRD(6,1).EQ.87) IWRD(6,1)=1
c         ! UTIKAT
      IF (IWRD(6,1).EQ.88) IWRD(6,1)=29
c        ! ZAPNOUT
      IF (IWRD(6,1).EQ.89) IWRD(6,1)=34
c        ! VYPNOUT
      IF (IWRD(6,1).EQ.66) IWRD(6,1)=27
c        ! HLEDAT
      IF (IWRD(6,1).EQ.67) IWRD(6,1)=49
c        ! PIT
      IF (IWRD(6,1).EQ.68) IWRD(6,1)=25
c        ! DIVAT
      IF (IWRD(6,1).EQ.70) IWRD(6,1)=33
c        ! zatahnout
      IF (IWRD(6,1).EQ.71) IWRD(6,1)=29
c        ! zapalit
      IF (IWRD(6,1).EQ.62) IWRD(6,1)=47
c        ! ULOZIT
      IF (IWRD(6,1).EQ.63) IWRD(6,1)=41
c        ! ODSUNOUT
      IF (IWRD(6,1).EQ.64) IWRD(6,1)=40
c        ! VOLAT
      IF (IWRD(6,1).EQ.65) IWRD(6,1)=40
c        ! TELEFONOVAT
C
C
C                    STUL
C?????IF (IWRD(6,3).EQ.97)INUM=INUM-1
C
C *** VYPUSTENI A CIST, A ODLOZIT ***
C                           A                  VZIT
      IF (IWRD(6,INUM-1).EQ.108.AND.IWRD(6,INUM).EQ.26)INUM=INUM-2
C
C                           A                 ODLOZIT
      IF (IWRD(6,INUM-1).EQ.108.AND.IWRD(6,INUM).EQ.23)INUM=INUM-2
C
C                            MM-B-6  (LINE 1760)
C
C 7-B
C
C **** CASOVE ZAVISLE PREDMETY ****
C
C *** CITAC TAHU ***
C
  171 IR=ITST(1)
      NAME(2)='SD'
      IF (IWRD(1,1).NE.'  ') ITST(22)=0
c        !NECO ODPOVEDEL
      ITST(27)=ITST(5)
c                         !cislo posledniho tahu
      ITST(5)=ITST(5)+1
      R=URAN()
c                !ABY SE ZMENILO
      CALL cas(10,ITIM)
      IC=ITIM(4)*1200+ITIM(3)*20+ITIM(2)/3
      IF (IC-ITST(18).LT.0)ITST(18)=0
c          !prekrocen den
      IF (ITST(5).GT.1) ITST(5)=ITST(5)+(IC-ITST(18))/10
      ITST5=ITST(5)
c                            !pripocteni tahu za cekani
  173 F='(/'' Trv  V m to moc dlouho.'')'
      IF (IC-ITST(18).GE.10.AND.IFSD(ITST(5)).EQ.95) WRITE(LU,f)
      ITST(18)=IC
C
C *** je ve vode ***
C
      IF (ITST(1).EQ.46.AND.(IWRD(6,1).NE.1.OR.IWRD(6,2).NE.11.OR.
     1IWRD(5,2).NE.2).AND.IXT(320).EQ.0) then
c         CALL SGLOD(NAME,LU,176)
          p1=lu
          p2=176
          p3=0
          p4=0
          p5=0
          return
      endif
      IT=0
      IF (ITST(32).NE.0)IT=ITST(5)-ITST(32)
      IF (IT.GT.15)IXT(352)=4651
      IF (IT.GT.15)IXT(351)=4649
      IF (IT.GT.24.AND.IXT(319).NE.0)IXT(319)=4652
      IF (IT.GT.24)IXT(353)=4650
      IF (IT.GT.36)IXT(354)=4653
      IF (IT.GT.36)IXT(318)=4645
C
C ** udery zvonu **
C
      IC=0
      IF (ITST5/25.-ITST5/25.EQ.0)IC=ITST5/25+6
  174 IF (IC.GT.12)IC=IC-12
      IF (IC.GT.12) GOTO 174
  200 F='(/'' Zvon zazvonil ''I3'' kr t.'')'
      IF (IC.NE.0.AND.(IR.LT.28.OR.IR.GT.53)) WRITE(LU,f)IC
C
C *** jestlize neda po hluku SKORE BODY, odpocet max. skore
C
      IC=ITST5-(ITST5/25)*25
      IF (IC.EQ.13.AND.IWRD(6,1).NE.20.AND.IWRD(6,2).NE.40)
     1  ITST(3)=ITST(3)-2
C
C *** kazdy 12. tah po uderu zvonu v sidle krik ***
C
  181 F='(/'' Usly¨el jste ‘ensk˜ k©ik'')'
      IF (IC.EQ.12.AND.IR.LT.28.AND.ITST5.LT.450) WRITE(LU,f)
C
C *** v noci venku vlci vyti ***
C
  182 F='(/'' Usly¨el jste vl‡¡ vyt¡'')'
      IF (IC.EQ.12.AND.IR.GT.53.AND.ITST5.GE.300.AND.ITST5.LT.450)
     1 WRITE(LU,f)
C
C *** jinak vrana ***
C
  183 F='(/'' Sly¨el jste kr kat vr nu'')'
      IF (IC.EQ.12.AND.IR.GT.53.AND.ITST5.LT.300) WRITE(LU,f)
C
  184 F='(/'' Usly¨el jste rachot padajic¡ho kamen¡'')'
      IF (IC.EQ.12.AND.IR.LT.54.AND.IR.GT.27.AND.ITST5.LT.450)
     1 WRITE(LU,f)
C
C  *** uvnitr standardne kameni ***
C
C
C * uzavreti zahradni brany po 18. hod
C
      IF (ITST(5).GT.300)IXT(27)=0
C
C                            MM-B-7  (LINE 1840)
C 8-B
C
C
C
C *** zvoneni telefonu ***
C
      IF (IVEN(73).NE.11) GOTO 194
C
C * O LISTKU *
C
      IF (IVEN(22).GT.20000.AND.IC.EQ.12)IVEN(73)=122
C
C * O DIVANI NA VECI *
C
      IF (IVEN(4).GT.20000.AND.IVEN(6).GT.20000.AND.ITST5.EQ.120)
     1IVEN(73)=222
C
C * O POLICII *
C
      IF (ITST(8).EQ.0.AND.IC.EQ.5.AND.IRES(ITST(9)).NE.0)IVEN(73)=322
C
C * O LUCERNE *
C
      IF (ITD(IVEN(4)).EQ.4.AND.IR.EQ.11) IVEN(73)=422
C
C ** JE HRAC POBLIZ **
C
  194 continue
  195 F='(/'' Zaslechl jste zvonˆn¡ telefonu'')'
      IF (IFSD(IVEN(73)).EQ.22.AND.IR.GT.9.AND.IR.LT.19.AND.IWRD(6,1)
     1.NE.42.AND.IWRD(6,1).NE.40) WRITE(LU,f)
      IF (IVEN(73).NE.11)IVEN(73)=IVEN(73)+1000
      IF (IVEN(73).GE.7000)IVEN(73)=11
C
C *** ZNICENI SIDLA ***
C
C
C   * po 24. hod. se propadnou se chodby *
C
      IF (ITST5.GE.450.AND.IXT(26).NE.0) then
c         CALL SGLOD(NAME,LU,185)
          p1=lu
          p2=185
          p3=0
          p4=0
          p5=0
          return
      endif
C
C   * po 01.40 v sidle kour *
C
  188 F='(/'' Kou© V s dr ‘d¡ ke ka¨li'')'
      IF (IR.GT.18.AND.IR.LT.28.AND.ITST5.GT.490) WRITE(LU,f)
C
C   * po 02.00 se rozpadne vrchni poschodi *
C
      IF (IR.GT.18.AND.IR.LT.28.AND.ITST5.GE.500) then
c     1 CALL SGLOD(NAME,LU,189)
          p1=lu
          p2=189
          p3=0
          p4=0
          p5=0
          return
      endif
C
C   * je u brany po 24.00 hod. -> jediny spravny konec hry *
C
      IF (IR.EQ.93.AND.ITST5.GE.451.AND.IXT(26).EQ.0) then
c     1 CALL SGLOD(NAME,LU,191)
          p1=lu
          p2=191
          p3=0
          p4=0
          p5=0
          return
      endif
C
C   *  po 04.00 uz se s timto hracem neda nic delat *
C
      IF (ITST5.GE.550) then
c      CALL SGLOD(NAME,LU,192)
          p1=lu
          p2=192
          p3=0
          p4=0
          p5=0
          return
      endif
C
C *** LUCERNA A BATERIE ***
C
C
C  *** pokud je stara baterie v bludisti a hrac ne, udela se nova ***
C
      IF (IVEN(8).EQ.0.AND.IVEN(47).GT.227.AND.IVEN(47).LT.235
     1.AND.ITST(1).LT.28) IVEN(47)=0
      IF (IVEN(8).EQ.0.AND.IVEN(47).EQ.0) IVEN(8)=NORM(6)+228
C
C  *** zhasnuta lucerna ***
C
      IF (IFD(IVEN(4)).EQ.0) GOTO 210
C
      ITST(6)=ITST(6)+1
      IBT=ITST(6)-(ITST(6)/1000)*1000
C
      IF (IBT.LT.100) GOTO 210
C
C  *** baterie v lucerne dosla ***
C
      IF (IBT.EQ.120) IVEN(4)=IVEN(4)-1000
C
C  *** lucernu a baterii nema ***
C
      IF (IVEN(4).LT.20000) GOTO 210
      IF (IVEN(8).LT.20000) GOTO 204
C
C  *** kdyz ma baterii, automaticky se vymeni ***
C
      IVEN(8)=0
      ITST(6)=(ITST(6)/1000)*1000
  202 F='(/'' Vymˆnil jsem V m sl bnouc¡ baterii'')'
      WRITE(LU,f)
C
C  *** a odlozi starou baterii tam kde je ***
C
      IVEN(47)=20200+IR
      GOTO 210
C
C  *** ibt>100, baterie slabne, pri 120 zhasne ***
C
  204 IBT=IBT-100
      if (IBT.EQ.20) then
  206    F='(/'' Va¨e lucerna zhasla'')'
         WRITE(LU,f)
      else
C
C  *** upozorneni ***
C
  208 F='(/'' Baterie ve Va¨¡ lucernˆ sl bne'')'
      IF (IBT/5..EQ.1.*INT(IBT/5.)) WRITE(LU,f)
      endif
C
C
C=====C                      MM-B-8  (LINE 1920)
C 9-B C
C=====C
C
C   *** SVICKA ***
C
  210 IF (IFD(IVEN(19)).EQ.0) GOTO 220
c   ! nesviti
C
C  *** stav po 1000 ***
C
      ITST(6)=ITST(6)+1000
      IBT=ITST(6)/1000
      IF (IBT.LT.20) GOTO 220
C
C  *** pokud je v kapli pred dohorenim, okamzite nova ***
C
      IF (IBT.EQ.20.AND.IVEN(19).EQ.1216) ITST(6)=ITST(6)-20000
C
C  *** po vyhoreni na nova v kapli ***
C
      IF (IBT.EQ.32) then
          itst(6)=itst(6)-32000
  214     F='(/'' Va¨e sv¡‡ka £plnˆ vyho©ela'')'
          IF (IVEN(19).GT.20000) WRITE(LU,f)
          IVEN(19)=1216
      endif
C
C  *** pokud ji nese a dohoriva, pak upozorneni ***
C
  212 F='(/'' Va¨e sv¡‡ka pomalu doho©¡v '')'
      IF (IBT.EQ.29.AND.IVEN(19).GT.20000) WRITE(LU,f)
C
C
C *** ZAPALKY ***
C
  220 IF (IFD(IVEN(31)).EQ.0) GOTO 230
      ITST(7)=ITST(7)+10000
      IBT=ITST(7)/10000
C
C  *** vydrzi jen tri tahy ***
C
      IF (IBT.LT.3) GOTO 230
      ITST(7)=ITST(7)-30000
  222 F='(/'' Va¨e z palka doho©ela'')'
      IF (IVEN(31).GT.20000) WRITE(LU,f)
      IVEN(31)=118
C
C *** POCHODEN ***
C
  230 IF (IVEN(27).EQ.0.AND.IR.EQ.53.AND.IXT(320).NE.0)IVEN(27)=1242
      IF (IVEN(27).EQ.0.AND.IR.EQ.42.AND.IXT(320).EQ.0)IVEN(27)=1245
      IF (IVEN(27).EQ.0.AND.(IR.EQ.28.OR.IR.EQ.33)) IVEN(27)=1230
C
C *** pochoden jinde likvidujem ***
C
      IF (IVEN(27).NE.0.AND.IVEN(27).LT.20000.AND.IR.LT.9.AND.
     1 IFSD(IVEN(27)).GT.27.AND.IFSD(IVEN(27)).LT.54) IVEN(27)=0
      IF (IVEN(27).EQ.0) ITST(7)=ITST(7)-100*ITFD(ITST(7))
      IF (IFD(IVEN(27)).EQ.0) GOTO 250
C
C *** pochoden nesmi nest ven ***
C
      ITST(7)=ITST(7)+100
      IBT=ITFD(ITST(7))
      IF ((IR.LT.28.OR.IR.GT.53).AND.IVEN(27).GT.21000) WRITE(LU,244)
      IF ((IR.LT.28.OR.IR.GT.53).AND.IFD(IVEN(27)).EQ.1)
     1IVEN(27)=IVEN(27)-1000
C
C *** upozorneni na dohoreni ***
C
      IF (IBT.LT.21) GOTO 250
  242 F='(/'' Va¨e pochode¤ prsk  a sl bne jej¡ jas'')'
      IF (IBT.LT.25.AND.IVEN(27).GT.21000) WRITE(LU,f)
C
C *** dohorela ***
C
  244 FORMAT(/' Va¨e pochode¤ doho©ela')
      IF (IBT.EQ.25.AND.IVEN(27).GT.21000) WRITE(LU,244)
C
      IF (IBT.EQ.25.AND.IFD(IVEN(27)).EQ.1)IVEN(27)=IVEN(27)-1000
C
C *** LABORATOR A ZACHOD ***
C
C     *** OBNOVENI VCHODU ***
C
  250 IXT(16)=(IXT(16)/10000)*10000+2724
      IXT(57)=0
      IVEN(68)=27
      IC=ITST5-(ITST5/25)*25
      IF (IC.GT.21) IXT(16)=(IXT(16)/10000)*10000+8824
      IF (IC.EQ.0) IXT(57)=8988
      IF (IC.GT.21) IVEN(68)=88
C
C  *** POKUD MA S SEBOU ZIDLI, OTEVRE SE VYCHOD do podkrovi ***
C
      IXT(271)=0
      IF (IVEN(16).EQ.519)IXT(271)=8719
C
C *** ZRANENI ***
C
      DO 251 I=1,17
      IF (IFD(IRES(I)).EQ.1) THEN
         IF (PROB(0.3)) IRES(I)=IRES(I)-1000
      ELSEIF (IFD(IRES(I)).EQ.2) THEN
         IF (PROB(0.3)) THEN
            IRES(I)=IRES(I)-1000
         ELSEIF (PROB(0.3)) THEN
             IRES(I)=IRES(I)+1000
         ENDIF
      ELSEIF (IFD(IRES(I)).EQ.9) THEN
         IF (PROB(0.5)) IRES(I)=IRES(I)+1000
      ELSEIF (IFD(IRES(I)).EQ.8) THEN
         IF (PROB(0.3)) THEN
            IRES(I)=IRES(I)+1000
         ELSEIF (PROB(0.3)) THEN
            IRES(I)=IRES(I)-1000
         ENDIF
      ENDIF
  251 CONTINUE
      IF (ITST(31).EQ.1) THEN
         IF (PROB(0.3)) ITST(31)=0
      ELSEIF (ITST(31) .EQ. 2) THEN
         IF (PROB(0.3)) THEN
            ITST(31)=1
         ELSEIF (PROB(0.3)) THEN
            ITST(31)=3
         ENDIF
      ENDIF
C
C **** UDALOSTI ZAVISLE NA OBYVATELICH ****
C
C *** VRAH ***
C
C *** pokud ma vrah zbran a odhalil vrazdu, zabije ho ***
C
      IF (ITST(8).EQ.0.AND.IR.EQ.IFSD(IRES(ITST(9))).AND.IRES(ITST(9))
     1.LT.10000.AND.(IVEN(10).EQ.10200+ITST(9).OR.IVEN(11).EQ.10300+
     1ITST(9).OR.IVEN(12).EQ.10300+ITST(9)).AND.ITST5-1.NE.
     2ITST(13)) then
c         CALL SGLOD(NAME,LU,251)
           p1=lu
           p2=251
           p3=0
           p4=0
           p5=0
           return
       endif
C
C *** SKRET ***
C
      IF (IFSD(IRES(13)).NE.IR.OR.ITST(13).EQ.ITST5-1.OR.IRES(13)
     1.GT.10000) GOTO 253
      FLAG=PROB(0.8)
      IF (FLAG.AND.(IVEN(34).EQ.17213)) WRITE(LU,252)
  252 FORMAT(/' Sk©et hodil po V s sekerou a netrefil se.'/' Proklel',
     1' V s, obˆhl V s a sekeru si vzal')
      IF (.NOT. FLAG.AND.(IVEN(34).EQ.17213)) then
c         CALL SGLOD(NAME,LU,254)
          p1=lu
          p2=254
          p3=0
          p4=0
          p5=0
          return
      endif
C
C *** RYTIR ***
C
C *** pokud prijde hrac ke vchodu, objevi se u zadni brany ***
C
  253 IF (IR.EQ.92.AND.IRES(16).EQ.0)IRES(16)=995
      IF (IFSD(IRES(16)).NE.IR.OR.IRES(16).GT.1999.OR.(IWRD(6,1).NE.20
     1.AND. PROB(0.67))) GOTO 260
      IWRD(7,1)=2
      IWRD(6,1)=46
      IWRD(1,2)='RY'
      IWRD(2,2)='TI'
      IWRD(3,2)='R '
      IWRD(4,2)='  '
      IWRD(5,2)=4
      IWRD(6,2)=16
  254 F='(/'' Ryt¡© hroznˆ za©val a za£to‡il na V s.'')'
      WRITE(LU,f)
      NAME(2)='SH'
c      CALL SGLOD(NAME,LU)
      p1=lu
      p2=0
      p3=0
      p4=0
      p5=0
      return
C
C *** VAMPIR ***
C
C   *** pokud je ve stejne mistnosti a hrac nema krizek, konec ***
C
  260 IF ((IFSD(IRES(5)).EQ.IR).AND.(IVEN(5).LT.20000)
     1.AND.(ITST(13).NE.ITST5-1)) then
c     CALL SGLOD(NAME,LU,262)
          p1=lu
          p2=262
          p3=0
          p4=0
          p5=0
          return
      endif
C
C   *** Inicializace vampira v hrobce po 8.00 hod. ***
C
      IF (IRES(5).EQ.300.AND.ITST5.GE.50) IRES(5)=5
C
C *** SKRITEK ***
C
      IF ((IFSD(IRES(11)).NE.IR).OR.(ITST(13).EQ.
     1ITST5-1).OR.(IRES(11).GT.10000)) GOTO 270
C
C ** NECO NA UKRADENI **
C
      IC=0
      DO 261 J=NORM(52),52
      IF (IVEN(J).GT.20000)IC=J
      IF (IC.NE.0) GOTO 264
  261 CONTINUE
C
      DO 263 J=1,NORM(52)
      IF (IVEN(J).GT.20000)IC=J
      IF (IC.NE.0) GOTO 264
  263 CONTINUE
      GOTO 266
C
C VECI Z HALY SI ULOZI DO STUDNY
C
  264 DO 265 J=1,52
      IF ((IVEN(J)/10000.EQ.1).AND.(IFSD(IVEN(J)).EQ.
     111))IVEN(J)=IVEN(J)-10011+79
  265 CONTINUE
C
C  *** odnese to do haly ***
C
  266 IF (IC.NE.0) IVEN(IC)=IVEN(IC)-10000+11
C
C  *** a zmizne pryc ***
C
      IF (IR.LT.62) IRES(11)=870
      IF (IR.GT.61) IRES(11)=854
  267 F='(/'' SKž‹TEK V s kopl do zadnice a utekl'')'
      IF (IC.EQ.0) WRITE(LU,f)
C
  268 F='(/'' Probˆhl kolem V s SKž‹TEK a ''4A2'' je pry‡.'')'
      IF (IC.NE.0) WRITE(LU,f)(ITEM(L,IC),L=1,4)
C
C
C                            MM-B-10  (LINE 2080)
C 11-B
C
C ** VLKODLAK **
C
C * premena *
C
C *** neni zabit a je po 18.00
C
  270 IF (IRES(ITST(39)).GT.10000.OR.IRES(ITST(39)).EQ.0.OR.ITST(5)
     1.LT.300) GOTO 280
C
C *** neni videt mesicek ***
C
      IF (IROM(IFSD(IRES(ITST(39)))).LT.20000.AND.IFSD(IRES(ITST(39)))
     1.LT.54) GOTO 280
C
C *** vlkodlak ***
C
      IRES(10)=IRES(ITST(39))
      IRES(ITST(39))=0
  275 FORMAT(/' Ne‘ jste mohl nˆco u‡init, ',
     14A2' se zmˆnil na VLKODLAKA.'/' Vid¡te, jak mu rost',
     2'ou vlasy, tes ky a dr py.')
      IF (IR.EQ.IFSD(IRES(10))) WRITE(LU,275)(IRSN(L,ITST(39)),L=1,4)
C
C * UTOK VLKODLAKA *
C
  280 IF ((IR.EQ.IFSD(IRES(10))).AND.(ITST5-1.NE.ITST(13))
     1.AND.(IRES(10).LT.10000)) then
c     CALL SGLOD(NAME,LU,285)
          p1=lu
          p2=285
          p3=0
          p4=0
          p5=0
          return
      endif
C
C * VYJICI DEMONI *
C
      IC=NORM(3)-1
      ID=NORM(8)+1
      IF (ITD(IVEN(35)).EQ.5.AND.IC.NE.0.AND.IWRD(6,1).EQ.1) then
c     1 CALL SGLOD(NAME,LU,287,ID)
          p1=lu
          p2=287
          p3=id
          p4=0
          p5=0
          return
      endif
C
C * KRTCI PAST *
C
      IF (IR.EQ.52.AND.IFSD(IRES(4)).EQ.52)IXT(258)=0
C
C ** SMICH LIDI **
C
      IF (IVEN(53).GT.20000.OR.IR.EQ.1.OR.IR.EQ.20.OR.IR.EQ.25) GOTO 295
  293 F='(/'' V¨iml jste si, ‘e se V m ''4A2'' smˆje'')'
      DO 290 I=2,17
      IF (I.EQ.4.OR.I.EQ.15.OR.IFSD(IRES(I)).NE.IR) GOTO 290
      IF (IRES(I) .GT. 10000) GOTO 290
      WRITE(LU,f)(IRSN(L,I),L=1,4)
      GOTO 295
  290 CONTINUE
C
C *** JE TREBA SVETLO? ***
C
C   VZIT, SPRCHOVAT, DAT, PODIVAT, CIST, NAJIT
C
  295 IF (IWRD(6,1).EQ.21.OR.IWRD(6,1).EQ.22.OR.IWRD(6,1).EQ.24.OR.
     1IWRD(6,1).EQ.25.OR.IWRD(6,1).EQ.26.OR.IWRD(6,1).EQ.27)
     1 CALL MMRL
C
C ****** VOLANI PATRICNEHO SEGMENTU:  C, F, H, J ******
C
  296 IF (IWRD(6,1).LT.18) NAME(2)='SC'
      IF (IWRD(6,1).GT.17.AND.IWRD(6,1).LT.36) NAME(2)='SF'
      IF (IWRD(6,1).GT.35.AND.IWRD(6,1).LT.54) NAME(2)='SH'
      IF (IWRD(6,1).GT.53) NAME(2)='SJ'
      IF (INUM.EQ.1.AND.IWRD(6,1).EQ.25) NAME(2)='SC'
      IWRD(7,1)=INUM
c     CALL SGLOD(NAME,LU)
      p1=lu
      p2=0
      p3=0
      p4=0
      p5=0
      return
 9999 CONTINUE
      NAME(2)='SD'
c     CALL SGLOD(NAME,LU,11111)
      p1=lu
      p2=11111
      p3=0
      p4=0
      p5=0
      return
      END
*
*
      FUNCTION IWORD(LINE1,ZAC,DEL,WORD1,PLEN)
c    :,17-09-1982 MAN    8536E U BM
C
C     HLEDA SLOVO 'WORD1' O  DELCE 'LEN' NA  RADKU  'LINE1'
C     POCINAJE POZICI 'ZAC' A KONCE POZICI 'KON'. FUNKCNI
C     HODNOTA:
C             IWORD=0 .. NENI NALEZENO
C             IWORD=N .. NALEZENO OD POZICE 'N'
C
C     POZN.: VSECHNA PISMENA MUSI BYT PREKODOVANA NA
C            VELKA!
C
C
      IMPLICIT INTEGER (A-Z)
C
      DIMENSION PP(20),KONC(50),KZAC(5),KKON(5)
      DIMENSION WORD1(1),LINE1(1)
      INTEGER*2 WORD(40),LINE(40)
C
      LOGICAL PASS1
C
      DATA KZAC /1,7,23,80,88/
c         ! ZACATEK KONCOVEK DELKY 'I'
      DATA KKON /6,21,77,84,88/
c        ! KONEC KONCOVEK DELKY 'I'
      DATA KONC /
     1           'AE','IO','UY',
     2           'OU','AM','EM','IM','UM','OM','MI','YM',
     3           'OV','IO','VE','EC','HI','CH','AC','HA',
     3           'MI','EM','II','MI','YM','IE','TE','EN','EE',
     3           'TI','AT','YA','TA','YC','HE','HO','EM','UI',
     3           'MU','IH','OA',
     4           'TU','ME','TE','MA','TE','CH',
     B           '  ','  ','  ','  '/
C    ^
C    ! .. DELKA KONCOVEK
C
C *** MODIFIKACE PRO MMM ***
C
      CALL CZECH(LINE1,LINE,8)
      CALL CZECH(WORD1,WORD,8)
      PASS1=.FALSE.
      DO 20 LEN=PLEN,1,-1
      CALL SGET(WORD,LEN,B)
      IF (B.NE.32) GOTO 5
20    CONTINUE
      GOTO 40
c         ! NENALEZENO
5     IF (DEL .LT. ZAC) GOTO 40
c  ! NEHLEDA SE NA RADKU DELKY <= 0
C
C     NAJDI SLOVO V 1 PADU/SG
C
      B=ZAC
10    POM=JSTR(LINE,B,DEL,WORD,1,LEN)
      IF (POM.LT.0) GOTO 29
      IF (IABC(LINE,ZAC,DEL,POM).EQ.1 .AND.
     +    IABC(LINE,ZAC,DEL,POM+LEN-1).EQ.2) GOTO 100
c      ! SLOVO NALEZENO
      B=B+1
c         ! HLEDEJ DALSI VYSKYT
      GOTO 10
C
C     NAJDI SLOVO V JINEM PADU NEZ 1/SG
C
29    IF (DEL-ZAC.LE.2) GOTO 40
c ! V JINYCH PADECH JE SLOVA DELSI NEZ 3
      B=ZAC
c            ! POMOCNY ZACATEK RADKU
      LEN1=LEN
c         ! POMOCNA DELKA VZORU
      STR=0
c            ! PRIZNAK SAMOHLASKY JAKO POSL. PISMENE
      IF (LEN1.LE.2) GOTO 40
c            ! (PRILIS KRATKE SLOVO)
      PASS1=.TRUE.
c                      ! PRIZNAK 1. PRUCHODU
      CALL SMOVE(WORD,1,LEN,PP,1)
c       ! VZOR DO POMOCNEHO POLE
      CALL SGET(WORD,LEN1,CHAR)
c         ! POSLEDNI ZNAK SLOVA
      IF (CHAR.EQ.65.OR.CHAR.EQ.69.OR.
     +    CHAR.EQ.79.OR.CHAR.EQ.85.OR.CHAR.EQ.89) STR=1
C
C     HLEDANI SLOV S OHLEDEM NA SKLONOVANI
C
30    POM=JSTR(LINE,B,DEL,PP,1,LEN1-STR)
      IF (POM.LT.0) GOTO 40
      B=POM+1
      IF (IABC(LINE,ZAC,DEL,POM) .NE. 1) GOTO 30
c       ! ZACATEK SLOVA?
      JJ=POM+LEN1-1-STR
      DO 33 J=JJ,JJ+5
c         ! NAJDI KONEC SLOVA V ROZMEZI
      IF (IABC(LINE,ZAC,DEL,J) .EQ. 2) GOTO 34
c        ! 0 AZ 5 ZNAKU
33    CONTINUE
c          ! OD NALEZENEHO KONCE ZAKLADU
      GOTO 30
c                 ! NENALEZENO, HLEDEJ DAL
34    CONTINUE
      IF (J .EQ. JJ) GOTO 100
c       ! NALEZENO, KONCOVKA DELKY 0
      J=J-JJ
c                       ! DELKA KONCOVKY
c     DO 35 K=KZAC(J),KKON(J),J
      k=kzac(j)
c                 ! SPOROVNEJ VSECHNY
35    IF (JSCOM(LINE,JJ+1,JJ+J,KONC,K,E) .EQ. 0) GOTO 100
c       ! KONCOVKY
c35    CONTINUE
      k=k+j
      if(k.le.kkon(j)) goto 35
c                           ! VHODNE DELKY
      GOTO 30
C
C     KONEC PROHLEDAVANI / DRUHY PRUCHOD?
C
40    IWORD=0
c    ! NENALEZENO
      LEN1=LEN
      CALL SGET(PP,LEN1-1,CHAR)
c        ! JE-LI PREDPOSL.
      IF (.NOT.PASS1.OR.CHAR.NE.69) RETURN
      CALL SGET(PP,LEN1,CHAR)
c           ! PISMENO 'E',
      CALL SPUT(PP,LEN1-1,CHAR)
c         ! VYNECH HO
      LEN1=LEN1-1
c                      ! UPRAV DELKU VZORU
      PASS1=.FALSE.
c                    ! DRUHY PRUCHOD
      STR=0
c                ! OPET POCATECNI
      B=ZAC
c                ! PODMINKY
      GOTO 30
C
100   IWORD=POM
c  ! NALEZENO
      RETURN
      END
*
*
      integer*2 FUNCTION IABC(POLE,ZAC,KON,POS)
c      ,21-10-1981 MANUAL B.M.
C
C     FUNKCE ZJISTUJE, ZDA JE ZNAK UPROSTRED SLOVA (=0),
C     NA ZACATKU (=1), NA KONCI (=2) NEBO ZDA JE ZNAK
C     MEZI JINYMI ZNAKY NEZ PISMENY (=3)
C     HODNOTY ZAC, KON A POS JSOU UDANY VE ZNACICH!
C
      IMPLICIT INTEGER (A-Z)
C
      DIMENSION BEG(2),POLE(1)
C
c     DATA MA,MZ,VA,VZ /97,122,65,90/
      data ma,mz,va,vz/97,122,65,90/
C
      IABC=0
      BEG(1)=ZAC
      BEG(2)=KON
      J=-1
      DO 10 I=1,2
      K=0
      IF (BEG(I) .NE. POS) GOTO 5
      K=I
      GOTO 9
C
5     CALL SGET(POLE,POS+J,ZN)
      IF (ZN .LT. MA .AND. ZN .GT. VZ .OR. ZN .LT. VA .OR.
     :    ZN .GT. MZ) K=I
9     J=J+2
c     IABC=IABC.OR.K
      iabc=ior3(iabc,k)
10    CONTINUE
      RETURN
      END
c
c
      integer*2 function ior3 (a,b)
      integer*2 a,b
      if(a.eq.0.or.b.eq.0) goto 1
      if(a.eq.b) goto 2
      if(a.eq.3.or.b.eq.3) goto 3
1     ior3=a+b
      return
2     ior3=a
      return
3     ior3=3
      return
      end

