$storage:2
$nodebug
      SUBROUTINE SGET(BUF,KDE,CO)
C     22-06-1988 MMLB.FOR SM
      INTEGER*2 BUF(1),KDE,CO,K,I
      CHARACTER*2 ZNAK
      EQUIVALENCE (ZNAK,K)
C
      IF(KDE.LT.1) RETURN
      I=KDE/2
      IF(I*2.EQ.KDE) THEN
          K=BUF(I)
          CO=ZNAK(2:2)
          CO=CO-8192
      ELSE
          K=BUF(I+1)
          CO=ZNAK(1:1)
          CO=CO-8192
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE SPUT(BUF,KDE,CO)
C     22-06-1988 MMLB.FOR SM
      INTEGER*2 BUF(1),KDE,CO,I,K
      CHARACTER*2 ZNAK
      EQUIVALENCE (ZNAK,K)
C
      IF(KDE.LT.1) RETURN
      I=KDE/2
      IF(I*2.EQ.KDE) THEN
          K=BUF(I)
          ZNAK(2:2)=CHAR(CO)
          BUF(I)=K
      ELSE
          K=BUF(I+1)
          ZNAK(1:1)=CHAR(CO)
          BUF(I+1)=K
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE SMOVE(RETEZ,ODKUD,POKUD,VYSLED,KAM)
C     19-04-1987 MMLB.FOR SM
      INTEGER*2 RETEZ(1),ODKUD,POKUD,VYSLED(1),KAM,I,J,K
C
      IF(POKUD.LT.ODKUD) RETURN
      J=KAM
      DO 1 I=ODKUD,POKUD
          CALL SGET(RETEZ,I,K)
          CALL SPUT(VYSLED,J,K)
1         J=J+1
      RETURN
      END
C
C
      INTEGER*2 FUNCTION JSTR(RETEZ,ODKUD,POKUD,PODRET,ZACAT,KONEC)
C     19-04-1987 MMLB.FOR SM
      INTEGER*2 RETEZ(1),ODKUD,POKUD,PODRET(1),ZACAT,KONEC,I,J,K,L,Z1,Z2
C
      JSTR=-1
      IF(POKUD.LT.ODKUD) RETURN
      IF(KONEC.LT.ZACAT) RETURN
      IF(KONEC-ZACAT.GT.POKUD-ODKUD) RETURN
      K=(POKUD-ODKUD)-(KONEC-ZACAT)+1
      DO 1 I=1,K
      DO 2 J=ZACAT,KONEC
      L=ODKUD+I-1+J-1
      CALL SGET(PODRET,J,Z1)
      CALL SGET(RETEZ,L,Z2)
      IF(Z1.NE.Z2) GOTO 1
2     CONTINUE
      JSTR=ODKUD+I-1
      RETURN
1     CONTINUE
      RETURN
      END
C
C
      INTEGER*2 FUNCTION JSCOM(RETEZ,ODKUD,POKUD,STRING,KDE,IER)
C     19-04-1987 MMLB.FOR SM
      INTEGER*2 RETEZ(1),ODKUD,POKUD,STRING(1),KDE,I,J,Z1,Z2,IER
C
      JSCOM=0
      IF(POKUD.LT.ODKUD) RETURN
      J=KDE-1
      DO 1 I=ODKUD,POKUD
      J=J+1
      CALL SGET(RETEZ,I,Z1)
      CALL SGET(STRING,J,Z2)
      CALL CZECH(Z1,Z1,1)
      CALL CZECH(Z2,Z2,1)
      IF(Z1-Z2) 2,1,3
1     CONTINUE
      RETURN
2     JSCOM=-1
      RETURN
3     JSCOM=1
      RETURN
      END
C
C
      SUBROUTINE REIO(CODE,LU,BUF,KOLIK)
C     19-04-1987 MMLB.FOR SM
      INTEGER*2 CODE,LU,BUF(1),KOLIK,I
C
      IF(CODE.EQ.1) THEN
          DO 10 I=1,36
10        BUF(I)='  '
          READ(0,1)(BUF(I),I=1,36)
          DO 11 KOLIK=72,1,-1
              CALL SGET(BUF,KOLIK,I)
              IF(I.NE.32) GOTO 12
11        CONTINUE
          KOLIK=1
          GOTO 13
12        KOLIK=(KOLIK+1)/2
      ELSE
          WRITE(0,2)(BUF(I),I=1,KOLIK)
      ENDIF
1     FORMAT(40A2)
2     FORMAT(' ',40A2)
13    RETURN
      END
C
C
      SUBROUTINE SGLOD(NAME,P1,P2,P3,P4,P5)
C     19-04-1987 MMLB.FOR SM
      INTEGER*2 NAME(1),P1,P2,P3,P4,P5
C
      IF(NAME(2).EQ.'SB') THEN
          CALL MMSB(P1,P2,P3,P4,P5)
      ELSEIF(NAME(2).EQ.'SA') THEN
          CALL MMSA(P1,P2,P3,P4,P5)
      ELSEIF(NAME(2).EQ.'SC') THEN
          CALL MMSC(P1,P2,P3,P4,P5)
      ELSEIF(NAME(2).EQ.'SD') THEN
          CALL MMSD(P1,P2,P3,P4,P5)
      ELSEIF(NAME(2).EQ.'SE') THEN
          CALL MMSE(P1,P2,P3,P4,P5)
      ELSEIF(NAME(2).EQ.'SF') THEN
          CALL MMSF(P1,P2,P3,P4,P5)
      ELSEIF(NAME(2).EQ.'SG') THEN
          CALL MMSG(P1,P2,P3,P4,P5)
      ELSEIF(NAME(2).EQ.'SH') THEN
          CALL MMSH(P1,P2,P3,P4,P5)
      ELSEIF(NAME(2).EQ.'SI') THEN
          CALL MMSI(P1,P2,P3,P4,P5)
      ELSEIF(NAME(2).EQ.'SJ') THEN
          CALL MMSJ(P1,P2,P3,P4,P5)
      ELSEIF(NAME(2).EQ.'SK') THEN
          CALL MMSK(P1,P2,P3,P4,P5)
*     ELSEIF(NAME(2).EQ.'SL') THEN
*         CALL MMSL(P1,P2,P3,P4,P5)
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE CLCUC(BUF,LENG)
      INTEGER*2 BUF(1),LENG,K,I,J,MALA(15),VELKA(15)
      DATA MALA /160,159,212,130,216,161,229,162,253,231,156,163,133,
     +           236,167/
      DATA VELKA/181,172,210,144,183,214,213,224,252,230,155,233,222,
     +           237,166/
      IF(LENG.LT.1) RETURN
      DO 1 I=1,LENG
          CALL SGET(BUF,I,K)
          IF(K.GE.128.AND.K.LE.255) THEN
              DO 2 J=1,15
                  IF(K.EQ.MALA(J)) THEN
                      K=VELKA(J)
                      CALL SPUT(BUF,I,K)
                      GOTO 1
                  ENDIF
2             CONTINUE
              GOTO 1
          ENDIF
          IF(K.LT.97.OR.K.GT.122) GOTO 1
          K=K-32
          CALL SPUT(BUF,I,K)
1     CONTINUE
      RETURN
      END
C
C
      INTEGER*2 FUNCTION KCVT (CISLO)
      INTEGER*2 CISLO,I,J,K(1)
      IF(CISLO.GT.99) RETURN
      I=CISLO/10
      J=CISLO-I*10
      IF(I.EQ.0) THEN
          CALL SPUT(K,1,32)
      ELSE
          CALL SPUT(K,1,48+I)
      ENDIF
      CALL SPUT(K,2,48+J)
      KCVT=K(1)
      RETURN
      END
C
C
      SUBROUTINE CZECH(CESKY,ASCII,DELKA)
      INTEGER*2 CESKY(1),ASCII(1),DELKA,TAB(2,15),I,J,K
      DATA TAB/181,65,
     +         172,67,
     +         210,68,
     +         144,69,
     +         183,69,
     +         214,73,
     +         213,78,
     +         224,79,
     +         252,82,
     +         230,83,
     +         155,84,
     +         133,85,
     +         222,85,
     +         237,89,
     +         166,90/
C  Procedura prevadi velky cesky text v poli CESKY na ascii text v poli ASCII.
C  Pocet znaku je DELKA.
C
      IF(DELKA.LT.1) RETURN
      DO 1 I=1,DELKA
      CALL SGET(CESKY,I,K)
      DO 2 J=1,15
      IF(K.NE.TAB(1,J)) GOTO 2
      CALL SPUT(ASCII,I,TAB(2,J))
      GOTO 1
2     CONTINUE
      CALL SPUT(ASCII,I,K)
1     CONTINUE
      RETURN
      END
C
C
      LOGICAL FUNCTION ROVNA(SLOVO1,SLOVO2)
      INTEGER*2 SLOVO1,SLOVO2,S1,S2,K1,K2,L1,L2
      ROVNA=.FALSE.
      CALL CZECH(SLOVO1,S1,2)
      CALL CZECH(SLOVO2,S2,2)
      CALL SGET(S1,1,K1)
      CALL SGET(S2,1,L1)
      CALL SGET(S1,2,K2)
      CALL SGET(S2,2,L2)
      IF(K1.EQ.L1.AND.K2.EQ.L2) ROVNA=.TRUE.
      RETURN
      END
C
C
C     interface to subroutine time (n,str)
C     character*10 str [near,reference]
C     integer*2 n [value]
C     end
C
C
C     subroutine cas(leng,buf)
C     character*10 tstr
C     integer*2 leng,buf(5),tm(5),i,j
C     equivalence (tstr,tm(1))
C     call time(leng,tstr)
C     buf(1)=0
C     call sget(tm,1,i)
C     call sget(tm,2,j)
C     buf(4)=(i-48)*10+(j-48)
C     call sget(tm,4,i)
C     call sget(tm,5,j)
C     buf(3)=(i-48)*10+(j-48)
C     call sget(tm,7,i)
C     call sget(tm,8,j)
C     buf(2)=(i-48)*10+(j-48)
C     buf(5)=0
C     return
C     end
C
C
      subroutine cas(leng,buf)
      integer*2 buf(5),leng
      call gettim(buf(4),buf(3),buf(2),buf(1))
      return
      end
c
c
*     interface to integer*2 function spawn
*    +          [c,varying,alias:'spawnlp']
*    +          (mode)
*    integer*2 mode
*     end
C
C
*$large
*      subroutine mmread ( )
*      character*64 fname
*$include:'mmco.for'
*1     write(*,'('' Jmeno souboru se zapsanou hrou :''\)')
*      read(*)fname
*      open(10,file=fname,status='old',form='binary',err=1)
*      read(10)iwrd,ivrb,irnm,irom
*      read(10)irsn,ires,iprp,idtn,iven
*      read(10)ixt,icls,itst,name,rnd,ntag,iwp
*      read(10)klice,idelky,identk,ipr
*      close(10)
*      return
*      end
C
C
*      subroutine mmwrite ( )
*      character*64 fname
*$include:'mmco.for'
*1     write(*,'('' Jmeno souboru pro zapis hry :''\)')
*      read(*)fname
*      open(10,file=fname,status='new',form='binary',err=1)
*      write(10)iwrd,ivrb,irnm,irom
*      write(10)irsn,ires,iprp,idtn,iven
*      write(10)ixt,icls,itst,name,rnd,ntag,iwp
*      write(10)klice,idelky,identk,ipr
*      close(10)
*      return
*      end
