C
C                             MM-11 (LINE 880)
C 1-A
      subroutine MMSA

      include 'common.i'
      character*8 chvrb(89)	
      equivalence(ivrb, chvrb)
C
C ******* SEGMENT A *******
C
C      THIS SEGMENT INITIATES THE GAME BY GENERATING THE MYSTERY, MAGIC
C      WORDS, ETC. THIS SEGMENT IS ONLY ENTERED ONCE EXCEPT TO
C      CHANGE THE MYSTERY ON THE FIRST MOVE.
C
      DIMENSION ITIM(5)
      IF(RND.NE.2.)GOTO 1000
C
C ****** INDEX ******                                    PAGE
C
C      A. SET UP MYSTERY                                 A-3
C         1. PICK SCENE
C         2. PICK MURDERER
C         3. PICK WEAPON
C
C      B. SET UP RANDOM ITEMS                            A-4
C         1. WELLS
C         2. BATTERY
C         3. MOLE
C
C      C. GENERATE MAGIC WORDS                           A-4
C
C ***** SET UP FIRST RANDOM VARIABLE *****
C
      CALL EXEC(11,ITIM)
      RND=(ITIM(2)*ITIM(3)+10)/3600.
      IM=1000*RND
      ITST(23)=IM
      RND=.001*IM
      LU=IPR(1)
      ITST(21)=IPR(3)
      LX=IPR(3)
      IF(IPR(4).NE.0)ITST(38)=IPR(4)
      ITST(30)=ITST(30)+3*ITST(29)
C
C **** WRITE INTRODUCTION ****
C
   10 WRITE(6,100)NTAG,IM,ITST(15)
  100 FORMAT(/"HI "10A2//" WELCOME TO MYSTERY MANSON.   ",
     !"MYSTERY"I4".    REVISION"I3" - 23 JUL 81."/" A COMPUTERIZED ",
C
C ** THE REVISION LEVEL IS CHANGED WHENEVER AN OLD SUSPENDED GAME
C    WILL NOT WORK THE SAME OR WHEN THE GAME MIGHT NOT APPEAR TO BE
C    THE SAME AS BEFORE. OTHERWISE ONLY THE DATE IS CHANGED.
C
     2"FANTASY, COMPLIMENTS OF BILL WOLPERT-NUWES KEYPORT,WA."//
     3" THE ELEMENTS OF MYSTERY MANSION ARE BASED ON THE ",
     4"FACTS, FICTIONS AND"/" FANTASIES OF THE PAST, PRESENT AND ",
     5"FUTURE. THE SCENARIO WAS DESIGNED"/" TO CHALLENGE THE DARING ",
     6"AND YET, ENTERTAIN THE CURIOUS. ESCAPE FOR A"/" FEW MOMENTS ",
     7"AND EXPERIENCE FRUSTRATION AND TRIUMPH, HOPE AND DESPAIR,"/
     8" POWER, LUST AND GREED. USING WHAT YOU KNOW, WHAT YOU CAN ",
     9"FIND OUT AND"/" WHAT WORKS; TELL THE COMPUTER WHAT YOU WANT ",
     1"OR THINK YOU SHOULD DO."//
     1" YOU ARE IN FRONT OF A HEAVY IRON GATE WHICH IS APPARENTLY THE ",
     2"ONLY"/" WAY THROUGH A HIGH BRICK WALL PROTECTING AN OLD MANSION,"
     3" JUST VISIBLE"/" THROUGH THE GATE. A ROAD LEADS TO THE EAST AND "
     4" WEST ALONG THE WALL."/" IT IS DAWN AND A THIN LOW FOG IS JUST ",
     5"CLEARING FROM THE COOL AREAS."/" BEHIND YOU TO THE SOUTH ",
     5"ON THE OTHER SIDE OF THE ROAD IS A HIGHWAY"/" GOING SOUTH AS ",
     6"FAR AS YOU CAN SEE. YOU CAN JUST SEE THE TAXI THAT"/
     7" DROPPED YOU OFF, DRIVING OUT OF SIGHT.")
C
C                            MM-A-2  (LINE 1040)
C 3-A
C *** ADDITIONAL TIME ***
C
      ITST(18)=ITIM(4)*1200+ITIM(3)*20+ITIM(2)/3
      ITST(19)=100*ITIM(4)+ITIM(3)
C
C ***** SET UP MYSTERY *****
C
C *** PICK ROOM ***
C
 1000 RND=RND*7.7-INT(RND*7.7)
      IC=INT(RND*77)+1
      IF ((( IC .GT. 9) .AND. (IC .LT. 19)) .OR. ((IC .GT. 27)
     1.AND. (IC .LT. 54))) GOTO 1000
      ITST(8)=IC
C *** PICK MURDERER ***
C
      RND=RND*7.7-INT(RND*7.7)
      ID=INT(RND*4)
      IF (ID .EQ. 0) IE=7
      IF (ID .EQ. 1) IE=8
      IF (ID .EQ. 2) IE=9
      IF (ID .EQ. 3) IE=2
      IF((IC .LT. 28) .AND. (ID .EQ. 0)) IE=3
      IF((IC .LT. 28) .AND. (ID .EQ. 1)) IE=8
      IF((IC .LT. 28) .AND. (ID .EQ. 2)) IE=14
      IF((IC .LT. 28) .AND. (ID .EQ. 3)) IE=2
      IF((IC .LT. 10) .AND. (ID .EQ. 0)) IE=3
      IF((IC .LT. 10) .AND. (ID .EQ. 1)) IE=7
      IF((IC .LT. 10) .AND. (ID .EQ. 2)) IE=9
      IF((IC .LT. 10) .AND. (ID .EQ. 3)) IE=14
      ITST(9)=IE
C
C *** PICK WEAPON ***
C
      RND=RND*7.7-INT(RND*7.7)
      IF=INT(RND+.5)
      RND=RND*7.7-INT(RND*7.7)
      IF((IE .EQ. 3) .AND. (IF .EQ. 0)) IG=INT(RND*9)+1
      IF((IE .EQ. 3) .AND. (IF .EQ. 1)) IG=INT(RND*9)+19
      IF((IE .EQ. 7) .AND. (IF .EQ. 0)) IG=INT(RND*9)+1
      IF((IE .EQ. 7) .AND. (IF .EQ. 1)) IG=INT(RND*24)+54
      IF((IE .EQ. 8) .AND. (IF .EQ. 0)) IG=INT(RND*9)+19
      IF((IE .EQ. 8) .AND. (IF .EQ. 1)) IG=INT(RND*24)+54
      IF((IE .EQ. 9) .AND. (IF .EQ. 0)) IG=INT(RND*9)+1
      IF((IE .EQ. 9) .AND. (IF .EQ. 1)) IG=INT(RND*24)+54
      IF((IE .EQ. 14) .AND. (IF .EQ. 0)) IG=INT(RND*9)+1
      IF((IE .EQ. 14) .AND. (IF .EQ. 1)) IG=INT(RND*9)+19
      IF((IE .EQ. 2) .AND. (IF .EQ. 0)) IG=INT(RND*9)+19
      IF((IE .EQ. 2) .AND. (IF .EQ. 1)) IG=INT(RND*24)+54
C
      RND=RND*7.7-INT(RND*7.7)
      IVEN(INT(RND*6)+9)=(IVEN(INT(RND*6)+9)/100)*100+IG
      ITST(10)=INT(RND*6)+9
      IF(INT(RND*6) .EQ. 0) ITST(11)=5
C
C *** PICK ROOM FOR CORPSE ***
C
      IF(ITST(10) .GT. 12) IRES(1)=10000+IC
      IF(ITST(10) .GT. 12) GOTO 2000
      RND=RND*7.7-INT(RND*7.7)
      IF=INT(RND+.5)
      RND=RND*7.7-INT(RND*7.7)
      IF((IE .EQ. 3) .AND. (IF .EQ. 0)) IG=INT(RND*9)+1
      IF((IE .EQ. 3) .AND. (IF .EQ. 1)) IG=INT(RND*9)+19
      IF((IE .EQ. 7) .AND. (IF .EQ. 0)) IG=INT(RND*9)+1
      IF((IE .EQ. 7) .AND. (IF .EQ. 1)) IG=INT(RND*24)+54
      IF((IE .EQ. 8) .AND. (IF .EQ. 0)) IG=INT(RND*9)+19
      IF((IE .EQ. 8) .AND. (IF .EQ. 1)) IG=INT(RND*24)+54
      IF((IE .EQ. 9) .AND. (IF .EQ. 0)) IG=INT(RND*9)+1
      IF((IE .EQ. 9) .AND. (IF .EQ. 1)) IG=INT(RND*24)+54
      IF((IE .EQ. 14) .AND. (IF .EQ. 0)) IG=INT(RND*9)+1
      IF((IE .EQ. 14) .AND. (IF .EQ. 1)) IG=INT(RND*9)+19
      IF((IE .EQ. 2) .AND. (IF .EQ. 0)) IG=INT(RND*9)+19
      IF((IE .EQ. 2) .AND. (IF .EQ. 1)) IG=INT(RND*24)+54
      IRES(1)=10000+IG
C
C                            MM-A-3  (LINE 1120)
C 4-A
C
C ***** SET UP WELLS *****
C
 2000 IC=INT(RND*3)+78
      IXT(326)=75+100*IC
      IF (IC .EQ. 79) IXT(266)=7579
      IC=IC+1
      IF (IC .EQ. 81) IC=78
      IXT(327)=76+100*IC
      IF(IC .EQ. 79) IXT(266)=7679
      IC=IC+1
      IF (IC .EQ. 81) IC=78
      IXT(328)=77+100*IC
      IF (IC .EQ. 79) IXT(266)=7779
C
C ***** SET UP BATTERY *****
C
      RND=RND*7.7-INT(RND*7.7)
      IVEN(8)=200+INT(RND*6+29)
C
C ***** SET UP MOLE *****
C
      RND=RND*7.7-INT(RND*7.7)
      IRES(4)=INT(RND*9+43)
C
C ***** SET UP MAGIC WORDS *****
C
C *** SET UP LETTERS THAT ARE REVERSIBLE ***
C
      IWRD(1,1)=1HM
      IWRD(2,1)=1HI
      IWRD(3,1)=1HW
      IWRD(4,1)=1HO
      IWRD(5,1)=1HX
      IWRD(1,2)=1HM
      IWRD(2,2)=1HI
      IWRD(3,2)=1HW
      IWRD(4,2)=1HO
      IWRD(5,2)=1HH
C
C *** JUGGLE LETTERS RANDOMLY ***
C
      DO 3300 I=1,2
      RND=RND*7.7-INT(RND*7.7)
      DO 3300 J=1,INT(RND*10)+5
      RND=RND*7.7-INT(RND*7.7)
      IF (RND .LT. .5) GOTO 3100
      IC=IWRD(1,I)
      IWRD(1,I)=IWRD(3,I)
      IWRD(3,I)=IC
 3100 RND=RND*7.7-INT(RND*7.7)
      IF (RND .LT. .5) GOTO 3200
      IC=IWRD(2,I)
      IWRD(2,I)=IWRD(4,I)
      IWRD(4,I)=IC
 3200 RND=RND*7.7-INT(RND*7.7)
      IF (RND .LT. .5) GOTO 3300
      IC=IWRD(5,1)
      IWRD(5,1)=IWRD(5,2)
      IWRD(5,2)=IC
 3300 CONTINUE
      RND=RND*7.7-INT(RND*7.7)
      IF (RND .LT. .5) GOTO 3400
      IC=IWRD(1,1)
      IWRD(1,1)=IWRD(5,1)
      IWRD(5,1)=IC
 3400 RND=RND*7.7-INT(RND*7.7)
      IF (RND .LT. .5) GOTO 3500
      IC=IWRD(1,2)
      IWRD(1,2)=IWRD(5,2)
      IWRD(5,2)=IC
 3500 CONTINUE

C
C                            MM-A-4  (LINE 1200)
C 5-A
C *** CREATE WORDS FROM LETTERS ***
C
C * 1=NORMAL 2=REVERSE 3=UPSIDE DOWN 4=REVERSE AND UPSIDE DOWN *
C
c
c  following KJC to eliminate endian problems

      do i=1, 2
          write(chvrb(4*i),'(5a1)') (char(iwrd(ix,i)), ix=1,5)  
          write(chvrb(4*i+1),'(5a1)') (char(iwrd(ix,i)), ix=5,1,-1)

         if ((iwrd(1,i) .eq. ichar('W')) .or.
     +       (iwrd(1,i) .eq. ichar('M'))) then
              write(chvrb(4*i+2),'(5a1)') char(iwrd(3,i)),
     +          char(iwrd(2,i)), char(iwrd(1,i)),
     +          char(iwrd(4,i)), char(iwrd(5,i))
              write(chvrb(4*i+3),'(5a1)') char(iwrd(5,i)),
     +          char(iwrd(4,i)), char(iwrd(1,i)),
     +          char(iwrd(2,i)), char(iwrd(3,i))
          else
              write(chvrb(4*i+2),'(5a1)') char(iwrd(1,i)),
     +          char(iwrd(2,i)), char(iwrd(5,i)),
     +          char(iwrd(4,i)), char(iwrd(3,i))
              write(chvrb(4*i+3),'(5a1)') char(iwrd(3,i)),
     +          char(iwrd(4,i)), char(iwrd(5,i)),
     +          char(iwrd(2,i)), char(iwrd(1,i))
          endif
      enddo


c Here's the original code for above
c
c     DO 3700 I=1,2
c     IVRB(1,4*I)=IWRD(1,I)-32+(IWRD(2,I)-32)/256
c     IVRB(2,4*I)=IWRD(3,I)-32+(IWRD(4,I)-32)/256
c     IVRB(3,4*I)=IWRD(5,I)
c     IVRB(1,4*I+1)=IWRD(5,I)-32+(IWRD(4,I)-32)/256
c     IVRB(2,4*I+1)=IWRD(3,I)-32+(IWRD(2,I)-32)/256
c     IVRB(3,4*I+1)=IWRD(1,I)
c     IF ((IWRD(1,I) .NE. 1HW) .AND. (IWRD(1,I) .NE. 1HM)) GOTO 3600
c     IF ((IWRD(1,I) .NE. 87) .AND. (IWRD(1,I) .NE. 77)) GOTO 3600
c     IVRB(1,4*I+2)=IWRD(3,I)-32+(IWRD(2,I)-32)/256
c     IVRB(2,4*I+2)=IWRD(1,I)-32+(IWRD(4,I)-32)/256
c     IVRB(3,4*I+2)=IWRD(5,I)
c     IVRB(1,4*I+3)=IWRD(5,I)-32+(IWRD(4,I)-32)/256
c     IVRB(2,4*I+3)=IWRD(1,I)-32+(IWRD(2,I)-32)/256
c     IVRB(3,4*I+3)=IWRD(3,I)
c     GOTO 3700
c3600 IVRB(1,4*I+2)=IWRD(1,I)-32+(IWRD(2,I)-32)/256
c     IVRB(2,4*I+2)=IWRD(5,I)-32+(IWRD(4,I)-32)/256
c     IVRB(3,4*I+2)=IWRD(3,I)
c     IVRB(1,4*I+3)=IWRD(3,I)-32+(IWRD(4,I)-32)/256
c     IVRB(2,4*I+3)=IWRD(5,I)-32+(IWRD(2,I)-32)/256
c     IVRB(3,4*I+3)=IWRD(1,I)
c3700 CONTINUE

c      do ixxx = 4, 10
c	write(*,'("ivrb(",i2,") ",4a2)') ixxx, (ivrb(ixx, ixxx), ixx=1,4)
c      enddo	

C
C *** SET WEAPON NAME ***
C
      data it_bb/2h  /

      IWP(3)=it_bb
      IWP(4)=it_bb

      data it_gu/2hGU/, it_n /2hN /, it_kn/2hKN/, it_if/2hIF/
      data it_e /2hE /, it_ax/2hAX/, it_sw/2hSW/, it_or/2hOR/
      data it_d /2hD /, it_vi/2hVI/, it_al/2hAL/, it_cl/2hCL/
      data it_ub/2hUB/
      
      IF(ITST(10).EQ.9)IWP(1)=it_GU
      IF(ITST(10).EQ.9)IWP(2)=it_N
      IF(ITST(10).EQ.10)IWP(1)=it_KN
      IF(ITST(10).EQ.10)IWP(2)=it_IF
      IF(ITST(10).EQ.10)IWP(3)=it_E
      IF(ITST(10).EQ.11)IWP(1)=it_AX
      IF(ITST(10).EQ.11)IWP(2)=it_E
      IF(ITST(10).EQ.12)IWP(1)=it_SW
      IF(ITST(10).EQ.12)IWP(2)=it_OR
      IF(ITST(10).EQ.12)IWP(3)=it_D
      IF(ITST(10).EQ.13)IWP(1)=it_VI
      IF(ITST(10).EQ.13)IWP(2)=it_AL
      IF(ITST(10).EQ.14)IWP(1)=it_CL
      IF(ITST(10).EQ.14)IWP(2)=it_UB
C
C *** PICK WEREWOLF ***
C
 3800 RND=RND*7.7-INT(RND*7.7)
      IC=INT(RND*6+2)
      IF(IC.EQ.4)IC=8
      IF(IC.EQ.5)IC=9
      IF(IC.EQ.6)IC=14
      IF(IC.EQ.ITST(9))GOTO 3800
      ITST(39)=IC
C

 3810 NAME(2)=is_SB
      CALL exex(8,NAME,0,0,0,0,0)
 9999 CONTINUE
      NAME(2)=is_SD
      CALL exex(8,NAME,LU,11111,0,0,0)
      END


