C
C                             MM-7  (LINE 560)
C 8-M
      SUBROUTINE MMRI(IV,IRS)

c      COMMON/MMBC/IVRB(4,89),IRNM(8,56),IRSN(4,17),IPRP(4,17),
c     1IDTN(4,11),ICLS(4,12),RND,IWRD(9,9),IPR(5),
c     2IROM(99),IRES(17),IVEN(107),IXT(360),ITST(40),NAME(3),NTAG(10),
c     3IWP(4)

       include 'common.i'

C
C *** FUNCTIONS ***
C
      IFSD(I)=I-(I/100)*100
      IFD(I)=I/1000-(I/10000)*10
C
C ******* SUBROUTINE I *******
C
C *** SET VARIABLES ***
C
      LU=IPR(1)
      IR=ITST(1)
      LX=ITST(21)
C
   50 IC=0
      DO 1000 J=1,53
      IF(IV.EQ.1.AND.(IVEN(J).GT.10000.OR.IVEN(J)-100*(IVEN(J)/100)
     1.NE.IR))GOTO 1000
      IF(IV.EQ.2.AND.IVEN(J).LT.20000)GOTO 1000
      IF(IV.EQ.4.AND.IRS.NE.J)GOTO 1000
      IF(IV.EQ.3.AND.(IVEN(J).LT.10000.OR.IVEN(J).GT.20000.OR.
     1IFSD(IVEN(J)).NE.IRS))GOTO 1000
C
C *** SKIP ITEMS IN ROOM DESCRIPTION ***
C
      IF(IV.EQ.1.AND.J.EQ.4.AND.(IVEN(4).EQ.493.OR.IVEN(4).EQ.1493))
     1GOTO 1000
      IF(IV.EQ.1.AND.J.EQ.5.AND.IVEN(5).EQ.1316)GOTO 1000
      IF(IV.EQ.1.AND.J.EQ.17.AND.IVEN(17).EQ.1141)GOTO 1000
      IF(IV.EQ.1.AND.J.EQ.19.AND.IR.EQ.16)GOTO 1000
      IF(IV.EQ.1.AND.J.EQ.29.AND.IVEN(29).EQ.1214)GOTO 1000
      IF(IV.EQ.1.AND.J.EQ.30.AND.IR.EQ.25)GOTO 1000
      IF(IV.EQ.1.AND.J.EQ.37.AND.IVEN(37).EQ.1153)GOTO 1000
      IF(IV.EQ.1.AND.J.EQ.39.AND.IVEN(39).EQ.1253)GOTO 1000
      IF(IV.EQ.1.AND.J.EQ.40.AND.IVEN(40).EQ.1151)GOTO 1000
      IF(IV.EQ.1.AND.J.EQ.42.AND.IVEN(42).EQ.1151)GOTO 1000
      IF(IV.EQ.1.AND.J.EQ.46.AND.IVEN(46).EQ.1101)GOTO 1000
      IF(IV.EQ.1.AND.J.EQ.28.AND.IVEN(28).EQ.1184)GOTO 1000
      IF(IV.EQ.1.AND.IC.EQ.0)write(6,100)
      IF(IV.EQ.2.AND.J.EQ.53)GOTO 1000
  100 FORMAT(/" YOU CAN SEE:")
      IF(IV.EQ.5.AND.(J-1)/3.-INT((J-1)/3.).EQ.0)write(6,110)J,IVEN(J)
      IF(IV.EQ.5.AND.(J-1)/3.-INT((J-1)/3.).NE.0)write(6,111)IVEN(J)
  110 FORMAT(I3,I6," ",$)
  111 FORMAT(I6," ",$)
C
      IF(J.EQ.1)write(6,210)
  210 FORMAT(" THE VAMPIRE RING ",$)
C
      IF(J.EQ.2)write(6,220)
  220 FORMAT(" A SET OF KEYS    ",$)
C
      IF(J.EQ.3)write(6,230)
  230 FORMAT(" A WIND UP CLOCK  ",$)
C
      IF(J.EQ.4)write(6,240)
  240 FORMAT(" A BATTERY LANTERN",$)
C
      IF(J.EQ.5)write(6,250)
  250 FORMAT(" A SILVER CROSS   ",$)
C
      IF(J.EQ.6)write(6,260)
  260 FORMAT(" A POCKET COMPASS ",$)
C
      IF(J.EQ.7)write(6,270)
  270 FORMAT(" A PIRATE TREASURE",$)
C
      IF(J.EQ.8)write(6,280)
  280 FORMAT(" A NEW BATTERY    ",$)
C
      IF(J.EQ.9)write(6,290)
  290 FORMAT(" A HAND GUN       ",$)
C
C                             MM-8  (LINE 640)
C 9-M
      IF(J.EQ.10)write(6,300)
  300 FORMAT(" A BUTCHER KNIFE  ",$)
C
      IF(J.EQ.11)write(6,310)
  310 FORMAT(" A TWO BLADED AXE ",$)
C
      IF(J.EQ.12)write(6,320)
  320 FORMAT(" A SWORD          ",$)
C
      IF(J.EQ.13.AND.ITST(10).NE.13)write(6,330)
  330 FORMAT(" A VIAL OF POISON ",$)
C
      IF(J.EQ.13.AND.ITST(10).EQ.13)write(6,335)
  335 FORMAT(" AN EMPTY VIAL    ",$)
C
      IF(J.EQ.14)write(6,340)
  340 FORMAT(" A CLUB           ",$)
C
      IF(J.EQ.15)write(6,350)
  350 FORMAT(" A COIL OF ROPE   ",$)
C
      IF(J.EQ.16)write(6,360)
  360 FORMAT(" A DESK CHAIR     ",$)
C
      IF(J.EQ.17)write(6,370)
  370 FORMAT(" AN UNEVEN AMULET ",$)
C
      IF(J.EQ.18)write(6,380)
  380 FORMAT(" AN OLD TALISMAN  ",$)
C
      IF(J.EQ.19.AND.IFD(IVEN(19)).EQ.1)write(6,390)
  390 FORMAT(" A LIT CANDLE     ",$)
C
      IF(J.EQ.19.AND.IFD(IVEN(19)).NE.1)write(6,395)
  395 FORMAT(" A CANDLE         ",$)
C
      IF(J.EQ.20)write(6,400)
  400 FORMAT(" A SILVER BULLET  ",$)
C
      IF(J.EQ.21)write(6,410)
  410 FORMAT(" A BOOK           ",$)
C
      IF(J.EQ.22)write(6,420)
  420 FORMAT(" A NOTE           ",$)
C
      IF(J.EQ.23)write(6,430)
  430 FORMAT(" A MATTER XMITTER ",$)
C
      IF(J.EQ.24)write(6,440)
  440 FORMAT(" SOME FOOD        ",$)
C
      IF(J.EQ.25)write(6,450)
  450 FORMAT(" A WITCH'S BROOM  ",$)
C
      IF(J.EQ.26)write(6,460)
  460 FORMAT(" A MATTER RECEIVER",$)
C
      IF(J.EQ.27.AND.IFD(IVEN(27)).EQ.1)write(6,470)
  470 FORMAT(" A BURNING TORCH  ",$)
C
      IF(J.EQ.27.AND.IFD(IVEN(27)).NE.1)write(6,475)
  475 FORMAT(" A USELESS TORCH  ",$)
C
      IF(J.EQ.28)write(6,480)
  480 FORMAT(" A SMALL ROAD MAP ",$)
C
      IF(J.EQ.29)write(6,490)
  490 FORMAT(" A WOODEN WEDGE   ",$)
C
      IF(J.EQ.30)write(6,500)
  500 FORMAT(" A DUSTY GLOBE    ",$)
C
      IF(J.EQ.31)write(6,510)
  510 FORMAT(" A KITCHEN MATCH  ",$)
C
      IF(J.EQ.32)write(6,520)
  520 FORMAT(" A RUSTY SHOVEL   ",$)
C
C                             MM-9  (LINE 720)
C 10-M
C
      IF(J.EQ.33)write(6,530)
  530 FORMAT(" A SMALL OILCAN   ",$)
C
      IF(J.EQ.34)write(6,540)
  540 FORMAT(" A GREASY HATCHET ",$)
C
      IF(J.EQ.35)write(6,550)
  550 FORMAT(" A MYSTERY CHEST  ",$)
C
      IF(J.EQ.36)write(6,560)
  560 FORMAT(" A BAG OF PEARLS  ",$)
C
      IF(J.EQ.37)write(6,570)
  570 FORMAT(" AN LARGE EMERALD ",$)
C
      IF(J.EQ.38)write(6,580)
  580 FORMAT(" TWELVE GOLD COINS",$)
C
      IF(J.EQ.39)write(6,590)
  590 FORMAT(" A SILVER GOBLET  ",$)
C
      IF(J.EQ.40)write(6,600)
  600 FORMAT(" SEVERAL DIAMONDS ",$)
C
      IF(J.EQ.41)write(6,610)
  610 FORMAT(" SOME FINE JEWELRY",$)
C
      IF(J.EQ.42)write(6,620)
  620 FORMAT(" A RUBY NECKLACE  ",$)
C
      IF(J.EQ.43)write(6,630)
  630 FORMAT(" A JEWELED CROWN  ",$)
C
      IF(J.EQ.44)write(6,640)
  640 FORMAT(" SOME DARTS       ",$)
C
      IF(J.EQ.45)write(6,650)
  650 FORMAT(" A METAL SHIELD   ",$)
C
      IF(J.EQ.46)write(6,660)
  660 FORMAT(" A DUNGEON KEY    ",$)
C
      IF(J.EQ.47)write(6,670)
  670 FORMAT(" AN OLD BATTERY   ",$)
C
      IF(J.EQ.48)write(6,680)
  680 FORMAT(" A GAUDY GAUNTLET ",$)
C
      IF(J.EQ.49)write(6,690)
  690 FORMAT(" A PARANOID PARROT",$)
C
      IF(J.EQ.50)write(6,700)
  700 FORMAT(" A MENACING MACE  ",$)
C
      IF(J.EQ.51)write(6,710)
  710 FORMAT(" A BLANK          ",$)
C
      IF(J.EQ.52)write(6,720)
  720 FORMAT(" A BLANK          ",$)
C
      IF(J.EQ.53)write(6,730)
  730 FORMAT(" YOUR CLOTHES     ",$)
C
c     IF((IV.NE.5.OR.J/3.-INT(J/3.).EQ..0).AND.LU.NE.6)write(6,900)
      IF((IV.NE.5.OR.J/3.-INT(J/3.).EQ..0))write(6,900) 
  900 FORMAT(2x)
      IC=1
 1000 CONTINUE
      IF((IV.EQ.2.OR.IV.EQ.3).AND.IC.EQ.0)write(6,1010)
 1010 FORMAT(" NOTHING")
      IF(LX.EQ.0.OR.LU.EQ.LX)RETURN
      LU=LX
      GOTO 50
C
      END
C
      subroutine exex(id,iname,i1,i2,i3,i4,i5)

      implicit integer(a-z)
      dimension ip(5),ix(5),iname(3)
      character tmp*6
      write(tmp, '(3a2)') iname
      call exexc(tmp, i1,i2,i3,i4,i5)

      end
C                             MM-10 (LINE 800)
C 11-M
      SUBROUTINE MMRL
c      COMMON/MMBC/IWRD(9,9),IPR(5),IVRB(4,89),IRNM(8,56),IROM(99),
c     1IRSN(4,17),IRES(17),IPRP(4,17),IDTN(4,11),IVEN(107),
c     2IXT(360),ICLS(4,12),ITST(40),NAME(3),RND,NTAG(10)

       include 'common.i'

C
C *** FUNCTIONS ***
C
      IFSD(I)=I-(I/100)*100
      IFD(I)=I/1000-(I/10000)*10
C
C ******* SUBROUTINE L *******
C
C *** SET VARIABLES ***
C
      LU=IPR(1)
      IR=ITST(1)
C
C *** IS IT LIGHT HERE ***
C
C ** IS THERE A LIGHT SOURCE **
C
C * LANTERN, CANDLE OR TORCH ON AND HERE OR WITH PLAYER OR INHABITANT *
C
      DO 1153 I=1,3
C
      IF(I.EQ.1)K=4
      IF(I.EQ.2)K=19
      IF(I.EQ.3)K=27
      IF(IFD(IVEN(K)).EQ.1.AND.((IVEN(K).LT.10000.AND.IFSD(IVEN(K))
     1.EQ.IR).OR.IVEN(K).GT.20000.OR.(IVEN(K).GT.10000.AND.IVEN(K)
     2.LT.20000.AND.IFSD(IRES(IFSD(IVEN(K)))).EQ.IR))) RETURN
C
 1153 CONTINUE
C
C * LAMP *
C
      IF ((IR .EQ. 10) .AND. (IVEN(76) .GT. 1000)) RETURN
C
C * FIREPLACE *
C
      IF ((IR .EQ. 14) .AND. (IVEN(89) .GT. 1000)) RETURN
C
C * MATCH *
C
      IF (IVEN(31) .GT. 21000) RETURN
C
C ** IS HE OUTSIDE AND DAYTIME **
C
      IF((IR .GT. 53) .AND. (ITST(5) .LT. 300)) RETURN
      IF (IR .GT. 53) CALL exex(8,NAME,LU,1150,1,0,0)
C
C ** IS THERE AN OPEN CURTAIN **
C
      IF ((IROM(IR) .GT. 20000) .AND. (ITST(5) .LT. 300)) RETURN
      IF (IROM(IR) .GT. 20000) CALL exex(8,NAME,LU,1150,1,0,0)
      IF((ITST(5) .LT. 300) .AND. (IROM(IR) .GT. 10000))
     1CALL exex(8,NAME,LU,1150,2,0,0)
      CALL exex(8,NAME,LU,1150,0,0,0)
C
      END

