      Subroutine exec (a,b,c,d,e)
      common/mypar/p1,p2,p3
      INTEGER*2 p1,p2,p3
      Integer*2 a,c,d,e
      integer*2 b(3)
      integer*2 x(3)
      character*6 bb
      equivalence (bb,x)
      do 111 i=1,3
      x(i)=b(i)
111   continue 
      p1=c
      p2=d
      p3=e
      if (bb .eq. 'MMSA  ') call MMSA
      if (bb .eq. 'MMSB  ') call MMSB
      if (bb .eq. 'MMSC  ') CALL MMSC
      if (bb .eq. 'MMSD  ') CALL MMSD
      if (bb .eq. 'MMSE  ') CALL MMSE         
      if (bb .eq. 'MMSF  ') CALL MMSF
      if (bb .eq. 'MMSG  ') CALL MMSG         
      if (bb .eq. 'MMSH  ') CALL MMSH
      if (bb .eq. 'MMSI  ') CALL MMSI
      if (bb .eq. 'MMSJ  ') CALL MMSJ
      if (bb .eq. 'MMSK  ') CALL MMSK
      stop
      Return
      end

      Subroutine rmpar (ivect)
      common/mypar/p1,p2,p3
      INTEGER*2 p1,p2,p3
      Integer*2 Ivect(5)
      ivect(1)=p1
      ivect(2)=p2
      ivect(3)=p3
      ivect(4)=0
      ivect(5)=0
      return
      end

      Integer*2 function freeze (IVRBX,IROM,IRES,IVEN,IXT,ITST,IWP)
      Integer*2 IVRBX(4,*),IROM(99),IRES(17),IVEN(107),IXT(360)
      Integer*2 ITST(40),IWP(4)
      Integer*2 IBUFF(700)
      character*80 sname
      write(6,552) 'Filename ? '
 552  format (1xa$)
      read (5,553) sname
 553  format (a80)
      open (unit=16,file=sname,status='new',form='unformatted',
     $     recordtype='fixed',RECORDSIZE= 128,err=554)
      DO 555 I=1,8                                                    
      DO 555 J=1,4                                                    
 555  IBUFF(4*i+j)=IVRBX(J,I)  
      do 556 i=1,99
 556  ibuff(32+i)=irom(i)
      do 557 i=1,17
 557  ibuff(131+i)=ires(i)
      do 558 i=1,107
 558  ibuff(148+i)=iven(i)
      do 559 i=1,360
 559  ibuff(255+i)=ixt(i)
      do 560 i=1,40
 560  ibuff(615+i)=itst(i)
      do 561 i=1,4
 561  ibuff(655+i)=iwp(i)
      write(16) (ibuff(i),i=1,256)
      write(16) (ibuff(i),i=257,512)
      write(16) (ibuff(i),i=513,659)
      close(16)
      freeze=-2
      return 
 554  Write (6,*) ' Error saving file'
      freeze=2
      return
      end

      Integer*2 function thaw (IVRBX,IROM,IRES,IVEN,IXT,ITST,IWP)
      Integer*2 IVRBX(4,*),IROM(99),IRES(17),IVEN(107),IXT(360)
      Integer*2 ITST(40),IWP(4)
      Integer*2 IBUFF(700)
      character*80 sname
      write(6,552) 'Filename ? '
 552  format (1xa$)
      read (5,553) sname
 553  format (a80)
      open (unit=16,file=sname,status='old',form='unformatted',
     $     recordtype='fixed',RECORDSIZE= 128,err=554)
      read(16) (ibuff(i),i=1,256)
      read(16) (ibuff(i),i=257,512)
      read(16) (ibuff(i),i=513,659)
      close(16)

      DO 555 I=1,8                                                    
      DO 555 J=1,4                                                    
 555  IVRBX(J,I)=IBUFF(4*I+J)  
      do 556 i=1,99
 556  irom(i)=ibuff(32+i)
      do 557 i=1,17
 557  ires(i)=ibuff(131+i)
      do 558 i=1,107
 558  iven(i)=ibuff(148+i)
      do 559 i=1,360
 559  ixt(i)=ibuff(255+i)
      do 560 i=1,40
 560  itst(i)=ibuff(615+i)
      do 561 i=1,4
 561  iwp(i)=ibuff(655+i)
      thaw=-2
      return 
 554  Write (6,*) ' Error restoring file'
      thaw=2
      return
      end

      Subroutine reio (ia,b)
      INTEGER*2 IA(72)
      INTEGER*2 B
      CHARACTER*144 A
      INTEGER*2 JA(72)
      equivalence (A,JA)
      read (5,551) a
 551  format (a144)
      DO 100 I=1,72
       ia(i) = iand( JA(I),57311)         
       if (ia(i) .eq. 0) ia(i) = 8224
       if (iand (ia(i),255) .eq. 0) ia(i)=ia(i)+32
       if (iand (ia(i),65280) .eq. 0) ia(i)=ia(i)+32*256
 100  CONTINUE
      return
      end

      Integer*2 function moveas (a)
      integer*2 a
      moveas = a
      return
      end

      Subroutine ourtime (iarray)
      integer*2 iarray(5)
      character*8 itime
      character*1 dummy
      call time(itime)
      iarray(1)=0
      read(itime,561) iarray(4),dummy,iarray(3),dummy,iarray(2)
 561  format (i2,a,i2,a,i2) 
      iarray(5)=0
      return
      end
