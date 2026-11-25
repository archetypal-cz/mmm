C     Shared F77 helpers, compiled by both builds (gfortran and f2c).
C
C     REIOS: REIO(2,...) called with a text literal.  The original passes a
C     CHARACTER constant where REIO expects an INTEGER*2 buffer; that works
C     by accident on x86 but breaks WebAssembly's strict call signatures.
      SUBROUTINE REIOS(CODE,LU,TEXT,KOLIK)
      INTEGER*2 CODE,LU,KOLIK
      CHARACTER*(*) TEXT
      WRITE(*,1) TEXT(1:2*KOLIK)
1     FORMAT(' ',A)
      RETURN
      END
