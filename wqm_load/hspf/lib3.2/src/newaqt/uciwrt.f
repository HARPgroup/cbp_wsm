C
C
C
      SUBROUTINE   UCIWRT
     I                   (MESSFL,OUTFL,EMFG)
C
C     + + + PURPOSE + + +
C     Write out a UCI file to disk.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,OUTFL,EMFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     OUTFL  - Fortran unit number for output UCI file
C     EMFG   - english/metric units flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      LTYPRC,NREC,I,J
      CHARACTER*78 TBUFF
      CHARACTER*80 UCIBUF
C
C     + + + EXTERNALS + + +
      EXTERNAL     GETUCI,QFCLOS,ZWRSCR
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT (A80)
C
C     + + + END SPECIFICATIONS + + +
C
C     outputting UCI file
      TBUFF= ' '
      J= 2
      DO 5 I= 2,6
        CALL ZWRSCR (TBUFF,I,J)
 5    CONTINUE
      TBUFF= 'Writing UCI file to disk'
      I= 6
      J= 26
      CALL ZWRSCR (TBUFF(1:30),I,J)
C
      LTYPRC= -2
      NREC  = 1
 10   CONTINUE
        CALL GETUCI (LTYPRC,
     M               NREC,
     O               UCIBUF)
        WRITE (OUTFL,2010) UCIBUF
      IF (NREC.GT.0) GO TO 10
C
C     close UCI file
      I= 0
      CALL QFCLOS (OUTFL,I)
C
      RETURN
      END
