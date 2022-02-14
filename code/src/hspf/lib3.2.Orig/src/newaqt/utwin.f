C
C
C
      SUBROUTINE   PUTWIN
     I                    (TMXWIN,TWIPEI,TMAPWI,TPLTWI,
     I                     TWTYPE,TNWIN,TWINDI)
C
C     + + + PURPOSE + + +
C     update window set info
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    TMXWIN,TWIPEI(TMXWIN),TMAPWI,TPLTWI,
     1           TWTYPE(TMXWIN),TNWIN
      REAL       TWINDI(4,TMXWIN)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     TMXWIN - maximum number of open windows
C     TWINDI - position on screen where window is found
C     TWIPEI - flag indicating window to be opened and closed for resizing
C     TMAPWI - active map window
C     TPLTWI - active plot window
C     TWTYPE - window type
C     TNWIN  - number of windows defined
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwindo.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      MAPWIN = TMAPWI
      PLTWIN = TPLTWI
      NWIN   = TNWIN
      DO 10 I = 1,MXWIN
        WIPEIT(I)   = TWIPEI(I)
        WTYPE(I)    = TWTYPE(I)
        WINDIM(1,I) = TWINDI(1,I)
        WINDIM(2,I) = TWINDI(2,I)
        WINDIM(3,I) = TWINDI(3,I)
        WINDIM(4,I) = TWINDI(4,I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETWIN
     I                    (TMXWIN,
     O                     TWIPEI,TMAPWI,TPLTWI,
     O                     TWTYPE,TNWIN,TWINDI)
C
C     + + + PURPOSE + + +
C     get window set info
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    TMXWIN,TWIPEI(TMXWIN),TMAPWI,TPLTWI,
     1           TWTYPE(TMXWIN),TNWIN
      REAL       TWINDI(4,TMXWIN)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     TMXWIN - maximum number of open windows
C     TWINDI - position on screen where window is found
C     TWIPEI - flag indicating window to be opened and closed for resizing
C     TMAPWI - active map window
C     TPLTWI - active plot window
C     TWTYPE - window type
C     TNWIN  - number of windows defined
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwindo.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      TMAPWI = MAPWIN
      TPLTWI = PLTWIN
      TNWIN  = NWIN
      DO 10 I = 1,MXWIN
        TWIPEI(I)   = WIPEIT(I)
        TWTYPE(I)   = WTYPE(I)
        TWINDI(1,I) = WINDIM(1,I)
        TWINDI(2,I) = WINDIM(2,I)
        TWINDI(3,I) = WINDIM(3,I)
        TWINDI(4,I) = WINDIM(4,I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   WINMAP
     O                   (MACT)
C
C     + + + PURPOSE + + +
C     find out which map window is currently active
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MACT
C
C     + + + ARGUMENT DEFINTIONS + + +
C     MACT   - number of active map window
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwindo.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      MACT = MAPWIN
C
      RETURN
      END
C
C
C
      SUBROUTINE   WINPLT
     O                   (PACT)
C
C     + + + PURPOSE + + +
C     find out which plot window is currently active
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     PACT
C
C     + + + ARGUMENT DEFINTIONS + + +
C     PACT   - number of active plot window
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwindo.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      PACT = PLTWIN
C
      RETURN
      END
