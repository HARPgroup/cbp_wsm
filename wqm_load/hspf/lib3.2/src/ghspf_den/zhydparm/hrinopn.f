C
C
C
      SUBROUTINE   GETNXT
     I                   (OPTYP,
     M                    OPID)
C
C     + + + PURPOSE + + +
C     deterimine the id number of the next operation of the
C     specified type
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OPTYP,OPID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPTYP  - operation type 1-PERLND, 2-IMPLND, etc.
C     OPID   - operation id number
C
C     + + + COMMON BLOCKS- INTERP1 + + +
      INCLUDE    'crin1.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IPOS, LOPID
C
C     + + + END SPECIFICATIONS + + +
C
      LOPID= 99999
      IPOS = 0
 10   CONTINUE
        IPOS = IPOS + 1
        IF (OPNTAB(4,IPOS).EQ.OPTYP) THEN
C         type match
          IF (OPNTAB(3,IPOS) .GT. OPID) THEN
C           might be next
            IF (OPNTAB(3,IPOS) .LT. LOPID) THEN
C             current next
              LOPID = OPNTAB(3,IPOS)
            END IF
          END IF
        END IF
      IF (IPOS.LT.NOPNS) GO TO 10
C
      IF (LOPID .NE. 99999) THEN
C       found a match
        OPID= LOPID
      ELSE
C       no match
        OPID= 0
      END IF
C
      RETURN
      END
