C
C
C
      SUBROUTINE   GETOSV
     I                  (KEYST,KEYND,MAXOSV,
     O                  OSV)
C
C     + + + PURPOSE + + +
C     Move an osv from osvfl into memory
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   KEYND,KEYST,MAXOSV,OSV(MAXOSV)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEYST  - starting record number
C     KEYND  - ending record number
C     MAXOSV - maximum size of osv
C
C     + + + PARAMETERS + + +
      INCLUDE 'posvm.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cosvm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   KEY,START,ILEN
C
C     + + + EXTERNAL + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
C     start at first position
      START= 1
C     how much to read
      ILEN = 500
C
      DO 10 KEY= KEYST,KEYND
C       loop thru records
C       read a record from memory
        CALL COPYI (ILEN,OSVM(1,KEY),
     O              OSV(START))
C       next position in memory
        START= START+ ILEN
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PUTOSV
     I                   (KEYST,KEYND,MAXOSV,OSV)
C
C     + + + PURPOSE + + +
C     Move an osv from memory to osvfl
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    KEYND,KEYST,MAXOSV,OSV(MAXOSV)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEYST  - starting record number
C     KEYND  - ending record number
C     MAXOSV - maximum size of osv
C
C     + + + PARAMETERS + + +
      INCLUDE 'posvm.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cosvm.inc'
C
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   KEY,ILEN,START
C
C     + + + EXTERNAL + + +
      EXTERNAL  COPYI
C
C     + + + END SPECIFICATIONS + + +
C
C     start at first position
      START= 1
C     how much to read
      ILEN = 500
C
      DO 10 KEY= KEYST,KEYND
C       loop thru records
C       write a record to memory
        CALL COPYI(ILEN,OSV(START),
     O             OSVM(1,KEY))
C       next position in memory
        START= START+ ILEN
 10   CONTINUE
C
      RETURN
      END
