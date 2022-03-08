C
C     parameter for size of in memory OSUP file
      INTEGER   MAXTSF
      PARAMETER(MAXTSF=5000)
C
C     in memory common block for OSUP file
      COMMON/CTSGPM/TSGPM
      INTEGER       TSGPM(200,MAXTSF)
C
