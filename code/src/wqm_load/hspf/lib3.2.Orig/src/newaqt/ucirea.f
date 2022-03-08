C
C
C
      SUBROUTINE   UCIREA
     I                   (MESSFL,UCIFL,PTHNAM,WDMSFL,
     M                    DPRAQF,EMFG,FILES,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     Read a UCI file from disk and perform preliminary checks.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,UCIFL,RETCOD,DPRAQF,EMFG,WDMSFL,FILES(15)
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     UCIFL  - Fortran unit number for UCI file
C     RETCOD - return code, -1 - not a HSPF UCI file, -2 problem with parms
C     DPRAQF - Subjective value for deep aquifer recharge (hspexp)
C     PTHNAM - character string of path of options selected to get here
C     EMFG   - english/metric units flag (1-eng,2-metric)
C     WDMSFL - wdm file unit number
C     FILES  - array of hspf file unit numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,SGRP,SCLU,LPTH,I1,I0,MSGFL,INITFG,CLEN,
     1             ECOUNT,KWDDIM(1),KWDTYP(1),CONT,KCNT
      REAL         DEPSUM
      CHARACTER*1  CHSTR1(20),KWDLIB(12)
      CHARACTER*4  CTEMP
      CHARACTER*48 WNDNAM
      LOGICAL      IOPEN
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR1,CHSTR)
      CHARACTER*20 CHSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    QFCLOS,PRNTXT,INTERP,WMSGTT,KEYUCI,DMPKEY,HDMEST
      EXTERNAL    ZLNTXT,ZWNSOP,ZWNSET,FILBLK,UCIINP,GETDEP
      EXTERNAL    WCH2UD
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (12A1,2I4)
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))
      IF (LPTH .GT. 0) THEN
C       path name available
        WNDNAM= 'Get ('//PTHNAM(1)(1:LPTH)//'G)'
      ELSE
C       no pathname
        WNDNAM= 'Get (G)'
      END IF
      CALL ZWNSET (WNDNAM)
C
C     reading uci file message
      SCLU = 201
      SGRP = 51
      CALL HDMEST (MESSFL,SCLU,SGRP)
C
C     input file opened, process files block in input file
C     if any files are already open, close them
      DO 10 I = 1,14
        IF (I.NE.11) THEN
C         don't do for wdm file
          IF (FILES(I).NE.0) THEN
C           already open, close and set to zero
            INQUIRE (UNIT=FILES(I),OPENED=IOPEN)
            IF (IOPEN) THEN
              CLOSE (UNIT=FILES(I))
            END IF
            FILES(I) = 0
          END IF
        END IF
 10   CONTINUE
C     use wdm data file
      FILES(11)= WDMSFL
      IF (WDMSFL.EQ.0) THEN
C       using multiple wdm files, dont want to open any wdm files
C       specified in the uci file files block
        CTEMP = 'WDM1'
        CALL WCH2UD (CTEMP,
     O               FILES(11))
        CTEMP = 'WDM2'
        CALL WCH2UD (CTEMP,
     O               FILES(12))
        CTEMP = 'WDM3'
        CALL WCH2UD (CTEMP,
     O               FILES(13))
        CTEMP = 'WDM4'
        CALL WCH2UD (CTEMP,
     O               FILES(14))
      END IF
C     use hspf msg file
      FILES(15)= MESSFL
      CALL FILBLK (UCIFL,
     M             FILES,
     O             RETCOD)
      WRITE (99,*) 'FILBLK RETCOD',RETCOD
C     back to beginning of input file
      REWIND(UCIFL)
C
      ECOUNT = 0
      IF (RETCOD .EQ. 0) THEN
C       file block processed without error
C       read users uci file into memory
        MSGFL = FILES(1)
        CALL UCIINP (UCIFL,MESSFL,MSGFL)
C       close UCI file
        I = 0
        CALL QFCLOS (UCIFL,I)
C       get major keywords (RUN)
        SCLU  = 201
        SGRP  = 21
        INITFG= 1
        CLEN  = 20
        CALL WMSGTT (MESSFL,SCLU,SGRP,INITFG,
     M               CLEN,
     O               CHSTR1,CONT)
        READ (CHSTR,1000)  KWDLIB,KWDDIM,KWDTYP
C       look for major keywords
        CLEN  = 4
        CALL KEYUCI (I1,CLEN,I0,I0,I1,KWDLIB,KWDDIM,KWDTYP,
     M               ECOUNT,
     O               KCNT)
        CALL DMPKEY
        IF (ECOUNT .EQ. 0) THEN
C         a clean run data set was found
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'Get ('//PTHNAM(1)(1:LPTH)//'G)'
          ELSE
C           no pathname
            WNDNAM= 'Get (G)'
          END IF
          CALL ZWNSET (WNDNAM)
C         preprocessing uci file message
          SCLU = 201
          SGRP = 58
          CALL HDMEST (MESSFL,SCLU,SGRP)
C         interpret it but dont make instruction files
          CALL INTERP (SCLU,I0,
     M                 FILES,
     O                 RETCOD)
          IF (RETCOD.EQ.0) THEN
C           check DEEPFR values in perlnd for hspexp subjective paramater
            CALL GETDEP (DEPSUM)
            IF (DEPSUM.GT.0.0) THEN
C             there is deep aquifer recharge
              DPRAQF= 1
            END IF
          END IF
        END IF
      END IF
C
      IF (RETCOD .NE. 0 .OR. ECOUNT .NE. 0) THEN
C       let user know there was a problem
        SCLU= 55
        SGRP= 16
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'Get ('//PTHNAM(1)(1:LPTH)//'G) Problem'
        ELSE
C         no pathname
          WNDNAM= 'Get (G) Problem'
        END IF
        CALL ZWNSET (WNDNAM)
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   AQVRSN
C
C     + + + PURPOSE + + +
C     Dummy routine to include unix what version information for the
C     newaqt library.
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*64  VERSN
C
C     + + + END SPECIFICATIONS + + +
C
      INCLUDE 'fversn.inc'
C
      RETURN
      END
