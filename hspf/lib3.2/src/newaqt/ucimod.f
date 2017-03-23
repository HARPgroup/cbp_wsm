C
C
C
      SUBROUTINE   UCIMOD
     I                   (MESSFL,HSPMFL,PTHNAM,
     M                    DPRAQF,EMFG)
C
C     + + + PURPOSE + + +
C     Modify any UCI file parameters.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,HSPMFL,DPRAQF,EMFG
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     HSPMFL - Fortran unit number for HSPF message file
C     DPRAQF - Subjective value for deep aquifer recharge
C     PTHNAM - character string of path of options selected to get here
C     EMFG   - english/metric units flag (1-english,2-metric)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU,SGRP,IOP,BLKCOD,LPTH,I1,I,J,UNIT(1),OPERNO
      CHARACTER*48 WNDNAM
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL     QRESP,UMGLOB,UMOPNS,UMFTAB,UMMLTB,
     #             UMBLCK,UMFORM,ZLNTXT,ZWNSOP,ZWNSET,
     #             ZSTCMA,UMOPER,UMSPEC
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 53
C
      I1 = 1
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))
C
 10   CONTINUE
C       do main UCI modify menu
        SGRP= 1
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'UCI ('//PTHNAM(1)(1:LPTH)//'U)'
        ELSE
C         no pathname
          WNDNAM= 'UCI (U)'
        END IF
        CALL ZWNSET (WNDNAM)
        CALL QRESP (MESSFL,SCLU,SGRP,IOP)
        OPERNO = IOP-6
C
C       allow 'Prev' command
        I= 4
        J= 1
        CALL ZSTCMA (I,J)
C
C       process block selected to modify
        GO TO (110,120,130,140,150,155,160,160,160,160,160,160,
     1         160,160,160,250,260,270,280,290,300,310,320,330), IOP
C
 110    CONTINUE
C         global block
          UNIT(1) = EMFG
          CALL UMGLOB (MESSFL,SCLU,PTHNAM(1),
     M                 UNIT(1))
          EMFG = UNIT(1)
          GO TO 999
C
 120    CONTINUE
C         operation sequence block
          CALL UMOPNS (MESSFL,SCLU,PTHNAM(1))
          GO TO 999
C
 130    CONTINUE
C         ftables block
          CALL UMFTAB (MESSFL,SCLU,PTHNAM(1))
          GO TO 999
C
 140    CONTINUE
C         special actions block
          CALL UMSPEC (MESSFL,SCLU,PTHNAM(1))
          GO TO 999
C
 150    CONTINUE
C         files block
          BLKCOD= 6
          CALL UMBLCK (MESSFL,SCLU,BLKCOD,PTHNAM(1))
          GO TO 999
C
 155    CONTINUE
C         categories block
          BLKCOD= 8
          CALL UMBLCK (MESSFL,SCLU,BLKCOD,PTHNAM(1))
          GO TO 999
C
 160    CONTINUE
C         operation type block
          CALL UMOPER (MESSFL,SCLU,PTHNAM(1),EMFG,OPERNO,
     M                 DPRAQF)
          GO TO 999
C
 250    CONTINUE
C         external sources block
          BLKCOD= 1
          CALL UMBLCK (MESSFL,SCLU,BLKCOD,PTHNAM(1))
          GO TO 999
C
 260    CONTINUE
C         formats block
          CALL UMFORM (MESSFL,SCLU,PTHNAM(1))
          GO TO 999
C
 270    CONTINUE
C         network block
          BLKCOD= 2
          CALL UMBLCK (MESSFL,SCLU,BLKCOD,PTHNAM(1))
          GO TO 999
C
 280    CONTINUE
C         external targets block
          BLKCOD= 3
          CALL UMBLCK (MESSFL,SCLU,BLKCOD,PTHNAM(1))
          GO TO 999
C
 290    CONTINUE
C         schematic block
          BLKCOD= 4
          CALL UMBLCK (MESSFL,SCLU,BLKCOD,PTHNAM(1))
          GO TO 999
C
 300    CONTINUE
C         mass-link block
          CALL UMMLTB (MESSFL,SCLU,PTHNAM(1))
          GO TO 999
C
 310    CONTINUE
C         monthdata block
          BLKCOD= 9
          CALL UMBLCK (MESSFL,SCLU,BLKCOD,PTHNAM(1))
          GO TO 999
C
 320    CONTINUE
C         pathnames block
          BLKCOD= 10
          CALL UMBLCK (MESSFL,SCLU,BLKCOD,PTHNAM(1))
          GO TO 999
C
 330    CONTINUE
C         all done
          IOP= 0
          GO TO 999
C
 999    CONTINUE
C
C     turn off 'Prev' command
      I= 4
      J= 0
      CALL ZSTCMA (I,J)
C
      IF (IOP.GT.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   UMGLOB
     I                   (MESSFL,SCLU,PTHNAM,
     M                    EMFG)
C
C     + + + PURPOSE + + +
C     Modify Global block parameters.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,EMFG(1)
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     PTHNAM - character string of path of options selected to get here
C     EMFG   - english/metric units flag (1-english,2-metric)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER         SGRP,INUM,ILEN,IBAS,IRET,LPTH,I1,ECOUNT,I0,I5
      INTEGER         START(5),ENDAT(5),SEDAT(10),RUNMIN,SPOUT(1),
     1                OUTLVL(1),RESFG(1),RUNFG(1),DUMSTR(5),DUMEND(5)
      CHARACTER*48    WNDNAM
      CHARACTER*78    UCINFO(1)
      CHARACTER*80    RNINFO
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL    Q1INIT,QSTCTF,QSETIB,Q1EDIT,QGTCTF,QGETIB
      EXTERNAL    ZLNTXT,ZWNSOP,ZWNSET,GLOBLK,PUTGLO,COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I5 = 5
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))
C
      SGRP= 10
      IF (LPTH .GT. 0) THEN
C       path name available
        WNDNAM= 'Global ('//PTHNAM(1)(1:LPTH)//'UGl)'
      ELSE
C       no pathname
        WNDNAM= 'Global (UGl)'
      END IF
      CALL ZWNSET (WNDNAM)
      CALL Q1INIT (MESSFL,SCLU,SGRP)
C     get info from global block
      ECOUNT = 0
      CALL GLOBLK (I0,MESSFL,
     M             ECOUNT,
     O             SEDAT,DUMSTR,DUMEND,RUNMIN,OUTLVL(1),SPOUT(1),
     O             RESFG(1),RUNFG(1),EMFG(1),RNINFO)
C     put run info into c*78
      UCINFO(1) = RNINFO
      CALL COPYI (I5,SEDAT(1),START)
      CALL COPYI (I5,SEDAT(6),ENDAT)
C     set uciinfo
      IBAS= 1
      ILEN= 78
      CALL QSTCTF(IBAS,ILEN,UCINFO)
C     start date
      INUM= 5
      CALL QSETIB(INUM,IBAS,START)
C     end date
      IBAS= 6
      CALL QSETIB(INUM,IBAS,ENDAT)
C     run interp output level
      INUM= 1
      IBAS= 11
      CALL QSETIB(INUM,IBAS,OUTLVL)
C     special actions output level
      IBAS= 12
      CALL QSETIB(INUM,IBAS,SPOUT)
C     resume flag
      IBAS= 13
      CALL QSETIB(INUM,IBAS,RESFG)
C     run flag
      IBAS= 14
      CALL QSETIB(INUM,IBAS,RUNFG)
C     english/metric units flag
      IBAS= 15
      CALL QSETIB(INUM,IBAS,EMFG)
C     edit
      CALL Q1EDIT(IRET)
      IF (IRET .EQ. 1) THEN
C       retrieve parms
C       get uciinfo
        IBAS= 1
        ILEN= 78
        CALL QGTCTF(IBAS,ILEN,UCINFO)
C       start date
        INUM= 5
        CALL QGETIB(INUM,IBAS,START)
C       end date
        IBAS= 6
        CALL QGETIB(INUM,IBAS,ENDAT)
C       run interp output level
        INUM= 1
        IBAS= 11
        CALL QGETIB(INUM,IBAS,OUTLVL)
C       special actions output level
        IBAS= 12
        CALL QGETIB(INUM,IBAS,SPOUT)
C       resume flag
        IBAS= 13
        CALL QGETIB(INUM,IBAS,RESFG)
C       run flag
        IBAS= 14
        CALL QGETIB(INUM,IBAS,RUNFG)
C       english/metric units flag
        IBAS= 15
        CALL QGETIB(INUM,IBAS,EMFG)
C       put global block parameters to in memory uci file
        RNINFO = UCINFO(1)
        CALL PUTGLO (START,ENDAT,OUTLVL,SPOUT,
     I               RESFG,RUNFG,EMFG,RNINFO)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   UMOPNS
     I                   (MESSFL,SCLU,PTHNAM)
C
C     + + + PURPOSE + + +
C     Modify Operation Sequence block parameters.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     PTHNAM - character string of path of options selected to get here
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I1,SGRP,I,I0,I78,TMPKEY,RPTFLG,
     1             IRET,LPTH,OPS,OPLEN,CVAL(1,200),
     2             OPVAL(2),MXSEL(1),MNSEL(1),CONTFG,BACK,
     3             EREC,SREC,KEY,BROW,NROW,HRMIN(2),TOTROW
      CHARACTER*1  TBUFF(78,1)
      CHARACTER*48 WNDNAM
      CHARACTER*78 TBUF78
      CHARACTER*80 UCIBF,TMPBF
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL    Q1EDIT,QSETI,QGETI,QSETOP,QGETOP,DELUCI,PUTUCI
      EXTERNAL    ZLNTXT,ZWNSOP,ZWNSET,Q1INIT,COMUCI
      EXTERNAL    Q2INIT,QSTBFB,QGTBFB,Q2EDIT,PRNTXT
      EXTERNAL    GETSE,GETUCI,REPUCI,CARVAR,CVARAR,Q2GTCO
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (30X,I2,1X,I2)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (4X,'INGRP              INDELT ',I2,':',I2)
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      I1   = 1
      I78  = 78
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))
C
      IF (LPTH .GT. 0) THEN
C       path name available
        WNDNAM= 'Operations Sequence ('//PTHNAM(1)(1:LPTH)//'UO)'
      ELSE
C       no pathname
        WNDNAM= 'Operations Sequence (UO)'
      END IF
C
 5    CONTINUE
        I = 3
        CALL GETSE (I,I1,
     O              SREC,EREC)
        IF (SREC.NE.0) THEN
C         this block exists to modify
          KEY = SREC
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
C         get from this line ingrp time step
          READ (UCIBF,1000) HRMIN(1),HRMIN(2)
C         first screen for ingrp time step
          BACK = 0
          SGRP = 22
          CALL ZWNSET (WNDNAM)
          CALL Q1INIT (MESSFL,SCLU,SGRP)
          OPS  = 1
          OPLEN= 2
          MNSEL(1) = 0
          MXSEL(1) = 2
          OPVAL(1)= 0
          OPVAL(2)= 0
          IF (HRMIN(1).NE.0) THEN
C           set the hours option to 'on'
            OPVAL(1) = 1
          ELSE IF (HRMIN(2).NE.0) THEN
C           set the minutes option to 'on'
            OPVAL(1) = 2
          END IF
C         set option fields for specifying hours or minutes
          CALL QSETOP (OPS,OPLEN,MXSEL,MNSEL,OPVAL)
C         set values of hours and minutes for editing
          CALL QSETI (OPLEN,HRMIN)
C         edit either hour or minute value
          CALL Q1EDIT (
     O                 IRET)
          IF (IRET.EQ.2 .OR. IRET.EQ.7) THEN
C           previous or interrupt out
            CONTFG = 0
          ELSE IF (IRET.EQ.1) THEN
C           user wants to continue
            CONTFG = 1
C           read whether user is specifying hours or minutes
            CALL QGETOP (OPLEN,
     O                   OPVAL)
C           read hour or minute value
            CALL QGETI (OPLEN,
     O                  HRMIN)
            IF (OPVAL(1).EQ.1) THEN
C             user is specifying hours, so minutes must be zero
              HRMIN(2) = 0
            ELSE IF (OPVAL(1).EQ.2 .OR. OPVAL(2).EQ.1) THEN
C             user is specifying mins, so hours must be zero
              HRMIN(1) = 0
            END IF
            UCIBF = ' '
            WRITE (UCIBF,2000) HRMIN(1),HRMIN(2)
            CALL REPUCI (KEY,UCIBF)
          END IF
C
C         second screen for operations sequence on/off
 40       CONTINUE
            IF (CONTFG.EQ.1) THEN
              SGRP = 21
              CALL ZWNSET (WNDNAM)
C             initialize screen
              CALL Q2INIT (MESSFL,SCLU,SGRP)
              KEY = SREC
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
              BROW= 0
              NROW= 1
 50           CONTINUE
C               fill in screen with records from in-memory uci
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
                BROW = BROW + 1
                TBUF78 = 'NO ' // UCIBF
                CALL CVARAR (I78,TBUF78,I78,TBUFF(1,1))
C               put this record on the screen
                CALL QSTBFB (I78,NROW,BROW,TBUFF)
                TMPKEY = KEY
C               look ahead two records for end of block
                CALL GETUCI (I0,
     M                       TMPKEY,
     O                       TMPBF)
                CALL GETUCI (I0,
     M                       TMPKEY,
     O                       TMPBF)
              IF (TMPKEY.NE.EREC) GO TO 50
C             let user edit screen
              CALL Q2EDIT (BROW,
     O                     IRET)
              RPTFLG= 0
              TOTROW= BROW
              IF (IRET.EQ.1) THEN
C               user wants to continue
C               get records from screen
                KEY = SREC
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
                BROW  = 0
                NROW  = 1
 60             CONTINUE
                  BROW = BROW + 1
                  CALL GETUCI (I0,
     M                         KEY,
     O                         UCIBF)
C                 get this record from the screen
                  CALL QGTBFB (I78,NROW,BROW,
     O                         TBUFF)
                  CALL CARVAR (I78,TBUFF(1,1),I78,TBUF78)
                  UCIBF = TBUF78(4:78) // '     '
C                 put this record back to in-memory uci
                  CALL REPUCI (KEY,UCIBF)
C                 see if we want to copy or delete it
                  CALL Q2GTCO (I1,BROW,
     O                         CVAL)
                  IF (CVAL(1,BROW).EQ.2) THEN
C                   user wants to drop this line
                    RPTFLG = 1
                    IF (TOTROW.GT.1) THEN
C                     okay to delete a line
                      CALL DELUCI (KEY)
                      TOTROW = TOTROW - 1
                    ELSE
C                     this is only row left, can't delete it
                      SGRP = 23
                      CALL ZWNSET (WNDNAM)
                      CALL PRNTXT (MESSFL,SCLU,SGRP)
                    END IF
                  ELSE IF (CVAL(1,BROW).EQ.3) THEN
C                   user wants to copy this line
                    RPTFLG = 1
                    CALL PUTUCI (UCIBF,I1,KEY)
                    CALL GETUCI (I0,
     M                           KEY,
     O                           UCIBF)
                    TOTROW = TOTROW + 1
                  ELSE IF (CVAL(1,BROW).EQ.4) THEN
C                   user wants to comment out this line
                    IF (TOTROW.GT.1) THEN
C                     okay to comment a line
                      CALL COMUCI (KEY,
     M                             UCIBF)
                      CALL REPUCI (KEY,UCIBF)
                    END IF
                  END IF
C                 look ahead two records for end of block
                  TMPKEY = KEY
                  CALL GETUCI (I0,
     M                         TMPKEY,
     O                         TMPBF)
                  CALL GETUCI (I0,
     M                         TMPKEY,
     O                         TMPBF)
                IF (TMPKEY.NE.EREC) GO TO 60
              ELSE
C               user wants interrupt or previous
                BACK = 1
              END IF
            END IF
          IF (RPTFLG.EQ.1) GO TO 40
        ELSE
C         no operation sequence block to modify
          SGRP= 20
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'Operations Sequence ('
     $               //PTHNAM(1)(1:LPTH)//'UO) Problem'
          ELSE
C           no pathname
            WNDNAM= 'Operations Sequence (UO) Problem'
          END IF
          CALL ZWNSET (WNDNAM)
          CALL PRNTXT (MESSFL,SCLU,SGRP)
        END IF
      IF (BACK.EQ.1) GO TO 5
C
      RETURN
      END
C
C
C
      SUBROUTINE   UMOPER
     I                   (MESSFL,SCLU,PTHNAM,EMFG,OPERNO,
     M                    DPRAQF)
C
C     + + + PURPOSE + + +
C     Modify OPERATION type block parameters.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,DPRAQF,EMFG,OPERNO
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     DPRAQF - Subjective value for deep aquifer recharge (hspexp)
C     PTHNAM - character string of path of options selected to get here
C     EMFG   - english/metric units flag (1-english,2-metric)
C     OPERNO - operation type, 1-perlnd, 2-implnd, etc.
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,TCLU,SGRP,MXTBDP(7),LPTH,I1,GRPBSP(7),TBMOFG(100),
     1             MXSEL(1),MNSEL(1),IRET,TABIND,LMXTBL,OPID,
     2             RETPRV,MXTBDI(2),GRPBSI(2),MXTBDR(6),GRPBSR(6),
     3             GRPBAS,MXTBAD,INITFG,OLEN,CONT
      REAL         DEPSUM
      CHARACTER*48 WNDNAM
      CHARACTER*80 OBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (OBUFF,OBUF1)
      CHARACTER*1  OBUF1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL    MODOPN,PRNTXT,ZIPI,Q1INIT,QSETOP,Q1EDIT,QGETOP
      EXTERNAL    ZLNTXT,ZWNSOP,ZWNSET,ZGTRET,GETNXT,WMSGTT,GETDEP
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT (2I10)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
C     return here from previous from second menu
 5    CONTINUE
        RETPRV = 0
C       set prefix to window names
        CALL ZWNSOP (I1,PTHNAM(1))
C       length of path name
        LPTH= ZLNTXT(PTHNAM(1))
C
        OPID = 0
        CALL GETNXT (OPERNO,
     M               OPID)
        IF (OPID.NE.0) THEN
C         this operation exists to modify
          IF (OPERNO.EQ.1) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'PErlnd ('//PTHNAM(1)(1:LPTH)//'UPe)'
            ELSE
C             no pathname
              WNDNAM= 'PErlnd (UPe)'
            END IF
          ELSE IF (OPERNO.EQ.2) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'Implnd ('//PTHNAM(1)(1:LPTH)//'UI)'
            ELSE
C             no pathname
              WNDNAM= 'Implnd (UI)'
            END IF
          ELSE IF (OPERNO.EQ.3) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'RChres ('//PTHNAM(1)(1:LPTH)//'URc)'
            ELSE
C             no pathname
              WNDNAM= 'RChres (URc)'
            END IF
          ELSE IF (OPERNO.EQ.4) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'Copy ('//PTHNAM(1)(1:LPTH)//'UC)'
            ELSE
C             no pathname
              WNDNAM= 'Copy (UC)'
            END IF
          ELSE IF (OPERNO.EQ.5) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'PLtgen ('//PTHNAM(1)(1:LPTH)//'UPl)'
            ELSE
C             no pathname
              WNDNAM= 'PLtgen (UPl)'
            END IF
          ELSE IF (OPERNO.EQ.6) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'DIsply ('//PTHNAM(1)(1:LPTH)//'UDi)'
            ELSE
C             no pathname
              WNDNAM= 'DIsply (UDi)'
            END IF
          ELSE IF (OPERNO.EQ.7) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'DUranl ('//PTHNAM(1)(1:LPTH)//'UDu)'
            ELSE
C             no pathname
              WNDNAM= 'DUranl (UDu)'
            END IF
          ELSE IF (OPERNO.EQ.8) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'GEner ('//PTHNAM(1)(1:LPTH)//'UGe)'
            ELSE
C             no pathname
              WNDNAM= 'GEner (UGe)'
            END IF
          ELSE IF (OPERNO.EQ.9) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'MUtsin ('//PTHNAM(1)(1:LPTH)//'UMu)'
            ELSE
C             no pathname
              WNDNAM= 'MUtsin (UMu)'
            END IF
          END IF
          CALL ZWNSET (WNDNAM)
          IF (OPERNO.GE.1 .AND. OPERNO.LE.3) THEN
C           multiple screens for this operation type
            IF (OPERNO.EQ.1) THEN
C             init section modify flags to off
              TCLU    = 121
C             fill array of addresses for intermediate screens
              SGRP  = 40
              INITFG= 1
              LMXTBL= 0
  10          CONTINUE
                OLEN= 80
                CALL WMSGTT (MESSFL,TCLU,SGRP,INITFG,
     M                       OLEN,
     O                       OBUF1,CONT)
                LMXTBL= LMXTBL+ 1
                READ(OBUFF,1010) GRPBSP(LMXTBL),MXTBDP(LMXTBL)
                INITFG= 0
C               loop back if more
              IF (CONT.EQ.1) GO TO 10
              MXSEL(1)= LMXTBL
              MNSEL(1)= 1
            ELSE IF (OPERNO.EQ.2) THEN
C             init section modify flags to off
              TCLU    = 122
C             fill array of addresses for intermediate screens
              SGRP  = 40
              INITFG= 1
              LMXTBL= 0
  20          CONTINUE
                OLEN= 80
                CALL WMSGTT (MESSFL,TCLU,SGRP,INITFG,
     M                       OLEN,
     O                       OBUF1,CONT)
                LMXTBL= LMXTBL+ 1
                READ(OBUFF,1010) GRPBSI(LMXTBL),MXTBDI(LMXTBL)
                INITFG= 0
C               loop back if more
              IF (CONT.EQ.1) GO TO 20
              MXSEL(1)= LMXTBL
              MNSEL(1)= 1
            ELSE IF (OPERNO.EQ.3) THEN
C             init section modify flags to off
              TCLU    = 123
C             fill array of addresses for intermediate screens
              SGRP  = 40
              INITFG= 1
              LMXTBL= 0
  30          CONTINUE
                OLEN= 80
                CALL WMSGTT (MESSFL,TCLU,SGRP,INITFG,
     M                       OLEN,
     O                       OBUF1,CONT)
                LMXTBL= LMXTBL+ 1
                READ(OBUFF,1010) GRPBSR(LMXTBL),MXTBDR(LMXTBL)
                INITFG= 0
C               loop back if more
              IF (CONT.EQ.1) GO TO 30
              MXSEL(1)= LMXTBL
              MNSEL(1)= 1
            END IF
            SGRP = 50
            CALL ZIPI(LMXTBL,I0,TBMOFG)
            CALL Q1INIT (MESSFL,TCLU,SGRP)
C           determine which section(s) to modify
            CALL QSETOP(I1,LMXTBL,MXSEL,MNSEL,TBMOFG)
C           let user edit screen of values
            CALL Q1EDIT (IRET)
            IF (IRET.EQ.1) THEN
C             user wants to continue
C             modify which sections?
              CALL QGETOP (LMXTBL,
     O                     TBMOFG)
              TABIND= 0
 40           CONTINUE
C               loop through to see which sections to modify
                TABIND= TABIND+ 1
                IF (TBMOFG(TABIND).GT.0) THEN
                  CALL ZWNSET (WNDNAM)
C                 find out which tables to modify for this section
                  IF (OPERNO.EQ.1) THEN
                    TCLU = 121
                    SGRP = 50+TBMOFG(TABIND)
                    CALL MODOPN (MESSFL,TCLU,SCLU,SGRP,OPERNO,
     I                           GRPBSP(TBMOFG(TABIND)),EMFG,
     I                           MXTBDP(TBMOFG(TABIND)))
                    IF (TBMOFG(TABIND).EQ.1) THEN
C                     check DEEPFR values to maybe set subjective parameter
                      CALL GETDEP (DEPSUM)
                      IF (DEPSUM.GT.0.0) THEN
C                       there is deep aquifer recharge
                        DPRAQF= 1
                      END IF
                    END IF
                  ELSE IF (OPERNO.EQ.2) THEN
                    TCLU = 122
                    SGRP = 50+TBMOFG(TABIND)
                    CALL MODOPN (MESSFL,TCLU,SCLU,SGRP,OPERNO,
     I                           GRPBSI(TBMOFG(TABIND)),EMFG,
     I                           MXTBDI(TBMOFG(TABIND)))
                  ELSE IF (OPERNO.EQ.3) THEN
                    TCLU = 123
                    SGRP = 50+TBMOFG(TABIND)
                    CALL MODOPN (MESSFL,TCLU,SCLU,SGRP,OPERNO,
     I                           GRPBSR(TBMOFG(TABIND)),EMFG,
     I                           MXTBDR(TBMOFG(TABIND)))
                  END IF
C                 get response from previous screen
                  CALL ZGTRET (RETPRV)
                END IF
              IF (TABIND.GE.0 .AND. TABIND.LT.LMXTBL .AND. RETPRV.NE.2)
     1          GO TO 40
            END IF
          ELSE
C           only one screen for this operation type
            SGRP  = 50
            TCLU  = 120 + OPERNO
            GRPBAS= 0
            MXTBAD= 50
            CALL MODOPN (MESSFL,TCLU,SCLU,SGRP,OPERNO,
     I                   GRPBAS,EMFG,MXTBAD)
          END IF
        ELSE
C         no operation of this type to modify
          SGRP= 69
          IF (OPERNO.EQ.1) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'PErlnd ('//PTHNAM(1)(1:LPTH)//'UPe) Problem'
            ELSE
C             no pathname
              WNDNAM= 'PErlnd (UPe) Problem'
            END IF
          ELSE IF (OPERNO.EQ.2) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'Implnd ('//PTHNAM(1)(1:LPTH)//'UI) Problem'
            ELSE
C             no pathname
              WNDNAM= 'Implnd (UI) Problem'
            END IF
          ELSE IF (OPERNO.EQ.3) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'RChres ('//PTHNAM(1)(1:LPTH)//'URc) Problem'
            ELSE
C             no pathname
              WNDNAM= 'RChres (URc) Problem'
            END IF
          ELSE IF (OPERNO.EQ.4) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'Copy ('//PTHNAM(1)(1:LPTH)//'UC) Problem'
            ELSE
C             no pathname
              WNDNAM= 'Copy (UC) Problem'
            END IF
          ELSE IF (OPERNO.EQ.5) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'PLtgen ('//PTHNAM(1)(1:LPTH)//'UPl) Problem'
            ELSE
C             no pathname
              WNDNAM= 'PLtgen (UPl) Problem'
            END IF
          ELSE IF (OPERNO.EQ.6) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'DIsply ('//PTHNAM(1)(1:LPTH)//'UDi) Problem'
            ELSE
C             no pathname
              WNDNAM= 'DIsply (UDi) Problem'
            END IF
          ELSE IF (OPERNO.EQ.7) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'DUranl ('//PTHNAM(1)(1:LPTH)//'UDu) Problem'
            ELSE
C             no pathname
              WNDNAM= 'DUranl (UDu) Problem'
            END IF
          ELSE IF (OPERNO.EQ.8) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'GEner ('//PTHNAM(1)(1:LPTH)//'UGe) Problem'
            ELSE
C             no pathname
              WNDNAM= 'GEner (UGe) Problem'
            END IF
          ELSE IF (OPERNO.EQ.9) THEN
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'MUtsin ('//PTHNAM(1)(1:LPTH)//'UMu) Problem'
            ELSE
C             no pathname
              WNDNAM= 'MUtsin (UMu) Problem'
            END IF
          END IF
          CALL ZWNSET (WNDNAM)
          CALL PRNTXT (MESSFL,SCLU,SGRP)
        END IF
      IF (RETPRV.EQ.2) GO TO 5
C
      RETURN
      END
C
C
C
      SUBROUTINE   MODOPN
     I                   (MESSFL,TCLU,SCLU,SGRP,OPERNO,
     I                    GRPBAS,EMFG,MXTBAD)
C
C     + + + PURPOSE + + +
C     Modify parameter tables for any operation from UCI file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,TCLU,SCLU,SGRP,OPERNO,
     $            GRPBAS,EMFG,MXTBAD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     TCLU   - cluster number on message file of table template
C     SCLU   - cluster number on message file of tables to modify screen
C     SGRP   - group number on message file of tables to modify screen
C     GRPBAS - base screen group number for this group of tables
C     EMFG   - english/metric units flag (1-english,2-metric)
C     OPERNO - this operation number (1-perlnd,2-implnd)
C     MXTBAD - max number of tables on this screen
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,I1,TGRP,TABIND,I78,I12,HEDGRP,
     $            MXSEL(1),MNSEL(1),TBMOFG(50),IRET,IM1,
     $            INITFG,KGRP,CONT,OLEN,NUMREC,SREC,EREC,IDNO,
     $            TABGRP(4,136),KEY,BROW,NROW,CLEN(1),SRTFLG
      CHARACTER*1 TBUFF(78,1),TNMBUF(12,136),CVALC(12)
      CHARACTER*78 TBUF78
      CHARACTER*80 OBUFF,UCIBF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (OBUFF,OBUF1)
      CHARACTER*1  OBUF1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZIPI, GETUCI, REPUCI, CVARAR, CARVAR
      EXTERNAL    Q1INIT, Q1EDIT, QSETOP, QGETOP, WMSGTT, QSTBFB, QGTBFB
      EXTERNAL    Q2INIT, Q2EDIT, LENSTR, CHRCHR, PMXTXA, ZMNSST, ADJLEN
      EXTERNAL    CHKADJ, ESTCHK, RNGCHK, GETSE
C
C     + + + INPUT FORMATS + + +
 1020 FORMAT (8X,12A1,2X,4(4X,I3))
C
C     + + + END SPECIFICATIONS + + +
C
      IM1 = -1
      I0  = 0
      I1  = 1
      I12 = 12
      I78 = 78
C     first get info about each table
      TGRP  = 1
      INITFG= 1
      NUMREC= 0
  5   CONTINUE
        OLEN= 80
        CALL WMSGTT (MESSFL,TCLU,TGRP,INITFG,
     M               OLEN,
     O               OBUF1,CONT)
        NUMREC= NUMREC+ 1
        READ(OBUFF,1020) (TNMBUF(I,NUMREC),I=1,12),
     $                   (TABGRP(I,NUMREC),I=1,4)
        INITFG= 0
C       loop back if more
      IF (CONT.EQ.1) GO TO 5
C     init table modify flags to off
      MXSEL(1)= MXTBAD
      MNSEL(1)= 1
      CALL ZIPI(MXTBAD,I0,TBMOFG)
C
 10   CONTINUE
        CALL Q1INIT (MESSFL,TCLU,SGRP)
C       determine which table(s) to modify
        CALL QSETOP(I1,MXTBAD,MXSEL,MNSEL,TBMOFG)
C       let user edit screen of values
        CALL Q1EDIT (IRET)
        IF (IRET.EQ.1) THEN
C         user wants to continue
C         modify which tables?
          CALL QGETOP (MXTBAD,
     O                 TBMOFG)
          TABIND= 0
 40       CONTINUE
C           loop through to see which tables to modify
            TABIND= TABIND+ 1
            IF (TBMOFG(TABIND).GT.0) THEN
C             modify parameters for table selected
              KGRP= GRPBAS + TBMOFG(TABIND)
              TGRP= TABGRP(EMFG,KGRP)
C             check that this table exists
              CALL ESTCHK (MESSFL,SCLU,TCLU,TGRP,KGRP,OPERNO,
     I                     TNMBUF(1,KGRP),
     O                     SREC,EREC)
              IF (SREC.NE.0) THEN
C               some records exist in this table
C               check to see if record length needs adjusting
                CALL CHKADJ (MESSFL,TCLU,TGRP,
     O                       SRTFLG)
C               build header line
                CLEN(1)= LENSTR(I12,TNMBUF(1,KGRP))
                CALL CHRCHR (CLEN(1),TNMBUF(1,KGRP),CVALC(1))
                HEDGRP = 5
                CALL PMXTXA (MESSFL,SCLU,HEDGRP,I1,I1,IM1,I1,CLEN,
     I                       CVALC)
C               save this text
                CALL ZMNSST
C               initialize screen
                CALL Q2INIT (MESSFL,TCLU,TGRP)
                KEY = SREC
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
                IDNO= (OPERNO*1000)+KGRP
                BROW= 0
                NROW= 1
 50             CONTINUE
C                 fill in screen with records from in-memory uci
C                 expand range of operations if needed
                  CALL RNGCHK (OPERNO,KEY,
     M                         UCIBF)
                  IF (SRTFLG.EQ.1) THEN
C                   will need to take spaces out for long records
                    CALL ADJLEN (I0,
     M                           UCIBF)
                  END IF
                  TBUF78 = UCIBF
                  CALL CVARAR (I78,TBUF78,I78,TBUFF(1,1))
C                 put this record on the screen
                  BROW = BROW + 1
                  CALL QSTBFB (I78,NROW,BROW,TBUFF)
C                 get next record
                  CALL GETUCI (I0,
     M                         KEY,
     O                         UCIBF)
                IF (KEY.NE.EREC) GO TO 50
C               let user edit screen
                CALL Q2EDIT (BROW,
     O                       IRET)
                IF (IRET.EQ.1) THEN
C                 user wants to continue
                  IDNO = (OPERNO*1000)+KGRP
                  CALL GETSE (IDNO,I1,
     O                        SREC,EREC)
                  KEY = SREC
                  CALL GETUCI (I0,
     M                         KEY,
     O                         UCIBF)
                  BROW= 0
                  NROW= 1
 60               CONTINUE
                    BROW = BROW + 1
C                   get this record from the screen
                    CALL QGTBFB (I78,NROW,BROW,
     O                           TBUFF)
                    CALL CARVAR (I78,TBUFF(1,1),I78,TBUF78)
                    UCIBF = TBUF78
                    IF (SRTFLG.EQ.1) THEN
C                     will need to put spaces in for long records
                      CALL ADJLEN (I1,
     M                             UCIBF)
                    END IF
C                   put this record back to in-memory uci
                    CALL REPUCI (KEY,UCIBF)
                    CALL GETUCI (I0,
     M                           KEY,
     O                           UCIBF)
                  IF (KEY.NE.EREC) GO TO 60
                END IF
                IF (IRET.EQ.2) THEN
C                 user wants previous table
                  TABIND= TABIND- 2
                ELSE IF (IRET.EQ.7) THEN
C                 user wants to interrupt editing
                  TABIND= -1
                END IF
              END IF
            END IF
          IF (TABIND.GE.0 .AND. TABIND.LT.MXTBAD) GO TO 40
        ELSE
C         user wants back to General modify menu
          IRET= 1
        END IF
      IF (IRET.EQ.2) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   ESTCHK
     I                   (MESSFL,SCLU,TCLU,TGRP,KGRP,OPERNO,TNMBUF,
     O                    SREC,EREC)
C
C     + + + PURPOSE + + +
C     check to see if this table exists in the uci file,
C     create it if it does not exist and user wants it
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        MESSFL,SCLU,TCLU,TGRP,KGRP,OPERNO,SREC,EREC
      CHARACTER*1    TNMBUF(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - cluster number of screen messages
C     TCLU   - cluster number of table
C     TGRP   - group number of table
C     KGRP   - sequential table number
C     OPERNO - operation number
C     SREC   - starting record number
C     EREC   - ending record number
C     TNMBUF - table name for this table
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      NFLDS,SCOL(30),FLEN(30),APOS(30),IMIN(30),I0,I1,I3,
     $             NMHDRW,RETCOD,IMAX(30),IDEF(30),IDNO,RESP,LGRP,I78,
     $             SGRP,I,KEY,J,IPOS,OPID,IM1
      REAL         RMIN(30),RMAX(30),RDEF(30)
      CHARACTER*1  FTYP(30),HDRBUF(78,5),HBEXT(2),BLNK,CBLNK(3),CSTR(3)
      CHARACTER*80 UCIBF
C
C     + + + FUNCTIONS + + +
      INTEGER      STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL     WMSGTX,GETSE,QRESP,PUTUCI,CHRINS,STRFND,CHRCHR,GETNXT
      EXTERNAL     GETUCI,PUTKWD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CSTR /'*','*','*'/
      DATA CBLNK/' ',' ',' '/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (80A1)
 2010 FORMAT (2X,12A1)
 2020 FORMAT ('  END ',12A1)
 2040 FORMAT (I5,75X)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      IM1= -1
      I1 = 1
      I3 = 3
      I78= 78
      BLNK = ' '
C
C     check that this table exists
      IDNO = (OPERNO*1000)+KGRP
      CALL GETSE (IDNO,I1,
     O            SREC,EREC)
      IF (SREC.EQ.0) THEN
C       this table does not exist, do you want to add it?
        SGRP= 79
        RESP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        IF (RESP.EQ.1) THEN
C         user wants to add this table, figure out where to put it
          LGRP = KGRP
 10       CONTINUE
            IF (LGRP.GT.1) THEN
              LGRP = LGRP-1
              IDNO = (OPERNO*1000)+LGRP
              CALL GETSE (IDNO,I1,
     O                    SREC,EREC)
            END IF
          IF (SREC.EQ.0 .AND. LGRP.GT.1) GO TO 10
C         need to start this table after erec
          IDNO= (OPERNO*1000)+KGRP
C         put table header
          WRITE (UCIBF,2010) (TNMBUF(I),I=1,12)
          KEY = EREC
          CALL PUTUCI (UCIBF,I0,KEY)
C         get next record number
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          SREC = KEY
C
C         put header lines for this table
C         get this tables format info
          CALL WMSGTX (MESSFL,TCLU,TGRP,
     O                 NFLDS,SCOL,FLEN,FTYP,APOS,IMIN,IMAX,IDEF,
     O                 RMIN,RMAX,RDEF,
     O                 NMHDRW,HDRBUF,RETCOD)
          DO 100 I= 1,NMHDRW
            IF (FLEN(1).EQ.4) THEN
C             insert 2 blanks in header lines to restore to original
              J= 1
              HBEXT(1)= HDRBUF(77,I)
              HBEXT(2)= HDRBUF(78,I)
              CALL CHRINS (I78,J,BLNK,HDRBUF(1,I))
              CALL CHRINS (I78,J,BLNK,HDRBUF(1,I))
            ELSE
              HBEXT(1)= BLNK
              HBEXT(2)= BLNK
            END IF
            IF (STRFND(I78,HDRBUF(1,I),I3,CSTR).EQ.0) THEN
C             no comment string found, add one
              IPOS= STRFND(I78,HDRBUF(1,I),I3,CBLNK)
              CALL CHRCHR (I3,CSTR,HDRBUF(IPOS,I))
            END IF
            WRITE (UCIBF,2000)
     1            (HDRBUF(J,I),J=1,78),HBEXT
            CALL PUTUCI (UCIBF,IM1,KEY)
C           get next record number
            CALL GETUCI (IM1,
     M                   KEY,
     O                   UCIBF)
 100      CONTINUE
C
C         put a line for each operation
          OPID = 0
          CALL GETNXT (OPERNO,
     M                 OPID)
 200      CONTINUE
C           put operation id line to uci file in memory
            WRITE (UCIBF,2040) OPID
            CALL PUTUCI (UCIBF,I0,KEY)
C           get next record number
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
C           get next operation id
            CALL GETNXT (OPERNO,
     M                   OPID)
          IF (OPID.GT.0) GO TO 200
C
C         put end table header
          WRITE (UCIBF,2020) (TNMBUF(I),I=1,12)
          CALL PUTUCI (UCIBF,I0,KEY)
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          EREC = KEY
C
C         data structure needs to know that this table now exists
          CALL PUTKWD (I1,IDNO,TNMBUF,SREC,EREC,
     O                 RETCOD)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   RNGCHK
     I                   (OPERNO,KEY,
     M                    UCIBF)
C
C     + + + PURPOSE + + +
C     check to see if this record specifies a range of operations,
C     if so send back one record for each operation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        OPERNO,KEY
      CHARACTER*80   UCIBF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPERNO - operation number
C     UCIBF  - record read from in-memory uci
C     KEY    - key corresponding to this record
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,SNO,ENO,TMPKEY,OPID,COUNT
      CHARACTER*5  CENO,BLNK5
      CHARACTER*80 TMPBF
C
C     + + + EXTERNALS + + +
      EXTERNAL     REPUCI,GETNXT,PUTUCI,GETUCI
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (2I5,70X)
 1010 FORMAT (5X,A5,70X)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (I5)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      BLNK5 = '     '
C
C     check to see if ending number has been entered in range
      READ (UCIBF,1010) CENO
      IF (CENO.NE.BLNK5) THEN
C       something in end of range field, check it
        READ (UCIBF,1000) SNO,ENO
        IF (ENO.NE.SNO) THEN
C         need to expand out this range
          UCIBF(6:10) = BLNK5
C         look for operation of this type
          TMPBF = UCIBF
          TMPKEY= KEY
          OPID  = SNO-1
          COUNT = 0
 10       CONTINUE
            CALL GETNXT (OPERNO,
     M                   OPID)
            IF (OPID.GT.0 .AND. OPID.LE.ENO) THEN
C             found an operation in the range
              COUNT = COUNT + 1
              WRITE (CENO,2000) OPID
              TMPBF(1:5) = CENO
              IF (COUNT.EQ.1) THEN
C               first occurance, replace record read
                CALL REPUCI (TMPKEY,TMPBF)
                UCIBF = TMPBF
              ELSE
C               add this record to in-memory uci
                CALL PUTUCI (TMPBF,I0,TMPKEY)
C               get next record number
                CALL GETUCI (I0,
     M                       TMPKEY,
     O                       TMPBF)
              END IF
            END IF
          IF (OPID.NE.0 .AND. OPID.LT.ENO) GO TO 10
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   UMFORM
     I                   (MESSFL,SCLU,PTHNAM)
C
C     + + + PURPOSE + + +
C     Modify formats block from UCI file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     PTHNAM - character string of path of options selected to get here
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    SGRP,LPTH,I1
      CHARACTER*48 WNDNAM
C
C     + + + FUNCTIONS + + +
      INTEGER    ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL   PRNTXT
      EXTERNAL   ZLNTXT,ZWNSOP,ZWNSET
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))
C
      SGRP = 160
      IF (LPTH .GT. 0) THEN
C       path name available
        WNDNAM= 'FOrmats ('//PTHNAM(1)(1:LPTH)//'UFo)'
      ELSE
C       no pathname
        WNDNAM= 'FOrmats (UFo)'
      END IF
      CALL ZWNSET (WNDNAM)
      CALL PRNTXT (MESSFL,SCLU,SGRP)
C
      RETURN
      END
C
C
C
      SUBROUTINE   UMBLCK
     I                   (MESSFL,SCLU,BLKCOD,PTHNAM)
C
C     + + + PURPOSE + + +
C     Modify parameter tables for timeseries operations from UCI file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,BLKCOD
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     BLKCOD - block type code,
C              1 - Ext Sources
C              2 - Network
C              3 - Ext Targets
C              4 - Schematic
C              6 - Files
C              8 - Categories
C              9 - Month-data
C             10 - Pathnames
C     PTHNAM - character string of path of options selected to get here
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I1,I78,SREC,EREC,KEY,BROW,NROW,ID,RPTFLG,
     2             SGRP,IRET,LPTH,ITMP,CVAL(1,250),TOTROW,DONFG
      CHARACTER*1  TBUFF(78,1)
      CHARACTER*4  CHDEF,BLNK4
      CHARACTER*48 WNDNAM
      CHARACTER*78 TBUF78
      CHARACTER*80 UCIBF
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2INIT,QSTBFB,QGTBFB,Q2EDIT,ADDBLK,DELUCI,PUTUCI,Q2GTCO
      EXTERNAL   ZLNTXT,ZWNSOP,ZWNSET,GETSE,GETUCI,REPUCI,CARVAR,CVARAR
      EXTERNAL   DELBLK,DELKWD,COMUCI
C
C     + + + INPUT FORMATS + + +
 1012 FORMAT(I2)
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT(A4)
 2011 FORMAT(I2)
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      I1   = 1
      I78  = 78
      BLNK4= '    '
      CHDEF= 'DEF '
C
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))
C
      IF (BLKCOD.EQ.1) THEN
        SGRP = 150
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'External Sources ('//PTHNAM(1)(1:LPTH)//'UEXT-S)'
        ELSE
C         no pathname
          WNDNAM= 'External Sources (UEXT-S)'
        END IF
        ID = 5
      ELSE IF (BLKCOD.EQ.2) THEN
        SGRP = 170
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'Network ('//PTHNAM(1)(1:LPTH)//'UN)'
        ELSE
C         no pathname
          WNDNAM= 'Network (UN)'
        END IF
        ID = 7
      ELSE IF (BLKCOD.EQ.3) THEN
        SGRP = 180
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'External Targets ('//PTHNAM(1)(1:LPTH)//'UEXT-T)'
        ELSE
C         no pathname
          WNDNAM= 'External Targets (UEXT-T)'
        END IF
        ID = 8
      ELSE IF (BLKCOD.EQ.4) THEN
        SGRP = 190
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'Schematic ('//PTHNAM(1)(1:LPTH)//'USC)'
        ELSE
C         no pathname
          WNDNAM= 'Schematic (USC)'
        END IF
        ID = 10
      ELSE IF (BLKCOD.EQ.6) THEN
        SGRP = 50
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'FIles ('//PTHNAM(1)(1:LPTH)//'UFi)'
        ELSE
C         no pathname
          WNDNAM= 'FIles (UFi)'
        END IF
        ID = 12
      ELSE IF (BLKCOD.EQ.8) THEN
        SGRP = 55
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'CAtegory ('//PTHNAM(1)(1:LPTH)//'UCa)'
        ELSE
C         no pathname
          WNDNAM= 'CAtegory (UCa)'
        END IF
        ID = 13
      ELSE IF (BLKCOD.EQ.9) THEN
        SGRP = 60
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'MOnth-data ('//PTHNAM(1)(1:LPTH)//'UMo)'
        ELSE
C         no pathname
          WNDNAM= 'MOnth-data (UMo)'
        END IF
        ID = 14
      ELSE IF (BLKCOD.EQ.10) THEN
        SGRP = 65
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'PAthnames ('//PTHNAM(1)(1:LPTH)//'UPa)'
        ELSE
C         no pathname
          WNDNAM= 'PAthnames (UPa)'
        END IF
        ID = 15
      END IF
      CALL GETSE (ID,I1,
     O            SREC,EREC)
      IF (SREC.NE.0) THEN
C       a starting record exists for this table.
C       in the unlikely instance that a uci file has no records between
C       a keyword and end keyword, dont let user edit, should never happen
        KEY = SREC
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        IF (KEY.NE.EREC) THEN
C         okay, there are records between as there should be
 40       CONTINUE
C           initialize screen
            CALL ZWNSET (WNDNAM)
            CALL Q2INIT (MESSFL,SCLU,SGRP)
            KEY = SREC
            BROW= 0
            NROW= 1
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
 50         CONTINUE
C             fill in screen with records from in-memory uci
              BROW = BROW + 1
C             put in defaults if needed
              IF (BLKCOD.EQ.1) THEN
                IF (UCIBF(1:4).EQ.'WDM ' .OR. UCIBF(1:4).EQ.' WDM') THEN
C                 update wdm for multiple wdm files, default to wdm1
                  WRITE (UCIBF(1:4),2010) 'WDM1'
                END IF
                IF (UCIBF(25:28).EQ.BLNK4) THEN
C                 change gap string from blank to default for editing
                  WRITE (UCIBF(25:28),2010) CHDEF
                END IF
                IF (UCIBF(39:42).EQ.BLNK4) THEN
C                 change tran string from blank to default for editing
                  WRITE (UCIBF(39:42),2010) CHDEF
                END IF
                READ (UCIBF(72:73),1012) ITMP
                IF (ITMP.LE.0) THEN
C                 change subscript from blank to default for editing
                  WRITE (UCIBF(72:73),2011) I1
                END IF
                READ (UCIBF(74:75),1012) ITMP
                IF (ITMP.LE.0) THEN
C                 change subscript from blank to default for editing
                  WRITE (UCIBF(74:75),2011) I1
                END IF
              ELSE IF (BLKCOD.EQ.2) THEN
                READ (UCIBF(25:26),1012) ITMP
                IF (ITMP.LE.0) THEN
C                 change subscript from blank to default for editing
                  WRITE (UCIBF(25:26),2011) I1
                END IF
                READ (UCIBF(27:28),1012) ITMP
                IF (ITMP.LE.0) THEN
C                 change subscript from blank to default for editing
                  WRITE (UCIBF(27:28),2011) I1
                END IF
                IF (UCIBF(39:42).EQ.BLNK4) THEN
C                 change tran string from blank to default for editing
                  WRITE (UCIBF(39:42),2010) CHDEF
                END IF
                READ (UCIBF(72:73),1012) ITMP
                IF (ITMP.LE.0) THEN
C                 change subscript from blank to default for editing
                  WRITE (UCIBF(72:73),2011) I1
                END IF
                READ (UCIBF(74:75),1012) ITMP
                IF (ITMP.LE.0) THEN
C                 change subscript from blank to default for editing
                  WRITE (UCIBF(74:75),2011) I1
                END IF
              ELSE IF (BLKCOD.EQ.3) THEN
                READ (UCIBF(25:26),1012) ITMP
                IF (ITMP.LE.0) THEN
C                 change subscript from blank to default for editing
                  WRITE (UCIBF(25:26),2011) I1
                END IF
                READ (UCIBF(27:28),1012) ITMP
                IF (ITMP.LE.0) THEN
C                 change subscript from blank to default for editing
                  WRITE (UCIBF(27:28),2011) I1
                END IF
                IF (UCIBF(39:42).EQ.BLNK4) THEN
C                 change tran string from blank to default for editing
                  WRITE (UCIBF(39:42),2010) CHDEF
                END IF
                IF (UCIBF(44:47).EQ.'WDM '.OR.
     1              UCIBF(44:47).EQ.' WDM') THEN
C                 update wdm for multiple wdm files, default to wdm1
                  WRITE (UCIBF(44:47),2010) 'WDM1'
                END IF
                IF (UCIBF(69:72).EQ.BLNK4) THEN
C                 change agg string from blank to default for editing
                  WRITE (UCIBF(69:72),2010) CHDEF
                END IF
              END IF
C             add field for drop or add record if needed
              IF (BLKCOD.EQ.3) THEN
C               remove characters to make room for copy/drop field
                TBUF78 = 'NO ' // UCIBF(1:72) // UCIBF(74:77)
              ELSE
                TBUF78 = 'NO ' // UCIBF
              END IF
              CALL CVARAR (I78,TBUF78,I78,TBUFF(1,1))
C             put this record on the screen
              CALL QSTBFB (I78,NROW,BROW,TBUFF)
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
            IF (KEY.NE.EREC) GO TO 50
C           let user edit screen
            CALL Q2EDIT (BROW,
     O                   IRET)
            RPTFLG= 0
            TOTROW= BROW
            IF (IRET.EQ.1) THEN
C             user wants to continue
              KEY = SREC
              BROW= 0
              NROW= 1
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
              DONFG = 0
 60           CONTINUE
                BROW = BROW + 1
C               get this record from the screen
                CALL QGTBFB (I78,NROW,BROW,
     O                       TBUFF)
                CALL CARVAR (I78,TBUFF(1,1),I78,TBUF78)
C               add field for drop or add record
                IF (BLKCOD.EQ.3) THEN
C                 need to put characters back for external targets records
                  UCIBF = TBUF78(4:74) // ' ' // TBUF78(75:78) // '    '
                  IF (UCIBF(69:71).EQ.'AGG') THEN
                    UCIBF(69:72) = 'AGGR'
                  END IF
                  IF (UCIBF(74:76).EQ.'INS') THEN
                    UCIBF(74:77) = 'INST'
                  ELSE IF (UCIBF(74:76).EQ.'REP') THEN
                    UCIBF(74:77) = 'REPL'
                  END IF
                ELSE
                  UCIBF = TBUF78(4:78) // '     '
                END IF
C               take out defaults if needed
                IF (BLKCOD.EQ.1) THEN
                  IF (UCIBF(25:28).EQ.CHDEF) THEN
C                   change gap string from def back to blank after editing
                    WRITE (UCIBF(25:28),2010) BLNK4
                  END IF
                  IF (UCIBF(39:42).EQ.CHDEF) THEN
C                   change tran string from def back to blnk after editing
                    WRITE (UCIBF(39:42),2010) BLNK4
                  END IF
                ELSE IF (BLKCOD.EQ.2) THEN
                  IF (UCIBF(39:42).EQ.CHDEF) THEN
C                   change tran string from def back to blnk after editing
                    WRITE (UCIBF(39:42),2010) BLNK4
                  END IF
                ELSE IF (BLKCOD.EQ.3) THEN
                  IF (UCIBF(39:42).EQ.CHDEF) THEN
C                   change tran string from def back to blnk after editing
                    WRITE (UCIBF(39:42),2010) BLNK4
                  END IF
                  IF (UCIBF(69:72).EQ.CHDEF) THEN
C                   change gap string from def back to blank after editing
                    WRITE (UCIBF(69:72),2010) BLNK4
                  END IF
                END IF
C               put this record back to in-memory uci
                CALL REPUCI (KEY,UCIBF)
C               see if we want to copy or delete it
                CALL Q2GTCO (I1,BROW,
     O                       CVAL)
                IF (CVAL(1,BROW).EQ.2) THEN
C                 user wants to drop this line
                  IF (TOTROW.GT.1) THEN
C                   okay to delete a line
                    CALL DELUCI (KEY)
                    TOTROW = TOTROW - 1
                    RPTFLG = 1
                  ELSE
C                   deleting this line means deleting this block
                    CALL DELBLK (ID)
                    CALL DELKWD (ID)
                    DONFG = 1
                    RPTFLG= 0
                  END IF
                ELSE IF (CVAL(1,BROW).EQ.3) THEN
C                 user wants to copy this line
                  RPTFLG = 1
                  CALL PUTUCI (UCIBF,I1,KEY)
                  CALL GETUCI (I0,
     M                         KEY,
     O                         UCIBF)
                  TOTROW = TOTROW + 1
                ELSE IF (CVAL(1,BROW).EQ.4) THEN
C                 user wants to comment out this line
                  IF (TOTROW.GT.1) THEN
C                   okay to comment a line
                    CALL COMUCI (KEY,
     M                           UCIBF)
                    CALL REPUCI (KEY,UCIBF)
                  END IF
                END IF
                IF (DONFG.EQ.0) THEN
                  CALL GETUCI (I0,
     M                         KEY,
     O                         UCIBF)
                END IF
              IF (KEY.NE.EREC .AND. DONFG.EQ.0) GO TO 60
            END IF
          IF (RPTFLG.EQ.1) GO TO 40
        END IF
      ELSE
C       no data available for this block
        IF (BLKCOD.EQ.1) THEN
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'External Sources ('//PTHNAM(1)(1:LPTH)//'UEXT-S)
     1               Problem'
          ELSE
C           no pathname
            WNDNAM= 'External Sources (UEXT-S) Problem'
          END IF
        ELSE IF (BLKCOD.EQ.2) THEN
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'Network ('//PTHNAM(1)(1:LPTH)//'UN) Problem'
          ELSE
C           no pathname
            WNDNAM= 'Network (UN) Problem'
          END IF
        ELSE IF (BLKCOD.EQ.3) THEN
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'External Targets ('//PTHNAM(1)(1:LPTH)//'UEXT-T) '
     1              //'Problem'
          ELSE
C           no pathname
            WNDNAM= 'External Targets (UEXT-T) Problem'
          END IF
        ELSE IF (BLKCOD.EQ.4) THEN
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'Schematic ('//PTHNAM(1)(1:LPTH)//'USC) Problem'
          ELSE
C           no pathname
            WNDNAM= 'Schematic (USC) Problem'
          END IF
        ELSE IF (BLKCOD.EQ.6) THEN
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'FIles ('//PTHNAM(1)(1:LPTH)//'UFi) Problem'
          ELSE
C           no pathname
            WNDNAM= 'FIles (UFi) Problem'
          END IF
        ELSE IF (BLKCOD.EQ.8) THEN
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'CAtegory ('//PTHNAM(1)(1:LPTH)//'UCa) Problem'
          ELSE
C           no pathname
            WNDNAM= 'CAtegory (UCa) Problem'
          END IF
        ELSE IF (BLKCOD.EQ.9) THEN
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'MOnth-Data ('//PTHNAM(1)(1:LPTH)//'UMo) Problem'
          ELSE
C           no pathname
            WNDNAM= 'MOnth-Data (UMo) Problem'
          END IF
        ELSE IF (BLKCOD.EQ.10) THEN
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'PAthnames ('//PTHNAM(1)(1:LPTH)//'UPa) Problem'
          ELSE
C           no pathname
            WNDNAM= 'PAthnames (UPa) Problem'
          END IF
        END IF
        CALL ZWNSET (WNDNAM)
        CALL ADDBLK (MESSFL,SCLU,SGRP,ID,I1,
     M               SREC,EREC)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ADDBLK
     I                   (MESSFL,SCLU,TGRP,IDNO,IPRMPT,
     M                    SREC,EREC)
C
C     + + + PURPOSE + + +
C     create a block if it does not exist, return starting and
C     ending records of created block, flag to prompt user if
C     wish to create this block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        MESSFL,SCLU,TGRP,IDNO,IPRMPT,SREC,EREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - cluster number of screen messages
C     TGRP   - group number of table
C     IDNO   - id number for this block
C     IPRMPT - flag to prompt user with question (1-ask,0-just do it)
C     SREC   - starting record of new block
C     EREC   - ending record of new block
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      NFLDS,SCOL(30),FLEN(30),APOS(30),IMIN(30),I0,I1,I3,
     $             NMHDRW,RETCOD,IMAX(30),IDEF(30),RESP,I78,CLEN,CONT,
     $             SGRP,I,KEY,J,IPOS,IM1,TMPID,I12,INITFG,
     $             KWDTYP(25),TCLU
      REAL         RMIN(30),RMAX(30),RDEF(30)
      CHARACTER*1  FTYP(30),HDRBUF(78,5),HBEXT(2),BLNK,CBLNK(3),CSTR(3),
     $             TNMBUF(12),TMPBUF(78)
      CHARACTER*12 TNAME
      CHARACTER*80 UCIBF,CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER      STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL     WMSGTX,GETSE,QRESP,PUTUCI,STRFND,CHRCHR
      EXTERNAL     GETUCI,PUTKWD,CVARAR,WMSGTT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CSTR /'*','*','*'/
      DATA CBLNK/' ',' ',' '/
C
C     + + + INPUT FORMATS + + +
 1080 FORMAT (16X,I4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (80A1)
 2010 FORMAT (A12)
 2020 FORMAT ('END ',A12)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      IM1= -1
      I1 = 1
      I3 = 3
      I12= 12
      I78= 78
      BLNK = ' '
C
      RESP= 1
      IF (IPRMPT.EQ.1) THEN
C       this block does not exist, do you want to add it?
        SGRP= 78
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
      END IF
      IF (RESP.EQ.1) THEN
C       user wants to add this block, figure out where to put it
        TMPID = IDNO
 10     CONTINUE
          IF (TMPID.GT.1) THEN
            TMPID= TMPID - 1
            CALL GETSE (TMPID,I1,
     O                  SREC,EREC)
          END IF
        IF (SREC.EQ.0 .AND. TMPID.GT.1) GO TO 10
C       find corresponding keyword
        SGRP  = 22
        TCLU  = 201
        INITFG= 1
        CLEN  = 80
        I     = 0
        TNAME = ' '
 20     CONTINUE
          I= I+ 1
          CALL WMSGTT (MESSFL,TCLU,SGRP,INITFG,
     M                 CLEN,
     O                 CHSTR1,CONT)
          READ(CHSTR,1080) KWDTYP(I)
          IF (KWDTYP(I).EQ.IDNO) THEN
C           found keyword we need
            TNAME = CHSTR(1:12)
          END IF
          INITFG= 0
          CLEN  = 80
        IF (CONT.EQ.1 .AND. TNAME.EQ.'            ') GO TO 20
C       build table header
        WRITE (UCIBF,2010) TNAME
C       need to start this table after erec
        KEY = EREC
        CALL PUTUCI (UCIBF,I0,KEY)
C       get next record number
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        SREC = KEY
C
C       put header lines for this table
C       get this tables format info
        CALL WMSGTX (MESSFL,SCLU,TGRP,
     O               NFLDS,SCOL,FLEN,FTYP,APOS,IMIN,IMAX,IDEF,
     O               RMIN,RMAX,RDEF,
     O               NMHDRW,HDRBUF,RETCOD)
        DO 100 I= 1,NMHDRW
C         need to move header over cuz 0th field not to go on uci file
          J = 75
          CALL CHRCHR (J,HDRBUF(4,I),TMPBUF(1))
          CALL CHRCHR (J,TMPBUF(1),HDRBUF(1,I))
          HDRBUF(76,I) = ' '
          HDRBUF(77,I) = ' '
          HDRBUF(78,I) = ' '
          IF (STRFND(I78,HDRBUF(1,I),I3,CSTR).EQ.0) THEN
C           no comment string found, add one
            IPOS= STRFND(I78,HDRBUF(1,I),I3,CBLNK)
            CALL CHRCHR (I3,CSTR,HDRBUF(IPOS,I))
          END IF
          HBEXT(1)= BLNK
          HBEXT(2)= BLNK
          WRITE (UCIBF,2000)
     1          (HDRBUF(J,I),J=1,78),HBEXT
          CALL PUTUCI (UCIBF,IM1,KEY)
C         get next record number
          CALL GETUCI (IM1,
     M                 KEY,
     O                 UCIBF)
 100    CONTINUE
C
        IF (IPRMPT.EQ.1) THEN
C         user asked for this block, put one line
          UCIBF = ' '
          CALL PUTUCI (UCIBF,I0,KEY)
C         get next record number
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
        END IF
C       put end table header
        WRITE (UCIBF,2020) TNAME
        CALL PUTUCI (UCIBF,I0,KEY)
C       get next record number
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        EREC = KEY
C
C       data structure needs to know that this table now exists
        CALL CVARAR (I12,TNAME,I12,TNMBUF)
        CALL PUTKWD (I1,IDNO,TNMBUF,SREC,EREC,
     O               RETCOD)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DELBLK
     I                   (IDNO)
C
C     + + + PURPOSE + + +
C     delete this block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        IDNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDNO   - id number for this block
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I1,SREC,EREC,KEY,SAVKEY
      CHARACTER*80 UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL     GETUCI,DELUCI,GETSE
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      CALL GETSE (IDNO,I1,
     O            SREC,EREC)
      KEY= SREC
      CALL DELUCI (KEY)
      I  = -2
C     delete all remaining records from this block
 10   CONTINUE
        CALL GETUCI (I,
     M               KEY,
     O               UCIBF)
        SAVKEY = KEY
        CALL DELUCI (KEY)
      IF (SAVKEY.NE.EREC) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   COMUCI
     I                   (KEY,
     M                    UCIBF)
C
C     + + + PURPOSE + + +
C     Comment out a uci line
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      KEY
      CHARACTER*80 UCIBF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEY    - record to comment
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I80,I3,IPOS
      CHARACTER*1  IBUF1(80),CMNT(3),CBLNK(3)
C
C     + + + FUNCTIONS + + +
      INTEGER   STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL  CVARAR,STRFND,CHRCHR,CARVAR,COMKEY
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CMNT/'*','*','*'/
      DATA CBLNK/' ',' ',' '/
C
C     + + + END SPECIFICATIONS + + +
C
      I3  = 3
      I80 = 80
      CALL CVARAR (I80,UCIBF,I80,IBUF1)
C
C     want to comment this record
      IF (STRFND(I80,IBUF1,I3,CMNT).EQ.0) THEN
C       no comment string found, add one
        IPOS = STRFND(I80,IBUF1,I3,CBLNK)
        CALL CHRCHR (I3,CMNT,IBUF1(IPOS))
C       change record type in in-memory uci
        CALL COMKEY (KEY)
      END IF
C
      CALL CARVAR (I80,IBUF1,I80,UCIBF)
C
      RETURN
      END
C
C
C
      SUBROUTINE   UMSPEC
     I                   (MESSFL,SCLU,PTHNAM)
C
C     + + + PURPOSE + + +
C     Modify parameter tables for special actions from UCI file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     PTHNAM - character string of path of options selected to get here
C
C     + + + PARAMETERS + + +
      INTEGER   MXSPEC
      PARAMETER (MXSPEC=100)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I1,I78,SREC,EREC,KEY,BROW,NROW,ID,DONFG,
     2             SGRP,IRET,LPTH,RESP,COUNT,CVAL(1,MXSPEC),KEY1,LKEY,
     3             TOTROW,FOUND,ADRCTY,REPFLG,TROW,PRVROW,ANOROW
      CHARACTER*1  TBUFF(78,MXSPEC)
      CHARACTER*48 WNDNAM
      CHARACTER*78 TBUF78
      CHARACTER*80 UCIBF
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL    Q2INIT,QSTBFB,QGTBFB,Q2EDIT,QRESP,Q2GTCO,UMSPAD,UMSPNA
      EXTERNAL    ZLNTXT,ZWNSOP,ZWNSET,GETSE,GETUCI,REPUCI,CARVAR,CVARAR
      EXTERNAL    DELBLK,DELUCI,DELKWD,PUTUCI,COMUCI,ADDBLK,UMSPMD
      EXTERNAL    PREUCI,MLCCHK,PRNTXT
C
C     + + + INPUT FORMATS + + +
 1003 FORMAT(I3)
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      I1   = 1
      I78  = 78
C
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))
C
      IF (LPTH .GT. 0) THEN
C       path name available
        WNDNAM= 'SPec-actions ('//PTHNAM(1)(1:LPTH)//'USp)'
      ELSE
C       no pathname
        WNDNAM= 'SPec-actions (USp)'
      END IF
C
C     check to see if any special actions records exist
      ID = 9
      CALL GETSE (ID,I1,
     O            SREC,EREC)
      IF (SREC.NE.0) THEN
C       some records exist in this table
C
C       do intermediate menu for which type of special actions record
 10     CONTINUE
          SGRP = 40
          CALL ZWNSET (WNDNAM)
          CALL QRESP (MESSFL,SCLU,SGRP,RESP)
          IF (RESP.NE.5) THEN
C           want to search for a particular type of record
 40         CONTINUE
              REPFLG= 0
              TOTROW= 0
              KEY   = SREC
              BROW  = 1
              NROW  = 0
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
 50           CONTINUE
C               fill in screen with records from in-memory uci
                IF (UCIBF(3:8).EQ.'UVQUAN') THEN
                  TOTROW = TOTROW + 1
                  IF (RESP.EQ.4 .AND. MXSPEC.GT.NROW) THEN
C                   user defined variable quantity name
                    TBUF78= 'NONE   ' // UCIBF(10:80)
                    NROW  = NROW + 1
                    CALL CVARAR (I78,TBUF78,I78,TBUFF(1,NROW))
                  END IF
                ELSE IF (UCIBF(3:8).EQ.'DISTRB') THEN
                  TOTROW = TOTROW + 1
                  IF (RESP.EQ.2 .AND. MXSPEC.GT.NROW) THEN
C                   distribute action definition
                    TBUF78= 'NONE  ' // UCIBF(9:80)
                    NROW  = NROW + 1
                    CALL CVARAR (I78,TBUF78,I78,TBUFF(1,NROW))
                  END IF
                ELSE IF (UCIBF(3:8).EQ.'UVNAME') THEN
C                 user defined variable name
                  TOTROW = TOTROW + 1
                  IF (RESP.EQ.3 .AND. MXSPEC.GT.NROW) THEN
                    NROW  = NROW + 1
                    TBUF78= 'NONE    ' // UCIBF(11:80)
                    CALL CVARAR (I78,TBUF78,I78,TBUFF(1,NROW))
                  END IF
C                 need to see if more lines go with this one
                  READ (UCIBF(17:19),1003) COUNT
                  IF (COUNT .GT. 2) THEN
 55                 CONTINUE
                      CALL GETUCI (I0,
     M                             KEY,
     O                             UCIBF)
                      IF (RESP.EQ.3 .AND. MXSPEC.GT.NROW) THEN
                        NROW  = NROW + 1
                        TBUF78= 'NONE    ' // UCIBF(11:80)
                        CALL CVARAR (I78,TBUF78,I78,TBUFF(1,NROW))
                      END IF
                      COUNT= COUNT- 2
                      TOTROW = TOTROW + 1
                    IF (COUNT .GT. 2) GO TO 55
                  END IF
                ELSE
C                 old style action or conditional
                  TOTROW = TOTROW + 1
                  IF (RESP.EQ.1 .AND. MXSPEC.GT.NROW) THEN
                    NROW  = NROW + 1
                    TBUF78= 'NO ' // UCIBF(1:75)
                    CALL CVARAR (I78,TBUF78,I78,TBUFF(1,NROW))
                  END IF
                END IF
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
              IF (KEY.NE.EREC) GO TO 50
C
              IF (NROW.EQ.0) THEN
C               no records of this type exist
                IF (LPTH .GT. 0) THEN
C                 path name available
                  WNDNAM= 'SPec-actions ('//PTHNAM(1)(1:LPTH)//
     $                    'USp) Problem'
                ELSE
C                 no pathname
                  WNDNAM= 'SPec-actions (USp) Problem'
                END IF
                CALL ZWNSET (WNDNAM)
C               this type of record does not exist, want to add it?
                SGRP= 49
                ADRCTY= 1
                CALL QRESP (MESSFL,SCLU,SGRP,ADRCTY)
                IF (ADRCTY.EQ.1) THEN
C                 user wants to add a record of this type
C                 figure out where to put it
                  KEY = SREC
C                 put one line
                  IF (RESP.EQ.2) THEN
                    UCIBF = '  DISTRB'
                  ELSE IF (RESP.EQ.3) THEN
                    UCIBF = '  UVNAME'
                  ELSE IF (RESP.EQ.4) THEN
                    UCIBF = '  UVQUAN'
                  ELSE IF (RESP.EQ.1) THEN
                    UCIBF = ' '
                  END IF
                  CALL PUTUCI (UCIBF,I1,KEY)
                  REPFLG = 1
                END IF
              ELSE
C               some records of this type exist
                IF (NROW.EQ.MXSPEC) THEN
C                 filled buffer with max number of rows, tell user
                  CALL ZWNSET (WNDNAM)
                  SGRP = 59
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                END IF
                SGRP = 40+RESP
C               initialize screen
                CALL Q2INIT (MESSFL,SCLU,SGRP)
C               put these records on the screen
                CALL QSTBFB (I78,NROW,BROW,TBUFF)
C               let user edit screen
                CALL ZWNSET (WNDNAM)
                CALL Q2EDIT (NROW,
     O                       IRET)
                IF (IRET.EQ.1) THEN
C                 user wants to continue
C                 get records from the screen
                  CALL QGTBFB (I78,NROW,BROW,
     O                         TBUFF)
                  CALL Q2GTCO (I1,NROW,
     O                         CVAL)
                  IF (RESP.EQ.3) THEN
C                   move actions from secondary to primary rows
                    TROW = NROW
 58                 CONTINUE
                      CALL CARVAR (I78,TBUFF(1,TROW),I78,TBUF78)
                      IF (CVAL(1,TROW).NE.1 .AND.
     1                    TBUF78(9:14).EQ.'      ') THEN
C                       this is a secondary line, action should be taken
C                       on line above
                        CVAL(1,TROW-1) = CVAL(1,TROW)
                      END IF
                      TROW = TROW - 1
                    IF (TROW.GT.1) GO TO 58
                  ELSE IF (RESP.EQ.1) THEN
C                   action or conditional line
C                   see if any lines are part of a multi-line conditional
C                   and if so, apply action to all lines
                    CALL MLCCHK (NROW,TBUFF,
     M                           CVAL)
                  END IF
                  KEY = SREC
                  BROW= 1
                  NROW= 0
                  CALL GETUCI (I0,
     M                         KEY,
     O                         UCIBF)
                  DONFG = 0
 60               CONTINUE
                    PRVROW= 0
                    FOUND = 0
                    IF (UCIBF(3:8).EQ.'UVQUAN') THEN
                      IF (RESP.EQ.4 .AND. MXSPEC.GT.NROW) THEN
C                       user defined variable quantity name
                        FOUND= 1
                        NROW = NROW + 1
                        CALL CARVAR (I78,TBUFF(1,NROW),I78,TBUF78)
                        UCIBF(10:80) = TBUF78(8:78)
C                       put this record back to in-memory uci
                        CALL REPUCI (KEY,UCIBF)
                      END IF
                    ELSE IF (UCIBF(3:8).EQ.'DISTRB') THEN
                      IF (RESP.EQ.2 .AND. MXSPEC.GT.NROW) THEN
C                       distribute action definition
                        FOUND= 1
                        NROW = NROW + 1
                        CALL CARVAR (I78,TBUFF(1,NROW),I78,TBUF78)
                        UCIBF(9:80) = TBUF78(7:78)
C                       put this record back to in-memory uci
                        CALL REPUCI (KEY,UCIBF)
                      END IF
                    ELSE IF (UCIBF(3:8).EQ.'UVNAME') THEN
                      IF (RESP.EQ.3 .AND. MXSPEC.GT.NROW) THEN
C                       user defined action name
                        FOUND= 1
                        NROW = NROW + 1
                        CALL CARVAR (I78,TBUFF(1,NROW),I78,TBUF78)
                        UCIBF(11:80) = TBUF78(9:78)
C                       put this record back to in-memory uci
                        CALL REPUCI (KEY,UCIBF)
                      END IF
                      KEY1 = KEY
C                     need to see if more lines go with this one
                      READ (UCIBF(17:19),1003) COUNT
                      IF (COUNT .GT. 2) THEN
C                       multiple lines make up this item
 65                     CONTINUE
                          CALL GETUCI (I0,
     M                                 KEY,
     O                                 UCIBF)
                          IF (RESP.EQ.3 .AND. MXSPEC.GT.NROW) THEN
C                           user defined action name
                            CVAL(1,NROW+1) = CVAL(1,NROW)
                            PRVROW= PRVROW+1
                            NROW  = NROW + 1
                          END IF
                          COUNT = COUNT- 2
                        IF (COUNT .GT. 2) GO TO 65
                      END IF
                    ELSE
                      IF (RESP.EQ.1 .AND. MXSPEC.GT.NROW) THEN
C                       old style action or conditional
                        FOUND= 1
                        NROW = NROW + 1
                      END IF
                    END IF
C                   see if we want to copy, delete, or comment it
                    IF (FOUND.EQ.1) THEN
C                     we found a record to check
                      IF (CVAL(1,NROW).EQ.2) THEN
C                       user wants to drop this line
 70                     CONTINUE
                          IF (TOTROW.GT.1) THEN
C                           okay to delete a line
                            CALL DELUCI (
     M                                   KEY)
                            TOTROW = TOTROW - 1
                            REPFLG = 1
                          ELSE
C                           deleting this line means deleting this block
                            CALL DELBLK (ID)
                            CALL DELKWD (ID)
                            REPFLG= 0
                            DONFG = 1
                            RESP  = 6
                          END IF
                          ANOROW = 0
                          IF (PRVROW.NE.0) THEN
C                           need to do this for a previous row
                            PRVROW = PRVROW -1
                            ANOROW = 1
                          END IF
                        IF (ANOROW.EQ.1) GO TO 70
                      ELSE IF (CVAL(1,NROW).EQ.3) THEN
C                       user wants to copy this line
                        LKEY = KEY
 80                     CONTINUE
                          REPFLG = 1
                          CALL PUTUCI (UCIBF,I1,KEY)
                          TOTROW = TOTROW + 1
                          ANOROW = 0
                          IF (PRVROW.NE.0) THEN
C                           need to do this for a previous row
                            CALL PREUCI
     M                                 (LKEY)
                            CALL PREUCI
     M                                 (LKEY)
                            CALL GETUCI (I0,
     M                                   LKEY,
     O                                   UCIBF)
                            PRVROW = PRVROW -1
                            ANOROW = 1
                          END IF
                        IF (ANOROW.EQ.1) GO TO 80
                        CALL GETUCI (I0,
     M                               KEY,
     O                               UCIBF)
                      ELSE IF (CVAL(1,NROW).EQ.4) THEN
C                       user wants to comment out this line
                        LKEY = KEY
 90                     CONTINUE
                        IF (TOTROW.GT.1) THEN
C                         okay to comment a line
                          CALL COMUCI (LKEY,
     M                                 UCIBF)
                          CALL REPUCI (LKEY,UCIBF)
                          REPFLG = 1
                          ANOROW = 0
                          IF (PRVROW.NE.0) THEN
C                           need to do this for a previous row
                            CALL PREUCI
     M                                 (LKEY)
                            CALL PREUCI
     M                                 (LKEY)
                            CALL GETUCI (I0,
     M                                   LKEY,
     O                                   UCIBF)
                            PRVROW = PRVROW -1
                            ANOROW = 1
                          END IF
                        IF (ANOROW.EQ.1) GO TO 90
                        END IF
                      ELSE IF (CVAL(1,NROW).EQ.5) THEN
C                       user wants to modify this line
                        REPFLG = 1
                        CALL ZWNSET (WNDNAM)
                        IF (RESP.EQ.1) THEN
C                         modify action line
                          CALL UMSPMD (MESSFL,SCLU,WNDNAM,KEY)
                        ELSE IF (RESP.EQ.3) THEN
C                         modify name line
                          CALL UMSPNA (MESSFL,SCLU,WNDNAM,KEY1)
                        END IF
                      ELSE IF (CVAL(1,NROW).EQ.6) THEN
C                       user wants to add a record
                        REPFLG = 1
                        CALL UMSPAD (MESSFL,SCLU,WNDNAM,
     M                               KEY)
                        TOTROW = TOTROW + 1
                      END IF
                    END IF
                    IF (DONFG.EQ.0) THEN
                      CALL GETUCI (I0,
     M                             KEY,
     O                             UCIBF)
                    END IF
                  IF (KEY.NE.EREC .AND. DONFG.EQ.0) GO TO 60
                END IF
              END IF
            IF (REPFLG.EQ.1) GO TO 40
          END IF
        IF (RESP.NE.5) GO TO 10
C
      ELSE
C       no special actions records, tell user
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'SPec-actions ('//PTHNAM(1)(1:LPTH)//'USp) Problem'
        ELSE
C         no pathname
          WNDNAM= 'SPec-actions (USp) Problem'
        END IF
        CALL ZWNSET (WNDNAM)
        SGRP = 41
        CALL ADDBLK (MESSFL,SCLU,SGRP,ID,I1,
     M               SREC,EREC)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   UMSPAD
     I                   (MESSFL,SCLU,WNDNAM,
     M                    KEY)
C
C     + + + PURPOSE + + +
C     add a special actions record (action or condition)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,SCLU,KEY
      CHARACTER*48 WNDNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     KEY    - key of current uci record
C     WNDNAM - window name
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SGRP,IRET,I1,I0,I2,
     1             MSEL(2),OPVAL(2)
      CHARACTER*80 UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL     Q1INIT,Q1EDIT,QSETOP,QGETOP,ZWNSET,PREUCI
      EXTERNAL     GETUCI,PUTUCI
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I2  = 2
C     do screen to specify which type to add and where
      CALL ZWNSET (WNDNAM)
      SGRP = 39
      CALL Q1INIT (MESSFL,SCLU,SGRP)
      OPVAL(1) = 1
      OPVAL(2) = 1
      MSEL(1) = 1
      MSEL(2) = 1
      CALL QSETOP (I2,I2,MSEL,MSEL,OPVAL)
      CALL Q1EDIT (IRET)
      IF (IRET.EQ.1) THEN
C       user wants to continue
        CALL QGETOP (I2,OPVAL)
        IF (OPVAL(1).EQ.1) THEN
C         if condition
          UCIBF = 'IF     THEN'
        ELSE IF (OPVAL(1).EQ.2) THEN
C         else condition
          UCIBF = 'ELSE'
        ELSE IF (OPVAL(1).EQ.3) THEN
C         else if condition
          UCIBF = 'ELSE IF     THEN'
        ELSE IF (OPVAL(1).EQ.4) THEN
C         end if condition
          UCIBF = 'END IF'
        ELSE IF (OPVAL(1).EQ.5) THEN
C         action line
          UCIBF = ' '
        END IF
        IF (OPVAL(2).EQ.1) THEN
C         add before
          CALL PREUCI
     M                (KEY)
C         put this record after the previous one
          CALL PUTUCI (UCIBF,I1,KEY)
C         go ahead two records
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
        ELSE IF (OPVAL(2).EQ.2) THEN
C         add after
          CALL PUTUCI (UCIBF,I1,KEY)
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MLCCHK
     I                   (NROW,TBUFF,
     M                    CVAL)
C
C     + + + PURPOSE + + +
C     check to see if records are part of a multi-line condition,
C     if so make all actions for this condition the same
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      NROW,CVAL(1,NROW)
      CHARACTER*1  TBUFF(78,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NROW   - number of rows of action/condition records
C     TBUFF  - array of character strings for each record
C     CVAL   - array of actions entered for each record
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,STWORD,BUFLEN,POS,ITMTYP,
     2             I78,I80,CSTART,CEND,ACTION,J
      CHARACTER*10 CITEM
      CHARACTER*78 TBUF78
      CHARACTER*80 UCIBF
C
C     + + + FUNCTIONS + + +
      INTEGER      CKNBLV,LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     CKNBLV,SPITEM,LENSTR,CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
      I78 = 78
      I80 = 80
C
      I = 0
 10   CONTINUE
        I = I + 1
C       check all rows
        CALL CARVAR (I78,TBUFF(1,I),I78,TBUF78)
        UCIBF = TBUF78(4:78) // '     '
C
        STWORD= CKNBLV (I80,UCIBF)
        IF (STWORD .LT. 1) THEN
C         set dummy stword on blank line
          STWORD= 1
        END IF
        CSTART = 0
        CEND   = 0
        IF (UCIBF(STWORD:STWORD+2) .EQ. 'IF ') THEN
C         if condition
          POS    = STWORD+ 3
          CSTART = I
        ELSE IF (UCIBF(STWORD:STWORD+7) .EQ. 'ELSE IF ') THEN
C         else if condition
          POS    = STWORD+ 8
          CSTART = I
        END IF
        IF (CSTART.GT.0) THEN
C         we have found the start of a condition, look for end
          BUFLEN= LENSTR (I80,UCIBF)
C         begin do-until loop on items
 20       CONTINUE
            IF (POS .LE. BUFLEN) THEN
C             more to do on this line
C             get next item
              CALL SPITEM (UCIBF,BUFLEN,
     M                     POS,
     O                     CITEM,ITMTYP)
              IF (ITMTYP.EQ.11) THEN
C               found delimeter
                CEND = I
              END IF
            ELSE
C             need to get next line
              I = I + 1
              CALL CARVAR (I78,TBUFF(1,I),I78,TBUF78)
              UCIBF = TBUF78 // '  '
              STWORD= CKNBLV (I80,UCIBF)
              IF (STWORD .LT. 1) THEN
C               set dummy stword on blank line
                STWORD= 1
              END IF
              BUFLEN= LENSTR (I80,UCIBF)
              POS   = STWORD
            END IF
          IF (CEND .EQ. 0) GO TO 20
C         found end of this conditional
          IF (CEND.GT.CSTART) THEN
C           have to do some checking
            ACTION = 0
            DO 30 J = CSTART,CEND
              IF (CVAL(1,J).EQ.2 .OR. CVAL(1,J).EQ.3 .OR.
     1            CVAL(1,J).EQ.4) THEN
C               remember that an action is wanted for this row
                ACTION = CVAL(1,J)
              END IF
 30         CONTINUE
            IF (ACTION.NE.0) THEN
              DO 40 J = CSTART,CEND
C               set all rows to this action
                CVAL(1,J) = ACTION
 40           CONTINUE
            END IF
          END IF
        END IF
      IF (I.LT.NROW) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   UMSPMD
     I                   (MESSFL,SCLU,WNDNAM,KEY)
C
C     + + + PURPOSE + + +
C     modify a special actions record (action or condition)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,SCLU,KEY
      CHARACTER*48 WNDNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     KEY    - key to this line of uci file
C     WNDNAM - window name
C
C     + + + PARAMETERS + + +
      INTEGER   MXCOND
      PARAMETER (MXCOND=10)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,STWORD,CLEN(10),TLEN,SGRP,IVAL(15),IRET,I1,I4,I10,
     1             MSEL(1),BUFLEN,POS,DONEFG,ITMTYP,SPCNT,I0,
     2             P1CNT,P2CNT,Q1CNT,CNUM,OPVAL(1),J,BLKCNT,LKEY,I2,
     3             LUSED,STWOR2,IPOS,LCNT,CVAL(1,MXCOND),CEXIST,ID,
     4             RETFLG,ERRCOD
      CHARACTER*1  TXT(35),P1(MXCOND,3),P2(MXCOND,3),BLNK,
     1             CTXT(34,MXCOND)
      CHARACTER*4  COMP(MXCOND),LOGI(MXCOND)
      CHARACTER*10 CITEM,QUAN1(MXCOND,2)
      CHARACTER*34 CTXT34,TMP34
      CHARACTER*80 UCIBF
C
C     + + + FUNCTIONS + + +
      INTEGER      CKNBLV,LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     CKNBLV,QGETI,QGETCT,QSETI,QSETCT,Q1INIT,Q1EDIT
      EXTERNAL     QSETOP,QGETOP,SPITEM,LENSTR,Q2GCTB,Q2SCTB,Q2INIT
      EXTERNAL     Q2EDIT,ZIPC,CVARAR,ZWNSET,CARVAR,PREUCI,GETUCI
      EXTERNAL     REPUCI,PUTUCI,Q2STCO,Q2GTCO,PRNTXT,DELUCI
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(2X,6A1,I3,I4,2A1,I3,I4,4I3,2I2,2X,6A1,3I3,16A1,1X,2I3)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(2X,6A1,I3,I4,2A1,I3,I4,4I3,2I2,2X,6A1,3I3,16A1,1X,2I3)
 2010 FORMAT(2X,6A1,I3,I4,23X,I2,2X,6A1,3I3,13A1,10X)
C
C     + + + END SPECIFICATIONS + + +
C
      BLNK= ' '
      I0  = 0
      I1  = 1
      I2  = 2
      I4  = 4
      I10 = 10
C     find first word to check if free-form conditional line
      LKEY = KEY
      CALL PREUCI
     M            (LKEY)
      CALL GETUCI (I0,
     M             LKEY,
     O             UCIBF)
      LUSED= 1
      I = 80
      STWORD= CKNBLV (I,UCIBF)
      IF (STWORD .LT. 1) THEN
C       set dummy stword on blank line
        STWORD= 1
      END IF
      IF ((UCIBF(STWORD:STWORD+2) .EQ. 'IF ') .OR.
     1    (UCIBF(STWORD:STWORD+3) .EQ. 'ELSE') .OR.
     1    (UCIBF(STWORD:STWORD+5) .EQ. 'END IF')) THEN
C       this is a condition line
        CALL ZWNSET (WNDNAM)
        SGRP = 46
        CALL Q1INIT (MESSFL,SCLU,SGRP)
C       determine which type
        IF (UCIBF(STWORD:STWORD+2) .EQ. 'IF ') THEN
C         if condition
          OPVAL(1)= 1
          CEXIST  = 1
          POS     = STWORD+ 3
        ELSE IF (UCIBF(STWORD:STWORD+7) .EQ. 'ELSE IF ') THEN
C         else if condition
          OPVAL(1)= 3
          CEXIST  = 1
          POS     = STWORD+ 8
        ELSE IF (UCIBF(STWORD:STWORD+3) .EQ. 'ELSE') THEN
C         else condition
          OPVAL(1)= 2
          CEXIST  = 0
        ELSE IF (UCIBF(STWORD:STWORD+5) .EQ. 'END IF') THEN
C         end if condition
          OPVAL(1)= 4
          CEXIST  = 0
        END IF
        MSEL(1) = 1
        CALL QSETOP (I1,I1,MSEL,MSEL,OPVAL)
        CALL Q1EDIT (IRET)
        IF (IRET.EQ.1) THEN
C         user wants to continue
          CALL QGETOP (I1,OPVAL)
          IF (CEXIST.EQ.1) THEN
C           some conditions exist, need to parse out rest of line
            I     = 80
            BUFLEN= LENSTR (I,UCIBF)
            DONEFG= 0
            SPCNT = 1
            P1CNT = 1
            P2CNT = 1
            I = 30
            CALL ZIPC (I,BLNK,P1)
            CALL ZIPC (I,BLNK,P2)
            Q1CNT = 1
C           begin do-until loop on items
 10         CONTINUE
              IF (POS .LE. BUFLEN) THEN
C               more to do on this line
C               get next item
                CALL SPITEM (UCIBF,BUFLEN,
     M                       POS,
     O                       CITEM,ITMTYP)
C               process item by code
                GO TO (100,100,100,110,110,110,120,130,140,140,150)
     $                 ITMTYP
 100            CONTINUE
C                 left parenthesis
                  P1(SPCNT,P1CNT) = CITEM(1:1)
                  P1CNT = P1CNT + 1
                GO TO 200
 110            CONTINUE
C                 right parenthesis
                  P2(SPCNT,P2CNT) = CITEM(1:1)
                  P2CNT = P2CNT + 1
                GO TO 200
 120            CONTINUE
C                 quantity
                  QUAN1(SPCNT,Q1CNT) = CITEM
                  Q1CNT = Q1CNT + 1
                GO TO 200
 130            CONTINUE
C                 comparison operator
                  COMP(SPCNT) = CITEM(1:4)
C                 initialize logical operator as well
                  LOGI(SPCNT) = '    '
                GO TO 200
 140            CONTINUE
C                 logical operator
                  LOGI(SPCNT) = CITEM(1:4)
                  SPCNT = SPCNT + 1
C                 start counts over for new conditional
                  P1CNT = 1
                  P2CNT = 1
                  Q1CNT = 1
                GO TO 200
 150            CONTINUE
C                 delimiter found
                  DONEFG = 1
C               end computed goto
 200            CONTINUE
              ELSE
C               need to get next line
                CALL GETUCI (I0,
     M                       LKEY,
     O                       UCIBF)
                I = 80
                STWOR2= CKNBLV (I,UCIBF)
                IF (STWOR2 .LT. 1) THEN
C                 set dummy stword on blank line
                  STWOR2= 1
                END IF
                BUFLEN= LENSTR (I,UCIBF)
                POS   = STWOR2
                LUSED = LUSED + 1
              END IF
C           end of do-until loop on items
            IF (DONEFG .EQ. 0) GO TO 10
          ELSE IF (CEXIST.EQ.0) THEN
C           no conditions exist yet, fill in a dummy
            P1(1,1) = 'B'
            P1(1,2) = 'B'
            P1(1,3) = 'B'
            P2(1,1) = 'B'
            P2(1,2) = 'B'
            P2(1,3) = 'B'
            QUAN1(1,1)= '          '
            QUAN1(1,2)= '          '
            COMP(1) = '    '
            LOGI(1) = '    '
            SPCNT   = 1
            LUSED   = 1
          END IF
          IF (OPVAL(1).EQ.1 .OR. OPVAL(1).EQ.3) THEN
C           if or else if condition wanted
C           set values for character fields
            CNUM = 10
            CLEN(1) = 1
            CLEN(2) = 1
            CLEN(3) = 1
            CLEN(4) = 10
            CLEN(5) = 4
            CLEN(6) = 10
            CLEN(7) = 1
            CLEN(8) = 1
            CLEN(9) = 1
            CLEN(10)= 4
            TLEN    = 34
            DO 250 I = 1,SPCNT
C             fill in character buffer for each row setting defaults
              DO 255 J = 1,3
                IF (P1(I,J).EQ.' ') THEN
                  CTXT(J,I) = 'B'
                ELSE
                  CTXT(J,I) = P1(I,J)
                END IF
 255          CONTINUE
              DO 260 J = 1,3
                IF (P2(I,J).EQ.' ') THEN
                  CTXT(J+27,I) = 'B'
                ELSE
                  CTXT(J+27,I) = P2(I,J)
                END IF
 260          CONTINUE
              CALL CVARAR (I10,QUAN1(I,1),I10,CTXT(4,I))
              CALL CVARAR (I4,COMP(I),I4,CTXT(14,I))
              CALL CVARAR (I10,QUAN1(I,2),I10,CTXT(18,I))
              IF (LOGI(I).EQ.'    ') THEN
                LOGI(I) = 'BLNK'
              END IF
              CALL CVARAR (I4,LOGI(I),I4,CTXT(31,I))
              CVAL(1,I) = 1
 250        CONTINUE
C
 5          CONTINUE
C             return here if error in entering conditions
              ERRCOD = 0
 15           CONTINUE
C               return here while looking at each action
C               do screen to edit fields
                CALL ZWNSET (WNDNAM)
                SGRP = 47
                CALL Q2INIT (MESSFL,SCLU,SGRP)
                CALL Q2SCTB (CNUM,CLEN,TLEN,I2,SPCNT,CTXT)
                CALL Q2STCO (I1,SPCNT,CVAL)
                CALL Q2EDIT (SPCNT,
     O                       IRET)
                IF (IRET.EQ.1) THEN
C                 user wants to continue
                  CALL Q2GCTB (CNUM,CLEN,TLEN,I2,SPCNT,
     O                         CTXT)
                  CALL Q2GTCO (I1,SPCNT,
     O                         CVAL)
                  RETFLG= 0
                  I = 1
 20               CONTINUE
C                   check each condition for action desired
                    IF (CVAL(1,I).EQ.2) THEN
C                     drop this condition
                      RETFLG = 1
                      IF (SPCNT.LE.1) THEN
C                       can't drop if only one, tell user
                        SGRP = 38
                        CALL ZWNSET (WNDNAM)
                        CALL PRNTXT (MESSFL,SCLU,SGRP)
                        I = I + 1
                      ELSE
C                       drop this condition
                        SPCNT = SPCNT-1
                        IF (SPCNT.GE.I) THEN
C                         there are more lines to follow, move up
                          DO 30 ID= I,SPCNT
                            CVAL(1,ID) = CVAL(1,ID+1)
                            DO 35 J= 1,34
                              CTXT(J,ID) = CTXT(J,ID+1)
 35                         CONTINUE
 30                       CONTINUE
                        END IF
                      END IF
                    ELSE IF (CVAL(1,I).EQ.3) THEN
C                     want to copy this condition
                      SPCNT = SPCNT+1
                      CVAL(1,I) = 1
                      IF (SPCNT .LE. MXCOND) THEN
C                       room for a new condition
                        ID = SPCNT
 40                     CONTINUE
C                         move conditions after this one down
                          CVAL(1,ID) = CVAL(1,ID-1)
                          DO 45 J= 1,34
                            CTXT(J,ID) = CTXT(J,ID-1)
 45                       CONTINUE
                          ID = ID - 1
                        IF (ID.NE.I) GO TO 40
                        CVAL(1,I+1) = 1
                      END IF
                      RETFLG = 1
                      I = I + 1
                    ELSE
C                     look at the next condition
                      I = I + 1
                    END IF
                  IF (I.LE.SPCNT) GO TO 20
                END IF
              IF (IRET.EQ.1 .AND. RETFLG.EQ.1) GO TO 15
C
              IF (IRET.EQ.1) THEN
C               user accepted
                DO 50 J = 1,SPCNT
C                 check for errors
                  I = 34
                  CALL CARVAR (I,CTXT(1,J),I,CTXT34)
                  IF (J.EQ.SPCNT .AND. CTXT34(31:34).NE.'BLNK') THEN
C                   error, on last condition cannot have 'or' or 'and'
                    ERRCOD = 2
                  END IF
                  IF (CTXT34(31:34).EQ.'BLNK' .AND. J.NE.SPCNT) THEN
C                   this is an error, can only have blank on last condition
                    ERRCOD = 1
                  END IF
 50             CONTINUE
                IF (ERRCOD.EQ.0) THEN
C                 put conditionals back
                  IF (OPVAL(1).EQ.1) THEN
C                   if condition
                    UCIBF = ' '
                    UCIBF(STWORD:STWORD+2) = 'IF '
                    IPOS = 4
                  ELSE IF (OPVAL(1).EQ.3) THEN
C                   else if condition
                    UCIBF = ' '
                    UCIBF(STWORD:STWORD+6) = 'ELSE IF '
                    IPOS = 9
                  END IF
                  LKEY  = KEY
                  LCNT  = 1
                  DO 280 J = 1,SPCNT
C                   put to uci buffer each conditional
                    I = 34
                    CALL CARVAR (I,CTXT(1,J),I,CTXT34)
C                   remove default blanks
                    BLKCNT = 0
                    IF (J.EQ.SPCNT .AND. CTXT34(31:34).NE.'BLNK') THEN
C                     error, on last condition cannot have 'or' or 'and'
                      ERRCOD = 2
                    END IF
                    IF (CTXT34(31:34).EQ.'BLNK') THEN
                      CTXT34(31:34) = '    '
                      BLKCNT = BLKCNT + 4
                      IF (J.NE.SPCNT) THEN
C                       this is an error, can only have blank on last condition
                        ERRCOD = 1
                      END IF
                    END IF
                    IF (CTXT34(30:30).EQ.'B') THEN
                      TMP34  = CTXT34
                      CTXT34(30:34) = TMP34(31:34) // ' '
                      BLKCNT = BLKCNT + 1
                    END IF
                    IF (CTXT34(29:29).EQ.'B') THEN
                      TMP34  = CTXT34
                      CTXT34(29:34) = TMP34(30:34) // ' '
                      BLKCNT = BLKCNT + 1
                    END IF
                    IF (CTXT34(28:28).EQ.'B') THEN
                      TMP34  = CTXT34
                      CTXT34(28:34) = TMP34(29:34) // ' '
                      BLKCNT = BLKCNT + 1
                    END IF
                    IF (CTXT34(3:3).EQ.'B') THEN
                      TMP34  = CTXT34
                      CTXT34(3:34) = TMP34(4:34) // ' '
                      BLKCNT = BLKCNT + 1
                    END IF
                    IF (CTXT34(2:2).EQ.'B') THEN
                      TMP34  = CTXT34
                      CTXT34(2:34) = TMP34(3:34) // ' '
                      BLKCNT = BLKCNT + 1
                    END IF
                    IF (CTXT34(1:1).EQ.'B') THEN
                      TMP34  = CTXT34
                      CTXT34(1:34) = TMP34(2:34) // ' '
                      BLKCNT = BLKCNT + 1
                    END IF
                    IF (IPOS+34.LT.74) THEN
C                     room to add on this record
                      UCIBF(IPOS:IPOS+34) = CTXT34
                    ELSE
C                     filled this line, save it
                      IF (LCNT.LT.LUSED) THEN
C                       can replace existing line
                        CALL REPUCI (LKEY,UCIBF)
                        CALL GETUCI (I0,
     M                               LKEY,
     O                               UCIBF)
                      ELSE IF (LCNT.EQ.LUSED) THEN
C                       can replace existing line
                        CALL REPUCI (LKEY,UCIBF)
                      ELSE
C                       this is a new line
                        CALL PUTUCI (UCIBF,I1,LKEY)
                        CALL GETUCI (I0,
     M                               LKEY,
     O                               UCIBF)
                      END IF
                      UCIBF = ' '
                      IPOS  = 4
                      UCIBF(IPOS:IPOS+34) = CTXT34
                      LCNT  = LCNT + 1
                    END IF
                    IPOS = IPOS + 34 - BLKCNT
 280              CONTINUE
C                 need a 'then'
                  UCIBF(IPOS+1:IPOS+6) = ' THEN'
                  IF (LCNT.LE.LUSED) THEN
C                   can replace existing line
                    CALL REPUCI (LKEY,UCIBF)
                  ELSE
C                   this is a new line
                    CALL PUTUCI (UCIBF,I1,LKEY)
                  END IF
                  IF (LCNT.LT.LUSED) THEN
C                   want to remove remaining lines
                    DO 290 I = LCNT+1,LUSED
C                     get the next record and drop it
                      CALL GETUCI (I0,
     M                             LKEY,
     O                             UCIBF)
                      CALL DELUCI (LKEY)
 290                CONTINUE
                  END IF
                ELSE IF (ERRCOD.EQ.1) THEN
C                 can only have blnk on last condition
                  SGRP = 51
                  CALL ZWNSET (WNDNAM)
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                ELSE IF (ERRCOD.EQ.2) THEN
C                 cannot have blnk on non-last condition
                  SGRP = 52
                  CALL ZWNSET (WNDNAM)
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                END IF
              END IF
            IF (ERRCOD.NE.0) GO TO 5
          ELSE IF (OPVAL(1).EQ.2) THEN
C           else condition
            UCIBF(STWORD:STWORD+3) = 'ELSE'
            UCIBF(STWORD+4:80) = ' '
            CALL REPUCI (KEY,UCIBF)
          ELSE IF (OPVAL(1).EQ.4) THEN
C           end if condition
            UCIBF(STWORD:STWORD+5) = 'END IF'
            UCIBF(STWORD+6:80) = ' '
            CALL REPUCI (KEY,UCIBF)
          END IF
          IF ((OPVAL(1).EQ.4.OR.OPVAL(1).EQ.2) .AND. LUSED.GT.1) THEN
C           want to remove remaining lines
            DO 300 I = 2,LUSED
C             get the next record and drop it
              CALL GETUCI (I0,
     M                     LKEY,
     O                     UCIBF)
              CALL DELUCI (LKEY)
 300        CONTINUE
          END IF
        END IF
      ELSE
C       this is an action line
        READ (UCIBF,1000,ERR=7) (TXT(I),I=1,6),IVAL(1),IVAL(2),
     1             (TXT(I),I=7,8),(IVAL(I),I=3,10),(TXT(I),I=10,15),
     2             (IVAL(I),I=11,13),(TXT(I),I=17,29),(TXT(I),I=31,33),
     3             IVAL(14),IVAL(15)
        TXT(9) = ' '
        TXT(16)= ' '
        TXT(30)= ' '
        TXT(34)= ' '
        TXT(35)= ' '
        SGRP = 45
        CALL Q1INIT (MESSFL,SCLU,SGRP)
C       determine if dated
        IF (IVAL(4).GT.0 .AND. IVAL(4).LT.32768) THEN
C         yes, dated special action
          OPVAL(1) = 1
        ELSE
C         no, undated special action
          OPVAL(1) = 2
        END IF
        MSEL(1) = 1
        CALL QSETOP (I1,I1,MSEL,MSEL,OPVAL)
        I = 15
        CALL QSETI (I,IVAL)
        I = 6
        CLEN(1) = 6
        CLEN(2) = 3
        CLEN(3) = 7
        CLEN(4) = 4
        CLEN(5) = 11
        CLEN(6) = 4
        TLEN = 35
        CALL QSETCT (I,CLEN,TLEN,TXT)
        CALL Q1EDIT (IRET)
        IF (IRET.EQ.1) THEN
C         user wants to continue
          I = 15
          CALL QGETI (I,IVAL)
          I = 6
          CALL QGETCT (I,CLEN,TLEN,TXT)
          CALL QGETOP (I1,OPVAL)
          IF (OPVAL(1).EQ.1) THEN
C           dated special action
            WRITE (UCIBF,2000) (TXT(I),I=1,6),IVAL(1),IVAL(2),TXT(7),
     1            TXT(8),(IVAL(I),I=3,10),(TXT(I),I=10,15),
     2            (IVAL(I),I=11,13),(TXT(I),I=17,29),(TXT(I),I=31,33),
     3            IVAL(14),IVAL(15)
          ELSE IF (OPVAL(1).EQ.2) THEN
C           undated special action
            WRITE (UCIBF,2010) (TXT(I),I=1,6),IVAL(1),IVAL(2),IVAL(10),
     1            (TXT(I),I=10,15),(IVAL(I),I=11,13),(TXT(I),I=17,29)
          END IF
          CALL REPUCI (KEY,UCIBF)
        END IF
        GO TO 2
 7      CONTINUE
C       error in reading record, may be secondary line of conditional
        SGRP = 53
        CALL ZWNSET (WNDNAM)
        CALL PRNTXT (MESSFL,SCLU,SGRP)
 2      CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   UMSPNA
     I                   (MESSFL,SCLU,WNDNAM,KEY)
C
C     + + + PURPOSE + + +
C     modify a special actions record for user defined name
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,SCLU,KEY
      CHARACTER*48 WNDNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     KEY    - key to this line of uci file
C     WNDNAM - window name
C
C     + + + PARAMETERS + + +
      INTEGER    MXUDNL
      PARAMETER (MXUDNL=10)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,CLEN(10),TLEN,SGRP,IVAL(3,MXUDNL),IRET,I1,I4,
     1             I0,LKEY,I2,COUNT,REMAIN,I3,I6,
     3             LUSED,LCNT,CVAL(1,MXUDNL),ID,RETFLG
      REAL         RVAL(MXUDNL)
      CHARACTER*1  CTXT(10,MXUDNL)
      CHARACTER*4  ACODE(MXUDNL)
      CHARACTER*6  UDNAME,VNAME(MXUDNL)
      CHARACTER*80 UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL     Q2GCTB,Q2SCTB,Q2INIT
      EXTERNAL     Q2EDIT,CVARAR,ZWNSET,CARVAR,PREUCI,GETUCI
      EXTERNAL     REPUCI,PUTUCI,Q2STCO,Q2GTCO,PRNTXT,DELUCI
      EXTERNAL     Q2SETI,Q2SETR,Q2GETI,Q2GETR
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(20X,A6,3I3,1X,F5.3,1X,A4,4X,A6,3I3,1X,F5.3,1X,A4)
 1001 FORMAT(20X,A6,3I3,1X,F5.3,1X,A4)
 1003 FORMAT(A6,I3)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(20X,A6,3I3,1X,F5.3,1X,A4,4X,A6,3I3,1X,F5.3,1X,A4)
 2001 FORMAT(20X,A6,3I3,1X,F5.3,1X,A4)
 2002 FORMAT('  UVNAME',2X,A6,I3,1X,A6,3I3,1X,F5.3,1X,A4)
 2003 FORMAT('  UVNAME',2X,A6,I3,1X,A6,3I3,1X,F5.3,1X,A4,4X,A6,3I3,1X,
     $       F5.3,1X,A4)
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I2  = 2
      I3  = 3
      I4  = 4
      I6  = 6
C
C     get this record
      LKEY = KEY
      CALL PREUCI
     M            (LKEY)
      CALL GETUCI (I0,
     M             LKEY,
     O             UCIBF)
C
C     see how many varaibles associated with this name
      READ (UCIBF(11:19),1003,ERR=7) UDNAME,COUNT
      REMAIN = COUNT
C
C     fill arrays for each user-defined name
      I = 0
      LUSED = 0
 10   CONTINUE
        IF (REMAIN.GE.2) THEN
          I = I + 2
          READ (UCIBF,1000,ERR=7) VNAME(I-1),IVAL(1,I-1),IVAL(2,I-1),
     1               IVAL(3,I-1),RVAL(I-1),ACODE(I-1),VNAME(I),
     2               IVAL(1,I),IVAL(2,I),IVAL(3,I),RVAL(I),ACODE(I)
          CVAL(1,I)  = 1
          CVAL(1,I-1)= 1
          LUSED = LUSED + 1
          REMAIN= REMAIN - 2
        ELSE IF (REMAIN.EQ.1) THEN
          I = I + 1
          READ (UCIBF,1001,ERR=7) VNAME(I),IVAL(1,I),IVAL(2,I),
     1               IVAL(3,I),RVAL(I),ACODE(I)
          CVAL(1,I)  = 1
          LUSED = LUSED + 1
          REMAIN= REMAIN - 1
        END IF
        CALL GETUCI (I0,
     M               LKEY,
     O               UCIBF)
      IF (REMAIN.GT.0) GO TO 10
C
 15   CONTINUE
C       return here while looking at each action
C       do screen to edit fields
        CALL ZWNSET (WNDNAM)
        SGRP = 54
        CALL Q2INIT (MESSFL,SCLU,SGRP)
        CALL Q2STCO (I1,COUNT,CVAL)
        CLEN(1) = 6
        CLEN(2) = 4
        TLEN    = 10
        DO 20 I = 1,COUNT
          CALL CVARAR(I6,VNAME(I),I6,CTXT(1,I))
          CALL CVARAR(I4,ACODE(I),I4,CTXT(7,I))
 20     CONTINUE
        CALL Q2SCTB (I2,CLEN,TLEN,I2,COUNT,CTXT)
        CALL Q2SETI (I3,COUNT,IVAL)
        CALL Q2SETR (I1,COUNT,RVAL)
        CALL Q2EDIT (COUNT,
     O               IRET)
        IF (IRET.EQ.1) THEN
C         user wants to continue
          CALL Q2GCTB (I2,CLEN,TLEN,I2,COUNT,
     O                 CTXT)
          CALL Q2GETI (I3,COUNT,
     O                 IVAL)
          CALL Q2GETR (I1,COUNT,
     O                 RVAL)
          DO 30 I = 1,COUNT
            CALL CARVAR(I6,CTXT(1,I),I6,VNAME(I))
            CALL CARVAR(I4,CTXT(7,I),I4,ACODE(I))
 30       CONTINUE
          CALL Q2GTCO (I1,COUNT,
     O                 CVAL)
C
          RETFLG= 0
          I = 1
 40       CONTINUE
C           check each line for action desired
            IF (CVAL(1,I).EQ.2) THEN
C             drop this name
              RETFLG = 1
              IF (COUNT.LE.1) THEN
C               can't drop if only one, tell user
                SGRP = 57
                CALL ZWNSET (WNDNAM)
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                I = I + 1
              ELSE
C               drop this name
                COUNT = COUNT-1
                IF (COUNT.GE.I) THEN
C                 there are more lines to follow, move up
                  DO 50 ID= I,COUNT
                    CVAL(1,ID) = CVAL(1,ID+1)
                    VNAME(ID)  = VNAME(ID+1)
                    IVAL(1,ID) = IVAL(1,ID+1)
                    IVAL(2,ID) = IVAL(2,ID+1)
                    IVAL(3,ID) = IVAL(3,ID+1)
                    RVAL(ID)   = RVAL(ID+1)
                    ACODE(ID)  = ACODE(ID+1)
 50               CONTINUE
                END IF
              END IF
            ELSE IF (CVAL(1,I).EQ.3) THEN
C             want to copy this name
              COUNT = COUNT+1
              CVAL(1,I) = 1
              IF (COUNT .LE. MXUDNL) THEN
C               room for a new name
                ID = COUNT
 60             CONTINUE
C                 move names after this one down
                  CVAL(1,ID) = CVAL(1,ID-1)
                  VNAME(ID)  = VNAME(ID-1)
                  IVAL(1,ID) = IVAL(1,ID-1)
                  IVAL(2,ID) = IVAL(2,ID-1)
                  IVAL(3,ID) = IVAL(3,ID-1)
                  RVAL(ID)   = RVAL(ID-1)
                  ACODE(ID)  = ACODE(ID-1)
                  ID = ID - 1
                IF (ID.NE.I) GO TO 60
                CVAL(1,I+1) = 1
              END IF
              RETFLG = 1
              I = I + 1
            ELSE
C             look at the next condition
              I = I + 1
            END IF
          IF (I.LE.COUNT) GO TO 40
        END IF
      IF (IRET.EQ.1 .AND. RETFLG.EQ.1) GO TO 15
C
      IF (IRET.EQ.1) THEN
C       user accepted, put names back
        LKEY  = KEY
        LCNT  = 0
        REMAIN= COUNT
        I     = 0
 80     CONTINUE
C         put to uci buffer each name
          IF (REMAIN.GE.2) THEN
            I = I + 2
            UCIBF = ' '
            IF (LCNT.EQ.0) THEN
C             first time to write this
              WRITE (UCIBF,2003) UDNAME,COUNT,VNAME(I-1),IVAL(1,I-1),
     1                   IVAL(2,I-1),IVAL(3,I-1),RVAL(I-1),ACODE(I-1),
     2                   VNAME(I),IVAL(1,I),IVAL(2,I),IVAL(3,I),RVAL(I),
     3                   ACODE(I)
            ELSE
              WRITE (UCIBF,2000) VNAME(I-1),IVAL(1,I-1),
     1                   IVAL(2,I-1),IVAL(3,I-1),RVAL(I-1),ACODE(I-1),
     2                   VNAME(I),IVAL(1,I),IVAL(2,I),IVAL(3,I),RVAL(I),
     3                   ACODE(I)
            END IF
            REMAIN= REMAIN - 2
            LCNT  = LCNT + 1
            IF (LCNT.LT.LUSED) THEN
C             can replace existing line
              CALL REPUCI (LKEY,UCIBF)
              CALL GETUCI (I0,
     M                     LKEY,
     O                     UCIBF)
            ELSE IF (LCNT.EQ.LUSED) THEN
C             can replace existing line
              CALL REPUCI (LKEY,UCIBF)
            ELSE
C             this is a new line
              CALL PUTUCI (UCIBF,I1,LKEY)
              CALL GETUCI (I0,
     M                     LKEY,
     O                     UCIBF)
            END IF
          ELSE
            I = I + 1
            UCIBF = ' '
            IF (LCNT.EQ.0) THEN
C             first time to write this
              WRITE (UCIBF,2002) UDNAME,COUNT,VNAME(I),IVAL(1,I),
     1                   IVAL(2,I),IVAL(3,I),RVAL(I),ACODE(I)
            ELSE
              WRITE (UCIBF,2001) VNAME(I),IVAL(1,I),IVAL(2,I),
     1                   IVAL(3,I),RVAL(I),ACODE(I)
            END IF
            REMAIN= REMAIN - 1
            LCNT  = LCNT + 1
            IF (LCNT.LT.LUSED) THEN
C             can replace existing line
              CALL REPUCI (LKEY,UCIBF)
              CALL GETUCI (I0,
     M                     LKEY,
     O                     UCIBF)
            ELSE IF (LCNT.EQ.LUSED) THEN
C             can replace existing line
              CALL REPUCI (LKEY,UCIBF)
            ELSE
C             this is a new line
              CALL PUTUCI (UCIBF,I1,LKEY)
              CALL GETUCI (I0,
     M                     LKEY,
     O                     UCIBF)
            END IF
          END IF
        IF (REMAIN.GT.0) GO TO 80
C
        IF (LCNT.LT.LUSED) THEN
C         want to remove remaining lines
          DO 90 I = LCNT+1,LUSED
C           get the next record and drop it
            CALL GETUCI (I0,
     M                   LKEY,
     O                   UCIBF)
            CALL DELUCI (LKEY)
 90       CONTINUE
        END IF
      END IF
C
      GO TO 2
 7    CONTINUE
C     error in reading record, may be secondary line of conditional
      SGRP = 56
      CALL ZWNSET (WNDNAM)
      CALL PRNTXT (MESSFL,SCLU,SGRP)
 2    CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   UMMLTB
     I                   (MESSFL,SCLU,PTHNAM)
C
C     + + + PURPOSE + + +
C     Modify parameter tables for MASS-LINK from UCI file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     PTHNAM - character string of path of options selected to get here
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I1,I78,SREC,EREC,KEY,BROW,NROW,ID,SAVKEY,ILEN(1),
     2             SGRP,IRET,LPTH,MCNT(1),MLTBID(50,3),I80,I9,I3,TABNO,
     3             CMLT(1),I4,I,J,TMPKEY,RPTFLG,CVAL(1,100),TOTROW
      CHARACTER*1  TBUFF(78,1),IBUF1(80),CEND(3),CMASS(9),BLNK,
     1             CMLNOS(4,50)
      CHARACTER*48 WNDNAM
      CHARACTER*78 TBUF78
      CHARACTER*80 UCIBF,TMPBF
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT,STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL    Q2INIT,QSTBFB,QGTBFB,Q2EDIT,PRNTXT,STRFND
      EXTERNAL    ZLNTXT,ZWNSOP,ZWNSET,GETSE,GETUCI,REPUCI,CARVAR,CVARAR
      EXTERNAL    ZIPC,INTCHR,QSCSTV,Q1INIT,Q1EDIT,COMUCI
      EXTERNAL    QSETCO,QGETCO,DELUCI,PUTUCI,Q2GTCO
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CEND/'E','N','D'/
      DATA CMASS/'M','A','S','S','-','L','I','N','K'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (12X,I8)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I3 = 3
      I4 = 4
      I9 = 9
      I78= 78
      I80= 80
      BLNK = ' '
C
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))
C
C     need to do intermediate screen to specify mass-link table
      ID = 11
      CALL GETSE (ID,I1,
     O            SREC,EREC)
      IF (SREC.GT.0) THEN
C       at least one mass-link table exists
        KEY = SREC
        MCNT(1)= 0
 10     CONTINUE
C         look at records in in-memory uci for start and end of table
          SAVKEY = KEY
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          CALL CVARAR (I80,UCIBF,I80,IBUF1)
          IF (STRFND(I9,IBUF1(3),I9,CMASS) .GT. 0) THEN
C           found beginning of a mass-link table
            MCNT(1) = MCNT(1) + 1
            READ (UCIBF,1000) MLTBID(MCNT(1),1)
            MLTBID(MCNT(1),2) = SAVKEY
          ELSE IF (STRFND(I3,IBUF1(3),I3,CEND) .GT. 0) THEN
C           found end of block, so done
            MLTBID(MCNT(1),3) = SAVKEY
          END IF
C         look ahead one record for end of block
          TMPKEY = KEY
          CALL GETUCI (I0,
     M                 TMPKEY,
     O                 TMPBF)
        IF (TMPKEY.NE.EREC) GO TO 10
C       now we know start and end of each mass link
        CALL ZIPC (4*50,BLNK,CMLNOS)
        IF (MCNT(1).GT.0) THEN
C         at least one m-l table has been specified, proceed
          SGRP  = 202
          CALL Q1INIT (MESSFL,SCLU,SGRP)
          DO 20 I= 1,MCNT(1)
C           fill in character buffer of valid mass-link ids
            CALL INTCHR (MLTBID(I,1),I4,I1,
     O                   J,CMLNOS(1,I))
 20       CONTINUE
          ILEN(1) = 4
          CALL QSCSTV (I1,I1,I1,MCNT,ILEN,ILEN(1)*4,CMLNOS)
          I= 3
          CMLT(1) = 1
          CALL QSETCO (I1,CMLT)
          IF (LPTH .GT. 0) THEN
C           path name available
            WNDNAM= 'MAss-Link ('//PTHNAM(1)(1:LPTH)//'UMa)'
          ELSE
C           no pathname
            WNDNAM= 'MAss-Link (UMa)'
          END IF
          CALL ZWNSET (WNDNAM)
          CALL Q1EDIT (IRET)
          IF (IRET.EQ.1) THEN
C           user wants to continue, get table number
            CALL QGETCO (I1,CMLT)
            TABNO = MLTBID(CMLT(1),1)
          ELSE
C           previous, kick out
            TABNO = 0
          END IF
        ELSE
C         need to catch condition of no m-l specifications
          TABNO = 0
        END IF
        IF (TABNO.GT.0) THEN
C         do second screen
 40       CONTINUE
C           initialize screen
            SGRP = 200
            CALL ZWNSET (WNDNAM)
            CALL Q2INIT (MESSFL,SCLU,SGRP)
            SREC= MLTBID(CMLT(1),2)
            EREC= MLTBID(CMLT(1),3)
            KEY = SREC
C           move to next key
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
            BROW= 0
            NROW= 1
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
 50         CONTINUE
C             fill in screen with records from in-memory uci
              BROW = BROW + 1
              TBUF78 = 'NO ' // UCIBF
              CALL CVARAR (I78,TBUF78,I78,TBUFF(1,1))
C             put this record on the screen
              CALL QSTBFB (I78,NROW,BROW,TBUFF)
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
            IF (KEY.LE.EREC) GO TO 50
C           let user edit screen
            CALL Q2EDIT (BROW,
     O                   IRET)
            RPTFLG= 0
            TOTROW= BROW
            IF (IRET.EQ.1) THEN
C             user wants to continue
              KEY = SREC
C             move to next key
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
              BROW= 0
              NROW= 1
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
 60           CONTINUE
                BROW = BROW + 1
C               get this record from the screen
                CALL QGTBFB (I78,NROW,BROW,
     O                       TBUFF)
                CALL CARVAR (I78,TBUFF(1,1),I78,TBUF78)
                UCIBF = TBUF78(4:78) // '     '
C               put this record back to in-memory uci
                CALL REPUCI (KEY,UCIBF)
C               see if we want to copy or delete it
                CALL Q2GTCO (I1,BROW,
     O                       CVAL)
                IF (CVAL(1,BROW).EQ.2) THEN
C                 user wants to drop this line
                  RPTFLG = 1
                  IF (TOTROW.GT.1) THEN
C                   okay to delete a line
                    CALL DELUCI (KEY)
                    TOTROW = TOTROW - 1
                  ELSE
C                   this is only row left, can't delete it
                    SGRP = 203
                    CALL ZWNSET (WNDNAM)
                    CALL PRNTXT (MESSFL,SCLU,SGRP)
                  END IF
                ELSE IF (CVAL(1,BROW).EQ.3) THEN
C                 user wants to copy this line
                  RPTFLG = 1
                  CALL PUTUCI (UCIBF,I1,KEY)
                  CALL GETUCI (I0,
     M                         KEY,
     O                         UCIBF)
                  TOTROW = TOTROW + 1
                ELSE IF (CVAL(1,BROW).EQ.4) THEN
C                 user wants to comment out this line
                  IF (TOTROW.GT.1) THEN
C                   okay to comment a line
                    CALL COMUCI (KEY,
     M                           UCIBF)
                    CALL REPUCI (KEY,UCIBF)
                  END IF
                END IF
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
              IF (KEY.LE.EREC) GO TO 60
            END IF
          IF (RPTFLG.EQ.1) GO TO 40
        END IF
      ELSE
C       no data available for this block
        SGRP= 201
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'MAss-link ('//PTHNAM(1)(1:LPTH)//'UMa) Problem'
        ELSE
C         no pathname
          WNDNAM= 'MAss-link (UMa) Problem'
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
      SUBROUTINE   UMFTAB
     I                   (MESSFL,SCLU,PTHNAM)
C
C     + + + PURPOSE + + +
C     Modify parameter tables for FTABLE operations from UCI file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     PTHNAM - character string of path of options selected to get here
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I1,ITAB,SGRP,INUM,RNUM,ILEN,IRET,NROW,I3,
     $            IVAL(3,30),CVAL(2,3,30),LPTH,LNFTAB,TMPKEY,
     $            CNUM,I80,J,ROWS,I0,IVAL2(1,1),CVAL2(7,3,5),CPLACE,
     $            KTYP,KEYST,KEYND,NFTAB,FTABNO(200),FTABRW(200),
     $            FTABCL(200),NCMT(200),DATKEY(200,2),CMTKEY(200),KEY,
     $            SAVKEY,IM1,MOREFG,EREC,I78,BROW
      REAL        RVAL(100),RVAL2(1)
      CHARACTER*1 TBUFF(80,30),BLNK,HEADR(80),CMNT(3),IBUF1(80),CEND(3)
      CHARACTER*48 WNDNAM
      CHARACTER*78 TBUF78
      CHARACTER*80 UCIBF,TMPBF
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT,STRFND,CHRINT
C
C     + + + EXTERNALS + + +
      EXTERNAL    PRNTXT,QRESCX,ZIPC,ZIPI,ZGTRET,STRFND,GETSE
      EXTERNAL    ZLNTXT,ZWNSOP,ZWNSET,CVARAR,CARVAR,CHRINT
      EXTERNAL    Q2INIT,QSTBFB,QGTBFB,Q2EDIT,GETUCI,REPUCI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CEND/'E','N','D'/
      DATA CMNT/'*','*','*'/
C
C     + + + END SPECIFICATIONS + + +
C
      IM1= -1
      I1 = 1
      I0 = 0
      I3 = 3
      I78= 78
      I80= 80
      BLNK= ' '
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))
      IF (LPTH .GT. 0) THEN
C       path name available
        WNDNAM= 'FTable ('//PTHNAM(1)(1:LPTH)//'UFt)'
      ELSE
C       no pathname
        WNDNAM= 'FTable (UFt)'
      END IF
C
      KTYP= 4
      CALL GETSE (KTYP,I1,
     O            KEYST,KEYND)
      IF (KEYST.GT.0) THEN
C       ftables exist, go ahead
        KEY = KEYST
        NFTAB = 0
 10     CONTINUE
C         look at records in in-memory uci for start and end of table
          SAVKEY = KEY
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          CALL CVARAR (I80,UCIBF,I80,IBUF1)
C         not at end, must be start of another FTABLE
          NFTAB= NFTAB+ 1
C         start with table number
          FTABNO(NFTAB)= CHRINT (I3,IBUF1(13))
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
          CALL CVARAR (I80,UCIBF,I80,IBUF1)
C         now get number of rows and columns
          I= 5
          FTABRW(NFTAB)= CHRINT(I,IBUF1)
          FTABCL(NFTAB)= CHRINT(I,IBUF1(6))
          NCMT(NFTAB)  = 0
 20       CONTINUE
C           look at records in in-memory uci for start and end of comments
            SAVKEY = KEY
            CALL GETUCI (IM1,
     M                   KEY,
     O                   UCIBF)
            CALL CVARAR (I80,UCIBF,I80,IBUF1)
            IF (STRFND(I80,IBUF1,I3,CMNT).GT.0) THEN
C             found a comment
              NCMT(NFTAB) = NCMT(NFTAB) + 1
              IF (NCMT(NFTAB).EQ.1) THEN
C               first comment for this ftable
                CMTKEY(NFTAB) = SAVKEY
              END IF
              MOREFG = 1
            ELSE
              MOREFG = 0
            END IF
          IF (MOREFG.EQ.1) GO TO 20
          DATKEY(NFTAB,1) = SAVKEY
 30       CONTINUE
C           look at records in in-memory uci for start and end of data
            SAVKEY = KEY
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
            CALL CVARAR (I80,UCIBF,I80,IBUF1)
            IF (STRFND(I3,IBUF1(3),I3,CEND) .GT. 0) THEN
C             found end of this ftable
              DATKEY(NFTAB,2) = SAVKEY
              MOREFG = 0
            ELSE
              MOREFG = 1
            END IF
          IF (MOREFG.EQ.1) GO TO 30
C         now we know all we need to about this ftable, do next
C         look ahead one record for end of block
          TMPKEY = KEY
          CALL GETUCI (I0,
     M                 TMPKEY,
     O                 TMPBF)
        IF (TMPKEY.NE.KEYND) GO TO 10
C       now begin screens
        ILEN= 80*NFTAB
        CALL ZIPC(ILEN,BLNK,TBUFF)
        INUM= 3
        DO 40 I= 1,NFTAB
          IVAL(1,I)= FTABNO(I)
          IVAL(2,I)= FTABRW(I)
          IVAL(3,I)= FTABCL(I)
 40     CONTINUE
        CNUM= 2
        ILEN= CNUM*3*NFTAB
        CALL ZIPI(ILEN,I1,CVAL)
        SGRP= 31
        IF (NFTAB.GT.48) THEN
C         too many lines for one aide screen
          LNFTAB = 48
        ELSE
          LNFTAB = NFTAB
        END IF
        CALL ZWNSET (WNDNAM)
        CALL QRESCX(MESSFL,SCLU,SGRP,INUM,I1,CNUM,LNFTAB,I1,
     M              IVAL,RVAL,CVAL,TBUFF)
C       get user exit command value
        CALL ZGTRET (IRET)
        IF (IRET .EQ. 1) THEN
C         edit spec tables headers
          DO 50 ITAB= 1,NFTAB
C           loop through ftables
            ROWS = NCMT(ITAB)
            IF (CVAL(1,1,ITAB) .EQ. 2) THEN
C             edit header for this ftable
              SGRP = 32
              CNUM = 7
              IVAL2(1,1) = 0
              RVAL2(1) = 0.0
              ILEN= CNUM*3*ROWS
              CALL ZIPI(ILEN,I1,CVAL2)
              KEY = CMTKEY(ITAB)
              DO 13 I= 1,ROWS
C               write characters to local ch*1 buffer
                CALL GETUCI (IM1,
     M                       KEY,
     O                       UCIBF)
                CALL CVARAR (I80,UCIBF,I80,HEADR)
                DO 14 J= 1,70
C                 remove comment marks from header for screen
                  IF (HEADR(J).EQ.'*' .AND. HEADR(J+1).EQ.'*' .AND.
     1                HEADR(J+2).EQ.'*') THEN
                    HEADR(J)  = ' '
                    HEADR(J+1)= ' '
                    HEADR(J+2)= ' '
                  END IF
C                 write headers for each row to tbuff
                  TBUFF(J,I) = HEADR(J)
 14             CONTINUE
 13           CONTINUE
              CALL ZWNSET (WNDNAM)
              CALL QRESCX(MESSFL,SCLU,SGRP,I1,I1,CNUM,ROWS,I1,
     M                    IVAL2,RVAL2,CVAL2,TBUFF)
C             get user exit command value
              CALL ZGTRET (IRET)
              IF (IRET .EQ. 1) THEN
C               put headers back
                KEY = CMTKEY(ITAB)
                DO 17 I= 1,ROWS
                  CALL GETUCI (IM1,
     M                         KEY,
     O                         UCIBF)
                  DO 16 J= 1,70
                    HEADR(J) = TBUFF(J,I)
 16               CONTINUE
C                 write headers for each row to common
                  CALL CARVAR (I80,HEADR,I80,UCIBF)
                  IF ((ZLNTXT(UCIBF).GT.0) .AND.
     1                (STRFND(I80,UCIBF,I3,CMNT).EQ.0)) THEN
C                   this is a header line and no comment marks appear,
C                   comment marks need to be added
                    CPLACE = ZLNTXT(UCIBF) + 2
                    IF (CPLACE.LT.79) THEN
C                     room to put comment marks
                      HEADR(CPLACE) = '*'
                      HEADR(CPLACE+1) = '*'
                      HEADR(CPLACE+2) = '*'
C                     put header back
                      CALL CARVAR (I80,HEADR,I80,UCIBF)
                    END IF
                  END IF
C                 put this record back to in-memory uci
                  CALL REPUCI (KEY,UCIBF)
 17             CONTINUE
              END IF
            END IF
 50       CONTINUE
C         edit spec tables data
          DO 60 ITAB= 1,NFTAB
            EREC = DATKEY(ITAB,2)
            IF (CVAL(2,1,ITAB) .EQ. 2) THEN
C             edit data for this ftable
              NROW= FTABRW(ITAB)
              RNUM= FTABCL(ITAB)
C             save data values
              SGRP= 30+ RNUM
              CALL ZWNSET (WNDNAM)
              CALL Q2INIT (MESSFL,SCLU,SGRP)
              KEY = DATKEY(ITAB,1)
              BROW= 0
              NROW= 1
 70           CONTINUE
C               fill in screen with records from in-memory uci
                CALL GETUCI (I0,
     M                       KEY,
     O                       UCIBF)
                TBUF78 = UCIBF
                CALL CVARAR (I78,TBUF78,I78,TBUFF(1,1))
                BROW = BROW + 1
C               put this record on the screen
                CALL QSTBFB (I78,NROW,BROW,TBUFF)
              IF (KEY.NE.EREC) GO TO 70
C             let user edit screen
              CALL Q2EDIT (BROW,
     O                     IRET)
              IF (IRET.EQ.1) THEN
C               user wants to continue
                KEY = DATKEY(ITAB,1)
                BROW= 0
                NROW= 1
 80             CONTINUE
                  BROW = BROW + 1
                  CALL GETUCI (I0,
     M                         KEY,
     O                         UCIBF)
C                 get this record from the screen
                  CALL QGTBFB (I78,NROW,BROW,
     O                         TBUFF)
                  CALL CARVAR (I78,TBUFF(1,1),I78,TBUF78)
                  UCIBF = TBUF78
C                 put this record back to in-memory uci
                  CALL REPUCI (KEY,UCIBF)
                IF (KEY.NE.EREC) GO TO 80
              END IF
            END IF
 60       CONTINUE
        END IF
      ELSE
C       no ftables available
        SGRP= 30
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'FTable ('//PTHNAM(1)(1:LPTH)//'UFt) Problem'
        ELSE
C         no pathname
          WNDNAM= 'FTable (UFt) Problem'
        END IF
        CALL ZWNSET (WNDNAM)
        CALL PRNTXT(MESSFL,SCLU,SGRP)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NEWDSN
     I                   (XTSCOD,OLD,NEW)
C
C     + + + PURPOSE + + +
C     swap an old data set number for a new data set number for a
C     new scenario
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    XTSCOD,OLD,NEW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XTSCOD - which ts block to look in (3-ext targets)
C     OLD    - original data set number
C     NEW    - copy's data set number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I0,I1,ID,SREC,EREC,KEY,ITMP,OLDD,NEWD,J
      CHARACTER*4   OLDWDI,NEWWDI,CTMP
      CHARACTER*80  UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL      GETSE,GETUCI,REPUCI,WID2UA
C
C     + + + INPUT FORMATS + + +
 1012 FORMAT(I4)
 1015 FORMAT(A4)
C
C     + + + OUTPUT FORMATS + + +
 2012 FORMAT(I4)
 2015 FORMAT(A4)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C     convert this dsnid to a wdm id and dsn
      CALL WID2UA (I0,OLD,
     O             J,OLDD,OLDWDI)
      CALL WID2UA (I0,NEW,
     O             J,NEWD,NEWWDI)
C
      IF (XTSCOD.EQ.3) THEN
C       look in external targets records
        ID = 8
        CALL GETSE (ID,I1,
     O              SREC,EREC)
        IF (SREC.NE.0) THEN
C         some records exist in this table
          KEY = SREC
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
 50       CONTINUE
            READ (UCIBF(44:47),1015) CTMP
            IF (CTMP.EQ.'WDM ') THEN
C             update this to new mult wdm
              CTMP = 'WDM1'
            END IF
            READ (UCIBF(50:53),1012) ITMP
C           see if this dsn is the one we want
            IF (ITMP.EQ.OLDD .AND. CTMP.EQ.OLDWDI) THEN
C             yes it is, replace it
              ITMP = NEWD
              CTMP = NEWWDI
              WRITE (UCIBF(50:53),2012) ITMP
              WRITE (UCIBF(44:47),2015) CTMP
              CALL REPUCI (KEY,UCIBF)
            END IF
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBF)
          IF (KEY.NE.EREC) GO TO 50
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NEWFIL
     I                   (CBAS)
C
C     + + + PURPOSE + + +
C     swap an old file name for a new file name for a new scenario
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*8    CBAS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CBAS   - base name to be included in new file names
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I8,J,I0,I1,I80,ID,SREC,EREC,KEY,SPOS,FPOS,IUNIT,
     1              RCL,RET
      CHARACTER*1   CBAS1(8),UCIBF1(80)
      CHARACTER*12  ACC,STAT
      CHARACTER*30  FRM
      CHARACTER*64  FILNAM
      CHARACTER*80  UCIBF,TMPBF
      LOGICAL       IOPEN
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR,CVARAR,GETSE,GETUCI,REPUCI,FILOPN
C
C     + + + INPUT FORMATS + + +
1000  FORMAT(I3)
C
C     + + + END SPECIFICATIONS + + +
C
      I8 = 8
      I0 = 0
      I1 = 1
      I80= 80
      ID = 12
      CALL GETSE (ID,I1,
     O            SREC,EREC)
      IF (SREC.NE.0) THEN
C       some records exist in files block
        KEY = SREC
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
 50     CONTINUE
          IF (UCIBF(1:6) .EQ. 'MESSU ') THEN
C           see if file is open
            FILNAM = UCIBF(17:78)
            INQUIRE (FILE=FILNAM,OPENED=IOPEN)
            IF (IOPEN) THEN
C             close old echo file
              READ (UCIBF(11:13),1000) IUNIT
              CLOSE (IUNIT)
            END IF
C           replace echo file name
            CALL CVARAR (I8,CBAS,I8,CBAS1)
            J = LENSTR (I8,CBAS1)
            TMPBF = ' '
            TMPBF = UCIBF(1:16) // CBAS(1:J) // '.ECH'
            CALL REPUCI (KEY,TMPBF)
C           try to open with new name
            FILNAM = TMPBF(17:78)
            INQUIRE (FILE=FILNAM,OPENED=IOPEN)
            IF (IOPEN) THEN
C             already open
            ELSE
              READ (UCIBF(11:13),1000) IUNIT
              ACC=  'SEQUENTIAL'
              FRM=  'FORMATTED'
              RCL=   132
              STAT= 'UNKNOWN'
              CALL FILOPN
     I                    (ACC,FRM,RCL,STAT,IUNIT,FILNAM,
     O                     RET)
            END IF
          ELSE IF (UCIBF(1:6) .EQ. '      ') THEN
C           see if file is open
            FILNAM = UCIBF(17:78)
            INQUIRE (FILE=FILNAM,OPENED=IOPEN)
            IF (IOPEN) THEN
C             close old output file
              READ (UCIBF(11:13),1000) IUNIT
              CLOSE (IUNIT)
            END IF
C           replace output file name
            CALL CVARAR (I8,CBAS,I8,CBAS1)
            J = LENSTR (I8,CBAS1)
C           save 3 character extension for new name
            CALL CVARAR (I80,UCIBF,I80,UCIBF1)
            FPOS = LENSTR (I80,UCIBF1)
            SPOS = FPOS-3
            TMPBF = ' '
            TMPBF = UCIBF(1:16) // CBAS(1:J) // UCIBF(SPOS:FPOS)
            CALL REPUCI (KEY,TMPBF)
C           try to open with new name
            FILNAM = TMPBF(17:78)
            INQUIRE (FILE=FILNAM,OPENED=IOPEN)
            IF (IOPEN) THEN
C             already open
            ELSE
              READ (UCIBF(11:13),1000) IUNIT
              ACC=  'SEQUENTIAL'
              FRM=  'FORMATTED'
              RCL=   132
              STAT= 'UNKNOWN'
              CALL FILOPN
     I                    (ACC,FRM,RCL,STAT,IUNIT,FILNAM,
     O                     RET)
            END IF
          END IF
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
        IF (KEY.NE.EREC) GO TO 50
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKADJ
     I                   (MESSFL,SCLU,SGRP,
     O                    SRTFLG)
C
C     + + + PURPOSE + + +
C     check to see if nessessary to adjust the length of an
C     80 character string to 78 characters for edit screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        MESSFL,SCLU,SGRP,SRTFLG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - cluster number
C     SGRP   - group number for screen to check
C     SRTFLG - shorten flag, zero if not needed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     NFLDS,SCOL(30),FLEN(30),APOS(30),IMIN(30),
     $            NMHDRW,RETCOD,IMAX(30),IDEF(30)
      REAL        RMIN(30),RMAX(30),RDEF(30)
      CHARACTER*1 FTYP(30),HDRBUF(78,5)
C
C     + + + EXTERNALS + + +
      EXTERNAL    WMSGTX
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WMSGTX (MESSFL,SCLU,SGRP,
     O             NFLDS,SCOL,FLEN,FTYP,APOS,IMIN,IMAX,IDEF,
     O             RMIN,RMAX,RDEF,NMHDRW,HDRBUF,RETCOD)
      IF (FLEN(1).EQ.4 .OR. FLEN(1).EQ.8) THEN
C       need to remove two characters from this string
        SRTFLG = 1
      ELSE
        SRTFLG = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ADJLEN
     I                   (ADJFLG,
     M                    STRG)
C
C     + + + PURPOSE + + +
C     adjust the length of an 80 character string to
C     78 characters for edit screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        ADJFLG
      CHARACTER*80   STRG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ADJFLG - shorten(0) or lengthen(1) flag
C     STRG   - character string to adjust
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J
      CHARACTER*80 TMP
C
C     + + + INPUT FORMATS + + +
1000  FORMAT(2I5)
1010  FORMAT(2I4)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ADJFLG.EQ.0) THEN
C       need to remove two characters from this string
        READ(STRG,1000,ERR=10) I,J
C       numeric, remove 1 and 6
        TMP = STRG(2:5) // STRG(7:80) // '  '
        GO TO 20
 10     CONTINUE
C         not number, remove 9 and 10
          TMP = STRG(1:8) // STRG(11:80) // '  '
 20     CONTINUE
      ELSE IF (ADJFLG.EQ.1) THEN
C       need to add two characters to this string
        READ(STRG,1010,ERR=30) I,J
C       numeric, add at 1 and 6
        TMP = ' ' // STRG(1:4) // ' ' // STRG(5:78)
        GO TO 40
 30     CONTINUE
C         not number, add at 9 and 10
          TMP = STRG(1:8) // '  ' // STRG(9:78)
 40     CONTINUE
      END IF
C
      STRG= TMP
C
      RETURN
      END
C
C
C
      SUBROUTINE   PUTGLO
     I                   (SDATIM,EDATIM,OUTLEV,SPOUT,
     I                    RESMFG,RUNFG,UNIT,RNINFO)
C
C     + + + PURPOSE + + +
C     put global block back into memory uci file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      OUTLEV,RESMFG,RUNFG,SPOUT,
     $             SDATIM(5),EDATIM(5),UNIT
      CHARACTER*80 RNINFO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     OUTLEV - run interpreter output level
C     SPOUT  - special actions output level
C     RESMFG - resume flag - 0:standard mode, 1:resume mode
C     RUNFG  - run flag - 1:run with no errors
C                         0:just interp
C     UNIT   - english/metric units flag (english-1,metric-2)
C     RNINFO - character string of run information
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I1,ITYP,KEY,KEYST,KEYND
      CHARACTER*80 UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL  GETUCI,GETSE,REPUCI
C
C     + + + INPUT FORMATS + + +
 2030 FORMAT (2X,'START   ',I8,4(1X,I2),'  END',I8,4(1X,I2))
 2040 FORMAT (2X,'RUN INTERP OUTPUT LEVEL',2I5)
 2050 FORMAT (2X,'RESUME ',I5,' RUN ',I5,'            ',
     $        '       UNIT SYSTEM ',I5)
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
C     find table in uci (type 2 from hspf.seq, grp 22, col 3
      ITYP= 2
      CALL GETSE(ITYP,I1,
     O           KEYST,KEYND)
      IF (KEYST.NE.0) THEN
C       putting global block
        KEY    = KEYST
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        CALL REPUCI (KEY,RNINFO)
C       put dates and times
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        WRITE (UCIBF,2030,ERR=30) SDATIM,EDATIM
        CALL REPUCI (KEY,UCIBF)
 30     CONTINUE
C       put the "level" for quantity of run interpreter output
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        WRITE (UCIBF,2040,ERR=40)  OUTLEV,SPOUT
        CALL REPUCI (KEY,UCIBF)
 40     CONTINUE
C       ascertain functions to be performed in this run
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        WRITE (UCIBF,2050,ERR=60)  RESMFG, RUNFG, UNIT
 60     CONTINUE
        CALL REPUCI (KEY,UCIBF)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETDEP
     O                   (DEPSUM)
C
C     + + + PURPOSE + + +
C     get sum of deepfr values to set subjective parm in expert sys
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   DEPSUM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DEPSUM - sum of deepfr values from perlnd
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I0,I1,ID,SREC,EREC,KEY
      REAL          RTMP
      CHARACTER*80  UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL      GETSE,GETUCI,RNGCHK
C
C     + + + INPUT FORMATS + + +
 1012 FORMAT(F10.3)
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
      DEPSUM = 0.0
      ID = 1012
      CALL GETSE (ID,I1,
     O            SREC,EREC)
      IF (SREC.NE.0) THEN
C       some records exist in this table
        KEY = SREC
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
 50     CONTINUE
          READ (UCIBF(51:60),1012) RTMP
          DEPSUM = DEPSUM + RTMP
          CALL RNGCHK (I1,KEY,
     M                 UCIBF)
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBF)
        IF (KEY.NE.EREC) GO TO 50
      END IF
C
      RETURN
      END
