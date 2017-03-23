C
C
C
      SUBROUTINE   WDTGTQ
     I                    (WDMSFL,DSN,DELT,DATES,NVAL,
     I                     DTRAN,QUALFG,TUNITS,
     O                     RVAL,QVAL,RETCOD)
C
C     + + + PURPOSE + + +
C     Gets timeseries information from the WDMSFL.  Returns the time-
C     series data values and the associated array of quality codes.
C
C     + + + HISTORY + + +
C     Mike Horn, Oct 1993, Computing Centre for Water Research
C     KMFlynn, 6-character names
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DELT,DATES(6),NVAL,DTRAN,QUALFG,
     1          TUNITS,RETCOD, QVAL(NVAL)
      REAL      RVAL(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     DELT   - time step for get
C     DATES  - starting date
C     NVAL   - number of values
C     DTRAN  - transformation code
C              0 - ave,same
C              1 - sum,div
C              2 - max
C              3 - min
C     QUALFG - allowed quality code
C     TUNITS - time units for get
C     RVAL   - array to place retrieved values in
C     QVAL   - array to place retrieved quality codes in
C     RETCOD - return code
C                0 - everything O.K.
C               -8 - invalid date
C              -14 - date specified not within valid range for data set
C              -20 - problem with one or more of following:
C                    GPFLG, DXX, NVAL, QUALVL, LTSTEP, LTUNIT
C              -21 - date from WDM doesn't match expected date
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GPFLG,GPOSEN,GPIND,LTSTEP,LTUNIT,TDSFRC,TGROUP,TSPTAD,
     1          ENDDAT(6),GPSDAT(6),GETDAT(6),TSPSC1,TSPSC2,
     2          COMPFG,TSFORM,VBTIME,TSSTEP,TCODE,GETQK,RIND
      INTEGER*4 I4NVAL
      REAL      DEFVAL,TOLR,TSFILL,GETQRA,RTMP
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + INTRINSICS + + +
      INTRINSIC FLOAT, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPR, WTPMCK, WTFNDG, WDATCP, WTGTVQ, WDRCGO, WTDSPX
      EXTERNAL  WTSCSC
C
C     + + + END SPECIFICATIONS + + +
C
      I4NVAL= NVAL
      LTSTEP= DELT
      LTUNIT= TUNITS
      GPFLG = 1
      TSFILL= 0.0
C
C     check the user supplied parameters
      CALL WTPMCK (GPFLG,DTRAN,DATES,NVAL,QUALFG,
     M             LTSTEP,LTUNIT,
     O             RETCOD)
      IF (RETCOD.EQ.0) THEN
C       check the data set and figure out which groups have been req.
        CALL WTFNDG (WDMSFL,DSN,GPFLG,DATES,LTSTEP,LTUNIT,I4NVAL,
     O               TDSFRC,TSFILL,TGROUP,TOLR,TSPTAD,
     1               GPIND,GPOSEN,GPSDAT,ENDDAT,RETCOD)
      END IF
C     fill in RVAL with defaults
      DEFVAL= TSFILL
C     max
      IF (DTRAN.EQ.2) DEFVAL= -1.0E30
C     min
      IF (DTRAN.EQ.3) DEFVAL= 1.0E30
      CALL ZIPR (NVAL,DEFVAL,
     O           RVAL)
      CALL ZIPR (NVAL,DEFVAL,
     O           QVAL)
C
      IF (RETCOD.EQ.0) THEN
C       get additional parameters
        RIND= WDRCGO(WDMSFL,TDSFRC)
        CALL WTDSPX (WIBUFF(1,RIND),
     O               COMPFG,TSFORM,VBTIME,TSSTEP,TCODE)
        GETQK= 0
C       can we do a quick get?
        IF (VBTIME.EQ.1) THEN
C         yes, if time units and step ok
          IF (TCODE.LE.4.AND.LTUNIT.LE.4) THEN
C           time units days or shorter, a quick get may work
            CALL WTSCSC (LTUNIT,LTSTEP,TSPSC1)
            CALL WTSCSC (TCODE,TSSTEP,TSPSC2)
            GETQRA= 1.0E-8+ FLOAT(TSPSC2)/FLOAT(TSPSC1)
            RTMP  = GETQRA
            IF (RTMP.LT.1.0) THEN
C             wdm interval less than user interval
              RTMP= 1.0/ GETQRA
            END IF
            IF (MOD(RTMP,1.0).LT.1.0E-6) THEN
C             ok to do a quick get
              GETQK= 1
            END IF
          ELSE IF (TCODE.EQ.LTUNIT) THEN
C           time units are the same, a quick get will work
            GETQK = 1
            GETQRA= 1.0E-8+ FLOAT(TSSTEP)/FLOAT(LTSTEP)
          END IF
        ELSE
C         do a general get
          GETQK= 0
        END IF
C       make a working copy of the starting date
        CALL WDATCP (DATES,GETDAT)
C       get the data
        CALL WTGTVQ (WDMSFL,DSN,GPOSEN,NVAL,LTUNIT,LTSTEP,DTRAN,
     I               QUALFG,ENDDAT,TDSFRC,TSFILL,TGROUP,
     I               GETQK,GETQRA,VBTIME,
     M               RVAL,QVAL,GETDAT,GPSDAT,GPIND,
     O               RETCOD)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTGTVQ
     I                    (WDMSFL,DSN,GPOSEN,NVAL,GTTUN,GTTST,GTTRN,
     I                     QUALFG,ENDDAT,TDSFRC,TSFILL,TGROUP,
     I                     GETQK,GETQRA,VBTIME,
     M                     RVAL,QVAL,GETDAT,GPSDAT,GPIND,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     fills in RVAL array with data values from WDMS DSN
C             in parallel with QVAL which contains quality codes
C
C     + + + HISTORY + + +
C     Mike Horn, Oct 1993, Computing Centre for Water Research
C     KMFlynn, 6-character names, replace tsfill with 0 for quality
C              flag for missing data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    WDMSFL,DSN,GPOSEN,NVAL,GTTUN,GTTST,GTTRN,
     1           QUALFG,ENDDAT(6),TDSFRC,TGROUP,GETQK,
     1           VBTIME,GETDAT(6),GPSDAT(6),GPIND,RETCOD,
     1           QVAL(NVAL)
      REAL       RVAL(NVAL),TSFILL,GETQRA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - timeseries data set number
C     GPOSEN - end data group pointer index
C     NVAL   - number of values
C     GTTUN  - get time units
C     GTTST  - get time step
C     GTTRN  - get transformation code
C     QUALFG - get quality code
C     ENDDAT - end of get date array
C     TDSFRC - first record in data set
C     TSFILL - missing data filler code
C     TGROUP - data group pointer units
C     GETQK  - do a quick get
C     GETQRA - quick get time step ratio (user/dsn)
C     VBTIME - variable timestep indicator
C     RVAL   - array of values retrieved from WDMS file
C     QVAL   - array of quality codes retrieved from WDMS file
C     GETDAT - current get date array
C     GPSDAT - start date of first group
C     GPIND  - get group index number
C     RETCOD - return code
C                  0 - everything O.K.
C                -21 - date from WDM doesn't match expected date
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwtsds.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CURNXT(6),GETNXT(6),TMPNXT(6),WDADD,GTADD,IONE,CHK,ICNT,
     1          CHKS,TMPTUN,GPEDAT(6),NEWGRP,BADJFG,ADDAFG,EGPOS,TMPOS,
     2          I,TMPDAT(6)
      INTEGER*4 GETSPN,TMPSPN,CURSPN,I4ONE,I4NVAL,DPOS
      REAL      FRAC,CFRAC,RTMP
C
C     + + + FUNCTIONS + + +
      INTEGER   TIMCHK
C
C     + + + INTRINSICS + + +
      INTRINSIC FLOAT,ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDATCP, WTEGRP, WTSKVL, WTGTNV, TIMADD, TIMDIF, TIMCHK
C
C     + + + END SPECIFICATIONS + + +
C
C     always calculate ending intervals on first time through
      WDADD = 1
      GTADD = 1
      NEWGRP= 1
      IONE  = 1
      I4ONE = 1
      DPOS  = 1
      BADJFG= 1
      ADDAFG= 0
      I4NVAL= NVAL
      FRAC  = 0.0
      CFRAC = 0.0
      EGPOS = 0
      CALL WDATCP (GETDAT,TMPDAT)
C
 10   CONTINUE
        IF (NEWGRP.GE.1) THEN
C         find out the end of the group
          CALL WTEGRP (GPSDAT,TGROUP,
     O                 GPEDAT)
          IF (GPIND.EQ.GPOSEN) THEN
C           this is the last group, dont fill too far
            CALL WDATCP (ENDDAT,GPEDAT)
          END IF
C         skip values in group as required
          CALL WTSKVL (WDMSFL,GPIND,GPSDAT,TMPDAT,
     I                 TDSFRC,TSFILL,TGROUP,BADJFG,ADDAFG,VBTIME,
     O                 CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O                 CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O                 RETCOD)
          IF (RETCOD.EQ.-11) THEN
C           data has not started yet, this is ok
            RETCOD= 0
          END IF
C         how many intervals in group
          CALL TIMDIF (TMPDAT,GPEDAT,GTTUN,GTTST,
     O                 TMPOS)
          EGPOS = EGPOS+ TMPOS
          IF (GETQK.NE.0.AND.EGPOS.EQ.TMPOS) THEN
C           first quick get, be sure to use correct boundary
            CALL TIMDIF (CURDAT,GETDAT,GTTUN,GTTST,
     O                   GETQK)
            GETQK= GETQK+ 1
          END IF
          NEWGRP= 0
        END IF
        IF (RETCOD.EQ.0) THEN
          IF (WDADD.EQ.1) THEN
C           calculate new ending date on WDMS date
C           get the next WDS value
            CALL WTGTNV (WDMSFL,
     M                   CURCNT,CURNOV,CURCMP,CURREC,CURBKS,CURTST,
     1                   CURTUN,CURQUA,CURPOS,CURDAT,
     O                   CURVAL,CURNXT)
            TMPTUN= GTTUN
            IF (CURTUN.LT.TMPTUN) TMPTUN= CURTUN
          END IF
          IF (GETQK.EQ.0) THEN
C           not a quick get, do it all
            IF (GTADD.EQ.1) THEN
C             calculate new ending date for RVAL
              CALL TIMADD (GETDAT,GTTUN,GTTST,I4ONE,
     O                     GETNXT)
C             how many short units in get and WDMS block
              CALL TIMDIF (GETDAT,GETNXT,TMPTUN,IONE,
     O                     GETSPN)
              IF (GTTRN .LE. 1) THEN
C               some sort of data available, so initialize to zero
                RVAL(DPOS) = 0.0
                QVAL(DPOS) = CURQUA
              END IF
            END IF
C
C           figure out which interval should be incremented
            CHK= TIMCHK(GETNXT,CURNXT)
            IF (CHK.EQ.1) THEN
C             add interval to get counter
              GTADD= 1
              WDADD= 0
            ELSE IF (CHK.EQ.0) THEN
C             add intervals to both counters
              GTADD= 1
              WDADD= 1
            ELSE
C             add interval to WDMS fill counter
              GTADD= 0
              WDADD= 1
            END IF
C
            IF (QUALFG.GE.CURQUA) THEN
C             only process data of acceptable quality
              IF (GTTRN.LE.1) THEN
C               store end of this interval
                IF (CHK.EQ.1) THEN
C                 interval ends due to get
                  CALL WDATCP (GETNXT,TMPNXT)
                ELSE
C                 interval ends due to wdm or both
                  CALL WDATCP (CURNXT,TMPNXT)
                END IF
C               calc short units to end of interval
                CHKS= TIMCHK(GETDAT,CURDAT)
                IF (CHKS.EQ.1) THEN
                  CALL TIMDIF (CURDAT,TMPNXT,TMPTUN,IONE,
     O                         TMPSPN)
                ELSE
                  CALL TIMDIF (GETDAT,TMPNXT,TMPTUN,IONE,
     O                         TMPSPN)
                END IF
C
                IF (GTTRN.EQ.0) THEN
C                 transform is ave,same
                  FRAC= FLOAT(TMPSPN)/FLOAT(GETSPN)
                  RVAL(DPOS)= RVAL(DPOS)+ FRAC* CURVAL
		  QVAL(DPOS) = CURQUA
                  CFRAC= CFRAC+ FRAC
C
                ELSE IF (GTTRN.EQ.1) THEN
C                 transform is sum,div
C                 how many short units spanned in WDMS interval
                  CALL TIMDIF (CURDAT,CURNXT,TMPTUN,IONE,
     O                         CURSPN)
                  FRAC= FLOAT(TMPSPN)/ FLOAT(CURSPN)
                  RVAL(DPOS)= RVAL(DPOS)+ FRAC* CURVAL
		  QVAL(DPOS) = CURQUA
                  CFRAC= CFRAC+ (FLOAT(TMPSPN)/FLOAT(GETSPN))
                END IF
                IF (FRAC.GT.1.0) THEN
                  WRITE (*,*) 'BAD FRAC,TMPSPN:',FRAC,TMPSPN
                  WRITE (*,*) '  DSN,CTST,CTUN:',DSN,CURTST,CURTUN
                  WRITE (*,*) '  GETSPN,CURSPN:',GETSPN,CURSPN
                  WRITE (*,*) '  CURDAT:       ',CURDAT
                  WRITE (*,*) '  CURNXT:       ',CURNXT
                  WRITE (*,*) '  GETDAT:       ',GETDAT
                  WRITE (*,*) '  GETNXT:       ',GETNXT
                  WRITE (*,*) '  TMPNXT:       ',TMPNXT
                END IF
              ELSE IF (GTTRN.EQ.2) THEN
C               transform is max
                IF (RVAL(DPOS).LT.CURVAL) THEN
                  RVAL(DPOS)= CURVAL
		  QVAL(DPOS) = CURQUA
                  CFRAC     = 1.0
                END IF
              ELSE IF (GTTRN.EQ.3) THEN
C               transform is min
                IF (RVAL(DPOS).GT.CURVAL) THEN
                  RVAL(DPOS)= CURVAL
		  QVAL(DPOS) = CURQUA
                  CFRAC     = 1.0
                END IF
              END IF
            END IF
C
            IF (GTADD.EQ.1) THEN
C             get ready to increment get counter
              CALL WDATCP (GETNXT,GETDAT)
C             adjust value if some data didnt meet quality
              IF (CFRAC.LT.1.0.AND.CFRAC.GT.0.0) THEN
                RVAL(DPOS)= RVAL(DPOS)/ CFRAC
	        QVAL(DPOS)= CURQUA
              ELSE IF (CFRAC.LE.0.0) THEN
                RVAL(DPOS)= TSFILL
	        QVAL(DPOS)= 30
              END IF
              CFRAC= 0.0
              DPOS = DPOS+ 1
            END IF
C
            IF (WDADD.EQ.1) THEN
C             get ready to increment WDMS counter
              CALL WDATCP (CURNXT,CURDAT)
              CURCNT= CURCNT+ 1
              IF (TIMCHK(CURDAT,GPEDAT).LE.0) THEN
C               at the group boundary, update start of group date
                CALL WDATCP (GPEDAT,GPSDAT)
                CALL WDATCP (GPEDAT,TMPDAT)
                NEWGRP= 1
                GPIND = GPIND+ 1
              END IF
            END IF
          ELSE
C           a quick get
            FRAC= FRAC+ GETQRA
            IF (ABS(GETQRA-1.0).LT.1.0E-5) THEN
C             no transform required
              IF (QUALFG.GE.CURQUA) THEN
C               only use data of acceptable quality
                RVAL(DPOS)= CURVAL
                QVAL(DPOS)= CURQUA
              END IF
              DPOS= DPOS+ 1
              FRAC= 0.0
            ELSE IF (GETQRA.LT.1.0) THEN
C             dsn interval less than user requested
              IF (QUALFG.GE.CURQUA) THEN
C               only use data of acceptable quality
                IF (GTTRN.LE.1 .AND. CFRAC.LT.1.0E-20) THEN
C                 data available & 1st time,   so initialize to zero
                  RVAL(DPOS) = 0.0
		  QVAL(DPOS)= CURQUA
                END IF
                CFRAC= CFRAC+ GETQRA
                IF (GTTRN.EQ.0) THEN
C                 transform is aver
                  RVAL(DPOS)= RVAL(DPOS)+ GETQRA* CURVAL
		  QVAL(DPOS)= CURQUA
                ELSE IF (GTTRN.EQ.1) THEN
C                 transform is sum
                  RVAL(DPOS)= RVAL(DPOS)+ CURVAL
		  QVAL(DPOS)= CURQUA
                ELSE IF (GTTRN.EQ.2) THEN
C                 transform is max
                  IF (CURVAL.GT.RVAL(DPOS)) THEN
                          RVAL(DPOS)= CURVAL
                          QVAL(DPOS)= CURQUA
                        ENDIF
                ELSE IF (GTTRN.EQ.3) THEN
C                 transform is min
                  IF (CURVAL.LT.RVAL(DPOS)) THEN
                          RVAL(DPOS)= CURVAL
                          QVAL(DPOS)= CURQUA
                        ENDIF
                END IF
              END IF
              IF (ABS(FRAC-1.0).LT.1.0E-5) THEN
C               completed this user interval
                IF (ABS(CFRAC-1.0).GT.1.0E-5) THEN
C                 some missing data
                  IF (GTTRN.LE.1.AND.CFRAC.GT.0.0) THEN
C                   adjust result
                    RVAL(DPOS)= RVAL(DPOS)/CFRAC
		    QVAL(DPOS)= CURQUA
                  END IF
                END IF
                DPOS = DPOS+ 1
                FRAC = 0.0
                CFRAC= 0.0
              END IF
            ELSE
C             dsn interval greater than user requested
              ICNT= GETQRA
C             may not start of wdm data boundary
              I   = GETQK
              RTMP= CURVAL
              IF (GTTRN.EQ.1) THEN
C               transform is sum/div
                RTMP= RTMP/GETQRA
              END IF
 20           CONTINUE
                IF (QUALFG.GE.CURQUA) THEN
C                 only use data of acceptable quality
                  RVAL(DPOS)= RTMP
                END IF
                DPOS= DPOS+ 1
                I   = I+ 1
              IF (I.LE.ICNT.AND.DPOS.LE.I4NVAL) GO TO 20
C             assume start on data boundary next time
              GETQK= 1
            END IF
C           always get the next dsn data value
            CALL WDATCP (CURNXT,CURDAT)
            CURCNT= CURCNT+ 1
          END IF
        ELSE IF (RETCOD.EQ.-10) THEN
C         missing entire group
          DPOS  = EGPOS+ 1
C         reset where we are in process of getting data
          CALL WDATCP (GPEDAT,GETDAT)
          GTADD = 1
          RETCOD= 0
        END IF
C
        IF (DPOS.GT.EGPOS .AND. NEWGRP.EQ.0) THEN
C         at the group boundary, update start of group date
          CALL WDATCP (GPEDAT,GPSDAT)
          CALL WDATCP (GPEDAT,TMPDAT)
          NEWGRP= 1
          GPIND = GPIND+ 1
        END IF
C
      IF (DPOS.LE.I4NVAL.AND.RETCOD.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDTGQR
     I                    (WDMSFL,DSN,DELT,DATES,NVAL,
     I                     DTRAN,QUALFL,QUALFH,TUNITS,
     O                     RVAL,RETCOD)
C
C     + + + PURPOSE + + +
C     Gets timeseries information from the WDMSFL.  Returns the time-
C     series values for a range of quality codes.
C
C     + + + HISTORY + + +
C     Mike Horn, Oct 1993, Computing Centre for Water Research
C     KMFlynn, 6-character names
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DELT,DATES(6),NVAL,DTRAN,QUALFL,
     1          QUALFH,TUNITS,RETCOD
      REAL      RVAL(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     DELT   - time step for get
C     DATES  - starting date
C     NVAL   - number of values
C     DTRAN  - transformation code
C              0 - ave,same
C              1 - sum,div
C              2 - max
C              3 - min
C     QUALFL - lower allowed quality code
C     QUALFH - higher allowed quality code
C     TUNITS - time units for get
C     RVAL   - array to place retrieved values in
C     RETCOD - return code
C                0 - everything O.K.
C               -8 - invalid date
C              -14 - date specified not within valid range for data set
C              -20 - problem with one or more of following:
C                    GPFLG, DXX, NVAL, QUALVL, LTSTEP, LTUNIT
C              -21 - date from WDM doesn't match expected date
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GPFLG,GPOSEN,GPIND,LTSTEP,LTUNIT,TDSFRC,TGROUP,TSPTAD,
     1          ENDDAT(6),GPSDAT(6),GETDAT(6),TSPSC1,TSPSC2,
     2          COMPFG,TSFORM,VBTIME,TSSTEP,TCODE,GETQK,RIND
      INTEGER*4 I4NVAL
      REAL      DEFVAL,TOLR,TSFILL,GETQRA,RTMP
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + INTRINSICS + + +
      INTRINSIC FLOAT, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPR, WTPMCK, WTFNDG, WDATCP, WTGTQR, WDRCGO, WTDSPX
      EXTERNAL  WTSCSC
C
C     + + + END SPECIFICATIONS + + +
C
      I4NVAL= NVAL
      LTSTEP= DELT
      LTUNIT= TUNITS
      GPFLG = 1
      TSFILL= 0.0
C
C     check the user supplied parameters
      CALL WTPMCK (GPFLG,DTRAN,DATES,NVAL,QUALFH,
     M             LTSTEP,LTUNIT,
     O             RETCOD)
      IF (RETCOD.EQ.0) THEN
C       check the data set and figure out which groups have been req.
        CALL WTFNDG (WDMSFL,DSN,GPFLG,DATES,LTSTEP,LTUNIT,I4NVAL,
     O               TDSFRC,TSFILL,TGROUP,TOLR,TSPTAD,
     1               GPIND,GPOSEN,GPSDAT,ENDDAT,RETCOD)
      END IF
C     fill in RVAL with defaults
      DEFVAL= TSFILL
C     max
      IF (DTRAN.EQ.2) DEFVAL= -1.0E30
C     min
      IF (DTRAN.EQ.3) DEFVAL= 1.0E30
      CALL ZIPR (NVAL,DEFVAL,
     O           RVAL)
C
      IF (RETCOD.EQ.0) THEN
C       get additional parameters
        RIND= WDRCGO(WDMSFL,TDSFRC)
        CALL WTDSPX (WIBUFF(1,RIND),
     O               COMPFG,TSFORM,VBTIME,TSSTEP,TCODE)
        GETQK= 0
C       can we do a quick get?
        IF (VBTIME.EQ.1) THEN
C         yes, if time units and step ok
          IF (TCODE.LE.4.AND.LTUNIT.LE.4) THEN
C           time units days or shorter, a quick get may work
            CALL WTSCSC (LTUNIT,LTSTEP,TSPSC1)
            CALL WTSCSC (TCODE,TSSTEP,TSPSC2)
            GETQRA= 1.0E-8+ FLOAT(TSPSC2)/FLOAT(TSPSC1)
            RTMP  = GETQRA
            IF (RTMP.LT.1.0) THEN
C             wdm interval less than user interval
              RTMP= 1.0/ GETQRA
            END IF
            IF (MOD(RTMP,1.0).LT.1.0E-6) THEN
C             ok to do a quick get
              GETQK= 1
            END IF
          ELSE IF (TCODE.EQ.LTUNIT) THEN
C           time units are the same, a quick get will work
            GETQK = 1
            GETQRA= 1.0E-8+ FLOAT(TSSTEP)/FLOAT(LTSTEP)
          END IF
        ELSE
C         do a general get
          GETQK= 0
        END IF
C       make a working copy of the starting date
        CALL WDATCP (DATES,GETDAT)
C       get the data
        CALL WTGTQR (WDMSFL,DSN,GPOSEN,NVAL,LTUNIT,LTSTEP,DTRAN,
     I               QUALFL,QUALFH,ENDDAT,TDSFRC,TSFILL,TGROUP,
     I               GETQK,GETQRA,VBTIME,
     M               RVAL,GETDAT,GPSDAT,GPIND,
     O               RETCOD)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTGTQR
     I                    (WDMSFL,DSN,GPOSEN,NVAL,GTTUN,GTTST,GTTRN,
     I                     QUALFL,QUALFH,ENDDAT,TDSFRC,TSFILL,TGROUP,
     I                     GETQK,GETQRA,VBTIME,
     M                     RVAL,GETDAT,GPSDAT,GPIND,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     Fills in RVAL array with data values from WDMS DSN.
C
C     + + + HISTORY + + +
C     Mike Horn, Oct 1993, Computing Centre for Water Research
C     KMFlynn, 6-character names
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    WDMSFL,DSN,GPOSEN,NVAL,GTTUN,GTTST,GTTRN,
     1           QUALFL,QUALFH,ENDDAT(6),TDSFRC,TGROUP,GETQK,
     1           VBTIME,GETDAT(6),GPSDAT(6),GPIND,RETCOD
      REAL       RVAL(NVAL),TSFILL,GETQRA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - timeseries data set number
C     GPOSEN - end data group pointer index
C     NVAL   - number of values
C     GTTUN  - get time units
C     GTTST  - get time step
C     GTTRN  - get transformation code
C     QUALFL - get lower quality code
C     QUALFH - get higher quality code
C     ENDDAT - end of get date array
C     TDSFRC - first record in data set
C     TSFILL - missing data filler code
C     TGROUP - data group pointer units
C     GETQK  - do a quick get
C     GETQRA - quick get time step ratio (user/dsn)
C     VBTIME - variable timestep indicator
C     RVAL   - array of values retrieved from WDMS file
C     GETDAT - current get date array
C     GPSDAT - start date of first group
C     GPIND  - get group index number
C     RETCOD - return code
C                  0 - everything O.K.
C                -21 - date from WDM doesn't match expected date
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwtsds.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CURNXT(6),GETNXT(6),TMPNXT(6),WDADD,GTADD,IONE,CHK,ICNT,
     1          CHKS,TMPTUN,GPEDAT(6),NEWGRP,BADJFG,ADDAFG,EGPOS,TMPOS,
     2          I,TMPDAT(6)
      INTEGER*4 GETSPN,TMPSPN,CURSPN,I4ONE,I4NVAL,DPOS
      REAL      FRAC,CFRAC,RTMP
C
C     + + + FUNCTIONS + + +
      INTEGER   TIMCHK
C
C     + + + INTRINSICS + + +
      INTRINSIC FLOAT,ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDATCP, WTEGRP, WTSKVL, WTGTNV, TIMADD, TIMDIF, TIMCHK
C
C     + + + END SPECIFICATIONS + + +
C
C     always calculate ending intervals on first time through
      WDADD = 1
      GTADD = 1
      NEWGRP= 1
      IONE  = 1
      I4ONE = 1
      DPOS  = 1
      BADJFG= 1
      ADDAFG= 0
      I4NVAL= NVAL
      FRAC  = 0.0
      CFRAC = 0.0
      EGPOS = 0
      CALL WDATCP (GETDAT,TMPDAT)
C
 10   CONTINUE
        IF (NEWGRP.GE.1) THEN
C         find out the end of the group
          CALL WTEGRP (GPSDAT,TGROUP,
     O                 GPEDAT)
          IF (GPIND.EQ.GPOSEN) THEN
C           this is the last group, dont fill too far
            CALL WDATCP (ENDDAT,GPEDAT)
          END IF
C         skip values in group as required
          CALL WTSKVL (WDMSFL,GPIND,GPSDAT,TMPDAT,
     I                 TDSFRC,TSFILL,TGROUP,BADJFG,ADDAFG,VBTIME,
     O                 CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O                 CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O                 RETCOD)
          IF (RETCOD.EQ.-11) THEN
C           data has not started yet, this is ok
            RETCOD= 0
          END IF
C         how many intervals in group
          CALL TIMDIF (TMPDAT,GPEDAT,GTTUN,GTTST,
     O                 TMPOS)
          EGPOS = EGPOS+ TMPOS
          IF (GETQK.NE.0.AND.EGPOS.EQ.TMPOS) THEN
C           first quick get, be sure to use correct boundary
            CALL TIMDIF (CURDAT,GETDAT,GTTUN,GTTST,
     O                   GETQK)
            GETQK= GETQK+ 1
          END IF
          NEWGRP= 0
        END IF
        IF (RETCOD.EQ.0) THEN
          IF (WDADD.EQ.1) THEN
C           calculate new ending date on WDMS date
C           get the next WDS value
            CALL WTGTNV (WDMSFL,
     M                   CURCNT,CURNOV,CURCMP,CURREC,CURBKS,CURTST,
     1                   CURTUN,CURQUA,CURPOS,CURDAT,
     O                   CURVAL,CURNXT)
            TMPTUN= GTTUN
            IF (CURTUN.LT.TMPTUN) TMPTUN= CURTUN
          END IF
          IF (GETQK.EQ.0) THEN
C           not a quick get, do it all
            IF (GTADD.EQ.1) THEN
C             calculate new ending date for RVAL
              CALL TIMADD (GETDAT,GTTUN,GTTST,I4ONE,
     O                     GETNXT)
C             how many short units in get and WDMS block
              CALL TIMDIF (GETDAT,GETNXT,TMPTUN,IONE,
     O                     GETSPN)
              IF (GTTRN .LE. 1) THEN
C               some sort of data available, so initialize to zero
                RVAL(DPOS) = 0.0
              END IF
            END IF
C
C           figure out which interval should be incremented
            CHK= TIMCHK(GETNXT,CURNXT)
            IF (CHK.EQ.1) THEN
C             add interval to get counter
              GTADD= 1
              WDADD= 0
            ELSE IF (CHK.EQ.0) THEN
C             add intervals to both counters
              GTADD= 1
              WDADD= 1
            ELSE
C             add interval to WDMS fill counter
              GTADD= 0
              WDADD= 1
            END IF
C
            IF (QUALFH.GE.CURQUA .AND. QUALFL .LE. CURQUA) THEN
C             only process data of acceptable quality
              IF (GTTRN.LE.1) THEN
C               store end of this interval
                IF (CHK.EQ.1) THEN
C                 interval ends due to get
                  CALL WDATCP (GETNXT,TMPNXT)
                ELSE
C                 interval ends due to wdm or both
                  CALL WDATCP (CURNXT,TMPNXT)
                END IF
C               calc short units to end of interval
                CHKS= TIMCHK(GETDAT,CURDAT)
                IF (CHKS.EQ.1) THEN
                  CALL TIMDIF (CURDAT,TMPNXT,TMPTUN,IONE,
     O                         TMPSPN)
                ELSE
                  CALL TIMDIF (GETDAT,TMPNXT,TMPTUN,IONE,
     O                         TMPSPN)
                END IF
C
                IF (GTTRN.EQ.0) THEN
C                 transform is ave,same
                  FRAC= FLOAT(TMPSPN)/FLOAT(GETSPN)
                  RVAL(DPOS)= RVAL(DPOS)+ FRAC* CURVAL
                  CFRAC= CFRAC+ FRAC
C
                ELSE IF (GTTRN.EQ.1) THEN
C                 transform is sum,div
C                 how many short units spanned in WDMS interval
                  CALL TIMDIF (CURDAT,CURNXT,TMPTUN,IONE,
     O                         CURSPN)
                  FRAC= FLOAT(TMPSPN)/ FLOAT(CURSPN)
                  RVAL(DPOS)= RVAL(DPOS)+ FRAC* CURVAL
                  CFRAC= CFRAC+ (FLOAT(TMPSPN)/FLOAT(GETSPN))
                END IF
                IF (FRAC.GT.1.0) THEN
                  WRITE (*,*) 'BAD FRAC,TMPSPN:',FRAC,TMPSPN
                  WRITE (*,*) '  DSN,CTST,CTUN:',DSN,CURTST,CURTUN
                  WRITE (*,*) '  GETSPN,CURSPN:',GETSPN,CURSPN
                  WRITE (*,*) '  CURDAT:       ',CURDAT
                  WRITE (*,*) '  CURNXT:       ',CURNXT
                  WRITE (*,*) '  GETDAT:       ',GETDAT
                  WRITE (*,*) '  GETNXT:       ',GETNXT
                  WRITE (*,*) '  TMPNXT:       ',TMPNXT
                END IF
              ELSE IF (GTTRN.EQ.2) THEN
C               transform is max
                IF (RVAL(DPOS).LT.CURVAL) THEN
                  RVAL(DPOS)= CURVAL
                  CFRAC     = 1.0
                END IF
              ELSE IF (GTTRN.EQ.3) THEN
C               transform is min
                IF (RVAL(DPOS).GT.CURVAL) THEN
                  RVAL(DPOS)= CURVAL
                  CFRAC     = 1.0
                END IF
              END IF
            END IF
C
            IF (GTADD.EQ.1) THEN
C             get ready to increment get counter
              CALL WDATCP (GETNXT,GETDAT)
C             adjust value if some data didnt meet quality
              IF (CFRAC.LT.1.0.AND.CFRAC.GT.0.0) THEN
                RVAL(DPOS)= RVAL(DPOS)/ CFRAC
              ELSE IF (CFRAC.LE.0.0) THEN
                RVAL(DPOS)= TSFILL
              END IF
              CFRAC= 0.0
              DPOS = DPOS+ 1
            END IF
C
            IF (WDADD.EQ.1) THEN
C             get ready to increment WDMS counter
              CALL WDATCP (CURNXT,CURDAT)
              CURCNT= CURCNT+ 1
              IF (TIMCHK(CURDAT,GPEDAT).LE.0) THEN
C               at the group boundary, update start of group date
                CALL WDATCP (GPEDAT,GPSDAT)
                CALL WDATCP (GPEDAT,TMPDAT)
                NEWGRP= 1
                GPIND = GPIND+ 1
              END IF
            END IF
          ELSE
C           a quick get
            FRAC= FRAC+ GETQRA
            IF (ABS(GETQRA-1.0).LT.1.0E-5) THEN
C             no transform required
                  IF (QUALFH.GE.CURQUA .AND. QUALFL .LE. CURQUA) THEN
C               only use data of acceptable quality
                RVAL(DPOS)= CURVAL
              END IF
              DPOS= DPOS+ 1
              FRAC= 0.0
            ELSE IF (GETQRA.LT.1.0) THEN
C             dsn interval less than user requested
                  IF (QUALFH.GE.CURQUA .AND. QUALFL .LE. CURQUA) THEN
C               only use data of acceptable quality
                IF (GTTRN.LE.1 .AND. CFRAC.LT.1.0E-20) THEN
C                 data available & 1st time,   so initialize to zero
                  RVAL(DPOS) = 0.0
                END IF
                CFRAC= CFRAC+ GETQRA
                IF (GTTRN.EQ.0) THEN
C                 transform is aver
                  RVAL(DPOS)= RVAL(DPOS)+ GETQRA* CURVAL
                ELSE IF (GTTRN.EQ.1) THEN
C                 transform is sum
                  RVAL(DPOS)= RVAL(DPOS)+ CURVAL
                ELSE IF (GTTRN.EQ.2) THEN
C                 transform is max
                  IF (CURVAL.GT.RVAL(DPOS)) RVAL(DPOS)= CURVAL
                ELSE IF (GTTRN.EQ.3) THEN
C                 transform is min
                  IF (CURVAL.LT.RVAL(DPOS)) RVAL(DPOS)= CURVAL
                END IF
              END IF
              IF (ABS(FRAC-1.0).LT.1.0E-5) THEN
C               completed this user interval
                IF (ABS(CFRAC-1.0).GT.1.0E-5) THEN
C                 some missing data
                  IF (GTTRN.LE.1.AND.CFRAC.GT.0.0) THEN
C                   adjust result
                    RVAL(DPOS)= RVAL(DPOS)/CFRAC
                  END IF
                END IF
                DPOS = DPOS+ 1
                FRAC = 0.0
                CFRAC= 0.0
              END IF
            ELSE
C             dsn interval greater than user requested
              ICNT= GETQRA
C             may not start of wdm data boundary
              I   = GETQK
              RTMP= CURVAL
              IF (GTTRN.EQ.1) THEN
C               transform is sum/div
                RTMP= RTMP/GETQRA
              END IF
 20           CONTINUE
                    IF (QUALFH.GE.CURQUA .AND. QUALFL .LE. CURQUA) THEN
C                 only use data of acceptable quality
                  RVAL(DPOS)= RTMP
                END IF
                DPOS= DPOS+ 1
                I   = I+ 1
              IF (I.LE.ICNT.AND.DPOS.LE.I4NVAL) GO TO 20
C             assume start on data boundary next time
              GETQK= 1
            END IF
C           always get the next dsn data value
            CALL WDATCP (CURNXT,CURDAT)
            CURCNT= CURCNT+ 1
          END IF
        ELSE IF (RETCOD.EQ.-10) THEN
C         missing entire group
          DPOS  = EGPOS+ 1
C         reset where we are in process of getting data
          CALL WDATCP (GPEDAT,GETDAT)
          GTADD = 1
          RETCOD= 0
        END IF
C
        IF (DPOS.GT.EGPOS .AND. NEWGRP.EQ.0) THEN
C         at the group boundary, update start of group date
          CALL WDATCP (GPEDAT,GPSDAT)
          CALL WDATCP (GPEDAT,TMPDAT)
          NEWGRP= 1
          GPIND = GPIND+ 1
        END IF
C
      IF (DPOS.LE.I4NVAL.AND.RETCOD.EQ.0) GO TO 10
C
      RETURN
      END
