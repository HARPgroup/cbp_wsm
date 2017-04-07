C
C
C
      SUBROUTINE   HSPF
     I                   (FILES,FILNAM,
     O                    RETCOD)
CGaryShenk     I                   (FILES,
C
C     + + + PURPOSE + + +
C     Main routine for HSPF without file management or system dep stuff.
C     Copies Data Set from User's Control Input to memory, finds start
C     and end of the Data Set and hands control to appropriate part
C     of HSPF software for execution of user's instructions.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FILES(15),RETCOD
C
C     + + + ARGUMENT DEFINTIONS + + +
C     FILES  - unit numbers of files from files block in uci or application
C     RETCOD - return code - 0:run completed
C                            1:interp only
C                            2:errors in interp
C                            3:no run keyword found
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I1,SCLU,SGRP,MESSU,MSGFL,INITFG,CLEN,CONT,KCNT,
     $             ECOUNT,KWDDIM(1),KWDTYP(1),MKFILS
      CHARACTER*1  CHSTR1(20),KWDLIB(12)
      character*64 FILNAM
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR1,CHSTR)
      CHARACTER*20 CHSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL  PMXTFT, KEYUCI, DMPKEY, INTERP, OSUPER, WMSGTT
      EXTERNAL  INIKEY, HDMEST
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (12A1,2I4)
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      I0= 0
C     no errors yet
      ECOUNT= 0
C
      MESSU= FILES(1)
      MSGFL= FILES(15)
C
      SCLU= 201
C
C     title block to output file
      SGRP= 1
      CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
C
C     start of job message
      SGRP= 2
      CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
C
C     first pre-process input for a data set message
      SGRP= 3
      CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
C
C     get major keywords (RUN)
      SGRP= 21
      INITFG= 1
      CLEN= 20
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1000) KWDLIB,KWDDIM,KWDTYP
C
C     look for a fresh set of keywords
      CALL INIKEY
C
C     look for major keywords
      CLEN= 4
      CALL KEYUCI (I1,CLEN,I0,I0,I1,KWDLIB,KWDDIM,KWDTYP,
     M             ECOUNT,
     O             KCNT)
      CALL DMPKEY
C
      IF (ECOUNT .EQ. 0) THEN
C       a clean run data set was found, interpret it
        SGRP= 58
        CALL HDMEST (MSGFL,SCLU,SGRP)
        MKFILS= 1
        CALL INTERP (SCLU,MKFILS,
     M               FILES,
     O               RETCOD)
C
        IF (RETCOD .EQ. 0) THEN
C         run data set interpreted without error - run it
          SGRP= 59
          CALL HDMEST (MSGFL,SCLU,SGRP)
CGaryShenk          CALL OSUPER (FILES)
          CALL OSUPER (FILES,FILNAM)
C         all done - end of job
          SGRP= 4
          CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
        END IF
      ELSE
C       no run keyword found
        RETCOD= 3
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   INTERP
     I                    (SCLU,MKFILS,
     M                     FILES,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     Read and process a run data set in the user's control input
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    SCLU,MKFILS,FILES(15),RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SCLU   - message file cluster containing info needed here
C     MKFILS - flag to indicate if instruction files should be written,
C                0-no,1-yes
C     FILES  - array of file unit numbers
C     RETCOD - return code 0:interp ok
C                          1:no execution desired
C                          2:errors found
C
C     + + + PARAMETERS + + +
      INTEGER      MAXBLK,MAXMLK
      PARAMETER    (MAXBLK=25)
      PARAMETER    (MAXMLK=50)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION INTERP1 + + +
      INCLUDE    'crin1.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      EMFG,RUNWID,I,I1,I0,KEYND,KEYST,CONDFG,
     $             MESSU,MSGFL,LKWOPR,NKWOPR,
     $             NDAMON(12),RUNMIN,RUNFG,SEDAT(10),
     $             SGRP,MSLINX(MAXMLK,3),NMLTBS,KTYP,SPOUT
      CHARACTER*80 CHSTR,DUMCHR
      CHARACTER*12 BLNK12
      CHARACTER*1  KWDOPR(8,MAXBLK)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1),(SEDAT,SDATIM),(SEDAT(6),EDATIM)
      CHARACTER*1  CHSTR1(80)
      INTEGER      SDATIM(5),EDATIM(5)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TABBLK,MSLKBK,OPNBLK,DUMPER,HDMES3
      EXTERNAL   TIMSER,OSUP,PSPECL,ZIPI,MDATBK,HDMESN
      EXTERNAL   PTHBLK,GETSE,PMXTFT,UCIGEN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NDAMON/31, 0,31,30,31,30,31,31,30,31,30,31/
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C
      I0= 0
      I1= 1
C
      MESSU= FILES(1)
      MSGFL= FILES(15)
C
C     put file unit numbers in common block
      DO 10 I= 1, 15
        FILE(I)= FILES(I)
 10   CONTINUE
C
C     interp message to output file
      SGRP= 5
      CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
C
C     warning info
      I= 10
      CALL ZIPI (I,I0,
     O           WCOUNT)
C
C     error info
      ECOUNT= 0
C
      CALL UCIGEN (MSGFL,MESSU,MKFILS,MAXOPN,MAXBLK,
     M             ECOUNT,WCOUNT,
     O             SEDAT,SDATIM,EDATIM,RUNMIN,OUTLEV,
     O             RESMFG,RUNFG,EMFG,DUMCHR,NIVLS,IVLLIB,
     O             EXGTAB,GRPTAB,OPNTAB,NXGRPS,NGRPS,
     O             NOPNS,LKWOPR,NKWOPR,KWDOPR,SPOUT)
C
C     ftables block
      KTYP= 4
      CALL GETSE (KTYP,I1,
     O            KEYST,KEYND)
      CALL TABBLK (KEYST,KEYND,OUTLEV,MESSU,
     I             MSGFL,MAXFTB,
     M             ECOUNT,
     O             TABINX,NFTABS)
C
C     mass-link block
      CALL MSLKBK (OUTLEV,MESSU,
     I             MSGFL,MAXMLK,
     M             ECOUNT,
     O             MSLINX,NMLTBS)
C
C     month-data block
      KTYP= 14
      CALL GETSE (KTYP,I1,
     O            KEYST,KEYND)
      CALL MDATBK (KEYST,KEYND,OUTLEV,MESSU,
     I             MSGFL,MAXMDT,
     M             ECOUNT,
     O             MDTINX,NMDATS)
C
C     pathnames block
      KTYP= 15
      CALL GETSE (KTYP,I1,
     O            KEYST,KEYND)
      CALL PTHBLK (KEYST,KEYND,OUTLEV,MESSU,MSGFL,
     M             ECOUNT)
C
C     process the operation-type blocks
      CALL OPNBLK (SDATIM,EDATIM,NDAMON,EMFG,SCLU,
     I             LKWOPR,NKWOPR,KWDOPR)
C     black out table name on screen
      BLNK12 = '            '
      CALL HDMESN (I0)
      CALL HDMES3 (BLNK12)
C
C     process any special action instructions
      KTYP= 9
      CALL GETSE (KTYP,I1,
     O            KEYST,KEYND)
      CALL PSPECL (KEYST,KEYND,NDAMON,SDATIM,EDATIM,SPOUT,
     O             RUNWID,CONDFG)
C
C     look for formats block
      KTYP= 6
      CALL GETSE (KTYP,I1,
     O            KEYST,KEYND)
      IF ( (KEYST .NE. 0) .AND. (OUTLEV .GT. 2) ) THEN
C       formats block is present, processing message
        SGRP= 10
        CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
C       dump formats block
        CALL DUMPER (KEYST,KEYND,MESSU)
C       end processing message
        SGRP= 11
        CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
      END IF
C
C     process blocks dealing with time series linkages -
C     ext sources, network, and ext targets
      CALL TIMSER (SDATIM,EDATIM,MAXMLK,MSLINX,NMLTBS,RUNWID)
C
      IF (MKFILS .EQ. 1) THEN
C       write the operations supervisor instruction file
        CALL OSUP (SDATIM,EDATIM,RUNMIN,RUNWID)
      END IF
C
C     interp complete message
      SGRP= 12
      CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
C
      IF (RUNFG .EQ. 0) THEN
C       user did not want to execute
        SGRP= 14
        CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
        RETCOD= 1
      END IF
C
      IF (ECOUNT .GT. 0) THEN
C       errors found in uci
        SGRP= 13
        CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
        RETCOD= 2
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OPNBLK
     I                   (SDATIM,EDATIM,NDAMON,EMFG,SCLU,
     I                    LKWOPR,NKWOPR,KWDOPR)
C
C     + + + PURPOSE + + +
C     Process each <operation - type> block in the run data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     SDATIM(5),EDATIM(5),NDAMON(12),EMFG,SCLU,
     $            LKWOPR,NKWOPR
      CHARACTER*1 KWDOPR(LKWOPR,NKWOPR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     SCLU   - cluster containing general info
C     LKWOPR - length of operation name keywords
C     NKWOPR - number of operation name keywords
C     KWDOPR - operation name keywords
C
C     + + + PARAMETERS + + +
      INCLUDE    'pmxosv.inc'
C
C     + + + COMMON BLOCKS- INTERP2 + + +
      INCLUDE    'crin2.inc'
      INCLUDE    'crin2c.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      GRP,I,I0,I1,I12,I2,IX,KTYP,KCNT,
     $             J,K,KEYND,KEYST,LTABT,NDELT,NTABT,
     $             OMCODE,OMCNT,OSVKEY,START,CNUM,CLEN(1),
     $             MESSU,MSGFL,LCLU,SGRP,INITFG,CONT,
     $             TABBAS,TKEYST,TKEYND,TABXXX(MAXTTP)
      CHARACTER*12 OPTNAM
      CHARACTER*80 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1),(OPTNAM,OPTNM1)
      CHARACTER*1  CHSTR1(80),OPTNM1(12)
C
C     + + + FUNCTIONS + + +
      INTEGER    OPNNO, CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   DUMPER,PPERLN,PIMPLN,PRCHRE,PCOPY,PPLTGN
      EXTERNAL   OPNNO,CHKSTR,PDISPL,PDURAN,PGENER,PMUTSN,WMSGTT
      EXTERNAL   GETKNM,GETSE,PMXTFC,KEYUCI,ZIPI,HDMES2,HDMESN
C
C     + + + INPUT FORMATS + + +
 1020 FORMAT (3X,I3,2X,A12)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I2= 2
      I12= 12
      IX= 32767
      MESSU= FILE(1)
      MSGFL= FILE(15)
C
      OSVKEY= 0
C
C     loop thru all operations by type
      OMCNT= 0
 10   CONTINUE
        OMCNT= OMCNT+ 1
        KTYP= 100
        CALL GETSE (KTYP,OMCNT,
     O              KEYST,KEYND)
C
        IF (KEYST .EQ. 0) THEN
C         we are done
          OPTNAM= ' '
        ELSE
          CALL HDMES2 (KTYP,OMCNT)
C         put blank operation number to screen
          CALL HDMESN (I0)
C         block for what operation-type was supplied?
          CALL GETKNM (KTYP,OMCNT,
     O                 OPTNAM)
          OPTYP= OPTNAM(1:8)
          OMCODE= CHKSTR(LKWOPR,NKWOPR,OPTNM1,KWDOPR)
C         get keyword info about tables
          LCLU= OMCODE+ 120
          LTABT= 3
          SGRP= 1
          INITFG= 1
          NTABT= 1
          TABBAS= OMCODE*1000
C
 20       CONTINUE
            CLEN(1)= 80
            CALL WMSGTT (MSGFL,LCLU,SGRP,INITFG,
     M                   CLEN,
     O                   CHSTR1,CONT)
            READ (CHSTR,1020) TABDIM(NTABT),TABNAM(NTABT)
            TABXXX(NTABT)= TABBAS+ NTABT
            NTABT= NTABT+ 1
            INITFG= 0
          IF (CONT .EQ. 1) GO TO 20
C
          IF (OUTLEV .GT. 0) THEN
C           doing this type
            CNUM= 1
            CLEN(1)= 8
            SGRP= 41
            CALL PMXTFC (MSGFL,MESSU,SCLU,SGRP,CNUM,CLEN,OPTNM1)
            IF (OUTLEV .GT. 5) THEN
C             dump user's control input
              CALL DUMPER (KEYST,KEYND,MESSU)
            END IF
          END IF
C
C         find and validate the keys to each table of information
C         contained in this operation-type block
C
          CALL KEYUCI (NTABT,I12,I2,KTYP,OMCNT,
     I                 TABNM1,TABDIM,TABXXX,
     M                 ECOUNT,
     O                 KCNT)
          KYST(1)= 1
          IF (NTABT .GT. 1) THEN
            DO 30 J= 2, NTABT
              KYST(J)= KYST(J- 1)+ TABDIM(J- 1)
  30         CONTINUE
          END IF
C
          CALL ZIPI (MAXTBL,I0,
     O               TABKST)
          CALL ZIPI (MAXTBL,I0,
     O               TABKND)
C
C         loop thru all tables
          I= 0
  40      CONTINUE
C           loop thru all occurances
            I= I+ 1
            K= 0
  50        CONTINUE
              K= K+ 1
              CALL GETSE (TABXXX(I),K,
     O                    TKEYST,TKEYND)
              IF (TKEYST .GT. 0) THEN
C               save it
                J= KYST(I)+ K- 1
                TABKST(J)= TKEYST
                TABKND(J)= TKEYND
              END IF
            IF (TKEYST .GT. 0) GO TO 50
          IF (I .LT. NTABT) GO TO 40
C
          LTABTS= LTABT
          NTABTS= NTABT
C
C         find the first operation of this type, in opntab
          OPNO= OPNNO (OPTYP,I0,IX,MAXOPN,OPNTAB,I1,NOPNS)
C
C         whiledo opno> 0
 60       CONTINUE
            IF (OPNO .NE. 0) THEN
C             process the input to an operation of this type
              GRP= OPNTAB(6,OPNO)
              NDELT= GRPTAB(3,GRP)
              OPTNO= OPNTAB(3,OPNO)
C             put operation number to screen
              CALL HDMESN (OPTNO)
C
              IF (OMCODE .EQ. 1) THEN
C               perlnd module
                CALL PPERLN (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                       OSVKEY)
C
              ELSE IF (OMCODE .EQ. 2) THEN
C               implnd module
                CALL PIMPLN (OUTLEV,MESSU,MSGFL,RESMFG,
     I                       NDELT,SDATIM,NDAMON,OPNO,EMFG,MAXOPN,
     I                       MAXOSV,
     M                       OSVKEY,OPNTAB,ECOUNT)
C
              ELSE IF (OMCODE .EQ. 3) THEN
C               rchres module
                CALL PRCHRE (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                       OSVKEY)
C
              ELSE IF (OMCODE .EQ. 4) THEN
C               copy module
                CALL PCOPY (NDELT,SDATIM,NDAMON,MAXOSV,
     M                      OSVKEY)
C
              ELSE IF (OMCODE .EQ. 5) THEN
C               pltgen module
                 CALL PPLTGN (NDELT,SDATIM,NDAMON,MAXOSV,
     M                        OSVKEY)
C
              ELSE IF (OMCODE .EQ. 6) THEN
C               disply module
                CALL PDISPL (NDELT,SDATIM,NDAMON,MAXOSV,
     M                       OSVKEY)
C
              ELSE IF (OMCODE .EQ. 7) THEN
C               duranl module
                CALL PDURAN (NDELT,SDATIM,EDATIM,NDAMON,MAXOSV,
     M                       OSVKEY)
C
              ELSE IF (OMCODE .EQ. 8) THEN
C               gener module
                CALL PGENER (NDELT,SDATIM,NDAMON,MAXOSV,
     M                       OSVKEY)
C
              ELSE IF (OMCODE .EQ. 9) THEN
C               mutsin module
                CALL PMUTSN (NDELT,SDATIM,NDAMON,MAXOSV,
     M                       OSVKEY)
C
              END IF
C
C             find the next operation of this type
              START= OPNO+ 1
              OPNO= OPNNO (OPTYP,I0,IX,MAXOPN,OPNTAB,START,NOPNS)
            END IF
          IF (OPNO .GT. 0) GO TO 60
C         end whiledo
C
          IF (OUTLEV .GT. 0) THEN
C           done this type
            CNUM= 1
            CLEN(1)= 8
            SGRP= 42
            CALL PMXTFC (MSGFL,MESSU,SCLU,SGRP,CNUM,CLEN,OPTNM1)
          END IF
        END IF
      IF (OPTNAM .NE. ' ') GO TO 10
C
      RETURN
      END
