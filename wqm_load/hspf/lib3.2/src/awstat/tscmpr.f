C
C
C
      SUBROUTINE   TSCMPR
     I                    (MESSFL,WDMFL,IGR,SELFG,PTHNAM,
     M                     DSNCNT,DSNBMX,DSNBUF)
C
C     + + + PURPOSE + + +
C     Control the calculation of flow-duration statistics,
C     flow-duration tables and flow-duration plots when 
C     comparing 2 time series.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMFL,IGR,DSNCNT,DSNBMX,DSNBUF(DSNBMX),SELFG
      CHARACTER*8   PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMFL  - Fortran unit number of users WDM file
C     IGR    - graphics available flag
C              1 - graphics available, 2 - graphics not available
C     DSNCNT - number of data sets in the buffer
C     DSNBMX - size of data set buffer
C     DSNBUF - array of data set numbers to be processed
C     SELFG  - flag indication if user can select data sets from here
C     PTHNAM - path chosen to get here
C
C     + + + LOCAL VARIABLES   + + +
      INTEGER      I,I0,I1,I64,SCLU,SGRP,RETCOD,
     &             DELFG,FOUT,IVAL(35),CVAL(5,3),CNUM,ILEN,
     &             NCI,IPLOT,TSTUFG,PRECFG,IRET,IRET2,
     &             ISCN,NEWFUN,INQERR, I2, IAR(1), OPTC
      REAL         BOUND(2),CLASS(35),RVAL(2),BADVAL(2)
      CHARACTER*1  BLNK,BUFF(80,35),FLNAM1(64),NEWFL1(64)
      CHARACTER*8  LPTHNM
      CHARACTER*64 FLNAME,NEWFIL
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD, ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL  QRESP,  PRNTXT, QRESPX, QFOPFN, QFCLOS
      EXTERNAL  ZSTCMA, ZGTRET, ZWNSOP, GETFUN
      EXTERNAL  Q1INIT, QSETI,  QSETR,  QSETCO, QSETOP
      EXTERNAL  Q1EDIT, QGETI,  QGETR,  QGETCO, QGETOP
      EXTERNAL  Q2INIT, Q2SETR, Q2SETI, Q2EDIT, Q2GETR
      EXTERNAL  CHRCHR, ZIPI,   ZIPC,   CVARAR, CARVAR
      EXTERNAL  WDBSGR
      EXTERNAL  PRWMSE, FLODR2, STCLAS
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I2  = 2
      I64 = 64
      BLNK= ' '
      RETCOD = 0
      I = 35
      CALL ZIPI (I, I0, IVAL)
      RVAL(1) = 0.0
      BADVAL(1) = -999.
      BADVAL(2) = -999.
C
C     message file cluster used
      SCLU= 155
C     set window path name
      CALL ZWNSOP (I1,PTHNAM)
C
C     init output options
      IF (IGR.EQ.1) THEN
C       init to output graphics
        IPLOT = 1
      ELSE
C       graphics not available
        IPLOT= 0
      END IF
      ISCN = 1
C     open default output file
      CALL GETFUN (I1,FOUT)
      FLNAME = 'COMPAR.OUT'
      OPEN (UNIT=FOUT,FILE=FLNAME,STATUS='UNKNOWN')
C     init period of record and time step/units flags
      PRECFG= 1
      TSTUFG= 1
C     init to default number of intervals and set classes
      NCI = 35
      BOUND(1) = 1.0
      BOUND(2) = 10000.0
      CALL STCLAS ( NCI, BOUND,
     O              CLASS )
C
C     set default for compare option
      IF (SELFG.EQ.1) THEN
C       start with Select
        OPTC = 6
      ELSE
C       start with Output
        OPTC = 2
      END IF
 10   CONTINUE
C       do main compare menu
        IF (SELFG.EQ.1) THEN
C         optc:1-Return,2-Output,3-Define,4-Class,5-Analyze,6-Select
          SGRP= 1
        ELSE
C         optc:1-Return,2-Output,3-Define,4-Class,5-Analyze
          SGRP= 50
        END IF
        CALL QRESP (MESSFL,SCLU,SGRP,OPTC)
C
C       allow previous
        I= 4
        CALL ZSTCMA (I,I1)
C
        GO TO (600,200,300,400,500,100), OPTC
 100    CONTINUE
C         select data sets to analyze
          LPTHNM = 'SC      '
          CALL PRWMSE (MESSFL,WDMFL,DSNBMX,LPTHNM,
     M                 DSNBUF,DSNCNT)
          IF (MOD(DSNCNT,2) .NE. 0) THEN
C           must have even number of stations
            DSNCNT = DSNCNT - 1
            SGRP = 43
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
C         turn off previous command
          I= 4
          CALL ZSTCMA (I,I0)
C         reset window path name
          CALL ZWNSOP ( I1, PTHNAM )
          OPTC = 2
          GO TO 900
C
 200    CONTINUE
C         modify output options
 210      CONTINUE
C           return here on previous from failed open of output file
C           iplot: 0-No,1-LogProg,2-ArithProb,3-LogArith,4-Arith,Arith
C           iscn:  0-No,1-Yes
            NEWFUN = 0
            ILEN = 80
            CALL ZIPC (ILEN,BLNK,BUFF)
            CALL CVARAR (I64,FLNAME,I64,FLNAM1)
            CALL CHRCHR (I64,FLNAM1,BUFF)
            CNUM = 3
            CALL ZIPI (3*CNUM,I0,CVAL)
            CVAL(2,1) = IPLOT+ 1
            CVAL(3,1) = ISCN + 1
            SGRP= 2
            CALL QRESPX (MESSFL,SCLU,SGRP,I1,I1,CNUM,
     M                   IVAL,RVAL,CVAL,BUFF)
C           get user exit command value
            CALL ZGTRET (IRET)
            IF (IRET.EQ.1) THEN
C             user wants to continue
              IPLOT = CVAL(2,1)- 1
              ISCN  = CVAL(3,1) - 1
              CALL CHRCHR (I64, BUFF, NEWFL1)
              CALL CARVAR (I64, NEWFL1, I64, NEWFIL)
              INQUIRE (FILE=NEWFIL, NUMBER=NEWFUN, IOSTAT=INQERR)
              IF (INQERR .NE. 0) THEN
C               invalid top-level directory specified
                SGRP = 4
                CALL PRNTXT (MESSFL, SCLU, SGRP)
C               get user exit command value
                CALL ZGTRET (IRET)
                IF (IRET .EQ. 1) THEN
C                 user chose 'Accept'; reset output file to default
C                 close any current output file
                  IF (FOUT .NE. 0) THEN
                    CALL QFCLOS (FOUT, I0)
                    FOUT = 0
                  END IF
                  CALL GETFUN (I1, FOUT)
                  FLNAME = 'COMPAR.OUT'
                  OPEN (UNIT=FOUT, FILE=FLNAME, STATUS='UNKNOWN')
                END IF
C               prevent the following IF/THEN from being entered
                NEWFUN = FOUT
              END IF
              IF (NEWFUN .NE. FOUT) THEN
C               output to file different than current output file
C               close current output file
                CALL QFCLOS (FOUT,I0)
                FOUT = 0
C               open file for general output
                SGRP = 3
                CALL QFOPFN (MESSFL,SCLU,SGRP,NEWFL1,I0,
     O                       FOUT,RETCOD)
                IF (RETCOD.NE.0) THEN
C                 problem opening file
                  SGRP = 4
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
C                 get user exit command value
                  CALL ZGTRET (IRET)
                  IF (IRET.EQ.1) THEN
C                   user wants to continue, reset output file to default
                    CALL GETFUN (I1,FOUT)
                    FLNAME= 'COMPAR.OUT'
                    OPEN (UNIT=FOUT,FILE=FLNAME,STATUS='UNKNOWN')
                  END IF
                ELSE
C                 different file opened successfully
                  CALL CARVAR (I64, NEWFL1, I64, FLNAME)
                END IF
              END IF
            ELSE
C             user wants back to main compare menu
              IRET= 1
            END IF
          IF (IRET.EQ.2) GO TO 210
C
C         turn off previous command
          I= 4
          CALL ZSTCMA (I,I0)
          IF (IPLOT.GE.1 .AND. IGR.EQ.2) THEN
C           user wants graphics, but it is not available
            SGRP= 25
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            IPLOT= 0
          END IF
          OPTC = 3
          GO TO 900
C
 300    CONTINUE
C         get time period, step, and missing value indicator
          IF (DSNCNT .GE. 2) THEN
C           assume currently selected data sets will be used
            I = 32
            CALL WDBSGR ( WDMFL, DSNBUF(1), I, I1,
     O                    BADVAL(1), RETCOD )
            CALL WDBSGR ( WDMFL, DSNBUF(2), I, I1, 
     O                    BADVAL(2), RETCOD )
          END IF
 310      CONTINUE
            IRET2 = 1
            SGRP= 5
            CALL Q1INIT ( MESSFL, SCLU, SGRP )
            I = 2
            IVAL(1) = PRECFG
            IVAL(2) = TSTUFG
            CALL QSETCO ( I, IVAL )
            CALL QSETR ( I, BADVAL )
            CALL Q1EDIT ( IRET )
            IF (IRET.EQ.1) THEN
C             user wants to continue
              CALL QGETCO ( I, IVAL )
              PRECFG= IVAL(1)
              TSTUFG= IVAL(2)
              CALL QGETR ( I, RVAL )
              IF (ABS(RVAL(1)) .GT. .0000001  .AND.
     #            ABS(RVAL(2)) .GT. .0000001) THEN
C               no problem with 0.0 missing value
                BADVAL(1) = RVAL(1)
                BADVAL(2) = RVAL(2)
              ELSE
C               potential problem with zero tsfill, are you sure?
 320            CONTINUE
                  SGRP = 6
                  CALL Q1INIT ( MESSFL, SCLU, SGRP )
                  CALL QSETR  ( I, RVAL )
                  CALL Q1EDIT ( IRET2 )
                  IF (IRET2 .EQ. 1) THEN
C                   user wants to continue, get missing values
                    CALL QGETR ( I, BADVAL )
                  END IF
                IF (IRET2 .EQ. -1) GO TO 320
              END IF
            END IF
C           turn off previous command
            I= 4
            CALL ZSTCMA (I,I0)
          IF (IRET .EQ. -1  .OR.  IRET2 .EQ. 2) GO TO 310
          OPTC = 4
          GO TO 900
C
 400    CONTINUE
C         class intervals
 410      CONTINUE
            IRET2 = 1
            SGRP = 8
            CALL Q1INIT ( MESSFL, SCLU, SGRP )
C           set option
            IAR(1) = 1
            CALL QSETOP ( I1, I1, IAR, IAR, IAR )
            CALL QSETR  ( I2, BOUND )
            CALL QSETI  ( I1, NCI )
            CALL Q1EDIT ( IRET )
            IF (IRET .EQ. 1) THEN
C             user wants to continue, Class opt: 1-Standard,2-Specify
              CALL QGETOP ( I1, IAR )
              IF (IAR(1) .EQ. 1) THEN
C               Standard class, get lower and upper bounds, set no classes
                CALL QGETR ( I2, BOUND )
                NCI = 35
C               set up class intervals
                CALL STCLAS ( NCI, BOUND,
     O                        CLASS )
              ELSE
C               Specified class intervals, get no. of classes
                CALL QGETI ( I1, NCI )
C               set index numbers, get classes
                DO 430 I = 1, NCI
                  IVAL(I) = I
 430            CONTINUE
 450            CONTINUE
                  SGRP = 9
                  CALL Q2INIT ( MESSFL, SCLU, SGRP )
                  CALL Q2SETR  ( I1, NCI, CLASS )
                  CALL Q2SETI ( I1, NCI, IVAL )
                  CALL Q2EDIT ( NCI, IRET2 )
                  IF (IRET2 .EQ. 1) THEN
C                   user wants to continue, get classes
                    CALL Q2GETR (I1, NCI, CLASS )
                  END IF
                IF (IRET2 .EQ. -1) GO TO 450
              END IF
            END IF
          IF (IRET .EQ. -1  .OR.  IRET2 .EQ. 2) GO TO 410
C         turn off previous command
          I= 4
          CALL ZSTCMA (I,I0)
          OPTC = 5
          GO TO 900
C
 500    CONTINUE
C         do analysis
          IF (DSNCNT .LE. 0) THEN
C           nothing to analyze
            SGRP= 19
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          ELSE IF (MOD(DSNCNT,2) .NE. 0) THEN
C           must have even number of stations
            DSNCNT = DSNCNT - 1
            SGRP = 43
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          ELSE
C           data sets to analyze
            CALL FLODR2 (MESSFL,SCLU,WDMFL,DSNCNT,DSNBUF,IPLOT,ISCN,
     I                   NCI,CLASS,PRECFG,TSTUFG,FOUT,PTHNAM,SELFG,
     I                   BADVAL)
          END IF
C         turn off previous command
          I= 4
          CALL ZSTCMA (I,I0)
          OPTC = 2
          GO TO 900
C
 600    CONTINUE
C         all done
          GO TO 900
C
 900    CONTINUE
C
C       turn off previous
        I= 4
        CALL ZSTCMA (I,I0)
C
      IF (OPTC .NE. 1) GO TO 10
C
C     close files
      DELFG = 0
      CALL QFCLOS (FOUT,DELFG)
C
      RETURN
      END
C
C
C
      SUBROUTINE   FLODR2 
     I                    (MESSFL,SCLU,WDMFL,DSNCNT,DSN,IPLOT,ISCN,
     I                     NCI,CLASS,PRECFG,TSTUFG,FOUT,PTHNAM,SELFG,
     I                     BADVAL)
C
C     + + + PURPOSE + + +
C     This routine calculates flow-duration statistics and prints
C     the flow-duration table and calls routine to plot the flow-
C     duration data for use in comparing 2 time series.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,WDMFL,DSNCNT,DSN(DSNCNT),IPLOT,ISCN,
     1            NCI,PRECFG,TSTUFG,FOUT,SELFG
      REAL        CLASS(NCI), BADVAL(2)
      CHARACTER*8   PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     WDMFL  - Fortran unit number for input direct access file
C     DSNCNT - count of data sets
C     DSN    - array of data-set numbers for analysis
C     IPLOT  - plotting flag, 0- dont plot, 1- log-normal,
C              2- arith-normal, 3- log-arith, 4- arith-log
C     ISCN   - results to screen flag, 0-don't, 1-do
C     NCI    - number of class intervals
C     CLASS  - array of class interval values
C     PRECFG - indicates how to determine period of record
C              1 - use full period of record
C              2 - use common period of record
C              3 - user will specify period for each data set
C     TSTUFG - indicates how to determine time step/units
C              1 - use data set values
C              2 - allow user to specify time step/units
C     FOUT   - Fortran unit number of output file
C     PTHNAM - path chosen to get here
C     SELFG  - select data sets flag, if 1 user can select data sets
C              and values are assumed to be simulated/observed
C     BADVAL - array containing value recognized as missing for
C              each of the time series
C
C     + + + PARAMETERS + + +
      INTEGER   BUFMAX
      PARAMETER (BUFMAX = 3660)
C
C     + + + LOCAL VARIABLES   + + +
      INTEGER      I,J,K,N,I0,I1,I6,I8,I12,I20,I80,NUMA(35),QFLG,
     $             ERRFLG,SGRP,GRPFLG,SALEN,SAIND,ICHK(7),NJ,
     $             TEMP(12),RESP,TNUM,CNUMA,DTRAN,IRET,RETCOD,
     $             INUM,CNUM,NPTS,IVAL(13),CVAL(2,3),SCLUX,
     $             TSTEP,TUNITS,SDATIM(6),EDATIM(6),IWRT,NP,
     $             LEN,CORM,LDEVTY,LDEVCD,EMATRX(35,8),WNDFLG,
     $             ETOT(8),NUMB(35),A,B,RNUM,SCNFG,MAXL,CNUMB,
     $             ICLOS,IWAIT,WSID,I2,IXTYP,XTYP,CMPTYP,NBVAL,
     $             ZA,ZB,ZAB,ZANB,ZBNA
      REAL         SDIF(35),SDIF2(35),BIAS(35),SUMA(35),SUMB(35),
     $             PCTA(35),PCTB(35),CPCTA(35),CPCTB(35),PDIF(35),
     $             PDIF2(35),PBIAS(35),STEST,
     $             TSDIF,TSDIF2,TSUMA,TSUMB,TPCTA,TPCTB,TPBIAS,TBIAS,
     $             DIF,PCUMA,PCUMB,PSTERR,PABERR,BI,X,TPDIF,ABERR,
     $             STERR,PBI,ERRINT(7),TPDIF2,ZERO,RVAL(8),YX(BUFMAX) 
      CHARACTER*1  TBUFF(80),LBC(20,2),
     $             CTITL(12,2),LTITLE(80,2),BLNK,CXLAB(80) 
      CHARACTER*8  WNDNAM(2)
      CHARACTER*20 LBC20
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + INTRINSICS + + +
      INTRINSIC    REAL, ABS, INT, SQRT
C
C     + + + EXTERNALS + + +
      EXTERNAL     TIMDIF, TIMADD, PMXCNW, ZIPI, MKTITL, CVARAR
      EXTERNAL     CKDATE, CHRCHR, CTRSTR, QRESPM, ZWNSOP
      EXTERNAL     QRESP, GRFCPR, WDBSGI, ZMNSST, ZLNTXT
      EXTERNAL     COPYI, WDTGET, PRNTXI, ZSTCMA, ZGTRET, WTDATE
      EXTERNAL     PMXTXI, PRNTXT, ZIPC, PROPLT, GGATXB, GPLBXB
      EXTERNAL     GRFDIT, ANPRGT, PDNPLT, PLTONE, PSTUPW
      EXTERNAL     QSETR, QSETI, Q1EDIT, Q1INIT, DSINFO, GETTXT
C
C     + + + DATA INITIALIZATIONS   + + +
      DATA CTITL/'S','i','m','u','l','a','t','e','d',' ','-',' ',
     1           'O','b','s','e','r','v','e','d',' ',' ','-',' '/
      DATA ICHK/1,2,1,1,1,1,1/
      DATA ERRINT/-60.0,-30.0,-10.0,0.0,10.0,30.0,60.0/
C
C     + + + OUTPUT FORMATS   + + +
 2000 FORMAT ('1')
 2001 FORMAT (80A1)
 2003 FORMAT (1X,F9.2,I9,1X,3(F10.3,F10.1))
 2033 FORMAT (1X,F9.2,I9,1X,3(F10.3,8X,'* '))
C2004 FORMAT (1X,F9.2,I10,F10.2,I10,F10.2)
 2005 FORMAT (10X,I9,1X,3(F10.3,F10.1))
 2055 FORMAT (10X,I9,1X,3(F10.3,8X,'* '))
 2007 FORMAT (1X,F9.2,I5,I5,4F10.2,2F10.2)
 2008 FORMAT (9X,I6,I6,F9.2,F10.2,20X,2F10.2//)
 2010 FORMAT (1X,I5,1X,73A1)
 2011 FORMAT (' ')
 2012 FORMAT (1X,F9.2,I7,7I8)
 2013 FORMAT (
     # ' ---------  --------------------------------------------',
     # '----------------'/9X,8I8//)
C2004 FORMAT (1X,F9.2,I10,F10.2,I10,F10.2)
C2005 FORMAT (I8)
C2007 FORMAT (/I7,' values were tagged missing and excluded from',
C    &            ' analysis.')
C2020 FORMAT (1X,F8.1,5(2X,F9.1))
C2021 FORMAT (1X,F8.1,2X,F9.0,2(2X,F9.1,2X,F9.0))
 2061 FORMAT ('                           Mean               Root',
     #        ' mean'/
     #        '   Lower    Number    absolute error(1)     square',
     #        ' error(2)        Bias(3)      '/
     #        '   class      of     ------------------- ---------',
     #        '---------- -------------------'/
     #        '   limit     cases   Average    Percent   Average ',
     #        '   Percent  Average   Percent '/
     #        ' --------- --------- --------- --------- ---------',
     #        ' --------- --------- ---------')
 2062 FORMAT (' --------- --------- --------- --------- ---------',
     #        ' --------- --------- ---------')
 2063 FORMAT (/' Standard error of estimate = ',F10.2/
     # 10X,'= square root((n/n-1)*((tot.col.5)**2-(tot.col.7)**2))'//
     # ' (1) Average = sum(|S-O|/n)'/
     # '     Percent = 100 * (sum(|S-O|/O))/n  for all O > 0'/
     # ' (2) Average = square root(sum((S-O)**2)/n)'/
     # '     Percent = 100 * square root(sum(((S-O)/O)**2)/n) for all',
     # ' O > 0'/
     # ' (3) Average = sum (S-O)/n'/
     # '     Percent = 100 * sum (((S-O)/O)/n)  for all O > 0'/)
 2064 FORMAT (/' Number of pairs with a bad (missing) value not ',
     #         'used in analysis =', I8 )
 2073 FORMAT (
     # '         Cases equal or exceeding lower'/
     # '          limit & less then upper limit     Percen',
     # 't cases'/
     # '           ----------------------------       equa',
     # 'l or        Average of cases'/
     # '   Lower     Cases         Percent         exceedi',
     # 'ng limit   within class limits'/
     # '   class   --------- ------------------- ---------',
     # '---------- -------------------'/
     # '   limit   Sim   Obs Simulated  Observed Simulated',
     # '  Observed Simulated  Observed'/
     # ' --------- ---- ---- --------- --------- ---------',
     # ' --------- --------- ---------'/)
 2074 FORMAT (
     # '         Cases equal or exceeding lower'/
     # '          limit & less then upper limit     Percen',
     # 't cases'/
     # '           ----------------------------       equa',
     # 'l or        Average of cases'/
     # '   Lower     Cases         Percent         exceedi',
     # 'ng limit   within class limits'/
     # '   class   --------- ------------------- ---------',
     # '---------- -------------------'/
     # '   limit   Dsn1 Dsn2  DataSet1  DataSet2  DataSet1',
     # '  DataSet2  DataSet1  DataSet2'/
     # ' --------- ---- ---- --------- --------- ---------',
     # ' --------- --------- ---------'/)
 2075 FORMAT (' --------- ---- ---- --------- --------- ----------',
     # ' --------- --------- ---------')
 2076 FORMAT (/
     #  1X,I8,' Observed values are zero'/
     #  1X,I8,' Simulated values are zero'/
     #  1X,I8,' Observed values are zero when simulated are zero'/
     #  1X,I8,' Observed values are zero when simulated are not'/
     #  1X,I8,' Observed values are not zero when simulated are'/)
 2077 FORMAT (/
     #  1X,I8,' DataSet2 values are zero'/
     #  1X,I8,' DataSet1 values are zero'/
     #  1X,I8,' DataSet2 values are zero when DataSet1 values are zero'/
     #  1X,I8,' DataSet2 values are zero when DataSet1 values are not'/
     #  1X,I8,' DataSet2 values are not zero when DataSet1 values are'/)
 2081 FORMAT (/
     # '   Lower         Number of occurrences between indicated',
     # ' deviations    '/
     # '   class    --------------------------------------------',
     # '-----------------'/
     # '   limit          -60%    -30%    -10%      0%     10% ',     
     # '    30%     60%'/
     # ' ---------  --------------------------------------------',
     # '-----------------')
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I2  = 2
      I6  = 6
      I8  = 8
      I12 = 12
      I20 = 20
      I80 = 80
C     
      BLNK= ' '
      DTRAN = 1
      ERRFLG= 0
      RETCOD = 0
      MAXL = 10
      SCNFG = 1
      IWRT = 0
C     build window names
      WNDNAM(1)= 'Modify  '
      WNDNAM(2)= PTHNAM(1)(1:ZLNTXT(PTHNAM(1))) // 'CAM     '
C
C     get computer type
      I= 1
      CALL ANPRGT (I,CMPTYP)
C
      IF (PRECFG.EQ.2) THEN
C       need to get common time period for analysis
        CORM = 1
        CALL WTDATE (WDMFL,DSNCNT,DSN,CORM,
     O               SDATIM,EDATIM,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         common period found
 100      CONTINUE
C           back here on bad dates
            ERRFLG= 0
C           allow previous
            I= 4
            CALL ZSTCMA (I,I1)
            CALL COPYI (I6,SDATIM,IVAL)
            CALL COPYI (I6,EDATIM,IVAL(7))
            INUM= 12
            SGRP= 10
            CALL QRESPM (MESSFL,SCLU,SGRP,INUM,I1,I1,
     O                   IVAL,RVAL,CVAL,TBUFF)
C           get user exit command value
            CALL ZGTRET (IRET)
            IF (IRET.EQ.1) THEN
C             continue
              CALL COPYI (I6,IVAL,SDATIM)
              CALL COPYI (I6,IVAL(7),EDATIM)
C             check order of dates
              CALL CKDATE (SDATIM,EDATIM,
     O                     ERRFLG)
              IF (ERRFLG.GT.0) THEN
C               dates out of range
                IVAL(2)= IVAL(7)
                IVAL(3)= SDATIM(1)
                IVAL(4)= EDATIM(1)
                INUM= 4
                SGRP= 11
                CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,I0,I,IVAL)
              ELSE
C               dates are ok
                ERRFLG= 0
C               save dates in temporary variable
                CALL COPYI (I6,SDATIM,TEMP)
                CALL COPYI (I6,EDATIM,TEMP(7))
              END IF
            ELSE
C             user doesnt want to analyze now
              ERRFLG= -1
            END IF
          IF (ERRFLG.GT.0) GO TO 100
C         turn off previous
          I= 4
          CALL ZSTCMA (I,I0)
        ELSE
C         no common period
          SGRP= 12
          CALL PRNTXT (MESSFL,SCLU,SGRP)
          CALL ZGTRET (IRET)
          IF (IRET.EQ.1) THEN
C           set to use full period
            PRECFG= 1
          ELSE
C           dont do analysis
            ERRFLG= -1
          END IF
        END IF
      END IF
C
      IF (ERRFLG.EQ.0) THEN
C       all ok so far
C
        IF (IPLOT .GE. 1) THEN
C         initialize common block for graphics; set device type to screen
          LDEVTY = 1
          CALL GRFDIT (LDEVTY)
C         get device code
          I = 40
          CALL ANPRGT (I, LDEVCD)
        END IF
C
        DO 500 N= 1,DSNCNT,2
C         do analysis for each pair of data sets
          IF (RETCOD.LT.0) THEN
C           had problems on last data set, but ok to try the next one
            RETCOD= 0
          END IF
          IF (RETCOD.EQ.0) THEN
C           always continue unless user 'Interrupts' analysis
            IF (SELFG.NE.1) THEN
C             make default titles from scen/loc/constit attribs if poss
              CALL MKTITL (WDMFL,DSN(N),DSN(N+1),
     O                     LTITLE)
              CALL CHRCHR (I20,LTITLE(1,1),LBC(1,1))
              CALL CHRCHR (I20,LTITLE(1,2),LBC(1,2))
            ELSE
C             use default sim/observed as title
              DO 90 J = 1,2
C               set default title
                CALL ZIPC (I80,BLNK,LTITLE(1,J))
                CALL CHRCHR (I12,CTITL(1,J),LTITLE(1,J))
C               retrieve data for each pair
                NJ = N - 1 + J
C               add info about data set
                LEN = 80 - 13
                CALL DSINFO (WDMFL,DSN(NJ),LEN,LTITLE(13,J))
 90           CONTINUE
              LBC20 = 'Simulated           '
              CALL CVARAR (I20,LBC20,I20,LBC(1,1))
              LBC20 = 'Observed            '
              CALL CVARAR (I20,LBC20,I20,LBC(1,2))
            END IF
C
            ZA = 0
            ZB = 0
            ZAB = 0
            ZANB = 0
            ZBNA = 0
            NBVAL = 0
C
            DO 136 I = 1,NCI
              NUMA(I) = 0
              NUMB(I) = 0
              BIAS(I) = 0.0
              SUMA(I) = 0.0
              SUMB(I) = 0.0
              SDIF(I) = 0.0
              SDIF2(I) = 0.0
              PDIF(I) = 0.0
              PDIF2(I) = 0.0
              PBIAS(I) = 0.0
              DO 135 K = 1,8
                EMATRX(I,K) = 0
 135          CONTINUE
 136        CONTINUE
C
            IF (PRECFG.NE.2) THEN
C             get start/end dates from data set
C             CALL WTFNDT (WDMFL,DSN(N),I1,
C    O                     J,SDATIM,EDATIM,RETCOD)
C             need to get common time period for analysis
              CORM = 1
              CALL WTDATE (WDMFL,I2,DSN(N),CORM,
     O                     SDATIM,EDATIM,RETCOD)
            ELSE
C             using common period from above, put saved dates back in place
              CALL COPYI (I6,TEMP,SDATIM)
              CALL COPYI (I6,TEMP(7),EDATIM)
            END IF
            GRPFLG= 0
            IF (PRECFG.EQ.3) THEN
C             user wants to specify dates
              GRPFLG= 1
              INUM= 12
              CALL COPYI (I6,SDATIM,IVAL)
              CALL COPYI (I6,EDATIM,IVAL(7))
            END IF
C           get time step/units on data set
            SALEN= 1
            SAIND= 33
            CALL WDBSGI (WDMFL,DSN(N),SAIND,SALEN,
     O                   TSTEP,RETCOD)
            IF (RETCOD.EQ.0) THEN
C             time step found, now get units
              SAIND= 17
              CALL WDBSGI (WDMFL,DSN(N),SAIND,SALEN,
     O                     TUNITS,RETCOD)
            END IF
            IF (TSTUFG.EQ.2 .OR. RETCOD.NE.0) THEN
C             user wants (or needs) to specify time step/units
              GRPFLG= GRPFLG+ 2
              IF (GRPFLG.EQ.2) THEN
C               only need time step/units and transformation type
                INUM= 1
              ELSE
C               doing dates too
                INUM= 13
              END IF
              IVAL(INUM)= TSTEP
              CNUM= 2
              CVAL(1,1)= TUNITS
              CVAL(2,1)= DTRAN + 1
            END IF
            IF (GRPFLG.GT.0) THEN
C             user needs to specify something
 140          CONTINUE
C               back here on bad date specification
                ERRFLG= 0
C               show which data set
                SGRP= 13
                CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,-I1,I1,DSN(N))
C               save text
                CALL ZMNSST
C               allow interrupt
                I= 16
                CALL ZSTCMA (I,I1)
                SGRP= 13+ GRPFLG
                CALL QRESPM (MESSFL,SCLU,SGRP,INUM,I1,CNUM,
     M                       IVAL,RVAL,CVAL,TBUFF)
C               get user exit command value
                CALL ZGTRET (IRET)
                IF (IRET.EQ.1) THEN
C                 user wants to continue
                  IF (GRPFLG.EQ.2) THEN
                    TSTEP = IVAL(1)
                    TUNITS= CVAL(1,1)
                    DTRAN = CVAL(2,1) - 1
                  ELSE
                    CALL COPYI (I6,IVAL,SDATIM)
                    CALL COPYI (I6,IVAL(7),EDATIM)
C                   check order of dates
                    CALL CKDATE (SDATIM,EDATIM,
     O                           ERRFLG)
                    IF (ERRFLG.GT.0) THEN
C                     invalid dates
                      SGRP= 17
                      CALL PRNTXT (MESSFL,SCLU,SGRP)
                    END IF
                    IF (GRPFLG.EQ.3) THEN
                      TSTEP = IVAL(13)
                      TUNITS= CVAL(1,1)
                      DTRAN = CVAL(2,1) - 1
                    END IF
                  END IF
                ELSE
C                 user wants out of analysis
                  RETCOD= 1
                END IF
              IF (ERRFLG.GT.0) GO TO 140
C             turn off interrupt
              I= 16
              CALL ZSTCMA (I,I0)
            END IF
C
            IF (RETCOD.EQ.0) THEN
C             ok so far
              CALL COPYI (I6,EDATIM,TEMP(7))
 142          CONTINUE
C               determine number of values
                CALL COPYI (I6,SDATIM,TEMP)
                CALL TIMDIF (SDATIM,TEMP(7),TUNITS,TSTEP,NPTS)
                IF (NPTS .GT. BUFMAX/2) THEN
C                 can only get BUFMAX at a time
                  NPTS = BUFMAX/2
                END IF
C
C               file being read
                SGRP = 23
                CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,J)
C               begin to fill plot array
                QFLG = 30
                CALL WDTGET (WDMFL,DSN(N),TSTEP,SDATIM,
     I                       NPTS,DTRAN,QFLG,TUNITS,
     O                       YX(1),RETCOD)
                CALL WDTGET (WDMFL,DSN(N+1),TSTEP,SDATIM,
     I                       NPTS,DTRAN,QFLG,TUNITS,
     O                       YX(NPTS+1),RETCOD)
                IF (RETCOD .NE. 0) THEN
C                 error reading file, error code &
                  SGRP = 24
                  CALL PRNTXI (MESSFL,SCLU,SGRP,RETCOD)
                ELSE
C                 begin loop to fill class intervals.
                  ZERO= 1.0E-9
                  DO 150 NP = 1,NPTS
C                   A is simulated,   B is observed
                    A = NP
                    B = NP + NPTS            
                    IF (ABS(YX(A)-BADVAL(1)) .GT. ZERO  .AND.
     $                  ABS(YX(B)-BADVAL(2)) .GT. ZERO) THEN
C                     both values are assumed valid
                      I = NCI + 1
 144                  CONTINUE
                        I = I - 1
                      IF (YX(A).LT.CLASS(I) .AND. I.GT.1)   GO TO 144
                      NUMA(I) = NUMA(I) + 1
                      SUMA(I) = SUMA(I) + YX(A)
C
                      J = NCI + 1
 146                  CONTINUE
                        J = J - 1
                      IF (YX(B).LT.CLASS(J) .AND. J.GT.1)   GO TO 146
                      SUMB(J) = SUMB(J) + YX(B)
                      NUMB(J) = NUMB(J) + 1
C
                      DIF = YX(A) - YX(B)
                      SDIF(J) = SDIF(J) + ABS(DIF)
                      SDIF2(J) = SDIF2(J) + (DIF)**2
                      IF (ABS(YX(B)) .GT. ZERO) THEN
                        PDIF(J) = PDIF(J) + ABS(DIF)/YX(B)
                        PDIF2(J) = PDIF2(J) + (DIF/YX(B))**2
                        PBIAS(J) = PBIAS(J) + DIF/YX(B)
                      END IF
                      BIAS(J) = BIAS(J) + DIF
C
C                     do zero event counter
                      IF (ABS(YX(A)) .LT. ZERO) THEN
                        ZA = ZA + 1
                        IF (ABS(YX(B)) .LT. ZERO) THEN
                          ZB  = ZB + 1
                          ZAB = ZAB + 1
                        ELSE
                          ZANB = ZANB + 1
                        END IF
                      ELSE IF (ABS(YX(B)) .LT. ZERO) THEN
                        ZB   = ZB + 1
                        ZBNA = ZBNA + 1
                      END IF
C
C                     compute error matrix
                      K = 0
                      X = YX(B)
                      IF (X.LE.0.0) X = CLASS(J)
                      IF (X.LE.0.0) THEN
                        X = 0.0
                      ELSE
                        X = 100.0*DIF/X
                      END IF
 147                  CONTINUE
                        K = K + 1
                      IF(K .LT. 7 .AND. X .GT. ERRINT(K)) GO TO 147
                      EMATRX(J,K) = EMATRX(J,K) + 1
                    ELSE
C                     bad/missing value on simulated or(and) observed
                      NBVAL = NBVAL + 1
                    END IF
 150              CONTINUE
C
C                 check adjusted start date with end date
                  CALL TIMADD (TEMP,TUNITS,TSTEP,NPTS,
     O                         SDATIM)
                  CALL COPYI (I6,TEMP(7),EDATIM(1))
                  CALL CKDATE (SDATIM,EDATIM,ERRFLG)
C                 -1 =sdatim<edatim,  0=they are equal,  1=sdatim>edatim
C                 go back to get more data.
                END IF
              IF (ERRFLG.LT.0 .AND. RETCOD.EQ.0) GO TO 142
            END IF
C
C           table includes error analysis
            TBIAS = 0.0
            TNUM = 0
            TSDIF = 0.0
            TSDIF2 = 0.0
            TPCTA = 0.0
            TPCTB = 0.0
            TSUMA = 0.0
            TSUMB = 0.0
            TPDIF = 0.0
            TPDIF2 = 0.0
            TPBIAS = 0.0
C
            DO 174 I = 1,NCI
              TNUM = TNUM + NUMB(I)
              TSUMA = TSUMA + SUMA(I)
              TSUMB = TSUMB + SUMB(I)
              TSDIF = TSDIF + SDIF(I)
              TSDIF2 = TSDIF2 + SDIF2(I)
              TBIAS = TBIAS + BIAS(I)
              TPDIF = TPDIF + PDIF(I)
              TPDIF2 = TPDIF2 + PDIF2(I)
              TPBIAS = TPBIAS + PBIAS(I)
 174        CONTINUE
C
C           write out heading
            CALL CTRSTR (I80,LTITLE(1,1))
            CALL CTRSTR (I80,LTITLE(1,2))
            WRITE(FOUT,2000)
            WRITE (FOUT,2001) (LTITLE(I,1),I=1,80)
            WRITE (FOUT,2001) (LTITLE(I,2),I=1,80)
            WRITE (FOUT,2011)
            WRITE (FOUT,2061)
C
            PCTA(1) = 0.0
            PCTB(1) = 0.0
            DO 184 I = 1,NCI
              PCTA(I) = 100.0*REAL(NUMA(I))/REAL(TNUM)
              PCTB(I) = 100.0*REAL(NUMB(I))/REAL(TNUM)
              TPCTA = TPCTA + PCTA(I)
              TPCTB = TPCTB + PCTB(I)
              IF (NUMB(I).GE.1) THEN
                ABERR = SDIF(I)/REAL(NUMB(I))
                STERR = SQRT(SDIF2(I)/REAL(NUMB(I)))
                BI = BIAS(I)/REAL(NUMB(I))
                PABERR = 100.0*PDIF(I)/ REAL(NUMB(I))
                PSTERR = 100.0*SQRT(PDIF2(I)/ REAL(NUMB(I)))
                PBI = 100.0*PBIAS(I)/ REAL(NUMB(I))
              ELSE
                ABERR = 0.0
                STERR = 0.0
                BI = 0.0
                PABERR = 0.0
                PSTERR = 0.0
                PBI = 0.0
              END IF
C
              IF (I .EQ. 1  .AND.  ZB .GT. 0) THEN
                WRITE(FOUT,2033) CLASS(I),NUMB(I),ABERR,STERR,BI
              ELSE
                WRITE(FOUT,2003) CLASS(I),NUMB(I),ABERR,
     #                           PABERR,STERR,PSTERR,BI,PBI
              END IF
 184        CONTINUE
C
            WRITE(FOUT,2062)      
            TSDIF = TSDIF/REAL(TNUM)
            TSDIF2 = SQRT (TSDIF2/REAL(TNUM))
Ckmf        corrected missplaced sqrt in tpdif and tpdif2, Mar 23, 98
Ckmf        TPDIF = 100.0*SQRT(TPDIF/REAL(TNUM))
Ckmf        TPDIF2 = 100.0*TPDIF2/REAL(TNUM)
            TPDIF = 100.0*TPDIF/REAL(TNUM)
            TPDIF2 = 100.0*SQRT(TPDIF2/REAL(TNUM))
            TPBIAS = 100.0*TPBIAS/REAL(TNUM)
            TBIAS = TBIAS/REAL(TNUM)
            IF (ZB .LT. 1) THEN
              WRITE(FOUT,2005) TNUM,TSDIF,TPDIF,TSDIF2,TPDIF2,
     #                         TBIAS,TPBIAS
            ELSE
              WRITE(FOUT,2055) TNUM,TSDIF,TSDIF2,TBIAS
            END IF
C
            IF (NBVAL .GT. 0) THEN
C             print out number of bad/missing values
              WRITE (FOUT,2064) NBVAL
              WRITE (FOUT,2011)
            END IF
C
C           standard error of estimate
            STEST = SQRT((REAL(TNUM)/REAL(TNUM-1))*(TSDIF2**2 
     #              - TBIAS**2))
            WRITE(FOUT,2063) STEST             
            WRITE(FOUT,2011)       
C       
C           print to screen
            RVAL(1) = TNUM
            RVAL(2) = TSDIF
            RVAL(3) = TPDIF
            RVAL(4) = TSDIF2
            RVAL(5) = TPDIF2
            RVAL(6) = TBIAS
            RVAL(7) = TPBIAS
            RVAL(8) = STEST
            RNUM = 8
            SGRP = 40
            IF (ISCN .EQ. 1) THEN  
              CALL Q1INIT (MESSFL, SCLU, SGRP)
              CALL QSETR (RNUM,RVAL)
              CALL Q1EDIT (IRET)
            END IF
C
C           write 2nd table
C           write heading
            WRITE(FOUT,2000)
            WRITE (FOUT,2001) (LTITLE(I,1),I=1,80)
            WRITE (FOUT,2001) (LTITLE(I,2),I=1,80)
            WRITE (FOUT,2011)
C           WRITE (FOUT,2072) (LBV(I,1),I=1,20), (LBV(I,2),I=1,20)
            IF (SELFG.EQ.1) THEN
C             use sim/obs wording
              WRITE (FOUT,2073)
            ELSE
C             use data set number wording
              WRITE (FOUT,2074)
            END IF
C           write contents of table
            CNUMA = TNUM
            CNUMB = TNUM
            CPCTA(1) = 100.0
            CPCTB(1) = 100.0
            DO 195 I = 1,NCI
              IF (NUMA(I).GT.0) SUMA(I) = SUMA(I)/REAL(NUMA(I))
              IF (NUMB(I).GT.0) SUMB(I) = SUMB(I)/REAL(NUMB(I))
              IF(I.GT.1) CNUMA = CNUMA - NUMA(I-1)
              IF(I.GT.1) CNUMB = CNUMB - NUMB(I-1)
              PCUMA = 100.0*REAL(CNUMA)/REAL(TNUM)
              PCUMB = 100.0*REAL(CNUMB)/REAL(TNUM)
              CPCTA(I) = PCUMA
              CPCTB(I) = PCUMB
              WRITE(FOUT,2007) CLASS(I),NUMA(I),NUMB(I),PCTA(I),
     #                   PCTB(I),CPCTA(I),CPCTB(I),SUMA(I),SUMB(I)
 195        CONTINUE
            WRITE (FOUT,2075)         
            TSUMA = TSUMA/REAL(TNUM)
            TSUMB = TSUMB/REAL(TNUM)
            WRITE(FOUT,2008) TNUM,TNUM,TPCTA,TPCTB,TSUMA,TSUMB
C
            IF ((ZA+ZB+ZAB+ZANB+ZBNA) .GT. 0) THEN
              WRITE (FOUT,2010)
C             _____ MEASURED ARE ZERO
C             _____ SIMULATED ARE ZERO
C             _____ MEASURED ARE ZERO WHEN SIMULATED ARE ZERO
C             _____ MEASURED ARE ZERO WHEN SIMULATED ARE NOT ZERO
C             _____ MEASURED ARE NOT ZERO WHEN SIMULATED ARE ZERO
              IVAL(1) = ZB
              IVAL(2) = ZA
              IVAL(3) = ZAB
              IVAL(4) = ZBNA
              IVAL(5) = ZANB
              IF (SELFG.EQ.1) THEN
C               use sim/observed wording
                WRITE (FOUT,2076) (IVAL(I),I=1,5)
                SGRP = 41
              ELSE
C               use data set number wording
                WRITE (FOUT,2077) (IVAL(I),I=1,5)
                SGRP = 46
              END IF
              INUM = 5
              IF (ISCN .EQ. 1) THEN
                CALL PMXTXI (MESSFL,SCLU,SGRP,MAXL,SCNFG,IWRT,
     $                       INUM,IVAL)
              END IF
            END IF
C
C           write bias table
C           write heading
            WRITE(FOUT,2000)
            WRITE (FOUT,2001) (LTITLE(I,1),I=1,80)
            WRITE (FOUT,2001) (LTITLE(I,2),I=1,80)
            WRITE (FOUT,2081)
C           write contents of table
            CALL ZIPI ( I8, I0, ETOT )
            DO 215 I = 1,NCI
              WRITE(FOUT,2012) CLASS(I),(EMATRX(I,K),K=1,8)
              DO 214 K = 1,8
                ETOT(K) = ETOT(K) + EMATRX(I,K)
 214          CONTINUE
 215        CONTINUE
            WRITE(FOUT,2013) (ETOT(K),K=1,8)
C           write totals to screen
            SGRP = 42
            IF (ISCN .EQ. 1) THEN
              CALL Q1INIT (MESSFL, SCLU, SGRP)
              CALL QSETI (I8,ETOT)
              CALL Q1EDIT (IRET)
            END IF
C
            IF (IPLOT.GE.1) THEN
C             do plotting, set defaults
              CALL GRFCPR (MESSFL,SCLU,CLASS,CPCTA,CPCTB,
     I                     LTITLE,NCI,IPLOT,LBC)
              WSID = 1 
 400          CONTINUE
C               do plotting menu
                CALL ZWNSOP (I1,PTHNAM)
                SGRP= 18
                CALL QRESP (MESSFL,SCLU,SGRP,RESP)
                IF (RESP.EQ.1) THEN
C                 modify stuff
                  CALL GGATXB (IXTYP)
                  CALL PROPLT (MESSFL,ICHK,WNDNAM,WNDFLG)
                  CALL GGATXB (XTYP)
                  IF (IXTYP .NE. XTYP) THEN
C                   user changed x axis type so change label
                    LEN = 80
                    SCLUX = 153
                    SGRP = 40 + XTYP
                    CALL GETTXT (MESSFL,SCLUX,SGRP,LEN,CXLAB)
                    CALL GPLBXB (CXLAB)
                  END IF
                  IF (WNDFLG .EQ. 1) THEN
C                   user changed device
                    IF (CMPTYP .NE. 1) THEN
C                     not pc, so close old workstation
                      IWAIT = 0
                      ICLOS = 1
                      CALL PDNPLT (WSID,ICLOS,IWAIT)
                    END IF
                  END IF
                ELSE IF (RESP.EQ.2) THEN
C                 generate the plot
                  IWAIT = 0
                  CALL PSTUPW (WSID, RETCOD)
                  CALL PLTONE
                  IF (CMPTYP .EQ. 1) THEN
C                   pc, always close workstation after plot
                    ICLOS = 1
                  ELSE
C                   for other types, just deactivate workstation
                    ICLOS = 0
                  END IF
                  CALL PDNPLT (WSID,ICLOS,IWAIT)
                END IF
              IF (RESP.NE.3) GO TO 400
            END IF
          END IF
C
          IF (ISCN .EQ. 0) THEN
C           no results to screen so show progress
            IF (N+1 .LT. DSNCNT) THEN
C             not last
              IWRT = 1
              SGRP = 44
              CALL PMXTXI (MESSFL,SCLU,SGRP,MAXL,SCNFG,IWRT,
     $                     I2,DSN(N))
            ELSE
              IWRT = 0
              SGRP = 45
              CALL PRNTXT (MESSFL,SCLU,SGRP)
            END IF
          END IF
C
 500    CONTINUE
        IF (CMPTYP .NE. 1) THEN
C         not pc, need to close workstation
          ICLOS = 1
          IWAIT = 0
          CALL PDNPLT (WSID,ICLOS,IWAIT)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GRFCPR
     I                    (MESSFL,SCLU,CLASS,CPCTA,CPCTB,
     I                     LTITLE,NCI,IPLOT,LBC)
C
C     + + + PURPOSE + + +
C     This routine fills the common block to plot a flow duration
C     curve using the HASS GKS routines.
C
C     + + + DUMMY VARIABLES + + +
      INTEGER     MESSFL,SCLU,NCI,IPLOT
      REAL        CLASS(35),CPCTA(35),CPCTB(35)
      CHARACTER*1 LTITLE(80,2),LBC(20,2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number on message file
C     CLASS  - top of class interval
C     CPCTA  - probabilities for first time series (measured)
C     CPCTB  - probabilities for second time series (observed)
C     LDEVTY - device type
C              1 - display monitor
C              2 - laser printer
C              3 - pen plotter
C              4 - CGM or GKS meta file
C              5 - DISSPLA meta file
C     LTITLE - title generated from data set station id or ts type
C     NCI    - number of class intervals
C     IPLOT  - plotting flag, 0- dont plot, 1- log-normal,
C              2- arith-normal, 3- log-arith, 4- arith-arith
C     LBC    - labels for each line on plot
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I1,I20,I80,SGRP,NCIM1,ILEN,NVAR,WHICH(3),VAR,
     &            BVALFG(4),RETCOD,TICS(4),YTYPE(2),XTYPE,NCRV,
     &            I240,LNTYP(2),COLOR(2),PATRN(2),SYMBL(2),TRANSF(3),
     &            I2,I3,IPOS,CTYPE(2)
      CHARACTER*1 CINVL(20),CPROBA(20),TITL(240),YLABL(80),YXLABL(80),
     &            ALAB(80), LBV(20,3), BLNK, CPROBB(20)
      REAL        Y(35), XA(35), PLMN(4), PLMX(4), YMIN(3), YMAX(3),
     &            ALEN, XB(35), YMN, YMX
C
C     + + + INTRINSICS + + +
      INTRINSIC   AMAX1, AMIN1
C
C     + + + FUNCTIONS + + +
      REAL        GAUSEX        
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    GAUSEX, CHRCHR, GETTXT, SCALIT
      EXTERNAL    GPVAR, GPLABL, GPSCLE, GPDATR, GPNCRV, ZIPC, GPCURV
      EXTERNAL    GPLBXB, GPWCXY, LENSTR, LFTSTR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CINVL/'C','l','a','s','s',' ','i','n','t','e','r','v','a',
     #           'l',' ',' ',' ',' ',' ',' '/
      DATA CPROBA/'N','o','r','m','a','l',' ','d','e','v','i','a','t',
     #            'e','s',' ','S','i','m','.'/
      DATA CPROBB/'N','o','r','m','a','l',' ','d','e','v','i','a','t',
     #            'e','s',' ','O','b','s','.'/
      DATA LNTYP,SYMBL,COLOR,PATRN/1,2,0,0,1,1,0,0/
      DATA ALAB/80*' '/,   BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
      I1  = 1
      I2  = 2
      I3  = 3
      I20 = 20
      I80 = 80
      I240 = 240
C
C     variable 2 = percents for flow A (measured)
C     variable 3 = percents for flow B (observed) 
C     variable 1 = flow class intervals
      NCRV = 2
      NVAR = 3
      CALL GPNCRV (NCRV,NVAR)
C
      ILEN = 80
      SGRP = 21
      CALL GETTXT (MESSFL,SCLU,SGRP,ILEN,YLABL)
      ILEN = 80
      SGRP = 22
      CALL GETTXT (MESSFL,SCLU,SGRP,ILEN,YXLABL)
C     set up plot title
      CALL ZIPC (I240,BLNK,TITL)
      CALL CHRCHR (I80,LTITLE(1,1),TITL(1))
      CALL LFTSTR (I80,TITL(1))
      IPOS = LENSTR(I80,TITL(1))
      IF (IPOS .GT. 0) THEN
C       add line separator symbol
        TITL(IPOS+1) = '&'
        CALL CHRCHR (I80,LTITLE(1,2),TITL(IPOS+2))
        CALL LFTSTR (I80,TITL(IPOS+2))
      ELSE
        CALL CHRCHR (I80,LTITLE(1,2),TITL(1))
        CALL LFTSTR (I80,TITL(1))
      END IF
C
      IF (IPLOT .LE. 2) THEN
        XTYPE = 6
        TRANSF(2) = 1
        TRANSF(3) = 1
      ELSE
        XTYPE = 1
        TRANSF(2) = 1
        TRANSF(3) = 1
      END IF
      IF (IPLOT.EQ. 1 .OR. IPLOT .EQ. 3) THEN
        YTYPE(1) = 2
        TRANSF(1) = 2
      ELSE
        YTYPE(1) = 1
        TRANSF(1) = 1
      END IF
      YTYPE(2) = 0
      CTYPE(1) = 6
      CTYPE(2) = 6
      ALEN = 0.0
C     set variable labels
      CALL GPLABL (XTYPE,YTYPE,ALEN,YLABL,YXLABL,ALAB,TITL)
      CALL GPLBXB (YXLABL)
C
      NCIM1 = NCI - 1
      DO 15 I = 1,NCIM1
        Y(I) = CLASS(I+1)
        IF (XTYPE .EQ. 6) THEN
          XA(I) = -GAUSEX(0.01*CPCTA(I+1))
          XB(I) = -GAUSEX(0.01*CPCTB(I+1))
        ELSE
          XA(I) = CPCTA(I+1)
          XB(I) = CPCTB(I+1)
        END IF
 15   CONTINUE
      VAR = 1
      CALL GPDATR (VAR,I1,NCIM1,Y,RETCOD)
      VAR = 2
      CALL GPDATR (VAR,NCI,NCIM1,XA,RETCOD)
      VAR = 3
      CALL GPDATR (VAR,NCI+NCIM1,NCIM1,XB,RETCOD)
C
C     1st curve has variable 1 & 2, 2nd curve has variable 1 & 3
      CALL GPWCXY (I1,I1,I2)   
      CALL GPWCXY (I2,I1,I3)
C
      WHICH(1) = 1
      WHICH(2) = 4
      WHICH(3) = 4
      YMIN(1) = CLASS(2)
      YMAX(1) = CLASS(NCI)
      YMAX(2) = XA(1)
      YMIN(2) = XA(NCIM1)
      YMAX(3) = XB(1)
      YMIN(3) = XB(NCIM1)
      CALL CHRCHR (I20,CPROBA,LBV(1,3))
      CALL CHRCHR (I20,CPROBB,LBV(1,2))
      CALL CHRCHR (I20,CINVL,LBV(1,1))
      CALL GPVAR (YMIN,YMAX,WHICH,TRANSF,LBV)
C
C     set curve type
      CALL GPCURV (CTYPE,LNTYP,SYMBL,COLOR,PATRN,LBC)
C
C     generate axis mins and maxs
      IF (XTYPE .EQ. 1) THEN
        YMN = AMIN1(YMIN(2),YMIN(3))
        YMX = AMAX1(YMAX(2),YMAX(3))
        CALL SCALIT (XTYPE,YMN,YMX,
     O               PLMN(4),PLMX(4))
      ELSE
        PLMN(4) = -3.0
        PLMX(4) = 3.0
      END IF
      CALL SCALIT (YTYPE(1),YMIN(1),YMAX(1),
     O             PLMN(1),PLMX(1))
      BVALFG(1)= 1
      BVALFG(2)= 1
      BVALFG(3)= 4
      BVALFG(4)= 4
      TICS(1) = 10
      TICS(4) = 10
      CALL GPSCLE (PLMN,PLMX,TICS,BVALFG)
C
      RETURN
      END
C
C
C
      SUBROUTINE   MKTITL
     I                    (WDMSFL,DSN1,DSN2,
     O                     LTITLE)
C
C     + + + PURPOSE + + +
C     This routine creates a title for each data set from
C     the wdm attributes for scenario,location, and constituent
C
C     + + + DUMMY VARIABLES + + +
      INTEGER     WDMSFL,DSN1,DSN2
      CHARACTER*1 LTITLE(80,2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - wdm file unit number
C     DSN1   - first data set number
C     DSN2   - second data set number
C     LTITLE - title generated from data set station id or ts type
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SAIND,SALEN,RETCOD,I8,DSN(2),I,NOATFG,I1
      CHARACTER*1  CBUFF1(8)
      CHARACTER*8  CSCEN(2),CLOCN(2),CCONS(2)
      CHARACTER*80 CTITLE(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDBSGC,CARVAR,INTCHR,CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      I8 = 8
C
      DSN(1) = DSN1
      DSN(2) = DSN2
C
      NOATFG = 0
      DO 10 I = 1,2
C       for each dsn get scen,locn,const
C
C       get attribute for scenario name
        SAIND = 288
        SALEN = 8
        CALL WDBSGC (WDMSFL,DSN(I),SAIND,SALEN,
     O               CBUFF1,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         put attrib to text array
          CALL CARVAR (I8,CBUFF1,I8,CSCEN(I))
        ELSE
          NOATFG = 1
        END IF
C
C       get next attribute for constituent name
        SAIND = 289
        SALEN = 8
        CALL WDBSGC (WDMSFL,DSN(I),SAIND,SALEN,
     O               CBUFF1,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         put attrib to text array
          CALL CARVAR (I8,CBUFF1,I8,CCONS(I))
        ELSE
          NOATFG = 1
        END IF
C
C       get next attribute for location name
        SAIND = 290
        SALEN = 8
        CALL WDBSGC (WDMSFL,DSN(I),SAIND,SALEN,
     O               CBUFF1,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         put attrib to text array
          CALL CARVAR (I8,CBUFF1,I8,CLOCN(I))
        ELSE
          NOATFG = 1
        END IF
 10   CONTINUE
C
      IF (NOATFG.EQ.1) THEN
C       these attributes were not found, used defaults
        LTITLE(1,1) = ' '
      ELSE
C       attributes found, create title
        CTITLE(1) = ' '
        CTITLE(2) = ' '
        IF (CSCEN(1).NE.CSCEN(2) .AND. CLOCN(1).NE.CLOCN(2) .AND.
     1      CCONS(1).NE.CCONS(2)) THEN
C         all three attribs differ
          CTITLE(1) = CSCEN(1) // ' ' // CCONS(1) // ' at ' // CLOCN(1)
          CTITLE(2) = CSCEN(2) // ' ' // CCONS(2) // ' at ' // CLOCN(2)
        ELSE IF (CSCEN(1).NE.CSCEN(2) .AND. CLOCN(1).NE.CLOCN(2)) THEN
C         constituents are same
          CTITLE(1) = CSCEN(1) // ' at ' // CLOCN(1)
          CTITLE(2) = CSCEN(2) // ' at ' // CLOCN(2)
        ELSE IF (CSCEN(1).NE.CSCEN(2) .AND. CCONS(1).NE.CCONS(2)) THEN
C         locations are same
          CTITLE(1) = CSCEN(1) // ' ' // CCONS(1)
          CTITLE(2) = CSCEN(2) // ' ' // CCONS(2)
        ELSE IF (CLOCN(1).NE.CLOCN(2) .AND. CCONS(1).NE.CCONS(2)) THEN
C         scenarios are same
          CTITLE(1) = CCONS(1) // ' at ' // CLOCN(1)
          CTITLE(2) = CCONS(2) // ' at ' // CLOCN(2)
        ELSE IF (CLOCN(1).NE.CLOCN(2)) THEN
C         only location differs
          CTITLE(1) = CLOCN(1)
          CTITLE(2) = CLOCN(2)
        ELSE IF (CCONS(1).NE.CCONS(2)) THEN
C         only constituent differs
          CTITLE(1) = CCONS(1)
          CTITLE(2) = CCONS(2)
        ELSE IF (CSCEN(1).NE.CSCEN(2)) THEN
C         only scenario differs
          CTITLE(1) = CSCEN(1)
          CTITLE(2) = CSCEN(2)
        ELSE
C         nothing differs, use data set numbers
          CALL INTCHR (DSN1,I8,I1,I,CBUFF1)
          CALL CARVAR (I8,CBUFF1,I8,CTITLE(1))
          CALL INTCHR (DSN2,I8,I1,I,CBUFF1)
          CALL CARVAR (I8,CBUFF1,I8,CTITLE(2))
        END IF
C       put new titles to output strings
        I = 80
        CALL CVARAR (I,CTITLE(1),I,LTITLE(1,1))
        CALL CVARAR (I,CTITLE(2),I,LTITLE(1,2))
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ASVRSN
C
C     + + + PURPOSE + + +
C     Dummy routine to include unix what version information for the
C     awstat library.
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
C
C
C
      SUBROUTINE   STCLAS
     I                    ( NCI, BOUND,
     O                      CLASS )
C
C     + + + PURPOSE + + +
C     Calculated nci-1 class intervals logrithmically distributed
C     between minimum and maximum limits found in BOUND.
C
C     NOTE:  This subroutine is currently written to calculate
C            35 classes, so NCI = 35 is required.  NCI has been
C            included in the argument list so that this routine
C            can be modified at a later date to calculate a
C            variable number of classes.  Proposed modifications
C            include (1) change 33.0 to nci-2 in calculation of cr,
C            (2) change 32 to nci-3 in do 110.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NCI
      REAL      BOUND(2), CLASS (NCI)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NCI    - number of class intervals
C     BOUND  - bounds for classes
C              (1) - top of first class, must be > 0.0
C              (2) - top of next to last class
C     CLASS  - class limits, beginning with 0.0, bound(1), ...,
C              bound(2)
C
C     + + + LOCAL VARIABLES + + + 
      INTEGER   I, N, L
      REAL      CR, CLOG, C
C
C     + + + INTRINSICS + + +
      INTRINSIC  ALOG10, INT
C         
C     + + + END SPECIFICATIONS + + +
C
C     set up class intervals
      CR = (BOUND(1)/BOUND(2))**(1.0/33.0)
      CLASS(1) = 0.0
      CLASS(2) = BOUND(1)
      CLASS(NCI)= BOUND(2)
      DO 110 N = 1,32
        I = NCI - N
        CLASS(I) = CLASS(I+1)*CR
 110  CONTINUE
C
C     round off class intervals
      DO 120 I = 2,NCI
        C = CLASS(I)
        CLOG = ALOG10(C) + 0.001
        IF (CLOG.LT.0.0) CLOG = CLOG - 1
        L = INT(CLOG)
        L = L - 1
        C = (C/(10.0**L)) + 0.5
        CLASS(I) = (INT(C))*(10.0**L)
 120  CONTINUE

      RETURN
      END
