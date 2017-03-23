C
C
C
      SUBROUTINE   SGPLOT
     I                   (MESSFL,WDMSFL,NDSN,ADSN,
     I                    CSCENM,CLOCNM,CCONNM,GRPMAX,
     M                    LNTYP,COLOR,PATTRN,SYMBOL,
     M                    TU,TS,SDATE,EDATE,DTRAN,
     M                    PLTDEV,ARHLOG)
C
C     + + + PURPOSE + + +
C     plot data at requested stations for requested time
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       WDMSFL,MESSFL,NDSN,ADSN(NDSN),
     $              LNTYP(*),COLOR(*),PATTRN(*),SYMBOL(*),
     $              GRPMAX,TU,TS,SDATE(6),EDATE(6),
     $              PLTDEV,ARHLOG(2),DTRAN
      CHARACTER*8   CSCENM(NDSN),CLOCNM(NDSN),CCONNM(NDSN)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     WDMSFL - Fortran unit number for daily data file
C     MESSFL - Fortran unit number for message file
C     NDSN   - Number of datasets to plot
C     ADSN   - Datasets to plot
C     CSCENM - Scenario name assoc with data in dataset
C     CLOCNM - Location name assoc with data in dataset
C     CCONNM - Constituent name assoc with data in dataset
C     GRPMAX - Max group size
C     LNTYP  - line type for plot
C     COLOR  - color of line on plot
C     PATTRN - fill pattern for plot
C     SYMBOL - symbol type for plot
C     TU     - Default timeunits
C     TS     - Default timestep
C     SDATE  - Default start date
C     EDATE  - Default end date
C     ARHLOG - arith or log scale
C     PLTDEV - plotting device
C     DTRAN  - time series transformation flag
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmaxd.inc'
      INCLUDE 'pbfmax.inc'
      INCLUDE 'ptsmax.inc'
      INTEGER MXSTRM
      PARAMETER (MXSTRM=50)
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxwin.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       QFLAG,WHICH(12),DTYPE(12),ISUB,CSTRM,PREVFG,
     $              I,I1,I0,I4,K,DEVCOD,SCLU,SGRP,RESP,L,TSYMBL(1),
     $              RETCOD,NPTS,IRET,IVAL2(6,MXSTRM),TLNTYP(1),
     $              CVAL(6,3,MAXD),IVAL,WINACT,LTS,LTU,I2,I7,I8,I6,
     $              CNUM,TYPIND(MAXD),LPLT,CMPTYP,DEVFIL,PLTCNT,LINCNT,
     $              MNLTZO,JTMP,MSEL(3),ILEN,PROBFG,
     $              CNTCON,CNTLOC,CNTSEN,PLTTYP,ITMP2(2),
     $              TTYP,TSYM,TLNT,TCOL,TPAT,TWHI,TDTY,LPAT(MAXD),
     $              STSTRM(6,MXSTRM),ENSTRM(6,MXSTRM),TCLU,ACT,
     $              SSDATE(2),SEDATE(2),REGLIN,STORMS,OPVAL(3),
     $              NVALS,I20,LEN1,LEN2,IPOS,STRMFG,
     $              CVAL2(1,MXSTRM),NEWDT,CLENG(2),IM1,PEKVOL(1),
     $              ISTRMM(1),INDX(BUFMAX),START(6),ENDDT(6)
      INTEGER       WIPEIT(MXWIN),MAPWIN,PLTWIN,WTYPE(MXWIN),NWIN
      REAL          WINDIM(4,MXWIN)
      REAL          R0,LL(2),RVAL,ARPCSC(1),TMPYX(BUFMAX),
     $              TRSTR1(MXSTRM),TRSTR2(MXSTRM),RSTRMP(1),
     $              ACOEF,BCOEF,RSQUAR,RVAL2(4,MXSTRM),
     $              STRMP1(MXSTRM),STRMP2(MXSTRM),AVSTV2(MXSTRM),
     $              AVSTV1(MXSTRM),RTEMP(1)
      CHARACTER*1   CLAB1(20),
     $              CLAB2(20),CVAL1(40)
      CHARACTER*7   CLINE,CCOLOR,CPATRN,CSYMBL,TMPSTR
      CHARACTER*8   PTHNAM(1),CDSID,CTRAN,CTUNIT
      CHARACTER*20  CLAB(MAXD),TCLA
      CHARACTER*80  TBUF80(MAXD),XLAB,YRLAB,YLLAB,ALAB,TPXLAB,TPYLAB
      CHARACTER*240 TITL,TPTITL
C
C     + + + EQUIVALENCES + + +
      CHARACTER*1  TBUFF(80,MAXD),TMPST1(7),TPTTL1(240)
      EQUIVALENCE (TBUF80,TBUFF),(TMPSTR,TMPST1),(TPTITL,TPTTL1)
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     QDTMPL,PRNTXI,QRESP,TIMDIF,LENSTR,ZIPR,QDBRPL,COPYR
      EXTERNAL     ZSTCMA,Q1INIT,QSETI,QSETR,QSETOP,CMSTRM,WSWLOC
      EXTERNAL     QGETR,QGETOP,ZIPI,PDNPLT,CVARAR,TSBTIM,TSBWDS,TSBGET
      EXTERNAL     Q1EDIT,QGETI,PRNTXT,ZGTRET,PLTXXX,DTPUT,STFRPL
      EXTERNAL     QRESCX,GPLEDG,QDFDPL,PMXCNW,GGTGLV,QDXYPT
      EXTERNAL     SGDATE,DTACT,DTGET,FITLIN,QDXYRG,CHRCHR,MATSTM
      EXTERNAL     Q2INIT,Q2EDIT,Q2SETI,Q2SETR,Q2STCO,Q2GTCO,DTADD
      EXTERNAL     ZMNSST,PMXTXA,QSETCO,QGETCO,SGLABL,GETWIN
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (A20,4A7)
C
C     + + + END SPECIFICATIONS + + +
C
      IM1   = -1
      I0    = 0
      I1    = 1
      I2    = 2
      I4    = 4
      I6    = 6
      I7    = 7
      I8    = 8
      I20   = 20
      R0    = 0.0
      DEVFIL= 0
      QFLAG = 31
      PLTTYP= 1
C     get window dimensions
      CALL GETWIN (MXWIN,
     O             WIPEIT,MAPWIN,PLTWIN,WTYPE,NWIN,WINDIM)
C
      CALL ZIPI (MAXD,I1,DTYPE)
      WRITE(99,*) TU,TS,GRPMAX
      WRITE(99,*) SDATE
      WRITE(99,*) EDATE
C
      ARPCSC(1)= 100.0
C     do routine to make labels for plots
      CALL SGLABL (NDSN,CSCENM,CLOCNM,CCONNM,TU,DTRAN,
     M             WHICH,TYPIND,
     O             CNTCON,CNTSEN,CNTLOC,
     O             ALAB,YRLAB,YLLAB,TITL,CLAB,CTRAN,CTUNIT)
C     set default for regression line and no storms
      REGLIN = 1
      STORMS = 2
      PEKVOL(1) = 1
      RSTRMP(1) = 99.0
      ISTRMM(1) = 50
C
      SCLU  = 71
      RESP  = 1
C
 11   CONTINUE
C       plot option loop
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        GO TO (15,20,30,40,45,50,60,70), RESP
C
 15     CONTINUE
C         plot device
          SGRP= 11
          CALL QRESP(MESSFL,SCLU,SGRP,
     M               PLTDEV)
          GO TO 90
C
 20     CONTINUE
C         time units and span
          PTHNAM(1) = 'AG'
          TCLU = 64
          CALL SGDATE (MESSFL,TCLU,PTHNAM)
C         get dates for current active date set in case they changed
          CALL DTACT (I)
          CALL DTGET (I,
     O                ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,TU,
     O                TS,DTRAN)
C         update labels for plots
          CALL SGLABL (NDSN,CSCENM,CLOCNM,CCONNM,TU,DTRAN,
     M                 WHICH,TYPIND,
     O                 CNTCON,CNTSEN,CNTLOC,
     O                 ALAB,YRLAB,YLLAB,TITL,CLAB,CTRAN,CTUNIT)
          GO TO 90
C
 30     CONTINUE
C         plot specs
          SGRP= 31
C
          DO 300 I = 1,NDSN
C           increment curve variables by 1 to account for 'none' option
            LNTYP(I)  = LNTYP(I)  + 1
            COLOR(I)  = COLOR(I) + 1
            PATTRN(I) = PATTRN(I) + 1
            SYMBOL(I) = SYMBOL(I) + 1
 300      CONTINUE
C
C         make previous available
          I= 4
          CALL ZSTCMA (I,I1)
C
          CNUM= 6
C         initialize character buffer
          CALL ZIPI (18*NDSN,I0,CVAL)
          DO 310 I= 1,NDSN
            TMPSTR= '?LINETY'
            CALL GGTGLV(I7,LNTYP(I),TMPST1,ILEN)
            CLINE = TMPSTR
            TMPSTR= '?COLOR'
            CALL GGTGLV(I7,COLOR(I),TMPST1,ILEN)
            CCOLOR= TMPSTR
            TMPSTR= '?FILL'
            CALL GGTGLV(I7,PATTRN(I),TMPST1,ILEN)
            CPATRN= TMPSTR
            TMPSTR= '?SYMBOL'
            CALL GGTGLV(I7,SYMBOL(I),TMPST1,ILEN)
            CSYMBL= TMPSTR
            WRITE(TBUF80(I),2000) CLAB(I),CLINE,CCOLOR,CPATRN,CSYMBL
            CVAL(2,1,I)= LNTYP(I)
            CVAL(3,1,I)= COLOR(I)
            CVAL(4,1,I)= PATTRN(I)
            CVAL(5,1,I)= SYMBOL(I)
            CVAL(6,1,I)= WHICH(I)
 310      CONTINUE
          CALL QRESCX (MESSFL,SCLU,SGRP,I1,I1,CNUM,NDSN,I1,
     M                 IVAL,RVAL,CVAL,TBUFF)
C         how did user exit
          CALL ZGTRET(I)
          IF (I .EQ. 1) THEN
C           with accept
            DO 320 I= 1,NDSN
              LNTYP(I) = CVAL(2,1,I)
              COLOR(I) = CVAL(3,1,I)
              PATTRN(I)= CVAL(4,1,I)
              SYMBOL(I)= CVAL(5,1,I)
              WHICH(I) = CVAL(6,1,I)
              IF (WHICH(I).EQ.3) THEN
                ALAB = CCONNM(I)
              ELSE IF (WHICH(I).EQ.2) THEN
                YRLAB = CCONNM(I)
              END IF
 320        CONTINUE
          END IF
C
          DO 330 I = 1,NDSN
C           decrement curve variables by 1 to account for 'none' option
            LNTYP(I)  = LNTYP(I)  - 1
            COLOR(I)  = COLOR(I)  - 1
            PATTRN(I) = PATTRN(I) - 1
            SYMBOL(I) = SYMBOL(I) - 1
 330      CONTINUE
C
C         make previous unavailable
          I= 4
          CALL ZSTCMA (I,I0)
          GO TO 90
C
 40     CONTINUE
C         set current window
C         do window screen
          PTHNAM(1) = 'AG'
          CALL WSWLOC (MESSFL,PTHNAM)
C         get current map window specs
          CALL GETWIN (MXWIN,
     O                 WIPEIT,MAPWIN,PLTWIN,WTYPE,NWIN,WINDIM)
          GO TO 90
C
 45     CONTINUE
C         edit axis type and scale for plot
C         make previous command available
          CALL ZSTCMA (I4,I1)
          SGRP= 61
          CALL Q1INIT(MESSFL,SCLU,SGRP)
C         percent scale
          CALL QSETR (I1,ARPCSC)
C         axis type
          MSEL(1)= 1
          MSEL(2)= 1
          OPVAL(1)= ARHLOG(1)
          OPVAL(2)= ARHLOG(2)
          CALL QSETOP(I2,I2,MSEL,MSEL,OPVAL)
C         let user make changes
          CALL Q1EDIT
     O               (IRET)
          IF (IRET .EQ. 1) THEN
C           get users changes
            CALL QGETR (I1,
     O                  ARPCSC)
            CALL QGETOP(I2,
     O                  OPVAL)
            ARHLOG(1) = OPVAL(1)
            ARHLOG(2) = OPVAL(2)
C           WRITE(99,*) 'edit ARHLOG,ARPCSC:',ARPCSC(1),ARHLOG(1)
          END IF
C         turn off previous command
          CALL ZSTCMA (I4,I0)
          GO TO 90
C
 50     CONTINUE
C         which type of plot?
 55       CONTINUE
            SGRP= 71
            PREVFG = 0
            CALL QRESP (MESSFL,SCLU,SGRP,PLTTYP)
            IF (PLTTYP.EQ.7) THEN
C             need to do screen to see if regression line wanted
C             storm or specified time units, peaks or volumes
C             make previous command available
              CALL ZSTCMA (I4,I1)
              SGRP= 72
              CALL Q1INIT(MESSFL,SCLU,SGRP)
C             regression line?
              MSEL(1) = 1
              MSEL(2) = 1
              OPVAL(1)= REGLIN
              OPVAL(2)= STORMS
              CALL QSETOP(I2,I2,MSEL,MSEL,OPVAL)
              CALL QSETR (I1,RSTRMP)
              CALL QSETI (I1,ISTRMM)
              CALL QSETCO (I1,PEKVOL)
C             let user make changes
              CALL Q1EDIT
     O                   (IRET)
              IF (IRET .EQ. 1) THEN
C               get users changes
                CALL QGETOP(I2,
     O                      OPVAL)
                REGLIN = OPVAL(1)
                STORMS = OPVAL(2)
                CALL QGETR (I1,
     O                      RSTRMP)
                CALL QGETI (I1,
     O                      ISTRMM)
                CALL QGETCO (I1,
     O                       PEKVOL)
              ELSE IF (IRET.EQ.2) THEN
C               user wants previous
                PREVFG = 1
              END IF
C             turn off previous command
              CALL ZSTCMA (I4,I0)
            END IF
          IF (PREVFG.EQ.1) GO TO 55
          GO TO 90
C
 60     CONTINUE
C         produce the plot
          IF (WIPEIT(PLTWIN).EQ.1 .AND. PLTDEV.EQ.1) THEN
C           window size has been modified, window needs to disappear
            CALL PDNPLT (PLTWIN,I1,I0)
            WIPEIT(PLTWIN) = 0
          END IF
C         device details
          CALL PLTXXX (PLTDEV,PLTWIN,WINDIM(1,PLTWIN),
     M                 DEVFIL,
     O                 DEVCOD,CMPTYP,WINACT,PLTCNT)
C         retrieving data message
          SGRP = 40
          CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,LINCNT)
          IF (PLTTYP.NE.5) THEN
C           want timeseries or xy plot, continue
            LTS= TS
            LTU= TU
          ELSE
C           want flow/duration plot, how many points?
            LTU = 4
            LTS = 1
          END IF
C         how many points
          WRITE(99,*) 'pl start:',SDATE,LTU,LTS
          WRITE(99,*) '     end:',EDATE
          CALL TIMDIF (SDATE,EDATE,LTU,LTS,
     O                 NPTS)
          WRITE(99,*) '    npts:',NPTS
C         set time unit parameters for plotting
          CALL TSBTIM (LTU,LTS,DTRAN,QFLAG)
          IF (NPTS.LE.0) THEN
C           no points to plot, tell user
            SGRP= 47
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            RETCOD = 1
          ELSE
C           we have some points, continue
            DO 510 K= 1,NDSN
C             get data
              JTMP= (K-1)* NPTS+ 1
              IF (JTMP+NPTS .GT. BUFMAX) THEN
C               oops,will blow out buffer
                WRITE(99,*) 'too many points to plot'
                SGRP= 46
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                RETCOD = 1
              ELSE
C               get data for plot
C               WRITE(99,*) 'b4g data:',I,K,JTMP,ADSN(K)
C               WRITE(99,*) '         ',YX(JTMP),YX(JTMP+NPTS-1)
C               WRITE(99,*) '         ',SDATE
C               CALL WDTGET (WDMSFL,ADSN(K),LTS,SDATE,
C    I                       NPTS,DTRAN,QFLAG,LTU,
C    O                       YX(JTMP),RETCOD)
C               WRITE(99,*) 'aft data:',RETCOD
C               WRITE(99,*) '         ',YX(JTMP),YX(JTMP+NPTS-1)
                CALL TSBWDS (WDMSFL,ADSN(K))
                CALL TSBGET (SDATE,NPTS,
     O                       YX(JTMP),RETCOD)
                IF (RETCOD .LT. 0) THEN
C                 could not retrieve data
                  SGRP= 41
                  CALL PRNTXI (MESSFL,SCLU,SGRP,ADSN(K))
                  RETCOD = 1
                END IF
              END IF
 510        CONTINUE
          END IF
C
          IF (RETCOD .EQ. 0) THEN
C           continue with plot, set title
C           set legend location upper left
            LL(1)= -1.0
            LL(2)= -1.0
            CALL GPLEDG (LL)
            IF (PLTDEV .GT. 1) THEN
              SGRP = 45
              CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,LINCNT)
            END IF
            IF (PLTTYP .EQ. 1) THEN
C             do standard timeseries plot
              MNLTZO= 0
              CALL QDTMPL(WINACT,ARHLOG,MNLTZO,ARPCSC,NDSN,WHICH,
     I                    NPTS,MAXD,BUFMAX,YX,SDATE,LTS,LTU,DTYPE,
     I                    TYPIND,LNTYP,COLOR,PATTRN,SYMBOL,
     I                    CLAB,YLLAB,YRLAB,ALAB,TITL,CMPTYP)
            ELSE IF (PLTTYP .EQ. 2) THEN
C             do residual timeseries plot
              IF (NDSN.GT.1) THEN
C               need 2 data sets for this type of plot
                MNLTZO= 3
C               data needs to be calculated
                DO 520 K = 1,NPTS
                  TMPYX(K) = YX(K+NPTS) - YX(K)
 520            CONTINUE
C               build title for this plot
                CALL CVARAR (I20,CLAB(1),I20,CLAB1)
                CALL CVARAR (I20,CLAB(2),I20,CLAB2)
                LEN1  = LENSTR(I20,CLAB1)
                LEN2  = LENSTR(I20,CLAB2)
                TPTITL= 'Residual Time Series Plot ('
                IPOS  = 28
                CALL CHRCHR (LEN2,CLAB2,TPTTL1(IPOS))
                IPOS  = IPOS+LEN2+1
                TPTTL1(IPOS) = '-'
                IPOS  = IPOS+2
                CALL CHRCHR (LEN1,CLAB1,TPTTL1(IPOS))
                IPOS  = IPOS+LEN1
                TPTTL1(IPOS) = ')'
                IPOS  = IPOS+2
C               add tu, dtran
                TPTTL1(IPOS) = 'f'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'o'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'r'
                IPOS = IPOS+2
                CALL CVARAR (I8,CTRAN,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                CALL CVARAR (I8,CTUNIT,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                IF (CNTSEN.EQ.1) THEN
C                 locn and con vary, add scen to title
                  CALL CVARAR (I8,CSCENM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTCON.EQ.1) THEN
C                 scen and loc vary, add constit to title
                  CALL CVARAR (I8,CCONNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTLOC.EQ.1) THEN
C                 scen and con vary, add location to title
                  TPTTL1(IPOS) = 'a'
                  IPOS = IPOS+1
                  TPTTL1(IPOS) = 't'
                  IPOS = IPOS+2
                  CALL CVARAR (I8,CLOCNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
C               don't allow fills or less than 100 percent of scale
                CALL ZIPI (MAXD,I0,LPAT)
                RTEMP(1) = 100.0
                CALL QDTMPL(WINACT,ARHLOG,MNLTZO,RTEMP,I1,WHICH,
     I                      NPTS,MAXD,BUFMAX,TMPYX,SDATE,LTS,LTU,DTYPE,
     I                      TYPIND,LNTYP,COLOR,LPAT,SYMBOL,
     I                      CLAB,YLLAB,YRLAB,ALAB,TPTITL,CMPTYP)
              ELSE
C               tell user not enough data sets
                SGRP= 50
                CALL PRNTXT (MESSFL,SCLU,SGRP)
              END IF
            ELSE IF (PLTTYP .EQ. 3) THEN
C             do cumulative differences timeseries plot
              IF (NDSN.GT.1) THEN
C               need 2 data sets for this type of plot
                MNLTZO= 3
C               data needs to be calculated
                TMPYX(1) = YX(1+NPTS) - YX(1)
                DO 530 K = 2,NPTS
                  TMPYX(K) = (YX(K+NPTS) - YX(K)) + TMPYX(K-1)
 530            CONTINUE
C               build title for this plot
                CALL CVARAR (I20,CLAB(1),I20,CLAB1)
                CALL CVARAR (I20,CLAB(2),I20,CLAB2)
                LEN1  = LENSTR(I20,CLAB1)
                LEN2  = LENSTR(I20,CLAB2)
                TPTITL= 'Cumulative Differences Time Series Plot ('
                IPOS  = 42
                CALL CHRCHR (LEN2,CLAB2,TPTTL1(IPOS))
                IPOS  = IPOS+LEN2+1
                TPTTL1(IPOS) = '-'
                IPOS  = IPOS+2
                CALL CHRCHR (LEN1,CLAB1,TPTTL1(IPOS))
                IPOS  = IPOS+LEN1
                TPTTL1(IPOS) = ')'
                IPOS  = IPOS+1
                TPTTL1(IPOS) = '&'
                IPOS  = IPOS+2
C               add tu, dtran
                TPTTL1(IPOS) = 'f'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'o'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'r'
                IPOS = IPOS+2
                CALL CVARAR (I8,CTRAN,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                CALL CVARAR (I8,CTUNIT,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                IF (CNTSEN.EQ.1) THEN
C                 locn and con vary, add scen to title
                  CALL CVARAR (I8,CSCENM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTCON.EQ.1) THEN
C                 scen and loc vary, add constit to title
                  CALL CVARAR (I8,CCONNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTLOC.EQ.1) THEN
C                 scen and con vary, add location to title
                  TPTTL1(IPOS) = 'a'
                  IPOS = IPOS+1
                  TPTTL1(IPOS) = 't'
                  IPOS = IPOS+2
                  CALL CVARAR (I8,CLOCNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
C               don't allow fills
                CALL ZIPI (MAXD,I0,LPAT)
                RTEMP(1) = 100.0
                CALL QDTMPL(WINACT,ARHLOG,MNLTZO,RTEMP,I1,WHICH,
     I                      NPTS,MAXD,BUFMAX,TMPYX,SDATE,LTS,LTU,DTYPE,
     I                      TYPIND,LNTYP,COLOR,LPAT,SYMBOL,
     I                      CLAB,YLLAB,YRLAB,ALAB,TPTITL,CMPTYP)
              ELSE
C               tell user not enough data sets
                SGRP= 52
                CALL PRNTXT (MESSFL,SCLU,SGRP)
              END IF
            ELSE IF (PLTTYP .EQ. 4) THEN
C             do bar chart timeseries plot
              MNLTZO= 0
C             force arithmetic
              ITMP2(1) = 1
              ITMP2(2) = 1
C             initialize temp buffer
              CALL ZIPR (BUFMAX,R0,TMPYX)
C             data needs to be arranged in buffer with spacing
              PROBFG = 0
              DO 550 L = 1,NDSN
                DO 540 K = 1,NPTS
                  ISUB = L+((K-1)*(NDSN+1))+(NPTS*(NDSN+1)*(L-1))
                  IF (ISUB.LE.BUFMAX) THEN
C                   room to add to buffer
                    TMPYX(ISUB) = YX(K+((L-1)*NPTS))
                  ELSE
C                   problem, too many points
                    PROBFG = 1
                  END IF
 540            CONTINUE
 550          CONTINUE
              NPTS = (NDSN+1)*NPTS
              IF (PROBFG.EQ.1) THEN
C               tell user too many points
                SGRP= 53
                CALL PRNTXT (MESSFL,SCLU,SGRP)
              END IF
C             set specs for blank 'spacing' data set
C             save current user's specs for this set
              TTYP = TYPIND(NDSN+1)
              TSYM = SYMBOL(NDSN+1)
              TLNT = LNTYP(NDSN+1)
              TCOL = COLOR(NDSN+1)
              TPAT = PATTRN(NDSN+1)
              TWHI = WHICH(NDSN+1)
              TDTY = DTYPE(NDSN+1)
              TCLA = CLAB(NDSN+1)
C             now set spacing data set specs
              TYPIND(NDSN+1) = NDSN+1
              SYMBOL(NDSN+1) = 0
              LNTYP(NDSN+1)  = 1
              COLOR(NDSN+1)  = 1
              PATTRN(NDSN+1) = 0
              WHICH(NDSN+1)  = 1
              DTYPE(NDSN+1)  = 1
              CLAB(NDSN+1)   = ' '
C             do plot
              CALL QDBRPL(WINACT,ITMP2,MNLTZO,ARPCSC,NDSN+1,WHICH,
     I                    NPTS,MAXD,BUFMAX,TMPYX,SDATE,LTS,LTU,DTYPE,
     I                    TYPIND,LNTYP,COLOR,PATTRN,SYMBOL,
     I                    CLAB,YLLAB,YRLAB,ALAB,TITL,CMPTYP)
C             put current user's specs back for this set
              TYPIND(NDSN+1)= TTYP
              SYMBOL(NDSN+1)= TSYM
              LNTYP(NDSN+1) = TLNT
              COLOR(NDSN+1) = TCOL
              PATTRN(NDSN+1)= TPAT
              WHICH(NDSN+1) = TWHI
              DTYPE(NDSN+1) = TDTY
              CLAB(NDSN+1)  = TCLA
            ELSE IF (PLTTYP .EQ. 5) THEN
C             do flow/duration plot
              XLAB  = 'Percent chance flow exceeded'
              LPLT  = 8
              CALL QDFDPL (LPLT,I2,WINACT,NPTS,BUFMAX,MAXD,
     I                     NDSN,TYPIND,COLOR,LNTYP,
     I                     CLAB,XLAB,YLLAB,YRLAB,TITL,I1,
     I                     DEVCOD,CMPTYP,
     M                     YX)
            ELSE IF (PLTTYP .EQ. 6) THEN
C             do difference xy plot
              IF (NDSN.GT.1) THEN
C               need 2 data sets for this type of plot
C               data needs to be calculated
                DO 560 K = 1,NPTS
C                 calculate sim - measured
                  TMPYX(K) = YX(K+NPTS) - YX(K)
 560            CONTINUE
                DO 570 K = 1,NPTS
C                 fill in measured
                  TMPYX(K+NPTS) = YX(K)
 570            CONTINUE
C               build title for this plot
                CALL CVARAR (I20,CLAB(1),I20,CLAB1)
                CALL CVARAR (I20,CLAB(2),I20,CLAB2)
                LEN1  = LENSTR(I20,CLAB1)
                LEN2  = LENSTR(I20,CLAB2)
                TPTITL= 'Difference Plot ('
                IPOS  = 18
                CALL CHRCHR (LEN2,CLAB2,TPTTL1(IPOS))
                IPOS  = IPOS+LEN2+1
                TPTTL1(IPOS) = '-'
                IPOS  = IPOS+2
                CALL CHRCHR (LEN1,CLAB1,TPTTL1(IPOS))
                IPOS  = IPOS+LEN1+1
                TPTTL1(IPOS) = 'v'
                IPOS  = IPOS+1
                TPTTL1(IPOS) = 's'
                IPOS  = IPOS+1
                TPTTL1(IPOS) = '.'
                IPOS  = IPOS+2
                CALL CHRCHR (LEN1,CLAB1,TPTTL1(IPOS))
                IPOS  = IPOS+LEN1
                TPTTL1(IPOS) = ')'
                IPOS  = IPOS+1
                TPTTL1(IPOS) = '&'
                IPOS  = IPOS+2
C               add tu, dtran
                TPTTL1(IPOS) = 'f'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'o'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'r'
                IPOS = IPOS+2
                CALL CVARAR (I8,CTRAN,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                CALL CVARAR (I8,CTUNIT,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                IF (CNTSEN.EQ.1) THEN
C                 locn and con vary, add scen to title
                  CALL CVARAR (I8,CSCENM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTCON.EQ.1) THEN
C                 scen and loc vary, add constit to title
                  CALL CVARAR (I8,CCONNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTLOC.EQ.1) THEN
C                 scen and con vary, add location to title
                  TPTTL1(IPOS) = 'a'
                  IPOS = IPOS+1
                  TPTTL1(IPOS) = 't'
                  IPOS = IPOS+2
                  CALL CVARAR (I8,CLOCNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                LPLT  = 1
                TPYLAB= 'DIFFERENCE'
                TPXLAB= CLAB(1)
                ITMP2(1)= 1
                ITMP2(2)= 1
                TSYMBL(1)= SYMBOL(1)
                TLNTYP(1)= 0
                IF (SYMBOL(1).EQ.0) THEN
C                 need a symbol, default to one
                  TSYMBL(1) = 1
                END IF
                CALL QDXYPT(LPLT,ITMP2,WINACT,NPTS,I2,
     I                      BUFMAX,TMPYX,CLAB,TPXLAB,TPYLAB,YRLAB,
     I                      TPTITL,I1,DEVCOD,TSYMBL,COLOR,TLNTYP,CMPTYP)
              ELSE
C               tell user not enough data sets
                SGRP= 54
                CALL PRNTXT (MESSFL,SCLU,SGRP)
              END IF
            ELSE IF (PLTTYP .EQ. 7) THEN
C             do xy plot
              IF (NDSN.LT.2) THEN
C               tell user not enough data sets
                SGRP= 54
                CALL PRNTXT (MESSFL,SCLU,SGRP)
              ELSE
C               have 2 data sets for this type of plot
C               build title for this plot
                CALL CVARAR (I20,CLAB(1),I20,CLAB1)
                CALL CVARAR (I20,CLAB(2),I20,CLAB2)
                LEN1  = LENSTR(I20,CLAB1)
                LEN2  = LENSTR(I20,CLAB2)
                IF (PEKVOL(1).EQ.1 .AND. STORMS.EQ.1) THEN
C                 do title for storm peaks plot
                  TPTITL= 'Storm Peaks Plot ('
                  IPOS  = 19
                ELSE IF (PEKVOL(1).EQ.2 .AND. STORMS.EQ.1) THEN
                  TPTITL= 'Storm Volume Plot ('
                  IPOS  = 20
                ELSE IF (STORMS.EQ.2) THEN
                  TPTITL= 'XY Plot ('
                  IPOS  = 10
                END IF
                CALL CHRCHR (LEN1,CLAB1,TPTTL1(IPOS))
                IPOS  = IPOS+LEN1+1
                TPTTL1(IPOS) = 'v'
                IPOS  = IPOS+1
                TPTTL1(IPOS) = 's'
                IPOS  = IPOS+1
                TPTTL1(IPOS) = '.'
                IPOS  = IPOS+2
                CALL CHRCHR (LEN2,CLAB2,TPTTL1(IPOS))
                IPOS  = IPOS+LEN2
                TPTTL1(IPOS) = ')'
                IPOS  = IPOS+1
                TPTTL1(IPOS) = '&'
                IPOS  = IPOS+2
C               add tu, dtran
                TPTTL1(IPOS) = 'f'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'o'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'r'
                IPOS = IPOS+2
                CALL CVARAR (I8,CTRAN,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                CALL CVARAR (I8,CTUNIT,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                IF (CNTSEN.EQ.1) THEN
C                 locn and con vary, add scen to title
                  CALL CVARAR (I8,CSCENM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTCON.EQ.1) THEN
C                 scen and loc vary, add constit to title
                  CALL CVARAR (I8,CCONNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTLOC.EQ.1) THEN
C                 scen and con vary, add location to title
                  TPTTL1(IPOS) = 'a'
                  IPOS = IPOS+1
                  TPTTL1(IPOS) = 't'
                  IPOS = IPOS+2
                  CALL CVARAR (I8,CLOCNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                STRMFG = 0
                IF (STORMS.EQ.1) THEN
C                 user wants storm plot, begin to find them
                  CALL TIMDIF(SDATE,EDATE,TU,TS,NVALS)
C                 must have at least 100 points to find 1 percent of points
                  IF (NVALS.LT.100) THEN
C                   not enough points to search, tell user
                    SGRP= 56
                    CALL PRNTXT (MESSFL,SCLU,SGRP)
                    STRMFG = 2
                  ELSE
C                   enough values, continue
                    STRMFG = 1
C                   find storms for first data set
                    CALL CMSTRM (SDATE,EDATE,ISTRMM(1),
     I                           ISTRMM(1),RSTRMP(1),BUFMAX,YX,
     I                           TS,TU,NVALS,
     M                           INDX,
     O                           CSTRM,STSTRM,ENSTRM,TRSTR1,STRMP1,
     O                           AVSTV1)
                    NPTS = CSTRM
C                   find storm runoff for second data set for matching days
                    CALL MATSTM (WDMSFL,ADSN(2),CSTRM,STSTRM,ENSTRM,
     I                           ISTRMM(1),BUFMAX,
     M                           TMPYX,
     O                           TRSTR2,STRMP2,AVSTV2,RETCOD)
                    DO 710 I = 1,CSTRM
C                     build storm table screen
                      CVAL2(1,I) = 1
                      RVAL2(1,I) = AVSTV1(I)
                      RVAL2(2,I) = STRMP1(I)
                      RVAL2(3,I) = AVSTV2(I)
                      RVAL2(4,I) = STRMP2(I)
                      IVAL2(1,I) = STSTRM(1,I)
                      IVAL2(2,I) = STSTRM(2,I)
                      IVAL2(3,I) = STSTRM(3,I)
                      IVAL2(4,I) = ENSTRM(1,I)
                      IVAL2(5,I) = ENSTRM(2,I)
                      IVAL2(6,I) = ENSTRM(3,I)
 710                CONTINUE
C                   build first header line
                    SGRP    = 62
                    CLENG(1)= 20
                    IPOS    = 1
                    CALL CHRCHR (CLENG(1),CLAB1,CVAL1(IPOS))
                    IPOS    = 21
                    CLENG(2)= 20
                    CALL CHRCHR (CLENG(2),CLAB2,CVAL1(IPOS))
                    CALL PMXTXA (MESSFL,SCLU,SGRP,I1,I1,IM1,I2,
     I                           CLENG,CVAL1)
                    CALL ZMNSST
C                   build second header line
                    SGRP = 63
                    CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,IM1,LINCNT)
                    CALL ZMNSST
                    SGRP = 59
                    CALL Q2INIT (MESSFL,SCLU,SGRP)
                    CALL Q2STCO (I1,CSTRM,CVAL2)
                    CALL Q2SETR (I4,CSTRM,RVAL2)
                    CALL Q2SETI (I6,CSTRM,IVAL2)
                    CALL Q2EDIT (CSTRM,
     O                           IRET)
                    IF (IRET .EQ. 1) THEN
C                     accept, do we want to add any date sets
                      CALL Q2GTCO (I1,CSTRM,
     O                             CVAL2)
                      DO 720 I = 1,CSTRM
                        IF (CVAL2(1,I).EQ.2) THEN
C                         user wants to add this as a date set
C                         get number of date sets
                          CALL DTADD (I1,NEWDT)
                          CDSID = '<STORM>'
                          SSDATE(1)= STSTRM(2,I)
                          SSDATE(2)= STSTRM(3,I)
                          SEDATE(1)= ENSTRM(2,I)
                          SEDATE(2)= ENSTRM(3,I)
                          START(1) = STSTRM(1,I)
                          START(2) = STSTRM(2,I)
                          START(3) = STSTRM(3,I)
                          START(4) = STSTRM(4,I)
                          START(5) = STSTRM(5,I)
                          START(6) = STSTRM(6,I)
                          ENDDT(1) = ENSTRM(1,I)
                          ENDDT(2) = ENSTRM(2,I)
                          ENDDT(3) = ENSTRM(3,I)
                          ENDDT(4) = ENSTRM(4,I)
                          ENDDT(5) = ENSTRM(5,I)
                          ENDDT(6) = ENSTRM(6,I)
                          CALL DTPUT (NEWDT,I0,CDSID,START,ENDDT,
     I                                SSDATE,SEDATE,TU,TS,DTRAN)
                        END IF
 720                  CONTINUE
                    END IF
C                   now put storm peaks or volumes in xy buffer
                    IF (PEKVOL(1).EQ.1) THEN
C                     want storm peaks plot
                      CALL COPYR (NPTS,STRMP1,TMPYX(1))
                      CALL COPYR (NPTS,STRMP2,TMPYX(NPTS+1))
                    ELSE
C                     want average storm volumes plot
                      CALL COPYR (NPTS,AVSTV1,TMPYX(1))
                      CALL COPYR (NPTS,AVSTV2,TMPYX(NPTS+1))
                    END IF
                  END IF
                END IF
                LPLT  = 1
                TPYLAB= CLAB(1)
                TPXLAB= CLAB(2)
                ITMP2(1)= 1
                ITMP2(2)= 1
                TSYMBL(1)= SYMBOL(1)
                TLNTYP(1)= 0
                IF (SYMBOL(1).EQ.0) THEN
C                 need a symbol, default to one
                  TSYMBL(1) = 1
                END IF
                IF (STRMFG.EQ.1) THEN
C                 do storm plot
                  IF (REGLIN.EQ.1 .AND. NPTS.GT.2) THEN
C                   want 45 degree line and regression line on plot
                    CALL FITLIN (NPTS,BUFMAX,TMPYX,
     O                           ACOEF,BCOEF,RSQUAR)
                    WRITE (99,*) 'REGRESSION: ',ACOEF,BCOEF,RSQUAR
C                   set legend location so it wont appear
                    LL(1)= -2.0
                    LL(2)= -2.0
                    CALL GPLEDG (LL)
                    CALL QDXYRG(LPLT,ITMP2,WINACT,NPTS,I2,
     I                          BUFMAX,TMPYX,CLAB,TPXLAB,TPYLAB,YRLAB,
     I                          TPTITL,I1,DEVCOD,TSYMBL,COLOR(1),
     I                          CMPTYP,ACOEF,BCOEF,RSQUAR)
C                   set legend location back to upper left
                    LL(1)= -1.0
                    LL(2)= -1.0
                    CALL GPLEDG (LL)
                  ELSE
C                   no regression line
                    CALL QDXYPT(LPLT,ITMP2,WINACT,NPTS,I2,
     I                          BUFMAX,TMPYX,CLAB,TPXLAB,TPYLAB,YRLAB,
     I                          TPTITL,I1,DEVCOD,TSYMBL,COLOR,
     I                          TLNTYP,CMPTYP)
                  END IF
                ELSE IF (STRMFG.EQ.0) THEN
C                 no storms, regular time period
                  IF (REGLIN.EQ.1 .AND. NPTS.GT.2) THEN
C                   want 45 degree line and regression line on plot
                    CALL FITLIN (NPTS,BUFMAX,YX,
     O                           ACOEF,BCOEF,RSQUAR)
                    WRITE (99,*) 'REGRESSION: ',ACOEF,BCOEF,RSQUAR
C                   set legend location so it wont appear
                    LL(1)= -2.0
                    LL(2)= -2.0
                    CALL GPLEDG (LL)
                    CALL QDXYRG(LPLT,ITMP2,WINACT,NPTS,I2,
     I                          BUFMAX,YX,CLAB,TPXLAB,TPYLAB,YRLAB,
     I                          TPTITL,I1,DEVCOD,TSYMBL,COLOR(1),
     I                          CMPTYP,ACOEF,BCOEF,RSQUAR)
C                   set legend location back to upper left
                    LL(1)= -1.0
                    LL(2)= -1.0
                    CALL GPLEDG (LL)
                  ELSE
C                   no regression line
                    CALL QDXYPT(LPLT,ITMP2,WINACT,NPTS,I2,
     I                          BUFMAX,YX,CLAB,TPXLAB,TPYLAB,YRLAB,
     I                          TPTITL,I1,DEVCOD,TSYMBL,COLOR(1),
     I                          TLNTYP,CMPTYP)
                  END IF
                END IF
              END IF
            ELSE IF (PLTTYP .EQ. 8) THEN
C             do frequency plot
              CALL STFRPL (MESSFL,WDMSFL,NDSN,ADSN,SDATE(1),EDATE(1),
     I                     TITL,ARHLOG(1),SSDATE(1),SEDATE(1),COLOR,
     I                     WINACT,CMPTYP,CLAB)
            END IF
          END IF
C
          IF (PLTDEV.GE.2 .AND. CMPTYP.EQ. 5) THEN
C           close print file on aviion
            CLOSE (UNIT=DEVFIL)
C           close workstation
            CALL PDNPLT(WINACT,I1,I0)
C           graph put on file 'gksplt.out//n'
            IF (PLTTYP.LT.5) THEN
              SGRP= 43
            ELSE IF (PLTTYP.EQ.5) THEN
              SGRP= 44
            ELSE IF (PLTTYP.EQ.6 .OR. PLTTYP.EQ.7) THEN
              SGRP= 48
            END IF
            CALL PRNTXI(MESSFL,SCLU,SGRP,PLTCNT)
          END IF
          GO TO 90
C
 70     CONTINUE
C         return
          GO TO 90
C
 90     CONTINUE
      IF (RESP .NE. 8) GO TO 11
C
      RETURN
      END
C
C
C
      SUBROUTINE   STRUNI
     I                   (NSTR,STR,
     O                    CNT,USTR)
C
C     + + + PURPOSE + + +
C     determine unique strings in str
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NSTR,CNT
      CHARACTER*8 STR(NSTR),USTR(NSTR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSTR   - number of strings to search
C     STR    - strings to search
C     CNT    - count of unique strings
C     USTR   - unique strings
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,MATCH
C
C     + + + END SPECIFICATIONS + + +
C
C     first string is unique
      CNT      = 1
      USTR(CNT)= STR(1)
      DO 20 I= 2,NSTR
C       look thru other strings
        MATCH= 0
        DO 10 J= 1,CNT
C         look thru existing uniques
          IF (STR(I) .EQ. USTR(J)) THEN
C           not unique
            MATCH= 1
          END IF
 10     CONTINUE
        IF (MATCH.EQ.0) THEN
C         unique
          CNT= CNT+ 1
          USTR(CNT)= STR(I)
        END IF
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   CMSTRM
     I                   (START,ENDDT,NSTRM,LSTRM,FSTRM,
     I                    BUFMAX,YX,TS,TU,NVALS,
     M                    INDX,
     O                    CSTRM,STSTRM,ENSTRM,TRSTRM,STRMPK,AVSTVL)
C
C     + + + PURPOSE + + +
C     computes storm dates
C
C     + + + DUMMY ARGUENTS + + +
      INTEGER   START(6),ENDDT(6),NSTRM,LSTRM,CSTRM,
     1          STSTRM(6,NSTRM),ENSTRM(6,NSTRM),BUFMAX,
     2          INDX(BUFMAX)
      REAL      FSTRM,TRSTRM(NSTRM),YX(BUFMAX),STRMPK(NSTRM),
     1          AVSTVL(NSTRM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     START  - Starting date array
C     ENDDT  - Ending date array
C     NSTRM  - Max number of storms to find
C     LSTRM  - Number of storms to find
C     CSTRM  - Number of storms found
C     FSTRM  - If < 0 percent of peak which defines storm
C              If > 0 flow duration percent which defines storm
C     STSTRM - Start date of storms
C     ENSTRM - End date of storms
C     TRSTRM - Total runoff for each storm (total storm volume)
C     STRMPK - storm peak volume
C     AVSTVL - average storm volume
C     BUFMAX - maximum number of values to search
C     YX     - array of values to search
C     INDX   - array of order numbers for values to search
C     TS     -
C     TU     -
C     NVALS  -
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TS,TU,NVALS,I,J,K,OPT,
     1          NEWFLG,STSTRJ,ENSTRJ,NPOS,STPOS,TOTDYS,CNTDYS
      REAL      RTMP,XSTRM,RCNT
C
C     + + + EXTERNALS + + +
      EXTERNAL  TIMADD, ASRTR
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C     + + + END SPECIFICATIONS + + +
C
C     sort values in place
      OPT= 0
      CALL ASRTR(OPT,NVALS,YX,INDX)
      WRITE(99,*) 'just sorted data'
C     look for storms
      WRITE(99,*) 'looking for ',LSTRM,' storms, tolr:',FSTRM
      IF (FSTRM.GT.0.0) THEN
        STPOS= NVALS*(FSTRM/100.)
        XSTRM= YX(INDX(STPOS))
        WRITE(99,*) '  STPOS,XSTRM:',STPOS,XSTRM
      ELSE
        STPOS= 0
        XSTRM= 0.0
      END IF
      CSTRM= 0
      NPOS = NVALS
 10   CONTINUE
        NEWFLG= 1
        IF (CSTRM.GT.0) THEN
C         see if this storm is in any existing ones
          I= 0
 15       CONTINUE
            I= I+ 1
            IF (INDX(NPOS) .GE. STSTRM(1,I) .AND.
     1          INDX(NPOS) .LE. ENSTRM(1,I)) THEN
C             already have it
C             WRITE(99,*) 'fail storm:',NPOS,INDX(NPOS)
              NEWFLG= 0
            END IF
          IF (NEWFLG.EQ.1 .AND. I.LT.CSTRM) GO TO 15
        END IF
        IF (NEWFLG.EQ.1) THEN
C         a new storm, find beginning of storm
          IF (ABS(XSTRM).LT.1E-4) THEN
            RTMP= YX(INDX(NPOS))* ABS(FSTRM)/100.0
          ELSE
            RTMP= XSTRM
          END IF
C         WRITE(99,*) 'new storm1:',NPOS,INDX(NPOS),YX(INDX(NPOS)),RTMP
          STSTRJ= INDX(NPOS)+ 1
 20       CONTINUE
            STSTRJ= STSTRJ- 1
          IF (YX(STSTRJ-1) .GT. RTMP .AND. STSTRJ.GT.1) GO TO 20
C         find end of storm
          ENSTRJ= INDX(NPOS)- 1
 30       CONTINUE
            ENSTRJ= ENSTRJ+ 1
          IF (YX(ENSTRJ+1) .GT. RTMP .AND. ENSTRJ.LT.NVALS) GO TO 30
          CSTRM= CSTRM+ 1
          STSTRM(1,CSTRM)= STSTRJ
          ENSTRM(1,CSTRM)= ENSTRJ
C         WRITE (99,*) 'new storm2:',CSTRM,NPOS,INDX(NPOS),STSTRJ,ENSTRJ
        END IF
        NPOS= NPOS- 1
      IF (CSTRM.LT.LSTRM .AND. NPOS.GT.STPOS) GO TO 10
      WRITE(99,*) 'out of find loop:',CSTRM,LSTRM,NPOS
      TOTDYS= 0
      DO 40 I= 1,CSTRM
C       WRITE(99,*)
C       WRITE(99,*) 'storm:',I
        J= STSTRM(1,I)- 1
C       WRITE(99,*) 'before:',J,YX(J)
C
        TRSTRM(I)= 0.0
C       initialize storm peak values
        STRMPK(I)= YX(STSTRM(1,I))
        RCNT = 0.0
        DO 35 J= STSTRM(1,I),ENSTRM(1,I)
          RCNT= RCNT + 1.0
          TRSTRM(I)= TRSTRM(I)+ YX(J)
          IF (YX(J).GT.STRMPK(I)) THEN
C           new storm peak
            STRMPK(I) = YX(J)
          END IF
 35     CONTINUE
C       compute average storm volume
        AVSTVL(I) = TRSTRM(I)/RCNT
C
        J= ENSTRM(1,I)+ 1
C       WRITE(99,*) 'after :',J,YX(J)
C       calculate calendar start day
        CNTDYS= ENSTRM(1,I)- STSTRM(1,I)+ 1
        TOTDYS= TOTDYS+ CNTDYS
C       WRITE(99,*) 'day cnt:',CNTDYS,TOTDYS
        K= STSTRM(1,I)- 1
        CALL TIMADD(START,TU,TS,K,
     O              STSTRM(1,I))
C       calculate calendar end day
        K= ENSTRM(1,I)- 1
        CALL TIMADD(START,TU,TS,K,
     O              ENSTRM(1,I))
        ENSTRM(4,I)= 24
 40   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   FITLIN
     I                   (NPTS,BUFMAX,YX,
     O                    ACOEF,BCOEF,RSQUAR)
C
C     + + + PURPOSE + + +
C     fit a line through a set of data points using least squares
C     regression.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NPTS,BUFMAX
      REAL          YX(BUFMAX),ACOEF,BCOEF,RSQUAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NPTS   - number of points (number of time steps)
C     BUFMAX - size of data buffer YX
C     YX     - data buffer
C     ACOEF  - 'a' coefficient in regression line (y=ax+b)
C     BCOEF  - 'b' coefficient in regression line (y=ax+b)
C     RSQUAR - 'r squared', the coefficient of determination
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I
      REAL        SUM1,SUM2,SUM3,SUM4,AVG1,AVG2
C
C     + + + END SPECIFICATIONS + + +
C
      SUM1 = 0.0
      SUM2 = 0.0
      SUM3 = 0.0
      SUM4 = 0.0
C
      DO 10 I = 1,NPTS
        SUM1 = SUM1 + YX(I)
        SUM2 = SUM2 + YX(I+NPTS)
 10   CONTINUE
C
      IF (SUM1.GT.0.0 .AND. SUM2.GT.0.0) THEN
C       go ahead and compute
        AVG1 = SUM1/NPTS
        AVG2 = SUM2/NPTS
C
        DO 20 I= 1,NPTS
          SUM3 = SUM3 + ((YX(I) - AVG1) * (YX(I+NPTS) - AVG2))
          SUM4 = SUM4 + ((YX(I+NPTS) - AVG2) * (YX(I+NPTS) - AVG2))
 20     CONTINUE
C
        ACOEF = SUM3/SUM4
        BCOEF = AVG1 - (ACOEF*AVG2)
C       Y = ACOEF*X + BCOEF
C
        SUM1 = 0
        SUM2 = 0
        DO 30 I = 1,NPTS
          SUM1= SUM1 +((ACOEF*YX(I+NPTS)+BCOEF-AVG1)*
     1                 (ACOEF*YX(I+NPTS)+BCOEF-AVG1))
          SUM2= SUM2 +((YX(I)-AVG1)*(YX(I)-AVG1))
 30     CONTINUE
        RSQUAR = SUM1/SUM2
      ELSE
C       regression doesnt make sense, return zeros
        ACOEF  = 0.0
        BCOEF  = 0.0
        RSQUAR = 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MATSTM
     I                   (WDMSFL,DSN,CSTRM,STSTRM,ENSTRM,NSTRM,BUFMAX,
     M                    TMPYX,
     O                    TRSTRM,STRMPK,AVSTVL,RETCOD)
C
C     + + + PURPOSE + + +
C     computes storm runoffs for multiple sets of dates
C
C     + + + DUMMY ARGUENTS + + +
      INTEGER   WDMSFL,DSN,CSTRM,NSTRM,BUFMAX,
     1          STSTRM(6,NSTRM),ENSTRM(6,NSTRM),RETCOD
      REAL      TRSTRM(NSTRM),TMPYX(BUFMAX),STRMPK(NSTRM),AVSTVL(NSTRM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - Dataset number to calculate stats on
C     NSTRM  - Max number of storms to find
C     CSTRM  - Number of storms to find
C     STSTRM - Start date of storms
C     ENSTRM - End date of storms
C     TRSTRM - Total runoff for each storm
C     RETCOD - Return code
C     BUFMAX - maximum number of values to in storm period
C     TMPYX  - array of values in storm period
C     STRMPK - array of storm peaks
C     AVSTVL - array of average storm volumes
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TS,TU,NVALS,DTRAN,QFLG,I,J,
     1          START(6),ENDDT(6)
      REAL      RCNT
C
C     + + + EXTERNALS + + +
      EXTERNAL  TIMDIF, WDTGET
C
C     + + + END SPECIFICATIONS + + +
C
      TS   = 1
      TU   = 4
      QFLG = 30
      DTRAN= 0
C
      DO 100 I = 1,CSTRM
C       get number of values
        START(1) = STSTRM(1,I)
        START(2) = STSTRM(2,I)
        START(3) = STSTRM(3,I)
        START(4) = STSTRM(4,I)
        START(5) = STSTRM(5,I)
        START(6) = STSTRM(6,I)
        ENDDT(1) = ENSTRM(1,I)
        ENDDT(2) = ENSTRM(2,I)
        ENDDT(3) = ENSTRM(3,I)
        ENDDT(4) = ENSTRM(4,I)
        ENDDT(5) = ENSTRM(5,I)
        ENDDT(6) = ENSTRM(6,I)
        CALL TIMDIF(START,ENDDT,TU,TS,NVALS)
C
        IF (NVALS .GT. 1000) THEN
C         can only read so much data
          NVALS= 1000
        END IF
C
        WRITE(99,*) 'number of days:',NVALS
C
C       get data
        CALL WDTGET(WDMSFL,DSN,TS,START,NVALS,DTRAN,QFLG,TU,
     O              TMPYX,RETCOD)
        WRITE(99,*) 'just got data:',RETCOD
C
        TRSTRM(I)= 0.0
C       initialize storm peak values
        STRMPK(I)= TMPYX(1)
        RCNT = 0.0
        DO 50 J= 1,NVALS
C         sum runoff values for these storm dates
          TRSTRM(I)= TRSTRM(I)+ TMPYX(J)
          RCNT= RCNT + 1.0
          IF (TMPYX(J).GT.STRMPK(I)) THEN
C           new storm peak
            STRMPK(I) = TMPYX(J)
          END IF
 50     CONTINUE
C       compute average storm volume
        AVSTVL(I) = TRSTRM(I)/RCNT
C
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   SGLABL
     I                   (NDSN,CSCENM,CLOCNM,CCONNM,TU,DTRAN,
     M                    WHICH,TYPIND,
     O                    CNTCON,CNTSEN,CNTLOC,
     O                    ALAB,YRLAB,YLLAB,TITL,CLAB,CTRAN,CTUNIT)
C
C     + + + PURPOSE + + +
C     creates labels for scenario generator plots
C
C     + + + DUMMY ARGUENTS + + +
      INTEGER       NDSN,CNTCON,WHICH(NDSN),TYPIND(NDSN),
     1              CNTSEN,CNTLOC,TU,DTRAN
      CHARACTER*8   CSCENM(NDSN),CLOCNM(NDSN),CCONNM(NDSN),
     1              CTRAN,CTUNIT
      CHARACTER*20  CLAB(NDSN)
      CHARACTER*80  YRLAB,YLLAB,ALAB
      CHARACTER*240 TITL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDSN   - number of data sets for which to set labels
C     CNTCON - count of constituents
C     CNTSEN - count of scenarios
C     CNTLOC - count of locations
C     WHICH  - which axis flag
C     TYPIND -
C     CSCENM - scenario names
C     CLOCNM - location names
C     CCONNM - constituent names
C     CLAB   - plot label
C     YRLAB  - right axis label
C     YLLAB  - left axis label
C     ALAB   - aux axis label
C     TITL   - title of plot
C     TU     - time units for data
C     DTRAN  - transformation function for data
C     CTRAN  -
C     CTUNIT -
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmaxd.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I8,SLEN,LLEN,CLEN,J,PRECFG
      CHARACTER*1  CSCEN1(8),CLOCN1(8),CCONN1(8),BLNK
      CHARACTER*8  CTMP,UNISEN(MAXD),UNICON(MAXD),UNILOC(MAXD)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    CVARAR,LENSTR,STRUNI
C
C     + + + OUTPUT FORMATS + + +
2010  FORMAT (20A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I8    = 8
      YRLAB = ' '
      YLLAB = ' '
      BLNK  = ' '
      PRECFG= 0
C
      IF (DTRAN.EQ.0) THEN
        CTRAN = 'MEAN   '
      ELSE IF (DTRAN.EQ.1) THEN
        CTRAN = 'SUMMED '
      ELSE IF (DTRAN.EQ.2) THEN
        CTRAN = 'MAXIMUM'
      ELSE IF (DTRAN.EQ.3) THEN
        CTRAN = 'MINIMUM'
      END IF
      IF (TU.EQ.1) THEN
        CTUNIT = 'SECONDLY'
      ELSE IF (TU.EQ.2) THEN
        CTUNIT = 'MINUTELY'
      ELSE IF (TU.EQ.3) THEN
        CTUNIT = 'HOURLY  '
      ELSE IF (TU.EQ.4) THEN
        CTUNIT = 'DAILY   '
      ELSE IF (TU.EQ.5) THEN
        CTUNIT = 'MONTHLY '
      ELSE IF (TU.EQ.6) THEN
        CTUNIT = 'YEARLY  '
      END IF
C
      IF (NDSN .EQ. 1) THEN
C       only one curve, put all info in title
        ALAB     = ' '
        YRLAB    = ' '
        YLLAB    = CCONNM(1)
        TITL     = CTUNIT // ' ' // CTRAN // ' ' // CSCENM(1) // ' '
     1             // CCONNM(1)//' at '//CLOCNM(1)
        CLAB(1)  = CSCENM(1) // ' ' // CLOCNM(1)
        WHICH(1) = 1
        TYPIND(1)= 1
      ELSE
C       count occurances of locations
        CALL STRUNI(NDSN,CLOCNM,
     O              CNTLOC,UNILOC)
C       count occurances of constit
        CALL STRUNI(NDSN,CCONNM,
     O              CNTCON,UNICON)
C       count occurances of scenarios
        CALL STRUNI(NDSN,CSCENM,
     O              CNTSEN,UNISEN)
        DO 3 I = 1,NDSN
          CTMP = CCONNM(I)
          IF (CTMP(1:2).EQ.'PR' .AND. CNTCON.GT.1) THEN
C           precip on aux axis
            WHICH(I)= 3
            ALAB    = CTMP
            IF (PRECFG.EQ.0) THEN
C             reduce count of constit for moved prec
              CNTCON= CNTCON- 1
            END IF
            PRECFG  = 1
          ELSE IF (YLLAB.EQ.' ' .OR. YLLAB.EQ.CCONNM(I)) THEN
C           we are plotting another constituent on left
            YLLAB   = CCONNM(I)
            WHICH(I)= 1
          ELSE
C           put on right for lack of a better place
            YRLAB   = CCONNM(I)
            WHICH(I)= 2
          END IF
          TYPIND(I)= I
 3      CONTINUE
C
        IF (CNTCON.EQ.1 .AND. CNTLOC.EQ.1 .AND. PRECFG.EQ.0) THEN
C         only scen varies
          TITL= CTUNIT // ' ' // CTRAN // ' ' //CCONNM(1) // ' at '
     1          // CLOCNM(1)
          DO 4 I= 1,NDSN
            CLAB(I) = CSCENM(I)
 4        CONTINUE
        ELSE IF (CNTSEN.EQ.1 .AND. CNTLOC.EQ.1) THEN
C         only const varies
          TITL= CTUNIT // ' ' // CTRAN // ' ' //CSCENM(1) // ' at '
     1          // CLOCNM(1)
          DO 5 I= 1,NDSN
            CLAB(I) = CCONNM(I)
 5        CONTINUE
        ELSE IF (CNTSEN.EQ.1 .AND. CNTCON.EQ.1) THEN
C         only locn varies
          TITL= CTUNIT // ' ' // CTRAN // ' ' //CSCENM(1) // ' '
     1          // CCONNM(1)
          DO 6 I= 1,NDSN
            CLAB(I) = CLOCNM(I)
 6        CONTINUE
        ELSE IF (CNTLOC.EQ.1) THEN
C         scen and con vary
          TITL= 'Analysis Plot for '// CTUNIT // ' ' // CTRAN // ' '
     1           // ' at ' // CLOCNM(1)
          DO 7 I= 1,NDSN
            CLAB(I) = CSCENM(I) // ' ' // CCONNM(I)
 7        CONTINUE
        ELSE IF (CNTCON.EQ.1) THEN
C         scen and loc vary
          TITL= 'Analysis Plot for '// CTUNIT // ' ' // CTRAN // ' '
     1          // CCONNM(1)
          DO 8 I= 1,NDSN
            CLAB(I) = CSCENM(I) // ' ' // CLOCNM(I)
 8        CONTINUE
        ELSE IF (CNTSEN.EQ.1) THEN
C         locn and con vary
          TITL= 'Analysis Plot for ' // CTUNIT // ' ' // CTRAN // ' '
     1          // CSCENM(1)
          DO 9 I= 1,NDSN
            CLAB(I) = CLOCNM(I) // ' ' // CCONNM(I)
 9        CONTINUE
        ELSE
C         everything varies
          TITL= 'Analysis Plot for ' // CTUNIT // ' '//CTRAN//' Values'
C         initialize curve labels scenario name, reach name
          DO 10 I= 1,NDSN
C           build headers for legend
            CALL CVARAR (I8,CSCENM(I),I8,CSCEN1)
            CALL CVARAR (I8,CLOCNM(I),I8,CLOCN1)
            CALL CVARAR (I8,CCONNM(I),I8,CCONN1)
            CLAB(I) = ' '
            SLEN = LENSTR(I8,CSCEN1)
            LLEN = LENSTR(I8,CLOCN1)
            CLEN = LENSTR(I8,CCONN1)
            IF (SLEN+LLEN+2+CLEN.GT.20) THEN
C             only 20 chars allowed, need to shorten
              CLEN = 18-SLEN-LLEN
            END IF
            WRITE (CLAB(I),2010) (CSCEN1(J),J=1,SLEN),BLNK,
     1                           (CLOCN1(J),J=1,LLEN),BLNK,
     2                           (CCONN1(J),J=1,CLEN)
C            CLAB(I) = CSCENM(I) // ' ' // CLOCNM(I) // ' ' //CCONNM(I)
            WRITE(99,*) CSCENM(I),CLOCNM(I),CCONNM(I)
 10       CONTINUE
        END IF
      END IF
C
      RETURN
      END
