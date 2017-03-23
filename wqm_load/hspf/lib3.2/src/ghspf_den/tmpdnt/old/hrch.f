C
C
C
      SUBROUTINE   PRCHRE
     I                    (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                     OSVKEY)
C
C     + + + PURPOSE + + +
C     Process the input for the rchres module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDAMON(12),NDELT,OSVKEY,SDATIM(5),EMFG,MAXOSV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     MAXOSV - maximum size of osv
C     OSVKEY - last osv file record written
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION RGEN1 + + +
      INCLUDE    'crhge.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
!     INCLUDE    'chrchnit.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,LAST,LEV,OSVKND,OSVKST,OSVREC
C
C     + + + EXTERNALS + + +
      EXTERNAL   PRGEN,PHYDR,PADCAL,PCONS,PHTRCH,PSED,PGQUAL,PRQUAL
      EXTERNAL   PACID,RCHRST,PUTOSV
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),
     $        /,' PROCESSING RCHRES NO:',I4,
     $            '     TIME STEP(DELT):',I5,'  MINS')
 2010 FORMAT (/,' FINISHED PROCESSING RCHRES NO. ',I4,
     $        /,' ',132('+'))
 2020 FORMAT (  ' ',132('-'))
C
C     + + + END SPECIFICATIONS + + +
C
      IF (RESMFG.EQ.1) THEN
C       read the general part of the osv from osvfl
C       - not implemented in this release of hspf
      ELSE
C       initialize the entire osv area
        DO 10 I= 1,MAXOSV
          IPAD(I)= -999
 10     CONTINUE
      END IF
C
C     file units in use by this opertation
      MESSU = FILE(1)
      MSGFL = FILE(15)
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2000)  OPTNO, NDELT
      END IF
C
C     minimum size of osv for this operation
      OSVREC= 1
C
C     process the general input
      CALL PRGEN
     I            (NDELT,SDATIM,NDAMON,
     O             LAST)
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2020)
      END IF
C     put english/metric units flag into rchres common
      UUNITS = EMFG
C
      IF (RESMFG.EQ.1) THEN
C       read in the rest of the osv from osvfl
C       - not implemented in this release of hspf
      END IF
C
C     input for section hydr
      IF (HYDRFG.EQ.1) THEN
        CALL PHYDR
        IF (OUTLEV.GT.1) THEN
          WRITE (MESSU,2020)
        END IF
        OSVREC= 19
      END IF
C
C     adcalc
      IF (ADFG.EQ.1) THEN
        CALL PADCAL
        IF (OUTLEV.GT.1) THEN
          WRITE (MESSU,2020)
        END IF
        OSVREC= 19
      END IF
C
C     cons
      IF (CONSFG.EQ.1) THEN
        CALL PCONS
        IF (OUTLEV.GT.1) THEN
          WRITE (MESSU,2020)
        END IF
        OSVREC= 21
      END IF
C
C     htrch
      IF (HTFG.EQ.1) THEN
        CALL PHTRCH
        IF (OUTLEV.GT.1) THEN
          WRITE (MESSU,2020)
        END IF
        OSVREC= 21
      END IF
C
C     sedtrn
      IF (SEDFG.EQ.1) THEN
        CALL PSED
        IF (OUTLEV.GT.1) THEN
          WRITE (MESSU,2020)
        END IF
        OSVREC= 22
      END IF
C
C     gqual
      IF (GQFG.EQ.1) THEN
        CALL PGQUAL
     O             (OSVREC)
        IF (OUTLEV.GT.1) THEN
          WRITE (MESSU,2020)
        END IF
      END IF
C
C     oxrx
      IF (OXFG.EQ.1) THEN
        CALL PRQUAL
     O             (OSVREC) 
        IF (OUTLEV.GT.1) THEN 
          WRITE (MESSU,2020) 
        END IF  
      END IF 
C
C     acidph
      IF (ACIDFG .EQ. 1) THEN 
        CALL PACID
        IF (OUTLEV.GT.1) THEN  
          WRITE (MESSU,2020)
        END IF  
        OSVREC= 30 
      END IF  
C
C     set flux accumulators to zero
      DO 20 LEV= 2,5
        CALL RCHRST(LEV)
 20   CONTINUE
C
      VSOILW = ength * bedwidd * zdpth * porop 
      bedarea = ength * bedwidd 
C     write the osv to disc and record the keys in opntab
      OSVKST= OSVKEY+ 1
      OSVKND= OSVKEY+ OSVREC
      CALL PUTOSV
     I            (OSVKST,OSVKND,MAXOSV,IPAD)
      OPNTAB(7,OPNO)= OSVKST  
      OPNTAB(8,OPNO)= OSVKND 
      OSVKEY        = OSVKND 
C
      IF (OUTLEV.GT.0) THEN 
        WRITE (MESSU,2010)  OPTNO  
      END IF 
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRGEN
     I                   (NDELT,SDATIM,NDAMON,
     O                    LAST)
C
C     + + + PURPOSE + + +
C     Process the general input for the rchres module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LAST,NDAMON(12),NDELT,SDATIM(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C     LAST   - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION RGEN1 + + +
      INCLUDE    'crhge.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      ERRFG,I,I1,I2,I3,I10,I12,I1440,
     #             IVAL(12),J,JLKDUM,L,PDELT,I11,SCLU,SGRP
C
C     + + + FUNCTIONS + + +
      INTEGER    DAYMNH
C
C     + + + INTRINSICS + + +
      INTRINSIC  MIN0,MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMNH,ITABLE,OMSTI,OMSG,HSCKFL
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING GENERAL INPUT')
 2050 FORMAT (/,' FINISHED PROCESSING GENERAL INPUT')
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 340
      I1   = 1
      I2   = 2
      I3   = 3
      I10  = 10
      I11  = 11
      I12  = 12
      I1440= 1440
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2000)
      END IF
C
C     incorporate information obtained from global
C     and opn sequence blocks
      RCHNO = OPTNO
      DELT  = NDELT
      DELT60= DELT/60.0
      DELTS = DELT*60.0
C
      DO 10 J= 1,5
        DATIM(J)= SDATIM(J)
 10   CONTINUE
C
      NDAYS= DAYMNH(YR,MON,NDAMON)
      DO 20 J= 1,12
        NDAY(J)= NDAMON(J)
 20   CONTINUE
C
      HRFG  = 1
      DAYFG = 1
      PIVLNO= 0
      STFG  = 1
      SPIVL=  0
      SPOPNO= OPNO
C
      IF (MON.LT.12) THEN
        NXTMON= MON+ 1
      ELSE
        NXTMON= 1
      END IF
C
C     process active sections vector
      CALL ITABLE
     I             (I1,I1,I10,I1,
     M              ASVEC)
C
C     make separate activity flag for acidph
      ASVEC(11)  = 0
      IF (ASVEC(10) .EQ. 2) THEN
        ASVEC(10)= 0
        ASVEC(11)= 1
      ELSE IF (ASVEC(10) .EQ. 3) THEN
        ASVEC(10)= 1
        ASVEC(11)= 1
      END IF
C
C     find the highest numbered active section
      LAST= 0
      DO 50 L= 1,11
        IF (ASVEC(L).EQ.1) THEN
          LAST= L
        END IF
 50   CONTINUE
C
      IF (LAST.EQ.0) THEN
C       error - there are no active sections
        CALL OMSTI (RCHNO)
        SGRP = 27
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C     check validity of active sections vector
      IF (LAST.GT.2) THEN
C       one or more "quality" sections are active
        IF (ADFG.EQ.0) THEN
C         error - section adcalc must be active if one or
C         more quality sections are active
          SGRP = 28
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
C
C       check flags in rqual sections for consistency
        ERRFG= 0
        DO 80 I= 1,3
          IF (ASVEC(11-I).EQ.1.AND.ASVEC(10-I).EQ.0) THEN
            ERRFG= 1
          END IF
 80     CONTINUE
C
        IF (ERRFG.EQ.1) THEN
C         error - if any section of rqual is active, then all
C         preceding sections of rqual must also be active
          SGRP = 29
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF
C
C     process print - info
      CALL ITABLE
     I             (I2,I1,I12,I1,
     M              IVAL)
C
      DO 105 I= 1,10
        PFLAG(I)= IVAL(I)
 105  CONTINUE
C
C     pflag for acidph is same as for phcarb (not input separately)
      PFLAG(11)= PFLAG(10)
C
      PIVL= IVAL(11)
      PYREND= IVAL(12)
C
C     set printout levels for active sections to 6
C     and find the minimum level
      RCHPFG= 6
      DO 110 I= 1,11
        IF (ASVEC(I).EQ.0) THEN
          PFLAG(I)= 6
        END IF
        JLKDUM= PFLAG(I)
        RCHPFG= MIN0(RCHPFG,JLKDUM)
 110  CONTINUE
C
      IF (RCHPFG.EQ.2) THEN
C       check pivl for validity
        PDELT= PIVL*NDELT
        IF (MOD(I1440,PDELT).NE.0) THEN
C         error - printout frequency, as implied by
C         pivl, must be an integer fraction of a day
          CALL OMSTI (OPTNO)
          CALL OMSTI (PDELT)
          SGRP = 30
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF
C
C     process table - type gen-info
      CALL ITABLE
     I             (I3,I1,I11,I1,
     M              IVAL)
      DO 140 J= 1,5
        RCHID(J)= IVAL(J)
 140  CONTINUE
C
      NEXITS= IVAL(6)
      DO 150 J= 1,4
        UNIT(J+1)= IVAL(J+6)
 150  CONTINUE
C
C     check output files - if not open,
C     then open them with a standard name
      DO 160 J= 4,5
        IF (UNIT(J) .GT. 0) THEN
          CALL HSCKFL
     I                (UNIT(J))
        END IF
 160  CONTINUE
C
      LKFG= IVAL(11)
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2050)
      END IF
C
      RETURN
      END

C
C
C
      SUBROUTINE   RCHRES
     I                    (STIVL,WIDTH,FSTCAL)
C
C     + + + PURPOSE + + +
C     Simulate hydrological and/or water quality processes for a
C     reach/mixed reservoir for one INSPAN.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    STIVL,WIDTH,FSTCAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIVL  - of inpad
C     WIDTH  - inpad width
C     FSTCAL - flag indicating first interval of run
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION RGEN2 + + +
      INCLUDE    'crhge.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IDELT
      CHARACTER*6 OPTYP
C
C     + + + EXTERNALS + + +
      EXTERNAL    RPTOT,ADDTIM,SPECL,HYDR,ADCALC,CONS,HTRCH,SEDTRN,
     $            GQUAL,RQUAL,ACIDPH,RBAROT,RPRINT,UPQUAN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        OPTYP/'RCHRES'/
C
C     + + + END SPECIFICATIONS + + +
C
      IVL  = STIVL- 1
      IVL1 = STIVL
      IDELT= DELT
C
      IF (STIVL .EQ. 1) THEN
C       put initial values of point-valued time series into INPAD
        CALL RPTOT
      END IF
C
C     time loop
      DO 10 IVL= STIVL,WIDTH+ STIVL- 1
        IVL1= IVL+ 1
        SPIVL= SPIVL+ 1
C       increment time and set time-related flags
        CALL ADDTIM
     I             (IDELT,NDAY,PIVL,PYREND,
     M              DATIM,PIVLNO,
     O              NDAYS,NXTMON,HRFG,DAYFG,EDAYFG,
     O              EMONFG,EPYRFG)
C       hour and day flags are always set on in first interval, to
C       force calculation of intermittently computed values
        IF (STFG .EQ. 1) THEN
C         first interval of run
          STFG = 0
          HRFG = 1
          DAYFG= 1
        END IF
C
        IF (SPAFP .GT. 0 .AND. SPAFP .LE. SPAKND) THEN
C         special actions are being taken and there is at least one left
          CALL SPECL (OPTYP,RCHNO,SPAKND,SPOPNO,DATIM,MESSU,SPIVL,
     M                SPAFP)
        END IF
C
C       perform the simulation
        IF (HYDRFG .EQ. 1) THEN
          CALL HYDR
        END IF
C
        IF (ADFG .EQ. 1) THEN
          CALL ADCALC
C
          IF (CONSFG .EQ. 1) THEN
            CALL CONS
          END IF
C
          IF (HTFG .EQ. 1) THEN
            CALL HTRCH
          END IF
C
          IF (SEDFG .EQ. 1) THEN
            CALL SEDTRN
          END IF
C
          IF (GQFG .EQ. 1) THEN
            CALL GQUAL
          END IF
C
          IF (OXFG .EQ. 1) THEN
            CALL RQUAL
          END IF
C
          IF (ACIDFG .EQ. 1) THEN
            CALL ACIDPH
     I                 (FSTCAL)
          END IF
        END IF
C
C       output time series
        CALL RPTOT
        CALL RBAROT
C
C       handle flux accumulation, printout
        IF (RCHPFG .LT. 6) THEN
          CALL RPRINT
        END IF
C
C       update pipes for user-defined variable quantities
        CALL UPQUAN (SPIVL,SPOPNO)
C
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   RBAROT
C
C     + + + PURPOSE + + +
C     Place the current values of all bar-valued output
C     time series in the INPAD.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION RGEN2 + + +
      INCLUDE  'crhge.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL HYDRRB,CONSRB,HTRB,SEDRB,GQRB,OXRB,NUTRB,
     $         PLKRB,PHRB,ACIDRB
C
C     + + + END SPECIFICATIONS + + +
C
      IF (HYDRFG .EQ. 1) THEN
        CALL HYDRRB
      END IF
C
      IF (CONSFG .EQ. 1) THEN
        CALL CONSRB
      END IF
C
      IF (HTFG .EQ. 1) THEN
        CALL HTRB
      END IF
C
      IF (SEDFG .EQ. 1) THEN
        CALL SEDRB
      END IF
C
      IF (GQFG .EQ. 1) THEN
        CALL GQRB
      END IF
C
      IF (OXFG .EQ. 1) THEN
        CALL OXRB
C
        IF (NUTFG .EQ. 1) THEN
          CALL NUTRB
C
          IF (PLKFG .EQ. 1) THEN
            CALL PLKRB
C
            IF (PHFG .EQ. 1) THEN
              CALL PHRB
            END IF
          END IF
        END IF
      END IF
C
      IF (ACIDFG .EQ. 1) THEN
        CALL ACIDRB
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   RCHACC
     I                    (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for printout.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION RGEN2 + + +
      INCLUDE    'crhge.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   HYDACC,CONACC,HTACC,SEDACC,GQACC,OXACC,
     $           NUTACC,PLKACC,PHACC,ACIACC
C
C     + + + END SPECIFICATIONS + + +
C
      IF (HYDRFG .EQ. 1 .AND. PFLAG(1) .LT. 6) THEN
C       section hydr is active and producing printout
        CALL HYDACC
     I             (FRMROW,TOROW)
      END IF
C
C     section ADCALC has no printout fluxes
      IF (CONSFG .EQ. 1 .AND. PFLAG(3) .LT. 6) THEN
C       section cons is active and producing printout
        CALL CONACC
     I             (FRMROW,TOROW)
      END IF
C
      IF (HTFG .EQ. 1 .AND. PFLAG(4) .LT. 6) THEN
C       section htrch is active and producing printout
        CALL HTACC
     I            (FRMROW,TOROW)
      END IF
C
      IF (SEDFG .EQ. 1 .AND. PFLAG(5) .LT. 6) THEN
C       section sedtrn is active and producing printout
        CALL SEDACC
     I             (FRMROW,TOROW)
      END IF
C
      IF (GQFG .EQ. 1 .AND. PFLAG(6) .LT. 6) THEN
C       section gqual is active and producing printout
        CALL GQACC
     I            (FRMROW,TOROW)
      END IF
C
      IF (OXFG .EQ. 1) THEN
        IF (PFLAG(7) .LT. 6) THEN
C         section oxrx is active and producing printout
          CALL OXACC
     I              (FRMROW,TOROW)
        END IF
C
        IF (NUTFG .EQ. 1) THEN
          IF (PFLAG(8) .LT. 6) THEN
C           section nutrx is active and producing printout
            CALL NUTACC
     I                 (FRMROW,TOROW)
          END IF
C
          IF (PLKFG .EQ. 1) THEN
            IF (PFLAG(9) .LT. 6) THEN
C             section plank is active and producing printout
              CALL PLKACC
     I                   (FRMROW,TOROW)
            END IF
C
            IF (PHFG .EQ. 1 .AND. PFLAG(10) .LT. 6) THEN
C             section phcarb is active and producing printout
              CALL PHACC
     I                  (FRMROW,TOROW)
            END IF
          END IF
        END IF
      END IF
C
      IF (ACIDFG .EQ. 1 .AND. PFLAG(11) .LT. 6) THEN
C       section acidph is active and producing output
        CALL ACIACC
     I             (FRMROW,TOROW)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   RCHPRT
     I                    (UNITFG,LEV,PRINTU)
C
C     + + + PURPOSE + + +
C     Perform printout and continuity checks for module RCHRES.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV,PRINTU,UNITFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION RGEN2 + + +
      INCLUDE     'crhge.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL        FACTA,FACTB
      CHARACTER*4 FLUXID,RQFLID(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL    HYDPRT,CONPRT,HTPRT,SEDPRT,GQPRT,OXPRT,NUTPRT,
     $            PLKPRT,PHPRT,ACIPRT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        RQFLID /'LBS ','KG  '/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (HYDRFG .EQ. 1 .AND. PFLAG(1) .LE. LEV) THEN
C       section hydr is active and produces printout at this level
        CALL HYDPRT
     I             (UNITFG,LEV,PRINTU)
      END IF
C
      IF (CONSFG .EQ. 1 .AND. PFLAG(3) .LE. LEV) THEN
C       section cons is active and produces printout at this level
        CALL CONPRT
     I             (LEV,PRINTU)
      END IF
C
      IF (HTFG .EQ. 1 .AND. PFLAG(4) .LE. LEV) THEN
C       section htrch is active and produces printout at this level
        CALL HTPRT
     I            (UNITFG,LEV,PRINTU)
      END IF
C
      IF (SEDFG .EQ. 1 .AND. PFLAG(5) .LE. LEV) THEN
C       section sedtrn is active and produces printout at this level
        CALL SEDPRT
     I             (UNITFG,LEV,PRINTU)
      END IF
C
      IF (GQFG .EQ. 1 .AND. PFLAG(6) .LE. LEV) THEN
C       section gqual is active and produces printout at this level
        CALL GQPRT
     I            (LEV,PRINTU)
      END IF
C
      IF (OXFG .EQ. 1) THEN
C       assign values to parameters used for conversion from
C       internal to external units for section rqual - assign
C       value to fluxid for printout
        IF (UNITFG .EQ. 1) THEN
C         printout is in english system
          IF (UUNITS .EQ. 1) THEN
C           english to english
            FACTA= 6.23E-05
            FACTB= 0.0
          ELSE
C           metric to english
            FACTA= 2.2E-03
            FACTB= 0.0
          END IF
          FLUXID= RQFLID(1)
        ELSE
C         printout is in metric system
          IF (UUNITS .EQ. 1) THEN
C           english to metric
            FACTA= 2.83E-05
            FACTB= 0.0
          ELSE
C           metric to english
            FACTA= 1.0E-03
            FACTB= 0.0
          END IF
          FLUXID= RQFLID(2)
        END IF
C
        IF (PFLAG(7) .LE. LEV) THEN
C         section oxrx is active and produces printout at this level
          CALL OXPRT
     I              (LEV,PRINTU,FACTA,FACTB,FLUXID)
        END IF
C
        IF (NUTFG .EQ. 1) THEN
          IF (PFLAG(8) .LE. LEV) THEN
C           section nutrx is active and produces printout at this
C           level
            CALL NUTPRT
     I                 (LEV,PRINTU,FACTA,FACTB,FLUXID)
          END IF
C
          IF (PLKFG .EQ. 1) THEN
            IF (PFLAG(9) .LE. LEV) THEN
C             section plank is active and produces printout at this
C             level
              CALL PLKPRT
     I                      (LEV,PRINTU,FACTA,FACTB,FLUXID)
            END IF
C
            IF (PHFG .EQ. 1 .AND. PFLAG(10) .LE. LEV) THEN
C             section phcarb is active and produces printout at this
C             level
              CALL PHPRT
     I                     (LEV,PRINTU,FACTA,FACTB,FLUXID)
            END IF
          END IF
        END IF
      END IF
C
      IF (ACIDFG .EQ. 1 .AND. PFLAG(11) .LE. LEV) THEN
C         section acidph is active and producing output at this level
        CALL ACIPRT
     I                 (UNITFG,LEV,PRINTU)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   RCHRST
     I                    (LEV)
C
C     + + + PURPOSE + + +
C     Reset flux accumulators and state variables used in material
C     balance checks.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION RGEN2 + + +
      INCLUDE    'crhge.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   HYDRST,CONRST,HTRST,SEDRST,GQRST,OXRST,NUTRST,
     $           PLKRST,PHRST,ACIRST
C
C     + + + END SPECIFICATIONS + + +
C
      IF (HYDRFG .EQ. 1 .AND. PFLAG(1) .LT. 6) THEN
C       section hydr is active and producing printout
        CALL HYDRST
     I             (LEV)
      END IF
C
C     section ADCALC has no printout fluxes
      IF (CONSFG .EQ. 1 .AND. PFLAG(3) .LT. 6) THEN
C       section cons is active and producing printout
        CALL CONRST
     I             (LEV)
      END IF
C
      IF (HTFG .EQ. 1 .AND. PFLAG(4) .LT. 6) THEN
C       section htrch is active and producing printout
        CALL HTRST
     I            (LEV)
      END IF
C
      IF (SEDFG .EQ. 1 .AND. PFLAG(5) .LT. 6) THEN
C       section sedtrn is active and producing printout
        CALL SEDRST
     I             (LEV)
      END IF
C
      IF (GQFG .EQ. 1 .AND. PFLAG(6) .LT. 6) THEN
C       section gqual is active and producing printout
        CALL GQRST
     I            (LEV)
      END IF
C
      IF (OXFG .EQ. 1) THEN
        IF (PFLAG(7) .LT. 6) THEN
C         section oxrx is active and producing printout
          CALL OXRST
     I              (LEV)
        END IF
C
        IF (NUTFG .EQ. 1) THEN
          IF (PFLAG(8) .LT. 6) THEN
C           section nutrx is active and producing printout
            CALL NUTRST
     I                 (LEV)
          END IF
C
          IF (PLKFG .EQ. 1) THEN
            IF (PFLAG(9) .LT. 6) THEN
C             section plank is active and producing printout
              CALL PLKRST
     I                   (LEV)
            END IF
C
            IF (PHFG .EQ. 1 .AND. PFLAG(10) .LT. 6) THEN
C             section phcarb is active and producing printout
              CALL PHRST
     I                  (LEV)
            END IF
          END IF
        END IF
      END IF
C
      IF (ACIDFG .EQ. 1 .AND. PFLAG(11) .LT. 6) THEN
C       section acidph is active and producing output
        CALL ACIRST
     I             (LEV)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   RPRINT
C
C     + + + PURPOSE + + +
C     Accumulate fluxes, produce printed output and perform
C     materials balance checks for RCHRES module.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION RGEN2 + + +
      INCLUDE    'crhge.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    EXDAT(5),I1,I2,I3,I4,I5,J,PRINTU,UNITFG
C
C     + + + EXTERNALS + + +
      EXTERNAL   RCHACC,EXDATE,RCHPRT,RCHRST
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('1',//,' REACH/MIXED-RESERVOIR NO.',I4,6X,20X,5A4,6X,
     $        'REPORT FOR',I4,' INTERVALS ENDING ',I4,'/',I2,'/',
     $        I2,' ',I2,':',I2)
 2010 FORMAT ('1',//,' REACH/MIXED-RESERVOIR NO.',I4,6X,20X,5A4,30X,
     $        'REPORT FOR DAY ',I4,'/',I2,'/',I2)
 2020 FORMAT ('1',//,' REACH/MIXED-RESERVOIR NO.',I4,6X,20X,5A4,31X,
     $        'REPORT FOR MONTH ',I4,'/',I2)
 2030 FORMAT ('1',//,' REACH/MIXED-RESERVOIR NO.',I4,6X,20X,5A4,16X,
     $        'REPORT FOR PRINTOUT YEAR ENDING ',I4,'/',I2)
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      I2= 2
      I3= 3
      I4= 4
      I5= 5
C
C     flux accumulation
      IF (RCHPFG .EQ. 2) THEN
C       some printout at the pivl level is being produced, so
C       accumulate fluxes to this level
        CALL RCHACC
     I             (I1,I2)
      END IF
C
C     always accumulate to the daily level
      CALL RCHACC
     I           (I1,I3)
C
      IF (EDAYFG .EQ. 1) THEN
C       it's the last interval of the day - accumulate daily
C       fluxes to the month level
        CALL RCHACC
     I             (I3,I4)
C
        IF (EMONFG .EQ. 1) THEN
C         it's the last interval of the month - accumulate
C         monthly fluxes to the year level
          CALL RCHACC
     I               (I4,I5)
        END IF
      END IF
C
C     printout and continuity check
      DO 110 UNITFG= 1,2
C
        PRINTU= PUNIT(UNITFG)
        IF (PRINTU .NE. 0) THEN
C         printout is required in this set of external units.
C         unitfg= 1 for english, 2 for metric.  printu is the
C         fortran logical unit no. to be used for printout
          IF (PIVLNO .EQ. PIVL .AND. RCHPFG .EQ. 2) THEN
C           it's time to handle any pivl level printout, and some
C           is required
C           convert hour and minute fields in date/time
C           to external format
            CALL EXDATE
     I                 (DATIM,
     O                  EXDAT)
            WRITE (PRINTU,2000)  RCHNO, RCHID, PIVL, EXDAT
            CALL RCHPRT
     I                 (UNITFG,I2,PRINTU)
          END IF
C
          IF (EDAYFG .EQ. 1) THEN
            IF (RCHPFG .LE. 3) THEN
C             it's time to handle daily printout
              WRITE (PRINTU,2010)  RCHNO, RCHID, (DATIM(J),J=1,3)
              CALL RCHPRT
     I                   (UNITFG,I3,PRINTU)
            END IF
C
            IF (EMONFG .EQ. 1) THEN
              IF (RCHPFG .LE. 4) THEN
C               it's time to handle monthly printout
                WRITE (PRINTU,2020)  RCHNO, RCHID, (DATIM(J),J=1,2)
                CALL RCHPRT
     I                     (UNITFG,I4,PRINTU)
              END IF
C
              IF (EPYRFG .EQ. 1) THEN
C               it's time to handle yearly printout
                WRITE (PRINTU,2030)  RCHNO, RCHID, (DATIM(J),J=1,2)
                CALL RCHPRT
     I                     (UNITFG,I5,PRINTU)
              END IF
            END IF
          END IF
        END IF
 110  CONTINUE
C
C     reset flux accumulators and state variables used in material
C     balance checks
      IF (PIVLNO .EQ. PIVL .AND. RCHPFG .EQ. 2) THEN
C       reset any pivl level variables in use
        CALL RCHRST
     I             (I2)
      END IF
C
      IF (EDAYFG .EQ. 1) THEN
C       reset any daily variables in use
        CALL RCHRST
     I             (I3)
C
        IF (EMONFG .EQ. 1) THEN
C         reset any monthly variables in use
          CALL RCHRST
     I               (I4)
C
          IF (EPYRFG .EQ. 1) THEN
C           reset any yearly variables in use
            CALL RCHRST
     I                 (I5)
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   RPTOT
C
C     + + + PURPOSE + + +
C     Place the current values of all point-valued output
C     time series in the INPAD.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION RGEN2 + + +
      INCLUDE  'crhge.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL HYDRRP,CONSRP,HTRP,SEDRP,GQRP,OXRP,NUTRP,
     $         PLKRP,PHRP,ACIDRP
C
C     + + + END SPECIFICATIONS + + +
C
      IF (HYDRFG .EQ. 1) THEN
        CALL HYDRRP
      END IF
C
      IF (CONSFG .EQ. 1) THEN
        CALL CONSRP
      END IF
C
      IF (HTFG .EQ. 1) THEN
        CALL HTRP
      END IF
C
      IF (SEDFG .EQ. 1) THEN
        CALL SEDRP
      END IF
C
      IF (GQFG .EQ. 1) THEN
        CALL GQRP
      END IF
C
      IF (OXFG .EQ. 1) THEN
        CALL OXRP
C
        IF (NUTFG .EQ. 1) THEN
          CALL NUTRP
C
          IF (PLKFG .EQ. 1) THEN
            CALL PLKRP
C
            IF (PHFG .EQ. 1) THEN
              CALL PHRP
            END IF
          END IF
        END IF
      END IF
C
      IF (ACIDFG .EQ. 1)  THEN
        CALL ACIDRP
      END IF
C
      RETURN
      END
