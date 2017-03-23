C
C
C
      SUBROUTINE PIQUAL
     I                  (MESSFL,OUTLEV,
     M                   ECOUNT)
C
C     + + + PURPOSE + + +
C     Process input for section iqual
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,ECOUNT,OUTLEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     ECOUNT - count of run interp errors
C     OUTLEV - run interp output level
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IQUAL1 + + +
      INCLUDE    'ciliq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I1,I2,I4,IVAL(11),J,N,I,SCLU,SGRP,RETCOD
      REAL         RVAL(12),SQOLIM,R0
      CHARACTER*12 CSTR
C
C     + + + EQUIVALENCES + + +
      CHARACTER*1  CSTR1(12)
      EQUIVALENCE (CSTR,CSTR1)
C
C     + + + EXTERNALS + + +
      EXTERNAL     ITABLE,RTABLE,OMSG,OMSTC,ZIPR,MDATBL
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION IQUAL')
 2010 FORMAT (3A4)
 2040 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION IQUAL')
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      R0 = 0.0
C
      SCLU= 326
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
C     initialize month-data input
      I= 120
      CALL ZIPR (I,R0,
     O           IQAFXM)
      CALL ZIPR (I,R0,
     O           IQACNM)
C
C     initialize atmospheric deposition fluxes
      I= 50
      CALL ZIPR (I,R0,
     O           IQCF3)
      CALL ZIPR (I,R0,
     O           IQCF4)
C
C     find number of quality constituents - table-type nquals
      I2= 30
      I4= 1
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             IVAL)
      NQUAL= IVAL(1)
C
C     get atmospheric deposition flags - table-type iql-ad-flags
      I2= 31
      I4= 20
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             IQADFG)
C
C     read in month-data tables where necessary
      DO 50 J= 1, NQUAL
        N= 2*(J- 1)+ 1
        IF (IQADFG(N) .GT. 0) THEN
C         monthly flux must be read
          CALL MDATBL
     I                (IQADFG(N),
     O                 IQAFXM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
          IF (UUNITS .EQ. 1) THEN
C           convert from qty/ac.day to qty/ac.ivl
            DO 10 I= 1, 12
              IQAFXM(I,J)= IQAFXM(I,J)*DELT60/24.0
 10         CONTINUE
          ELSE IF (UUNITS .EQ. 2) THEN
C           convert from qty/ha.day to qty/ac.ivl (not qty/ha.ivl)
            DO 20 I= 1, 12
              IQAFXM(I,J)= IQAFXM(I,J)*0.4047*DELT60/24.0
 20         CONTINUE
          END IF
        END IF
        IF (IQADFG(N+1) .GT. 0) THEN
C         monthly ppn conc must be read
          CALL MDATBL
     I                (IQADFG(N+1),
     O                 IQACNM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
          IF (UUNITS .EQ. 1) THEN
C           convert from qty/ft3 to qty/ac.in
            DO 30 I= 1, 12
              IQACNM(I,J)= IQACNM(I,J)*3630.0
 30         CONTINUE
          ELSE IF (UUNITS .EQ. 2) THEN
C           convert from qty/L to qty/ac.in (not qty/ha.in)
            DO 40 I= 1, 12
              IQACNM(I,J)= IQACNM(I,J)*102833.0
 40         CONTINUE
          END IF
        END IF
 50   CONTINUE
C
C     initialize counters
      NQSD= 0
      NQOF= 0
C     initialize outflow and washoff arrays
      DO 60 J= 1,7
        SOQS(J) = 0.0
        SOQO(J) = 0.0
 60   CONTINUE
C
      DO 90 N= 1,NQUAL
C       get id's and flags - table-type qual-props
        I2= 32
        I4= 8
        CALL ITABLE (I2,N,I4,UUNITS,
     M               IVAL)
        DO 70 J= 1,3
          QUALID(J,N)= IVAL(J)
 70     CONTINUE
        QTYID(1,N)= IVAL(4)
C
        IF (IVAL(5).GT.0) THEN
C         this is a qualsd
          NQSD= NQSD+ 1
          IF (NQSD.GT.7) THEN
C           error - too many sediment associated quality constituents
            WRITE (CSTR,2010) (QUALID(J,N),J=1,3)
C           is this the right variable to write ???
            I = 12
            CALL OMSTC (I,CSTR1)
            SGRP= 1
            CALL OMSG (MESSU,MESSFL,SCLU,SGRP,
     M                 ECOUNT)
            NQSD= 7
          END IF
          QSDFP(N)    = NQSD
          VPFWFG(NQSD)= IVAL(6)
        ELSE
          QSDFP(N)= 0
        END IF
C
        IF (IVAL(7).NE.0) THEN
C         this is a qualof
          NQOF= NQOF+ 1
          IF (NQOF.GT.7) THEN
C           error - too many overland-flow-associated
C           quality constituents
            WRITE (CSTR,2010) (QUALID(J,N),J=1,3)
C           is this the right variable to write ???
            I= 12
            CALL OMSTC (I,CSTR1)
            SGRP= 2
            CALL OMSG (MESSU,MESSFL,SCLU,SGRP,
     M                 ECOUNT)
            NQOF= 7
          END IF
          QSOFP(N)= NQOF
          IF (IVAL(7) .LT. 0) THEN
C           special case for ches bay - allow constant conc qualof
            QSOFP(N) = -NQOF
          END IF
          VQOFG(NQOF)= IVAL(8)
        ELSE
          QSOFP(N)= 0
C
          J= 2*(N- 1)+ 1
          IF ( (IQADFG(J) .NE. 0) .OR. (IQADFG(J+1) .NE. 0) ) THEN
C           error - non-qualof cannot have atmospheric deposition
            WRITE (CSTR,2010) (QUALID(I,N),I=1,3)
            I= 12
            CALL OMSTC (I,CSTR1)
            SGRP= 3
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            IQADFG(J)= 0
            IQADFG(J+1)= 0
          END IF
        END IF
C
C       read in storage on surface and values for any
C       parameters which do not vary seasonally - qual-input
        I2= 33
        I4= 5
        CALL RTABLE (I2,N,I4,UUNITS,
     M               RVAL)
C
        IF (QSDFP(N).GT.0) THEN
          POTFW(NQSD)= RVAL(2)
        END IF
C
        IF (QSOFP(N).GT.0) THEN
          SQO(NQOF)   = RVAL(1)
          ACQOP(NQOF) = RVAL(3)
          SQOLIM      = RVAL(4)
C         compute removal rate
          REMQOP(NQOF)= ACQOP(NQOF)/SQOLIM
          WSFAC(NQOF) = 2.30/RVAL(5)
        END IF
C
        IF (QSOFP(N) .LT. 0) THEN
C         special case for ches bay - allow constant conc qualof
C         this converts units from mg/l to lb/ac/in
          ACQOP(NQOF) = RVAL(3)*0.2266
          SQO(NQOF)   = 0.0
        END IF
C
        IF (QSDFP(N).GT.0) THEN
          IF (VPFWFG(NQSD).EQ.1) THEN
C           get monthly values of washoff potency
C           factor - table-type mon-potfw
            I2= 34
            I4= 12
            CALL RTABLE (I2,NQSD,I4,UUNITS,
     M                   POTFWM(1,NQSD))
          END IF
        END IF
C
        IF (QSOFP(N).GT.0) THEN
          IF (VQOFG(NQOF).EQ.1) THEN
C           get monthly values of accumulation rates -
C           table-type mon-accum
            I2= 35
            I4= 12
            CALL RTABLE (I2,NQOF,I4,UUNITS,
     M                   ACQOPM(1,NQOF))
C           get monthly values of limiting storage
C           table-type mon-sqolim
            I2=36
            I4=12
            CALL RTABLE (I2,NQOF,I4,UUNITS,
     M                   RVAL)
C           calculate monthly values of removal rate
            DO 80 J = 1,12
              REMQOM(J,NQOF)= ACQOPM(J,NQOF)/RVAL(J)
 80         CONTINUE
          END IF
        END IF
 90   CONTINUE
C
      IF (OUTLEV.GT.1) THEN
C       end processing message
        WRITE (MESSU,2040)
      END IF
C
      RETURN
      END
C
C     4.2(2).6
C
      SUBROUTINE IQUAL
C
C     + + + PURPOSE + + +
C     ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IQUAL2 + + +
      INCLUDE   'ciliq.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + + +
      INTEGER   FP,N,QOFP,QSFP,J
      REAL      SUROQO,SUROQS,ATDPFX,ATDPCN
C
C     + + + EXTERNALS + + +
      EXTERNAL WASHSD,WASHOF
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
C     Simulate washoff of quality constituents (other than solids,
C     Heat, dox, and co2) using simple relationships with solids
C     And/or water yield
C
C     nqual is the number of constituents being simulated
      DO 140 N= 1,NQUAL
C       simulate constituent n
C
C       simulate by association with solids
        QSFP= QSDFP(N)
        IF (QSFP.GT.0) THEN
C         constituent n is simulated by association with solids
C         the value of qsfp refers to the set of solids
C         associated parameters to use
C
C         get input time series
          IF (SLDFG.EQ.0) THEN
C           read time series from inpad
            SOSLD= PAD(SOSDFP+IVL1)
          ELSE
C           sosld is available from solids
          END IF
C
          CALL WASHSD (DAYFG,VPFWFG(QSFP),SOSLD,POTFWM(1,QSFP),
     I                 MON,NXTMON,DAY,NDAYS,
     M                 POTFW(QSFP),
     O                 SOQS(QSFP))
C
          SUROQS= SOQS(QSFP)
C
        ELSE
          SUROQS= 0.0
C
        END IF
C
C       simulate by association with overland flow
        QOFP= QSOFP(N)
        IF (QOFP.NE.0) THEN
C         constituent n is simulated by association with overland
C         flow
C         the value of qofp refers to the set of overland flow
C         associated parameters to use
C
C         get input time series
          IF (IWATFG.EQ.0) THEN
C           read time series from inpad
            SURO= PAD(SOFP+IVL1)
          ELSE
C           suro is available from iwater
          END IF
          PREC= PAD(PRECFP+IVL1)
C
C         initialize atmospheric deposition
          ATDPFX= 0.0
          ATDPCN= 0.0
          J= (N-1)*2+ 1
          IF (IQADFG(J) .LE. -1) THEN
C           flux time series
            ATDPFX= PAD(IQAFFP(N)+IVL1)
          END IF
          IF (IQADFG(J+1) .LE. -1) THEN
C           conc time series
            ATDPCN= PAD(IQACFP(N)+IVL1)
          END IF
C
          IF (QOFP .GT. 0) THEN
C           standard qualof simulation
            CALL WASHOF (DAYFG,VQOFG(QOFP),ACQOPM(1,QOFP),
     I                   REMQOM(1,QOFP),SURO,WSFAC(QOFP),
     I                   SOQOCX(QOFP),MON,NXTMON,DAY,NDAYS,
     I                   PREC,IQADFG(J),IQADFG(J+1),IQAFXM(1,N),
     I                   IQACNM(1,N),ATDPFX,ATDPCN,
     M                   ACQOP(QOFP),REMQOP(QOFP),SQO(QOFP),
     O                   SOQO(QOFP),SOQOC(QOFP),IQADDR(N),IQADWT(N))
C
          ELSE
C           special case for ches bay - constant conc of qualof
C           input value of acqop = mg/l and soqo = lb/ac
C           note - this assumes that qty = lb
C           note - acqop is converted to (lb/ac/in) in the run interpeter
C           the computed concs (soqoc and soqc) are reported in qty/ft3
C           the internal units are lb/ac/in and external units are lb/ft3
C           the storage (sqo) is reported as zero
            SOQO(ABS(QOFP))  = SURO * ACQOP(ABS(QOFP))
            SOQOC(ABS(QOFP)) = ACQOP(ABS(QOFP))
            SQO(ABS(QOFP))   = 0.0
          END IF
C
          SUROQO= SOQO(ABS(QOFP))
C
        ELSE
          SUROQO= 0.0
C
        END IF
C
C       sum outflows of constituent n from the land surface
        SOQUAL(N) = SUROQS+SUROQO
C
C       compute the concentration if required - units are
C       qty/acre-in.
        FP= SOQCFP(N)
        IF (FP.GT.0) THEN
C
          IF (IWATFG.EQ.0) THEN
C           read time series from inpad
            SURO= PAD(SOFP+IVL1)
          ELSE
C           suro is available from iwater
          END IF
C
          IF (SURO.GT.0.0) THEN
            SOQC(N)= SOQUAL(N)/SURO
          ELSE
            SOQC(N)= -1.0E30
          END IF
C
        END IF
C
 140  CONTINUE
C
      RETURN
      END
C
C     4.2(2).9.1.5
C
      SUBROUTINE IQACC
     I                 (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section iqual
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IQUAL2 + + +
      INCLUDE    'ciliq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES +
      INTEGER    N
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N= 1,7
        IQCF1(N,TOROW)= IQCF1(N,TOROW)+ IQCF1(N,FRMROW)
        IQCF2(N,TOROW)= IQCF2(N,TOROW)+ IQCF2(N,FRMROW)
 10   CONTINUE
C
      DO 20 N= 1,10
        IQCF3(N,TOROW)= IQCF3(N,TOROW)+ IQCF3(N,FRMROW)
        IQCF4(N,TOROW)= IQCF4(N,TOROW)+ IQCF4(N,FRMROW)
 20   CONTINUE
C
      RETURN
      END
C
C     4.2(2).8.4
C
      SUBROUTINE IQALIB
C
C     + + + PURPOSE + + +
C     Handle section iqual
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IQUAL2 + + +
      INCLUDE 'ciliq.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N,QOFP,QSFP
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N= 1,NQUAL
C       sediment associated constituents
        QSFP= QSDFP(N)
        IF (QSFP.GT.0) THEN
C         this qual is a qualsd
          IF (SOQSFP(QSFP).GE.1) THEN
            PAD(SOQSFP(QSFP)+IVL1)= SOQS(QSFP)
          END IF
        END IF
C
C       surface runoff-associated quality constituents
        QOFP= ABS(QSOFP(N))
        IF (QOFP.GT.0) THEN
C         this qual is a qualof
          IF (SOQOFP(QOFP).GE.1) THEN
            PAD(SOQOFP(QOFP)+IVL1)= SOQO(QOFP)
          END IF
          IF (SOQOCX(QOFP).GE.1) THEN
            PAD(SOQOCX(QOFP)+IVL1)= SOQOC(QOFP)
          END IF
          IF (IQADDX(N) .GE. 1) THEN
            PAD(IQADDX(N)+IVL1)= IQADDR(N)
          END IF
          IF (IQADWX(N) .GE. 1) THEN
            PAD(IQADWX(N)+IVL1)= IQADWT(N)
          END IF
        END IF
C
C       output of global variables
        IF (SOQFP(N).GE.1)   PAD(SOQFP(N) +IVL1)= SOQUAL(N)
        IF (SOQCFP(N).GE.1)  PAD(SOQCFP(N)+IVL1)= SOQC(N)
 10   CONTINUE
C
      RETURN
      END
C
C     4.2(2).7.4
C
      SUBROUTINE IQALIP
C
C     + + + PURPOSE + + +
C     Handle section iqual
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IQUAL2 + + +
      INCLUDE 'ciliq.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N,QOFP
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N= 1,NQUAL
C
        QOFP= ABS(QSOFP(N))
        IF (QOFP.NE.0) THEN
C         this qual is a qualof
          IF (SQOFP(QOFP).GE.1) PAD(SQOFP(QOFP)+IVL1)=SQO(QOFP)
        END IF
C
 10   CONTINUE
C
      RETURN
      END
C
C     4.2(2).9.2.6
C
      SUBROUTINE IQPRT
     I                 (UNITFG,LEV,PRINTU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units,
C     and produce printout.
C     Note: local arrays have
C     identical sizes and structures to the corresponding arrays in
C     the osv apart from dropping the dimension lev for fluxes
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV,PRINTU,UNITFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IQUAL2 + + +
      INCLUDE    'ciliq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    J,N,QOFP,QSFP,ADFG,M
      REAL       FACTA,PCFLX1,PCFLX2,PSOQAL,PSTAT1,PCFLX3,PCFLX4,PADTOT
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,1H ,'*** IQUAL ***')
 2010 FORMAT (/,1H ,'  FLUXES AND STORAGES',9X,'STORAGE:',12X,
     $        'OUTFLOW FLUXES:')
 2020 FORMAT (' ',30X,'FLOW ASSOC',14X,'SOLIDS      FLOW     TOTAL')
 2030 FORMAT (' ',31X,'CONSTITNT',10X,'ASSOCIATED     ASSOC')
 2040 FORMAT (' ',37X,'SQO',16X,'SOQS      SOQO    SOQUAL')
 2050 FORMAT (' ',2X,3A4,' (',A4,'/AC)',6X,1PG10.3,10X,3G10.3)
 2060 FORMAT (' ',2X,3A4,' (',A4,'/HA)',6X,1PG10.3,10X,3G10.3)
 2070 FORMAT (//,' ',2X,'ATMOSPHERIC DEPOSITION FLUXES OF FLOW-',
     $        'ASSOCATED CONSTITUENTS',//,' ',30X,'<------ DEPOSITION',
     $        ' FLUXES -------->',/,' ',2X,'CONSTITUENT',24X,'DRY',7X,
     $        'WET',10X,'TOTAL')
 2080 FORMAT (' ',2X,3A4,' (',A4,'/AC)',6X,1PG10.3,G10.3,5X,G10.3)
 2090 FORMAT (' ',2X,3A4,' (',A4,'/HA)',6X,1PG10.3,G10.3,5X,G10.3)
C
C     + + + END SPECIFICATIONS + + +
C
C     print headings on unit printu
      WRITE (PRINTU,2000)
      WRITE (PRINTU,2010)
      WRITE (PRINTU,2020)
      WRITE (PRINTU,2030)
      WRITE (PRINTU,2040)
C
C     assign values to parameter used for conversion from
C     internal to external units
      IF (UNITFG.EQ.1) THEN
C       english system
        FACTA= 1.0
      ELSE
C       metric system
        FACTA= 2.471
      END IF
C
      DO 10 N= 1,NQUAL
C
        QSFP= QSDFP(N)
        IF (QSFP.GT.0) THEN
C         constituent is simulated by association with solids
          PCFLX1= IQCF1(QSFP,LEV)*FACTA
        ELSE
          PCFLX1= 0.0
        END IF
C
        QOFP= ABS(QSOFP(N))
        IF (QOFP.GT.0) THEN
C         constituent is simulated by association with
C         overland flow
C         storage
          PSTAT1= SQO(QOFP)*FACTA
C         flux
          PCFLX2= IQCF2(QOFP,LEV)*FACTA
        ELSE
          PSTAT1= 0.0
          PCFLX2= 0.0
C
        END IF
C
C       sum outflows of constituent from the land surface
        PSOQAL= PCFLX1+ PCFLX2
C
        IF (UNITFG.EQ.1) THEN
C         english system
          WRITE (PRINTU,2050)  (QUALID(J,N),J=1,3),
     $      QTYID(1,N), PSTAT1, PCFLX1, PCFLX2, PSOQAL
        ELSE
C         metric system
          WRITE (PRINTU,2060)  (QUALID(J,N),J=1,3),
     $      QTYID(1,N), PSTAT1, PCFLX1, PCFLX2, PSOQAL
        END IF
C
 10   CONTINUE
C
      ADFG= 0
      DO 20 N=1, NQUAL
        J= (N-1)*2+ 1
        IF ( (IQADFG(J) .NE. 0) .OR. (IQADFG(J+1) .NE. 0) ) THEN
          ADFG= 1
        END IF
 20   CONTINUE
C
      IF (ADFG .EQ. 1) THEN
C       there is atmospheric deposition to output
        WRITE (PRINTU,2070)
        DO 30 N= 1, NQUAL
          J= (N-1)*2+ 1
          IF ( (IQADFG(J) .NE. 0) .OR. (IQADFG(J+1) .NE. 0) ) THEN
            PCFLX3= IQCF3(N,LEV)*FACTA
            PCFLX4= IQCF4(N,LEV)*FACTA
            PADTOT= PCFLX3+ PCFLX4
            IF (UNITFG .EQ. 1) THEN
C             english units
              WRITE (PRINTU,2080) (QUALID(M,N),M=1,3),QTYID(1,N),
     #                             PCFLX3,PCFLX4,PADTOT
            ELSE
C             metric units
              WRITE (PRINTU,2090) (QUALID(M,N),M=1,3),QTYID(1,N),
     #                             PCFLX3,PCFLX4,PADTOT
            END IF
          END IF
 30     CONTINUE
      END IF
C
      RETURN
      END
C
C     4.2(2).9.3.5
C
      SUBROUTINE IQRST
     I                 (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators in section iqual
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IQUAL2 + + +
      INCLUDE    'ciliq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    N
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N= 1,7
        IQCF1(N,LEV)= 0.0
        IQCF2(N,LEV)= 0.0
 10   CONTINUE
C
      DO 20 N= 1,10
        IQCF3(N,LEV)= 0.0
        IQCF4(N,LEV)= 0.0
 20   CONTINUE
C
      RETURN
      END
C
C     4.2(2).6.2
C
      SUBROUTINE WASHOF
     I                  (DAYFG,VQOFG,ACQOPM,REMQOM,SURO,WSFAC,SOQOCX,
     I                   MON,NXTMON,DAY,NDAYS,PREC,ADFXFG,ADCNFG,
     I                   ADFXMN,ADCNMN,ADFLX,ADCNC,
     M                   ACQOP,REMQOP,SQO,
     O                   SOQO,SOQOC,ADFXFX,ADCNFX)
C
C     + + + PURPOSE + + +
C     Simulate accumulation of a quality constituent on the land
C     surface and its removal using a constant unit rate and by direct
C     washoff by overland flow
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,SOQOCX,VQOFG,ADFXFG,
     #           ADCNFG
      REAL       ACQOPM(12),ACQOP,REMQOM(12),REMQOP,SOQO,SOQOC,SQO,
     #           SURO,WSFAC,PREC,ADFXMN(12),ADCNMN(12),ADFLX,ADCNC,
     #           ADFXFX,ADCNFX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VQOFG  - ???
C     ACQOPM - ???
C     REMQOM - ???
C     SURO   - surface output
C     WSFAC  - ???
C     SOQOCX - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     PREC   - precipitation during current interval in inches
C     ADFXFG - flag indicating source of dry deposition flux
C              positive means monthly, -1 means time series,
C              0 means none
C     ADCNFG - flag indicating source of wet deposition flux
C              postive means monthly, -1 means time series,
C              0 means none
C     ADFXMN - monthly dry atmospheric deposition in qty/ac
C     ADCNMN - monthly wet atmospheric deposition in qty/ac.inch
C     ADFLX  - current dry atmospheric deposition in qty/ac
C     ADCNC  - current wet atmospheric deposition in qty/ac.inch
C     ACQOP  - ???
C     REMQOP - ???
C     SQO    - ???
C     SOQO   - ???
C     SOQOC  - ???
C     ADFXFX - actual dry atmospheric deposition flux for current interval
C     ADCNFX - actual wet atmospheric deposition flux for current interval
C
C     + + + LOCAL VARIABLES + + +
      REAL       DUMMY
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  EXP
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
C
        IF (VQOFG.EQ.1) THEN
C         accumulation rate of this quality constituent is allowed
C         to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate acqop between two values from the
C         monthly array acqopm(12) for this overland flow associated
C         quality constituent (no. qofp)
          ACQOP= DAYVAL(ACQOPM(MON),ACQOPM(NXTMON),DAY,NDAYS)
C         removal unit rate of this quality constituent is allowed
C         to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate remqop between two values from the
C         monthly array remqom(12) for this overland flow associated
C         quality constituent (no. qofp)
          REMQOP= DAYVAL(REMQOM(MON),REMQOM(NXTMON),DAY,NDAYS)
C
        END IF
C
C       update storage due to accumulation and removal which occurs
C       independent of runoff - units are qty/acre
        SQO= ACQOP+ SQO*(1.0- REMQOP)
C
      END IF
C
C     handle atmospheric deposition
C     dry deposition
      IF (ADFXFG .LE. -1) THEN
C       dry flux has been input as a time series
        ADFXFX= ADFLX
      ELSE IF (ADFXFG .GE. 1) THEN
C       dry flux is taken from monthly values
        ADFXFX= DAYVAL(ADFXMN(MON),ADFXMN(NXTMON),DAY,NDAYS)
      ELSE
        ADFXFX= 0.0
      END IF
C     wet deposition
      IF (ADCNFG .LE. -1) THEN
C       wet deposition concentration has been input as a time series
        ADCNFX= PREC*ADCNC
      ELSE IF (ADCNFG .GE. 1) THEN
C       wet deposition concentration is taken from monthly values
        ADCNFX= PREC*DAYVAL(ADCNMN(MON),ADCNMN(NXTMON),DAY,NDAYS)
      ELSE
        ADCNFX= 0.0
      END IF
C     update storage
      SQO= SQO+ ADFXFX+ ADCNFX
C
C     simulate washoff by overland flow - units are qty/acre-ivl
      IF (SURO.GT.0.0) THEN
C       there is overland flow
        IF (SQO.GT.0.0) THEN
C         there is some quality constituent (no. qofp) in storage;
C         washoff can occur
          DUMMY= 1.0-EXP(-SURO*WSFAC)
          SOQO = SQO*DUMMY
C
C         update storage of constituent - units are in qty/acre
          SQO=SQO- SOQO
        ELSE
C         no washoff load
          SOQO= 0.0
        END IF
C
      ELSE
        SOQO= 0.0
      END IF
C
C     compute and output concentration if required - units are qty/
C     acre-in.
      IF (SOQOCX.GE.1) THEN
        IF (SURO.GT.0.0) THEN
          SOQOC= SOQO/SURO
        ELSE
C         soqoc is undefined
          SOQOC= -1.0E30
        END IF
      END IF
C
      RETURN
      END
C
C     4.2(2).6.1
C
      SUBROUTINE WASHSD
     I                  (DAYFG,VPFWFG,SOSLD,POTFWM,MON,NXTMON,DAY,
     I                   NDAYS,
     M                   POTFW,
     O                   SOQS)
C
C     + + + PURPOSE + + +
C     Simulate washoff of a quality constituent from the land surface
C     by association with solids
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,VPFWFG
      REAL       POTFW,POTFWM(12),SOQS,SOSLD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VPFWFG - ???
C     SOSLD  - ???
C     POTFWM - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     POTFW  - ???
C     SOQS   - ???
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
C
        IF (VPFWFG.EQ.1) THEN
C         washoff potency factors are allowed to vary throughout the
C         year
C         interpolate for the daily value
C         linearly interpolate potfw between two values from the
C         monthly array potfwm(12) for this solids associated quality
C         constituent (no. qsfp)
          POTFW= DAYVAL(POTFWM(MON),POTFWM(NXTMON),DAY,NDAYS)
        ELSE
C         washoff potency factors do not vary throughout the year.
C         potfw value has been supplied by the run interpreter
        END IF
C
      END IF
C
C     associate with washoff of solids - units are qty/acre-ivl
C
C     if (sosld.eq.0.0) then
      IF ((ABS(SOSLD)).LE.0.0) THEN
        SOQS= 0.0
      ELSE
        SOQS= SOSLD*POTFW
      END IF
C
      RETURN
      END
