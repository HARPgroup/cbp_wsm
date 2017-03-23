C
C
C
      SUBROUTINE   PPQUAL
C
C     + + + PURPOSE + + +
C     Process input for section pqual
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PQUAL1 + + +
      INCLUDE    'cplpq.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I1,I2,I4,IVAL(16),J,N,I12,SGRP,SCLU,RETCOD,I
      REAL         RVAL(12),SQOLIM,R0,CVT
      CHARACTER*12 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(12)
C
C     + + + EXTERNALS + + +
      EXTERNAL   ITABLE,RTABLE,OMSG,OMSTC
      EXTERNAL   ZIPR,MDATBL
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION PQUAL')
 2040 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION PQUAL')
 2070 FORMAT (3A4)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
      R0  = 0.0
      I1  = 1
      I12 = 12
C
      IF (UUNITS .EQ. 1) THEN
C       english units - conversion from lb/ft3 to mg/l
        CVT= 6.238E-5
      ELSE
C       metric units - conversion from kg/l to mg/l
        CVT= 1.0E-6
      END IF
C
C     error/warn message cluster
      SCLU= 307
C
C     initialize month-data input
      I= 120
      CALL ZIPR (I,R0,
     O           PQAFXM)
      CALL ZIPR (I,R0,
     O           PQACNM)
C
C     initialize atmospheric deposition fluxes
      I= 50
      CALL ZIPR (I,R0,
     O           PQCF6)
      CALL ZIPR (I,R0,
     O           PQCF7)
C
C     find number of quality constituents - table-type nquals
      I2= 52
      I4= 1
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             IVAL)
      NQUAL= IVAL(1)
C
C     get atmospheric deposition flags - table-type pql-ad-flags
      I2= 53
      I4= 20
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             PQADFG)
C
C     read in month-data tables where necessary
      DO 50 J= 1, NQUAL
        N= 2*(J- 1)+ 1
        IF (PQADFG(N) .GT. 0) THEN
C         monthly flux must be read
          CALL MDATBL
     I                (PQADFG(N),
     O                 PQAFXM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
          IF (UUNITS .EQ. 1) THEN
C           convert from qty/ac.day to qty/ac.ivl
            DO 10 I= 1, 12
              PQAFXM(I,J)= PQAFXM(I,J)*DELT60/24.0
 10         CONTINUE
          ELSE IF (UUNITS .EQ. 2) THEN
C           convert from qty/ha.day to qty/ac.ivl (not qty/ha.ivl)
            DO 20 I= 1, 12
              PQAFXM(I,J)= PQAFXM(I,J)*0.4047*DELT60/24.0
 20         CONTINUE
          END IF
        END IF
        IF (PQADFG(N+1) .GT. 0) THEN
C         monthly ppn conc must be read
          CALL MDATBL
     I                (PQADFG(N+1),
     O                 PQACNM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
          IF (UUNITS .EQ. 1) THEN
C           convert from qty/ft3 to qty/ac.in
            DO 30 I= 1, 12
              PQACNM(I,J)= PQACNM(I,J)*3630.0
 30         CONTINUE
          ELSE IF (UUNITS .EQ. 2) THEN
C           convert from qty/L to qty/ac.in (not qty/ha.in)
            DO 40 I= 1, 12
              PQACNM(I,J)= PQACNM(I,J)*102833.0
 40         CONTINUE
          END IF
        END IF
 50   CONTINUE
C
C     initialize counters
      NQSD= 0
      NQOF= 0
      NQIF= 0
      NQGW= 0
C     initialize flux arrays
      DO 60 J= 1,7
        WASHQS(J)= 0.0
        SCRQS(J) = 0.0
        SOQO(J)  = 0.0
        IOQUAL(J)= 0.0
        AOQUAL(J)= 0.0
 60   CONTINUE
C
      DO 120 N= 1,NQUAL
C       get id's and flags - table-type qual-props
        I2= 54
        I4= 13
        CALL ITABLE (I2,N,I4,UUNITS,
     M               IVAL)
C
        DO 80 J= 1,3
          QUALID(J,N)= IVAL(J)
 80     CONTINUE
C
        QTYID(1,N)= IVAL(4)
C
        IF (IVAL(5) .GT. 0) THEN
C         this is a qualsd
          NQSD= NQSD+ 1
C
          IF (NQSD.GT.7) THEN
C           error - too many sediment associated quality constituents
            WRITE (CHSTR,2070) (QUALID(J,N),J=1,3)
            CALL OMSTC (I12,CHSTR1)
            SGRP= 1
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            NQSD= 7
          END IF
C
          QSDFP(N)    = NQSD
          VPFWFG(NQSD)= IVAL(6)
          VPFSFG(NQSD)= IVAL(7)
        ELSE
C         not sed related
          QSDFP(N)= 0
        END IF
C
        IF (IVAL(8).GT.0) THEN
C         this is a qualof
          NQOF= NQOF+ 1
C
          IF (NQOF.GT.7) THEN
C           error - too many overland-flow-associated quality constituents
            WRITE (CHSTR,2070) (QUALID(J,N),J=1,3)
            CALL OMSTC (I12,CHSTR1)
            SGRP= 2
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            NQOF= 7
          END IF
C
          QSOFP(N)   = NQOF
          VQOFG(NQOF)= IVAL(9)
        ELSE
C         not a qualof
          QSOFP(N)= 0
C
          J= 2*(N- 1)+ 1
          IF ( (PQADFG(J) .NE. 0) .OR. (PQADFG(J+1) .NE. 0) ) THEN
C           error - non-qualof cannot have atmospheric deposition
            WRITE (CHSTR,2070) (QUALID(I,N),I=1,3)
            CALL OMSTC (I12,CHSTR1)
            SGRP= 5
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            PQADFG(J)= 0
            PQADFG(J+1)= 0
          END IF
        END IF
C
        IF (IVAL(10).GT.0) THEN
C         this is a qualif
          NQIF= NQIF+ 1
C
          IF (NQIF.GT.7) THEN
C           error - too many interflow-associated quality constituents
            WRITE (CHSTR,2070) (QUALID(J,N),J=1,3)
            CALL OMSTC (I12,CHSTR1)
            SGRP= 3
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            NQIF= 7
          END IF
C
          QIFWFP(N)   = NQIF
          VIQCFG(NQIF)= IVAL(11)
        ELSE
C         not a qualif
          QIFWFP(N)= 0
        END IF
C
        IF (IVAL(12).GT.0) THEN
C         this is a qualgw
          NQGW= NQGW+ 1
C
          IF (NQGW.GT.7) THEN
C           error - too many groundwater-associated
C           quality constituents
            WRITE (CHSTR,2070) (QUALID(J,N),J=1,3)
            CALL OMSTC (I12,CHSTR1)
            SGRP= 4
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            NQGW= 7
          END IF
C
          QAGWFP(N)   = NQGW
          VAQCFG(NQGW)= IVAL(13)
        ELSE
C         not a qualgw
          QAGWFP(N)= 0
        END IF
C
C       read in storage on surface and values for any
C       parameters which do not vary seasonally - qual-input
        I2= 55
        I4= 8
        CALL RTABLE (I2,N,I4,UUNITS,
     M               RVAL)
C
        IF (QSDFP(N).GT.0) THEN
          POTFW(NQSD)= RVAL(2)
          POTFS(NQSD)= RVAL(3)
        END IF
C
        IF (QSOFP(N).GT.0) THEN
          SQO(NQOF)  = RVAL(1)
          ACQOP(NQOF)= RVAL(4)
          SQOLIM     = RVAL(5)
C
C         compute removal rate
          REMQOP(NQOF)= ACQOP(NQOF)/SQOLIM
          WSFAC(NQOF) = 2.30/RVAL(6)
        END IF
C
        IF (QIFWFP(N).GT.0) THEN
          IOQC(NQIF)= RVAL(7)
        END IF
C
        IF (QAGWFP(N).GT.0) THEN
          AOQC(NQGW)= RVAL(8)
        END IF
C
        IF (QSDFP(N).GT.0) THEN
C         allow optional no interpolation of potency factor
C         for chesapeake bay model
          IF (VPFWFG(NQSD).GT.0) THEN
C           get monthly vals of washoff pot factor - table-type mon-potfw
            I2= 56
            I4= 12
            CALL RTABLE (I2,NQSD,I4,UUNITS,
     M                   POTFWM(1,NQSD))
          END IF
C
          IF (VPFSFG(NQSD).EQ.1) THEN
C           get monthly vals of scour pot factor - table-type mon-potfs
            I2= 57
            I4= 12
            CALL RTABLE (I2,NQSD,I4,UUNITS,
     M                   POTFSM(1,NQSD))
          END IF
        END IF
C
        IF (QSOFP(N).GT.0) THEN
C
          IF (VQOFG(NQOF).EQ.1) THEN
C           get monthly values of accum rates - table-type mon-accum
            I2= 58
            I4= 12
            CALL RTABLE (I2,NQOF,I4,UUNITS,
     M                   ACQOPM(1,NQOF))
C
C           get monthly values of limiting storage - table-type mon-sqolim
            I2= 59
            I4= 12
            CALL RTABLE (I2,NQOF,I4,UUNITS,
     M                   RVAL)
C
C           calculate monthly values of removal rate
            DO 90 J= 1,12
              REMQOM(J,NQOF)= ACQOPM(J,NQOF)/RVAL(J)
 90         CONTINUE
          END IF
        END IF
C
        IF (QIFWFP(N).GT.0) THEN
          IF (NQIF.GT.0) THEN
            IF (VIQCFG(NQIF).GT.0) THEN
C             get monthly values of concentration in
C             interflow - table-type mon-ifwconc
              I2= 60
              I4= 12
              CALL RTABLE (I2,NQIF,I4,UUNITS,
     M                     IOQCM(1,NQIF))
C
C             modification to allow units of mg/l (for chesapeake bay)
              IF (VIQCFG(NQIF).EQ.3 .OR. VIQCFG(NQIF).EQ.4) THEN
                DO 100 I4 = 1,12
                  IOQCM(I4,NQIF) = IOQCM(I4,NQIF)*CVT
 100            CONTINUE
                VIQCFG(NQIF) = VIQCFG(NQIF) - 2
              END IF
C             end chesapeake bay modification
            END IF
          END IF
        END IF
C
        IF (QAGWFP(N).GT.0 .AND. VAQCFG(NQGW).GT.0) THEN
C         get monthly values of concentration in
C         groundwater - table-type mon-grndconc
          I2= 61
          I4= 12
          CALL RTABLE (I2,NQGW,I4,UUNITS,
     M                 AOQCM(1,NQGW))
C
C         modification to allow units of mg/l (for chesapeake bay)
          IF (VAQCFG(NQGW).EQ.3.OR.VAQCFG(NQGW).EQ.4) THEN
            DO 110 I4 = 1,12
              AOQCM(I4,NQGW) = AOQCM(I4,NQGW)*CVT
 110        CONTINUE
            VAQCFG(NQGW) = VAQCFG(NQGW) - 2
          END IF
C         end chesapeake bay modification
        END IF
C       end nqual loop
 120  CONTINUE
C
      IF (OUTLEV.GT.1) THEN
C       finished processing message
        WRITE (MESSU,2040)
      END IF
C
      RETURN
      END
C
C     4.2(1).7
C
      SUBROUTINE PQUAL
C
C     + + + PURPOSE + + +
C     Simulate quality constituents (other than sediment, heat, dox,
C     and co2) using simple relationships with sediment and water
C     yield
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PQUAL2 + + +
      INCLUDE    'cplpq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    FP,N,QAFP,QIFP,QOFP,QSFP,J
      REAL       SUROQO,SUROQS,ATDPFX,ATDPCN
C
C     + + + EXTERNALS + + +
      EXTERNAL   QUALSD,QUALOF,QUALIF,QUALGW
C
C     + + + END SPECIFICATIONS + + +
C
C     nqual is the number of constituents being simulated
      DO 10 N= 1,NQUAL
C       simulate constituent n
C
C       simulate by association with sediment
        QSFP= QSDFP(N)
        IF (QSFP.GT.0) THEN
C         constituent n is simulated by association with sediment
C         the value of qsfp refers to the set of sediment
C         associated parameters to use
C
C         get input time series
          IF (SEDFG.EQ.0) THEN
C           get time series from inpad
            WSSD = PAD(WSDFP+IVL1)
            SCRSD= PAD(CSDFP+IVL1)
          ELSE
C           wssd and scrsd are available from sedmnt
          END IF
C
          CALL QUALSD (DAYFG,VPFWFG(QSFP),VPFSFG(QSFP),WSSD,SCRSD,
     I                 POTFWM(1,QSFP),POTFSM(1,QSFP),
     $                 MON,NXTMON,DAY,NDAYS,
     M                 POTFW(QSFP),POTFS(QSFP),
     O                 WASHQS(QSFP),SCRQS(QSFP),SOQS(QSFP))
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
        IF (QOFP.GT.0) THEN
C         constituent n is simulated by association with overland
C         flow
C         the value of qofp refers to the set of overland flow
C         associated parameters to use
C
C         get input time series
          IF (PWATFG.EQ.0) THEN
C           get time series from inpad
            SURO= PAD(SOFP+IVL1)
          ELSE
C           suro is available from pwater
          END IF
          PREC= PAD(PRECFP+IVL1)
C
C         initialize atmospheric deposition
          ATDPFX= 0.0
          ATDPCN= 0.0
          J= (N-1)*2+ 1
          IF (PQADFG(J) .LE. -1) THEN
C           flux time series
            ATDPFX= PAD(PQAFFP(N)+IVL1)
          END IF
          IF (PQADFG(J+1) .LE. -1) THEN
C           conc time series
            ATDPCN= PAD(PQACFP(N)+IVL1)
          END IF
C
          CALL QUALOF (DAYFG,VQOFG(QOFP),ACQOPM(1,QOFP),REMQOM(1,QOFP),
     I                 MON,NXTMON,DAY,NDAYS,SURO,WSFAC(QOFP),
     I                 SOQOCX(QOFP),PREC,PQADFG(J),PQADFG(J+1),
     I                 PQAFXM(1,N),PQACNM(1,N),ATDPFX,ATDPCN,
     M                 ACQOP(QOFP),REMQOP(QOFP),SQO(QOFP),
     O                 SOQO(QOFP),SOQOC(QOFP),PQADDR(N),
     O                 PQADWT(N))
C
          SUROQO= SOQO(QOFP)
        ELSE
          SUROQO= 0.0
        END IF
C
C       sum outflows of constituent n from the land surface
        SOQUAL(N) = SUROQS+ SUROQO
        POQUAL(N) = SOQUAL(N)
C
C       compute the concentration if required - units are
C       qty/acre-inch
        FP= SOQCFP(N)
        IF (FP.GT.0) THEN
          IF (PWATFG.EQ.0) THEN
C           read time series from inpad
            SURO= PAD(SOFP+IVL1)
          ELSE
C           suro is available from pwater
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
C       simulate quality constituent in interflow
        QIFP= QIFWFP(N)
        IF (QIFP.GT.0) THEN
C         constiuent n is present in interflow
C         get input time series
          IF (PWATFG.EQ.0) THEN
            IFWO= PAD(IOFP+IVL1)
          ELSE
C           ifwo is available from pwater
          END IF
C
          CALL QUALIF (DAYFG,VIQCFG(QIFP),IFWO,IOQCM(1,QIFP),
     I                 MON,NXTMON,DAY,NDAYS,
     M                 IOQC(QIFP),
     O                 IOQUAL(QIFP))
C
C         cumulate outflow
          POQUAL(N)= POQUAL(N)+ IOQUAL(QIFP)
        END IF
C
C       simulate quality constituent in active groundwater outflow
        QAFP= QAGWFP(N)
        IF (QAFP.GT.0) THEN
C         constituent n is present in groundwater
C         get groundwater time series input
          IF (PWATFG.EQ.0) THEN
            AGWO= PAD(AOFP+IVL1)
          ELSE
C           agwo is available from pwater
          END IF
C
          CALL QUALGW (DAYFG,VAQCFG(QAFP),AGWO,AOQCM(1,QAFP),
     I                 MON,NXTMON,DAY,NDAYS,
     M                 AOQC(QAFP),
     O                 AOQUAL(QAFP))
C
C         cumulate outflow
          POQUAL(N)= POQUAL(N)+ AOQUAL(QAFP)
        END IF
C
        FP= POQCFP(N)
        IF (FP.GT.0) THEN
C         compute the concentration of constituent n in the total
C         outflow
C         get water outflow time series
          IF (PWATFG.EQ.0) THEN
            PERO= PAD(POFP+IVL1)
          ELSE
C           pero is available from pwater
          END IF
C
          IF (PERO.GT.0.0) THEN
            POQC(N)= POQUAL(N)/PERO
          ELSE
            POQC(N)= -1.0E30
          END IF
C
        END IF
C
 10   CONTINUE
C
      RETURN
      END
C
C     4.2(1).15.1.5
C
      SUBROUTINE PQACC
     I                 (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section pqual
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PQUAL2 + + +
      INCLUDE  'cplpq.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N= 1,7
        PQCF1(N,TOROW)= PQCF1(N,TOROW)+ PQCF1(N,FRMROW)
        PQCF2(N,TOROW)= PQCF2(N,TOROW)+ PQCF2(N,FRMROW)
        PQCF3(N,TOROW)= PQCF3(N,TOROW)+ PQCF3(N,FRMROW)
        PQCF4(N,TOROW)= PQCF4(N,TOROW)+ PQCF4(N,FRMROW)
        PQCF5(N,TOROW)= PQCF5(N,TOROW)+ PQCF5(N,FRMROW)
 10   CONTINUE
C
      DO 20 N= 1, 10
        PQCF6(N,TOROW)= PQCF6(N,TOROW)+ PQCF6(N,FRMROW)
        PQCF7(N,TOROW)= PQCF7(N,TOROW)+ PQCF7(N,FRMROW)
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE PQALPB
C
C     + + + PURPOSE + + +
C     handle section pqual
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PQUAL2 + + +
      INCLUDE    'cplpq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    N,QAFP,QIFP,QOFP,QSFP
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N= 1,NQUAL
C       sediment associated constituents
        QSFP= QSDFP(N)
        IF (QSFP.GT.0) THEN
C         this qual is a qualsd
          IF (WSQSFP(QSFP).GE.1) THEN
            PAD(WSQSFP(QSFP)+IVL1)= WASHQS(QSFP)
          END IF
          IF (SCQSFP(QSFP).GE.1) THEN
            PAD(SCQSFP(QSFP)+IVL1)= SCRQS(QSFP)
          END IF
          IF (SOQSFP(QSFP).GE.1) THEN
            PAD(SOQSFP(QSFP)+IVL1)= SOQS(QSFP)
          END IF
C
        END IF
C
C       surface runoff-associated quality constituents
        QOFP= QSOFP(N)
        IF (QOFP.GT.0) THEN
C         this qual is a qualof
          IF (SOQOFP(QOFP).GE.1) THEN
            PAD(SOQOFP(QOFP)+IVL1)= SOQO(QOFP)
          END IF
          IF (SOQOCX(QOFP).GE.1) THEN
            PAD(SOQOCX(QOFP)+IVL1)= SOQOC(QOFP)
          END IF
          IF (PQADDX(N) .GE. 1) THEN
            PAD(PQADDX(N)+IVL1)= PQADDR(N)
          END IF
          IF (PQADWX(N) .GE. 1) THEN
            PAD(PQADWX(N)+IVL1)= PQADWT(N)
          END IF
C
        END IF
C
C       interflow associated constituents
        QIFP= QIFWFP(N)
        IF (QIFP.GT.0) THEN
C         this qual is a qualif
          IF (IOQFP(QIFP).GE.1) THEN
            PAD(IOQFP(QIFP)+IVL1)= IOQUAL(QIFP)
          END IF
        END IF
C
C       groundwater associated constituents
        QAFP= QAGWFP(N)
        IF (QAFP.GT.0) THEN
C         this qual is a qualgw
          IF (AOQFP(QAFP).GE.1) THEN
            PAD(AOQFP(QAFP)+IVL1)= AOQUAL(QAFP)
          END IF
        END IF
C
C       output of global variables
        IF (SOQFP(N).GE.1) THEN
          PAD(SOQFP(N) +IVL1)= SOQUAL(N)
        END IF
        IF (SOQCFP(N).GE.1) THEN
          PAD(SOQCFP(N)+IVL1)= SOQC(N)
        END IF
        IF (POQFP(N).GE.1) THEN
          PAD(POQFP(N) +IVL1)= POQUAL(N)
        END IF
        IF (POQCFP(N).GE.1) THEN
          PAD(POQCFP(N)+IVL1)= POQC(N)
        END IF
C
 10   CONTINUE
C
      RETURN
      END
C
C     4.2(1).13.7
C
      SUBROUTINE PQALPT
C
C     + + + PURPOSE + + +
C     ???
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PQUAL2 + + +
      INCLUDE   'cplpq.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N,QOFP
C
C     + + + END SPECIFICATION + + +
C
C     handle section pqual
      DO 10 N= 1,NQUAL
C
        QOFP= QSOFP(N)
        IF (QOFP.GT.0) THEN
C         this qual is a qualof
          IF (SQOFP(QOFP).GE.1) THEN
            PAD(SQOFP(QOFP)+IVL1)= SQO(QOFP)
          END IF
        END IF
C
 10   CONTINUE
C
      RETURN
      END
C
C     4.2(1).15.2.7
C
      SUBROUTINE PQPRT
     I                 (UNITFG,LEV,PRINTU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units,
C     and produce printout
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    UNITFG,LEV,PRINTU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PQUAL2 + + +
      INCLUDE    'cplpq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    J,N,QAFP,QIFP,QOFP,QSFP,ADFG,M
      REAL       FACTA,PCFLX1,PCFLX2,PCFLX3,PCFLX4,PCFLX5,PPOQAL,
     $           PSOQAL,PSTAT1,PCFLX6,PCFLX7,PADTOT
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,1H ,'*** PQUAL ***')
 2010 FORMAT (/,1H ,'  FLUXES AND STORAGES',9X,'STORAGE:',12X,
     $        'OUTFLOW FLUXES:')
 2020 FORMAT (' ',33X,'OF FLOW',10X,
     $  '<---------------SURFACE----------------> INTERFLOW GRNDWATER',
     $        15X,'TOTAL')
 2030 FORMAT (' ',30X,'ASSOCIATED',10X,
     $        ' SEDIMENT ASSOCIATED      FLOW     TOTAL',33X,'OUTFLOW')
 2040 FORMAT (' ',30X,' CONSTITNT',10X,
     $        '   WASHOFF     SCOUR     ASSOC')
 2050 FORMAT (' ',37X,'SQO',10X,
     $  '    WASHQS     SCRQS      SOQO    SOQUAL    IOQUAL    AOQUAL',
     $        14X,'POQUAL')
 2060 FORMAT (3X,3A4,' (',A4,'/AC)',6X,1PG10.3,10X,6G10.3,10X,G10.3)
 2070 FORMAT (' ',2X,3A4,' (',A4,'/HA)',6X,1PG10.3,10X,6G10.3,10X,G10.3)
 2080 FORMAT (//,' ',2X,'ATMOSPHERIC DEPOSITION FLUXES OF FLOW-',
     $        'ASSOCATED CONSTITUENTS',//,' ',30X,'<------ DEPOSITION',
     $        ' FLUXES -------->',/,' ',2X,'CONSTITUENT',24X,'DRY',7X,
     $        'WET',10X,'TOTAL')
 2090 FORMAT (' ',2X,3A4,' (',A4,'/AC)',6X,1PG10.3,G10.3,5X,G10.3)
 2100 FORMAT (' ',2X,3A4,' (',A4,'/HA)',6X,1PG10.3,G10.3,5X,G10.3)
C
C     + + + END SPECIFICATIONS + + +
C
C     print headings on unit printu
      WRITE (PRINTU,2000)
      WRITE (PRINTU,2010)
      WRITE (PRINTU,2020)
      WRITE (PRINTU,2030)
      WRITE (PRINTU,2040)
      WRITE (PRINTU,2050)
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
        IF (QSFP.NE.0) THEN
C         constituent is simulated by association with sediment
          PCFLX1= PQCF1(QSFP,LEV)*FACTA
          PCFLX2= PQCF2(QSFP,LEV)*FACTA
        ELSE
          PCFLX1= 0.0
          PCFLX2= 0.0
        END IF
C
        QOFP= QSOFP(N)
        IF (QOFP.NE.0) THEN
C         constituent is simulated by association with
C         overland flow
C         storage
          PSTAT1= SQO(QOFP)*FACTA
C         flux
          PCFLX3= PQCF3(QOFP,LEV)*FACTA
        ELSE
          PSTAT1= 0.0
          PCFLX3= 0.0
C
        END IF
C
C       sum outflows of constituent from the land surface
        PSOQAL= PCFLX1+ PCFLX2+ PCFLX3
C
        QIFP= QIFWFP(N)
        IF (QIFP.NE.0) THEN
C         constituent is simulated in interflow
          PCFLX4= PQCF4(QIFP,LEV)*FACTA
        ELSE
          PCFLX4= 0.0
        END IF
C
        QAFP= QAGWFP(N)
        IF (QAFP.NE.0) THEN
C         constituent is simulated in groundwater flow
          PCFLX5= PQCF5(QAFP,LEV)*FACTA
        ELSE
          PCFLX5= 0.0
        END IF
C
C       cumulate total outflow
        PPOQAL= PSOQAL+ PCFLX4+ PCFLX5
C
        IF (UNITFG.EQ.1) THEN
C         english system
          WRITE (PRINTU,2060) (QUALID(J,N),J=1,3),QTYID(1,N),
     $      PSTAT1, PCFLX1, PCFLX2, PCFLX3, PSOQAL, PCFLX4, PCFLX5,
     $      PPOQAL
        ELSE
C         metric system
          WRITE (PRINTU,2070) (QUALID(J,N),J=1,3),QTYID(1,N),
     $      PSTAT1, PCFLX1, PCFLX2, PCFLX3, PSOQAL, PCFLX4, PCFLX5,
     $      PPOQAL
        END IF
C
 10   CONTINUE
C
      ADFG= 0
      DO 20 N=1, NQUAL
        J= (N-1)*2+ 1
        IF ( (PQADFG(J) .NE. 0) .OR. (PQADFG(J+1) .NE. 0) ) THEN
          ADFG= 1
        END IF
 20   CONTINUE
C
      IF (ADFG .EQ. 1) THEN
C       there is atmospheric deposition to output
        WRITE (PRINTU,2080)
        DO 30 N= 1, NQUAL
          J= (N-1)*2+ 1
          IF ( (PQADFG(J) .NE. 0) .OR. (PQADFG(J+1) .NE. 0) ) THEN
            IF (PQADFG(J) .NE. 0) THEN
              PCFLX6= PQCF6(N,LEV)*FACTA
            ELSE
              PCFLX6= 0.0
            END IF
            IF (PQADFG(J+1) .NE. 0) THEN
              PCFLX7= PQCF7(N,LEV)*FACTA
            ELSE
              PCFLX7= 0.0
            END IF
            PADTOT= PCFLX6+ PCFLX7
            IF (UNITFG .EQ. 1) THEN
C             english units
              WRITE (PRINTU,2090) (QUALID(M,N),M=1,3),QTYID(1,N),
     #                             PCFLX6,PCFLX7,PADTOT
            ELSE
C             metric units
              WRITE (PRINTU,2100) (QUALID(M,N),M=1,3),QTYID(1,N),
     #                             PCFLX6,PCFLX7,PADTOT
            END IF
          END IF
 30     CONTINUE
      END IF
C
      RETURN
      END
C
C     4.2(1).15.3.5
C
      SUBROUTINE PQRST
     I                 (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators in section pqual
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
C     + + + COMMON BLOCKS- SCRTCH, VERSION PQUAL2 + + +
      INCLUDE    'cplpq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    N
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N= 1,7
        PQCF1(N,LEV)= 0.0
        PQCF2(N,LEV)= 0.0
        PQCF3(N,LEV)= 0.0
        PQCF4(N,LEV)= 0.0
        PQCF5(N,LEV)= 0.0
 10   CONTINUE
C
      DO 20 N= 1, 10
        PQCF6(N,LEV)= 0.0
        PQCF7(N,LEV)= 0.0
 20   CONTINUE
C
      RETURN
      END
C
C     4.2(1).7.4
C
      SUBROUTINE QUALGW
     I                  (DAYFG,VAQCFG,AGWO,AOQCM,MON,NXTMON,DAY,NDAYS,
     M                   AOQC,
     O                   AOQUAL)
C
C     + + + PURPOSE + + +
C     Simulate quality constituents by fixed concentration in
C     groundwater flow
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,VAQCFG
      REAL       AGWO,AOQC,AOQCM(12),AOQUAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VAQCFG - ???
C     AGWO   - ???
C     AOQCM  - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     AOQC   - ???
C     AOQUAL - ???
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
        IF (VAQCFG.GT.0) THEN
C
C         modification to allow optional no interpolation of
C         monthly values  (modified for chesapeake bay)
          IF (VAQCFG.EQ.2) THEN
C           no interpolation
            AOQC = AOQCM(MON)
          ELSE
C           concentrations are allowed to vary throughout the year
C           interpolate for the daily value
C           linearly interpolate aoqc between two values from the
C           monthly array aoqcm(12) for this interflow quality
C           constituent (no. qafp)
            AOQC = DAYVAL(AOQCM(MON),AOQCM(NXTMON),DAY,NDAYS)
          END IF
        ELSE
C         concentrations do not vary throughout the year.
C         aoqc value has been supplied by the run interpreter
        END IF
C
      END IF
C
C     simulate constituents carried by groundwater flow - units
C     are qty/acre-ivl
      IF (AGWO.GT.0.0) THEN
        AOQUAL= AOQC*AGWO
      ELSE
        AOQUAL= 0.0
      END IF
C
      RETURN
      END
C
C     4.2(1).7.3
C
      SUBROUTINE QUALIF
     I                  (DAYFG,VIQCFG,IFWO,IOQCM,
     I                   MON,NXTMON,DAY,NDAYS,
     M                   IOQC,
     O                   IOQUAL)
C
C     + + + PURPOSE + + +
C     Simulate quality constituents by fixed concentration in interflow
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,VIQCFG
      REAL       IFWO,IOQC,IOQCM(12),IOQUAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VIQCFG - ???
C     IFWO   - ???
C     IOQCM  - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     IOQC   - ???
C     IOQUAL - ???
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
        IF (VIQCFG.GT.0) THEN
C
C         modification to allow optional no interpolation of
C         monthly values  (modified for chesapeake bay)
          IF (VIQCFG.EQ.2) THEN
C           no interpolation
            IOQC = IOQCM(MON)
          ELSE
C           concentrations are allowed to vary throughout the year
C           interpolate for the daily value
C           linearly interpolate ioqc between two values from the
C           monthly array ioqcm(12) for this interflow quality
C           constituent (no. qifp)
            IOQC = DAYVAL(IOQCM(MON),IOQCM(NXTMON),DAY,NDAYS)
          END IF
        ELSE
C         concentrations do not vary throughout the year.
C         ioqc value has been supplied by the run interpreter
        END IF
C
      END IF
C
C     simulate constituents carried by interflow - units are
C     qty/acre-ivl
      IF (IFWO.GT.0.0) THEN
        IOQUAL= IOQC*IFWO
      ELSE
        IOQUAL= 0.0
      END IF
C
      RETURN
      END
C
C     4.2(1).7.2
C
      SUBROUTINE   QUALOF
     I                   (DAYFG,VQOFG,ACQOPM,REMQOM,MON,NXTMON,
     I                    DAY,NDAYS,SURO,WSFAC,SOQOCX,PREC,ADFXFG,
     I                    ADCNFG,ADFXMN,ADCNMN,ADFLX,ADCNC,
     M                    ACQOP,REMQOP,SQO,
     O                    SOQO,SOQOC,ADFXFX,ADCNFX)
C
C     + + + PURPOSE + + +
C     Simulate accumulation of a quality constituent on the land
C     surface and its removal by a constant unit rate and by overland
C     flow
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,SOQOCX,VQOFG,ADFXFG,ADCNFG
      REAL       ACQOPM(12),ACQOP,REMQOM(12),REMQOP,SOQO,SOQOC,SQO,
     #           SURO,WSFAC,PREC,ADFXMN(12),ADCNMN(12),ADFLX,ADCNC,
     #           ADFXFX,ADCNFX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VQOFG  - ???
C     ACQOPM - ???
C     REMQOM - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     SURO   - surface output
C     WSFAC  - ???
C     SOQOCX - ???
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
      INTRINSIC   EXP
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
        IF (VQOFG.EQ.1) THEN
C         accumulation rate of this quality constituent is allowed
C         to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate acqop between two values from the
C         monthly array acqopm(12) for this overland flow associated
C         quality constituent (no. qofp)
          ACQOP= DAYVAL(ACQOPM(MON),ACQOPM(NXTMON),DAY,NDAYS)
C
C         removal unit rate of this quality constituent is allowed
C         to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate remqop between two values from the
C         monthly array remqom(12) for this overland flow associated
C         quality constituent (no. qofp)
          REMQOP= DAYVAL(REMQOM(MON),REMQOM(NXTMON),DAY,NDAYS)
        END IF
C
C       update storage due to accumulation and removal which occurs
C       independent of runoff - units are qty/acre
        SQO= ACQOP+ SQO*(1.0-REMQOP)
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
C         there is some quality constituent (no. qofp) in storage,
C         washoff can occur
          DUMMY= SURO*WSFAC
          IF (DUMMY .LT. 1.0E-5) THEN
C           washoff too small for stable calculation - set to zero
            SOQO= 0.0
          ELSE
C           calculate washoff
            DUMMY= 1.0- EXP(-DUMMY)
            SOQO = SQO*DUMMY
C
C           update storage of constituent - units are in qty/acre
            SQO=SQO- SOQO
          END IF
        ELSE
C         no washoff load
          SOQO= 0.0
        END IF
      ELSE
        SOQO= 0.0
      END IF
C
C     compute and output concentration if required - units are
C     qty/acre-inch
      IF (SOQOCX.GE.1) THEN
        IF (SURO.GT.0.0) THEN
          SOQOC= SOQO/SURO
        ELSE
C         soqoc is undefined
          SOQOC= -1.0E30
        END IF
C
      END IF
C
      RETURN
      END
C
C     4.2(1).7.1
C
      SUBROUTINE QUALSD
     I                  (DAYFG,VPFWFG,VPFSFG,WSSD,SCRSD,POTFWM,
     I                   POTFSM,MON,NXTMON,DAY,NDAYS,
     M                   POTFW,POTFS,
     O                   WASHQS,SCRQS,SOQS)
C
C     + + + PURPOSE + + +
C     Simulate removal of a quality constituent from the land
C     surface by association with sediment
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,VPFSFG,VPFWFG
      REAL       POTFS,POTFSM(12),POTFW,POTFWM(12),SCRQS,SCRSD,SOQS,
     $           WASHQS,WSSD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VPFWFG - ???
C     VPFSFG - ???
C     WSSD   - ???
C     SCRSD  - ???
C     POTFWM - ???
C     POTFSM - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     POTFW  - ???
C     POTFS  - ???
C     WASHQS - ???
C     SCRQS  - ???
C     SOQS   - ???
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
        IF (VPFWFG.GT.0) THEN
C
C         modification to allow optional no interpolation of
C         monthly values  (modified for chesapeake bay)
          IF (VPFWFG.EQ.2) THEN
C           no interpolation
            POTFW = POTFWM(MON)
          ELSE
C           potency factors are allowed to vary throughout the year
C           interpolate for the daily value
C           linearly interpolate potfw between two values from the
C           monthly array potfwm(12) for this sediment associated quality
C           constituent (no. qsfp)
            POTFW = DAYVAL(POTFWM(MON),POTFWM(NXTMON),DAY,NDAYS)
          END IF
        ELSE
C         washoff potency factors do not vary throughout the year.
C         potfw value has been supplied by the run interpreter
        END IF
C
        IF (VPFSFG.EQ.1) THEN
C         scour potency factors are allowed to vary throughout the
C         year
C         interpolate for the daily value
C         linearly interpolate potfs between two values from the
C         monthly array potfsm(12) for this sediment associated
C         quality constituent (no. qsfp)
          POTFS= DAYVAL(POTFSM(MON),POTFSM(NXTMON),DAY,NDAYS)
        ELSE
C         scour potency factors do not vary throughout the year.
C         potsw value has been supplied by the run interpreter
        END IF
C
      END IF
C
C     associate with washoff of detached sediment - units are
C     qty/acre-ivl
      IF (ABS(WSSD).LE.0.0) THEN
        WASHQS= 0.0
      ELSE
        WASHQS= WSSD*POTFW
      END IF
C
C     associate with scouring of soil matrix - units are
C     qty/acre-ivl
      IF (ABS(SCRSD).LE.0.0) THEN
        SCRQS= 0.0
      ELSE
        SCRQS= SCRSD*POTFS
      END IF
C
C     sum removals
      SOQS= WASHQS+SCRQS
C
      RETURN
      END
