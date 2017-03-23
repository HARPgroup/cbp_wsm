C
C
C
      SUBROUTINE   PSEDMT
C
C     + + + PURPOSE + + +
C     Process the input for section sedmnt
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SEDMNT1 + + +
      INCLUDE    'cplse.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I1,I2,I4,J,N
      REAL      RVAL(6)
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITABLE,RTABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION SEDMNT')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION SEDMNT')
C
C     + + + END SPECIFICATIONS + + +
C
      I1=1
C
C     initialize variables not set in SDRST
      DO 10 J= 1, 5
        SDIF(J)= 0.0
 10   CONTINUE
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
      DRYDFG= 1
C
C     process values in table-type sed-parm1
      I2= 27
      I4= 3
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             SEDPM1)
C
C     process values in table-type sed-parm2
      I2= 28
      I4= 6
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             RVAL)
C
      SMPF = RVAL(1)
      KRER = RVAL(2)
      JRER = RVAL(3)
      AFFIX= RVAL(4)
      COVER= RVAL(5)
      NVSI = RVAL(6)*DELT/1440.
C
C     process values in table-type sed-parm3
      I2= 29
      I4= 4
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             RVAL)
C
      KSER= RVAL(1)
      JSER= RVAL(2)
      KGER= RVAL(3)
      JGER= RVAL(4)
C
C     if necessary, set csnofg
      IF (PWATFG.EQ.0) THEN
C       csnofg not available from pwater
C        I2= 26
C        I4= 1
C        CALL ITABLE(I2,I1,I4,UUNITS,
C     M              CSNOFG)
        CSNOFG= 0
      END IF
C
      IF (CRVFG.EQ.1) THEN
C       get monthly values of erosion-related cover -
C       table-type mon-cover
        I2= 30
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               COVERM)
      END IF
C
      IF (VSIVFG.NE.0) THEN
C       get monthly values of net vertical sediment
C       input - table-type mon-nvsi
        I2= 31
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               NVSIM)
C       convert to internal units
        DO 20 J= 1,12
          NVSIM(J)= NVSIM(J)*DELT/1440.
 20     CONTINUE
      END IF
C
C     initial detached storage - table-type sed-stor
      I2= 32
      CALL RTABLE (I2,I1,NBLKS,UUNITS,
     M             DETSB)
C
C     initialize sediment transport capacity and segment-wide detached storage
      DETS= 0.0
      IF ( (NBLKS .EQ. 1) .AND. (SDOPFG .EQ. 1) ) THEN
        STCAP= DELT60*KSER*(SURS/DELT60)**JSER
      ELSE
        STCAP= 0.0
      END IF
      DO 40 N= 1, NBLKS
        DETS= DETS+ DETSB(N)*NBLKSI
        IF ( (NBLKS .GT. 1) .AND. (SDOPFG .EQ. 1) ) THEN
          STCAPB(N)= DELT60*KSER*(SURSB(N)/DELT60)**JSER
          STCAP= STCAP+ STCAPB(N)*NBLKSI
        ELSE
          STCAPB(N)= 0.0
        END IF
 40   CONTINUE
C
      IF (OUTLEV.GT.1) THEN
C       end processing message
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
C
C     4.2(1).4
C
      SUBROUTINE SEDMNT
C
C     + + + PURPOSE + + +
C     Produce and remove sediment from the land surface
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SEDMNT2 + + +
      INCLUDE    'cplse.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
      REAL       DUMMY,PREC,RAIN
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL,DETACH,SOSED1,SOSED2,ATACH
C
C     + + + END SPECIFICATIONS + + +
C
C     get input time series
      PREC= PAD(PRECFP+IVL1)
      IF (CSNOFG.NE.1) GO TO 30
C       effects of snow are considered
        IF (SNOWFG.NE.0) GO TO 10
C         get rainfall and snow cover data from inpad
          RAIN  = PAD(RNFFP+IVL1)
          SNOCOV= PAD(SNOCFP+IVL1)
          GO TO 20
 10     CONTINUE
          RAIN= RAINF
C         snocov is available from snow
 20     CONTINUE
C
        GO TO 40
 30   CONTINUE
C       all precipitation is assumed to be rain
        RAIN= PREC
C       snocov is not required
 40   CONTINUE
C
      IF (SLSDFP.LE.0) GO TO 50
        SLSED= PAD(SLSDFP+IVL1)
        GO TO 60
 50   CONTINUE
        SLSED= 0.0
 60   CONTINUE
C
      IF (PWATFG.NE.0) GO TO 90
C       read time series supplied by pwater
        SURO= PAD(SOFP+IVL1)
        SURS= PAD(SSFP+IVL1)
        IF (NBLKS.LE.1) GO TO 80
          DO 70 I= 1,NBLKS
            SUROB(I)= PAD(SOBFP(I)+IVL1)
            SURSB(I)= PAD(SSBFP(I)+IVL1)
 70       CONTINUE
 80     CONTINUE
C
        GO TO 100
 90   CONTINUE
C       the above time series are available from pwater
 100  CONTINUE
C
C     estimate the quantity of sediment particles detached from the
C     soil surface by rainfall and augment the detached sediment
C     storage
      CALL DETACH (DAYFG,CRVFG,COVERM,MON,NXTMON,DAY,NDAYS,RAIN,
     I             CSNOFG,SNOCOV,DELT60,SMPF,KRER,JRER,NBLKS,
     M             COVER,DETS,DETSB,
     O             DET)
C
      IF (DAYFG.NE.1) GO TO 130
C       it is the first interval of the day
        IF (VSIVFG.EQ.0) GO TO 110
C         net vert. input values are allowed to vary throughout the
C         year
C         interpolate for the daily value
C         units are tons/acre-ivl
C         linearly interpolate nvsi between two values from the
C         monthly array nvsim(12)
          NVSI= DAYVAL(NVSIM(MON),NVSIM(NXTMON),DAY,NDAYS)
          GO TO 120
 110    CONTINUE
C         net vert. input values do not vary throughout the year.
C         nvsi value has been supplied by the run interpreter
 120    CONTINUE
        IF (VSIVFG.EQ.2) then
          if (DRYDFG.EQ.1) THEN
C           last day was dry, add a whole days load in first interval
C           detailed output will show load added over the whole day.
            DUMMY = NVSI* (1440./DELT)
C
            IF (NBLKS.GT.1) THEN
              DO 129 I=1,NBLKS
                DETSB(I) = DETSB(I) + DUMMY
 129          CONTINUE
            END IF
            DETS = DETS + DUMMY
          ELSE
C           dont accumulate until tomorrow, maybe
C            NVSI = 0.0
          end if
        END IF
C
 130  CONTINUE

C     augment the detached sediment storage by external(vertical)
C     inputs of sediment - dets and detsb units are tons/acre
      DUMMY= SLSED
      IF (VSIVFG.LT.2) DUMMY = DUMMY + NVSI
C
      IF (NBLKS.LE.1) GO TO 150
        DO 140 I= 1,NBLKS
          DETSB(I)= DETSB(I)+ DUMMY
 140    CONTINUE
C
 150  CONTINUE
C
      DETS= DETS+ DUMMY
C
C     washoff of detached sediment from the soil surface
      IF (SDOPFG.NE.1) GO TO 160
C       use method 1
        CALL SOSED1 (NBLKS,SURO,SUROB,SURS,SURSB,DELT60,KSER,JSER,
     I               KGER,JGER,NBLKSI,
     M               DETS,DETSB,
     O               WSSD,SCRSD,SOSED,WSSDB,SCRSDB,SOSDB,STCAP,
     $               STCAPB)
C
        GO TO 170
 160  CONTINUE
C       use method 2
        CALL SOSED2 (NBLKS,SURO,SUROB,DELT60,KSER,JSER,
     I               KGER,JGER,NBLKSI,
     M               DETS,DETSB,
     O               WSSD,SCRSD,SOSED,WSSDB,SCRSDB,SOSDB,STCAP,
     $               STCAPB)
C
 170  CONTINUE
C
C     attach detached sediment on the surface to the soil matrix
C     this code has been modified to allow dry day sed load
C     code modification for chesapeake bay
      IF (DAYFG.EQ.1) THEN
C       first interval of new day
        IF (DRYDFG.EQ.1) THEN
C         yesterday was dry, attach detached sediment
C         on the surface to the soil matrix
          CALL ATACH (NBLKS,AFFIX,NBLKSI,
     M                 DETS,DETSB)
        END IF
C       assume today will be dry
        DRYDFG = 1
      END IF
C
      IF (PREC.GT.0.0) THEN
C       today is wet
        DRYDFG = 0
      END IF
C
      RETURN
      END
C
C     4.2(1).4.4
C
      SUBROUTINE ATACH
     I                  (NBLKS,AFFIX,NBLKSI,
     M                   DETS,DETSB)
C
C     + + + PURPOSE + + +
C     Simulate attachment or compaction of detached sediment on the
C     surface.  The calculation is done at the start of each day, if
C     the previous day was dry
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NBLKS
      REAL       AFFIX,DETS,DETSB(5),NBLKSI
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NBLKS  - number of blocks into which pls zones will be subdivided
C     AFFIX  - ???
C     NBLKSI - ???
C     DETS   - ???
C     DETSB  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
C     this subroutine was modified to allow optional
C     sed loading on dry days (chesapeake bay)
C     precipitation did not occur during the previous day
C     the attachment of surface sediment to the soil matrix is
C     taken into account by decreasing the storage of detached
C     sediment
      IF (NBLKS.NE.1) GO TO 30
C       surface and near-surface zones of the land segment have
C       not been subdivided into blocks
        DETS= DETS*(1.0-AFFIX)
        GO TO 50
 30   CONTINUE
C       surface and near-surface zones of the land segment have
C       been subdivided into blocks
        DETS= 0.0
        DO 40 I= 1,NBLKS
          DETSB(I)= DETSB(I)*(1.0-AFFIX)
          DETS    = DETS+ DETSB(I)*NBLKSI
 40     CONTINUE
C
 50   CONTINUE
C
      RETURN
      END
C
C     4.2(1).4.1
C
      SUBROUTINE DETACH
     I                  (DAYFG,CRVFG,COVERM,MON,NXTMON,DAY,NDAYS,RAIN,
     I                   CSNOFG,SNOCOV,DELT60,SMPF,KRER,JRER,NBLKS,
     M                   COVER,DETS,DETSB,
     O                   DET)
C
C     + + + PURPOSE + + +
C     Detach soil by rainfall
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    CRVFG,CSNOFG,DAY,DAYFG,MON,NBLKS,NDAYS,NXTMON
      REAL       COVER,COVERM(12),DELT60,DET,DETS,DETSB(5),JRER,KRER,
     $           RAIN,SMPF,SNOCOV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     CRVFG  - ???
C     COVERM - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     RAIN   - ???
C     CSNOFG - ???
C     SNOCOV - ???
C     DELT60 - simulation time interval in hours
C     SMPF   - ???
C     KRER   - ???
C     JRER   - ???
C     NBLKS  - number of blocks into which pls zones will be subdivided
C     COVER  - ???
C     DETS   - ???
C     DETSB  - ???
C     DET    - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
      REAL       CR
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG.NE.1) GO TO 30
C       it is the first interval of the day
        IF (CRVFG.NE.1) GO TO 10
C         erosion related cover is allowed to vary throughout the
C         year
C         interpolate for the daily value
C         linearly interpolate cover between two values from the
C         monthly array coverm(12)
          COVER= DAYVAL(COVERM(MON),COVERM(NXTMON),DAY,NDAYS)
          GO TO 20
 10     CONTINUE
C         erosion related cover does not vary throughout the year.
C         cover value has been supplied by the run interpreter
 20     CONTINUE
C
 30   CONTINUE
C
      IF (RAIN.LE.0.0) GO TO 80
C       simulate detachment because it is raining
C       find the proportion of area shielded from raindrop impact by
C       snowpack and other cover
        IF (CSNOFG .EQ. 1) THEN
C         snow is being considered
          IF (SNOCOV .GT. 0.0) THEN
C           there is a snowpack
            CR= COVER+ (1.0- COVER)*SNOCOV
          ELSE
            CR= COVER
          END IF
        ELSE
          CR= COVER
        END IF
C
C       calculate the rate of soil detachment, delt60= delt/60 -
C       units are tons/acre-ivl
        DET= DELT60*(1.0-CR)*SMPF*KRER*(RAIN/DELT60)**JRER
C
C       augment detached sediment storage - units are tons/acre
        DETS= DETS+ DET
        IF (NBLKS.LE.1) GO TO 70
          DO 60 I= 1,NBLKS
            DETSB(I)= DETSB(I)+ DET
 60       CONTINUE
C
 70     CONTINUE
C
        GO TO 90
 80   CONTINUE
C       no rain - either it is snowing or it is "dry"
        DET= 0.0
C
 90   CONTINUE
C
      RETURN
      END
C
C     4.2(1).15.1.3
C
      SUBROUTINE SDACC
     I                 (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section sedmnt
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
C     + + + COMMON BLOCKS- SCRTCH, VERSION SEDMNT2 + + +
      INCLUDE    'cplse.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I2,J
C
C     + + + EXTERNALS + + +
      EXTERNAL   ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2=2
C     handle flux groups containing segment-wide variables
C
      IF (SLSDFP.LE.0) GO TO 10
C       lateral input fluxes are being considered and printed
        SDIF(TOROW)= SDIF(TOROW)+ SDIF(FRMROW)
 10   CONTINUE
C
      CALL ACCVEC (I2,SDCF1(1,FRMROW),
     M             SDCF1(1,TOROW))
C
      CALL ACCVEC (I2,SDCF3(1,FRMROW),
     M             SDCF3(1,TOROW))
C
      IF (NBLKS.LE.1) GO TO 30
C       handle variables dealing with individual blocks in the
C       segment
C
        DO 20 J= 1,2
          CALL ACCVEC (NBLKS,SDCF2(1,J,FRMROW),
     M                 SDCF2(1,J,TOROW))
 20     CONTINUE
C
 30   CONTINUE
C
      RETURN
      END
C
C     4.2(1).15.2.4
C
      SUBROUTINE SDPRT
     I                 (UNITFG,LEV,PRINTU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units
C     and print out results.  the arrays pstat1 through
C     pstat3, piflx, and pcflx1 through pcflx3 have identical
C     structures to sdst1 through sdst3, sdif, and sdcf1 through
C     sdcf3 apart from dropping the dimension lev for fluxes
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
C     + + + COMMON BLOCKS- SCRTCH, VERSION SEDMNT2 + + +
      INCLUDE    'cplse.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I2,J
      REAL       MFACTA,MFACTB,PCFLX1(2),PCFLX2(5,2),PCFLX3(2),
     $           PDETS,PDETSB(5),PSDIF
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,1H ,'*** SEDMNT ***')
 2010 FORMAT (/,1H ,'  STATE VARIABLES',26X,'STORAGE')
 2020 FORMAT (' ',46X,'DETS',15X,'COVER')
 2030 FORMAT (' ',43X,'TONS/AC',12X,'FRACTION')
 2040 FORMAT (' ',41X,'TONNES/HA',12X,'FRACTION')
 2050 FORMAT (' ','    SEGMENT-WIDE',24X,1PG10.3,10X,G10.3)
 2060 FORMAT (' ',6X,'BLOCK',I2,27X,1PG10.3)
 2070 FORMAT (/,1H ,'  FLUXES',35X,'WASHOFF',15X,'SCOUR',
     $        5X,'TOTAL',12X,'VERTICAL ADDITIONS',6X,'LATERAL INFLOW')
 2080 FORMAT (' ',46X,'WSSD',15X,'SCRSD',5X,'SOSED',17X,'DET',
     $        6X,'NVSI',15X,'SLSED')
 2090 FORMAT (' ',40X,'   TONS/AC',10X,'   TONS/AC   TONS/AC',10X,
     $        '   TONS/AC   TONS/AC',10X,'   TONS/AC')
 2100 FORMAT (' ',40X,' TONNES/HA',10X,' TONNES/HA TONNES/HA',10X,
     $        ' TONNES/HA TONNES/HA',10X,' TONNES/HA')
 2110 FORMAT (' ','    SEGMENT-WIDE',24X,1PG10.3,10X,2G10.3,10X,2G10.3,
     $        10X,G10.3)
 2120 FORMAT (/,1H ,'  FLUXES',35X,'WASHOFF',15X,'SCOUR',
     $        5X,'TOTAL',12X,'VERTICAL ADDITIONS')
 2130 FORMAT (' ',46X,'WSSD',15X,'SCRSD',5X,'SOSED',17X,'DET',
     $        6X,'NVSI')
 2140 FORMAT (' ',40X,'   TONS/AC',10X,'   TONS/AC   TONS/AC',10X,
     $        '   TONS/AC   TONS/AC')
 2150 FORMAT (' ',40X,' TONNES/HA',10X,' TONNES/HA TONNES/HA',10X,
     $        ' TONNES/HA TONNES/HA')
 2160 FORMAT (' ','    SEGMENT-WIDE',24X,1PG10.3,10X,2G10.3,10X,2G10.3)
 2170 FORMAT (' ',6X,'BLOCK',I2,27X,1PG10.3,10X,2G10.3)
C
C     + + + END SPECIFICATIONS + + +
C
      I2=2
C     dimensionless variables - do not have to be converted
C
C     assign conversion constant for dimensional variables
      IF (UNITFG.NE.1) GO TO 10
C       english system
        MFACTA= 1.0
        GO TO 20
 10   CONTINUE
C       metric system
        MFACTA= 2.241
 20   CONTINUE
C
      MFACTB= 0.0
C
C     convert dimensional variables to external units
C
C     segment-wide state variables
      PDETS= DETS*MFACTA
C
      IF (SLSDFP.LE.0) GO TO 30
C       lateral inflows are being handled
        PSDIF= SDIF(LEV)*MFACTA
 30   CONTINUE
C
C     computed fluxes
      CALL TRNVEC (I2,SDCF1(1,LEV),MFACTA,MFACTB,
     O             PCFLX1)
C
      CALL TRNVEC (I2,SDCF3(1,LEV),MFACTA,MFACTB,
     O             PCFLX3)
C
      IF (NBLKS.LE.1) GO TO 60
C       handle variables which apply to individual blocks in the
C       segment
C
        DO 40 I= 1,NBLKS
          PDETSB(I)= DETSB(I)*MFACTA
 40     CONTINUE
C
        DO 50 J= 1,2
          CALL TRNVEC (NBLKS,SDCF2(1,J,LEV),MFACTA,MFACTB,
     O                 PCFLX2(1,J))
 50     CONTINUE
C
 60   CONTINUE
C
C     write to unit printu
C
      WRITE (PRINTU,2000)
C
      WRITE (PRINTU,2010)
      WRITE (PRINTU,2020)
C
      IF (UNITFG.NE.1) GO TO 70
        WRITE (PRINTU,2030)
        GO TO 80
 70   CONTINUE
        WRITE (PRINTU,2040)
 80   CONTINUE
C
      WRITE (PRINTU,2050)  PDETS, COVER
C
      IF (NBLKS.LE.1) GO TO 100
C       write state variables for individual blocks
        DO 90 I= 1,NBLKS
          WRITE (PRINTU,2060)  I, PDETSB(I)
 90     CONTINUE
C
 100  CONTINUE
C
C     total segment wide sediment outflow
      SOSED= PCFLX1(1)+ PCFLX1(2)
C
C     fluxes
      IF (SLSDFP.LE.0) GO TO 130
C       lateral inflow is considered
        WRITE (PRINTU,2070)
        WRITE (PRINTU,2080)
C
        IF (UNITFG.NE.1) GO TO 110
          WRITE (PRINTU,2090)
          GO TO 120
 110    CONTINUE
          WRITE (PRINTU,2100)
 120    CONTINUE
C
        WRITE (PRINTU,2110)  PCFLX1, SOSED, PCFLX3, PSDIF
        GO TO 160
C
 130  CONTINUE
C       no lateral inflow considered
        WRITE (PRINTU,2120)
        WRITE (PRINTU,2130)
C
        IF (UNITFG.NE.1) GO TO 140
          WRITE (PRINTU,2140)
          GO TO 150
 140    CONTINUE
          WRITE (PRINTU,2150)
 150    CONTINUE
C
        WRITE (PRINTU,2160)  PCFLX1, SOSED, PCFLX3
C
 160  CONTINUE
C
      IF (NBLKS.LE.1) GO TO 180
C       write fluxes for individual blocks}
        DO 170 I= 1,NBLKS
C         total block outflow
          SOSED= PCFLX2(I,1)+ PCFLX2(I,2)
          WRITE (PRINTU,2170)  I, (PCFLX2(I,J),J=1,2), SOSED
 170    CONTINUE
C
 180  CONTINUE
C
      RETURN
      END
C
C     4.2(1).15.3.3
C
      SUBROUTINE SDRST
     I                 (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables
C     used in material balance check for section sedmnt
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
C     + + + COMMON BLOCKS- SCRTCH, VERSION SEDMNT2 + + +
      INCLUDE    'cplse.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I2,J
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2=2
C     handle flux groups containing segment-wide variables
C
      IF (SLSDFP.LE.0) GO TO 10
C       lateral input fluxes are being printed
        SDIF(LEV)= 0.0
 10   CONTINUE
C
      CALL SETVEC (I2,0.0,
     O             SDCF1(1,LEV))
C
      CALL SETVEC (I2,0.0,
     O             SDCF3(1,LEV))
C
      IF (NBLKS.LE.1) GO TO 30
C       handle variables dealing with individual blocks in a
C       segment
        DO 20 J= 1,2
          CALL SETVEC (NBLKS,0.0,
     O                 SDCF2(1,J,LEV))
 20     CONTINUE
C
 30   CONTINUE
C
      RETURN
      END
C
C     4.2(1).4.2
C
      SUBROUTINE SOSED1
     I                  (NBLKS,SURO,SUROB,SURS,SURSB,DELT60,KSER,JSER,
     I                   KGER,JGER,NBLKSI,
     M                   DETS,DETSB,
     O                   WSSD,SCRSD,SOSED,WSSDB,SCRSDB,SOSDB,STCAP,
     $                   STCAPB)
C
C     + + + PURPOSE + + +
C     Warning,  this method of computing sediment removal contains a
C     dimensionally non-homogeneous term (surs+ suro).  this introduces
C     additional dependence of the results on the simulation interval
C     delt.  so far, it has only been used with delt of 15 and 5
C     minutes.
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NBLKS
      REAL       DELT60,DETS,DETSB(5),JGER,JSER,KGER,KSER,NBLKSI,SCRSD,
     $           SCRSDB(5),SOSED,SOSDB(5),SURO,SUROB(5),SURS,SURSB(5),
     $           WSSD,WSSDB(5),STCAP,STCAPB(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NBLKS  - number of blocks into which pls zones will be subdivided
C     SURO   - surface output
C     SURS   - ???
C     SUROB  - ???
C     SURSB  - ???
C     DELT60 - simulation time interval in hours
C     KSER   - ???
C     JSER   - ???
C     KGER   - ???
C     JGER   - ???
C     NBLKSI - ???
C     DETS   - ???
C     DETSB  - ???
C     WSSD   - ???
C     SCRSD  - ???
C     SOSED  - ???
C     WSSDB  - ???
C     SCRSDB - ???
C     SOSDB  - ???
C     STCAP  - ???
C     STCAPB - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
      REAL       ARG
C
C     + + + END SPECIFICATIONS + + +
C
C     Remove both detached surface sediment and soil matrix by surface
C     Flow using method 1
C
      IF (NBLKS.NE.1) GO TO 50
C       surface and near-surface zones of the land segment have not
C       been subdivided into blocks
C
        IF (SURO.LE.0.0) GO TO 30
C         surface runoff occurs, so sediment and soil matrix
C         particles may be removed, delt60= delt/60
C         get argument used in transport equations
          ARG= SURS+ SURO
C
C         calculate capacity for removing detached sediment - units
C         are tons/acre-ivl
          STCAP= DELT60*KSER*(ARG/DELT60)**JSER
C
          IF (STCAP.LE.DETS) GO TO 10
C           there is insufficient detached storage, base sediment
C           removal on that available, wssd is in tons/acre-ivl
            WSSD= DETS*SURO/ARG
            GO TO 20
 10       CONTINUE
C           there is sufficient detached storage, base sediment
C           removal on the calculated capacity
            WSSD= STCAP*SURO/ARG
 20       CONTINUE
C
          DETS= DETS- WSSD
C
C         calculate scour of matrix soil by surface runoff -
C         units are tons/acre-ivl
          SCRSD= DELT60*KGER*(ARG/DELT60)**JGER
C
          SCRSD= SCRSD*SURO/ARG
C
C         total removal by runoff
          SOSED= WSSD+ SCRSD
C
          GO TO 40
 30     CONTINUE
C         no runoff occurs, so no removal by runoff
          WSSD = 0.0
          SCRSD= 0.0
          SOSED= 0.0
          STCAP=0.0
C
 40     CONTINUE
C
        GO TO 110
 50   CONTINUE
C       surface and near-surface zones of the land segment have
C       been subdivided into blocks
C       initialize segment-wide variables
        WSSD = 0.0
        SCRSD= 0.0
        SOSED= 0.0
        DETS = 0.0
        STCAP=0.0
C
        DO 100 I= 1,NBLKS
          IF (SUROB(I).LE.0.0) GO TO 80
C           surface runoff occurs from this block, so soil
C           particles may be removed - delt60= delt/60
C           get argument used in transport equations
            ARG= SURSB(I) + SUROB(I)
C
C           calculate capacity for removing detached sediment -
C           units are tons/acre-ivl
            STCAPB(I)= DELT60*KSER*(ARG/DELT60)**JSER
C
            IF (STCAPB(I).LE.DETSB(I)) GO TO 60
C             there is insufficient detached storage, base sediment
C             removal on that available, wssdb is in tons/acre-ivl
              WSSDB(I)= DETSB(I)*SUROB(I)/ARG
              GO TO 70
 60         CONTINUE
C             there is sufficient detached storage, base sediment
C             removal on the calculated capacity
              WSSDB(I)= STCAPB(I)*SUROB(I)/ARG
 70         CONTINUE
C
C           update storage of detached sediment
            DETSB(I)= DETSB(I)- WSSDB(I)
C
C           calculate scour of matrix soil by surface runoff -
C           units are in tons/acre-ivl
            SCRSDB(I)= DELT60*KGER*(ARG/DELT60)**JGER
C
C           total removal by runoff
            SOSDB(I)= WSSDB(I)+ SCRSDB(I)
C
C           update land segment-wide variables
            SOSED= SOSED+ SOSDB(I)*NBLKSI
            WSSD = WSSD+ WSSDB(I)*NBLKSI
            SCRSD= SCRSD+ SCRSDB(I)*NBLKSI
C
            GO TO 90
 80       CONTINUE
C           no removals for this block, so no removal by runoff
            WSSDB(I) = 0.0
            SCRSDB(I)= 0.0
            SOSDB(I) = 0.0
C
            STCAPB(I)=0.0
 90       CONTINUE
C
          DETS = DETS+ DETSB(I)*NBLKSI
          STCAP=STCAP + STCAPB(I)*NBLKSI
C
 100    CONTINUE
C
 110  CONTINUE
C
      RETURN
      END
C
C     4.2(1).4.3
C
      SUBROUTINE SOSED2
     I                  (NBLKS,SURO,SUROB,DELT60,KSER,JSER,
     I                   KGER,JGER,NBLKSI,
     M                   DETS,DETSB,
     O                   WSSD,SCRSD,SOSED,WSSDB,SCRSDB,SOSDB,STCAP,
     $                   STCAPB)
C
C     + + + PURPOSE + + +
C     Warning,  this method of computing sediment removal has not
C     been tested.  but it is dimensionally homogeneous
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NBLKS
      REAL       DELT60,DETS,DETSB(5),JGER,JSER,KGER,KSER,NBLKSI,SCRSD,
     $           SCRSDB(5),SOSED,SOSDB(5),SURO,SUROB(5),WSSD,WSSDB(5),
     $           STCAP,STCAPB(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NBLKS  - number of blocks into which pls zones will be subdivided
C     SURO   - surface output
C     SUROB  - ???
C     DELT60 - simulation time interval in hours
C     KSER   - ???
C     JSER   - ???
C     KGER   - ???
C     JGER   - ???
C     NBLKSI - ???
C     DETS   - ???
C     DETSB  - ???
C     WSSD   - ???
C     SCRSD  - ???
C     SOSED  - ???
C     WSSDB  - ???
C     SCRSDB - ???
C     SOSDB  - ???
C     STCAP  - ???
C     STCAPB - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
C     Remove both detached surface sediment and soil matrix by surface
C     Flow using method 2
C
      IF (NBLKS.NE.1) GO TO 50
C       surface and near-surface zones of the land segment have not
C       been subdivided into blocks
C
        IF (SURO.LE.0.0) GO TO 30
C         surface runoff occurs, so sediment and soil matrix
C         particles may be removed, delt60= delt/60
C
C         calculate capacity for removing detached sediment - units
C         are tons/acre-ivl
          STCAP= DELT60*KSER*(SURO/DELT60)**JSER
C
          IF (STCAP.LE.DETS) GO TO 10
C           there is insufficient detached storage, base sediment
C           removal on that available, wssd is in tons/acre-ivl
            WSSD= DETS
            DETS= 0.0
            GO TO 20
 10       CONTINUE
C           there is sufficient detached storage, base sediment
C           removal on the calculated capacity
            WSSD= STCAP
            DETS= DETS- WSSD
 20       CONTINUE
C
C         calculate scour of matrix soil by surface runoff -
C         units are tons/acre-ivl
          SCRSD= DELT60*KGER*(SURO/DELT60)**JGER
C
C         total removal by runoff
          SOSED= WSSD+ SCRSD
C
          GO TO 40
 30     CONTINUE
C         no runoff occurs, so no removal by runoff
          WSSD = 0.0
          SCRSD= 0.0
          SOSED= 0.0
          STCAP=0.0
C
 40     CONTINUE
C
        GO TO 110
 50   CONTINUE
C       surface and near-surface zones of the land segment have
C       been subdivided into blocks
C       initialize segment-wide variables
        WSSD = 0.0
        SCRSD= 0.0
        SOSED= 0.0
        DETS = 0.0
        STCAP= 0.0
C
        DO 100 I= 1,NBLKS
          IF (SUROB(I).LE.0.0) GO TO 80
C           surface runoff occurs from this block, so soil
C           particles may be removed - delt60= delt/60
C
C           calculate capacity for removing detached sediment -
C           units are tons/acre-ivl
            STCAPB(I)= DELT60*KSER*(SUROB(I)/DELT60)**JSER
C
            IF (STCAPB(I).LE.DETSB(I)) GO TO 60
C             there is insufficient detached storage, base sediment
C             removal on that available, wssdb is in tons/acre-ivl
              WSSDB(I)= DETSB(I)
              GO TO 70
 60         CONTINUE
C             there is sufficient detached storage, base sediment
C             removal on the calculated capacity
              WSSDB(I)= STCAPB(I)
 70         CONTINUE
C
C           update storage of detached sediment
            DETSB(I)= DETSB(I)- WSSDB(I)
C
C           calculate scour of matrix soil by surface runoff -
C           units are in tons/acre-ivl
            SCRSDB(I)= DELT60*KGER*(SUROB(I)/DELT60)**JGER
C
C           total removal by runoff
            SOSDB(I)= WSSDB(I)+ SCRSDB(I)
C
C           update land segment-wide variables
            SOSED= SOSED+ SOSDB(I)*NBLKSI
            WSSD = WSSD+ WSSDB(I)*NBLKSI
            SCRSD= SCRSD+ SCRSDB(I)*NBLKSI
C
            GO TO 90
 80       CONTINUE
C           no removals for this block, so no removal by runoff
            WSSDB(I) = 0.0
            SCRSDB(I)= 0.0
            SOSDB(I) = 0.0
            STCAPB(I)=0.0
C
 90       CONTINUE
C
          DETS = DETS+ DETSB(I)*NBLKSI
          STCAP=STCAP + STCAPB(I)*NBLKSI
C
 100    CONTINUE
C
 110  CONTINUE
C
      RETURN
      END
C
C     4.2(1).14.3
C
      SUBROUTINE SEDTPB
C
C     + + + PURPOSE + + +
C     ???
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SEDMNT2 + + +
      INCLUDE   'cplse.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section sedmnt
      IF (WSDFP.GE.1)   PAD(WSDFP +IVL1)= WSSD
      IF (CSDFP.GE.1)   PAD(CSDFP +IVL1)= SCRSD
      IF (SOSDFP.GE.1)  PAD(SOSDFP+IVL1)= SOSED
      IF (DETFP.GE.1)   PAD(DETFP +IVL1)= DET
C
      IF (NBLKS.LE.1) GO TO 20
        DO 10 I= 1,NBLKS
          IF (WSDBFP(I).GE.1)  PAD(WSDBFP(I)+IVL1)= WSSDB(I)
          IF (CSDBFP(I).GE.1)  PAD(CSDBFP(I)+IVL1)= SCRSDB(I)
          IF (SOSDBX(I).GE.1)  PAD(SOSDBX(I)+IVL1)= SOSDB(I)
 10     CONTINUE
C
 20   CONTINUE
C
      RETURN
      END
C
C     4.2(1).13.4
C
      SUBROUTINE SEDTPT
C
C     + + + PURPOSE + + +
C     ???
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SEDMNT2 + + +
      INCLUDE   'cplse.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLE + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section sedmnt
C
      IF (DETSFP.GE.1)  PAD(DETSFP+IVL1)= DETS
      IF (STCFP.GE.1) PAD(STCFP + IVL1) =STCAP
C
      IF (NBLKS.LE.1) GO TO 20
        DO 10 I= 1,NBLKS
C         quantities belonging to block i
          IF (DETSBX(I).GE.1)  PAD(DETSBX(I)+IVL1) = DETSB(I)
          IF (STCFPB(I).GE.1) PAD(STCFPB(I) + IVL1)=STCAPB(I)
 10     CONTINUE
C
 20   CONTINUE
C
      RETURN
      END
