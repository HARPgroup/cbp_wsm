C
C
C
      SUBROUTINE   PPWATR
     O                    (OSVREC)
C
C     + + + PURPOSE + + +
C     Process input for pwater section of module perlnd
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER OSVREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OSVREC - number of OSV records needed for PWATER
C              2 if IFFCFG= 1
C              3 if IFFCFG= 2
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER1 + + +
      INCLUDE    'cplpw.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    TBSB,TBNO,NVAL,RDFLAG,I
      REAL       AGWRC,RVAL(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL   ITABLE,RTABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION PWATER')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION PWATER')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV.GT.1) THEN
C       processing section message
        WRITE (MESSU,2000)
      END IF
C
C     warning message counter initialization
      PWWCNT(1)= 0
C
C     error message counter initialization
      PWECNT(1)= 0
C
C     table of coordinates for function used to
C     evaluate upper zone behavior
      UZRA(1)   = 0.0
      UZRA(2)   = 1.25
      UZRA(3)   = 1.50
      UZRA(4)   = 1.75
      UZRA(5)   = 2.00
      UZRA(6)   = 2.10
      UZRA(7)   = 2.20
      UZRA(8)   = 2.25
      UZRA(9)   = 2.5
      UZRA(10)  = 4.0
C
      INTGRL(1) = 0.0
      INTGRL(2) = 1.29
      INTGRL(3) = 1.58
      INTGRL(4) = 1.92
      INTGRL(5) = 2.36
      INTGRL(6) = 2.81
      INTGRL(7) = 3.41
      INTGRL(8) = 3.8
      INTGRL(9) = 7.1
      INTGRL(10)= 3478.
C
C     initialize flag state variables
      SMSFG = 0
      FSMSFG= 0
C
C     initialize some other variables
      RLZRAT= -1.0E30
      LZFRAC= -1.0E30
C
C     process values in table - type pwat-parm1
      TBNO= 10
      TBSB= 1
      NVAL= 10
      CALL ITABLE(TBNO,TBSB,NVAL,UUNITS,
     M            PWPM1)
C
C     check to see if we need to read pwat-parm4
      RDFLAG= 0
      DO 10 I= 4, 9
        IF (PWPM1(I) .EQ. 0) THEN
C         parm is constant - need table
          RDFLAG= 1
        END IF
 10   CONTINUE
C
C     set OSVREC based on IFFCFG
      IF (IFFCFG .EQ. 1) THEN
C       pstemp not needed
        OSVREC= 2
      ELSE
C       need pstemp
        OSVREC= 3
      END IF
C
C     process values in table - type pwat-parm2
      TBNO= 11
      TBSB= 1
      NVAL= 7
      CALL RTABLE(TBNO,TBSB,NVAL,UUNITS,
     M            PWPM2)
C
C     convert to internal units
      INFILT= INFILT*DELT60
C
C     groundwater recession parameters
C  ICPRB change, JAN 2010: Implementing nonlinear groundwater storage-discharge relationship: Q = (k^beta)*(S^beta), 
C                          where Q = discharge, S = groundwater storage; need to redefine KGW:
C  Let AGWRC = k = -LOG(PWPM2(7)) and KVARY = (beta - 1), and need to require that KVARY >= 0.00001
C       AGWRC= PWPM2(7)   ! original code
       AGWRC = -LOG(PWPM2(7))
C  DEFINING KGW with KVARY:
C      KGW  = 1.0- AGWRC**(DELT60/24.0) ! original equation, prior to ICPRB change
C	
       KGW  = (AGWRC**(KVARY+1))*(DELT60/24.0)
C END ICPRB change
C
C     process values in table - type pwat-parm3
      TBNO= 12
      TBSB= 1
      NVAL= 7
      CALL RTABLE(TBNO,TBSB,NVAL,UUNITS,
     M            PWPM3)
C
      IF (RDFLAG .EQ. 1) THEN
C       process values in table - type pwat-parm4
        TBNO= 13
        TBSB= 1
        NVAL= 6
        CALL RTABLE(TBNO,TBSB,NVAL,UUNITS,
     M              PWST5)
      ELSE
C       dummy values until dayval is called on first interval
        DO 20 I= 1, 6
          PWST5(I)= 0.0
 20     CONTINUE
      END IF
C
C     process values in table - type pwat-parm5
C     fzg and fzgl - frozen ground parameters for
C     corps of engineers - chicago district 10/93
      TBNO= 14
      TBSB= 1
      NVAL= 2
      CALL RTABLE(TBNO,TBSB,NVAL,UUNITS,
     M            PWPM5)
C
      IF (VCSFG.EQ.1) THEN
C       get monthly interception storage capacity - table-type mon-icep
        TBNO= 17
        TBSB= 1
        NVAL= 12
        CALL RTABLE(TBNO,TBSB,NVAL,UUNITS,
     M              CEPSCM)
      END IF
C
      IF (VUZFG.EQ.1) THEN
C       get monthly values of uzsn - table-type mon-uzsn
        TBNO= 18
        TBSB= 1
        NVAL= 12
        CALL RTABLE(TBNO,TBSB,NVAL,UUNITS,
     M              UZSNM)
      END IF
C
      IF (VNNFG.EQ.1) THEN
C       get monthly values of manning's n - table-type mon-manning
        TBNO= 19
        TBSB= 1
        NVAL= 12
        CALL RTABLE(TBNO,TBSB,NVAL,UUNITS,
     M              NSURM)
      END IF
C
      IF (VIFWFG.EQ.1) THEN
C       get monthly interflow inflow params - table-type mon-interflw
        TBNO= 20
        TBSB= 1
        NVAL= 12
        CALL RTABLE(TBNO,TBSB,NVAL,UUNITS,
     M              INTFWM)
      END IF
C
      IF (VIRCFG.EQ.1) THEN
C       get monthly interflow recession constant - table-type mon-irc
        TBNO= 21
        TBSB= 1
        NVAL= 12
        CALL RTABLE(TBNO,TBSB,NVAL,UUNITS,
     M              IRCM)
      END IF
C
      IF (VLEFG.EQ.1) THEN
C       get monthly lower zone e-t parm - table-type mon-lzetparm
        TBNO= 22
        TBSB= 1
        NVAL= 12
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               LZETPM)
      END IF
C
C     initial conditions
C
C     process values in table-type pwat-state1
      TBNO= 15
      TBSB= 1
      NVAL= 7
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             PWST1)
C
      IF (NBLKS.GT.1) THEN
C       process vals applicable to each block - table-type pwat-blkstat
        SURS= 0.0
        UZS = 0.0
        IFWS= 0.0
C
        TBNO= 16
        NVAL= 3
        DO 30 TBSB= 1,NBLKS
          CALL RTABLE(TBNO,TBSB,NVAL,UUNITS,
     M                RVAL)
C
          SURSB(TBSB)= RVAL(1)
          UZSB(TBSB) = RVAL(2)
          IFWSB(TBSB)= RVAL(3)
C
          SURS= SURS+ SURSB(TBSB)
          UZS = UZS + UZSB(TBSB)
          IFWS= IFWS+ IFWSB(TBSB)
 30     CONTINUE
C
C       compute segment-wide average
        SURS= SURS* NBLKSI
        UZS = UZS * NBLKSI
        IFWS= IFWS* NBLKSI
      END IF
C
C     total storage in the pls
C
      PERS= CEPS+ SURS+ IFWS+ UZS+ LZS+ AGWS
C
      IF (OUTLEV.GT.1) THEN
C       end processing section message
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
C
C     4.2(1).3
C
      SUBROUTINE   PWATER
C
C     + + + PURPOSE + + +
C     Simulate the water budget for a pervious land segment.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER2 + + +
      INCLUDE     'cplpw.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ECNT1,I
      REAL        GWI,IPERC,LZRAT,MSUPY,MSUPYB(5),PREC,REMPET
C
C     + + + FUNCTIONS + + +
      REAL        DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL    DAYVAL,ICEPT,SURFAC,INTFLW,UZONE,LZONE,GWATER,EVAPT
C
C     + + + END SPECIFICATIONS + + +
C
C     get input PET
      PETINP= PAD(PETIFP + IVL1)
C
      IF (CSNOFG .EQ. 1) THEN
C       snow is being considered - allow for it
C       find the moisture supplied to interception storage
C       rainf is rainfall in inches/ivl. adjust for fraction of land
C       segment covered by snow. WYIELDL is the water yielded by the
C       snowpack in inches/ivl. it has already been adjusted to an
C       effective yield over the entire land segment
C
C       get input time series
        IF (AIRTFG .EQ. 0) THEN
C         get air temperature data
          AIRTMP= PAD(AIRTFP + IVL1)
        ELSE
C         air temperatures, in degrees f, are available from atemp
        END IF
C
        IF (SNOWFG .EQ. 0) THEN
C         get snow time series
          RAINF = PAD(RNFFP + IVL1)
          SNOCOV= PAD(SNOCFP + IVL1)
          WYIELD= PAD(WYFP + IVL1)
          IF (ICEFG .EQ. 1) THEN
C           effects of frozen ground are considered
            PACKI= PAD(PIFP + IVL1)
          END IF
        ELSE
C         the above time series are available from snow
        END IF
C
        SUPY= RAINF * (1.0 - SNOCOV) + WYIELD
C
        IF (HRFG .EQ. 1) THEN
C         it is time to recalculate intermittently computed numbers
C
C         adjustment factor for input pet to account for forest and
C         snowcover
          PETADJ= (1.0 - FOREST) * (1.0 - SNOCOV) + FOREST
C
          IF (AIRTMP .LT. PETMAX) THEN
C           adjustment factor may be reduced
            IF (AIRTMP .LT. PETMIN) THEN
C             pet is completely shut off
              PETADJ= 0.0
            ELSE
              IF (PETADJ .GT. 0.5) THEN
                PETADJ= 0.5
              END IF
            END IF
          END IF
C
          IF (ICEFG .EQ. 1) THEN
C           calculate factor to reduce infiltration and percolation
C           to account for frozen ground
            IF (IFFCFG .EQ. 1) THEN
C             enhanced version of the original method -
C             this algorithm defaults to the original result if
C             the parameters (fzg and fzgl) have the default values
              INFFAC= 1.0- FZG*PACKI
              IF (INFFAC .LT. FZGL) THEN
                INFFAC= FZGL
              END IF
            END IF
          ELSE
            INFFAC= 1.0
          END IF
        ELSE
C         petadj and inffac remain unchanged
        END IF
C
C       adjust input pet
        PET= PETINP * PETADJ
      ELSE
C       snow is not being considered
C       all precipitation is assumed to be rain
        PREC  = PAD(PRECFP + IVL1)
        SUPY  = PREC
        PET   = PETINP
        INFFAC= 1.0
      END IF
C
C     adjust INFFAC based on soil temperature
      IF (HRFG .EQ. 1) THEN
        IF (IFFCFG .EQ. 2) THEN
C         optional method - based on lower soil temperature and fzgl
          IF (PSTFG .EQ. 0) THEN
C           get soil temperatures from the inpad
            LGTMP= PAD(LGTFP + IVL1)
          ELSE
C           soil temperatures have been calculated in pstemp
          END IF
          IF (LGTMP .LE. 0.0) THEN
C           frozen ground
            INFFAC= FZGL
          ELSE
C           not frozen
            INFFAC= 1.0
          END IF
        END IF
      END IF
C         
C     simulate interception
      CALL ICEPT
     I          (VCSFG,DAYFG,CEPSCM,MON,NXTMON,DAY,NDAYS,SUPY,
     M           CEPSC,CEPS,
     O           CEPO)
C
      IF (SLIFP .GT. 0) THEN
C       surface lateral inflow timseries
        SURLI= PAD(SLIFP + IVL1)
      ELSE
C       surface lateral inflow not considered
        SURLI= 0.0
      END IF
C
C     surface inflow is the sum of interception outflow and surface
C     lateral inflow (if any)
      SURI= CEPO + SURLI
C
C     Msupy is used below and again in subroutine group surfac
C     Surss is the value of surs at the start of the ivl -
C     it is used by module section MSTLAY
      MSUPY= SURI + SURS
      SURSS= SURS
C
      IF (NBLKS .GT. 1) THEN
        DO 10 I= 1,NBLKS
          MSUPYB(I)= SURI + SURSB(I)
          SURSSB(I)= SURSB(I)
 10     CONTINUE
      END IF
C
C     set flags which indicate whether or not there is anything to
C     infiltrate and whether or not this is the first in a series of
C     wet intervals
C
      IF (MSUPY .GT. 0.0) THEN
C       there is surface moisture supply
        IF (SMSFG .EQ. 0) THEN
C         this is the first interval with surface moisture supply
C         after one or more intervals with none
          FSMSFG= 1
        ELSE
C         this is not the first wet interval
          FSMSFG= 0
        END IF
C       there is surface moisture supply
        SMSFG= 1
      ELSE
C       there is no surface moisture supply
        SMSFG = 0
        FSMSFG= 0
      END IF
C
C     Uzsn may be a varying parameter - the code to interpolate its
C     current value is placed here because UZSN is used in several
C     places in the code below this point
      IF (DAYFG .EQ. 1) THEN
C       it is the first interval of the day
        IF (VUZFG .EQ. 1) THEN
C         uzsn is allowed to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate uzsn between two values from the
C         monthly array uzsnm(12)
          UZSN= DAYVAL(UZSNM(MON),UZSNM(NXTMON),DAY,NDAYS)
        ELSE
C         uzsn does not vary throughout the year and
C         has been supplied by the run interpreter
        END IF
      END IF
C
C     determine the current value of the lower zone storage ratio
      LZRAT= LZS / LZSN
C
      IF (SMSFG .EQ. 1) THEN
C       simulate behavior of water on the land surface - surface
C       detention, infiltration, interflow input, surface outflow
        ECNT1= PWECNT(1)
        CALL SURFAC
     I             (LZRAT,INFILT,INFEXP,INFFAC,INFILD,FSMSFG,DAYFG,
     I              VNNFG,NSURM,LSUR,SLSUR,VIFWFG,INTFWM,
     I              NBLKS,MSUPY,MSUPYB,UZSN,UZS,DELT60,UZRA,INTGRL,
     I              RTOPFG,UZFG,UZSB,MON,NXTMON,DAY,NDAYS,NBLKSI,
     I              LSNO,MESSU,MSGFL,DATIM,
     M              DEC,SRC,NSUR,INTFW,SURS,SURSB,ECNT1,
     O              INFIL,UZI,IFWI,SURO,INFILB,UZIB,IFWIB,
     O              SUROB)
C
      ELSE
C       storage on the surface and outputs from it will be zero
        SURS = 0.0
        SURO = 0.0
        IFWI = 0.0
        INFIL= 0.0
        UZI  = 0.0
C
        IF (NBLKS .GT. 1) THEN
C         values for the individual blocks are also zero
          DO 20 I= 1,NBLKS
            SURSB(I) = 0.0
            SUROB(I) = 0.0
            IFWIB(I) = 0.0
            UZIB(I)  = 0.0
            INFILB(I)= 0.0
 20       CONTINUE
        END IF
C
      END IF
C
C     simulate interflow
      IF (ILIFP .GT. 0) THEN
C       interflow lateral inflow timseries
        IFWLI= PAD(ILIFP + IVL1)
      ELSE
C       interflow lateral not being considered
        IFWLI= 0.0
      END IF
      CALL INTFLW
     I           (DAYFG,VIRCFG,IRCM,MON,NXTMON,DAY,NDAYS,
     I            DELT60,NBLKS,IFWI,IFWIB,IFWLI,NBLKSI,
     M            IRC,IFWK1,IFWK2,IFWS,IFWSB,UZS,UZSB,
     O            IFWO,IFWOB)
C
C     simulate upper zone behavior
      CALL UZONE
     I          (NBLKS,UZSN,UZI,UZIB,INFILT,INFFAC,LZRAT,
     I           NBLKSI,
     M           UZS,UZSB,
     O           PERC,PERCB)
C
C     collect inflows to lower zone and groundwater
      IPERC= PERC + INFIL
C
C     simulate lower zone behavior
      CALL LZONE
     I          (IPERC,LZRAT,
     M           LZFRAC,LZS,RLZRAT,
     O           LZI)
C
C     simulate groundwater behavior
      GWI= IPERC - LZI
      IF (ALIFP .GT. 0) THEN
C       groundwater laterial inflow timseries
        AGWLI= PAD(ALIFP + IVL1)
      ELSE
C       groundwater laterial not considered
        AGWLI= 0.0
      END IF
      CALL GWATER
     I           (DEEPFR,GWI,KVARY,DAYFG,KGW,AGWLI,
     M            AGWS,GWVS,
     O            IGWI,AGWI,AGWO)
C
C     simulate ET
      CALL EVAPT
     I          (PET,BASETP,NBLKS,UZSN,AGWETP,KVARY,DAYFG,VLEFG,
     I           LZETPM,MON,NXTMON,DAY,NDAYS,LZSN,DELT60,NBLKSI,
     M           AGWO,CEPS,UZS,UZSB,AGWS,GWVS,LZETP,RPARM,LZS,
     O           REMPET,TAET,BASET,CEPE,UZET,UZETB,AGWET,LZET)
C
C     find total outflow
      PERO= SURO + IFWO + AGWO
C
C     total input of water to the pervious land segment
      WATIN= SUPY + SURLI + IFWLI + AGWLI
C
C     net input of water to the pervious land segment
      WATDIF= WATIN - (PERO + IGWI + TAET)
C
C     total moisture storage
      PERS= CEPS + SURS + IFWS + UZS + LZS + AGWS
C
      RETURN
      END
C
C
C
      SUBROUTINE    DISPOS
     I                    (IMIN,IMAX,RATIO,MSUPY,UZSN,UZS,DELT60,DEC,
     I                     SRC,UZRA,INTGRL,RTOPFG,UZFG,
     I                     LSNO,MESSU,MSGFL,DATIM,
     M                     SURS,ECNT1,
     O                     INFIL,UZI,IFWI,SURO)
C
C     + + + PURPOSE + + +
C     Dispose of moisture supply on either an individual block of
C     the land segment or on an entire land segment.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECNT1,MSGFL,LSNO,MESSU,
     $           RTOPFG,UZFG,DATIM(5)
      REAL       DEC,DELT60,IFWI,IMAX,IMIN,INFIL,INTGRL(10),MSUPY,
     $           RATIO,SRC,SURO,SURS,UZI,UZRA(10),UZS,UZSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IMIN   - ???
C     IMAX   - ???
C     RATIO  - ???
C     MSUPY  - ???
C     UZSN   - upper zone nominal storage
C     UZS    - initial upper zone storage
C     DELT60 - simulation time interval in hours
C     DEC    - ???
C     SRC    - ???
C     UZRA   - ???
C     INTGRL - ???
C     RTOPFG - ???
C     UZFG   - ???
C     LSNO   - line number in the opn sequence block of uci
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of error message file
C     SURS   - ???
C     ECNT1  - ???
C     INFIL  - ???
C     UZI    - ???
C     IFWI   - ???
C     SURO   - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      REAL       IIMAX,IIMIN,OVERI,OVERII,PDRO,PIFWI,PSUR,
     $           UNDRI,UNDRII,UZFRAC
C
C     + + + EXTERNALS + + +
      EXTERNAL   DIVISN, UZINF, UZINF2, PROUTE
C
C     + + + END SPECIFICATIONS + + +
C
C     determine how much of the MSUPY falls above and below the
C     infiltration line in the infiltration/interflow/surface runoff
C     figure
      CALL DIVISN
     I           (IMIN,IMAX,MSUPY,
     O            OVERI,UNDRI)
C
C     the quantity under I is infiltrated moisture
      INFIL= UNDRI
      IF (OVERI .GT. 0.0) THEN
C       there is some potential interflow inflow and maybe surface
C       detention/outflow -- the sum of these is potential direct
C       runoff
        PDRO= OVERI
C       determine how much of this potential direct runoff will
C       be taken by the upper zone
        IF (UZFG .EQ. 0) THEN
          CALL UZINF
     I              (PDRO,UZSN,UZS,UZRA,INTGRL,
     I               LSNO,MESSU,MSGFL,DATIM,
     M               ECNT1,
     O               UZI)
        ELSE
          CALL UZINF2
     I               (PDRO,UZSN,UZS,
     O                UZI)
        END IF
        IF (UZI .GT. PDRO) THEN
          UZI= PDRO
        END IF
        UZFRAC= UZI/PDRO
C
C       determine how much of the msupy falls above and below the
C       "infiltration + interflow" line in the infiltration/
C       interflow/surface runoff figure. the prefix "ii" is used on
C       variables associated with this line
        IIMIN= IMIN*RATIO
        IIMAX= IMAX*RATIO
        CALL DIVISN
     I             (IIMIN,IIMAX,MSUPY,
     O              OVERII,UNDRII)
C
C       psur is potential surface detention/runoff
        PSUR = OVERII
C       pifwi is potential interflow inflow
        PIFWI= PDRO - PSUR
        IFWI = PIFWI*(1.0 - UZFRAC)
C
        IF (PSUR .GT. 0.0) THEN
C         there will be something on or running off the surface
C         reduce it to account for the upper zone's share
          PSUR= PSUR*(1.0 - UZFRAC)
C
C         determine how much of this potential surface detention/
C         outflow will run off in this time interval
          CALL PROUTE
     I               (PSUR,RTOPFG,DELT60,DEC,SRC,MESSU,MSGFL,LSNO,
     M                SURS,ECNT1,
     O                SURO)
C
        ELSE
C         there is nothing to store on the surface or to run off
          SURS= 0.0
          SURO= 0.0
        END IF
      ELSE
C       there is no potential direct runoff or contribution to the
C       upper zone
        SURS= 0.0
        SURO= 0.0
        IFWI= 0.0
        UZI = 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DIVISN
     I                   (MIN,MAX,MSUPY,
     O                    OVER,UNDER)
C
C     + + + PURPOSE + + +
C     Determine the divisions of the quantities of the moisture supply
C     above and below the "infiltration" line or the "infiltration +
C     interflow" line in the infiltration/interflow/surface runoff
C     figure.  This routine is used either to simulate the behavior of
C     an individual block of the land segment or the entire land
C     segment.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       MAX,MIN,MSUPY,OVER,UNDER
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MIN    - ???
C     MAX    - ???
C     MSUPY  - ???
C     OVER   - ???
C     UNDER  - ???
C
C     + + + END SPECIFICATIONS + + +
C
      IF (MSUPY .LE. MIN) THEN
C       msupy line is entirely below other line
        UNDER= MSUPY
        OVER = 0.0
      ELSE
        IF (MSUPY .GT. MAX) THEN
C         msupy line is entirely above other line
          UNDER= (MIN + MAX)*0.5
          OVER =MSUPY - UNDER
        ELSE
C         msupy line crosses the other line
          OVER = ((MSUPY - MIN)**2)*0.5/(MAX - MIN)
          UNDER= MSUPY - OVER
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ETAGW
     I                  (AGWETP,KVARY,
     M                   REMPET,AGWS,TAET,GWVS,
     O                   AGWET)
C
C     + + + PURPOSE + + +
C     Simulate ET from active groundwater storage.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       AGWET,AGWETP,AGWS,GWVS,KVARY,REMPET,TAET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     AGWETP - ???
C     KVARY  - ???
C     REMPET - ???
C     AGWS   - ???
C     TAET   - ???
C     GWVS   - ???
C     AGWET  - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       GWPET
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
C     Agwetp is the input parameter governing et from active groundwater
      IF (AGWETP .GT. 0.0) THEN
C       there is et from groundwater
C       determine remaining capacity
        GWPET= REMPET*AGWETP
C
        IF (GWPET .GT. AGWS) THEN
C         groundwater et is limited by quantity available
          AGWET= AGWS
          AGWS = 0.0
        ELSE
C         groundwater et will not exhaust storage, so empty at
C         potential
          AGWET= GWPET
          AGWS = AGWS - AGWET
        END IF
C
C       update variables
        IF ((ABS(KVARY)) .GT. 0.0) THEN
          GWVS= GWVS - AGWET
        END IF
        TAET  = TAET + AGWET
        REMPET= REMPET - AGWET
      ELSE
C       there is no et from groundwater
        AGWET = 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ETBASE
     I                   (BASETP,
     M                    AGWO,REMPET,TAET,
     O                    BASET)
C
C     + + + PURPOSE + + +
C     Simulate ET from baseflow.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       AGWO,BASET,BASETP,REMPET,TAET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BASETP - ???
C     AGWO   - ???
C     REMPET - ???
C     TAET   - ???
C     BASET  - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       BASPET
C
C     + + + END SPECIFICATIONS + + +
C
      IF (BASETP .GT. 0.0) THEN
C       there is et from baseflow
        BASPET= BASETP*REMPET
        IF (BASPET .GT. AGWO) THEN
C         baseflow et is limited by quantity available
          BASET= AGWO
          AGWO = 0.0
        ELSE
C         baseflow et will not exhaust storage, so empty at potential
          BASET= BASPET
          AGWO = AGWO - BASET
        END IF
C
C       update totals
        TAET  = TAET + BASET
        REMPET= REMPET - BASET
      ELSE
C       no et from baseflow
        BASET= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ETLZON
     I                   (DAYFG,VLEFG,MON,DAY,LZETPM,NXTMON,
     I                    NDAYS,LZSN,DELT60,
     M                    LZETP,REMPET,RPARM,LZS,TAET,
     O                    LZET)
C
C     + + + PURPOSE + + +
C     Simulate ET from the lower zone.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,VLEFG
      REAL       DELT60,LZET,LZETP,LZETPM(12),LZS,LZSN,REMPET,RPARM,
     $           TAET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VLEFG  - ???
C     MON    - calendar month
C     DAY    - day of month
C     LZETPM - ???
C     NXTMON - next calendar month
C     NDAYS  - no. of days in this month
C     LZSN   - ???
C     DELT60 - simulation time interval in hours
C     LZETP  - ???
C     REMPET - ???
C     RPARM  - ???
C     LZS    - ???
C     TAET   - ???
C     LZET   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       LZPET
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG .EQ. 1) THEN
C       it is the first interval of the day
        IF (VLEFG .EQ. 1) THEN
C         lower zone et parameter is allowed to vary throughout the
C         year
C         interpolate for the daily value
C         linearly interpolate lzetp between two values from the
C         monthly array lzetpm(12)
          LZETP= DAYVAL(LZETPM(MON),LZETPM(NXTMON),DAY,NDAYS)
        ELSE
C         lower zone et parameter does not vary throughout the year.
C         lzetp value has been supplied by the run interpreter
        END IF
C
C       it is time to recalculate et opportunity parameter
C       rparm is max et opportunity - inches/ivl
        RPARM= 0.25/(1.0 - LZETP)*(LZS/LZSN)*DELT60/24.0
      END IF
C
      IF (REMPET .GT. 0.0) THEN
C       there remains an et demand
        IF (LZS .GT. 0.02) THEN
C         assume et can take place
          IF (ABS(LZETP - 1.0) .LE. 1.0E-5) THEN
C           special case - will try to draw et from whole land
C           segment at remaining potential rate
            LZPET= REMPET
          ELSE
C           usual case - desired et will vary over the whole land seg
            IF (REMPET .GT. RPARM) THEN
C             potential exceeds opportunity
              LZPET= 0.5*RPARM
            ELSE
C             potential exceeds opportunity over only part of the
C             land segment
              LZPET= REMPET*(1.0 - REMPET/(2.0*RPARM))
            END IF
C
            IF (LZETP .LT. 0.5) THEN
C             reduce the et to account for area devoid of vegetation
              LZPET= LZPET*2.0*LZETP
            END IF
          END IF
C
          IF (LZPET .LT. (LZS- 0.02) ) THEN
C           lower zone et will not exhaust storage, so empty at
C           potential
            LZET= LZPET
          ELSE
C           lower zone et is limited by quantity available
            LZET= LZS- 0.02
          END IF
C
C         update variables
          LZS   = LZS - LZET
          TAET  = TAET + LZET
          REMPET= REMPET - LZET
        ELSE
C         assume no et can take place
          LZET= 0.0
        END IF
      ELSE
C       no more et demand
        LZET= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ETUZON
     I                   (NBLKS,UZSN,NBLKSI,
     M                    REMPET,UZS,UZSB,TAET,
     O                    UZET,UZETB)
C
C     + + + PURPOSE + + +
C     Simulate ET from the upper zone
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NBLKS
      REAL       NBLKSI,REMPET,TAET,UZET,UZETB(5),UZS,UZSB(5),UZSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NBLKS  - number of blocks into which pls zones will be subdivided
C     UZSN   - upper zone nominal storage
C     NBLKSI - ???
C     REMPET - ???
C     UZS    - initial upper zone storage
C     UZSB   - ???
C     TAET   - ???
C     UZET   - ???
C     UZETB  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + EXTERNALS + + +
      EXTERNAL   ETUZS
C
C     + + + END SPECIFICATIONS + + +
C
      IF (NBLKS .EQ. 1) THEN
C       surface and near-surface zones of the land segment have not
C       been subdivided into blocks
        CALL ETUZS
     I            (UZSN,REMPET,
     M             UZS,
     O             UZET)
C
C       update totals
        TAET  = TAET + UZET
        REMPET= REMPET - UZET
      ELSE
C       surface and near-surface zones of the land segment have
C       been subdivided into blocks
C       initialize segment-wide variables
        UZET= 0.0
        UZS = 0.0
C
        DO 10 I= 1,NBLKS
          CALL ETUZS
     I              (UZSN,REMPET,
     M               UZSB(I),
     O               UZETB(I))
C
          UZS = UZS + UZSB(I)
          UZET= UZET + UZETB(I)
 10     CONTINUE
C
C       convert total to segment average
        UZS = UZS*NBLKSI
        UZET= UZET*NBLKSI
C
C       update totals
        TAET  = TAET + UZET
        REMPET= REMPET - UZET
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ETUZS
     I                  (UZSN,REMPET,
     M                   UZS,
     O                   UZET)
C
C     + + + PURPOSE + + +
C     This is a subsidiary subroutine for computing upper zone ET.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       REMPET,UZET,UZS,UZSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UZSN   - upper zone nominal storage
C     REMPET - ???
C     UZS    - initial upper zone storage
C     UZET   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       UZPET,UZRAT
C
C     + + + END SPECIFICATIONS + + +
C
      IF (UZS .GT. 0.001) THEN
C       there is et from the upper zone
C       estimate the uzet opportunity
        UZRAT= UZS/UZSN
        IF (UZRAT .GT. 2.0) THEN
          UZPET= REMPET
        ELSE
          UZPET= 0.5*UZRAT*REMPET
        END IF
C
C       calculate the actual et
        IF (UZPET .GT. UZS) THEN
C         upper zone et is limited by quantity available
          UZET= UZS
          UZS = 0.0
        ELSE
C         upper zone et will not exhaust storage, so empty at
C         potential
          UZET= UZPET
          UZS = UZS - UZET
        END IF
      ELSE
C       there is no et from upper zone
        UZET= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   EVAPT
     I                  (PET,BASETP,NBLKS,UZSN,AGWETP,KVARY,DAYFG,
     I                   VLEFG,LZETPM,MON,NXTMON,DAY,
     I                   NDAYS,LZSN,DELT60,NBLKSI,
     M                   AGWO,CEPS,UZS,UZSB,AGWS,GWVS,LZETP,RPARM,LZS,
     O                   REMPET,TAET,BASET,CEPE,UZET,UZETB,AGWET,LZET)
C
C     + + + PURPOSE + + +
C     Simulate evapotranspiration (ET)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NBLKS,NDAYS,NXTMON,VLEFG
      REAL       AGWET,AGWETP,AGWO,AGWS,BASET,BASETP,CEPE,CEPS,DELT60,
     $           GWVS,KVARY,LZET,LZETP,LZETPM(12),LZS,LZSN,NBLKSI,PET,
     $           REMPET,RPARM,TAET,UZET,UZETB(5),UZS,UZSB(5),UZSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PET    - ???
C     BASETP - ???
C     NBLKS  - number of blocks into which pls zones will be subdivided
C     UZSN   - upper zone nominal storage
C     UZSB   - ???
C     AGWETP - ???
C     KVARY  - ???
C     DAYFG  - flag for first day or day change
C     VLEFG  - ???
C     LZETPM - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     LZSN   - ???
C     DELT60 - simulation time interval in hours
C     NBLKSI - ???
C     AGWO   - ???
C     CEPS   - ???
C     UZS    - initial upper zone storage
C     AGWS   - ???
C     GWVS   - ???
C     LZETP  - ???
C     RPARM  - ???
C     LZS    - ???
C     REMPET - ???
C     TAET   - ???
C     BASET  - ???
C     CEPE   - ???
C     UZET   - ???
C     UZETB  - ???
C     AGWET  - ???
C     LZET   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + EXTERNALS + + +
      EXTERNAL   ETBASE, EVICEP, ETUZON, ETLZON, ETAGW
C
C     + + + END SPECIFICATIONS + + +
C
C     Taet is total actual et - inches/ivl
      TAET  = 0.0
C     Rempet is remaining potential et - inches/ivl
      REMPET= PET
      IF (REMPET .GT. 0.0) THEN
C       simulate et from baseflow
        CALL ETBASE
     I             (BASETP,
     M              AGWO,REMPET,TAET,
     O              BASET)
C
        IF (REMPET .GT. 0.0) THEN
C         simulate evaporation from interception
          CALL EVICEP
     M               (CEPS,REMPET,TAET,
     O                CEPE)
C
          IF (REMPET .GT. 0.0) THEN
C           simulate et from the upper zone
            CALL ETUZON
     I                 (NBLKS,UZSN,NBLKSI,
     M                  REMPET,UZS,UZSB,TAET,
     O                  UZET,UZETB)
C
            IF (REMPET .GT. 0.0) THEN
C               simulate et from active groundwater
              CALL ETAGW
     I                  (AGWETP,KVARY,
     M                   REMPET,AGWS,TAET,GWVS,
     O                   AGWET)
            ELSE
              AGWET= 0.0
            END IF
          ELSE
            UZET= 0.0
            IF (NBLKS .GT. 1) THEN
              DO 10 I= 1,NBLKS
                UZETB(I)= 0.0
 10           CONTINUE
            END IF
C
            AGWET= 0.0
          END IF
        ELSE
          CEPE= 0.0
          UZET= 0.0
          IF (NBLKS .GT. 1) THEN
            DO 20 I= 1,NBLKS
              UZETB(I)= 0.0
 20         CONTINUE
          END IF
C
          AGWET= 0.0
        END IF
      ELSE
        BASET= 0.0
        CEPE = 0.0
        UZET = 0.0
        IF (NBLKS .GT. 1) THEN
          DO 30 I= 1,NBLKS
            UZETB(I)= 0.0
 30      CONTINUE
        END IF
C
        AGWET= 0.0
      END IF
C
C     Et from lower zone is handled here because it must be called
C     every interval to make sure that seasonal variation in
C     parameter LZETP and recalculation of RPARM are correctly done
C     simulate ET from the lower zone
      CALL ETLZON
     I           (DAYFG,VLEFG,MON,DAY,LZETPM,NXTMON,
     I            NDAYS,LZSN,DELT60,
     M            LZETP,REMPET,RPARM,LZS,TAET,
     O            LZET)
C
      RETURN
      END
C
C
C
      SUBROUTINE   EVICEP
     M                   (CEPS,REMPET,TAET,
     O                    CEPE)
C
C     + + + PURPOSE + + +
C     Simulate evaporation from interception storage.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       CEPE,CEPS,REMPET,TAET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CEPS   - ???
C     REMPET - ???
C     TAET   - ???
C     CEPE   - ???
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CEPS .GT. 0.0) THEN
C       there is something in interception storage to evaporate
        IF (REMPET .GT. CEPS) THEN
C         evaporation from interception storage is limited
C         by quantity available
          CEPE= CEPS
          CEPS= 0.0
        ELSE
C         interception evaporation will not exhaust storage, so
C         empty at potential
          CEPE= REMPET
          CEPS= CEPS - CEPE
        END IF
C
C       update totals
        TAET  = TAET + CEPE
        REMPET= REMPET - CEPE
      ELSE
C       there is no evaporation from interception storage
        CEPE= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GWATER
     I                   (DEEPFR,GWI,KVARY,DAYFG,KGW,AGWLI,
     M                    AGWS,GWVS,
     O                    IGWI,AGWI,AGWO)
C
C     + + + PURPOSE + + +
C     Simulate groundwater behavior.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAYFG
      REAL       AGWI,AGWLI,AGWO,AGWS,DEEPFR,GWI,GWVS,IGWI,
     $           KGW,KVARY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DEEPFR - ???
C     GWI    - ???
C     KVARY  - ???
C     DAYFG  - flag for first day or day change
C     KGW    - ???
C     AGWLI  - ???
C     AGWS   - ???
C     GWVS   - ???
C     IGWI   - ???
C     AGWI   - ???
C     AGWO   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       AINFLO
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
      IF (GWI .GT. 0.0) THEN
C       groundwater inflow components
        IGWI= DEEPFR*GWI
        AGWI= GWI - IGWI
      ELSE
        IGWI= 0.0
        AGWI= 0.0
      END IF
C
C     active groundwater
C     total inflow includes lateral inflow
      AINFLO= AGWI + AGWLI
      AGWO  = 0.0
C     evaluate groundwater recharge parameter
C
C ICPRB change, Nov2009: Implementing nonlinear groundwater storage-discharge algorithm:
C   New expression for storage is: S(t) = [S(t0)^(1-beta) - (1-beta)*k^beta*(t-t0)]^(1/(1-beta))
C   First need to comment out old code for KVARY:
C      IF ((ABS(KVARY)) .GT. 0.0) THEN
C       update the index to variable groundwater slope
C        GWVS= GWVS + AINFLO
C        IF (DAYFG .EQ. 1) THEN
C         cut it back
C          IF (GWVS .GT. 0.0001) THEN
C            GWVS= GWVS*0.97
C          ELSE
C            GWVS= 0.0
C          END IF
C        END IF
C
C       groundwater outflow(baseflow)
C        IF (AGWS .GT. 1.0E-20) THEN
C          AGWO= KGW*(1.0 + KVARY*GWVS)*AGWS
C        END IF
C      ELSE
C  Next need to add new expression for change in storage, using KVARY = (beta-1):
        IF (AGWS .GT. 1.0E-20) THEN
C          AGWO= KGW*AGWS
           AGWO = (1.0-(1.0 + KVARY*KGW*AGWS**KVARY)
     o**(-1.0/KVARY))*AGWS
        END IF
C      END IF
C END ICPRB change
C
      AGWS= AGWS + (AINFLO - AGWO)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ICEPT
     I                (VCSFG,DAYFG,CEPSCM,MON,NXTMON,DAY,NDAYS,SUPY,
     M                 CEPSC,CEPS,
     O                 CEPO)
C
C     + + + PURPOSE + + +
C     Simulate the interception of moisture by vegetal or other
C     ground cover.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,VCSFG
      REAL       CEPO,CEPS,CEPSC,CEPSCM(12),SUPY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VCSFG  - ???
C     DAYFG  - flag for first day or day change
C     CEPSCM - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     SUPY   - ???
C     CEPSC  - ???
C     CEPS   - ???
C     CEPO   - ???
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG .EQ. 1) THEN
C       it is the first interval of the day
        IF (VCSFG .EQ. 1) THEN
C         interception capacity allowed to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate cepsc between two values from the
C         monthly array cepscm(12)
          CEPSC= DAYVAL(CEPSCM(MON),CEPSCM(NXTMON),DAY,NDAYS)
        ELSE
C         interception capacity does not vary throughout the year
C         cepsc value has been supplied by the run interpreter
        END IF
      END IF
C
C     add to interception storage
      CEPS= CEPS + SUPY
      IF (CEPS .GT. CEPSC) THEN
C       there is outflow from interception storage
        CEPO= CEPS - CEPSC
        CEPS= CEPSC
      ELSE
        CEPO= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   INTFLW
     I                   (DAYFG,VIRCFG,IRCM,MON,NXTMON,DAY,NDAYS,
     I                    DELT60,NBLKS,IFWI,IFWIB,IFWLI,NBLKSI,
     M                    IRC,IFWK1,IFWK2,IFWS,IFWSB,UZS,UZSB,
     O                    IFWO,IFWOB)
C
C     + + + PURPOSE + + +
C     Simulate interflow.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NBLKS,NDAYS,NXTMON,VIRCFG
      REAL       DELT60,IFWI,IFWIB(5),IFWK1,IFWK2,IFWLI,IFWO,IFWOB(5),
     $           IFWS,IFWSB(5),IRC,IRCM(12),NBLKSI,UZS,UZSB(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VIRCFG - ???
C     IRCM   - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     DELT60 - simulation time interval in hours
C     NBLKS  - number of blocks into which pls zones will be subdivided
C     IFWI   - ???
C     IFWIB  - ???
C     IFWLI  - ???
C     NBLKSI - ???
C     IRC    - ???
C     IFWK1  - ???
C     IFWK2  - ???
C     IFWS   - ???
C     IFWSB  - ???
C     UZS    - initial upper zone storage
C     UZSB   - ???
C     IFWO   - ???
C     IFWOB  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
      REAL       DUMMY,INFLO,KIFW,OUTFLO,STOR,VALUE
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  ALOG, EXP
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG .EQ. 1) THEN
C       it is the first interval of the day
        IF (VIRCFG .EQ. 1) THEN
C         interflow recession constant is allowed to vary
C         throughout the year
C         interpolate for the daily value
C         linearly interpolate irc between two values from the
C         monthly array ircm(12)
          IRC= DAYVAL(IRCM(MON),IRCM(NXTMON),DAY,NDAYS)
        ELSE
C         interflow recession constant does not vary throughout the
C         year.  irc value has been supplied by the run interpreter
        END IF
C
C       derive parameters used in routing
        DUMMY= 24.0/DELT60
        KIFW = -ALOG(IRC)/DUMMY
        IFWK2= 1.0 - EXP(-KIFW)
        IFWK1= 1.0 - (IFWK2/KIFW)
      END IF
C
      IF (NBLKS .EQ. 1) THEN
C       surface and near-surface zones of the land segment have not
C       been subdivided into blocks
        INFLO= IFWI + IFWLI
        VALUE= INFLO + IFWS
        IF (VALUE .GT. 0.00002) THEN
C         there is something worth routing
          IFWO= (IFWK1*INFLO) + (IFWK2*IFWS)
          IFWS= VALUE - IFWO
        ELSE
C         nothing worth routing-dump back to uzs
          IFWO= 0.0
          IFWS= 0.0
          UZS = UZS + VALUE
        END IF
      ELSE
C       surface and near-surface zones of the land segment have
C       been subdivided into blocks
C       initialize segment wide variables
        IFWO= 0.0
        IFWS= 0.0
C
        DO 10 I=1,NBLKS
          STOR = IFWSB(I)
          INFLO= IFWIB(I) + IFWLI
          VALUE= INFLO + STOR
          IF (VALUE .GT. 0.00002) THEN
C           there is something worth routing
            OUTFLO= (IFWK1*INFLO) + (IFWK2*STOR)
            STOR  = VALUE - OUTFLO
          ELSE
C           nothing worth routing dump back to uzs
            OUTFLO = 0.0
            STOR   = 0.0
            UZSB(I)= UZSB(I) + VALUE
          END IF
C
          IFWOB(I)= OUTFLO
          IFWSB(I)= STOR
C         increment land segment wide variables
          IFWS    = IFWS + STOR
          IFWO    = IFWO + OUTFLO
 10     CONTINUE
C
C       mean response of whole segment
        IF (IFWO .GT. 0.0)  THEN
          IFWO= IFWO*NBLKSI
        END IF
C
        IF (IFWS .GT. 0.0)  THEN
          IFWS= IFWS*NBLKSI
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   LZONE
     I                  (IPERC,LZRAT,
     M                   LZFRAC,LZS,RLZRAT,
     O                   LZI)
C
C     + + + PURPOSE + + +
C     Simulate lower zone behavior.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       IPERC,LZFRAC,LZI,LZRAT,LZS,RLZRAT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IPERC  - ???
C     LZRAT  - ???
C     LZFRAC - ???
C     LZS    - ???
C     RLZRAT - ???
C     LZI    - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       INDX
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IPERC .GT. 0.0) THEN
C       if necessary, recalculate the fraction of infiltration plus
C       percolation which will be taken by lower zone
        IF (ABS(LZRAT - RLZRAT) .GT. 0.02) THEN
C         it is time to recalculate
          RLZRAT= LZRAT
          IF (LZRAT .LE. 1.0) THEN
            INDX  = 2.5 - 1.5*LZRAT
            LZFRAC= 1.0 - LZRAT*(1.0/(1.0 + INDX))**INDX
          ELSE
            INDX  = 1.5*LZRAT - 0.5
            LZFRAC= (1.0/(1.0 + INDX))**INDX
          END IF
        ELSE
C         keep the old value of lzfrac
        END IF
C
C       lower zone inflow
        LZI= LZFRAC*IPERC
        LZS= LZS + LZI
      ELSE
C       no inflow
        LZI= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PROUTE
     I                   (PSUR,RTOPFG,DELT60,DEC,SRC,MESSU,MSGFL,LSNO,
     M                    SURS,ECNT1,
     O                    SURO)
C
C     + + + PURPOSE + + +
C     Determine how much potential surface detention (PSUR) runs
C     off in one simulation interval.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    RTOPFG,MESSU,MSGFL,LSNO,ECNT1
      REAL       DEC,DELT60,PSUR,SRC,SURO,SURS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PSUR   - ???
C     RTOPFG - ???
C     DELT60 - simulation time interval in hours
C     DEC    - ???
C     SRC    - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of error message file
C     LSNO   - line number in the opn sequence block of uci
C     ECNT1  - ???
C     SURS   - ???
C     SURO   - surface output
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    COUNT,SGRP,SCLU
      REAL       A1,CHANGE,DFSURO,DSURO,DUMMY,FACT,FSURO,RATIO,SSUPR,
     $           DTERM,STERM,SURSE,SURSM,SURSNW,TSURO,FFACT,DFACT
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSG,OMSTI,OMSTR
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 303
C
      IF (PSUR .GT. 0.0002) THEN
C       something is worth routing on the surface
        IF (RTOPFG .NE. 1) THEN
C         do routing the new way
C         estimate the rate of supply to the overland flow surface -
C         inches/hour
          SSUPR= (PSUR - SURS)/DELT60
C         determine equilibrium depth for this supply rate
          SURSE= 0.0
          IF (SSUPR .GT. 0.0) THEN
            SURSE= DEC*SSUPR**0.6
          END IF
C         determine runoff by iteration - newton's method
C         estimate the new surface storage
          SURSNW= PSUR
          SURO  = 0.0
          COUNT = 0
C         dountil relative error is small
 10       CONTINUE
            IF (SSUPR .GT. 0.0) THEN
              RATIO= SURSNW/SURSE
              IF (RATIO .LE. 1.0) THEN
C               flow is increasing
                FACT= 1.0 + 0.6*RATIO**3
              ELSE
                FACT= 1.6
              END IF
            ELSE
C             ratio is arbitrarily large for supply rate <= 0
              RATIO= 1.0E30
              FACT = 1.6
            END IF
C
C           coefficient in outflow equation
            A1    = DELT60*SRC*FACT**1.667
            STERM = SURSNW**1.667
            COUNT = COUNT + 1
            FFACT = A1*STERM
            FSURO = FFACT - SURO
            DFACT = -1.667*FFACT
            DFSURO= DFACT/SURSNW - 1.0
            IF (RATIO .LE. 1.0) THEN
C             additional term required in derivative wrt suro
              DTERM =DFACT/(FACT*SURSE)*1.8*RATIO**2
              DFSURO= DFSURO + DTERM
            END IF
            DSURO= FSURO/DFSURO
C
            IF (COUNT .GT. 100) THEN
C             error message -- didn't converge
              CALL OMSTI (LSNO)
              CALL OMSTR (SURSE)
              CALL OMSTR (SURO)
              CALL OMSTR (SURSNW)
              CALL OMSTR (FSURO)
              CALL OMSTR (DFSURO)
              CALL OMSTR (DSURO)
              SGRP = 3
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECNT1)
            END IF
C
            SURO= SURO - DSURO
C           boundary condition- don't let suro go negative
            IF (SURO .LE. 1.0E-10) THEN
              SURO= 0.0
            END IF
            SURSNW= PSUR - SURO
            CHANGE= 0.0
C
            IF ((ABS(SURO)) .GT. 0.0) THEN
              CHANGE= ABS(DSURO/SURO)
            END IF
C
          IF (CHANGE .GE. 0.01) GO TO 10
C         enddo
          SURS= SURSNW
        ELSE
C         do routing the way it is done in arm, nps, and hspx
C         estimate the rate of supply to the overland flow surface -
C         inches/ivl
          SSUPR= PSUR - SURS
C         estimate the mean surface detention storage over the
C         interval
          SURSM= (SURS + PSUR)*0.5
C         estimate the equilibrium detention depth for this supply
C         rate - surse
          IF (SSUPR .GT. 0.0) THEN
C           preliminary estimate of surse
            DUMMY= DEC*SSUPR**0.6
            IF (DUMMY .GT. SURSM) THEN
C             flow is increasing
              SURSE= DUMMY
              DUMMY= SURSM*(1.0 + 0.6*(SURSM/SURSE)**3)
            ELSE
C             flow on surface is at equilibrium or receding
              DUMMY= SURSM*1.6
            END IF
          ELSE
C           flow on the surface is receding - equilibrium detention is
C           assumed equal to actual detention
            DUMMY= SURSM*1.6
          END IF
C
          TSURO= DELT60*SRC*DUMMY**1.667
C
C         check the temporary calculation of surface outflow
          IF (TSURO .GT. PSUR) THEN
C           too much surface runoff is estimated
            SURO= PSUR
            SURS= 0.0
          ELSE
            SURO= TSURO
            SURS= PSUR- SURO
          END IF
        END IF
      ELSE
C       send what is on the overland flow plane straight to the
C       channel
        SURO= PSUR
        SURS= 0.0
      END IF
C
      IF (SURO .LE. 1.0E-10) THEN
C       fix bug in on PC - underflow leads to "not a number"
        SURO= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PWAACC
     I                   (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section PWATER.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER2 + + +
      INCLUDE  'cplpw.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I2,I3,I6,I7,IDUMMY,J
C
C     + + + EXTERNALS + + +
      EXTERNAL  ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
      I3= 3
      I6= 6
      I7= 7
C     handle flux groups containing segment-wide variables
C
      IDUMMY= SLIFP + ILIFP + ALIFP
      IF (IDUMMY .GT. 0) THEN
C       lateral input fluxes are being considered and printed
        CALL ACCVEC
     I             (I3,PWIF(1,FRMROW),
     M              PWIF(1,TOROW))
      END IF
C
      CALL ACCVEC
     I           (I6,PWCF1(1,FRMROW),
     M            PWCF1(1,TOROW))
C
      CALL ACCVEC
     I           (I7,PWCF3(1,FRMROW),
     M            PWCF3(1,TOROW))
C
      CALL ACCVEC
     I           (I6,PWCF5(1,FRMROW),
     M            PWCF5(1,TOROW))
C
      CALL ACCVEC
     I           (I2,PWCF7(1,FRMROW),
     M            PWCF7(1,TOROW))
C
      IF (NBLKS .GT. 1) THEN
C       handle variables dealing with individual blocks in the
C       segment
C
        DO 10 J= 1,2
          CALL ACCVEC
     I               (NBLKS,PWCF2(1,J,FRMROW),
     M                PWCF2(1,J,TOROW))
 10     CONTINUE
C
        CALL ACCVEC
     I             (NBLKS,PWCF4(1,FRMROW),
     M              PWCF4(1,TOROW))
C
        DO 20 J= 1,4
          CALL ACCVEC
     I               (NBLKS,PWCF6(1,J,FRMROW),
     M                PWCF6(1,J,TOROW))
 20     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PWAPRT
     I                   (UNITFG,LEV,PRINTU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units, calculate
C     water balance, and print out results.  The arrays PSTAT1 through
C     Pstat3, piflx, and pcflx1 through pcflx7 have identical
C     structures to PWST1 through PWST3, PWIF, and PWCF1 through
C     Pwcf7 apart from dropping the dimension lev for fluxes.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UNITFG,LEV,PRINTU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER2 + + +
      INCLUDE   'cplpw.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I1,I2,I3,I6,I7,IDUMMY,J
      REAL        DFACTA,DFACTB,PCFLX1(6),PCFLX2(5,2),PCFLX3(7),
     $            PCFLX4(5),PCFLX5(6),PCFLX6(5,4),PCFLX7(2),PIFLX(3),
     $            PPERS,PPERSS,PSTAT1(7),PSTAT2(5,3)
      CHARACTER*8 UNITID
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC, BALCHK
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** PWATER ***')
 2010 FORMAT (/,'   STATE VARIABLES',13X,
     $  '      PERS      CEPS      SURS       UZS      IFWS       LZS',
     $        '      AGWS      GWVS    INFFAC    PETADJ')
 2020 FORMAT (/,'   STATE VARIABLES',13X,
     $  '      PERS      CEPS      SURS       UZS      IFWS       LZS',
     $        '      AGWS      GWVS')
 2030 FORMAT (31X,8(8X,'IN'))
 2040 FORMAT (31X,8(8X,'MM'))
 2050 FORMAT ('     SEGMENT-WIDE',14X,F10.3,F10.3,F10.3,2F10.3,
     $        F10.3,4F10.3)
 2060 FORMAT ('     SEGMENT-WIDE',14X,F10.3,F10.3,F10.3,2F10.3,
     $         F10.3,2F10.3)
 2070 FORMAT (7X,'BLOCK',I2,37X,F10.3,2F10.3)
 2080 FORMAT (/,'   FLUXES')
 2090 FORMAT ('     EXTNL INFLOWS & OUTFLOWS  ',
     $  '  MOISTURE<-----OUTFLOWS TO STREAM---------->(SUM) DEEP PERC',
     $        '     <----LATERAL INFLOWS---->')
 2100 FORMAT (31X,'      SUPY      SURO      IFWO      AGWO',
     $        '      PERO      IGWI     SURLI     IFWLI     AGWLI')
 2110 FORMAT (31X,9(8X,'IN'))
 2120 FORMAT (31X,9(8X,'MM'))
 2130 FORMAT (7X,'SEGMENT-WIDE',12X,9F10.3)
 2140 FORMAT ('     EXTNL INFLOWS & OUTFLOWS  ',
     $  '  MOISTURE<-----OUTFLOWS TO STREAM---------->(SUM) DEEP PERC')
 2150 FORMAT (31X,'      SUPY      SURO      IFWO      AGWO',
     $        '      PERO      IGWI')
 2160 FORMAT (31X,6(8X,'IN'))
 2170 FORMAT (31X,6(8X,'MM'))
 2180 FORMAT (7X,'SEGMENT-WIDE',12X,6F10.3)
 2190 FORMAT (9X,'BLOCK',I2,25X,2F10.3)
 2200 FORMAT (/,'     EVAPOTRANSPIRATION         POTENTIAL<---------',
     $        '-----------ET COMPONENTS-------------------->(SUM)')
 2210 FORMAT (31X,'       PET      CEPE      UZET      LZET',
     $        '     AGWET     BASET      TAET')
 2220 FORMAT (31X,7(8X,'IN'))
 2230 FORMAT (31X,7(8X,'MM'))
 2240 FORMAT (7X,'SEGMENT-WIDE',12X,7F10.3)
 2250 FORMAT (9X,'BLOCK',I2,35X,F10.3)
 2260 FORMAT (/,'     INTERNAL FLUXES',11X,
     $  '      IFWI       UZI     INFIL      PERC       LZI      AGWI')
 2270 FORMAT (31X,6(8X,'IN'))
 2280 FORMAT (31X,6(8X,'MM'))
 2290 FORMAT (7X,'SEGMENT-WIDE',12X,6F10.3)
 2300 FORMAT (9X,'BLOCK',I2,15X,4F10.3)
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      I2= 2
      I3= 3
      I6= 6
      I7= 7
C     dimensionless variables do not need to be converted
C
C     assign conversion constant for dimensional variables with
C     depth units
      IF (UNITFG .EQ. 1) THEN
C       english system
        DFACTA= 1.0
      ELSE
C       metric system
        DFACTA= 25.4
      END IF
C
      DFACTB= 0.0
C
C     convert dimensional variables to external units
C
C     segment-wide state variables
      CALL TRNVEC
     I           (I7,PWST1,DFACTA,DFACTB,
     O            PSTAT1)
C
      PPERS = PWST4(1)*DFACTA
      PPERSS= PWST4(LEV)*DFACTA
C
      IDUMMY= SLIFP + ILIFP + ALIFP
      IF (IDUMMY .GT. 0) THEN
C       lateral inflows are being handled
        CALL TRNVEC
     I             (I3,PWIF(1,LEV),DFACTA,DFACTB,
     O              PIFLX)
      END IF
C
C     computed fluxes
      CALL TRNVEC
     I           (I6,PWCF1(1,LEV),DFACTA,DFACTB,
     O            PCFLX1)
C
      CALL TRNVEC
     I           (I7,PWCF3(1,LEV),DFACTA,DFACTB,
     O            PCFLX3)
C
      CALL TRNVEC
     I           (I6,PWCF5(1,LEV),DFACTA,DFACTB,
     O            PCFLX5)
C
      CALL TRNVEC
     I           (I2,PWCF7(1,LEV),DFACTA,DFACTB,
     O            PCFLX7)
C
      IF (NBLKS .GT. 1) THEN
C       handle variables which apply to individual blocks in the segment
        DO 10 J= 1,3
          CALL TRNVEC
     I               (NBLKS,PWST2(1,J),DFACTA,DFACTB,
     O                PSTAT2(1,J))
 10     CONTINUE
C
        DO 20 J= 1,2
          CALL TRNVEC
     I               (NBLKS,PWCF2(1,J,LEV),DFACTA,DFACTB,
     O                PCFLX2(1,J))
 20     CONTINUE
C
        CALL TRNVEC
     I             (NBLKS,PWCF4(1,LEV),DFACTA,DFACTB,
     O              PCFLX4)
C
        DO 30 J= 1,4
          CALL TRNVEC
     I               (NBLKS,PWCF6(1,J,LEV),DFACTA,DFACTB,
     O                PCFLX6(1,J))
 30     CONTINUE
C
      END IF
C
C     write to unit PRINTU
C
      WRITE (PRINTU,2000)
C
      IF (CSNOFG .EQ. 1) THEN
        WRITE (PRINTU,2010)
      ELSE
        WRITE (PRINTU,2020)
      END IF
C
      IF (UNITFG .EQ. 1) THEN
        WRITE (PRINTU,2030)
      ELSE
        WRITE (PRINTU,2040)
      END IF
C
      IF (CSNOFG .EQ. 1) THEN
        WRITE (PRINTU,2050)  PPERS, PSTAT1, PWST3
      ELSE
        WRITE (PRINTU,2060)  PPERS, PSTAT1
      END IF
C
      IF (NBLKS .GT. 1) THEN
C       write state variables for individual blocks
        DO 40 I= 1,NBLKS
          WRITE (PRINTU,2070)  I, (PSTAT2(I,J),J=1,3)
 40     CONTINUE
      END IF
C
C     fluxes
      WRITE (PRINTU,2080)
C     external inflows & outflows
      IDUMMY= SLIFP + ILIFP + ALIFP
      IF (IDUMMY .GT. 0) THEN
C       lateral inflow is considered
        WRITE (PRINTU,2090)
        WRITE (PRINTU,2100)
C
        IF (UNITFG .EQ. 1) THEN
          WRITE (PRINTU,2110)
        ELSE
          WRITE (PRINTU,2120)
        END IF
C
        WRITE (PRINTU,2130)  PCFLX1, PIFLX
      ELSE
C       no lateral inflow considered
        WRITE (PRINTU,2140)
        WRITE (PRINTU,2150)
C
        IF (UNITFG .EQ. 1) THEN
          WRITE (PRINTU,2160)
        ELSE
          WRITE (PRINTU,2170)
        END IF
C
        WRITE (PRINTU,2180)  PCFLX1
      END IF
C
      IF (NBLKS .GT. 1) THEN
C       write fluxes for individual blocks
        DO 50 I= 1,NBLKS
          WRITE (PRINTU,2190)  I, (PCFLX2(I,J),J=1,2)
 50     CONTINUE
      END IF
C
C     evapotranspiration printout
C
      WRITE (PRINTU,2200)
      WRITE (PRINTU,2210)
C
      IF (UNITFG .EQ. 1) THEN
        WRITE (PRINTU,2220)
      ELSE
        WRITE (PRINTU,2230)
      END IF
C
      WRITE (PRINTU,2240)  PCFLX3
C
      IF (NBLKS .GT. 1) THEN
        DO 60 I= 1,NBLKS
          WRITE (PRINTU,2250)  I, PCFLX4(I)
 60     CONTINUE
      END IF
C
C     internal fluxes
      WRITE (PRINTU,2260)
C
      IF (UNITFG .EQ. 1) THEN
        WRITE (PRINTU,2270)
      ELSE
        WRITE (PRINTU,2280)
      END IF
C
      WRITE (PRINTU,2290)  PCFLX5
C
      IF (NBLKS .GT. 1) THEN
        DO 70 I= 1,NBLKS
          WRITE (PRINTU,2300)  I, (PCFLX6(I,J),J=1,4)
 70     CONTINUE
      END IF
C
C     water balance check and report
      IF (UNITFG .EQ. 1) THEN
C       english
        UNITID= '  INCHES'
      ELSE
C       metric
        UNITID= '      MM'
      END IF
C
      I= 1
      CALL BALCHK
     I           (I,LSNO,DATIM,MESSU,PRINTU,MSGFL,
     I            PPERSS,PPERS,PCFLX7(1),PCFLX7(2),UNITID,I1,
     M            PWWCNT(1))
C
      RETURN
      END
C
C
C
      SUBROUTINE   PWARST
     I                   (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables
C     used in material balance check for section PWATER.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER2 + + +
      INCLUDE    'cplpw.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I2,I3,I6,I7,J
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
      I3= 3
      I6= 6
      I7= 7
C     handle flux groups containing segment-wide variables
C
C     lateral input fluxes are being printed
      CALL SETVEC
     I           (I3,0.0,
     O            PWIF(1,LEV))
C
      CALL SETVEC
     I           (I6,0.0,
     O            PWCF1(1,LEV))
C
      CALL SETVEC
     I           (I7,0.0,
     O            PWCF3(1,LEV))
C
      CALL SETVEC
     I           (I6,0.0,
     O            PWCF5(1,LEV))
C
      CALL SETVEC
     I           (I2,0.0,
     O            PWCF7(1,LEV))
C
      IF (NBLKS .GT. 1) THEN
C       handle variables dealing with individual blocks in a
C       segment
        DO 10 J= 1,2
          CALL SETVEC
     I               (NBLKS,0.0,
     O                PWCF2(1,J,LEV))
 10     CONTINUE
C
        CALL SETVEC
     I             (NBLKS,0.0,
     O              PWCF4(1,LEV))
C
        DO 20 J= 1,4
          CALL SETVEC
     I               (NBLKS,0.0,
     O                PWCF6(1,J,LEV))
C
 20     CONTINUE
      END IF
C
C     keep present water storage in state variable used for
C     material balance check
      PWST4(LEV)= PWST4(1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PWATPB
C
C     + + + PURPOSE + + +
C     Handle section PWATER.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER2 + + +
      INCLUDE    'cplpw.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
C     Segment-wide quantities
      IF (RPMFP .GE. 1) THEN
        PAD(RPMFP + IVL1) = RPARM
      END IF
C
      IF (SURIFP .GE. 1)  THEN
        PAD(SURIFP + IVL1)= SURI
      END IF
C
      IF (INFFP .GE. 1)   THEN
        PAD(INFFP + IVL1) = INFIL
      END IF
C
      IF (UZIFP .GE. 1)   THEN
        PAD(UZIFP + IVL1) = UZI
      END IF
C
      IF (IIFP .GE. 1)    THEN
        PAD(IIFP + IVL1)  = IFWI
      END IF
C
      IF (SOFP .GE. 1)    THEN
        PAD(SOFP + IVL1)  = SURO
      END IF
C
      IF (IOFP .GE. 1)    THEN
        PAD(IOFP + IVL1)  = IFWO
      END IF
C
      IF (PCFP .GE. 1)    THEN
        PAD(PCFP + IVL1)  = PERC
      END IF
C
      IF (LZIFP .GE. 1)   THEN
        PAD(LZIFP + IVL1) = LZI
      END IF
C
      IF (AIFP .GE. 1)    THEN
        PAD(AIFP + IVL1)  = AGWI
      END IF
C
      IF (IGIFP .GE. 1)   THEN
        PAD(IGIFP + IVL1) = IGWI
      END IF
C
      IF (AOFP .GE. 1)    THEN
        PAD(AOFP + IVL1)  = AGWO
      END IF
C
      IF (POFP .GE. 1)    THEN
        PAD(POFP + IVL1)  = PERO
      END IF
C
      IF (PETFP .GE. 1)   THEN
        PAD(PETFP + IVL1) = PET
      END IF
C
      IF (TAETFP .GE. 1)  THEN
        PAD(TAETFP + IVL1)= TAET
      END IF
C
      IF (BASEFP .GE. 1)  THEN
        PAD(BASEFP + IVL1)= BASET
      END IF
C
      IF (CEFP .GE. 1)    THEN
        PAD(CEFP + IVL1)  = CEPE
      END IF
C
      IF (UEFP .GE. 1)    THEN
        PAD(UEFP + IVL1)  = UZET
      END IF
C
      IF (AEFP .GE. 1)    THEN
        PAD(AEFP + IVL1)  = AGWET
      END IF
C
      IF (LZETFP .GE. 1)  THEN
        PAD(LZETFP + IVL1)= LZET
      END IF
C
      IF (NBLKS .GT. 1) THEN
        DO 10 I= 1,NBLKS
C         quantities which apply to block i
          IF (INFBFP(I) .GE. 1)  THEN
            PAD(INFBFP(I) + IVL1)= INFILB(I)
          END IF
C
          IF (UZIBFP(I) .GE. 1)  THEN
            PAD(UZIBFP(I) + IVL1)= UZIB(I)
          END IF
C
          IF (IIBFP(I) .GE. 1)   THEN
            PAD(IIBFP(I) + IVL1) = IFWIB(I)
          END IF
C
          IF (SOBFP(I) .GE. 1)   THEN
            PAD(SOBFP(I) + IVL1) = SUROB(I)
          END IF
C
          IF (IOBFP(I) .GE. 1)   THEN
            PAD(IOBFP(I) + IVL1) = IFWOB(I)
          END IF
C
          IF (PCBFP(I) .GE. 1)   THEN
            PAD(PCBFP(I) + IVL1) = PERCB(I)
          END IF
C
          IF (UEBFP(I) .GE. 1)   THEN
            PAD(UEBFP(I) + IVL1) = UZETB(I)
          END IF
 10     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE  PWATPT
C
C     + + + PURPOSE + + +
C     Handle section PWATER.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWATER2 + + +
      INCLUDE   'cplpw.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
C     segment-wide quantities
      IF (CEPSFP .GE. 1) THEN
        PAD(CEPSFP + IVL1)= CEPS
      END IF
C
      IF (UZSFP .GE. 1) THEN
        PAD(UZSFP +IVL1)= UZS
      END IF
C
      IF (LZSFP .GE. 1) THEN
        PAD(LZSFP + IVL1)= LZS
      END IF
C
      IF (SSFP .GE. 1) THEN
        PAD(SSFP + IVL1)= SURS
      END IF
C
      IF (ISFP .GE. 1) THEN
        PAD(ISFP + IVL1)= IFWS
      END IF
C
      IF (AGWSFP .GE. 1) THEN
        PAD(AGWSFP + IVL1)= AGWS
      END IF
C
      IF (PERSFP .GE. 1) THEN
        PAD(PERSFP + IVL1)= PERS
      END IF
C
      IF (NBLKS .GT. 1) THEN
        DO 10 I= 1,NBLKS
C         quantities belonging to block i
          IF (UZSBFP(I) .GE. 1) THEN
             PAD(UZSBFP(I) + IVL1)= UZSB(I)
          END IF
C
          IF (SSBFP(I) .GE. 1) THEN
            PAD(SSBFP(I) + IVL1)= SURSB(I)
          END IF
C
          IF (ISBFP(I) .GE. 1) THEN
            PAD(ISBFP(I) + IVL1)= IFWSB(I)
          END IF
 10     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SURFAC
     I                   (LZRAT,INFILT,INFEXP,INFFAC,INFILD,FSMSFG,
     I                    DAYFG,VNNFG,NSURM,LSUR,SLSUR,VIFWFG,INTFWM,
     I                    NBLKS,MSUPY,MSUPYB,UZSN,UZS,DELT60,UZRA,
     I                    INTGRL,RTOPFG,UZFG,UZSB,MON,NXTMON,DAY,NDAYS,
     I                    NBLKSI,LSNO,MESSU,MSGFL,DATIM,
     M                    DEC,SRC,NSUR,INTFW,SURS,SURSB,ECNT1,
     O                    INFIL,UZI,IFWI,SURO,INFILB,UZIB,IFWIB,
     O                    SUROB)
C
C     + + + PURPOSE + + +
C     Distribute the water available for infiltration and runoff -
C     units of fluxes are in./ivl
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,FSMSFG,MON,NBLKS,NDAYS,NXTMON,RTOPFG,
     $           VIFWFG,VNNFG,LSNO,MESSU,DATIM(5),
     $           ECNT1,MSGFL,UZFG
      REAL       DEC,DELT60,IFWI,IFWIB(5),INFEXP,INFIL,INFILB(5),
     $           INFILD,INFILT,INFFAC,INTFW,INTFWM(12),INTGRL(10),
     $           LSUR,LZRAT,MSUPY,MSUPYB(5),NBLKSI,NSUR,NSURM(12),
     $           SLSUR,SRC,SURO,SUROB(5),SURS,SURSB(5),UZI,UZIB(5),
     $           UZRA(10),UZSB(5),UZS,UZSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LZRAT  - ???
C     INFILT - ???
C     INFEXP - ???
C     INFFAC - ???
C     INFILD - ???
C     FSMSFG - ???
C     DAYFG  - flag for first day or day change
C     VNNFG  - ???
C     NSURM  - ???
C     LSUR   - ???
C     SLSUR  - ???
C     VIFWFG - ???
C     INTFWM - ???
C     NBLKS  - number of blocks into which pls zones will be subdivided
C     MSUPY  - ???
C     MSUPYB - ???
C     UZSN   - upper zone nominal storage
C     UZS    - initial upper zone storage
C     DELT60 - simulation time interval in hours
C     UZRA   - ???
C     INTGRL - ???
C     RTOPFG - ???
C     UZFG   - ???
C     UZSB   - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     NBLKSI - ???
C     LSNO   - line number in the opn sequence block of uci
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     DEC    - ???
C     SRC    - ???
C     NSUR   - ???
C     INTFW  - ???
C     SURS   - ???
C     SURSB  - ???
C     ECNT1  - ???
C     INFIL  - ???
C     UZI    - ???
C     IFWI   - ???
C     SURO   - ???
C     INFILB - ???
C     UZIB   - ???
C     IFWIB  - ???
C     SUROB  - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I4
      REAL       DELTI,IBAR,IMAX,IMIN,IMNB,IMXB,RATIO
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + INRINSICS + + +
      INTRINSIC  REAL,SQRT
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL,DISPOS
C
C     + + + END SPECIFICATIONS + + +
C
C     Establish locations of sloping lines on infiltration/inflow/sur
C     runoff figure.  Prefix "I" refers to "infiltration" line
C     Ibar is the mean infiltration capacity over the segment
C     internal units of INFILT are inches/ivl
      IBAR= INFILT/(LZRAT**INFEXP)
C
      IF (INFFAC .LT. 1.0) THEN
C       adjust ibar to account for frozen ground
        IBAR= IBAR*INFFAC
      END IF
C
C     find the maximum and minimum infiltration capacities
      IMAX= IBAR*INFILD
C     Infild is an input parameter - ratio of maximum to mean
C     infiltration capacity
      IMIN= IBAR - (IMAX - IBAR)
C
      IF (FSMSFG .EQ. 1 .OR. DAYFG .EQ. 1) THEN
C       it is time to recompute any varying parameters
        IF (VNNFG .EQ. 1) THEN
C         mannings n is allowed to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate nsur between two values from the
C         monthly array nsurm(12)
          NSUR= DAYVAL(NSURM(MON),NSURM(NXTMON),DAY,NDAYS)
        ELSE
C         mannings n does not vary throughout the year.
C         nsur value has been supplied by the run interpreter
        END IF
C
C       calculate parameters for routing surface runoff
        DEC= 0.00982*(NSUR*LSUR/SQRT(SLSUR))**0.6
        SRC= 1020.0*(SQRT(SLSUR)/(NSUR*LSUR))
C
        IF (VIFWFG .EQ. 1) THEN
C         interflow parmeters are allowed to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate intfw between two values from the
C         monthly array intfwm(12)
          INTFW= DAYVAL(INTFWM(MON),INTFWM(NXTMON),DAY,NDAYS)
        ELSE
C         interflow parameter does not vary throughout the year.
C         intfw value has been supplied by the run interpreter
        END IF
      END IF
C
C     Ratio is the ratio of the ordinates of the "infiltration +
C     interflow" line to those of the "infiltration" line
      RATIO= INTFW*(2.0**LZRAT)
      IF (RATIO .LE. 1.0) THEN
        RATIO= 1.0001
      END IF
C
C     now allocate the water among the alternative destinations
      IF (NBLKS .EQ. 1) THEN
C       surface and near-surface zones of the land-segment have
C       not been subdivided into blocks
C       determine what happens to the moisture supply
        CALL DISPOS
     I             (IMIN,IMAX,RATIO,MSUPY,UZSN,UZS,DELT60,DEC,
     I              SRC,UZRA,INTGRL,RTOPFG,UZFG,
     I              LSNO,MESSU,MSGFL,DATIM,
     M              SURS,ECNT1,
     O              INFIL,UZI,IFWI,SURO)
      ELSE
C       surface and near surface zones of the land segment have
C       been subdivided into blocks - handle them one at a time and
C       use mean values to indicate overall response of the land
C       segment
C       initialize variables which indicate overall behavior
        INFIL= 0.0
        UZI  = 0.0
        IFWI = 0.0
        SURS = 0.0
        SURO = 0.0
C
        DO 10 I=1,NBLKS
C         find, by interpolation, the minimum and maximum values
C         of the "infiltration" line in the infiltration/interflow/
C         surface runoff figure, for this block
          DELTI= (IMAX - IMIN)*NBLKSI
          I4   =I - 1
          IMNB = IMIN + REAL(I4)*DELTI
          IMXB = IMNB + DELTI
C
C         determine what happens to the moisture supply on this block
          CALL DISPOS
     I               (IMNB,IMXB,RATIO,MSUPYB(I),UZSN,UZSB(I),DELT60,
     I                DEC,SRC,UZRA,INTGRL,RTOPFG,UZFG,
     I                LSNO,MESSU,MSGFL,DATIM,
     M                SURSB(I),ECNT1,
     O                INFILB(I),UZIB(I),IFWIB(I),SUROB(I))
C
C         accumulate values across the various blocks
          INFIL= INFIL + INFILB(I)
          UZI  = UZI + UZIB(I)
          IFWI = IFWI + IFWIB(I)
          SURS = SURS + SURSB(I)
          SURO = SURO + SUROB(I)
 10     CONTINUE
C
C       convert accumulated values to mean values
        INFIL= INFIL*NBLKSI
        UZI  = UZI*NBLKSI
        IFWI = IFWI*NBLKSI
        SURS = SURS*NBLKSI
        SURO = SURO*NBLKSI
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   UZINF
     I                  (PDRO,UZSN,UZS,UZRA,INTGRL,LSNO,
     I                   MESSU,MSGFL,DATIM,
     M                   ECNT1,
     O                   UZI)
C
C     + + + PURPOSE + + +
C     Compute the inflow to the upper zone during this time interval.
C     Do this using a table look-up to handle the non-analytic integral
C     given in supporting documentation.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECNT1,MSGFL,LSNO,MESSU,DATIM(5)
      REAL       INTGRL(10),PDRO,UZI,UZRA(10),UZS,UZSN
C
C     + + + ARGUMENT DEFINTIONS + + +
C     PDRO   - ???
C     UZSN   - upper zone nominal storage
C     UZS    - initial upper zone storage
C     UZRA   - ???
C     INTGRL - ???
C     LSNO   - line number in the opn sequence block of uci
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     ECNT1  - ???
C     UZI    - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,SCLU,SGRP
      REAL       INTGA,INTGB,INTG1,INTG2,UZRAA,UZRAB,UZR1,UZR2
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSTR,OMSG,OMSTI,OMSTD
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 303
C     find the value of the integral at initial UZRA
      UZRAA= UZS/UZSN
C     in FORTRAN do not use an ordinary do loop, since the I counter
C     is needed following the loop
C     dountil
      I= 0
 10   CONTINUE
        I   = I + 1
        UZR1= UZRA(I)
        UZR2= UZRA(I + 1)
      IF (UZR2 .LT. UZRAA .AND. I .NE. 9)  GO TO 10
C     enddo
C
      IF (UZR2 .LT. UZRAA) THEN
C       say that the solution could not be found
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (UZR1)
        CALL OMSTR (UZR2)
        CALL OMSTR (UZRAA)
        SGRP = 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECNT1)
      END IF
C
C     do linear interpolation
      INTGA= INTGRL(I) + (UZRAA - UZR1)/(UZR2 - UZR1)*(INTGRL(I + 1)
     $       - INTGRL(I))
C
C     now find the value of the integral at the end of the time step
      INTGB= INTGA + PDRO/UZSN
C
C     find the value of UZRA after inflow is admitted to the upper
C     zone - reverse the table look-up process
C     start with same I just used
C     in FORTRAN do not use an ordinary do loop, since the I counter
C     is needed following the loop
C     dountil
      I= 0
 20   CONTINUE
        I    = I + 1
        INTG1= INTGRL(I)
        INTG2= INTGRL(I + 1)
      IF (INTG2 .LT. INTGB .AND. I .NE. 9)  GO TO 20
C     enddo
C
      IF (INTG2 .LT. INTGB) THEN
C       say that the solution could not be found
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (INTG1)
        CALL OMSTR (INTG2)
        CALL OMSTR (INTGB)
        SGRP = 2
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECNT1)
      END IF
C
C     do linear interpolation
      UZRAB= UZRA(I) + (INTGB  - INTG1)/(INTG2 - INTG1)*
     $       (UZRA(I + 1) - UZRA(I))
C
C     now it is known how much the inflow will change UZRA -
C     translate this to a quantity of inflow, inches/ivl
      UZI= (UZRAB - UZRAA)*UZSN
C
      RETURN
      END
C
C
C
      SUBROUTINE   UZINF2
     I                   (PDRO,UZSN,UZS,
     O                    UZI)
C
C     + + + PURPOSE + + +
C     Compute inflow to upper zone during this interval, using
C     "fully forward" type algorithm  as used in HSPX,ARM and NPS.
C     Note:  although this method should give results closer to those
C     produced by HSPX, etc , its output will be more sensitive to
C     Delt than that given by subroutine uzinf
C
C     + + + DUMMY ARGUMENTS + + +
      REAL    PDRO,UZSN,UZS,UZI
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PDRO   - ???
C     UZSN   - upper zone nominal storage
C     UZS    - initial upper zone storage
C     UZI    - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL    UZRAT,K1,K2,UZFRAC
C
C     + + + END SPECIFICATIONS + + +
C
      UZRAT= UZS/UZSN
C
      IF (UZRAT .LT. 2.0) THEN
        K1    = 3.0 - UZRAT
        UZFRAC= 1.0 - (UZRAT*0.5)*((1.0/(1.0 + K1))**K1)
      ELSE
C
        K2    = (2.0*UZRAT) - 3.0
        UZFRAC= (1.0/(1.0 + K2))**K2
      END IF
C
      UZI= PDRO*UZFRAC
C
      RETURN
      END
C
C
C
      SUBROUTINE   UZONE
     I                  (NBLKS,UZSN,UZI,UZIB,INFILT,INFFAC,
     I                   LZRAT,NBLKSI,
     M                   UZS,UZSB,
     O                   PERC,PERCB)
C
C     + + + PURPOSE + + +
C     Simulate upper zone behavior.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NBLKS
      REAL       INFFAC,INFILT,LZRAT,NBLKSI,PERC,PERCB(5),UZI,
     $           UZIB(5),UZS,UZSB(5),UZSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NBLKS  - number of blocks into which pls zones will be subdivided
C     UZSN   - upper zone nominal storage
C     UZI    - ???
C     UZIB   - ???
C     INFILT - ???
C     INFFAC - ???
C     LZRAT  - ???
C     NBLKSI - ???
C     UZS    - initial upper zone storage
C     UZSB   - ???
C     PERC   - ???
C     PERCB  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + EXTERNALS + + +
      EXTERNAL   UZONES
C
C     + + + END SPECIFICATIONS + + +
C
      IF (NBLKS .EQ. 1) THEN
C       surface and near-surface zones of the land segment have not
C       been subdivided into blocks
        CALL UZONES
     I             (UZSN,UZI,INFILT,INFFAC,LZRAT,
     M              UZS,
     O              PERC)
      ELSE
C       surface and near-surface zones of the land segment have
C       been subdivided into blocks
C       initialize segment-wide variables
        UZS = 0.0
        PERC= 0.0
C
        DO 10 I= 1,NBLKS
          CALL UZONES
     I               (UZSN,UZIB(I),INFILT,INFFAC,LZRAT,
     M                UZSB(I),
     O                PERCB(I))
          UZS = UZS+ UZSB(I)
          PERC= PERC+ PERCB(I)
 10     CONTINUE
C       convert totals to averages
        UZS = UZS*NBLKSI
        PERC= PERC*NBLKSI
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   UZONES
     I                   (UZSN,UZI,INFILT,INFFAC,LZRAT,
     M                    UZS,
     O                    PERC)
C
C     + + + PURPOSE + + +
C     This is a subsidiary subroutine for computing upper zone behavior
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       INFFAC,INFILT,LZRAT,PERC,UZI,UZS,UZSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UZSN   - upper zone nominal storage
C     UZI    - ???
C     INFILT - ???
C     INFFAC - ???
C     LZRAT  - ???
C     UZS    - initial upper zone storage
C     PERC   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       UZRAT
C
C     + + + END SPECIFICATIONS + + +
C
C     percolation will be based on UZRAT at the start of the interval
      UZRAT= UZS/UZSN
C
C     add inflow to UZS
      UZS  = UZS+ UZI
C
C     percolation
      IF ((UZRAT - LZRAT) .GT. 0.01) THEN
C       simulate percolation
C       units of perc are inches/ivl
        PERC= 0.1*INFILT*INFFAC*UZSN*(UZRAT - LZRAT)**3
C
        IF (PERC .GT. UZS) THEN
C         computed value is too high so merely empty storage
          PERC= UZS
          UZS = 0.0
        ELSE
C         computed value is ok
          UZS= UZS - PERC
        END IF
      ELSE
C       assume there is no percolation
        PERC= 0.0
      END IF
C
      RETURN
      END
