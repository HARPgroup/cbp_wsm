C
C
C
      SUBROUTINE   PIWTGS
     I                   (OUTLEV)
C
C     + + + PURPOSE + + +
C     Process input for section iwtgas
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OUTLEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTLEV - run interp output level
C
C      + + + COMMON BLOCKS- SCRTCH, VERSION IWTGAS1 + + +
      INCLUDE    'cilig.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TBNO,TBSB,NVAL,IVAL(2)
      REAL      ELEV,RVAL(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITABLE,RTABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION IWTGAS')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION IWTGAS')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
C     process values in table-type iwt-parm1
      TBNO= 25
      TBSB= 1
      NVAL= 2
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             IVAL)
      WTFVFG= IVAL(1)
C
      IF (IWATFG.EQ.0) THEN
        CSNOFG= IVAL(2)
      END IF
C
C     process values in table-type iwt-parm2
      TBNO= 26
      TBSB= 1
      NVAL= 3
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             RVAL)
      ELEV= RVAL(1)
C
C     compute factor for correcting dissolved gas
C     saturated concentrations for altitude
      ELEVGC= ((288.0- 0.00198*ELEV)/288.0)**5.256
C
C     dissolved gas concentrations in interflow and groundwater
      AWTF= RVAL(2)
      BWTF= RVAL(3)
C
      IF (WTFVFG.EQ.1) THEN
C       check for monthly values of awtf - table-type mon-awtf
        TBNO= 27
        TBSB= 1
        NVAL= 12
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               AWTFM)
      END IF
C
      IF (WTFVFG.EQ.1) THEN
C       check for monthly values of bwtf -
C       table-type mon-bwtf
        TBNO= 28
        TBSB= 1
        NVAL= 12
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               BWTFM)
      END IF
C
C     initial values - table-type iwt-init
      TBNO= 29
      TBSB= 1
      NVAL= 3
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             IGST1)
C
      IF (OUTLEV.GT.1) THEN
C       end processing message
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWTGAS
C
C     + + + PURPOSE + + +
C     Estimate water temperature, dissolved oxygen, and carbon
C     dioxide in the outflows from a impervious land
C     segment. calculate associated fluxes through exit gate
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWTGAS2 + + +
      INCLUDE 'cilig.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + + +
      REAL      ABSTMP,DUMMY
C
C     + + + FUNCTIONS + + + +
      REAL      DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL  DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
C     get hydrological time series
      IF (IWATFG.EQ.0) THEN
C       get time series from inpad
        SURO= PAD(SOFP+IVL1)
      ELSE
C       suro is available from iwater
      END IF
C
      IF (AIRTFG.EQ.0) THEN
C       get air temperature in deg f from the inpad
        AIRTMP= PAD(AIRTFP+IVL1)
      ELSE
C       air temperatures, in degrees f, are available from section atemp
      END IF
C
C     convert to centigrade
      AIRTC= (AIRTMP-32.0)*0.555
C
C     obtain latest values for temperature calculation parameters
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
        IF (WTFVFG.EQ.1) THEN
C         water temperature regression parameters are allowed to
C         vary throughout the year
C         interpolate for the daily values
C         linearly interpolate awtf between two values from the
C         monthly array awtfm(12)
          AWTF= DAYVAL(AWTFM(MON),AWTFM(NXTMON),DAY,NDAYS)
C         linearly interpolate bwtf between two values from the
C         monthly array bwtfm(12)
          BWTF= DAYVAL(BWTFM(MON),BWTFM(NXTMON),DAY,NDAYS)
        ELSE
C         water temperature regression parameters do not vary
C         throughout the year. values for awtf and bwtf have been
C         supplied by the run interpreter
        END IF
      END IF
C
      IF (SURO.GT.0.0) THEN
C       there is surface outflow
C       calculate impervious surface outflow temperature - in deg. c
        SOTMP= AWTF+ BWTF*AIRTC
C
        IF (SOTMP.LT.0.5) THEN
C         don't let water temp drop below 0.5 deg c
          SOTMP= 0.5
        END IF
C
        IF (CSNOFG.EQ.1) THEN
C         adjust surface outflow temperature if snowmelt is occurring
          IF (SNOWFG.EQ.0) THEN
            WYIELD= PAD(WYFP+IVL1)
          ELSE
C           wyield is available from snow
          END IF
C
          IF (WYIELD.GT.0.0) THEN
C           snowmelt is occuring
            SOTMP= 0.5
          END IF
        END IF
      ELSE
C       for zero outflow, report outflow temperatures as "undefined"
        SOTMP= -1.0E30
      END IF
C
C     compute outflow of heat energy in water - units are deg. c-in./ivl
      SOHT= SOTMP*SURO
C
C     calculate dissolved oxygen and carbon dioxide concentrations
C     in impervious surface runoff - units are mg/l of do and
C     and mg c/l of co2
      IF (SURO.GT.0.0) THEN
C       there is surface outflow, oxygen calculation
        DUMMY = SOTMP*(0.007991-0.77774E-4*SOTMP)
        SODOX = (14.652+ SOTMP*(-0.41022+ DUMMY))*ELEVGC
C
C       carbon dioxide calculation
        ABSTMP= SOTMP+ 273.16
        DUMMY = 2385.73/ABSTMP- 14.0184+ 0.0152642*ABSTMP
        SOCO2 = 10.0**DUMMY*3.16E-04*ELEVGC*12000.0
      ELSE
C       there is no surface outflow - values are undefined
        SODOX= -1.0E30
        SOCO2= -1.0E30
      END IF
C
C     calculate outflow mass of dox - units are mg-in./l-ivl
      SODOXM= SODOX*SURO
C
C     calculate outflow mass of co2 - units are mg-in./l-ivl
      SOCO2M= SOCO2*SURO
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWGACC
     I                   (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section iwtgas
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWTGAS2 + + +
      INCLUDE  'cilig.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I3
C
C     + + + EXTERNALS + + +
      EXTERNAL  ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I3=3
      CALL ACCVEC (I3,IGCF1(1,FRMROW),
     M             IGCF1(1,TOROW))
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWGPRT
     I                   (UNITFG,LEV,PRINTU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units.
C     Note: local arrays have identical sizes and structures to the
C     corresponding arrays in the osv apart from dropping
C     the dimension lev for fluxes
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEV,PRINTU,UNITFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWTGAS2 + + +
      INCLUDE  'cilig.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL      EFACTA,MFACTA,PCFLX1,PCFLX2,PCFLX3,PSTAT,
     $          TFACTA,TFACTB
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** IWTGAS ***')
 2010 FORMAT (/,'   STATE VARIABLES',13X,'WATER TEMP',11X,
     $        'DISSOLVED:   OXYGEN      CARBON DIOXIDE')
 2020 FORMAT (36X,'SOTMP',25X,'SODOX',15X,'SOCO2')
 2030 FORMAT (36X,'DEG F',26X,'MG/L',11X,'MG OF C/L')
 2040 FORMAT (36X,'DEG C',26X,'MG/L',11X,'MG OF C/L')
 2050 FORMAT (31X,F10.1,10X,2(10X,F10.3) )
 2060 FORMAT ('   FLUXES',28X,'HEAT           DISSOLVED:   OXYGEN',
     $        6X,'CARBON DIOXIDE')
 2070 FORMAT ('     OUTFLOWS IN WATER',15X,'SOHT',24X,'SODOXM',14X,
     $        'SOCO2M')
 2080 FORMAT (34X,' BTU/AC',10X,2(15X,'LB/AC') )
 2090 FORMAT (34X,'KCAL/HA',10X,2(15X,'KG/HA') )
 2100 FORMAT (31X,1PE10.3,10X,2(10X,1PE10.3) )
C
C     + + + END SPECIFICATIONS + + +
C
C     assign values to parameters used for conversion from internal
C     to external units
C
      IF (UNITFG.EQ.1) THEN
C       english system
C       parameters for variables with energy units
        EFACTA= 407960.
C
C       parameters for variables with temperature units
        TFACTA= 1.8
        TFACTB= 32.0
C
C       parameters for variables for dissolved gases with mass units
        MFACTA= 0.2266
      ELSE
C       metric system
C       parameters for variables with energy units
        EFACTA= 253900.
C
C       parameters for variables with temperature units
        TFACTA= 1.0
        TFACTB= 0.0
C
C       parameters for variables for dissolved gases with mass units
        MFACTA= 0.2540
      END IF
C
      PSTAT = SOTMP*TFACTA+ TFACTB
C
C     state variables with concentration units do not
C     have to be converted
C
C     fluxes - energy units
      PCFLX1= IGCF1(1,LEV)*EFACTA
C
      PCFLX2= IGCF1(2,LEV)*MFACTA
      PCFLX3= IGCF1(3,LEV)*MFACTA
C
      WRITE (PRINTU,2000)
C
      WRITE (PRINTU,2010)
      WRITE (PRINTU,2020)
C
      IF (UNITFG.EQ.1) THEN
        WRITE (PRINTU,2030)
      ELSE
        WRITE (PRINTU,2040)
      END IF
C
      WRITE (PRINTU,2050)  PSTAT, SODOX, SOCO2
C
      WRITE (PRINTU,2060)
      WRITE (PRINTU,2070)
C
      IF (UNITFG.EQ.1) THEN
        WRITE (PRINTU,2080)
      ELSE
        WRITE (PRINTU,2090)
      END IF
C
      WRITE (PRINTU,2100)  PCFLX1, PCFLX2, PCFLX3
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWGRST
     I                   (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables
C     used in material balance check for section iwtgas
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWTGAS2 + + +
      INCLUDE  'cilig.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I3
C
C     + + + EXTERNALS + + +
      EXTERNAL  SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I3= 3
      CALL SETVEC (I3,0.0,
     O             IGCF1(1,LEV))
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWGSIB
C
C     + + + PURPOSE + + +
C     Handle section iwtgas
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWTGAS2 + + +
      INCLUDE 'cilig.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SOHTFP.GE.1)  PAD(SOHTFP+IVL1)= SOHT
      IF (SODOMX.GE.1)  PAD(SODOMX+IVL1)= SODOXM
      IF (SOCDMX.GE.1)  PAD(SOCDMX+IVL1)= SOCO2M
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWGSIP
C
C     + + + PURPOSE + + +
C     Handle section iwtgas
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWTGAS2 + + +
      INCLUDE 'cilig.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SOTFP.GE.1)   PAD(SOTFP +IVL1)= SOTMP
      IF (SODOFP.GE.1)  PAD(SODOFP+IVL1)= SODOX
      IF (SOCDFP.GE.1)  PAD(SOCDFP+IVL1)= SOCO2
C
      RETURN
      END
