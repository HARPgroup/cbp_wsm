C
C
C
      SUBROUTINE   PPWTGS
C
C     + + + PURPOSE + + +
C     Process input for section pwtgas
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWTGAS1 + + +
      INCLUDE    'cplps.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I1,I2,I4
      REAL      ELEV,RVAL(5)
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITABLE,RTABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION PWTGAS')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION PWTGAS')
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
C
      IF (OUTLEV.GT.1) THEN
C       processing section message
        WRITE (MESSU,2000)
      END IF
C
C     process values in table-type pwt-parm1
      I2= 44
      I4= 4
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             PGPM1)
C
C     process values in table-type pwt-parm2
      I2= 45
      I4= 5
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             RVAL)
      ELEV= RVAL(1)
C
C     compute factor for correcting dissolved gas
C     saturated concentrations for altitude
      ELEVGC= ((288.0- 0.00198*ELEV)/288.0)**5.256
C
C     dissolved gas concentrations in interflow
C     and groundwater
      IDOXP= RVAL(2)
      ICO2P= RVAL(3)
      ADOXP= RVAL(4)
      ACO2P= RVAL(5)
C
      IF (PWATFG.EQ.0 .AND. SEDFG.EQ.0) THEN
C       need to set csnofg
C        I2= 26
C        I4= 1
C        CALL ITABLE(I2,I1,I4,UUNITS,
C     M              CSNOFG)
        CSNOFG= 0
      END IF
C
      IF (IDVFG.EQ.1) THEN
C       check for monthly values of interflow do concentration -
C       table-type mon-ifwdox
        I2= 46
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               IDOXPM)
      END IF
C
      IF (ICVFG.EQ.1) THEN
C       check for monthly values of interflow co2 concentration -
C       table-type mon-ifwco2
        I2= 47
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               ICO2PM)
      END IF
C
      IF (GDVFG.EQ.1) THEN
C       check for monthly values of groundwater do concentration -
C       table-type mon-grnddox
        I2= 48
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               ADOXPM)
      END IF
C
      IF (GCVFG.EQ.1) THEN
C       check for monthly values of groundwater co2 concentration -
C       table-type mon-grndco2
        I2= 49
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               ACO2PM)
      END IF
C
C     initial temperatures - table-type pwt-temps
      I2= 50
      I4= 3
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             PGST1)
C
C     initial do and co2 concentrations - table-type pwt-gases
      I2= 51
      I4= 6
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             PGST2)
C
      IF (OUTLEV.GT.1) THEN
C       end processing message
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
C
C     4.2(1).6
C
      SUBROUTINE PWTGAS
C
C     + + + PURPOSE + + +
C     Estimate water temperature, dissolved oxygen, and carbon
C     dioxide in the outflows from a pervious land
C     segment. calculate associated fluxes through exit gates
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWTGAS2 + + +
      INCLUDE 'cplps.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL    ABSTMP,DUMMY
C
C     + + + FUNCTIONS + + +
      REAL    DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + END SPECIFICATIONS + + +
C
C     get hydrological time series
      IF (PWATFG.NE.0) GO TO 10
C       get time series from inpad
        SURO= PAD(SOFP+IVL1)
        IFWO= PAD(IOFP+IVL1)
        AGWO= PAD(AOFP+IVL1)
        GO TO 20
 10   CONTINUE
C       suro,ifwo, and agwo are available from pwater
 20   CONTINUE
C
C     outflow temperatures are based on soil temperatures - units
C     are deg. c
      IF (PSTFG.NE.0) GO TO 30
C       get soil temperatures from the inpad
        SLTMP= PAD(SLTFP+IVL1)
        ULTMP= PAD(ULTFP+IVL1)
        LGTMP= PAD(LGTFP+IVL1)
        GO TO 40
 30   CONTINUE
C       soil temperatures have been calculated in pstemp
 40   CONTINUE
C
      SOTMP= SLTMP
      IOTMP= ULTMP
      AOTMP= LGTMP
C
C     don't allow water temp to go below 0.5 deg c
      IF (SOTMP.LT.0.5) SOTMP= 0.5
      IF (IOTMP.LT.0.5) IOTMP= 0.5
      IF (AOTMP.LT.0.5) AOTMP= 0.5
C
      IF (CSNOFG.NE.1) GO TO 80
C       effects of snow are considered
C       adjust surface outflow temperature if snowmelt is occurring
        IF (SNOWFG.NE.0) GO TO 50
          WYIELD= PAD(WYFP+IVL1)
          GO TO 60
 50     CONTINUE
C         wyield is available from snow
 60     CONTINUE
C
        IF (WYIELD.LE.0.0) GO TO 70
C         snowmelt is occuring
          SOTMP= 0.5
 70     CONTINUE
C
 80   CONTINUE
C
C     for zero outflow, report outflow temperatures as "undefined"
C     if (suro.eq.0.0) sotmp= -1.0e30
      IF ((ABS(SURO)).LE.0.0) SOTMP= -1.0E30
C     if (ifwo.eq.0.0) iotmp= -1.0e30
      IF ((ABS(IFWO)).LE.0.0) IOTMP= -1.0E30
C     if (agwo.eq.0.0) aotmp= -1.0e30
      IF ((ABS(AGWO)).LE.0.0) AOTMP= -1.0E30
C
C     compute the outflow of heat energy in water - units are
C     deg. c-in./ivl
      SOHT= SOTMP*SURO
      IOHT= IOTMP*IFWO
      AOHT= AOTMP*AGWO
      POHT= SOHT+IOHT+AOHT
C
C     calculate dissolved oxygen and carbon dioxide concentrations
C     in surface runoff - units are mg/l for do and mg c/l for co2
      IF (SURO.LE.0.0) GO TO 90
C       there is surface outflow
C
C       oxygen calculation
        DUMMY= SOTMP*(0.007991-0.77774E-4*SOTMP)
        SODOX= (14.652+SOTMP*(-0.41022+DUMMY))*ELEVGC
C
C       carbon dioxide calculation
        ABSTMP= SOTMP+ 273.16
        DUMMY = 2385.73/ABSTMP- 14.0184+ 0.0152642*ABSTMP
        SOCO2 = 10.0**DUMMY*3.16E-04*ELEVGC*12000.0
        GO TO 100
 90   CONTINUE
C       there is no surface outflow - values are undefined
        SODOX= -1.0E30
        SOCO2= -1.0E30
 100  CONTINUE
C
      IF (DAYFG.NE.1) GO TO 190
C       it is the first interval of the day
        IF (IDVFG.NE.1) GO TO 110
C         interflow dox parameter is allowed to vary
C         throughout the year
C         interpolate for the daily value
C         linearly interpolate idoxp between two values from the
C         monthly array idoxpm(12)
          IDOXP= DAYVAL(IDOXPM(MON),IDOXPM(NXTMON),DAY,NDAYS)
          GO TO 120
 110    CONTINUE
C         interflow dox parameter does not vary throughout the
C         year. idoxp value has been supplied by the run interpreter
 120    CONTINUE
C
        IF (ICVFG.NE.1) GO TO 130
C         interflow co2 parameter is allowed to vary
C         throughout the year
C         interpolate for the daily value
C         linearly interpolate ico2p between two values from the
C         monthly array ico2pm(12)
          ICO2P= DAYVAL(ICO2PM(MON),ICO2PM(NXTMON),DAY,NDAYS)
          GO TO 140
 130    CONTINUE
C         interflow co2 parameter does not vary throughout the year.
C         ico2p value has been supplied by the run interpreter
 140    CONTINUE
C
        IF (GDVFG.NE.1) GO TO 150
C         groundwater flow dox parameter is allowed to vary
C         throughout the year
C         interpolate for the daily value
C         linearly interpolate adoxp between two values from the
C         monthly array adoxpm(12)
          ADOXP= DAYVAL(ADOXPM(MON),ADOXPM(NXTMON),DAY,NDAYS)
          GO TO 160
 150    CONTINUE
C         groundwater flow dox parameter does not vary throughout the
C         year. adoxp value has been supplied by the run interpreter
 160    CONTINUE
C
        IF (GCVFG.NE.1) GO TO 170
C         groundwater flow co2 parameter is allowed to vary
C         throughout the year
C         interpolate for the daily value
C         linearly interpolate aco2p between two values from the
C         monthly array aco2pm(12)
          ACO2P= DAYVAL(ACO2PM(MON),ACO2PM(NXTMON),DAY,NDAYS)
          GO TO 180
 170    CONTINUE
C         groundwater flow co2 parameter does not vary throughout the
C         year.  aco2p value has been supplied by the run interpreter
 180    CONTINUE
C
 190  CONTINUE
C
      IF (IFWO.LE.0.0) GO TO 200
C       there is interflow
        IODOX= IDOXP
        IOCO2= ICO2P
        GO TO 210
 200  CONTINUE
C       there is no interflow - values are undefined
        IODOX= -1.0E30
        IOCO2= -1.0E30
 210  CONTINUE
C
      IF (AGWO.LE.0.0) GO TO 220
C       there is groundwater flow
        AODOX= ADOXP
        AOCO2= ACO2P
        GO TO 230
 220  CONTINUE
C       there is no groundwater flow - values are undefined
        AODOX= -1.0E30
        AOCO2= -1.0E30
 230  CONTINUE
C
C     calculate outflow mass of dox - units are mg-in./l-ivl
      SODOXM= SODOX*SURO
      IODOXM= IODOX*IFWO
      AODOXM= AODOX*AGWO
      PODOXM= SODOXM+ IODOXM+ AODOXM
C
C     calculate outflow mass of co2 - units are mg-in./l-ivl
      SOCO2M= SOCO2*SURO
      IOCO2M= IOCO2*IFWO
      AOCO2M= AOCO2*AGWO
      POCO2M= SOCO2M+ IOCO2M+ AOCO2M
C
      RETURN
      END
C
C     4.2(1).15.1.4
C
      SUBROUTINE PWGACC
     I                  (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section pwtgas
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWTGAS2 + + +
      INCLUDE    'cplps.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I3,I6
C
C     + + + EXTERNALS + + +
      EXTERNAL   ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I3=3
      I6=6
      CALL ACCVEC (I3,PGCF1(1,FRMROW),
     M             PGCF1(1,TOROW))
C
      CALL ACCVEC (I6,PGCF2(1,FRMROW),
     M             PGCF2(1,TOROW))
C
      RETURN
      END
C
C     4.2(1).15.2.6
C
      SUBROUTINE PWGPRT
     I                  (UNITFG,LEV,PRINTU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units.
C     Note: local arrays have
C     identical sizes and structures to the corresponding arrays in
C     the osv apart from dropping the dimension lev for fluxes
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    UNITFG,LEV,PRINTU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWTGAS2 + + +
      INCLUDE    'cplps.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I3,I6
      REAL       EFACTA,EFACTB,MFACTA,MFACTB,PCFLX1(3),PCFLX2(6),
     $           PSTAT1(3),PPOCO2,PPODOX,PPOHT,TFACTA,TFACTB
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,1H ,'*** PWTGAS ***')
 2010 FORMAT (/,1H ,'  STATE VARIABLES')
 2020 FORMAT (/,1H ,30X,'<---SURFACE FLOW---><----INTERFLOW----->',
     $        '<-GROUNDWATER FLOW->')
 2030 FORMAT (' ','    WATER TEMPERATURES',23X,'SOTMP',
     $        15X,'IOTMP',15X,'AOTMP')
 2040 FORMAT (' ',6X,'(DEG F)',17X,3(10X,F10.1))
 2050 FORMAT (' ',6X,'(DEG C)',17X,3(10X,F10.1))
 2060 FORMAT (/,1H ,'    DISSOLVED GASES',16X,
     $        'SODOX     SOCO2     IODOX     IOCO2     AODOX     AOCO2')
 2070 FORMAT (' ',6X,'(MG/L)',18X,6F10.3)
 2080 FORMAT (/,1H ,'  FLUXES')
 2090 FORMAT (/,1H ,'    OUTFLOWS IN WATER',82X,'(TOTAL)')
 2100 FORMAT (' ',6X,'HEAT ENERGY (BTU/AC)',20X,'SOHT',16X,'IOHT',
     $        16X,'AOHT',16X,'POHT')
 2110 FORMAT (' ',6X,'HEAT ENERGY (KCAL/HA)',19X,'SOHT',16X,'IOHT',
     $        16X,'AOHT',16X,'POHT')
 2120 FORMAT (' ',8X,'(RELATIVE TO FREEZING)',4(10X,E10.4))
 2130 FORMAT (' ',6X,'DISSOLVED GASES (LB/AC)',5X,
     $  'SODOXM    SOCO2M    IODOXM    IOCO2M    AODOXM    AOCO2M',
     $        '    PODOXM    POCO2M')
 2140 FORMAT (' ',6X,'DISSOLVED GASES (KG/HA)',5X,
     $  'SODOXM    SOCO2M    IODOXM    IOCO2M    AODOXM    AOCO2M',
     $        '    PODOXM    POCO2M')
 2150 FORMAT (' ',30X,1P,8G10.3)
C
C     + + + END SPECIFICATIONS + + +
C
      I3=3
      I6=6
C     assign values to parameters used for conversion from internal
C     to external units
C
      IF (UNITFG.NE.1) GO TO 10
C       english system
C       parameters for variables with energy units
        EFACTA= 407960.
        EFACTB= 0.0
C
C       parameters for variables with temperature units
        TFACTA= 1.8
        TFACTB= 32.0
C
C       parameters for variables for dissolved gases with mass units
        MFACTA= 0.2266
        MFACTB= 0.0
        GO TO 20
C
 10   CONTINUE
C       metric system
C       parameters for variables with energy units
        EFACTA= 253900.
        EFACTB= 0.0
C
C       parameters for variables with temperature units
        TFACTA= 1.0
        TFACTB= 0.0
C
C       parameters for variables for dissolved gases with mass units
        MFACTA= 0.2540
        MFACTB= 0.0
C
 20   CONTINUE
C
C     state variables with temperature units
      CALL TRNVEC (I3,PGST1,TFACTA,TFACTB,
     O             PSTAT1)
C
C     state variables with concentration units do not have to be
C     converted
C     fluxes - energy units
      CALL TRNVEC (I3,PGCF1(1,LEV),EFACTA,EFACTB,
     O             PCFLX1)
C
C     fluxes of dissolved gases in mass units
      CALL TRNVEC (I6,PGCF2(1,LEV),MFACTA,MFACTB,
     O             PCFLX2)
C
C     do printout on unit printu
C
      WRITE (PRINTU,2000)
C
      WRITE (PRINTU,2010)
      WRITE (PRINTU,2020)
      WRITE (PRINTU,2030)
C
      IF (UNITFG.NE.1) GO TO 30
        WRITE (PRINTU,2040)  PSTAT1
        GO TO 40
 30   CONTINUE
        WRITE (PRINTU,2050)  PSTAT1
 40   CONTINUE
C
      WRITE (PRINTU,2060)
      WRITE (PRINTU,2070)  PGST2
C
      WRITE (PRINTU,2080)
      WRITE (PRINTU,2090)
      PPOHT = PCFLX1(1)+ PCFLX1(2)+ PCFLX1(3)
      PPODOX= PCFLX2(1)+ PCFLX2(3)+ PCFLX2(5)
      PPOCO2= PCFLX2(2)+ PCFLX2(4)+ PCFLX2(6)
C
      IF (UNITFG.NE.1) GO TO 50
        WRITE (PRINTU,2100)
        GO TO 60
 50   CONTINUE
        WRITE (PRINTU,2110)
 60   CONTINUE
C
      WRITE (PRINTU,2120)  PCFLX1, PPOHT
C
      IF (UNITFG.NE.1) GO TO 70
        WRITE (PRINTU,2130)
        GO TO 80
 70   CONTINUE
        WRITE (PRINTU,2140)
 80   CONTINUE
C
      WRITE (PRINTU,2150)  PCFLX2, PPODOX, PPOCO2
C
      RETURN
      END
C
C     4.2(1).15.3.4
C
      SUBROUTINE PWGRST
     I                  (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables
C     used in material balance check for section pwtgas
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWTGAS2 + + +
      INCLUDE    'cplps.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I3,I6
C
C     + + + EXTERNALS + + +
      EXTERNAL  SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I3=3
      I6=6
      CALL SETVEC (I3,0.0,
     O             PGCF1(1,LEV))
C
      CALL SETVEC (I6,0.0,
     O             PGCF2(1,LEV))
C
      RETURN
      END
C
C     4.2(1).14.4
C
      SUBROUTINE PWGSPB
C
C     + + + PURPOSE + + +
C     ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWTGAS2 + + +
      INCLUDE 'cplps.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section pwtgas
      IF (SOHTFP.GE.1)  PAD(SOHTFP+IVL1)= SOHT
      IF (IOHTFP.GE.1)  PAD(IOHTFP+IVL1)= IOHT
      IF (AOHTFP.GE.1)  PAD(AOHTFP+IVL1)= AOHT
      IF (POHTFP.GE.1)  PAD(POHTFP+IVL1)= POHT
C
      IF (SODOMX.GE.1)  PAD(SODOMX+IVL1)= SODOXM
      IF (SOCDMX.GE.1)  PAD(SOCDMX+IVL1)= SOCO2M
      IF (IODOMX.GE.1)  PAD(IODOMX+IVL1)= IODOXM
      IF (IOCDMX.GE.1)  PAD(IOCDMX+IVL1)= IOCO2M
      IF (AODOMX.GE.1)  PAD(AODOMX+IVL1)= AODOXM
      IF (AOCDMX.GE.1)  PAD(AOCDMX+IVL1)= AOCO2M
      IF (PODOMX.GE.1)  PAD(PODOMX+IVL1)= PODOXM
      IF (POCDMX.GE.1)  PAD(POCDMX+IVL1)= POCO2M
C
      RETURN
      END
C
C     4.2(1).13.6
C
      SUBROUTINE PWGSPT
C
C     + + + PURPOSE + + +
C      ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWTGAS2 + + +
      INCLUDE 'cplps.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section pwtgas
      IF (SOTFP.GE.1)  PAD(SOTFP +IVL1) = SOTMP
      IF (IOTFP.GE.1)  PAD(IOTFP +IVL1) = IOTMP
      IF (AOTFP.GE.1)  PAD(AOTFP +IVL1) = AOTMP
C
      IF (SODOFP.GE.1)  PAD(SODOFP+IVL1)= SODOX
      IF (SOCDFP.GE.1)  PAD(SOCDFP+IVL1)= SOCO2
      IF (IODOFP.GE.1)  PAD(IODOFP+IVL1)= IODOX
      IF (IOCDFP.GE.1)  PAD(IOCDFP+IVL1)= IOCO2
      IF (AODOFP.GE.1)  PAD(AODOFP+IVL1)= AODOX
      IF (AOCDFP.GE.1)  PAD(AOCDFP+IVL1)= AOCO2
C
      RETURN
      END
