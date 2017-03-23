C
C
C
      SUBROUTINE   PPHCAR
C
C     + + + PURPOSE + + +
C     Process input for section phcarb of rchres application module
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PHCARB1 + + +
      INCLUDE    'crhph.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I1,IX,IY,J,SCLU,SGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL   RTABLE,ITABLE,OMSTI,OMSG
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION PHCARB')
 2040 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION PHCARB')
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 350
      I1  = 1
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2000)
      END IF
C
C     error counter initialization
      PHECNT(1)= 0
      PHECNT(2)= 0
C
C     flags - table-type ph-parm1
      IX= 109
      IY= 2
      CALL ITABLE
     I             (IX,I1,IY,UUNITS,
     M              PHPM1)
C
      IF (NCONS.LE.0) THEN
        NCONS= ALKCON
      END IF
C
      IF (ALKCON.GT.NCONS) THEN
C       error - invalid no. for alkcon
        CALL OMSTI (RCHNO)
        CALL OMSTI (ALKCON)
        CALL OMSTI (NCONS)
        SGRP = 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
C
      END IF
C
C     other parameters - table-type ph-parm2
      IX= 110
      IY= 3
      CALL RTABLE
     I             (IX,I1,IY,UUNITS,
     M              PHPM2)
C
C     convert benthal releases from  /hr to  /ivl
      DO 20 J= 1,2
        BRCO2(J)= BRCO2(J)*DELT60
 20   CONTINUE
C
C     initial conditions.  table-type ph-init
      IX= 111
      IY= 3
      CALL RTABLE
     I             (IX,I1,IY,UUNITS,
     M              PHST)
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2040)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHCARB
C
C     + + + PURPOSE + + +
C     Simulate ph, carbon dioxide, total inorganic carbon,
C     and alkalinity
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PHCARB2 + + +
      INCLUDE    'crhph.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    CONVFG,PHDBFG,SGRP,SCLU
      REAL       ALK,COEFF1,COEFF2,COEFF3,COEFF4,
     $           DELTCD,HEST,HEST1,HLLIM,HLLIM1,HPLUS,HULIM,HULIM1,
     $           K1EQU,K2EQU,KCINV,KWEQU,S,SATCO2,TWKELV
      DOUBLE PRECISION DCO2,DTIC
C
C     + + + EXTERNALS + + +
      EXTERNAL   ADVECT,BENTH,PHCALC,OMSG,OMSTR,OMSTI,OMSTD
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 350
C     get inflowing material from pad
      IF (IICFP .GT. 0) THEN
        ITIC= PAD(IICFP + IVL1)
      ELSE
        ITIC= 0.0
      END IF
C
      IF (ICDFP .GT. 0) THEN
        ICO2= PAD(ICDFP + IVL1)
      ELSE
        ICO2= 0.0
      END IF
C
C     advect total inorganic carbon
      DTIC= TIC
      CALL ADVECT
     I            (ITIC,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I             EOVOL,NEXITS,
     M             DTIC,
     O             ROTIC,OTIC)
      TIC= DTIC
C
C     advect carbon dioxide
      DCO2= CO2
      CALL ADVECT
     I            (ICO2,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I             EOVOL,NEXITS,
     M             DCO2,
     O             ROCO2,OCO2)
      CO2= DCO2
C
      IF (VOL .GT. 0.0) THEN
C
        IF (CONSFG .EQ. 0) THEN
          CON(ALKCON)= PAD(CNFP(ALKCON) + IVL1)
        ELSE
C         con(alkcon) is available from section cons
        END IF
C
        ALK   = CON(ALKCON)
        TWKELV= TW + 273.16
C
C       convert tic, co2, and alk to molar concentrations for
C       duration of phcarb section
        TIC= TIC/12000.
        CO2= CO2/12000.
        ALK= ALK/50000.
C
        IF (AVDEPE .GT. 0.17) THEN
C
          IF (BENRFG .EQ. 1) THEN
C           simulate benthal release of co2
C           convert co2 to mg/l for use by benth
            CO2= CO2* 12000.
            CALL BENTH
     I                 (DOX,ANAER,BRCO2,SCRFAC,DEPCOR,
     M                  CO2,
     O                  BENCO2)
            CO2= CO2/ 12000.
          ELSE
C           benthal release of co2 is not considered
            BENCO2= 0.0
          END IF
C
C         calculate molar saturation concentration for co2 (satco2);
C         first, calculate henry's constant, s, for co2; s is defined
C         as the molar concentration of atmospheric co2/partial
C         pressure of co2; cfpres corrects the equation for effects
C         of elevation differences from sea level
          S= 10.**((2385.73/TWKELV) - 14.0184 + .0152642*TWKELV)
          SATCO2= 3.16E-04*CFPRES*S
C
C         calculate increase in co2 due to atmospheric invasion;
C         the co2 invasion is based on oxygen reaeration rate for
C         the control volume
          KCINV= CFCINV*KOREA
          IF (KCINV .GE. 1.0) THEN
            KCINV= .999
          END IF
C
C         calculate net molar co2 change due to co2 invasion,
C         zooplankton excretion and respiration, phytoplankton
C         and benthic algae respiration, bod decay, and benthal
C         release of co2
C
C         if decfg is on, phyco2 and balco2 have already been set
C         to zero
C
          DELTCD= KCINV*(SATCO2 - CO2) + (ZCO2 - PHYCO2 - BALCO2
     $            + DECCO2 + BENCO2)/12000.
C
C         calculate change in total inorganic carbon balance due
C         to net co2 change
          TIC= TIC + DELTCD
          IF (TIC .LT. 0.0) THEN
            TIC= 0.0
          END IF
        ELSE
C         too little water to warrant simulation of quality
C         processes; calculate values of co2 and ph state variables
C         based on only longitudinal advection
        END IF
C
C       calculate ionization product of water
        KWEQU= 10.**(-4470.99/TWKELV + 6.0875 - .01706*TWKELV)
C
C       calculate first dissociation constant of carbonic acid
        K1EQU= 10.**(-3404.71/TWKELV + 14.8435 - .032786*TWKELV)
C
C       calculate second dissociation constant of carbonic acid
        K2EQU= 10.**(-2902.39/TWKELV + 6.4980 - .02379*TWKELV)
C
C       assign values to variables and coefficients used in the
C       solution algorithm
C
C       set ph to 7.0 if it is undefined (due to no water in reach)
        IF (PH .LT. 0.0) THEN
          PH=7.0
        END IF
C
        HEST  = 10.**(-PH)
        HLLIM = 0.0
        HULIM = 1.0
        COEFF1= ALK + K1EQU
        COEFF2= -KWEQU + ALK*K1EQU + K1EQU*K2EQU - TIC*K1EQU
        COEFF3= -2.*K1EQU*K2EQU*TIC - K1EQU*KWEQU + ALK*K1EQU*K2EQU
        COEFF4= -K1EQU*K2EQU*KWEQU
C
C       store initial values for hest, hllim, and hulim; these
C       values will be used again if the solution technique for ph
C       does not produce a satisfactory solution within the allowed
C       number of iterations (phcnt)
        HEST1 = HEST
        HLLIM1= HLLIM
        HULIM1= HULIM
C
C       set printing option for internal variables in the
C       ph solution 0
        PHDBFG= 0
C
C       calculate ph
        CALL PHCALC
     I              (PHDBFG,PHCNT,COEFF1,COEFF2,COEFF3,COEFF4,MESSU,
     M                HEST,HLLIM,HULIM,
     O                PH,CONVFG,HPLUS)
C
        IF (CONVFG .EQ. 0) THEN
C         a satisfactory solution for ph has not been reached;
C         write error message; repeat solution technique to print
C         out values for internal variables
          CALL OMSTD (DATIM)
          CALL OMSTI (RCHNO)
          CALL OMSTR (COEFF1)
          CALL OMSTR (COEFF2)
          CALL OMSTR (COEFF3)
          CALL OMSTR (COEFF4)
          CALL OMSTR (ALK)
          CALL OMSTR (TIC)
          SGRP= 2
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               PHECNT(2))
C
C         set printing option for internal variables in the ph
C         solution 1
          PHDBFG= 1
C
          CALL PHCALC
     I                (PHDBFG,PHCNT,COEFF1,COEFF2,COEFF3,COEFF4,MESSU,
     M                 HEST1,HLLIM1,HULIM1,
     O                 PH,CONVFG,HPLUS)
C
        END IF
C
C       calculate co2 concentration (molar)
        CO2= TIC/(1. + K1EQU/HPLUS + K1EQU*K2EQU/(HPLUS**2))
C
C       convert tic, co2, and alk from moles/liter to mg/liter
        TIC= TIC*12000.
        CO2= CO2*12000.
        ALK= ALK*50000.
      ELSE
C       reach/res has gone dry during the interval; set ph
C       equal to an undefined value
        PH= -1.0E30
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHCALC
     I                    (PHDBFG,PHCNT,COEFF1,COEFF2,COEFF3,COEFF4,
     I                     MESSU,
     M                     HEST,HLLIM,HULIM,
     O                     PH,CONVFG,HPLUS)
C
C     + + + PURPOSE + + +
C     Calculate ph
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    CONVFG,MESSU,PHCNT,PHDBFG
      REAL       COEFF1,COEFF2,COEFF3,COEFF4,HEST,HLLIM,HPLUS,
     $           HULIM,PH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PHDBFG - ???
C     PHCNT  - ???
C     COEFF1 - ???
C     COEFF2 - ???
C     COEFF3 - ???
C     COEFF4 - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     HEST   - ???
C     HLLIM  - ???
C     HULIM  - ???
C     PH     - ???
C     CONVFG - ???
C     HPLUS  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    COUNT
      REAL       DFDH,QUADH
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS,ALOG10
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (' * ',6X,
     $        'HEST     QUADH      DFDH     HLLIM     HULIM     HPLUS',
     $        20X,' *')
 2010 FORMAT (' * ',1P6E10.3,20X,' *')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (PHDBFG .EQ. 1) THEN
        WRITE (MESSU,2000)
      END IF
C
      CONVFG= 0
      COUNT = 1
C
C     dountil count> phcnt or convfg= 1
 10   CONTINUE
C
C       evaluate quadratic and slope for solution equation
        QUADH= (((HEST + COEFF1)*HEST + COEFF2)*HEST + COEFF3)*HEST
     $         + COEFF4
        DFDH = ((4.*HEST + 3.*COEFF1)*HEST + 2.*COEFF2)*HEST + COEFF3
        COUNT= COUNT + 1
C
        IF (DFDH .LE. 0.0) THEN
C         slope of solution equation is zero or negative; solution
C         for hplus is not meaningful for such a slope; update
C         values for hllim, hulim, and hest to force convergence
C
          IF (QUADH .LT. 0.0) THEN
            IF (HEST .GE. HLLIM) THEN
              HLLIM= HEST
              HEST = 10.0*HEST
            END IF
          ELSE
            IF (HEST .LE. HULIM) THEN
              HULIM= HEST
              HEST = 0.10*HEST
            END IF
          END IF
C
          IF (PHDBFG .EQ. 1) THEN
            WRITE (MESSU,2010)  HEST, QUADH, DFDH, HLLIM, HULIM
          END IF
        ELSE
C         calculate new hydrogen ion concentration
          HPLUS= HEST - QUADH/DFDH
C
          IF ((ABS(HPLUS-HEST)/HPLUS) .GT. 0.10) THEN
C           difference between posterior estimate and prior
C           estimate is greater than 10 percent; adjust prior
C           estimate for next iteration
C
            IF (HPLUS .LE. HLLIM) THEN
C             assign value midway between prior estimate and
C             lower limit to prior estimate for next iteration
              HEST= (HEST + HLLIM)/2.0
            ELSE
              IF (HPLUS .GE. HULIM) THEN
C               assign value midway between prior estimate and
C               upper limit to prior estimate for next iteration
                HEST= (HEST + HULIM)/2.0
              ELSE
C               assign value of posterior estimate to prior
C               estimate for next iteration
                HEST= HPLUS
              END IF
            END IF
          ELSE
C           solution for hplus is complete; drop out of dountil
C           loop by turning on convergence indicator flag
            CONVFG= 1
          END IF
C
          IF (PHDBFG .EQ. 1) THEN
            WRITE (MESSU,2010)  HEST,QUADH,DFDH,HLLIM,HULIM,HPLUS
          END IF
        END IF
      IF (COUNT .LE. PHCNT .AND. CONVFG .NE. 1) GO TO 10
C
C     calculate ph
      PH= -ALOG10(HPLUS)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHACC
     I                   (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes in subroutine group phcarb for printout
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PHCARB2 + + +
      INCLUDE  'crhph.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I2
C
C     + + + EXTERNALS + + +
      EXTERNAL  ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
C     handle flux groups dealing with reach-wide variables
      CALL ACCVEC
     I            (I2,PHIF(1,FRMROW),
     M             PHIF(1,TOROW))
      CALL ACCVEC
     I            (I2,PHCF1(1,FRMROW),
     M             PHCF1(1,TOROW))
C
      IF (NEXITS .GT. 1) THEN
C       handle flux groups dealing with individual exit gates
        DO 10 I= 1,2
          CALL ACCVEC
     I                (NEXITS,PHCF2(1,I,FRMROW),
     M                 PHCF2(1,I,TOROW))
 10     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHPRT
     I                   (LEV,PRINTU,FACTA,FACTB,FLUXID)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units and print
C     out results
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEV,PRINTU
      REAL        FACTA,FACTB
      CHARACTER*4 FLUXID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C     FACTA  - ???
C     FACTB  - ???
C     FLUXID - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PHCARB2 + + +
      INCLUDE    'crhph.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I2,J
      REAL       PIFLX(2),PCFLX1(2),PCFLX2(5,2)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** PHCARB ***')
 2010 FORMAT (/,'   STATE VARIABLES',20X,'TIC       CO2        PH')
 2020 FORMAT (  ' ',30X,2(6X,'MG/L'))
 2030 FORMAT (  ' ',30X,3(1PE10.3))
 2040 FORMAT (/,'   FLUXES',27X,'TOTAL     TOTAL',
     $        '    INDIVIDUAL GATE OUTFLOWS')
 2050 FORMAT (  ' ',34X,'INFLOW   OUTFLOW',5I10)
 2060 FORMAT (/,'     INORGANIC CARBON (',A3,')',10X,'ITIC     ROTIC',
     $        16X,'OTIC')
 2070 FORMAT (  ' ',30X,7(1PE10.3))
 2080 FORMAT (/,'     CARBON DIOXIDE (',A3,')',12X,'ICO2     ROCO2',
     $          16X,'OCO2')
 2090 FORMAT (/,'   FLUXES',27X,'TOTAL     TOTAL')
 2100 FORMAT (/,'     INORGANIC CARBON (',A3,')',10X,'ITIC     ROTIC')
 2110 FORMAT (/,'     CARBON DIOXIDE (',A3,')',12X,'ICO2     ROCO2')
C
C     + + + END SPECIFICATIONS + + +
C
C     Note: local arrays have same dimensions as corresponding arrays
C      in osv, except for dropping of dimension lev, where applicable
C
      I2= 2
C     convert variables to external units
C
C     Rchres-wide variables
C
C     inflow fluxes
      CALL TRNVEC
     I            (I2,PHIF(1,LEV),FACTA,FACTB,
     O             PIFLX)
C
C     computed fluxes
      CALL TRNVEC
     I            (I2,PHCF1(1,LEV),FACTA,FACTB,
     O             PCFLX1)
C
      IF (NEXITS .GT. 1) THEN
C       exit-specific variables
        DO 10 I=1,2
          CALL TRNVEC
     I                (NEXITS,PHCF2(1,I,LEV),FACTA,FACTB,
     O                 PCFLX2(1,I))
 10     CONTINUE
      END IF
C
C     do printout on unit printu
      WRITE (PRINTU,2000)
C
      WRITE (PRINTU,2010)
      WRITE (PRINTU,2020)
      WRITE (PRINTU,2030)  PHST
C
      IF (NEXITS .GT. 1) THEN
        WRITE (PRINTU,2040)
        WRITE (PRINTU,2050)  (J,J=1,NEXITS)
C
        WRITE (PRINTU,2060)  FLUXID
        WRITE (PRINTU,2070)  PIFLX(1), PCFLX1(1),
     $    (PCFLX2(J,1),J=1,NEXITS)
C
        WRITE (PRINTU,2080)  FLUXID
        WRITE (PRINTU,2070)  PIFLX(2), PCFLX1(2),
     $    (PCFLX2(J,2),J=1,NEXITS)
      ELSE
        WRITE (PRINTU,2090)
        WRITE (PRINTU,2050)
C
        WRITE (PRINTU,2100)  FLUXID
        WRITE (PRINTU,2070)  PIFLX(1), PCFLX1(1)
C
        WRITE (PRINTU,2110)  FLUXID
        WRITE (PRINTU,2070)  PIFLX(2), PCFLX1(2)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHRB
C
C     + + + PURPOSE + + +
C     Handle subroutine group phcarb
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PHCARB2 + + +
      INCLUDE    'crhph.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,2
        IF (PHCF1X(I) .GE. 1) THEN
          PAD(PHCF1X(I) + IVL1)= PHCF1(I,1)
        END IF
 10   CONTINUE
C
      IF (NEXITS .GT. 1) THEN
        DO 30 J= 1,2
          DO 20 I= 1,NEXITS
            IF (PHCF2X(I,J) .GE. 1) THEN
              PAD(PHCF2X(I,J) + IVL1)= PHCF2(I,J,1)
            END IF
 20       CONTINUE
 30     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHRP
C
C     + + + PURPOSE + + +
C     Handle subroutine group phcarb
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PHCARB2 + + +
      INCLUDE    'crhph.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I=1,3
        IF (PHSTX(I) .GE. 1) THEN
          PAD(PHSTX(I) + IVL1)= PHST(I)
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHRST
     I                   (LEV)
C
C     + + + PURPOSE + + +
C     Reset flux and state variables for subroutine group phcarb
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- scrtch, version phcarb2 + + +
      INCLUDE    'crhph.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I2
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
C     handle flux groups dealing with reach-wide variables
      CALL SETVEC
     I            (I2,0.0,
     O             PHIF(1,LEV))
      CALL SETVEC
     I            (I2,0.0,
     O             PHCF1(1,LEV))
C
      IF (NEXITS .GT. 1) THEN
C       handle flux groups dealing with individual exit gates
        DO 10 I= 1,2
          CALL SETVEC
     I                (NEXITS,0.0,
     O                 PHCF2(1,I,LEV))
 10     CONTINUE
      END IF
C
      RETURN
      END
