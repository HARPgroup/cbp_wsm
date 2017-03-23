C
C
C
      SUBROUTINE   POXRX
C
C     + + + PURPOSE + + +
C     Process input for the oxrx section of rchres application module
C
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION OXRX1 + + +
      INCLUDE    'crhox.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I1,I2,I4,IVAL(1)
      REAL       RVAL(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL   RTABLE,ITABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION OXRX')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION OXRX')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2000)
      END IF
C
      I1= 1
C
      OXWCNT= 0
C
      IF (GQFG.EQ.1.AND.GQALFG(4).EQ.1) THEN
C       reaeration data have already been processed
      ELSE
C       flags - table-type ox-flags
        I2= 71
        I4=  1
        CALL ITABLE
     I               (I2,I1,I4,UUNITS,
     M                IVAL)
C
        OXPM1= IVAL(1)
C
        IF (HTFG.EQ.0) THEN
C         get elevation and compute pressure correction factor -
C         table-type elev
          I2= 73
          I4=  1
          CALL RTABLE
     I                 (I2,I1,I4,UUNITS,
     M                  RVAL(1))
          ELEV  = RVAL(1)
          CFPRES= ((288.0-0.001981*ELEV)/288.0)**5.256
C
        END IF
C
        IF (LKFG.EQ.1) THEN
C         rchres is a lake - get reaeration parameter from table-type
C         ox-cforea
          I2= 75
          I4=  1
          CALL RTABLE
     I                 (I2,I1,I4,UUNITS,
     M                  RVAL(1))
          CFOREA= RVAL(1)
C
        ELSE
C         rchres is a free-flowing stream
C         casentry reamfg
          GO TO (40,70,80) , REAMFG
C         case 1
 40         CONTINUE
C           tsivoglou method - table-type ox-tsivoglou
            I2= 76
            I4=  2
            CALL RTABLE
     I                   (I2,I1,I4,UUNITS,
     M                    RVAL(1))
C
            REAKT = RVAL(1)
            TCGINV= RVAL(2)
            IF (HYDRFG.EQ.0) THEN
C             read in len, delth - table-type ox-len-delth
              I2= 77
              I4=  2
              CALL RTABLE
     I                     (I2,I1,I4,UUNITS,
     M                      RVAL(1))
C
              LEN  = RVAL(1)
              DELTH= RVAL(2)
C
            ELSE
C             len, delth are available from hydr
C
            END IF
            GO TO 90
C
C         case 2
 70         CONTINUE
C           owen/churchill/o'connor-dobbins - table-type ox-tcginv
            I2= 78
            I4=  1
            CALL RTABLE
     I                   (I2,I1,I4,UUNITS,
     M                    RVAL(1))
C
            TCGINV= RVAL(1)
            GO TO 90
C
C         case 3
 80         CONTINUE
C           user formula - table-type ox-reaparm
            I2= 79
            I4=  4
            CALL RTABLE
     I                   (I2,I1,I4,UUNITS,
     M                    OXPM4(1))
C
 90       CONTINUE
C         endcase
C
        END IF
      END IF
C
      IF (BENRFG.EQ.1) THEN
C       get benthic release parms - table-type ox-benparm
        I2= 74
        I4=  6
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                OXPM3(1))
C
C       convert units from 1/hr to 1/ivl
        OXPM3(1)= OXPM3(1)*DELT60
        OXPM3(4)= OXPM3(4)*DELT60
        OXPM3(5)= OXPM3(5)*DELT60
C
      END IF
C
C     get general parameters - these always required
C     table-type ox-genparm
      I2= 72
      I4=  4
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              OXPM2(1))
C
C     convert units from 1/hr to 1/ivl
      KBOD20= KBOD20*DELT60
      KODSET= KODSET*DELT60
C
C     initial concentrations - table-type ox-init
      I2= 80
      I4=  3
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              RVAL(1))
C
      DOX  = RVAL(1)
      RDOX = DOX* VOL
      BOD  = RVAL(2)
      RBOD = BOD* VOL
      SATDO= RVAL(3)
C
C     initialize dissolved oxygen & bod fluxes
      DO 140 I= 1,5
        OXCF3(I,1)= 0.0
        OXCF4(I,1)= 0.0
 140  CONTINUE
      OXCF3(6,1)= 0.0
      OXCF3(7,1)= 0.0
      OXCF4(6,1)= 0.0
      OXCF4(7,1)= 0.0
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OXRX
C
C     + + + PURPOSE + + +
C     Simulate primary do, bod balances
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION OXRX2 + + +
      INCLUDE    'crhox.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL       DOREA,DOBEN,BODBNR,VOLSP
      DOUBLE PRECISION DPBOD,DPDOX
C
C     + + + EXTERNALS + + +
      EXTERNAL   ADVECT,SINK,OXBEN,OXREA,BODDEC
C
C     + + + END SPECIFICATIONS + + +
C
C     single precision version of vol
C
      VOLSP= VOL
C
C     get inflowing material from pad
C
      IF (IDOFP .GT. 0) THEN
        IDOX= PAD(IDOFP + IVL1)
      ELSE
        IDOX= 0.0
      END IF
C
      IF (IODFP .GT. 0) THEN
        IBOD= PAD(IODFP + IVL1)
      ELSE
        IBOD= 0.0
      END IF
C
C     advect dissolved oxygen
C
      DPDOX=DOX
      CALL ADVECT
     I            (IDOX,VOLS,SROVOL,VOL,EROVOL,SOVOL(1),
     I             EOVOL(1),NEXITS,
     M             DPDOX,
     O             RODOX,ODOX(1))
      DOX=DPDOX
C
C     advect bod
C
      DPBOD=BOD
      CALL ADVECT
     I            (IBOD,VOLS,SROVOL,VOL,EROVOL,SOVOL(1),
     I             EOVOL(1),NEXITS,
     M             DPBOD,
     O             ROBOD,OBOD(1))
      BOD=DPBOD
C
      IF (AVDEPE .GT. 0.17) THEN
C
C       sink bod
C
        CALL SINK
     I            (VOL,AVDEPE,KODSET,
     M             DPBOD,
     O             SNKBOD)
        BOD   =DPBOD
        SNKBOD= -SNKBOD
C
        IF (BENRFG .EQ. 1) THEN
C
C         simulate benthal oxygen demand and benthal release of bod,
C         and compute associated fluxes
C
          CALL OXBEN
     I               (BRBOD(1),BENOD,TW,DEPCOR,SCRFAC,TCBEN,
     I                EXPOD,EXPREL,
     M                DOX,BOD,
     O                DOBEN,BODBNR)
C
          BENDOX= -DOBEN*VOLSP
          BNRBOD= BODBNR*VOLSP
C
        ELSE
C
C         benthal influences are not considered
C
        END IF
C
        IF (LKFG .EQ. 1) THEN
C         get wind movement from inpad for calculation of
C         reaeration
          WIND= PAD(WDFP + IVL1)
        ELSE
          WIND= 0.0
        END IF
C
C       calculate oxygen reaeration
C
        IF (GQFG .EQ. 1 .AND. GQALFG(4) .EQ. 1) THEN
C         korea has already been calculated in section gqual
        ELSE
C
          CALL OXREA
     I               (LKFG,WIND,CFOREA,AVVELE,AVDEPE,TCGINV,
     I                REAMFG,REAK,REAKT,EXPRED,EXPREV,LEN,
     I                DELTH,TW,DELTS,DELT60,UUNITS,
     O                KOREA)
C
        END IF
C
C       calculate oxygen saturation level for current water
C       temperature; satdo is expressed as mg oxygen per liter
C
        SATDO= 14.652 + TW*(-0.41022 + TW*(0.007991 - 0.7777E-4*TW))
C
C       adjust satdo to conform to prevalent atmospheric pressure
C       conditions; cfpres is the ratio of site pressure to sea level
C       pressure
C
        SATDO= CFPRES*SATDO
C
C       compute dissolved oxygen value after reaeration,and
C       the reaeration flux
C
        DOREA = KOREA*(SATDO - DOX)
        DOX   = DOX + DOREA
        READOX= DOREA*VOLSP
C
C       calculate bod decay, and compute associated fluxes
C
        CALL BODDEC
     I              (TW,KBOD20,TCBOD,
     M               BOD,DOX,
     O               BODOX)
C
        BODDOX= -BODOX*VOLSP
        DECBOD= -BODOX*VOLSP
C
      ELSE
C
C       there is too little water to warrant simulation of
C       quality processes
C
        BODOX = 0.0
        READOX= 0.0
        BODDOX= 0.0
        BENDOX= 0.0
        DECBOD= 0.0
        BNRBOD= 0.0
        SNKBOD= 0.0
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   BODDEC
     I                    (TW,KBOD20,TCBOD,
     M                     BOD,DOX,
     O                     BODOX)
C
C     + + + PURPOSE + + +
C     Calculate bod decay
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       BOD,BODOX,DOX,KBOD20,TCBOD,TW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TW     - water temperature in degrees C
C     KBOD20 - ???
C     TCBOD  - ???
C     BOD    - ???
C     DOX    - dissolved oxygen concentration in mg/l
C     BODOX  - ???
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate concentration of oxygen required to satisfy computed bod
C     decay; bodox is expressed as mg oxygen/liter.ivl
C
      BODOX= (KBOD20*(TCBOD**(TW -20.)))*BOD
      IF (BODOX .GT. BOD) THEN
        BODOX= BOD
      END IF
C
C     adjust dissolved oxygen state variable to acount for oxygen
C     lost to bod decay, and compute concentration flux
      IF (BODOX .GE. DOX) THEN
        BODOX= DOX
        DOX  = 0.0
      ELSE
        DOX= DOX - BODOX
      END IF
C
C     adjust bod state variable to account for bod decayed
      BOD= BOD - BODOX
      IF (BOD .LT. 0.0001) THEN
        BOD= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OXACC
     I                   (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes in subroutine group oxrx for printout
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION OXRX2 + + +
      INCLUDE    'crhox.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I2,I7
C
C     + + + EXTERNALS + + +
      EXTERNAL  ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2=2
      I7=7
C
C     handle flux groups dealing with reach-wide variables
      CALL ACCVEC
     I            (I2,OXIF(1,FRMROW),
     M             OXIF(1,TOROW))
C
      CALL ACCVEC
     I            (I2,OXCF1(1,FRMROW),
     M             OXCF1(1,TOROW))
C
      IF (NEXITS .GT. 1) THEN
C       handle flux groups dealing with individual exit gates
        DO 10 I= 1,2
          CALL ACCVEC
     I                (NEXITS,OXCF2(1,I,FRMROW),
     M                 OXCF2(1,I,TOROW))
 10     CONTINUE
      END IF
C
      CALL ACCVEC
     I            (I7,OXCF3(1,FRMROW),
     M             OXCF3(1,TOROW))
C
      CALL ACCVEC
     I            (I7,OXCF4(1,FRMROW),
     M             OXCF4(1,TOROW))
C
      RETURN
      END
C
C
C
      SUBROUTINE   OXBEN
     I                   (BRBOD,BENOD,TW,DEPCOR,SCRFAC,TCBEN,
     I                    EXPOD,EXPREL,
     M                    DOX,BOD,
     O                    DOBEN,BODBNR)
C
C     + + + PURPOSE + + +
C     Simulate benthal oxygen demand and benthal release of BOD
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       BENOD,BOD,BRBOD(2),DEPCOR,DOX,SCRFAC,TW,TCBEN,
     $           EXPOD,EXPREL,DOBEN,BODBNR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BRBOD  - ???
C     BENOD  - ???
C     TW     - water temperature in degrees C
C     DEPCOR - ???
C     SCRFAC - ???
C     TCBEN  - ???
C     EXPOD  - ???
C     EXPREL - ???
C     DOX    - dissolved oxygen concentration in mg/l
C     BOD    - ???
C     DOBEN  - ???
C     BODBNR - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       BENOX,RELBOD
C
C     + + + INTRINSICS + + +
      INTRINSIC   EXP
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate amount of dissolved oxygen required to satisfy
C     benthal oygen demand (mg/m2.ivl)
      BENOX= BENOD*(TCBEN**(TW -20.))*(1.0 -EXP(-EXPOD*DOX))
C
C     adjust dissolved oxygen state variable to acount for oxygen
C     lost to benthos, and compute concentration flux
      DOBEN= DOX
      DOX  = DOX - BENOX*DEPCOR
      IF (DOX .GE. 0.001) THEN
        DOBEN= BENOX*DEPCOR
      END IF
      IF (DOX .LT. 0.001) THEN
        DOX= 0.0
      END IF
C
C     calculate benthal release of bod; release is a function of
C     dissolved oxygen (dox) and a step function of stream velocity;
C     brbod(1) is the aerobic benthal release rate; brbod(2) is the
C     base increment to benthal release under decreasing do
C     concentration; relbod is expressed as mg bod/m2.ivl
      RELBOD= (BRBOD(1)+BRBOD(2)*EXP( -EXPREL*DOX))*SCRFAC
C
C     add release to bod state variable and compute concentration flux
      BOD   = BOD + RELBOD*DEPCOR
      BODBNR= RELBOD*DEPCOR
C
      RETURN
      END
C
C
C
      SUBROUTINE   OXPRT
     I                   (LEV,PRINTU,FACTA,FACTB,FLUXID)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units
C     and print out results
C     Note: local arrays have same dimensions as corresponding arrays
C      in osv, except for dropping of dimension lev, where applicable
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
C     + + + COMMON BLOCKS- SCRTCH, VERSION OXRX2 + + +
      INCLUDE    'crhox.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I1,I2,I7,J
      REAL        PIFLX(2),PCFLX1(2),PCFLX2(5,2),PCFLX3(7),PCFLX4(7),
     $            PRDOX,PRDOXS,PRBOD,PRBODS,TCFLX3,TCFLX4,PROXST(2),
     $            DOXIN,DOXDIF,BODIN,BODDIF
      CHARACTER*8 UNITID
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,BALCHK
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,1H ,'*** OXRX ***')
 2010 FORMAT (/,1H ,'  DISSOLVED OXYGEN')
 2020 FORMAT (/,1H ,'  STATE VARIABLES')
 2030 FORMAT (' ','    CONCENTRATION  (MG/L)',15X,1PE10.3)
 2040 FORMAT (/,1H ,'    STORAGE (',A3,')',11X,1PE10.3)
 2050 FORMAT (/,1H ,'  FLUXES (',A3,')')
 2060 FORMAT (' ','    INFLOW',18X,1PE10.3)
 2070 FORMAT (' ',67X,'BENTHAL',4X,'NITRIFI-',6X,'PHYTO.',8X,'ZOO.',
     $        '  BENTH.ALG.')
 2080 FORMAT (' ','    OTHER GAINS/LOSSES',11X,'TOTAL  REAERATION',
     $        '   BOD DECAY',6X,'DEMAND',6X,'CATION',6X,'GROWTH',
     $        7X,'RESP.',6X,'GROWTH')
 2090 FORMAT (' ',6X,'(POS. INDICATES GAIN)',1X,8(1PE10.3,2X))
 2100 FORMAT (/,1H ,'    OUTFLOW',17X,1PE10.3)
 2110 FORMAT (/,1H ,'    OUTFLOWS',21X,'TOTAL',6X,
     $        'INDIVIDUAL GATE OUTFLOWS',/,' ',31X,'OUTFLOW',5(2X,I10))
 2120 FORMAT (' ',28X,6(1PE10.3,2X))
 2130 FORMAT ('     ',A3)
 2140 FORMAT (/,1H ,'  BIOCHEMICAL OXYGEN DEMAND')
 2150 FORMAT (' ',55X,'BENTHAL',15X,'DENITRIF-',6X,'PHYTO.',8X,'ZOO.',
     $        '  BENTH.ALG.')
 2160 FORMAT (' ','    OTHER GAINS/LOSSES',11X,'TOTAL       DECAY',
     $        5X,'RELEASE',8X,'SINK     ICATION',3(7X,'DEATH'))
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      I2= 2
      I7= 7
      WRITE (UNITID,2130) FLUXID(1:3)
C
C     initialize variables used in totals
      TCFLX3= 0.0
      TCFLX4= 0.0
C
C     convert variables to external units
C
C     Rchres-wide variables
C
C     inflow fluxes
      CALL TRNVEC
     I            (I2,OXIF(1,LEV),FACTA,FACTB,
     O             PIFLX)
C
C     computed fluxes
      CALL TRNVEC
     I            (I2,OXCF1(1,LEV),FACTA,FACTB,
     O             PCFLX1)
C
      CALL TRNVEC
     I            (I7,OXCF3(1,LEV),FACTA,FACTB,
     O             PCFLX3)
C
      DO 10 J=1,7
        TCFLX3= TCFLX3 + PCFLX3(J)
 10   CONTINUE
C
      CALL TRNVEC
     I            (I7,OXCF4(1,LEV),FACTA,FACTB,
     O             PCFLX4)
C
      DO 20 J=1,7
        TCFLX4= TCFLX4 + PCFLX4(J)
 20   CONTINUE
C
C     storages
      CALL TRNVEC
     I            (I2,OXST2(1,1),FACTA,FACTB,
     O             PROXST)
C
C     *** this cant be done here as rdox and rbod will not be accumulated
C     *** correctly ************************
C     *** i will try rqual, jlk 9/83 ***
C     rdox= dox*vol
C     rbod= bod*vol
C
      PRDOX = OXST2(1,1)*FACTA
      PRBOD = OXST2(2,1)*FACTA
      PRDOXS= OXST2(1,LEV)*FACTA
      PRBODS= OXST2(2,LEV)*FACTA
C
      IF (NEXITS .GT. 1) THEN
C       exit-specific variables
        DO 30 I= 1,2
          CALL TRNVEC
     I                (NEXITS,OXCF2(1,I,LEV),FACTA,FACTB,
     O                 PCFLX2(1,I))
 30     CONTINUE
      END IF
C
C     do printout on unit printu
C
C     dissolved oxygen printout
C
      WRITE (PRINTU,2000)
      WRITE (PRINTU,2010)
      WRITE (PRINTU,2020)
      WRITE (PRINTU,2030)  OXST(1)
      WRITE (PRINTU,2040)  FLUXID, PROXST(1)
      WRITE (PRINTU,2050)  FLUXID
      WRITE (PRINTU,2060)  PIFLX(1)
      WRITE (PRINTU,2070)
      WRITE (PRINTU,2080)
      WRITE (PRINTU,2090)  TCFLX3, (PCFLX3(J),J=1,7)
C
      IF (NEXITS .GT. 1) THEN
        WRITE (PRINTU,2110)  (J,J=1,NEXITS)
        WRITE (PRINTU,2120)  PCFLX1(1), (PCFLX2(J,1),J=1,NEXITS)
      ELSE
        WRITE (PRINTU,2100)  PCFLX1(1)
      END IF
C
C     material balance
      DOXIN = PIFLX(1) + PCFLX3(1)
      DOXDIF= DOXIN + PCFLX3(2) + PCFLX3(3) + PCFLX3(4) + PCFLX3(5) +
     $                PCFLX3(6) + PCFLX3(7) - PCFLX1(1)
      I= 3
      CALL BALCHK
     I            (I,RCHNO,DATIM,MESSU,PRINTU,MSGFL,
     I             PRDOXS,PRDOX,DOXIN,DOXDIF,UNITID,I1,
     M             OXWCNT)
C
C     bod printout
C
      WRITE (PRINTU,2140)
      WRITE (PRINTU,2020)
      WRITE (PRINTU,2030)  OXST(2)
      WRITE (PRINTU,2040)  FLUXID, PROXST(2)
      WRITE (PRINTU,2050)  FLUXID
      WRITE (PRINTU,2060)  PIFLX(2)
      WRITE (PRINTU,2150)
      WRITE (PRINTU,2160)
      WRITE (PRINTU,2090)  TCFLX4, (PCFLX4(J),J=1,7)
C
      IF (NEXITS .GT. 1) THEN
        WRITE (PRINTU,2110)  (J,J=1,NEXITS)
        WRITE (PRINTU,2120)  PCFLX1(2), (PCFLX2(J,2),J=1,NEXITS)
      ELSE
        WRITE (PRINTU,2100)  PCFLX1(2)
      END IF
C
C     material balance
      BODIN = PIFLX(2) + PCFLX4(2) + PCFLX4(5) + PCFLX4(6) + PCFLX4(7)
      BODDIF= BODIN + PCFLX4(1) + PCFLX4(3) + PCFLX4(4) - PCFLX1(2)
      I= 3
      CALL BALCHK
     I            (I,RCHNO,DATIM,MESSU,PRINTU,MSGFL,
     I             PRBODS,PRBOD,BODIN,BODDIF,UNITID,I1,
     M             OXWCNT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   OXRB
C
C     + + + PURPOSE + + +
C     Handle subroutine group oxrx
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION OXRX2 + + +
      INCLUDE    'crhox.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,2
        IF (OXCF1X(I) .GE. 1) THEN
          PAD(OXCF1X(I) + IVL1)= OXCF1(I,1)
        END IF
 10   CONTINUE
C
      IF (NEXITS .GT. 1) THEN
        DO 30 J= 1,2
          DO 20 I= 1,NEXITS
            IF (OXCF2X(I,J) .GE. 1) THEN
              PAD(OXCF2X(I,J) + IVL1)= OXCF2(I,J,1)
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
      SUBROUTINE   OXRP
C
C     + + + PURPOSE + + +
C     Handle subroutine group oxrx
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION OXRX2 + + +
      INCLUDE    'crhox.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,2
        IF (OXSTX(I) .GE. 1) THEN
          PAD(OXSTX(I) + IVL1)= OXST(I)
        END IF
 10   CONTINUE
C
      IF (SATFP .GE. 1) THEN
        PAD(SATFP + IVL1)= SATDO
      END IF
C
      RETURN
      END
C
C     4.2(3).10.3.6
C
      SUBROUTINE   OXRST
     I                   (LEV)
C
C     + + + PURPOSE + + +
C     Reset flux and state variables for subroutine group oxrx
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION OXRX2 + + +
      INCLUDE    'crhox.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I2,I7
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2=2
      I7=7
C     handle flux groups dealing with reach-wide variables
      CALL SETVEC
     I            (I2,0.0,
     O             OXIF(1,LEV))
C
      CALL SETVEC
     I            (I2,0.0,
     O             OXCF1(1,LEV))
C
      IF (NEXITS .GT. 1) THEN
C       handle flux groups dealing with individual exit gates
        DO 10 I= 1,2
          CALL SETVEC
     I                (NEXITS,0.0,
     O                 OXCF2(1,I,LEV))
 10     CONTINUE
      END IF
C
      CALL SETVEC
     I            (I7,0.0,
     O             OXCF3(1,LEV))
C
      CALL SETVEC
     I            (I7,0.0,
     O             OXCF4(1,LEV))
C
C     save storages for balance checks
      OXST2(1,LEV)= OXST2(1,1)
      OXST2(2,LEV)= OXST2(2,1)
C
      RETURN
      END
