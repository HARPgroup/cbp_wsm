C
C
C
      SUBROUTINE   PPLANK
C
C     + + + PURPOSE + + +
C     Process input for the plank section of the rchres application
C     module
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLANK1 + + +
      INCLUDE    'crhpl.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I1,I2,I4,J,SCLU,SGRP,N,RETCOD
      REAL       RVAL(6),TVAL,R0
C
C     + + + EXTERNALS + + +
      EXTERNAL   RTABLE,ITABLE,OMSG,ZIPI,OMSTI
      EXTERNAL   ZIPR,MDATBL
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION PLANK')
 2030 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION PLANK')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2000)
      END IF
C
      SCLU= 349
      I1  = 1
      R0 = 0.0
C
C     initialize month-data input
      I= 36
      CALL ZIPR (I,R0,
     O           PLAFXM)
      CALL ZIPR (I,R0,
     O           PLACNM)
C
C     initialize atmospheric deposition fluxes
      I= 15
      CALL ZIPR (I,R0,
     O           PKCF3)
      CALL ZIPR (I,R0,
     O           PKCF4)
C
      I= 4
      J= 0
      CALL ZIPI(I,J,PKECNT)
C
C     flags - table-type plnk-flgs
      I2= 95
      I4=  8
      CALL ITABLE
     I             (I2,I1,I4,UUNITS,
     M              PKFG)
C
      IF (ZOOFG.EQ.1.AND.PHYFG.EQ.0) THEN
C       error - zooplankton cannot be simulated without phytoplankton
        SGRP = 1
        CALL OMSTI (RCHNO)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (NSFG.EQ.1.AND.TAMFG.EQ.0) THEN
C       error - ammonia cannot be included in n supply if it is not
C       being simulated
        SGRP = 2
        CALL OMSTI (RCHNO)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (PO4FG.EQ.0) THEN
C       error - phosphate must be simulated if plankton are being
C       simulated
        SGRP = 3
        CALL OMSTI (RCHNO)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C     atmospheric deposition flags - table-type plnk-ad-flgs
      I2= 96
      I4=  6
      CALL ITABLE
     I             (I2,I1,I4,UUNITS,
     M              PLADFG)
C
C     read in month-data tables where necessary
      DO 30 J= 1, 3
        N= 2*(J- 1)+ 1
        IF (PLADFG(N) .GT. 0) THEN
C         monthly flux must be read
          CALL MDATBL
     I                (PLADFG(N),
     O                 PLAFXM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
          IF (UUNITS .EQ. 1) THEN
C           convert from lb/ac.day to mg.ft3/l.ft2.ivl
            DO 10 I= 1, 12
              PLAFXM(I,J)= PLAFXM(I,J)*0.3677*DELT60/24.0
 10         CONTINUE
          ELSE IF (UUNITS .EQ. 2) THEN
C           convert from kg/ha.day to mg.m3/l.m2.ivl
            DO 20 I= 1, 12
              PLAFXM(I,J)= PLAFXM(I,J)*0.1*DELT60/24.0
 20         CONTINUE
          END IF
        END IF
        IF (PLADFG(N+1) .GT. 0) THEN
C         monthly ppn conc must be read
          CALL MDATBL
     I                (PLADFG(N+1),
     O                 PLACNM(1,J),RETCOD)
        END IF
 30   CONTINUE
C
      IF (HTFG.EQ.0) THEN
C       fraction of surface exposed - table-type surf-exposed
        I2= 97
        I4=  1
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                RVAL)
        CFSAEX= RVAL(1)
      END IF
C
C     general parameters - group 1.  table-type plnk-parm1
      I2= 98
      I4=  6
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              PKPM1)
C
C     convert max growth rate to units 1/ivl
      MALGR= MALGR*DELT60
C
C     compute derived conversion factors
      CVBC  = BPCNTC/100.0
      CVNRBO= NONREF*CVBO
      CVPB  = 31.0/(1000.*CVBP)
      CVBCL = 31.0*RATCLP/CVPB
C
C     general parameters - group 2.  table-type plnk-parm2
      I2= 99
      I4=  7
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              PKPM2)
C
C
C     general parameters - group 3.  table-type plnk-parm3
      I2= 100
      I4=  6
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              PKPM3)
C
C     convert rates from 1/hr to 1/ivl
      DO 40 I= 1,4
        PKPM3(I)= PKPM3(I)*DELT60
 40   CONTINUE
C
      IF (PHYFG.EQ.1) THEN
C       phytoplankton-specific parms - table-type phyto-parm
        I2= 101
        I4=  6
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                PKPM4)
C
C       change settling rates to units of 1/ivl
        PHYSET= PHYSET*DELT60
        REFSET= REFSET*DELT60
C
        IF (ZOOFG.EQ.1) THEN
C         zooplankton-specific parameters.  table-type zoo-parm1
          I2= 102
          I4=  5
          CALL RTABLE
     I                 (I2,I1,I4,UUNITS,
     M                 PKPM5)
C
C         convert rates from 1/hr to 1/ivl
          DO 50 J= 1,5
            PKPM5(J)= PKPM5(J)*DELT60
 50       CONTINUE
C
C         second group of zoo parameters.  table-type zoo-parm2
          I2= 103
          I4=  4
          CALL RTABLE
     I                 (I2,I1,I4,UUNITS,
     M                  PKPM6)
C
        END IF
C
      END IF
C
      IF (BALFG.EQ.1) THEN
C       benthic algae-specific parms.  table-type benal-parm
        I2= 104
        I4=  3
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                PKPM7)
C       convert maximum benthic algae to micromoles of phosphorus
        MBAL= MBAL/CVPB
      END IF
C
C     initial conditions.  table-type plnk-init
      I2= 105
      I4=  6
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              RVAL)
C
      PHYTO= RVAL(1)
C
      IF (PHYFG .EQ. 0) THEN
C       initialize fluxes of inactive constituent
        ROPHYT= 0.0
        DO 60 I= 1, NEXITS
          OPHYT(I)= 0.0
 60     CONTINUE
      END IF
C
      IF (ZOOFG .EQ. 1) THEN
C       convert zoo to mg/l
        ZOO= RVAL(2)*ZOMASS
      ELSE
C       zooplankton not simulated, but use default values of
C       zomass and zoo to convert/compute zoo in case of bugs in later code
        ZOO= .03*.0003
C       initialize fluxes of inactive constituent
        ROZOO= 0.0
        DO 70 I= 1, NEXITS
          OZOO(I)= 0.0
 70     CONTINUE
      END IF
      BENAL= RVAL(3)
      ORN  = RVAL(4)
      ORP  = RVAL(5)
      ORC  = RVAL(6)
C
C     compute derived quantities
      PHYCLA= PHYTO*CVBCL
      BALCLA= BENAL*CVBCL
      TVAL  = BOD/CVBO
C
      IF (PHYFG.EQ.1) THEN
        TVAL= TVAL+ PHYTO
        IF (ZOOFG.EQ.1) THEN
          TVAL= TVAL+ ZOO
        END IF
      END IF
C
      TORN  = ORN+ CVBN*TVAL
      TORP  = ORP+ CVBP*TVAL
      TORC  = ORC+ CVBC*TVAL
      POTBOD= BOD
C
      IF (PHYFG.EQ.1) THEN
        POTBOD= POTBOD+ (CVNRBO*PHYTO)
        IF (ZOOFG.EQ.1) THEN
          POTBOD= POTBOD+ (CVNRBO*ZOO)
        END IF
      END IF
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2030)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLANK
C
C     + + + PURPOSE + + +
C     Simulate behavior of plankton populations and associated
C     reactions
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLANK2 + + +
      INCLUDE  'crhpl.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I,N
      REAL     BALLIT,CFLIT,EXTCLA,EXTSED,INLIT,PHYLIT,REFR,
     $         SNKORC,SNKORN,SNKORP,SNKPHY,ZEAT,DOPHY,DOZOO,DOBALG,
     $         BODPHY,BODZOO,BODBAL,TAMPHY,NO3PHY,PO4PHY,TAMBAL,NO3BAL,
     $         PO4BAL,NITZOO,PO4ZOO,VOLSP,INORN,INORP,INORC
      DOUBLE PRECISION DORC,DORN,DORP,DPHYTO
      CHARACTER*4 LIMIT(7),LIMC
C
C     + + + FUNCTIONS + + +
      REAL         DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL ADVPLK,SINK,ADVECT,DAYVAL
      EXTERNAL LITRCH,PHYRX,ZORX,BALRX
C
C     + + + DATA INITIALIZATIONS + + +
      DATA     LIMIT/'LIT ','NON ','TEM ','NIT ','PO4 ','NONE','WAT '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A4)
C
C     + + + END SPECIFICATIONS + + +
C
C     single precision version of vol
      VOLSP= VOL
C
C     define fraction of biomass which is refractory material
      REFR = 1.0 - NONREF
C
C     get time series
      IF (PRECFP .GE. 1) THEN
C       precipitation is input
        PREC= PAD(PRECFP+IVL1)
      ELSE
C       no precipitation
        PREC= 0.0
      END IF
      IF (HYDRFG .NE. 1) THEN
C       section hydr is inactive, so sarea must be on the pad if needed
        SAREA= PAD(SAFP+IVL1)
      END IF
C
C     compute atmospheric deposition influx
      DO 10 I= 1, 3
        N= 2*(I-1)+ 1
C       dry deposition
        IF (PLADFG(N) .LE. -1) THEN
          PLADDR(I)= SAREA*PAD(PLAFFP(I)+IVL1)
        ELSE IF (PLADFG(N) .GE. 1) THEN
          PLADDR(I)= SAREA*DAYVAL(PLAFXM(MON,I),PLAFXM(NXTMON,I),DAY,
     I                            NDAYS)
        ELSE
          PLADDR(I)= 0.0
        END IF
C       wet deposition
        IF (PLADFG(N+1) .LE. -1) THEN
          PLADWT(I)= PREC*SAREA*PAD(PLACFP(I)+IVL1)
        ELSE IF (PLADFG(N+1) .GE. 1) THEN
          PLADWT(I)= PREC*SAREA*DAYVAL(PLACNM(MON,I),PLACNM(NXTMON,I),
     I                                 DAY,NDAYS)
        ELSE
          PLADWT(I)= 0.0
        END IF
 10   CONTINUE
C
C     get inflowing material from pad; advect
      IF (PHYFG .EQ. 1) THEN
C       phytoplankton simulated
        IF (IPYFP .GT. 0) THEN
          IPHYTO= PAD(IPYFP + IVL1)
        ELSE
          IPHYTO= 0.0
        END IF
C       advect phytoplankton
        CALL ADVPLK
     I              (IPHYTO,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I               EOVOL,NEXITS,OREF,MXSTAY,SEED,DELTS,
     M               PHYTO,
     O               ROPHYT,OPHYT)
C
        DPHYTO= PHYTO
        CALL SINK
     I            (VOL,AVDEPE,PHYSET,
     M             DPHYTO,
     O             SNKPHY)
        PHYTO = DPHYTO
C
        IF (ZOOFG .EQ. 1) THEN
C         zooplankton on
          IF (IZFP .GT. 0) THEN
            IZOO= PAD(IZFP + IVL1)
          ELSE
            IZOO= 0.0
          END IF
C         advect zooplankton
          CALL ADVPLK
     I                (IZOO,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I                 EOVOL,NEXITS,OREF,MXSTAY,SEED,DELTS,
     M                 ZOO,
     O                 ROZOO,OZOO)
        END IF
      END IF
C
      IF (IONFP .GT. 0) THEN
C       input organic nitrogen
        IORN= PAD(IONFP + IVL1)
      ELSE
C       no input organic nitrogen
        IORN= 0.0
      END IF
C
      INORN= IORN+ PLADDR(1)+ PLADWT(1)
C
C     advect organic nitrogen
      DORN= ORN
      CALL ADVECT
     I            (INORN,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I             EOVOL,NEXITS,
     M             DORN,
     O             ROORN,OORN)
      CALL SINK
     I          (VOL,AVDEPE,REFSET,
     M           DORN,
     O           SNKORN)
      ORN= DORN
C
      IF (IOPFP .GT. 0) THEN
C       input organic phosphorus
        IORP= PAD(IOPFP + IVL1)
      ELSE
C       no input organic phosphorus
        IORP= 0.0
      END IF
C
      INORP= IORP+ PLADDR(2)+ PLADWT(2)
C
C     advect organic phosphorus
      DORP= ORP
      CALL ADVECT
     I            (INORP,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I             EOVOL,NEXITS,
     M             DORP,
     O             ROORP,OORP)
C
      CALL SINK
     I          (VOL,AVDEPE,REFSET,
     M           DORP,
     O           SNKORP)
      ORP= DORP
C
      IF (IOCFP .GT. 0) THEN
C       input total organic carbon
        IORC= PAD(IOCFP + IVL1)
      ELSE
C       no input total organic carbon
        IORC= 0.0
      END IF
C
      INORC= IORC+ PLADDR(3)+ PLADWT(3)
C
C     advect total organic carbon
      DORC= ORC
      CALL ADVECT
     I            (INORC,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I             EOVOL,NEXITS,
     M             DORC,
     O             ROORC,OORC)
      CALL SINK
     I          (VOL,AVDEPE,REFSET,
     M           DORC,
     O           SNKORC)
      ORC= DORC
C
      IF (AVDEPE .GT. 0.17) THEN
C       calculate solar radiation absorbed; solrad is the solar
C       radiation at gage, corrected for location of reach; 0.97
C       accounts for surface reflection (assumed 3 per cent); cfsaex
C       is the ratio of radiation incident to water surface to gage
C       radiation values (accounts for regional differences, shading
C       of water surface, etc); inlit is a measure of light intensity
C       immediately below surface of reach/res and is expressed as
C       ly/min
        SOLRAD= PAD(SOLFP + IVL1)
        INLIT = 0.97*CFSAEX*SOLRAD/DELT
C
        IF (SDLTFG .EQ. 1) THEN
C         influence of sediment on light extinction is considered
          IF (SEDFG .EQ. 0) THEN
C           read total sediment conc. from pad; units are mg/l
            SSED(4)= PAD(SSEDFP(4) + IVL1)
          ELSE
C           data are available from module section sedtrn
          END IF
C         estimate contribution of sediment to light extinction
          EXTSED= LITSED*SSED(4)
        ELSE
          EXTSED= 0.0
        END IF
C       calculate contribution of phytoplankton to light extinction
C       (self-shading)
        EXTCLA= .00452*PHYTO*CVBCL
C       calculate light available for algal growth
        CALL LITRCH
     I              (INLIT,EXTB,EXTCLA,EXTSED,AVDEPE,PHYFG,BALFG,
     O               PHYLIT,BALLIT,CFLIT)
C
        IF (PHYFG .EQ. 1) THEN
C         simulate phytoplankton
          CALL PHYRX
     I               (PHYLIT,TW,TALGRL,TALGRH,TALGRM,MALGR,CMMP,
     I                CMMNP,TAMFG,AMRFG,NSFG,CMMN,CMMLT,DELT60,
     I                CFLIT,ALR20,CVBPN,PHFG,DECFG,CVBPC,PALDH,
     I                NALDH,CLALDH,ALDL,ALDH,ANAER,OXALD,ALNPR,
     I                CVBO,REFR,CVNRBO,CVPB,CVBCL,LIMIT,CO2,
     M                PO4,NO3,TAM,DOX,ORN,ORP,ORC,BOD,PHYTO,
     O                LIMPHY,PHYCO2,PHYCLA,DOPHY,BODPHY,TAMPHY,
     O                NO3PHY,PO4PHY)
C
C         compute associated fluxes
          PHYDOX= DOPHY*VOLSP
          PHYBOD= BODPHY*VOLSP
          PHYTAM= TAMPHY*VOLSP
          PHYNO3= NO3PHY*VOLSP
          PHYPO4= PO4PHY*VOLSP
C
          IF (ZOOFG .EQ. 1) THEN
C           simulate zooplankton
            CALL ZORX
     I                (ZFIL20,TCZFIL,TW,PHYTO,MZOEAT,ZEXDEL,CVPB,
     I                 ZRES20,TCZRES,ANAER,ZOMASS,TAMFG,REFR,
     I                 ZFOOD,ZD,OXZD,CVBN,CVBP,CVBC,CVNRBO,CVBO,
     M                 DOX,BOD,ZOO,ORN,ORP,ORC,TAM,NO3,PO4,
     O                 ZEAT,ZCO2,DOZOO,BODZOO,NITZOO,PO4ZOO)
C
C           compute associated fluxes
            ZOODOX= -DOZOO*VOLSP
            ZOOBOD= BODZOO*VOLSP
            IF (TAMFG .NE. 0) THEN
              ZOOTAM= NITZOO*VOLSP
            ELSE
              ZOONO3= NITZOO*VOLSP
            END IF
            ZOOPO4= PO4ZOO*VOLSP
C           update phytoplankton state variable to account for
C           zooplankton predation
            PHYTO = PHYTO - ZEAT
C           convert phytoplankton expressed as mg biomass/l to
C           chlorophyll a expressed as ug/l
            PHYCLA= PHYTO*CVBCL
          ELSE
C           zooplankton not simulated
            ZCO2= 0.0
          END IF
        ELSE
C         phytoplankton and zooplankton not simulated
          PHYCO2= 0.0
          ZCO2  = 0.0
        END IF
C
        IF (BALFG .EQ. 1) THEN
C         simulate benthic algae
          CALL BALRX
     I               (BALLIT,TW,TALGRL,TALGRH,TALGRM,MALGR,CMMP,
     I                CMMNP,TAMFG,AMRFG,NSFG,CMMN,CMMLT,DELT60,
     I                CFLIT,ALR20,CVBPN,PHFG,DECFG,CVBPC,PALDH,
     I                NALDH,ALDL,ALDH,ANAER,OXALD,CFBALG,CFBALR,
     I                ALNPR,CVBO,REFR,CVNRBO,CVPB,MBAL,DEPCOR,
     I                LIMIT,CVBCL,CO2,
     M                PO4,NO3,TAM,DOX,ORN,ORP,ORC,BOD,BENAL,
     O                LIMBAL,BALCO2,BALCLA,DOBALG,BODBAL,
     O                TAMBAL,NO3BAL,PO4BAL)
C
C         compute associated fluxes
          BALDOX= DOBALG*VOLSP
          BALBOD= BODBAL*VOLSP
          BALTAM= TAMBAL*VOLSP
          BALNO3= NO3BAL*VOLSP
          BALPO4= PO4BAL*VOLSP
        ELSE
C         benthic algae not simulated
          BALCO2= 0.0
        END IF
      ELSE
C       not enough water in reach/res to warrant simulation of
C       quality processes
        PHYCO2= 0.0
        BALCO2= 0.0
        ZCO2  = 0.0
        PHYDOX= 0.0
        ZOODOX= 0.0
        BALDOX= 0.0
        PHYBOD= 0.0
        ZOOBOD= 0.0
        BALBOD= 0.0
        PHYTAM= 0.0
        ZOOTAM= 0.0
        BALTAM= 0.0
        PHYNO3= 0.0
        ZOONO3= 0.0
        BALNO3= 0.0
        PHYPO4= 0.0
        ZOOPO4= 0.0
        BALPO4= 0.0
C
        IF (PHYFG .EQ. 1) THEN
C         water scarcity limits phytoplankton growth
          LIMC  = LIMIT(7)
          READ(LIMC,1000) LIMPHY
          PHYCLA= PHYTO*CVBCL
        END IF
C
        IF (BALFG .EQ. 1) THEN
C         water scarcity limits benthic algae growth
          LIMC  = LIMIT(7)
          READ(LIMC,1000) LIMBAL
          BALCLA= BENAL*CVBCL
        END IF
      END IF
C
C     calculate total organic nitrogen, total organic phosphorus,
C     total organic carbon, and potential biochemical oxygen demand
      IF (PHYFG .EQ. 1) THEN
        IF (ZOOFG .EQ. 1) THEN
C         phyto and zoo on
          TORN  = ORN + CVBN*(ZOO + PHYTO + BOD/CVBO)
          TORP  = ORP + CVBP*(ZOO + PHYTO + BOD/CVBO)
          TORC  = ORC + CVBC*(ZOO + PHYTO + BOD/CVBO)
          POTBOD= BOD + CVNRBO*(ZOO + PHYTO)
        ELSE
C         phyto on, zoo off
          TORN  = ORN + CVBN*(PHYTO + BOD/CVBO)
          TORP  = ORP + CVBP*(PHYTO + BOD/CVBO)
          TORC  = ORC + CVBC*(PHYTO + BOD/CVBO)
          POTBOD= BOD + CVNRBO*PHYTO
        END IF
      ELSE
C       phyto and zoo off
        TORN  = ORN + CVBN*(BOD/CVBO)
        TORP  = ORP + CVBP*(BOD/CVBO)
        TORC  = ORC + CVBC*(BOD/CVBO)
        POTBOD= BOD
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ADVPLK
     I                    (IPLANK,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I                     EOVOL,NEXITS,OREF,MXSTAY,SEED,DELTS,
     M                     PLANK,
     O                     ROPLK,OPLK)
C
C     + + + PURPOSE + + +
C     Advect plankton
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NEXITS
      REAL       IPLANK,SROVOL,EROVOL,SOVOL(NEXITS),
     $           EOVOL(NEXITS),OREF,MXSTAY,SEED,DELTS,PLANK,
     $           ROPLK,OPLK(NEXITS)
      DOUBLE PRECISION VOL,VOLS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IPLANK - ???
C     VOLS   - ???
C     SROVOL - ???
C     VOL    - volume of water in reach above bed
C     EROVOL - ???
C     SOVOL  - ???
C     EOVOL  - ???
C     NEXITS - number of exits from the operation
C     OREF   - ???
C     MXSTAY - ???
C     SEED   - ???
C     DELTS  - ???
C     PLANK  - ???
C     ROPLK  - ???
C     OPLK   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    N
      REAL       OFLO,STAY,MSTAY,PLNKAD
      DOUBLE PRECISION DPLKAD
C
C     + + + EXTERNALS + + +
      EXTERNAL   ADVECT
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate concentration of plankton not subject to advection
C     during interval
      OFLO= (SROVOL + EROVOL)/DELTS
C
      IF (OREF .GT. 0.0 .AND. OFLO/OREF .LE. 100.0) THEN
        STAY= (MXSTAY - SEED)*(2.0**(-OFLO/OREF)) + SEED
      ELSE
        STAY= SEED
      END IF
C
      IF (PLANK .GT. STAY) THEN
C       convert stay to units of mass; this mass will be converted
C       back to units of concentration based on the volume of the
C       reach/res at the end of the interval
        MSTAY= STAY*VOLS
C
C       determine concentration of plankton subject to advection;
C       this value is passed into subroutine advect
        PLNKAD = PLANK - STAY
C
C       advect plankton
        DPLKAD= PLNKAD
        CALL ADVECT
     I              (IPLANK,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I               EOVOL,NEXITS,
     M               DPLKAD,
     O               ROPLK,OPLK)
        PLNKAD= DPLKAD
C
C       determine final concentration of plankton in reach/res after
C       advection
        IF (VOL .GT. 0.0) THEN
          PLANK= PLNKAD + MSTAY/VOL
        ELSE
          PLANK=PLNKAD
        END IF
      ELSE
C       no plankton leaves the reach/res
        ROPLK= 0.0
        DO 20 N= 1,NEXITS
          OPLK(N)= 0.0
 20     CONTINUE
        MSTAY= PLANK*VOLS
C
        IF (VOL .GT. 0.0) THEN
          PLANK= (MSTAY + IPLANK)/VOL
        ELSE
          PLANK= -1.0E30
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ALGRO
     I                   (LIGHT,PO4,NO3,TW,TALGRL,TALGRH,TALGRM,MALGR,
     I                    CMMP,CMMNP,TAMFG,AMRFG,TAM,NSFG,CMMN,CMMLT,
     I                    ALR20,CFLIT,DELT60,LIMIT,
     O                    LIMR,GRO,RES)
C
C     + + + PURPOSE + + +
C     Calculate unit growth and respiration rates for algae
C     population; both are expressed in units of per interval
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     TAMFG,AMRFG,NSFG
      REAL        LIGHT,PO4,NO3,TW,TALGRL,TALGRH,TALGRM,MALGR,
     $            CMMP,CMMNP,TAM,CMMN,CMMLT,ALR20,CFLIT,DELT60,
     $            GRO,RES,LIMR
      CHARACTER*4 LIMIT(7)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LIGHT  - ???
C     PO4    - ???
C     NO3    - dissolved nitrate concentration in mg/l
C     TW     - water temperature in degrees C
C     TALGRL - ???
C     TALGRH - ???
C     TALGRM - ???
C     MALGR  - ???
C     CMMP   - ???
C     CMMNP  - ???
C     TAMFG  - ???
C     AMRFG  - ???
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     NSFG   - ???
C     CMMN   - ???
C     CMMLT  - ???
C     ALR20  - ???
C     CFLIT  - ???
C     DELT60 - simulation time interval in hours
C     LIMIT  - ???
C     LIMR   - ???
C     GRO    - ???
C     RES    - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL        TCMALG,MALGRT,GROP,MALGN,LOLIM,MMN,GRON,GROL
      CHARACTER*4 LIM
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A4)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (LIGHT .GT. .001) THEN
C       sufficient light to support growth
        IF (PO4 .GT. 0.001 .AND. NO3 .GT. 0.001) THEN
C         sufficient nutrients to support growth
          IF (TW .GT. TALGRL .AND. TW .LT. TALGRH) THEN
C           water temperature allows growth
C
C           calculate temperature correction fraction
            IF (TW .LT. TALGRM) THEN
              TCMALG= (TW - TALGRL)/(TALGRM - TALGRL)
            ELSE
C             no temperature correction to maximum unit growth rate
C             is necessary; water temperature is in the optimum
C             range for phytoplankton growth
              TCMALG= 1.0
            END IF
C
C           perform temperature correction to maximum unit growth
C           rate; units of malgrt are per interval
            MALGRT= MALGR*TCMALG
C
C           calculate maximum phosphorus limited unit growth rate
            GROP= MALGRT*PO4*NO3/((PO4 + CMMP)*(NO3 + CMMNP))
C
C           calculate the maximum nitrogen limited unit growth rate
            IF (TAMFG .NE. 0) THEN
C             consider influence of tam on growth rate
              IF (AMRFG .NE. 0) THEN
C               calculate tam retardation to nitrogen limited
C               growth rate
                MALGN= MALGRT - 0.757*TAM + 0.051*NO3
C
C               check that calculated unit growth rate does not
C               exceed maximum allowable growth rate
                IF (MALGN .GT. MALGRT) THEN
                  MALGN= MALGRT
                ELSE
C                 check that calculated unit growth rate is not
C                 less than .001 of the maximum unit growth rate;
C                 if it is, set the unit growth rate equal to .001
C                 of the maximum unit growth rate
                  LOLIM= .001*MALGRT
                  IF (MALGN .LT. LOLIM) THEN
                    MALGN= LOLIM
                  END IF
                END IF
              ELSE
C               ammonia retardation is not considered
                MALGN= MALGRT
              END IF
C
              IF (NSFG .NE. 0) THEN
C               include tam in nitrogen pool for calculation of
C               nitrogen limited growth rate
                MMN= NO3 + TAM
              ELSE
C               tam is not included in nitrogen pool for calculation
C               of nitrogen limited growth
                MMN= NO3
              END IF
            ELSE
C             tam is not simulated
              MALGN= MALGRT
              MMN  = NO3
            END IF
C
C           calculate the maximum nitrogen limited unit growth rate
            GRON= (MALGN*MMN)/(CMMN + MMN)
C
C           calculate the maximum light limited unit growth rate
            GROL= (MALGRT*LIGHT)/(CMMLT + LIGHT)
C
C           find the actual algal unit growth rate (gro); gro is
C           the smallest of the three computed unit growth rates
C           (grop,gron,grol) and is expressed in units of per
C           interval; assign a three letter label to lim which will
C           be printed in the output to indicate the growth limiting
C           factor for the interval: limit(1)= 'lit'
C                                    limit(2)= 'non'
C                                    limit(3)= 'tem'
C                                    limit(4)= 'nit'
C                                    limit(5)= 'po4'
C                                    limit(6)= 'none'
C                                    limit(7)= 'wat'
C
            IF (GROP .LT. GRON .AND. GROP .LT. GROL) THEN
              GRO= GROP
              LIM= LIMIT(5)
            ELSE
              IF (GRON .LT. GROL) THEN
                GRO= GRON
                LIM= LIMIT(4)
              ELSE
                GRO= GROL
                LIM= LIMIT(1)
              END IF
            END IF
C
            IF (GRO .LT. (.000001*DELT60)) THEN
              GRO= 0.0
            END IF
C
            IF (GRO .GT. (.95*MALGRT)) THEN
C             there is no limiting factor to cause less than maximum
C             growth rate
              LIM= LIMIT(6)
            END IF
C
C           adjust growth rate if control volume is not entirely
C           contained within the euphotic zone; e.g. if only one
C           half of the control volume is in the euphotic zone, gro
C           would be reduced to one half of its specified value
            GRO= GRO*CFLIT
          ELSE
C           water temperature does not allow algal growth
            GRO= 0.0
            LIM= LIMIT(3)
          END IF
        ELSE
C         no algal growth occurs; necessary nutrients are not
C         available
          GRO= 0.0
          LIM= LIMIT(2)
        END IF
      ELSE
C       no algal growth occurs; necessary light is not available
        GRO= 0.0
        LIM= LIMIT(1)
      END IF
C
C     calculate unit algal respiration rate; res is expressed in
C     units of per interval; alr20 is the respiration rate at 20
C     degrees c
      RES= ALR20*TW/20.
C
C     save limiting factor character string as real
      READ(LIM,1000) LIMR
C
      RETURN
      END
C
C
C
      SUBROUTINE   BALDTH
     I                    (NSFG,NO3,TAM,PO4,PALDH,NALDH,ALDL,
     I                     ALDH,MBAL,DOX,ANAER,OXALD,BAL,DEPCOR,
     O                     DTHBAL)
C
C     + + + PURPOSE + + +
C     Calculate benthic algae death
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NSFG
      REAL       NO3,TAM,PO4,PALDH,NALDH,ALDL,ALDH,MBAL,
     $           DOX,ANAER,OXALD,BAL,DEPCOR,DTHBAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSFG   - ???
C     NO3    - dissolved nitrate concentration in mg/l
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     PO4    - ???
C     PALDH  - ???
C     NALDH  - ???
C     ALDL   - ???
C     ALDH   - ???
C     MBAL   - ???
C     DOX    - dissolved oxygen concentration in mg/l
C     ANAER  - ???
C     OXALD  - ???
C     BAL    - ???
C     DEPCOR - ???
C     DTHBAL - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       NIT,ALD,SLOF,BALMAX
C
C     + + + END SPECIFICATIONS + + +
C
C     determine whether to use high or low unit death rate; all
C     unit death rates are expressed in units of per interval
C
C     determine available inorganic nitrogen pool for test of
C     nutrient scarcity
      IF (NSFG .NE. 0) THEN
        NIT= NO3 + TAM
      ELSE
        NIT= NO3
      END IF
C
      IF (PO4 .GT. PALDH .AND. NIT .GT. NALDH) THEN
C       unit death rate is not incremented by nutrient scarcity
C       check for benthic algae overcrowding
        BALMAX= MBAL*DEPCOR
C
        IF (BAL .LT. BALMAX) THEN
C         unit death rate is not incremented by benthic algae
C         overcrowding
          ALD = ALDL
          SLOF= 0.0
        ELSE
C         augment unit death rate to account for benthic algae
C         overcrowding; set bal value equal to maximum; calculate
C         amount of benthic algae in excess of mbal; these benthic
C         algae are added to aldth
          ALD = ALDH
          SLOF= BAL - BALMAX
          BAL = BALMAX
        END IF
      ELSE
C       augment unit death rate to account for nutrient scarcity
        ALD= ALDH
      END IF
C
      IF (DOX .LT. ANAER) THEN
C       conditions are anaerobic, augment unit death rate
        ALD= ALD + OXALD
      END IF
C
C     use unit death rate to compute death rate; dthbal is expressed
C     as umoles of phosphorus per liter per interval
      DTHBAL= (ALD*BAL) + SLOF
C
      RETURN
      END
C
C
C
      SUBROUTINE   BALRX
     I                   (BALLIT,TW,TALGRL,TALGRH,TALGRM,MALGR,CMMP,
     I                    CMMNP,TAMFG,AMRFG,NSFG,CMMN,CMMLT,DELT60,
     I                    CFLIT,ALR20,CVBPN,PHFG,DECFG,CVBPC,PALDH,
     I                    NALDH,ALDL,ALDH,ANAER,OXALD,CFBALG,CFBALR,
     I                    ALNPR,CVBO,REFR,CVNRBO,CVPB,MBAL,DEPCOR,
     I                    LIMIT,CVBCL,CO2,
     M                    PO4,NO3,TAM,DOX,ORN,ORP,ORC,BOD,BENAL,
     O                    LIMBAL,BALCO2,BALCLA,DOBALG,BODBAL,TAMBAL,
     O                    NO3BAL,PO4BAL)
C
C     + + + PURPOSE + + +
C     Simulate behavior of benthic algae in units of umoles P per
C     liter; these units are used internally within BALRX so that
C     algal subroutines may be shared by PHYTO and BALRX; externally,
C     the benthic algae population is expressed in terms of areal
C     mass, since the population is resident entirely on the
C     bottom surface
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     AMRFG,DECFG,TAMFG,NSFG,PHFG
      REAL        ALDH,ALDL,ALNPR,ALR20,ANAER,BALCLA,BALCO2,
     $            BALLIT,BENAL,BOD,CFBALG,CFBALR,CFLIT,CMMLT,
     $            CMMN,CMMP,CMMNP,CO2,CVBCL,CVBO,CVBPC,
     $            CVBPN,CVNRBO,CVPB,DELT60,DEPCOR,DOX,
     $            MALGR,MBAL,NALDH,TAM,NO3,ORC,ORN,
     $            ORP,OXALD,PALDH,PO4,REFR,TALGRH,TALGRL,
     $            TALGRM,TW,DOBALG,BODBAL,TAMBAL,NO3BAL,PO4BAL
      REAL        LIMBAL
      CHARACTER*4 LIMIT(7)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BALLIT - ???
C     TW     - water temperature in degrees C
C     TALGRL - ???
C     TALGRH - ???
C     TALGRM - ???
C     MALGR  - ???
C     CMMP   - ???
C     CMMNP  - ???
C     TAMFG  - ???
C     AMRFG  - ???
C     NSFG   - ???
C     CMMN   - ???
C     CMMLT  - ???
C     DELT60 - simulation time interval in hours
C     CFLIT  - ???
C     ALR20  - ???
C     CVBPN  - ???
C     PHFG   - ???
C     DECFG  - ???
C     CVBPC  - ???
C     PALDH  - ???
C     NALDH  - ???
C     ALDL   - ???
C     ALDH   - ???
C     ANAER  - ???
C     OXALD  - ???
C     CFBALG - ???
C     CFBALR - ???
C     ALNPR  - ???
C     CVBO   - ???
C     REFR   - ???
C     CVNRBO - ???
C     CVPB   - ???
C     MBAL   - ???
C     DEPCOR - ???
C     LIMIT  - ???
C     CVBCL  - ???
C     CO2    - ???
C     PO4    - ???
C     NO3    - dissolved nitrate concentration in mg/l
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     DOX    - dissolved oxygen concentration in mg/l
C     ORN    - ???
C     ORP    - ???
C     ORC    - ???
C     BOD    - ???
C     BENAL  - ???
C     LIMBAL - ???
C     BALCO2 - ???
C     BALCLA - ???
C     DOBALG - ???
C     BODBAL - ???
C     TAMBAL - ???
C     NO3BAL - ???
C     PO4BAL - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       BAL,BALORC,BALORN,BALORP,DTHBAL,
     $           GRO,GROBAL,MINBAL,RES
C
C     + + + EXTERNALS + + +
      EXTERNAL   ALGRO,GROCHK,BALDTH,ORGBAL,NUTRUP
C
C     + + + END SPECIFICATIONS + + +
C
C     convert benal to units of umoles phosphorus/l (bal) for
C     internal calculations
      BAL= (BENAL/CVPB)*DEPCOR
C
C     compute unit growth and respiration rates for benthic algae;
C     determine growth limiting factor
      CALL ALGRO
     I           (BALLIT,PO4,NO3,TW,TALGRL,TALGRH,TALGRM,MALGR,CMMP,
     I            CMMNP,TAMFG,AMRFG,TAM,NSFG,CMMN,CMMLT,ALR20,
     I            CFLIT,DELT60,LIMIT,
     O            LIMBAL,GRO,RES)
C
C     calculate net growth rate of algae; grobal is expressed as
C     umoles phosphorus per liter per interval; benthic algae growth
C     will be expressed in terms of volume rather than area for the
C     duration of the subroutines subordinate to balrx; the output
C     values for benthic algae are converted to either mg biomass per
C     sq meter or mg chla per sq meter, whichever the user
C     specifies; cfbalg and cfbalr are the specified ratio of benthic
C     algae growth rate to phytoplankton growth rate and ratio of
C     benthic algae respiration rate to phytoplankton respiration
C     rate, respectively
C
      GROBAL= (GRO*CFBALG - RES*CFBALR)*BAL
C
      IF (GROBAL .GT. 0.0) THEN
C       adjust growth rate to account for limitations imposed by
C       availability of required nutrients
        CALL GROCHK
     I              (PO4,NO3,TAM,PHFG,DECFG,CO2,CVBPC,CVBPN,NSFG,
     M               GROBAL)
      END IF
C
C     calculate benthic algae death
      CALL BALDTH
     I            (NSFG,NO3,TAM,PO4,PALDH,NALDH,ALDL,
     I             ALDH,MBAL,DOX,ANAER,OXALD,BAL,DEPCOR,
     O             DTHBAL)
C
C     determine the new benthic algae population
      BAL= BAL + GROBAL
C
C     adjust net growth rate, if necessary, so that population
C     does not fall below minimum level
      MINBAL= .0001*DEPCOR
      IF (BAL .LT. MINBAL) THEN
        GROBAL= GROBAL - (MINBAL - BAL)
        BAL   = MINBAL
      END IF
      BAL= BAL - DTHBAL
C
C     adjust death rate, if necessary, so that population does not
C     drop below minimum level
      IF (BAL .LT. MINBAL) THEN
        DTHBAL= DTHBAL - (MINBAL - BAL)
        BAL   = MINBAL
      END IF
C
C     update do state variable to account for net effect of benthic
C     algae photosynthesis and respiration
      DOBALG= CVPB*CVBO*GROBAL
C     dox   = dox + (cvpb*cvbo*grobal)
      IF (DOX .GT. -DOBALG) THEN
        DOX= DOX + DOBALG
      ELSE
        DOBALG= -DOX
        DOX   = 0.0
      END IF
C
C     calculate amount of refractory organic constituents which result
C     from benthic algae death
      BALORN= REFR*DTHBAL*CVBPN*.014
      BALORP= REFR*DTHBAL*.031
      BALORC= REFR*DTHBAL*CVBPC*.012
C
C     calculate amount of nonrefractory organics (bod) which result
C     from benthic algae death
      BODBAL= CVNRBO*CVPB*DTHBAL
C
C     perform materials balance resulting from benthic algae death
      CALL ORGBAL
     I            (BALORN,BALORP,BALORC,BODBAL,
     M             ORN,ORP,ORC,BOD)
C
C     perform materials balance resulting from uptake of nutrients
C     by benthic algae
      CALL NUTRUP
     I            (GROBAL,NSFG,CVBPN,ALNPR,CVBPC,PHFG,DECFG,
     M             PO4,TAM,NO3,
     O             BALCO2,TAMBAL,NO3BAL,PO4BAL)
C
C     convert bal back to external units; benal is expressed as
C     mg biomass/m2 and balcla is expressed as ug chlorophyll a/m2
      BENAL = (BAL*CVPB)/DEPCOR
      BALCLA= BENAL*CVBCL
C
      RETURN
      END
C
C
C
      SUBROUTINE   GROCHK
     I                    (PO4,NO3,TAM,PHFG,DECFG,CO2,CVBPC,CVBPN,NSFG,
     M                     GROW)
C
C     + + + PURPOSE + + +
C     Check whether computed growth rate demands too much of any
C     nutrient; adjust growth rate, if necessary, so that at least
C     .001 mg/l of each nutrient remains after growth
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    PHFG,DECFG,NSFG
      REAL       PO4,NO3,TAM,CO2,CVBPC,CVBPN,GROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PO4    - ???
C     NO3    - dissolved nitrate concentration in mg/l
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     PHFG   - ???
C     DECFG  - ???
C     CO2    - ???
C     CVBPC  - ???
C     CVBPN  - ???
C     NSFG   - ???
C     GROW   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       UPLIMP,UPLIMN,UPLIMC,UPLIM
C
C     + + + INTRINSICS + + +
      INTRINSIC  AMIN1
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate growth rate which results in .001 mg/l of free po4
C     remaining after growth; uplimp is expressed as umoles
C     phosphorus per liter per interval
      UPLIMP= (PO4 - .001)*32.29
C
C     calculate growth rate which results in .001 mg/l of free
C     inorganic nitrogen remaining after growth; uplimn is expressed
C     as umoles phosphorus per interval
      IF (NSFG .EQ. 0) THEN
C       tam is not considered as a possible nutrient
        UPLIMN= (NO3 - .001)*71.43/CVBPN
      ELSE
        UPLIMN= (NO3 + TAM - .001)*71.43/CVBPN
      END IF
C
      UPLIMC= 1.0E30
      IF (PHFG .NE. 0 .AND. PHFG .NE. 2 .AND. DECFG .EQ. 0) THEN
C       phcarb is on, and co2 is being considered as a possible
C       limiting nutrient to algal growth
        IF (CO2 .GE. 0.0) THEN
C         calculate growth rate which results in .001 mg/l of free
C         carbon dioxide remaining after growth; uplimc is expressed
C         as umoles phosphorus per liter per interval
          UPLIMC= (CO2 - .001)*83.33/CVBPC
        END IF
      END IF
C
C     check that calculated growth does not result in less than
C     .001 mg/l of orthophosphate, inorganic nitrogen, or carbon
C     dioxide; if it does, adjust growth
      UPLIM= AMIN1(UPLIMP,UPLIMN,UPLIMC)
      IF (UPLIM .LT. GROW) THEN
C       reduce growth rate to limit
        GROW= UPLIM
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   LITRCH
     I                    (INLIT,EXTB,EXTCLA,EXTSED,AVDEPE,PHYFG,BALFG,
     O                     PHYLIT,BALLIT,CFLIT)
C
C     + + + PURPOSE + + +
C     Calculate light correction factor to algal growth (cflit);
C     determine amount of light available to phytoplankton and benthic
C     algae
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    PHYFG,BALFG
      REAL       INLIT,EXTB,EXTCLA,EXTSED,AVDEPE,PHYLIT,BALLIT,CFLIT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INLIT  - ???
C     EXTB   - ???
C     EXTCLA - ???
C     EXTSED - ???
C     AVDEPE - ???
C     PHYFG  - ???
C     BALFG  - ???
C     PHYLIT - ???
C     BALLIT - ???
C     CFLIT  - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       EXTCO,EUDEP
C
C
C     + + + INTRINSICS + + +
      INTRINSIC  EXP,AMIN1
C
C     + + + END SPECIFICATIONS + + +
C
      IF (INLIT .GT. 0.0) THEN
C       calculate extinction of light based on the base extinction
C       coefficient of the water incremented by self-shading effects
C       of phytoplankton and light extinction due to total sediment
C       suspension
        EXTCO= EXTB + EXTCLA + EXTSED
C
C       calculate euphotic depth; euphotic depth is the distance,
C       in feet, below the surface of the water body at which one
C       percent of incident light is present
        EUDEP= 4.60517/EXTCO
C
        IF (EUDEP .LT. AVDEPE) THEN
C         calculate fraction of layer which is contained in the
C         euphotic zone; this fraction, cflit, will be multiplied
C         by calculated growth in algro to assure that growth only
C         occurs in the euphotic zone
          CFLIT= EUDEP/AVDEPE
          IF (CFLIT .LT. .0001) THEN
            CFLIT= 0.0
          END IF
        ELSE
          CFLIT= 1.0
        END IF
C
        IF (PHYFG .NE. 0) THEN
C         calculate amount of light available to phytoplankton; all
C         phytoplankton are assumed to be at mid-depth of the reach;
C         light is expressed as langleys per interval
          PHYLIT= INLIT*EXP(-EXTCO*(.5*AMIN1(EUDEP,AVDEPE)))
          IF (PHYLIT .LT. .0001) THEN
            PHYLIT= 0.0
          END IF
        END IF
C
        IF (BALFG .NE. 0) THEN
C         calculate amount of light available to benthic algae; all
C         benthic algae are assumed to be at the bottom depth of the
C         reach
          BALLIT= INLIT*EXP(-EXTCO*AVDEPE)
          IF (BALLIT .LT. .0001) THEN
            BALLIT=0.0
          END IF
        END IF
      ELSE
C       there is no incident solar radiation; algal growth cannot
C       occur
        CFLIT = 0.0
        PHYLIT= 0.0
        BALLIT= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NUTRUP
     I                    (GROW,NSFG,CVBPN,ALNPR,CVBPC,PHFG,DECFG,
     M                     PO4,TAM,NO3,
     O                     ALCO2,TAMALG,NO3ALG,PO4ALG)
C
C     + + + PURPOSE + + +
C     Perform materials balance for transformation from inorganic to
C     organic material; uptake of PO4, NO3, TAM, and CO2 are considered
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NSFG,PHFG,DECFG
      REAL       GROW,CVBPN,ALNPR,CVBPC,PO4,TAM,NO3,ALCO2,
     $           TAMALG,NO3ALG,PO4ALG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GROW   - ???
C     NSFG   - ???
C     CVBPN  - ???
C     ALNPR  - ???
C     CVBPC  - ???
C     PHFG   - ???
C     DECFG  - ???
C     PO4    - ???
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     NO3    - dissolved nitrate concentration in mg/l
C     ALCO2  - ???
C     TAMALG - ???
C     NO3ALG - ???
C     PO4ALG - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       GROWN,ALTAM,ALNO3,NO3LIM,TAMLIM,TAMS,NO3S
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate po4 balance subsequent to algal uptake or release;
C     .031 is the conversion from umoles p per liter to mg of p per
C     liter
      PO4   = PO4 - .031*GROW
      PO4ALG= -.031*GROW
C
      TAMALG= 0.0
      IF (NSFG .NE. 0) THEN
C       calculate tam balance subsequent to algal uptake or release
C       express calculated growth rate in terms of equivalent
C       nitrogen; grown is expressed as umoles nitrogen per interval
        GROWN= GROW*CVBPN
C
        IF (GROW .LT. 0.0) THEN
C         algal respiration exceeds growth; nitrogen released by
C         respiration is released in the form of tam; no uptake or
C         release of no3 occurs
          ALTAM= GROWN
          ALNO3= 0.0
        ELSE
C         calculate amount of n uptake which is no3 and amount which
C         is tam
          ALNO3= ALNPR*GROWN
          ALTAM= GROWN - ALNO3
C         check that computed uptake of no3 does not consume more
C         than 99 percent of available free no3; if it does, satisfy
C         excess demand with free tam; no3lim is expressed as umoles
C         n per liter per interval
          NO3LIM= 70.72*NO3
C
          IF (ALNO3 .GT. NO3LIM) THEN
            ALTAM= ALTAM + ALNO3 - NO3LIM
            ALNO3= NO3LIM
          ELSE
C           check that calculated uptake of tam does not consume
C           more than 99 percent of available free tam; if it does,
C           satisfy excess demand with free no3; tamlim is expressed
C           as umoles n per liter per interval
            TAMLIM= 70.72*TAM
C
            IF (ALTAM .GT. TAMLIM) THEN
              ALNO3= ALNO3 + ALTAM - TAMLIM
              ALTAM= TAMLIM
            ELSE
C             calculated uptake of inorganic nitrogen is acceptable
            END IF
          END IF
        END IF
C
C       calculate net uptake or release of tam by algae; .014 is
C       the conversion from umoles of n per liter per interval to
C       mg n per liter per interval
        TAMS  = TAM
        TAMALG= -0.014*ALTAM
        TAM   = TAM - .014*ALTAM
        IF (TAM .LT. .001) THEN
          TAMALG= -TAMS
        END IF
        IF (TAM .LT. .001) THEN
          TAM= 0.0
        END IF
      ELSE
C       all inorganic n is in the form of no3
        ALNO3= GROW*CVBPN
      END IF
C
C     calculate no3 balance subsequent to algal uptake or release;
C     eliminate insignificant values of no3
      NO3S  = NO3
      NO3ALG= -.014*ALNO3
      NO3   = NO3 - .014*ALNO3
      IF (NO3 .LT. .001) THEN
        NO3ALG= -NO3S
      END IF
C
      IF (NO3 .LT. .001) THEN
        NO3= 0.0
      END IF
C
      IF (PHFG .NE. 0 .AND. DECFG .EQ. 0) THEN
C       calculate amount of algal uptake of co2; alco2 is expressed
C       as mg co2-c/liter
        ALCO2= GROW*CVBPC*.012
      ELSE
        ALCO2= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ORGBAL
     I                    (DTHORN,DTHORP,DTHORC,DTHBOD,
     M                     ORN,ORP,ORC,BOD)
C
C     + + + PURPOSE + + +
C     Perform materials balance for transformation from living to
C     dead organic material
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       DTHORN,DTHORP,DTHORC,DTHBOD,ORN,ORP,ORC,BOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DTHORN - ???
C     DTHORP - ???
C     DTHORC - ???
C     DTHBOD - ???
C     ORN    - ???
C     ORP    - ???
C     ORC    - ???
C     BOD    - ???
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate dead refractory organic nitrogen balance
C     subsequent to plankton death; plankton death may be
C     either algal death, zooplankton death, or phytoplankton
C     filtered by zooplankton but not assimilated
      ORN= ORN + DTHORN
C
C     calculate dead refractory organic phosphorus balance
C     subsequent to plankton death
      ORP= ORP + DTHORP
C
C     calculate dead refractory organic carbon balance
C     subsequent to plankton death
      ORC= ORC + DTHORC
C
C     calculate bod balance subsequent to plankton death
      BOD= BOD + DTHBOD
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHYDTH
     I                    (NSFG,NO3,TAM,PO4,PALDH,NALDH,PHYCLA,CLALDH,
     I                     ALDL,ALDH,DOX,ANAER,OXALD,STC,
     O                     DTHPHY)
C
C     + + + PURPOSE + + +
C     Calculate phytoplankton death
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NSFG
      REAL       NO3,TAM,PO4,PALDH,NALDH,CLALDH,ALDL,ALDH,
     $           DOX,ANAER,OXALD,STC,DTHPHY,PHYCLA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSFG   - ???
C     NO3    - dissolved nitrate concentration in mg/l
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     PO4    - ???
C     PALDH  - ???
C     NALDH  - ???
C     PHYCLA - ???
C     CLALDH - ???
C     ALDL   - ???
C     ALDH   - ???
C     DOX    - dissolved oxygen concentration in mg/l
C     ANAER  - ???
C     OXALD  - ???
C     STC    - ???
C     DTHPHY - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       NIT,ALD
C
C     + + + END SPECIFICATIONS + + +
C
C     determine whether to use high or low unit death rate; all unit
C     death rates are expressed in units of per interval
C     determine available inorganic nitrogen pool for test of nutrient
C     scarcity
      IF (NSFG .NE. 0) THEN
        NIT= NO3 + TAM
      ELSE
        NIT= NO3
      END IF
C
      IF (PO4 .GT. PALDH .AND. NIT .GT. NALDH) THEN
C       unit death rate is not incremented by nutrient scarcity
C       check for phytoplankton overcrowding
C
        IF (PHYCLA .LT. CLALDH) THEN
C         unit death rate is not incremented by phytoplankton
C         overcrowding
          ALD= ALDL
        ELSE
C         augment unit death rate to account for overcrowding
          ALD= ALDH
        END IF
C
      ELSE
C       augment unit death rate to account for nutrient scarcity
        ALD= ALDH
C
      END IF
C
C     augment unit death rate if conditions are anaerobic
      IF (DOX .LT. ANAER) THEN
        ALD= ALD + OXALD
      END IF
C
C     use unit death rate to compute death rate; aldth is expressed
C     as umoles of phosphorus per liter per interval
      DTHPHY= ALD*STC
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHYRX
     I                   (PHYLIT,TW,TALGRL,TALGRH,TALGRM,MALGR,CMMP,
     I                    CMMNP,TAMFG,AMRFG,NSFG,CMMN,CMMLT,DELT60,
     I                    CFLIT,ALR20,CVBPN,PHFG,DECFG,CVBPC,PALDH,
     I                    NALDH,CLALDH,ALDL,ALDH,ANAER,OXALD,ALNPR,
     I                    CVBO,REFR,CVNRBO,CVPB,CVBCL,LIMIT,CO2,
     M                    PO4,NO3,TAM,DOX,ORN,ORP,ORC,BOD,PHYTO,
     O                    LIMPHY,PHYCO2,PHYCLA,DOPHY,BODPHY,TAMPHY,
     O                    NO3PHY,PO4PHY)
C
C     + + + PURPOSE + + +
C     Simulate behavior of phytoplankton, as standing crop, in units
C     of umoles P per liter
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     AMRFG,DECFG,TAMFG,NSFG,PHFG
      REAL        ALDH,ALDL,ALNPR,ALR20,ANAER,BOD,CFLIT,
     $            CLALDH,CMMLT,CMMN,CMMNP,CMMP,CO2,CVBCL,
     $            CVBO,CVBPC,CVBPN,CVNRBO,CVPB,DELT60,
     $            DOX,MALGR,NALDH,TAM,NO3,
     $            ORC,ORN,ORP,OXALD,PALDH,PHYCLA,PHYCO2,
     $            PHYLIT,PHYTO,PO4,REFR,TALGRH,TALGRL,
     $            TALGRM,TW,DOPHY,BODPHY,TAMPHY,NO3PHY,
     $            PO4PHY,LIMPHY
      CHARACTER*4 LIMIT(7)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PHYLIT - ???
C     TW     - water temperature in degrees C
C     TALGRL - ???
C     TALGRH - ???
C     TALGRM - ???
C     MALGR  - ???
C     CMMP   - ???
C     CMMNP  - ???
C     TAMFG  - ???
C     AMRFG  - ???
C     NSFG   - ???
C     CMMN   - ???
C     CMMLT  - ???
C     DELT60 - simulation time interval in hours
C     CFLIT  - ???
C     ALR20  - ???
C     CVBPN  - ???
C     PHFG   - ???
C     DECFG  - ???
C     CVBPC  - ???
C     PALDH  - ???
C     NALDH  - ???
C     CLALDH - ???
C     ALDL   - ???
C     ALDH   - ???
C     ANAER  - ???
C     OXALD  - ???
C     ALNPR  - ???
C     CVBO   - ???
C     REFR   - ???
C     CVNRBO - ???
C     CVPB   - ???
C     CVBCL  - ???
C     LIMIT  - ???
C     CO2    - ???
C     PO4    - ???
C     NO3    - dissolved nitrate concentration in mg/l
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     DOX    - dissolved oxygen concentration in mg/l
C     ORN    - ???
C     ORP    - ???
C     ORC    - ???
C     BOD    - ???
C     PHYTO  - ???
C     LIMPHY - ???
C     PHYCO2 - ???
C     PHYCLA - ???
C     DOPHY  - ???
C     BODPHY - ???
C     TAMPHY - ???
C     NO3PHY - ???
C     PO4PHY - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       DTHPHY,GRO,GROPHY,PHYBD,PHYORC,PHYORN,
     $           PHYORP,RES,STC
C
C     + + + EXTERNALS + + +
      EXTERNAL   ALGRO,GROCHK,PHYDTH,ORGBAL,NUTRUP
C
C     + + + END SPECIFICATIONS + + +
C
C     convert phyto to units of umoles phosphorus (stc) and
C     ug chlorophyll a/l (phycla) for internal calculations
      STC   = PHYTO/CVPB
      PHYCLA= PHYTO*CVBCL
C
C     compute unit growth and respiration rates for phytoplankton;
C     determine growth limiting factor
      CALL ALGRO
     I           (PHYLIT,PO4,NO3,TW,TALGRL,TALGRH,TALGRM,MALGR,CMMP,
     I            CMMNP,TAMFG,AMRFG,TAM,NSFG,CMMN,CMMLT,ALR20,
     I            CFLIT,DELT60,LIMIT,
     O            LIMPHY,GRO,RES)
C
C     calculate net growth rate of phytoplankton; grophy is
C     expressed as umol phosphorus per liter per interval
      GROPHY= (GRO - RES)*STC
C
      IF (GROPHY .GT. 0.0) THEN
C       adjust growth rate to account for limitations imposed by
C       availability of required nutrients
        CALL GROCHK
     I              (PO4,NO3,TAM,PHFG,DECFG,CO2,CVBPC,CVBPN,NSFG,
     M               GROPHY)
C
      END IF
C
C     calculate phytoplankton death
      CALL PHYDTH
     I            (NSFG,NO3,TAM,PO4,PALDH,NALDH,PHYCLA,CLALDH,
     I             ALDL,ALDH,DOX,ANAER,OXALD,STC,
     O             DTHPHY)
C
C     determine the new phytoplankton population
      STC= STC + GROPHY
C
C     adjust net growth rate, if necessary, so population does not
C     fall below minimum level
      IF (STC .LT. .0025) THEN
        GROPHY= GROPHY - (.0025 - STC)
        STC   = .0025
      END IF
      STC= STC - DTHPHY
C
C     adjust death rate, if necessary, so that population does
C     not drop below minimum level
      IF (STC .LT. .0025) THEN
        DTHPHY= DTHPHY - (.0025 - STC)
        STC   = .0025
      END IF
C
C     update do state variable to account for net effect of
C     phytoplankton photosynthesis and respiration
      DOPHY= (CVPB*CVBO*GROPHY)
C     dox  = dox + (cvpb*cvbo*grophy)
      IF (DOX .GT. -DOPHY) THEN
        DOX= DOX + DOPHY
      ELSE
        DOPHY= -DOX
        DOX  = 0.0
      END IF
C
C     calculate amount of refractory organic constituents which
C     result from phytoplankton death
      PHYORN= REFR*DTHPHY*CVBPN*.014
      PHYORP= REFR*DTHPHY*.031
      PHYORC= REFR*DTHPHY*CVBPC*.012
C
C     calculate amount of nonrefractory organics (bod) which result
C     from phytoplankton death
      PHYBD = CVNRBO*CVPB*DTHPHY
      BODPHY= PHYBD
C
C     perform materials balance resulting from phytoplankton death
      CALL ORGBAL
     I            (PHYORN,PHYORP,PHYORC,PHYBD,
     M             ORN,ORP,ORC,BOD)
C
C     perform materials balance resulting from uptake of nutrients
C     by phytoplankton
      CALL NUTRUP
     I            (GROPHY,NSFG,CVBPN,ALNPR,CVBPC,PHFG,DECFG,
     M             PO4,TAM,NO3,
     O             PHYCO2,TAMPHY,NO3PHY,PO4PHY)
C
C     convert stc to units of mg biomass/l (phyto) and
C     ug chlorophyll a/l (phycla)
      PHYTO = STC*CVPB
      PHYCLA= PHYTO*CVBCL
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLKACC
     I                    (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes in subroutine group plank for printout
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLANK2 + + +
      INCLUDE    'crhpl.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL  ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      IF (PHYFG .EQ. 1) THEN
C       handle flux groups dealing with reach-wide variables
        PKIF(1,TOROW) = PKIF(1,TOROW) + PKIF(1,FRMROW)
        PKCF1(1,TOROW)= PKCF1(1,TOROW) + PKCF1(1,FRMROW)
C
        IF (NEXITS .GT. 1) THEN
C         handle flux groups dealing with individual exit gates
          CALL ACCVEC
     I                (NEXITS,PKCF2(1,1,FRMROW),
     M                 PKCF2(1,1,TOROW))
        END IF
C
        IF (ZOOFG .EQ. 1) THEN
C         handle flux groups dealing with reach-wide variables
          PKIF(2,TOROW) = PKIF(2,TOROW) + PKIF(2,FRMROW)
          PKCF1(2,TOROW)= PKCF1(2,TOROW) + PKCF1(2,FRMROW)
C
          IF (NEXITS .GT. 1) THEN
C           handle flux groups dealing with individual exit gates
            CALL ACCVEC
     I                  (NEXITS,PKCF2(1,2,FRMROW),
     M                   PKCF2(1,2,TOROW))
          END IF
        END IF
      END IF
C
C     accumulate fluxes of orn, orp, and toc
      PKIF(3,TOROW) = PKIF(3,TOROW) + PKIF(3,FRMROW)
      PKIF(4,TOROW) = PKIF(4,TOROW) + PKIF(4,FRMROW)
      PKIF(5,TOROW) = PKIF(5,TOROW) + PKIF(5,FRMROW)
      PKCF1(3,TOROW)= PKCF1(3,TOROW) + PKCF1(3,FRMROW)
      PKCF1(4,TOROW)= PKCF1(4,TOROW) + PKCF1(4,FRMROW)
      PKCF1(5,TOROW)= PKCF1(5,TOROW) + PKCF1(5,FRMROW)
      PKCF3(1,TOROW)= PKCF3(1,TOROW) + PKCF3(1,FRMROW)
      PKCF3(2,TOROW)= PKCF3(2,TOROW) + PKCF3(2,FRMROW)
      PKCF3(3,TOROW)= PKCF3(3,TOROW) + PKCF3(3,FRMROW)
      PKCF4(1,TOROW)= PKCF4(1,TOROW) + PKCF4(1,FRMROW)
      PKCF4(2,TOROW)= PKCF4(2,TOROW) + PKCF4(2,FRMROW)
      PKCF4(3,TOROW)= PKCF4(3,TOROW) + PKCF4(3,FRMROW)
C
      IF (NEXITS .GT. 1) THEN
C       handle flux groups dealing with individual exit gates
        CALL ACCVEC
     I              (NEXITS,PKCF2(1,3,FRMROW),
     M               PKCF2(1,3,TOROW))
C
        CALL ACCVEC
     I              (NEXITS,PKCF2(1,4,FRMROW),
     M               PKCF2(1,4,TOROW))
C
        CALL ACCVEC
     I              (NEXITS,PKCF2(1,5,FRMROW),
     M               PKCF2(1,5,TOROW))
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLKPRT
     I                    (LEV,PRINTU,FACTA,FACTB,FLUXID)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units, and print
C     Out results
C     Note: local arrays have same dimensions as corresponding arrays
C       in osv, except for dropping of dimension lev, where applicable
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
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLANK2 + + +
      INCLUDE    'crhpl.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,PADFG
      REAL       PIFLX(5),PCFLX1(5),PCFLX2(5,5),PZOO,PCFLX3(3),
     #           PCFLX4(3),PADTOT(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** PLANK ***')
 2010 FORMAT (/,'   STATE VARIABLES')
 2020 FORMAT (  '     PHYTOPLANKTON',18X,'PHYTO    PHYCLA    LIMPHY',
     #           7X,'ZOO')
 2030 FORMAT (  7X,'& ZOOPLANKTON',17X,'MG/L      UG/L',15X,'ORG/L')
 2040 FORMAT (  31X,2(1PE10.3),6X,A4,1PE10.3)
 2050 FORMAT (  '     PHYTOPLANKTON',18X,'PHYTO    PHYCLA    LIMPHY')
 2060 FORMAT (  37X,'MG/L      UG/L')
 2070 FORMAT (/,'     BENTHIC ALGAE',18X,'BENAL    BALCLA    LIMBAL')
 2080 FORMAT (  36X,'MG/M2     UG/M2')
 2090 FORMAT (/,'     ORGANIC CONSTITUENTS',13X,'ORN       ORP',
     #        7X,'ORC      TORN      TORP      TORC    POTBOD')
 2100 FORMAT (  31X,7(6X,'MG/L'))
 2110 FORMAT (  31X,7(1PE10.3))
 2120 FORMAT (/,'   FLUXES',27X,'TOTAL     TOTAL',
     #          '    INDIVIDUAL GATE OUTFLOWS')
 2130 FORMAT (  35X,'INFLOW   OUTFLOW',5I10)
 2140 FORMAT (  '     PHYTOPLANKTON (',A3,')',11X,'IPHYTO    ROPHYT',
     #          15X,'OPHYT')
 2150 FORMAT (/,'     ZOOPLANKTON (',A3,')',15X,'IZOO     ROZOO',
     #          16X,'OZOO')
 2160 FORMAT (/,'     ORGANIC NITROGEN (',A3,')',10X,'IORN     ROORN',
     #        16X,'OORN')
 2170 FORMAT (  31X,7(1PE10.3))
 2180 FORMAT (/,'     ORGANIC PHOSPHORUS (',A3,')',8X,'IORP     ROORP',
     #        16X,'OORP')
 2190 FORMAT (/,'     TOTAL ORGANIC CARBON (',A3,')',6X,
     #          'IORC     ROORC',16X,'OORC')
 2200 FORMAT (/,' ','  FLUXES',22X,'<---ATMOSPHERIC DEPOSITION--->',
     #          '     OTHER     TOTAL    INDIVIDUAL GATE OUTFLOWS')
 2210 FORMAT (  ' ',37X,'DRY       WET     TOTAL    INFLOW   OUTFLOW',
     #          5I10)
 2220 FORMAT (  '     PHYTOPLANKTON (',A3,')',41X,'IPHYTO    ROPHYT',
     #          15X,'OPHYT')
 2230 FORMAT (  61X,10(1PE10.3))
 2240 FORMAT (/,'     ZOOPLANKTON (',A3,')',45X,'IZOO     ROZOO',
     #          16X,'OZOO')
 2250 FORMAT (/,' ','    ORGANIC NITROGEN (',A3,')',5X,'PLADDR(1)',
     #          ' PLADWT(1)   ATM DEP      IORN     ROORN',16X,'OORN')
 2260 FORMAT (/,'  ','   ORGANIC PHOSPHORUS (',A3,')',3X,'PLADDR(2)',
     #          ' PLADWT(2)   ATM DEP      IORP     ROORP',16X,'OORP')
 2270 FORMAT (/,' ','    TOTAL ORGANIC CARBON (',A3,')',1X,'PLADDR(3)',
     #          ' PLADWT(3)   ATM DEP      IORC     ROORC',16X,'OORC')
 2280 FORMAT (/,'   FLUXES',27X,'TOTAL     TOTAL')
 2290 FORMAT (  '     PHYTOPLANKTON (',A3,')',11X,'IPHYTO    ROPHYT')
 2300 FORMAT (/,'     ZOOPLANKTON (',A3,')',15X,'IZOO     ROZOO')
 2310 FORMAT (/,'     ORGANIC NITROGEN (',A3,')',10X,'IORN     ROORN')
 2320 FORMAT (/,'     ORGANIC PHOSPHORUS (',A3,')',8X,'IORP     ROORP')
 2330 FORMAT (/,'     TOTAL ORGANIC CARBON (',A3,')',6X,
     $          'IORC     ROORC')
 2340 FORMAT (/,' ','  FLUXES',22X,'<---ATMOSPHERIC DEPOSITION--->',
     #          '     OTHER     TOTAL')
 2350 FORMAT (  '     PHYTOPLANKTON (',A3,')',41X,'IPHYTO    ROPHYT')
 2360 FORMAT (/,'     ZOOPLANKTON (',A3,')',45X,'IZOO     ROZOO')
 2370 FORMAT (/,' ','    ORGANIC NITROGEN (',A3,')',5X,'PLADDR(1)',
     #          ' PLADWT(1)   ATM DEP      IORN     ROORN')
 2380 FORMAT (/,'  ','   ORGANIC PHOSPHORUS (',A3,')',3X,'PLADDR(2)',
     #          ' PLADWT(2)   ATM DEP      IORP     ROORP')
 2390 FORMAT (/,' ','    TOTAL ORGANIC CARBON (',A3,')',1X,'PLADDR(3)',
     #          ' PLADWT(3)   ATM DEP      IORC     ROORC')
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1, 3
        PADTOT(I)= 0.0
 10   CONTINUE
C
      PADFG= 0
      DO 20 I= 1, 3
        J= (I-1)*2+ 1
        IF ( (PLADFG(J) .NE. 0) .OR. (PLADFG(J+1) .NE. 0) ) THEN
          PADFG= 1
        END IF
 20   CONTINUE
C
C     convert variables to external units
      IF (PHYFG .EQ. 1) THEN
C       phytoplankton inflow flux
        PIFLX(1) = PKIF(1,LEV)*FACTA
C
C       phytoplankton computed fluxes
        PCFLX1(1)= PKCF1(1,LEV)*FACTA
C
        IF (NEXITS .GT. 1) THEN
          CALL TRNVEC
     I                (NEXITS,PKCF2(1,1,LEV),FACTA,FACTB,
     O                 PCFLX2(1,1))
        END IF
C
        IF (ZOOFG .EQ. 1) THEN
C         zooplankton concentration
          PZOO     = ZOO/ZOMASS
C
C         zooplankton inflow flux
          PIFLX(2) = PKIF(2,LEV)*FACTA
C
C         zooplankton computed fluxes
          PCFLX1(2)= PKCF1(2,LEV)*FACTA
C
          IF (NEXITS .GT. 1) THEN
            CALL TRNVEC
     I                  (NEXITS,PKCF2(1,2,LEV),FACTA,FACTB,
     O                   PCFLX2(1,2))
          END IF
        END IF
      END IF
C
C     fluxes for organics
      DO 30 I= 3,5
C       inflow fluxes
        PIFLX(I) = PKIF(I,LEV)*FACTA
C
C       computed fluxes for organics
        PCFLX1(I)= PKCF1(I,LEV)*FACTA
        IF (PADFG .EQ. 1) THEN
          PCFLX3(I-2)= PKCF3(I-2,LEV)*FACTA
          PCFLX4(I-2)= PKCF4(I-2,LEV)*FACTA
          PADTOT(I-2)= PCFLX3(I-2)+ PCFLX4(I-2)
        END IF
C
        IF (NEXITS .GT. 1) THEN
          CALL TRNVEC
     I                (NEXITS,PKCF2(1,I,LEV),FACTA,FACTB,
     O                 PCFLX2(1,I))
        END IF
C
   30 CONTINUE
C
C     do printout on unit printu
      WRITE (PRINTU,2000)
C
      WRITE (PRINTU,2010)
C
      IF (PHYFG .EQ. 1) THEN
        IF (ZOOFG .EQ. 1) THEN
          WRITE (PRINTU,2020)
          WRITE (PRINTU,2030)
          WRITE (PRINTU,2040)  PKST1, PZOO
        ELSE
          WRITE (PRINTU,2050)
          WRITE (PRINTU,2060)
          WRITE (PRINTU,2040)  PKST1
        END IF
      END IF
C
      IF (BALFG .EQ. 1) THEN
        WRITE (PRINTU,2070)
        WRITE (PRINTU,2080)
        WRITE (PRINTU,2040)  PKST2
      END IF
C
      WRITE (PRINTU,2090)
      WRITE (PRINTU,2100)
      WRITE (PRINTU,2110)  PKST3
C
C     fluxes
C
      IF (NEXITS .GT. 1) THEN
        IF (PADFG .EQ. 0) THEN
          WRITE (PRINTU,2120)
          WRITE (PRINTU,2130)  (J,J=1,NEXITS)
C
          IF (PHYFG .EQ. 1) THEN
            WRITE (PRINTU,2140)  FLUXID
            WRITE (PRINTU,2170)  PIFLX(1), PCFLX1(1),
     #                          (PCFLX2(J,1),J=1,NEXITS)
C
            IF (ZOOFG .EQ. 1) THEN
              WRITE (PRINTU,2150)  FLUXID
              WRITE (PRINTU,2170)  PIFLX(2), PCFLX1(2),
     #                            (PCFLX2(J,2),J=1,NEXITS)
            END IF
          END IF
C
          WRITE (PRINTU,2160)  FLUXID
          WRITE (PRINTU,2170)  PIFLX(3), PCFLX1(3),
     #                        (PCFLX2(J,3),J=1,NEXITS)
C
          WRITE (PRINTU,2180)  FLUXID
          WRITE (PRINTU,2170)  PIFLX(4), PCFLX1(4),
     #                        (PCFLX2(J,4),J=1,NEXITS)
C
          WRITE (PRINTU,2190)  FLUXID
          WRITE (PRINTU,2170)  PIFLX(5), PCFLX1(5),
     #                        (PCFLX2(J,5),J=1,NEXITS)
C
        ELSE
          WRITE (PRINTU,2200)
          WRITE (PRINTU,2210)  (J,J=1,NEXITS)
C
          IF (PHYFG .EQ. 1) THEN
            WRITE (PRINTU,2220)  FLUXID
            WRITE (PRINTU,2230)  PIFLX(1), PCFLX1(1),
     #                          (PCFLX2(J,1),J=1,NEXITS)
C
            IF (ZOOFG .EQ. 1) THEN
              WRITE (PRINTU,2240)  FLUXID
              WRITE (PRINTU,2230)  PIFLX(2), PCFLX1(2),
     #                            (PCFLX2(J,2),J=1,NEXITS)
            END IF
          END IF
C
          WRITE (PRINTU,2250)  FLUXID
          WRITE (PRINTU,2170)  PCFLX3(1), PCFLX4(1), PADTOT(1),
     #                         PIFLX(3), PCFLX1(3),
     #                        (PCFLX2(J,3),J=1,NEXITS)
C
          WRITE (PRINTU,2260)  FLUXID
          WRITE (PRINTU,2170)  PCFLX3(2), PCFLX4(2), PADTOT(2),PIFLX(4),
     #      PCFLX1(4), (PCFLX2(J,4),J=1,NEXITS)
C
          WRITE (PRINTU,2270)  FLUXID
          WRITE (PRINTU,2170)  PCFLX3(3), PCFLX4(3), PADTOT(3),PIFLX(5),
     #      PCFLX1(5), (PCFLX2(J,5),J=1,NEXITS)
C
        END IF
      ELSE
C       single exit
        IF (PADFG .EQ. 0) THEN
          WRITE (PRINTU,2280)
          WRITE (PRINTU,2130)
C
          IF (PHYFG .EQ. 1) THEN
            WRITE (PRINTU,2290)  FLUXID
            WRITE (PRINTU,2170)  PIFLX(1), PCFLX1(1)
C
            IF (ZOOFG .EQ. 1) THEN
              WRITE (PRINTU,2300)  FLUXID
              WRITE (PRINTU,2170)  PIFLX(2), PCFLX1(2)
            END IF
          END IF
C
          WRITE (PRINTU,2310)  FLUXID
          WRITE (PRINTU,2170)  PIFLX(3), PCFLX1(3)
C
          WRITE (PRINTU,2320)  FLUXID
          WRITE (PRINTU,2170)  PIFLX(4), PCFLX1(4)
C
          WRITE (PRINTU,2330)  FLUXID
          WRITE (PRINTU,2170)  PIFLX(5), PCFLX1(5)
C
        ELSE
          WRITE (PRINTU,2340)
          WRITE (PRINTU,2130)
C
          IF (PHYFG .EQ. 1) THEN
            WRITE (PRINTU,2350)  FLUXID
            WRITE (PRINTU,2230)  PIFLX(1), PCFLX1(1)
C
            IF (ZOOFG .EQ. 1) THEN
              WRITE (PRINTU,2360)  FLUXID
              WRITE (PRINTU,2230)  PIFLX(2), PCFLX1(2)
            END IF
          END IF
C
          WRITE (PRINTU,2370)  FLUXID
          WRITE (PRINTU,2170)  PCFLX3(1), PCFLX4(1), PADTOT(1),
     #                         PIFLX(3), PCFLX1(3)
C
          WRITE (PRINTU,2380)  FLUXID
          WRITE (PRINTU,2170)  PCFLX3(2), PCFLX4(2), PADTOT(2),
     #                         PIFLX(4), PCFLX1(4)
C
          WRITE (PRINTU,2390)  FLUXID
          WRITE (PRINTU,2170)  PCFLX3(3), PCFLX4(3), PADTOT(3),
     #                         PIFLX(5), PCFLX1(5)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLKRB
C
C     + + + PURPOSE + + +
C     Handle subroutine group plank
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLANK2 + + +
      INCLUDE    'crhpl.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,5
        IF (PKCF1X(I) .GE. 1) THEN
          PAD(PKCF1X(I) + IVL1)= PKCF1(I,1)
        END IF
 10   CONTINUE
C
      IF (NEXITS .GT. 1) THEN
        DO 30 J= 1,5
          DO 20 I= 1,NEXITS
            IF (PKCF2X(I,J) .GE. 1) THEN
              PAD(PKCF2X(I,J) + IVL1)= PKCF2(I,J,1)
            END IF
 20       CONTINUE
 30     CONTINUE
      END IF
C
      DO 110 I= 1, 3
        IF (PLADDX(I) .GE. 1) THEN
          PAD(PLADDX(I) + IVL1)= PKCF3(I,1)
        END IF
        IF (PLADWX(I) .GE. 1) THEN
          PAD(PLADWX(I) + IVL1)= PKCF4(I,1)
        END IF
 110  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLKRP
C
C     + + + PURPOSE + + +
C     Handle subroutine group plank
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLANK2 + + +
      INCLUDE    'crhpl.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,7
        IF (PKST3X(I) .GE. 1) THEN
          PAD(PKST3X(I) + IVL1)= PKST3(I)
        END IF
 10   CONTINUE
C
      IF (PYFP .GE. 1) THEN
        PAD(PYFP + IVL1)  = PHYTO
      END IF
      IF (PYCLFP .GE. 1) THEN
        PAD(PYCLFP + IVL1)= PHYCLA
      END IF
      IF (BAFP .GE. 1) THEN
        PAD(BAFP + IVL1)  = BENAL
      END IF
      IF (BACLFP .GE. 1) THEN
        PAD(BACLFP + IVL1)= BALCLA
      END IF
      IF (ZFP .GE. 1) THEN
        PAD(ZFP + IVL1)   = ZOO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLKRST
     I                    (LEV)
C
C     + + + PURPOSE + + +
C     Reset flux and state variables for subroutine group plank
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLANK2 + + +
      INCLUDE    'crhpl.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
C     ???
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      IF (PHYFG .EQ. 1) THEN
C       handle flux groups dealing with reach-wide variables
        PKIF(1,LEV) = 0.0
        PKCF1(1,LEV)= 0.0
C
        IF (NEXITS .GT. 1) THEN
C         handle flux groups dealing with individual exit gates
          CALL SETVEC
     I                (NEXITS,0.0,
     O                 PKCF2(1,1,LEV))
        END IF
C
        IF (ZOOFG .EQ. 1) THEN
C         handle flux groups dealing with reach-wide variables
          PKIF(2,LEV) = 0.0
          PKCF1(2,LEV)= 0.0
C
          IF (NEXITS .GT. 1) THEN
C           handle flux groups dealing with individual exit gates
            CALL SETVEC
     I                  (NEXITS,0.0,
     O                   PKCF2(1,2,LEV))
          END IF
        END IF
      END IF
C
C     handle fluxes of orp, orn, and orc
      PKIF(3,LEV) = 0.0
      PKIF(4,LEV) = 0.0
      PKIF(5,LEV) = 0.0
      PKCF1(3,LEV)= 0.0
      PKCF1(4,LEV)= 0.0
      PKCF1(5,LEV)= 0.0
      PKCF3(1,LEV)= 0.0
      PKCF3(2,LEV)= 0.0
      PKCF3(3,LEV)= 0.0
      PKCF4(1,LEV)= 0.0
      PKCF4(2,LEV)= 0.0
      PKCF4(3,LEV)= 0.0
C
      IF (NEXITS .GT. 1) THEN
C       handle flux groups dealing with individual exit gates
        CALL SETVEC
     I              (NEXITS,0.0,
     O               PKCF2(1,3,LEV))
        CALL SETVEC
     I              (NEXITS,0.0,
     O               PKCF2(1,4,LEV))
        CALL SETVEC
     I              (NEXITS,0.0,
     O               PKCF2(1,5,LEV))
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZORX
     I                  (ZFIL20,TCZFIL,TW,PHYTO,MZOEAT,ZEXDEL,CVPB,
     I                   ZRES20,TCZRES,ANAER,ZOMASS,TAMFG,REFR,
     I                   ZFOOD,ZD,OXZD,CVBN,CVBP,CVBC,CVNRBO,CVBO,
     M                   DOX,BOD,ZOO,ORN,ORP,ORC,TAM,NO3,PO4,
     O                   ZEAT,ZCO2,DOZOO,ZBOD,ZNIT,ZPO4)
C
C     + + + PURPOSE + + +
C     Calculate zooplankton population balance
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    TAMFG,ZFOOD
      REAL       ZFIL20,TCZFIL,TW,PHYTO,MZOEAT,ZEXDEL,CVPB,ZRES20,
     #           TCZRES,ANAER,ZOMASS,REFR,ZD,OXZD,CVBN,CVBP,CVBC,
     #           CVNRBO,CVBO,DOX,BOD,ZOO,ORN,ORP,ORC,TAM,NO3,PO4,ZEAT,
     #           ZCO2,DOZOO,ZBOD,ZNIT,ZPO4
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ZFIL20 - ???
C     TCZFIL - ???
C     TW     - water temperature in degrees C
C     PHYTO  - ???
C     MZOEAT - ???
C     ZEXDEL - ???
C     CVPB   - ???
C     ZRES20 - ???
C     TCZRES - ???
C     ANAER  - ???
C     ZOMASS - ???
C     TAMFG  - ???
C     REFR   - ???
C     NONREF - ???
C     ZFOOD  - ???
C     ZD     - ???
C     OXZD   - ???
C     CVBN   - ???
C     CVBP   - ???
C     CVBC   - ???
C     CVNRBO - ???
C     CVBO   - ???
C     DOX    - dissolved oxygen concentration in mg/l
C     BOD    - ???
C     ZOO    - ???
C     ORN    - ???
C     ORP    - ???
C     ORC    - ???
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     NO3    - dissolved nitrate concentration in mg/l
C     PO4    - ???
C     ZEAT   - ???
C     ZCO2   - ???
C     DOZOO  - ???
C     ZBOD   - ???
C     ZNIT   - ???
C     ZPO4   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I1
      REAL       ZFIL,ZOEAT,ZEXDEC,ZEFF,ZOGR,ZEXMAS,ZREFEX,
     $           ZINGEX,ZNRFEX,ZRES,ZDTH,LOLIM,
     $           ZORN,ZORP,ZORC
C
C     + + + EXTERNALS + + +
      EXTERNAL   DECBAL,ORGBAL
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
C     calculate zooplankton unit grazing rate expressed as liters
C     of water filtered per mg zooplankton per interval
      ZFIL= ZFIL20*(TCZFIL**(TW - 20.))
C
C     calculate mass of phytoplankton biomass ingested per mg
C     zooplankton per interval
      ZOEAT= ZFIL*PHYTO
C
C     check that calculated unit ingestion rate does not exceed
C     maximum allowable unit ingestion rate (mzoeat); if it does,
C     set unit ingestion rate equal to mzoeat
C
      IF (ZOEAT .GE. MZOEAT) THEN
        ZOEAT= MZOEAT
C       nonrefractory portion of excretion is only partially
C       decomposed; zexdec is the fraction of nonrefractory
C       material which is decomposed
        ZEXDEC= ZEXDEL
      ELSE
C       calculated unit ingestion rate is acceptable
C       all nonrefractory excretion is decomposed
        ZEXDEC= 1.0
      END IF
C
C     calculate phytoplankton consumed by zooplankton; zeat is
C     expressed as mg biomass per interval
      ZEAT= ZOEAT*ZOO
C
C     check that calculated ingestion does not reduce phytoplankton
C     concentration to less than .0025 umoles p; if it does, adjust
C     ingestion so that .0025 umoles of phytoplankton (expressed as
C     p) remain
C
      IF ((PHYTO - ZEAT) .LT. (.0025*CVPB)) THEN
        ZEAT= PHYTO - (.0025*CVPB)
      END IF
C
C     calculate zooplankton assimilation efficiency
      IF (ZFOOD .EQ. 1) THEN
C       calculate assimilation efficiency resulting from ingestion
C       of high quality food; zeff is dimensionless
        ZEFF= -.06*PHYTO + 1.03
C
C       set upper limit on efficiency at 99 percent
        IF (ZEFF .GT. .99) THEN
          ZEFF= .99
        END IF
      ELSE IF (ZFOOD .EQ. 2) THEN
C       calculate assimilation efficiency resulting from ingestion
C       of medium quality food
        ZEFF= -.03*PHYTO + .47
C
C       set lower limit on efficiency at 20 percent
        IF (ZEFF .LT. .20) THEN
          ZEFF= .20
        END IF
      ELSE
C       calculate assimilation efficiency resulting from ingestion
C       of low quality food
        ZEFF= -.013*PHYTO + .17
C
C       set lower limit on efficiency at 3 percent
        IF (ZEFF .LT. .03) THEN
          ZEFF= .03
        END IF
      END IF
C
C     calculate zooplankton growth; zogr is expressed as mg biomass
C     per liter per interval
      ZOGR= ZEFF*ZEAT
C
C     calculate total zooplankton excretion (zexmass),excretion
C     decomposed to inorganic constituents (zingex), excretion
C     released as dead refractory constituents (zrefex), and
C     excretion released as dead nonrefractory material (znrfex)
      ZEXMAS= ZEAT - ZOGR
      ZREFEX= REFR*ZEXMAS
      ZINGEX= ZEXDEC*(ZEXMAS - ZREFEX)
      ZNRFEX= ZEXMAS - ZREFEX - ZINGEX
C
C     calculate zooplankton respiration; zres is expressed as mg
C     biomass per liter per interval
      ZRES  = ZRES20*(TCZRES**(TW - 20.))*ZOO
C
C     calculate zooplankton death; zdth is expressed as mg biomass
C     per liter per interval
      IF (DOX .GT. ANAER) THEN
C       calculate death using aerobic death rate
        ZDTH= ZD*ZOO
      ELSE
C       calculate death using sum of aerobic death rate and
C       anaerobic increment
        ZDTH= (ZD + OXZD)*ZOO
      END IF
C
C     calculate zooplankton population after growth, respiration,
C     and death; adjust respiration and death, if necessary, to
C     assure minimum population of zooplankton
C
C     first, account for net growth (growth - respiration)
      ZOO= ZOO + ZOGR - ZRES
C
C     maintain minimum population of .03 organisms per liter; zomass
C     is a user specified conversion factor from organisms/l to
C     mg biomass/l
      LOLIM= 0.03*ZOMASS
C
      IF (ZOO .LT. LOLIM) THEN
        ZRES= ZRES + ZOO - LOLIM
        ZOO = LOLIM
      ELSE
C       calculated respiration is acceptable
      END IF
C
C     subtract oxygen required to satisfy zooplankton respiration
C     from do state variable
      DOZOO= 1.1*ZRES
C     dox  = dox - 1.1*zres
      DOX  = DOX- DOZOO
C
      ZBOD= 0.0
      IF (DOX .LT. 0.0) THEN
C       include oxygen deficit in bod value
        ZBOD= -DOX
        DOX = 0.0
      END IF
C
C     subtract computed zooplankton death from zooplankton state
C     variable
      ZOO= ZOO - ZDTH
C
      IF (ZOO .LT. LOLIM) THEN
        ZDTH= ZDTH + ZOO - LOLIM
        ZOO = LOLIM
      ELSE
C       calculated death is acceptable
      END IF
C
C     calculate amount of inorganic constituents which are released
C     by zooplankton respiration and inorganic excretion
      ZNIT= (ZINGEX + ZRES)*CVBN
      ZPO4= (ZINGEX + ZRES)*CVBP
      ZCO2= (ZINGEX + ZRES)*CVBC
C
C     update state variables for inorganic constituents to account
C     for additions from zooplankton respiration and inorganic
C     excretion
      CALL DECBAL
     I            (TAMFG,I1,ZNIT,ZPO4,
     M             TAM,NO3,PO4)
C
C     calculate amount of refractory organic constituents which
C     result from zooplankton death and excretion
      ZORN= ((REFR*ZDTH) + ZREFEX)*CVBN
      ZORP= ((REFR*ZDTH) + ZREFEX)*CVBP
      ZORC= ((REFR*ZDTH) + ZREFEX)*CVBC
C
C     calculate amount of nonrefractory organics (bod) which result
C     from zooplankton death and excretion
      ZBOD= ZBOD + (ZDTH*CVNRBO) + (ZNRFEX*CVBO)
C
      CALL ORGBAL
     I            (ZORN,ZORP,ZORC,ZBOD,
     M             ORN,ORP,ORC,BOD)
C
      RETURN
      END
