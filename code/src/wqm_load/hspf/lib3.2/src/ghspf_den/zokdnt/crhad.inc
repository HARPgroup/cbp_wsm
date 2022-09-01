C     from CRHAD.INC   
C     RCHRES module common
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxexi.inc'
C
C            ADCALC section
      COMMON/SCRTCH/HYDRFG,ADFG,CONSFG,HTFG,SEDFG,GQFG,OXFG,NUTFG,
     $              PLKFG,PHFG,ACIDFG,DUMZ,PFLAG,DUMY,PIVL,PYREND,
     $              RCHPFG,UUNITS,IUNITS,OUNITS,PUNIT,MESSU,MSGFL,
     $              RCHNO,RCHID,DELT,DELT60,DELTS,NDAY,NDAYS,LKFG,
     $              NEXITS,HRFG,DAYFG,STFG,EDAYFG,EMONFG,EPYRFG,YR,
     $              MON,DAY,HR,MIN,IVL,IVL1,PIVLNO,NXTMON,SPAFP,
     $              SPAKND,SPIVL,SPOPNO,DUMGEN,DUM1,PREC,DUM2,PRECFP,
     $              DUM3,VOL,VOLS,DUM4,RO,ROS,DUM5,VOLFP,DUM6,O,OS,
     $              DUM7,OFP,DUM8,CRRAT,SROVOL,EROVOL,SOVOL,EOVOL,
     $              DUMRCH
      INTEGER       HYDRFG,ADFG,CONSFG,HTFG,SEDFG,GQFG,OXFG,NUTFG,
     $              PLKFG,PHFG,ACIDFG,DUMZ(2),PFLAG(11),DUMY(1),PIVL,
     $              PYREND,RCHPFG,UUNITS,IUNITS,OUNITS,PUNIT(2),MESSU,
     $              MSGFL,RCHNO,RCHID(5),NDAY(12),NDAYS,LKFG,NEXITS,
     $              HRFG,DAYFG,STFG,EDAYFG,EMONFG,EPYRFG,YR,MON,DAY,
     $              HR,MIN,IVL,IVL1,PIVLNO,NXTMON,SPAFP,SPAKND,SPIVL,
     $              SPOPNO,DUMGEN(15),PRECFP,VOLFP,OFP(MXEXIT)
      REAL          DELT,DELT60,DELTS,PREC,RO,ROS,O(MXEXIT),
     $              OS(MXEXIT),CRRAT,SROVOL,EROVOL,SOVOL(MXEXIT),
     $              EOVOL(MXEXIT)
      DOUBLE PRECISION  VOL,VOLS
      INTEGER       DUM1(505),DUM2(102),DUM3(2146),DUM4(718),
     $              DUM5(101),DUM6(711),DUM7(500),DUM8(4148),
     $              DUMRCH(8939)
C
      EQUIVALENCE   (ASVEC,HYDRFG),(UNIT,UUNITS),(TIMFG,HRFG),
     $              (DATIM,YR),(AROVOL,SROVOL),(AOVOL,SOVOL(1))
      INTEGER       ASVEC(11),UNIT(5),TIMFG(6),DATIM(5)
      REAL          AROVOL(2),AOVOL(MXEXIT,2)
C     end byte:  72000 word:18000