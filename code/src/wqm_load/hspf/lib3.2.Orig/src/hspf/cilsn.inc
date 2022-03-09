C     from CILSN.INC   
C     IMPLND module common
C            SNOW section
      COMMON/SCRTCH/AIRTFG,SNOWFG,IWATFG,SLDFG,IWGFG,IQALFG,DUMZ,
     $              PFLAG,DUMY,PIVL,PYREND,IMPPFG,UUNITS,IUNITS,
     $              OUNITS,PUNIT,MESSU,MSGFL,LSNO,LSID,DELT,DELT60,
     $              DUMX,NDAY,NDAYS,DUMW,HRFG,DAYFG,STFG,EDAYFG,
     $              EMONFG,EPYRFG,YR,MON,DAY,HR,MIN,IVL,IVL1,PIVLNO,
     $              NXTMON,SPAFP,SPAKND,SPIVL,SPOPNO,DUMGEN,DUM1,
     $              PRECFP,DUM2,AIRTMP,DUM3,AIRTFP,SNIF,DTMPG,WINMOV,
     $              SOLRAD,DTGFP,WINMFP,SOLRFP,CCFP,ICEFG,SVP,LAT,
     $              MELEV,SHADE,SNOWCF,COVIND,RDCSN,TSNOW,SNOEVP,
     $              CCFACT,MWATER,MGMELT,DRYFG,IREGFG,SNOFFG,RAINFG,
     $              HR6FG,SNWCNT,SNECNT,PACKF,PACKW,PACKI,PDEPTH,
     $              COVINX,NEGHTS,XLNMLT,RDENPF,SKYCLR,SNOCOV,DULL,
     $              ALBEDO,PAKTMP,DEWTMP,SNOTMP,SNST4,RDNSN,MNEGHS,
     $              PACKWC,NEGHT,GMELTR,COMPCT,SNOWEP,MOSTHT,VAP,
     $              PACKFP,PFFP,PWFP,PIFP,PDEPFP,RDPFFP,SNOCFP,ALBFP,
     $              PAKTFP,SNCF1,SNCF2,RAINF,SNFFP,SNEFP,WYFP,MELTFP,
     $              RNFFP,DUMIMP
      INTEGER       AIRTFG,SNOWFG,IWATFG,SLDFG,IWGFG,IQALFG,DUMZ(7),
     $              PFLAG(6),DUMY(6),PIVL,PYREND,IMPPFG,UUNITS,IUNITS,
     $              OUNITS,PUNIT(2),MESSU,MSGFL,LSNO,LSID(5),DUMX(1),
     $              NDAY(12),NDAYS,DUMW(2),HRFG,DAYFG,STFG,EDAYFG,
     $              EMONFG,EPYRFG,YR,MON,DAY,HR,MIN,IVL,IVL1,PIVLNO,
     $              NXTMON,SPAFP,SPAKND,SPIVL,SPOPNO,DUMGEN(15),
     $              PRECFP,AIRTFP,DTGFP,WINMFP,SOLRFP,CCFP,ICEFG,
     $              DRYFG,IREGFG,SNOFFG,RAINFG,HR6FG,SNWCNT(1),
     $              SNECNT(2),PACKFP,PFFP,PWFP,PIFP,PDEPFP,RDPFFP,
     $              SNOCFP,ALBFP,PAKTFP,SNFFP,SNEFP,WYFP,MELTFP,RNFFP
      REAL          DELT,DELT60,AIRTMP,SNIF(5),DTMPG,WINMOV,SOLRAD,
     $              SVP(40),LAT,MELEV,SHADE,SNOWCF,COVIND,RDCSN,TSNOW,
     $              SNOEVP,CCFACT,MWATER,MGMELT,PACKF,PACKW,PACKI,
     $              PDEPTH,COVINX,NEGHTS,XLNMLT,RDENPF,SKYCLR,SNOCOV,
     $              DULL,ALBEDO,PAKTMP,DEWTMP,SNOTMP,SNST4(5),RDNSN,
     $              MNEGHS,PACKWC,NEGHT,GMELTR,COMPCT,SNOWEP,MOSTHT,
     $              VAP,SNCF1(5,5),SNCF2(2,5),RAINF
      INTEGER       DUM1(3),DUM2(25),DUM3,DUMIMP(17724)
C
      EQUIVALENCE   (ASVEC,AIRTFG),(UNIT,UUNITS),(TIMFG,HRFG),
     $              (DATIM,YR),(PREC,SNIF),(SNPM1,LAT),(SNPM2,RDCSN),
     $              (SNST1,PACKF),(SNST2,RDENPF),(SNST3,PAKTMP),
     $              (PACK,SNST4),(SNOWF,SNCF1(1,1)),
     $              (PRAIN,SNCF1(2,1)),(SNOWE,SNCF1(3,1)),
     $              (WYIELD,SNCF1(4,1)),(MELT,SNCF1(5,1)),
     $              (PAKIN,SNCF2(1,1)),(PAKDIF,SNCF2(2,1))
      INTEGER       ASVEC(6),UNIT(5),TIMFG(6),DATIM(5)
      REAL          PREC,SNPM1(5),SNPM2(6),SNST1(7),SNST2(5),SNST3(3),
     $              PACK,SNOWF,PRAIN,SNOWE,WYIELD,MELT,PAKIN,PAKDIF
C     end byte:  72000 word:18000