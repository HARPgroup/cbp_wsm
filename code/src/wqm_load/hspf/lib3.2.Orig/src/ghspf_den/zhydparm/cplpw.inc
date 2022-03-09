C     from CPLPW.INC   
C     PERLND module common
C            PWATER section
      COMMON/SCRTCH/AIRTFG,SNOWFG,PWATFG,SEDFG,PSTFG,PWGFG,PQALFG,
     $              MSTLFG,PESTFG,NITRFG,PHOSFG,TRACFG,AGFG,PFLAG,
     $              PIVL,PYREND,PERPFG,UUNITS,IUNITS,OUNITS,PUNIT,
     $              MESSU,MSGFL,LSNO,LSID,DELT,DELT60,DELT5,NDAY,
     $              NDAYS,NBLKSI,NBLKS,HRFG,DAYFG,STFG,EDAYFG,EMONFG,
     $              EPYRFG,YR,MON,DAY,HR,MIN,IVL,IVL1,PIVLNO,NXTMON,
     $              SPAFP,SPAKND,SPIVL,SPOPNO,DUMGEN,DUM1,PRECA,DUM2,
     $              PRECFP,DUM3,AIRTMP,DUM4,AIRTFP,DUM5,ICEFG,DUM6,
     $              PACKI,DUM7,SNOCOV,DUM8,PIFP,DUM9,SNOCFP,DUM10,
     $              SNCF1,DUM11,RAINF,DUM12,WYFP,DUM13,RNFFP,PWIF,
     $              PETINP,SLIFP,ILIFP,ALIFP,PETIFP,CSNOFG,RTOPFG,
     $              UZFG,VCSFG,VUZFG,VNNFG,VIFWFG,VIRCFG,VLEFG,IFFCFG,
     $              FOREST,LZSN,INFILT,LSUR,SLSUR,KVARY,KGW,PETMAX,
     $              PETMIN,INFEXP,INFILD,DEEPFR,BASETP,AGWETP,FZG,
     $              FZGL,CEPSCM,UZSNM,NSURM,INTFWM,UZRA,INTGRL,IRCM,
     $              LZETPM,SMSFG,FSMSFG,PWWCNT,PWECNT,CEPS,SURS,UZS,
     $              IFWS,LZS,AGWS,GWVS,INFFAC,PETADJ,PWST4,CEPSC,UZSN,
     $              NSUR,INTFW,IRC,LZETP,SURSS,SRC,DEC,IFWK1,IFWK2,
     $              LZFRAC,RLZRAT,RPARM,PERSFP,CEPSFP,SSFP,UZSFP,ISFP
      COMMON/SCRTCH/LZSFP,AGWSFP,RPMFP,SURSB,UZSB,IFWSB,SURSSB,SSBFP,
     $              UZSBFP,ISBFP,PWCF1,PWCF3,PWCF5,PWCF7,CEPO,SURI,
     $              SOFP,IOFP,AOFP,POFP,IGIFP,PETFP,CEFP,UEFP,LZETFP,
     $              AEFP,BASEFP,TAETFP,IIFP,UZIFP,INFFP,PCFP,LZIFP,
     $              AIFP,SURIFP,PWCF2,PWCF4,PWCF6,SOBFP,IOBFP,UEBFP,
     $              IIBFP,UZIBFP,INFBFP,PCBFP,DUM14,LGTMP,DUM15,LGTFP,
     $              DUMPER
      INTEGER       AIRTFG,SNOWFG,PWATFG,SEDFG,PSTFG,PWGFG,PQALFG,
     $              MSTLFG,PESTFG,NITRFG,PHOSFG,TRACFG,AGFG,PFLAG(12),
     $              PIVL,PYREND,PERPFG,UUNITS,IUNITS,OUNITS,PUNIT(2),
     $              MESSU,MSGFL,LSNO,LSID(5),NDAY(12),NDAYS,NBLKS,
     $              HRFG,DAYFG,STFG,EDAYFG,EMONFG,EPYRFG,YR,MON,DAY,
     $              HR,MIN,IVL,IVL1,PIVLNO,NXTMON,SPAFP,SPAKND,SPIVL,
     $              SPOPNO,DUMGEN(15),PRECFP,AIRTFP,ICEFG,PIFP,SNOCFP,
     $              WYFP,RNFFP,SLIFP,ILIFP,ALIFP,PETIFP,CSNOFG,RTOPFG,
     $              UZFG,VCSFG,VUZFG,VNNFG,VIFWFG,VIRCFG,VLEFG,IFFCFG,
     $              SMSFG,FSMSFG,PWWCNT(1),PWECNT(1),PERSFP,CEPSFP,
     $              SSFP,UZSFP,ISFP,LZSFP,AGWSFP,RPMFP,SSBFP(5),
     $              UZSBFP(5),ISBFP(5),SOFP,IOFP,AOFP,POFP,IGIFP,
     $              PETFP,CEFP,UEFP,LZETFP,AEFP,BASEFP,TAETFP,IIFP,
     $              UZIFP,INFFP,PCFP,LZIFP,AIFP,SURIFP,SOBFP(5),
     $              IOBFP(5),UEBFP(5),IIBFP(5),UZIBFP(5),INFBFP(5),
     $              PCBFP(5),LGTFP
      REAL          DELT,DELT60,DELT5,NBLKSI,PRECA,AIRTMP,PACKI,
     $              SNOCOV,SNCF1(5,5),RAINF,PWIF(3,5),PETINP,FOREST,
     $              LZSN,INFILT,LSUR,SLSUR,KVARY,KGW,PETMAX,PETMIN,
     $              INFEXP,INFILD,DEEPFR,BASETP,AGWETP,FZG,FZGL,
     $              CEPSCM(12),UZSNM(12),NSURM(12),INTFWM(12),
     $              UZRA(10),INTGRL(10),IRCM(12),LZETPM(12),CEPS,SURS,
     $              UZS,IFWS,LZS,AGWS,GWVS,INFFAC,PETADJ,PWST4(5),
     $              CEPSC,UZSN,NSUR,INTFW,IRC,LZETP,SURSS,SRC,DEC,
     $              IFWK1,IFWK2,LZFRAC,RLZRAT,RPARM,SURSB(5),UZSB(5),
     $              IFWSB(5),SURSSB(5),PWCF1(6,5),PWCF3(7,5),
     $              PWCF5(6,5),PWCF7(2,5),CEPO,SURI,PWCF2(5,2,5),
     $              PWCF4(5,5),PWCF6(5,4,5),LGTMP
      INTEGER       DUM1,DUM2,DUM3(25),DUM4,DUM5(12),DUM6(61),DUM7(6),
     $              DUM8(22),DUM9(2),DUM10(2),DUM11(10),DUM12(2),
     $              DUM13,DUM14(241),DUM15(10),DUMPER(16922)
C
      EQUIVALENCE   (ASVEC,AIRTFG),(PRTDAT,PFLAG),(UNIT,UUNITS),
     $              (TIMFG,HRFG),(DATIM,YR),(WYIELD,SNCF1(4,1)),
     $              (SURLI,PWIF(1,1)),(IFWLI,PWIF(2,1)),
     $              (AGWLI,PWIF(3,1)),(PWPM1,CSNOFG),(PWPM2,FOREST),
     $              (PWPM3,PETMAX),(PWPM5,FZG),(PWST1,CEPS),
     $              (PWST3,INFFAC),(PERS,PWST4),(PWST5,CEPSC),
     $              (PWST2,SURSB),(PWSTX,SSBFP(1)),(SUPY,PWCF1(1,1)),
     $              (SURO,PWCF1(2,1)),(IFWO,PWCF1(3,1)),
     $              (AGWO,PWCF1(4,1)),(PERO,PWCF1(5,1)),
     $              (IGWI,PWCF1(6,1)),(PET,PWCF3(1,1))
      EQUIVALENCE   (CEPE,PWCF3(2,1)),(UZET,PWCF3(3,1)),
     $              (LZET,PWCF3(4,1)),(AGWET,PWCF3(5,1)),
     $              (BASET,PWCF3(6,1)),(TAET,PWCF3(7,1)),
     $              (IFWI,PWCF5(1,1)),(UZI,PWCF5(2,1)),
     $              (INFIL,PWCF5(3,1)),(PERC,PWCF5(4,1)),
     $              (LZI,PWCF5(5,1)),(AGWI,PWCF5(6,1)),
     $              (WATIN,PWCF7(1,1)),(WATDIF,PWCF7(2,1)),
     $              (SUROB,PWCF2(1,1,1)),(IFWOB,PWCF2(1,2,1)),
     $              (UZETB,PWCF4(1,1)),(IFWIB,PWCF6(1,1,1)),
     $              (UZIB,PWCF6(1,2,1)),(INFILB,PWCF6(1,3,1))
      EQUIVALENCE   (PERCB,PWCF6(1,4,1))
      INTEGER       ASVEC(12),PRTDAT(14),UNIT(5),TIMFG(6),DATIM(5),
     $              PWPM1(10),PWSTX(5,3)
      REAL          WYIELD,SURLI,IFWLI,AGWLI,PWPM2(7),PWPM3(7),
     $              PWPM5(2),PWST1(7),PWST3(2),PERS,PWST5(6),
     $              PWST2(5,3),SUPY,SURO,IFWO,AGWO,PERO,IGWI,PET,CEPE,
     $              UZET,LZET,AGWET,BASET,TAET,IFWI,UZI,INFIL,PERC,
     $              LZI,AGWI,WATIN,WATDIF,SUROB(5),IFWOB(5),UZETB(5),
     $              IFWIB(5),UZIB(5),INFILB(5),PERCB(5)
C     end byte:  72000 word:18000