C
C
C
      SUBROUTINE   PNITR
C
C     + + + PURPOSE + + +
C     Process input for section nitr
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION NITR1 + + +
      INCLUDE    'cplni.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I00,I1,I2,I3,I4,I2A,I2B,I4A,I4B,I,L,N,RETCOD,K,
     $             SCLU,SGRP
      REAL         R0,SUMFRC,TOLER,LIN(6),LINB(6,5),LTN(6),LTOTN,
     $             SOILD(4),LSN(6),LUN(6),LLN(6),LAN(6)
     .            , pwsoilD(4), fieldcp(4),pwwiltpt(4)    ! pw add 
      CHARACTER*60 HEADG,HEADGI
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL     SOLDAT,ITABLE,PLNTPM,FIRSTP,SVALP,STORGE,CYLDPM
      EXTERNAL     ZIPR,MDATBL,OMSTI,OMSTR,OMSG,RTABLE
      common/pwsoil/pwsoilD,fieldcp,pwwiltpt   
c will add Rtable( ****, wiltpt).... 
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TOLER/1.0E-5/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION NITR')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION NITR')
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 310
C
      I00=  0
      I0=   0
      R0= 0.0
      I1=   1
      I2=   2
      I3=   3
      I4=   4
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
C     initialize reaction fluxes
      I= 16
      CALL ZIPR (I,R0,
     O           SNRXF)
      CALL ZIPR (I,R0,
     O           UNRXF)
      CALL ZIPR (I,R0,
     O           LNRXF)
      CALL ZIPR (I,R0,
     O           ANRXF)
      I= 80
      CALL ZIPR (I,R0,
     O           SNRXFB)
      CALL ZIPR (I,R0,
     O           UNRXFB)
C
C     initialize month-data input
      I= 72
      CALL ZIPR (I,R0,
     O           NIAFXM)
      CALL ZIPR (I,R0,
     O           NIACNM)
C
C     initialize atmospheric deposition fluxes
      I= 30
      CALL ZIPR (I,R0,
     O           NCFX10)
      CALL ZIPR (I,R0,
     O           NCFX11)
C
C     initialize yield-based plant uptake fluxes and targets
      SNDFC= 0.0
      UNDFC= 0.0
      LNDFC= 0.0
      ANDFC= 0.0
      TNDFC= 0.0
      I= 4
      CALL ZIPR (I,R0,
     O           NUPTG)
      I= 25
      CALL ZIPR (I,R0,
     O           NCFX12)
C
C     initialize special action accumulators
      I= 15
      CALL ZIPR (I,R0,
     O           NITIF)
C
      MSGFL = FILE(15)
C
C     warning and error message counter initialization
      NWCNT(1)= 0
      NWCNT(2)= 0
      NWCNT(3)= 0
      NWCNT(4)= 0
      NWCNT(5)= 0
      NWCNT(6)= 0
      NECNT(1)= 0
C
C     heading for printing out total storages in surface, upper,
C     lower, and active groundwater layers
      HEADG(1:30)=   '     LORGN      AMAD      AMSU'
      HEADG(31:60)=  '       NO3      PLTN     RORGN'
C     ditto, for interflow storage,plus above-ground and litter compartments
      HEADGI(1:30)=  '      AMSU       NO3      SLON'
      HEADGI(31:60)= '      SRON    AGPLTN    LITTRN'
C
c   ! pw  will add 
        CALL SOLDAT (UUNITS,
     O               SOILM,SOILD)
c   ! pw  not adding because pwsoilD is already from SOLDAT call 
c       do j = 1, 4
c          pwsoild(j) = soild(j)
c       enddo 
c   ! pw added although already has "soilD and soilM"  
      IF (PESTFG.EQ.0) THEN
C       read in pesticide warning offsets, because some of those
C       warnings are also used by nitr
        PSWCNT(1)= 0
        PSWCNT(2)= 0
        PSWCNT(3)= 0
        PSWCNT(4)= 0
        PSWCNT(5)= 0
        PSWCNT(6)= 0
C       read in soil data - table type soil-data
        CALL SOLDAT (UUNITS,
     O               SOILM,SOILD)
      END IF
C
C     table-type nit-flags
      I2A= 85
      I4A= 10
      CALL ITABLE (I2A,I1,I4A,UUNITS,
     M             NITFG)
C
C     get atmospheric deposition flags - table-type nit-ad-flags
      I2A= 86
      I4A= 12
      CALL ITABLE (I2A,I1,I4A,UUNITS,
     M             NIADFG)
C
C     read in month-data tables where necessary
      DO 50 I= 1, 3
        DO 40 L= 1, 2
          N= 4*(I-1)+ 2*(L-1)+ 1
          IF (NIADFG(N) .GT. 0) THEN
C           monthly flux must be read
            CALL MDATBL
     I                  (NIADFG(N),
     O                   NIAFXM(1,I,L),RETCOD)
C           convert units to internal - not done by MDATBL
C           from lb/ac.day to lb/ac.ivl or from kg/ha.day to kg/ha.ivl
            DO 10 K= 1, 12
              NIAFXM(K,I,L)= NIAFXM(K,I,L)*DELT60/24.0
 10         CONTINUE
          END IF
          IF (NIADFG(N+1) .GT. 0) THEN
C           monthly ppn conc must be read
            CALL MDATBL
     /                  (NIADFG(N+1),
     O                   NIACNM(1,I,L),RETCOD)
C           convert units to internal - not done by MDATBL
            IF (UUNITS .EQ. 1) THEN
C             convert from mg/l to lb/ac.in
              DO 20 K= 1, 12
                NIACNM(K,I,L)= NIACNM(K,I,L)*0.226635
 20           CONTINUE
            ELSE IF (UUNITS .EQ. 2) THEN
C             convert from mg/l to kg/ha.in
              DO 30 K= 1, 12
                NIACNM(K,I,L)= NIACNM(K,I,L)*0.01
 30           CONTINUE
            END IF
          END IF
 40     CONTINUE
 50   CONTINUE
C
      IF (NUPTFG .EQ. 0) THEN
C       get first order plant uptake parameters
        I2A= 87
        I2B= 88
        CALL PLNTPM (MESSU,VNUTFG,I2A,UUNITS,DELT60,OUTLEV,I2B,
     O               KPLN,SKPLNM,UKPLNM,LKPLNM,AKPLNM)
      ELSE IF (NUPTFG .EQ. 1) THEN
C       get yield-based plant uptake parameters
C
C       get nitrogen parameters - table-type nit-yield
        I2A= 100
        I4A= 2
        CALL RTABLE (I2A,I1,I4A,UUNITS,
     M               NYLDPM)
C
C       get crop and soil parameters
        I2A= 101
        I2B= 102
        CALL CYLDPM (MESSU,MSGFL,I2A,UUNITS,OUTLEV,I2B,NDAY,SOILD,
     M               ECOUNT,
     O               WILTPT,NCRP,CRPDAT,CRPDAY,CRPFRC)
C
C       get monthly total uptake fractions - table-type mon-nupt-fr1
        I2A= 103
        I4A= 12
        CALL RTABLE (I2A,I1,I4A,UUNITS,
     M               NUPTFM)
C
C       check fractions for consistency
        SUMFRC= 0.0
        DO 52 I= 1, 12
          SUMFRC= SUMFRC+ NUPTFM(I)
 52     CONTINUE
        IF (ABS(SUMFRC-1.0) .GT. TOLER) THEN
C         error - fractions must sum to unity
          CALL OMSTI (LSNO)
          CALL OMSTR (SUMFRC)
          SGRP= 5
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
C
C       get monthly layer uptake fractions - table-type mon-nupt-fr2
        I2A= 104
        I4A= 12
        CALL RTABLE (I2A,I1,I4A,UUNITS,
     M               SNUPTM)
        CALL RTABLE (I2A,I2,I4A,UUNITS,
     M               UNUPTM)
        CALL RTABLE (I2A,I3,I4A,UUNITS,
     M               LNUPTM)
        CALL RTABLE (I2A,I4,I4A,UUNITS,
     M               ANUPTM)
C
C       check each month that fractions sum to unity
        DO 55 I= 1, 12
          SUMFRC= SNUPTM(I)+ UNUPTM(I)+ LNUPTM(I)+ ANUPTM(I)
          IF ( (ABS(SUMFRC-1.0) .GT. TOLER) .AND.
     $         (NUPTFM(I) .GT. 0.0) ) THEN
C           error - fractions don't sum to unity when there is uptake
            CALL OMSTI (LSNO)
            CALL OMSTI (I)
            CALL OMSTR (SUMFRC)
            SGRP= 6
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
 55     CONTINUE
C
      ELSE IF ( (NUPTFG .EQ. 2) .OR. (NUPTFG .EQ. -2) ) THEN
C       get saturation-kinetics plant uptake and immobilizaton parameters
c                                    
c! pw: the next 26 lines are added by pw! ??????? 
c      get nitrogen parameters - table-type nit-yield
c no     I2A = 100
c no     I4A = 2
c no     call RTABLE (I2A, I1, I4A, UUNITS,
c no M                NYLDPM)
c        get crop and soil parameters
c        I2A = 101
c        I2B = 102
c       CALL CYLDPM (MESSU,MSGFL,I2A,UUNITS,OUTLEV,I2B,NDAY,SOILD,
c    M               ECOUNT,
c    O               WILTPT,NCRP,CRPDAT,CRPDAY,CRPFRC)
c   CYLDPM gets wiltpt  ----- alternatively the following
c with Rtable( ****, wiltpt).... 
C     get wilting points - table-type soil-data2
c          goto 551  ! pw 9/1/06
      I1 = 1    ! from hperagut.f (CYLDPM())
      I= 4
      num1=101
      CALL RTABLE (NUM1,I1,I,UUNITS,
     M             WILTPT)
c      write(98,*) wiltpt
C     convert from in/in or in/cm to inches
         do jw=1,4
c        pwwiltpt = pwwiltpt * pwsoild ! wiltpt from in/in as (in) spec soil
           wiltpt(jw) = 0.1*0.5   ! 5   ! hardwireding   (1n/1n) 
           fieldcp(jw) = wiltpt(jw)**0.57   ! pw 8/25/06  (in/in) 
           pwwiltpt(jw) =   wiltpt(jw) * pwsoild(jw) !             as (in) 
           fieldcp(jw) = fieldcp(jw) * pwsoild(jw)   ! pw 8/25/06 , as (in)
           wiltpt(jw) = pwwiltpt(jw) ! wiltpt(jw) * pwsoild(jw)   as (in) 
c          fieldcp(jw) = -9.033*wiltpt(jw)*wiltpt(jw)+3.7062*wiltpt(jw)
c                        + 0.0027     ! fieldcp also for spec soil depth 
         enddo  
c! pw  end pw added
551          continue  ! pw 
        IF (VNUTFG .EQ. 0) THEN
C         get maximum rate constants - table-type nit-upimkmax
          I2A= 93
          I4A= 4
C
C         surface layer
          I= 1
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 SKSATN)
C
C         upper layer
          IF (NUPTFG .EQ. 2) THEN
C           use different table for each layer
            I= 2
          END IF
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 UKSATN)
C
C         lower layer
          IF (NUPTFG .EQ. 2) THEN
C           use different table for each layer
            I= 3
          END IF
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 LKSATN)
C
C         active groundwater layer
          IF (NUPTFG .EQ. 2) THEN
C           use different table for each layer
            I= 4
          END IF
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 AKSATN)
C
C         convert units from mg/l/day to mg/l/ivl
          DO 60 I= 1, 4
            SKSATN(I)= SKSATN(I)*DELT60/24.0
            UKSATN(I)= UKSATN(I)*DELT60/24.0
            LKSATN(I)= LKSATN(I)*DELT60/24.0
            AKSATN(I)= AKSATN(I)*DELT60/24.0
 60       CONTINUE
        ELSE
C         get monthly rate constants
C
C         get monthly nitrate uptake maximum rates - table-type mon-nitupni
          I2A= 95
                      I4A= 12
C
C         surface layer
          I= 1
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 SKUNIM)
C
C         upper layer
          IF (NUPTFG .EQ. 2) THEN
C           use different table for each layer
            I= 2
          END IF
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 UKUNIM)
C
C         lower layer
          IF (NUPTFG .EQ. 2) THEN
C           use different table for each layer
            I= 3
          END IF
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 LKUNIM)
C
C         active groundwater layer
          IF (NUPTFG .EQ. 2) THEN
C           use different table for each layer
            I= 4
          END IF
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 AKUNIM)
C
C         convert units from mg/l/day to mg/l/ivl
          DO 70 I= 1, 12
            SKUNIM(I)= SKUNIM(I)*DELT60/24.0
            UKUNIM(I)= UKUNIM(I)*DELT60/24.0
            LKUNIM(I)= LKUNIM(I)*DELT60/24.0
            AKUNIM(I)= AKUNIM(I)*DELT60/24.0
 70       CONTINUE
C
C         get monthly ammonia uptake maximum rates - table-type mon-nitupam
          I2A= 96
          I4A= 12
C
C         surface layer
          I= 1
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 SKUAMM)
C
C         upper layer
          IF (NUPTFG .EQ. 2) THEN
C           use different table for each layer
            I= 2
          END IF
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 UKUAMM)
C
C         lower layer
          IF (NUPTFG .EQ. 2) THEN
C           use different table for each layer
            I= 3
          END IF
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 LKUAMM)
C
C         active groundwater layer
          IF (NUPTFG .EQ. 2) THEN
C           use different table for each layer
            I= 4
          END IF
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 AKUAMM)
C
C         convert units from mg/l/day to mg/l/ivl
          DO 80 I= 1, 12
            SKUAMM(I)= SKUAMM(I)*DELT60/24.0
            UKUAMM(I)= UKUAMM(I)*DELT60/24.0
            LKUAMM(I)= LKUAMM(I)*DELT60/24.0
            AKUAMM(I)= AKUAMM(I)*DELT60/24.0
 80       CONTINUE
C
C         get monthly nitrate immobilization maximum rates - table-type
C         mon-nitimni
          I2A= 97
          I4A= 12
C
C         surface layer
          I= 1
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 SKINIM)
C
C         upper layer
          IF (NUPTFG .EQ. 2) THEN
C           use different table for each layer
            I= 2
          END IF
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 UKINIM)
C
C         lower layer
          IF (NUPTFG .EQ. 2) THEN
C           use different table for each layer
            I= 3
          END IF
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 LKINIM)
C
C         active groundwater layer
          IF (NUPTFG .EQ. 2) THEN
C           use different table for each layer
            I= 4
          END IF
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 AKINIM)
C
C         convert units from mg/l/day to mg/l/ivl
          DO 90 I= 1, 12
            SKINIM(I)= SKINIM(I)*DELT60/24.0
            UKINIM(I)= UKINIM(I)*DELT60/24.0
            LKINIM(I)= LKINIM(I)*DELT60/24.0
            AKINIM(I)= AKINIM(I)*DELT60/24.0
 90       CONTINUE
C
C         get monthly ammonia immobilization maximum rates - table-type
C         mon-nitimam
          I2A= 98
          I4A= 12
C
C         surface layer
          I= 1
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 SKIAMM)
C
C         upper layer
          IF (NUPTFG .EQ. 2) THEN
C           use different table for each layer
            I= 2
          END IF
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 UKIAMM)
C
C         lower layer
          IF (NUPTFG .EQ. 2) THEN
C           use different table for each layer
            I= 3
          END IF
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 LKIAMM)
C
C         active groundwater layer
          IF (NUPTFG .EQ. 2) THEN
C           use different table for each layer
            I= 4
          END IF
          CALL RTABLE (I2A,I,I4A,UUNITS,
     M                 AKIAMM)
C
C         convert units from mg/l/day to mg/l/ivl
          DO 100 I= 1, 12
            SKIAMM(I)= SKIAMM(I)*DELT60/24.0
            UKIAMM(I)= UKIAMM(I)*DELT60/24.0
            LKIAMM(I)= LKIAMM(I)*DELT60/24.0
            AKIAMM(I)= AKIAMM(I)*DELT60/24.0
 100      CONTINUE
        END IF
C
C       get half-saturation constants - table-type nit-upimcsat
        I2A= 94
        I4A= 4
C
C       surface layer
        I= 1
        CALL RTABLE (I2A,I,I4A,UUNITS,
     M               SCSATN)
C
C       upper layer
        IF (NUPTFG .EQ. 2) THEN
C         use different table for each layer
          I= 2
        END IF
        CALL RTABLE (I2A,I,I4A,UUNITS,
     M               UCSATN)
C
C       lower layer
        IF (NUPTFG .EQ. 2) THEN
C         use different table for each layer
          I= 3
        END IF
        CALL RTABLE (I2A,I,I4A,UUNITS,
     M               LCSATN)
C
C       active groundwater layer
        IF (NUPTFG .EQ. 2) THEN
C         use different table for each layer
          I= 4
        END IF
        CALL RTABLE (I2A,I,I4A,UUNITS,
     M               ACSATN)
      END IF
C
      IF (ALPNFG .EQ. 1) THEN
C       get parameters for aboveground plant uptake
        IF (VNUTFG .EQ. 0) THEN
C         get constant values - table-type nit-agutf
          I2A= 113
          I4A= 4
          CALL RTABLE (I2A,I1,I4A,UUNITS,
     M                 ANUTF)
        ELSE
C         get monthly values- table-type mon-nitagutf
          I2A= 114
          I4A= 12
          CALL RTABLE (I2A,I1,I4A,UUNITS,
     M                 SANUFM)
          CALL RTABLE (I2A,I2,I4A,UUNITS,
     M                 UANUFM)
          CALL RTABLE (I2A,I3,I4A,UUNITS,
     M                 LANUFM)
          CALL RTABLE (I2A,I4,I4A,UUNITS,
     M                 AANUFM)
        END IF
      END IF
C
      IF (AMVOFG .GT. 0) THEN
C       get parameters for ammonia volatilization - table-type nit-amvolat
        I2A= 99
        I4A= 6
        CALL RTABLE (I2A,I1,I4A,UUNITS,
     M               AMVOPM)
C
C       convert units of rates from 1/day to 1/ivl
        DO 110 I= 1,4
          AMVOPM(I)= AMVOPM(I)*DELT60/24.0
 110    CONTINUE
      END IF
C
      IF (VNPRFG .EQ. 0) THEN
C       get constant rates for below-ground plant return
C       table-type nit-bgplret
        I2A= 106
        I4A= 5
        CALL RTABLE (I2A,I1,I4A,UUNITS,
     M               KRETBN)
C       convert units of rates from 1/day to 1/ivl
        DO 120 I= 1, 4
          KRETBN(I)= KRETBN(I)*DELT60/24.0
 120    CONTINUE
        IF (ALPNFG .EQ. 1) THEN
C         get constant rates for above-ground plant return
C         table-type nit-agplret
          I2A= 107
          I4A= 4
          CALL RTABLE (I2A,I1,I4A,UUNITS,
     M                 KRETAN)
C         convert units of rates from 1/day to 1/ivl
          DO 130 I= 1, 3
            KRETAN(I)= KRETAN(I)*DELT60/24.0
 130      CONTINUE
        END IF
      ELSE
C       get monthly rates for plant return
C
C       below-ground return rates - table-type mon-npretbg
        I2A= 108
        I4A= 12
        CALL RTABLE (I2A,I1,I4A,UUNITS,
     M               SKRBNM)
        CALL RTABLE (I2A,I2,I4A,UUNITS,
     M               UKRBNM)
        CALL RTABLE (I2A,I3,I4A,UUNITS,
     M               LKRBNM)
        CALL RTABLE (I2A,I4,I4A,UUNITS,
     M               AKRBNM)
C       convert units of rates from 1/day to 1/ivl
        DO 140 I= 1, 12
          SKRBNM(I)= SKRBNM(I)*DELT60/24.0
          UKRBNM(I)= UKRBNM(I)*DELT60/24.0
          LKRBNM(I)= LKRBNM(I)*DELT60/24.0
          AKRBNM(I)= AKRBNM(I)*DELT60/24.0
 140    CONTINUE
C
C       below-ground return refractory fractions - table-type mon-npretfbg
        I2A= 111
        I4A= 12
        CALL RTABLE (I2A,I1,I4A,UUNITS,
     M               BNPRFM)
C
        IF (ALPNFG .EQ. 1) THEN
C         get above-ground and litter return rates
C
C         above-ground return rates - table-type mon-npretag
          I2A= 109
          I4A= 12
          CALL RTABLE (I2A,I1,I4A,UUNITS,
     M                 KRANM)
C
C         litter return rates - table-type mon-npretli
          I2A= 110
          I4A= 12
          CALL RTABLE (I2A,I1,I4A,UUNITS,
     M                 SKRLNM)
          CALL RTABLE (I2A,I2,I4A,UUNITS,
     M                 UKRLNM)
          DO 150 I= 1, 12
            KRANM(I)= KRANM(I)*DELT60/24.0
            SKRLNM(I)= SKRLNM(I)*DELT60/24.0
            UKRLNM(I)= UKRLNM(I)*DELT60/24.0
 150      CONTINUE
C
C         litter return refractory fractions - table-type mon-npretfli
          I2A= 112
          I4A= 12
          CALL RTABLE (I2A,I1,I4A,UUNITS,
     M                 LNPRFM)
        END IF
      END IF
C
C     get other parameters required to handle first-order reactions
      I2A= 89
      I2B= 90
      I4A= 10
      I4B= 7
      CALL FIRSTP (OUTLEV,UUNITS,DELT60,I2A,I1,I4A,I2B,I0,I4B,
     I             CNUMN,BNUMN,LSNO,MESSU,MSGFL,
     M             NWCNT(6),
     O             GNPM(1),SNPM(1),UNPM(1),LNPM(1),ANPM(1))
C     check that no3utf and nh4utf sum to one
      SUMFRC= GNPM(1)+ GNPM(2)
      IF (ABS(SUMFRC-1.0) .GT. TOLER) THEN
C       error - uptake fractions must sum to unity
        CALL OMSTI (LSNO)
        CALL OMSTR (SUMFRC)
        SGRP= 7
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (FORAFG.NE.0) THEN
C       get parameters to simulate ammonium ads/des using
C       single-valued freundlich equation
        I2A= 91
        I2B= 92
        CALL SVALP (OUTLEV,MESSU,UUNITS,I2A,I1,I2B,I0,
     O              GNPM(11),SNPM(8),UNPM(8),LNPM(8),
     $              ANPM(8))
      END IF
C
C     get parameters for organic nitrogen table-type nit-orgpm
      I2A= 105
      I4A= 4
      CALL RTABLE (I2A,I1,I4A,UUNITS,
     M             SORNPM)
      CALL RTABLE (I2A,I2,I4A,UUNITS,
     M             UORNPM)
      CALL RTABLE (I2A,I3,I4A,UUNITS,
     M             LORNPM)
      CALL RTABLE (I2A,I4,I4A,UUNITS,
     M             AORNPM)
C
C     state variables
C     counters for indicating when reaction rates are to be recalculated
      BIVLN= BNUMN
      CIVLN= CNUMN
C
C     initial storages
      I2A= 115
      I2B= 116
      I4A= 6
      I4B= 6
      CALL STORGE (MESSU,OUTLEV,UUNITS,NBLKS,NBLKSI,I2A,
     I             I4A,HEADG,I2B,I4B,HEADGI,
     M             I0,I00,
     O             LSN,LUN,LIN,SNB,UNB,LINB,
     O             LLN,LAN,LTN,LTOTN)
C
C     copy surface, upper, lower, and groundwater inorganic and plant storages
      DO 155 I= 2, 5
        SN(I)= LSN(I)
        UN(I)= LUN(I)
        LN(I)= LLN(I)
        AN(I)= LAN(I)
 155  CONTINUE
C
C     partition organic N storages
C
C     surface layer
      IF (SORNPM(1) .GT. 1.0E15) THEN
C       infinity - all labile is adsorbed
        SN(6)= 0.0
      ELSE
C       partition labile
        SN(6)= LSN(1)/(SORNPM(1)+ 1)
      END IF
      SN(1)= LSN(1)- SN(6)
      IF (SORNPM(2) .GT. 1.0E15) THEN
C       infinity - all refractory is adsorbed
        SN(8)= 0.0
      ELSE
C       partition refractory
        SN(8)= LSN(6)/(SORNPM(2)+ 1)
      END IF
      SN(7)= LSN(6)- SN(8)
C
C     upper layer
      IF (UORNPM(1) .GT. 1.0E15) THEN
C       infinity - all labile is adsorbed
        UN(6)= 0.0
      ELSE
C       partition labile
        UN(6)= LUN(1)/(UORNPM(1)+ 1)
      END IF
      UN(1)= LUN(1)- UN(6)
      IF (UORNPM(2) .GT. 1.0E15) THEN
C       infinity - all refractory is adsorbed
        UN(8)= 0.0
      ELSE
C       partition refractory
        UN(8)= LUN(6)/(UORNPM(2)+ 1)
      END IF
      UN(7)= LUN(6)- UN(8)
C
C     lower layer
      IF (LORNPM(1) .GT. 1.0E15) THEN
C       infinity - all labile is adsorbed
        LN(6)= 0.0
      ELSE
C       partition labile
        LN(6)= LLN(1)/(LORNPM(1)+ 1)
      END IF
      LN(1)= LLN(1)- LN(6)
      IF (LORNPM(2) .GT. 1.0E15) THEN
C       infinity - all refractory is adsorbed
        LN(8)= 0.0
      ELSE
C       partition refractory
        LN(8)= LLN(6)/(LORNPM(2)+ 1)
      END IF
      LN(7)= LLN(6)- LN(8)
C
C     active groundwater layer
      IF (AORNPM(1) .GT. 1.0E15) THEN
C       infinity - all labile is adsorbed
        AN(6)= 0.0
      ELSE
C       partition labile
        AN(6)= LAN(1)/(AORNPM(1)+ 1)
      END IF
      AN(1)= LAN(1)- AN(6)
      IF (AORNPM(2) .GT. 1.0E15) THEN
C       infinity - all refractory is adsorbed
        AN(8)= 0.0
      ELSE
C       partition refractory
        AN(8)= LAN(6)/(AORNPM(2)+ 1)
      END IF
      AN(7)= LAN(6)- AN(8)
C
C     copy interflow and aboveground storages
      DO 160 I= 1, 4
        IN(I)= LIN(I)
 160  CONTINUE
      IF (ALPNFG .EQ. 1) THEN
C       above-ground storages turned on
        AGPLTN= LIN(5)
        LITTRN= LIN(6)
      ELSE
C       above-ground is zero
        AGPLTN= 0.0
        LITTRN= 0.0
      END IF
C
C     compute total storages
      DO 170 I= 1, 8
        TN(I)= SN(I)+ UN(I)+ LN(I)+ AN(I)
 170  CONTINUE
      TN(3)= TN(3)+ IN(1)
      TN(4)= TN(4)+ IN(2)
      TN(6)= TN(6)+ IN(3)
      TN(8)= TN(8)+ IN(4)
      TN(5)= TN(5)+ AGPLTN+ LITTRN
C
      TOTNIT= 0.0
      DO 180 I= 1, 8
        TOTNIT= TOTNIT+ TN(I)
 180  CONTINUE
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
      SUBROUTINE   NITR
C
C     + + + PURPOSE + + +
C     Simulate nitrogen behavior in detail
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION NITR2 + + +
      INCLUDE     'cplni.inc'
      INCLUDE     'cmpad.inc'
c pw for dynamic litter, not using!       include     'pwgligl.inc'
      common/pwsoil/pwsoilD,fieldcp,pwwiltpt         ! pw 
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     BRXNFG,CRXNFG,I,J,TMPFG,N
      REAL        FSD,SDNB(3),TSAMSB(5),TSNO3B(5),TSSLNB(5),TSSRNB(5),
     $            MOISTM,RETLN
     M           , pwsoild(4),fieldcp(4),pwwiltpt(4)    ! pw add 
c ! pw           ,pwwiltpt(4), pwsoild(4), pwfake(5)    ! pw 
      CHARACTER*4 LAYID(4)
C
C     + + + FUNCTIONS + + +
      REAL        DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL    DAYVAL,AGRGET,SDFRAC,SEDMOV,TOPMOV,NITRXN,SUBMOV,
     #            YUPTGT,YUPINI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        LAYID/'SURF','UPPR','LOWR','GRND'/
C
C     + + + END SPECIFICATIONS + + +
C
      TMPFG= 0
C     is soil temperature required?
      IF (FORAFG .EQ. 0) TMPFG= 1
C
      IF ( (PESTFG .EQ. 0) .OR. (TMPFG .NE. 0) ) THEN
C       get time series needed for agrichemical sections
C       always do this if soil temp data is needed.
        CALL AGRGET (SEDFG,NBLKS,MSTLFG,PSTFG,TMPFG,IVL1,SOSDFP,
     I               SOSDBX,MSTFP,FRACFP,MSTBFP,
     I               FRACBX,SLTFP,ULTFP,LGTFP,
     O               SOSED,SOSDB,MST,FRAC,MSTB,FRACB,
     O               SLTMP,ULTMP,LGTMP)
      ELSE
C       the time series for the agri-chemical sections
C       are already available
      END IF
      PREC= PAD(PRECFP+IVL1)
C
C     compute atmospheric deposition influx
      DO 40 J= 1, 3
        DO 30 I= 1, 2
          N= 4*(J-1)+ 2*(I-1)+ 1
C         dry deposition
          IF (NIADFG(N) .LE. -1) THEN
C           deposition input as a time series
            NIADDR(J,I)= PAD(NIAFFP(J,I)+IVL1)
          ELSE IF (NIADFG(N) .GE. 1) THEN
C           deposition input as monthly values
            NIADDR(J,I)= DAYVAL(NIAFXM(MON,J,I),NIAFXM(NXTMON,J,I),DAY,
     I                          NDAYS)
          ELSE
C           no deposition
            NIADDR(J,I)= 0.0
          END IF
C         wet deposition
          IF (NIADFG(N+1) .LE. -1) THEN
C           deposition input as a time series
            NIADWT(J,I)= PREC*PAD(NIACFP(J,I)+IVL1)
          ELSE IF (NIADFG(N+1) .GE. 1) THEN
C           deposition input as monthly values
            NIADWT(J,I)= PREC*DAYVAL(NIACNM(MON,J,I),
     I                               NIACNM(NXTMON,J,I),DAY,NDAYS)
          ELSE
C           no deposition
            NIADWT(J,I)= 0.0
          END IF
 30     CONTINUE
 40   CONTINUE
C
C     determine when reactions should be done
C
      IF (BIVLN.EQ.BNUMN) THEN
C       biochemical reaction fluxes are to be recalculated this
C       interval
C       set biochemical reaction flag on
        BRXNFG= 1
        BIVLN = 1
      ELSE
C       biochemical reaction fluxes are not to be recalculated
C       this interval
        BIVLN = BIVLN+ 1
        BRXNFG= 0
      END IF
C
      IF (CIVLN.EQ.CNUMN) THEN
C       chemical reaction (adsorption/desorption) fluxes are
C       to be calculated this interval
C       set chemical (adsorption/desorption) reaction flag on
        CRXNFG= 1
        CIVLN = 1
      ELSE
C       chemical reaction (adsorption/desorption) fluxes are not
C       to be recalculated this interval
        CIVLN = CIVLN+ 1
        CRXNFG= 0
      END IF
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
        IF (VNPRFG .EQ. 1) THEN
C         plant return rate parameters are allowed to vary throughout
C         the year - interpolate for daily value
          KRETBN(1)= DAYVAL(SKRBNM(MON),SKRBNM(NXTMON),DAY,NDAYS)
          KRETBN(2)= DAYVAL(UKRBNM(MON),UKRBNM(NXTMON),DAY,NDAYS)
          KRETBN(3)= DAYVAL(LKRBNM(MON),LKRBNM(NXTMON),DAY,NDAYS)
          KRETBN(4)= DAYVAL(AKRBNM(MON),AKRBNM(NXTMON),DAY,NDAYS)
          KRETBN(5)= DAYVAL(BNPRFM(MON),BNPRFM(NXTMON),DAY,NDAYS)
          IF (ALPNFG .EQ. 1) THEN
C           above-ground compartments being simulated
            KRETAN(1)= DAYVAL(KRANM(MON),KRANM(NXTMON),DAY,NDAYS)
            KRETAN(2)= DAYVAL(SKRLNM(MON),SKRLNM(NXTMON),DAY,NDAYS)
            KRETAN(3)= DAYVAL(UKRLNM(MON),UKRLNM(NXTMON),DAY,NDAYS)
            KRETAN(4)= DAYVAL(LNPRFM(MON),LNPRFM(NXTMON),DAY,NDAYS)
          END IF
        END IF
        IF (VNUTFG .EQ. 1) THEN
C         plant uptake parameters are allowed to vary throughout the year
C         interpolate for the daily value
          IF (ALPNFG .EQ. 1) THEN
C           above-ground fractions
            ANUTF(1)= DAYVAL(SANUFM(MON),SANUFM(NXTMON),DAY,NDAYS)
            ANUTF(2)= DAYVAL(UANUFM(MON),UANUFM(NXTMON),DAY,NDAYS)
            ANUTF(3)= DAYVAL(LANUFM(MON),LANUFM(NXTMON),DAY,NDAYS)
            ANUTF(4)= DAYVAL(AANUFM(MON),AANUFM(NXTMON),DAY,NDAYS)
          END IF
          IF (NUPTFG .EQ. 0) THEN
C           first order plant uptake
            SKPLN= DAYVAL(SKPLNM(MON),SKPLNM(NXTMON),DAY,NDAYS)
            UKPLN= DAYVAL(UKPLNM(MON),UKPLNM(NXTMON),DAY,NDAYS)
            LKPLN= DAYVAL(LKPLNM(MON),LKPLNM(NXTMON),DAY,NDAYS)
            AKPLN= DAYVAL(AKPLNM(MON),AKPLNM(NXTMON),DAY,NDAYS)
          ELSE IF ( (NUPTFG .EQ. 2) .OR. (NUPTFG .EQ. -2) ) THEN
C           half-saturation kinetics for plant uptake and immobilization
            SKSATN(1)= DAYVAL(SKUNIM(MON),SKUNIM(NXTMON),DAY,NDAYS)
            SKSATN(2)= DAYVAL(SKUAMM(MON),SKUAMM(NXTMON),DAY,NDAYS)
            SKSATN(3)= DAYVAL(SKINIM(MON),SKINIM(NXTMON),DAY,NDAYS)
            SKSATN(4)= DAYVAL(SKIAMM(MON),SKIAMM(NXTMON),DAY,NDAYS)
            UKSATN(1)= DAYVAL(UKUNIM(MON),UKUNIM(NXTMON),DAY,NDAYS)
            UKSATN(2)= DAYVAL(UKUAMM(MON),UKUAMM(NXTMON),DAY,NDAYS)
            UKSATN(3)= DAYVAL(UKINIM(MON),UKINIM(NXTMON),DAY,NDAYS)
            UKSATN(4)= DAYVAL(UKIAMM(MON),UKIAMM(NXTMON),DAY,NDAYS)
            LKSATN(1)= DAYVAL(LKUNIM(MON),LKUNIM(NXTMON),DAY,NDAYS)
            LKSATN(2)= DAYVAL(LKUAMM(MON),LKUAMM(NXTMON),DAY,NDAYS)
            LKSATN(3)= DAYVAL(LKINIM(MON),LKINIM(NXTMON),DAY,NDAYS)
            LKSATN(4)= DAYVAL(LKIAMM(MON),LKIAMM(NXTMON),DAY,NDAYS)
            AKSATN(1)= DAYVAL(AKUNIM(MON),AKUNIM(NXTMON),DAY,NDAYS)
            AKSATN(2)= DAYVAL(AKUAMM(MON),AKUAMM(NXTMON),DAY,NDAYS)
            AKSATN(3)= DAYVAL(AKINIM(MON),AKINIM(NXTMON),DAY,NDAYS)
            AKSATN(4)= DAYVAL(AKIAMM(MON),AKIAMM(NXTMON),DAY,NDAYS)
          END IF
        ELSE
C         plant uptake parmameters for nitrogen do not vary
        END IF
C
        IF (NUPTFG .EQ. 1) THEN
C         yield-based plant uptake parameters vary monthly, and
C         daily targets must be calculated from a trapezoidal
C         function
C
          IF (STFG .EQ. 1) THEN
C           get initial values of previous month's final daily uptake target
            CALL YUPINI (DELT60,YR,MON,DAY,NDAY,NCRP,CRPDAT,CRPDAY,
     I                   CRPFRC,NUPTGT,NUPTFM,SNUPTM,UNUPTM,LNUPTM,
     I                   ANUPTM,
     O                   SPNUTG,UPNUTG,LPNUTG,APNUTG)
          END IF
          CALL YUPTGT (DELT60,YR,MON,DAY,NDAYS,NCRP,CRPDAT,CRPDAY,
     I                 CRPFRC,NUPTGT,NUPTFM,SNUPTM,UNUPTM,LNUPTM,ANUPTM,
     M                 SPNUTG,UPNUTG,LPNUTG,APNUTG,SNDFC,UNDFC,LNDFC,
     M                 ANDFC,TNDFC,
     O                 SNUPTG,UNUPTG,LNUPTG,ANUPTG)
C
        END IF
      END IF
C
      IF (ALPNFG .EQ. 1) THEN
C       calculate above-ground and litter compartment plant returns
C
        IF (BRXNFG .EQ. 1) THEN
C         recalculate reaction fluxes
          RETAGN= KRETAN(1)*AGPLTN
          RTLLN(1)= KRETAN(2)*LITTRN*(1.0- KRETAN(4))
          RTLLN(2)= KRETAN(3)*LITTRN*(1.0- KRETAN(4))
          RTRLN(1)= KRETAN(2)*LITTRN*KRETAN(4)
          RTRLN(2)= KRETAN(3)*LITTRN*KRETAN(4)
          RTLLN(3)= RTLLN(1)+ RTLLN(2)
          RTRLN(3)= RTRLN(1)+ RTRLN(2)
        END IF
        RETLN= RTLLN(3)+ RTRLN(3)
C
C       update above-ground storage
        IF (RETAGN .GT. AGPLTN) THEN
C         reduce plant return to make storage non-negative
          RETAGN= AGPLTN
          AGPLTN= 0.0
        ELSE
C         use calculated flux
          AGPLTN= AGPLTN- RETAGN
        END IF
C
C       update litter storage
        IF (RETLN .GT. LITTRN) THEN
C         reduce plant returns to make storage non-negative
          RTLLN(1)= RTLLN(1)*LITTRN/RETLN
          RTLLN(2)= RTLLN(2)*LITTRN/RETLN
          RTRLN(1)= RTRLN(1)*LITTRN/RETLN
          RTRLN(2)= RTRLN(2)*LITTRN/RETLN
          RTLLN(3)= RTLLN(1)+ RTLLN(2)
          RTRLN(3)= RTRLN(1)+ RTRLN(2)
          RETLN= LITTRN
          LITTRN= 0.0
        ELSE
C         use calculated flux
          LITTRN= LITTRN- RETLN
        END IF
C
C       update target storages
        LITTRN= LITTRN+ RETAGN
        SN(1)= SN(1)+ RTLLN(1)
        SN(7)= SN(7)+ RTRLN(1)
        UN(1)= UN(1)+ RTLLN(2)
        UN(7)= UN(7)+ RTRLN(2)
      ELSE
C       no above-ground compartments - zero fluxes
        RETAGN=   0.0
        RTLLN(1)= 0.0
        RTLLN(2)= 0.0
        RTLLN(3)= 0.0
        RTRLN(1)= 0.0
        RTRLN(2)= 0.0
        RTRLN(3)= 0.0
      END IF
C
      IF (NBLKS .EQ. 1) THEN
C       surface and upper layers of the land segment have not
C       been subdivided into blocks
C
C       update storages for atmospheric deposition
        SN(1)= SN(1)+ NIADDR(3,1)+ NIADWT(3,1)
        SN(3)= SN(3)+ NIADDR(2,1)+ NIADWT(2,1)
        SN(4)= SN(4)+ NIADDR(1,1)+ NIADWT(1,1)
        UN(1)= UN(1)+ NIADDR(3,2)+ NIADWT(3,2)
        UN(3)= UN(3)+ NIADDR(2,2)+ NIADWT(2,2)
        UN(4)= UN(4)+ NIADDR(1,2)+ NIADWT(1,2)
C
        IF (SOSED.GT.0.0) THEN
C         there is sediment/soil being eroded from the land surface
C
C         determine the fraction of nitrogen in the
C         surface layer storage being removed on/with sediment
          CALL SDFRAC (SOSED,SLME,LSNO,DATIM,MESSU,MSGFL,
     M                 PSWCNT(1),PSWCNT(2),
     O                 FSD)
C
C         transport particulate labile organic nitrogen with/on sediment
          CALL SEDMOV (FSD,
     M                 SN(1),
     O                 SEDN(1))
C
C         transport adsorbed ammonium with/on sediment
          CALL SEDMOV (FSD,
     M                 SN(2),
     O                 SEDN(2))
C
C         transport particulate refractory organic nitrogen with/on sediment
          CALL SEDMOV (FSD,
     M                 SN(7),
     O                 SEDN(3))
        ELSE
C         there is no sediment/soil being eroded from the land
C         surface so zero fluxes
          SEDN(1)= 0.0
          SEDN(2)= 0.0
          SEDN(3)= 0.0
C
        END IF
C
C       move solution ammonium with water in the topsoil layers
        CALL TOPMOV (FRAC,
     M               SN(3),UN(3),IN(1),
     O               TSAMS)
C
C       move solution nitrate with water in the topsoil layers
        CALL TOPMOV (FRAC,
     M               SN(4),UN(4),IN(2),
     O               TSNO3)
C
C       move solution labile organic n with water in the topsoil layers
        CALL TOPMOV (FRAC,
     M               SN(6),UN(6),IN(3),
     O               TSSLN)
C
C       move solution refractory organic n with water in the topsoil layers
        CALL TOPMOV (FRAC,
     M               SN(8),UN(8),IN(4),
     O               TSSRN)
c  ! pw adding
c         do jpw = 1, 4
c            pwsoildd(jpw) = wiltpt(jpw) / pwsoild(jpw)   
c         enddo   
c  ! pw added !!!!!!!!!!!!!!!!!!!!
C
C       perform reactions on nitrogen in the surface layer storage
        CALL NITRXN (LSNO,MSGFL,DATIM,MESSU,ITMAXA,GNPM,BRXNFG,CRXNFG,
     I               FORAFG,SLTMP,MST(1),SLSM,SNPM,LAYID(1),SKPLN,
     I               NUPTFG,FIXNFG,ALPNFG,AMVOFG,SNUPTG,NMXRAT,
     I               WILTPT(1),SORNPM,SKVOL,THVOL,TRFVOL,SKSATN,SCSATN,
     I               KRETBN(1),KRETBN(5),ANUTF(1),SURS,
     M               SNDFC,NWCNT,NECNT,AGPLTN,SN,SNRXF   ! )
     x             , pwsoild(1), uunits,fieldcp(1))    ! pw 
C
C       perform reactions on nitrogen in the upper layer
C       principal storage
        CALL NITRXN (LSNO,MSGFL,DATIM,MESSU,ITMAXA,GNPM,BRXNFG,CRXNFG,
     I               FORAFG,ULTMP,MST(2),ULSM,UNPM,LAYID(2),UKPLN,
     I               NUPTFG,FIXNFG,ALPNFG,AMVOFG,UNUPTG,NMXRAT,
     I               WILTPT(2),UORNPM,UKVOL,THVOL,TRFVOL,UKSATN,UCSATN,
     I               KRETBN(2),KRETBN(5),ANUTF(2),UZS,
     M               UNDFC,NWCNT,NECNT,AGPLTN,UN,UNRXF    ! )
     x             , pwsoild(2), uunits,fieldcp(2))    ! pw 
C
      ELSE
C       surface and upper layers of the land segment have
C       been subdivided into blocks
C       initialize segment-wide variables
C
        DO 50 J= 1,3
          SEDN(J)= 0.0
 50     CONTINUE
C
        DO 60 J= 1,5
          TSAMS(J)= 0.0
          TSNO3(J)= 0.0
          TSSLN(J)= 0.0
          TSSRN(J)= 0.0
 60     CONTINUE
C
        DO 65 J= 1,8
          SN(J)= 0.0
          UN(J)= 0.0
 65     CONTINUE
C
        DO 70 J= 1,4
          IN(J)= 0.0
 70     CONTINUE
C
        DO 75 J= 1, 16
          SNRXF(J)= 0.0
          UNRXF(J)= 0.0
 75     CONTINUE
C
        DO 130 I= 1,NBLKS
C
C         update storages for atmospheric deposition
          SNB(1,I)= SNB(1,I)+ (NIADDR(3,1)+ NIADWT(3,1))*NBLKSI
          SNB(3,I)= SNB(3,I)+ (NIADDR(2,1)+ NIADWT(2,1))*NBLKSI
          SNB(4,I)= SNB(4,I)+ (NIADDR(1,1)+ NIADWT(1,1))*NBLKSI
          UNB(1,I)= UNB(1,I)+ (NIADDR(3,2)+ NIADWT(3,2))*NBLKSI
          UNB(3,I)= UNB(3,I)+ (NIADDR(2,2)+ NIADWT(2,2))*NBLKSI
          UNB(4,I)= UNB(4,I)+ (NIADDR(1,2)+ NIADWT(1,2))*NBLKSI
C
C
          IF (SOSDB(I).GT.0.0) THEN
C           determine the fraction of nitrogen in the surface layer
C           storage being removed on/with sediment for this block
            CALL SDFRAC (SOSDB(I),SLME,LSNO,DATIM,MESSU,MSGFL,
     M                   PSWCNT(1),PSWCNT(2),
     O                   FSD)
C
C           transport particulate labile organic nitrogen with/on sediment
            CALL SEDMOV (FSD,
     M                   SNB(1,I),
     O                   SDNB(1))
C
C           transport adsorbed ammonium with/on sediment
            CALL SEDMOV (FSD,
     M                   SNB(2,I),
     O                   SDNB(2))
C
C           transport particulate refractory organic nitrogen with/on sediment
            CALL SEDMOV (FSD,
     M                   SNB(7,I),
     O                   SDNB(3))
C
C           cumulate block fluxes
            DO 80 J= 1,3
              SEDN(J)= SEDN(J)+ SDNB(J)
 80         CONTINUE
          ELSE
C           there is no sediment/soil being eroded from the land
C           surface from this block so zero fluxes
            DO 90 J= 1,3
              SDNB(J)= 0.0
 90         CONTINUE
C
          END IF
C
C         transport solution ammonium in the topsoil layers
          CALL TOPMOV (FRACB(1,I),
     M                 SNB(3,I),UNB(3,I),INB(1,I),
     O                 TSAMSB)
C
C         transport nitrate in the topsoil layers
          CALL TOPMOV (FRACB(1,I),
     M                 SNB(4,I),UNB(4,I),INB(2,I),
     O                 TSNO3B)
C
C         transport solution labile organic n in the topsoil layers
          CALL TOPMOV (FRACB(1,I),
     M                 SNB(6,I),UNB(6,I),INB(3,I),
     O                 TSSLNB)
C
C         transport solution refractory organic n in the topsoil layers
          CALL TOPMOV (FRACB(1,I),
     M                 SNB(8,I),UNB(8,I),INB(4,I),
     O                 TSSRNB)
C
C         cumulate block fluxes
          DO 100 J= 1,5
            TSAMS(J)= TSAMS(J)+ TSAMSB(J)
            TSNO3(J)= TSNO3(J)+ TSNO3B(J)
            TSSLN(J)= TSSLN(J)+ TSSLNB(J)
            TSSRN(J)= TSSRN(J)+ TSSRNB(J)
 100      CONTINUE
C
C         perform reactions on nitrogen in the surface layer storage
          CALL NITRXN (LSNO,MSGFL,DATIM,MESSU,ITMAXA,GNPM,BRXNFG,CRXNFG,
     I                 FORAFG,SLTMP,MSTB(1,I),SLSM,SNPM,LAYID(1),SKPLN,
     I                 NUPTFG,FIXNFG,ALPNFG,AMVOFG,SNUPTG,NMXRAT,
     I                 WILTPT(1),SORNPM,SKVOL,THVOL,TRFVOL,SKSATN,
     I                 SCSATN,KRETBN(1),KRETBN(5),ANUTF(1),SURSB(I),
     M                 SNDFC,NWCNT,NECNT,AGPLTN,SNB(1,I),SNRXFB(1,I)  ! )
     x             , pwsoild(1), uunits,fieldcp(1))    ! pw 
C
C         perform reactions on nitrogen in the upper layer
C         principal storage
          CALL NITRXN (LSNO,MSGFL,DATIM,MESSU,ITMAXA,GNPM,BRXNFG,CRXNFG,
     I                 FORAFG,ULTMP,MSTB(2,I),ULSM,UNPM,LAYID(2),UKPLN,
     I                 NUPTFG,FIXNFG,ALPNFG,AMVOFG,UNUPTG,NMXRAT,
     I                 WILTPT(2),UORNPM,UKVOL,THVOL,TRFVOL,UKSATN,
     I                 UCSATN,KRETBN(2),KRETBN(5),ANUTF(2),UZSB(I),
     M                 UNDFC,NWCNT,NECNT,AGPLTN,UNB(1,I),UNRXFB(1,I)  ! )
     x             , pwsoild(2), uunits,fieldcp(2))    ! pw 
C
C         cumulate block storages
          DO 110 J= 1,8
            SN(J)= SN(J)+ SNB(J,I)
            UN(J)= UN(J)+ UNB(J,I)
 110      CONTINUE
C
C         cumulate block storages in the upper layer
C         transitory (interflow) storage
          DO 120 J= 1,4
            IN(J)= IN(J)+ INB(J,I)
 120      CONTINUE
C
C         cumulate block fluxes
          DO 125 J= 1, 16
            SNRXF(J)= SNRXF(J)+ SNRXFB(J,I)
            UNRXF(J)= UNRXF(J)+ UNRXFB(J,I)
 125      CONTINUE
C
 130    CONTINUE
C
C       average sum of block storages and fluxes for the surface
C       and upper layer to get segment-wide values
C
        DO 140 J= 1,3
          SEDN(J)= SEDN(J)*NBLKSI
 140    CONTINUE
C
        DO 150 J= 1,5
          TSAMS(J)= TSAMS(J)*NBLKSI
          TSNO3(J)= TSNO3(J)*NBLKSI
          TSSLN(J)= TSSLN(J)*NBLKSI
          TSSRN(J)= TSSRN(J)*NBLKSI
 150    CONTINUE
C
        DO 155 J= 1,8
          SN(J)= SN(J)*NBLKSI
          UN(J)= UN(J)*NBLKSI
 155    CONTINUE
C
        DO 160 J= 1,4
          IN(J)= IN(J)*NBLKSI
 160    CONTINUE
C
        DO 165 J= 1, 16
          SNRXF(J)= SNRXF(J)*NBLKSI
          UNRXF(J)= UNRXF(J)*NBLKSI
 165    CONTINUE
C
      END IF
C
C     transport solution ammonium in the subsurface layers
      CALL SUBMOV (TSAMS(3),FRAC(6),FRAC(7),FRAC(8),
     M             LN(3),AN(3),
     O             SSAMS)
C
C     transport solution nitrate in the subsurface layers
      CALL SUBMOV (TSNO3(3),FRAC(6),FRAC(7),FRAC(8),
     M             LN(4),AN(4),
     O             SSNO3)
C
C     transport solution labile organic nitrogen in the subsurface layers
      CALL SUBMOV (TSSLN(3),FRAC(6),FRAC(7),FRAC(8),
     M             LN(6),AN(6),
     O             SSSLN)
C
C     transport solution refractory organic nitrogen in the subsurface layers
      CALL SUBMOV (TSSRN(3),FRAC(6),FRAC(7),FRAC(8),
     M             LN(8),AN(8),
     O             SSSRN)
C
C     perform reactions on nitrogen in the lower layer storage
      CALL NITRXN (LSNO,MSGFL,DATIM,MESSU,ITMAXA,GNPM,BRXNFG,CRXNFG,
     I             FORAFG,LGTMP,MST(4),LLSM,LNPM,LAYID(3),LKPLN,
     I             NUPTFG,FIXNFG,ALPNFG,AMVOFG,LNUPTG,NMXRAT,WILTPT(3),
     I             LORNPM,LKVOL,THVOL,TRFVOL,LKSATN,LCSATN,KRETBN(3),
     I             KRETBN(5),ANUTF(3),LZS,
     M             LNDFC,NWCNT,NECNT,AGPLTN,LN,LNRXF   ! )
     x             , pwsoild(3), uunits,fieldcp(3))    ! pw 
C
C     perform reactions on nitrogen in the active
C     groundwater storage
C     the calculation of mst(5) in section mstlay results in a
C     non-zero value when frac(8)=1.0.  this causes adsorption
C     calculations by sv to compute a negative nh3 storage;
C     moistm is a dummy soil moisture which is set to 0.0 if
C     frac(8)= 1.0.
      MOISTM = MST(5)
      IF (FRAC(8).GE.1.0) MOISTM= 0.0
C
      CALL NITRXN (LSNO,MSGFL,DATIM,MESSU,ITMAXA,GNPM,BRXNFG,CRXNFG,
     I             FORAFG,LGTMP,MOISTM,ALSM,ANPM,LAYID(4),AKPLN,
     I             NUPTFG,FIXNFG,ALPNFG,AMVOFG,ANUPTG,NMXRAT,WILTPT(4),
     I             AORNPM,AKVOL,THVOL,TRFVOL,AKSATN,ACSATN,KRETBN(4),
     I             KRETBN(5),ANUTF(4),AGWS,
     M             ANDFC,NWCNT,NECNT,AGPLTN,AN,ANRXF   ! )
     x             , pwsoild(4), uunits,fieldcp(4))    ! pw 
C
C     find total nitrogen outflows due to overland flow erosion
      SOSEDN= SEDN(1)+ SEDN(2)+ SEDN(3)
C
C     find total outflows from the pervious land segment
      PONO3 = TSNO3(1)+ TSNO3(5)+ SSNO3(3)
      PONH4 = TSAMS(1)+ TSAMS(5)+ SSAMS(3)+ SEDN(2)
      POORN = TSSLN(1)+ TSSLN(5)+ SSSLN(3)+ SEDN(1)+
     $        TSSRN(1)+ TSSRN(5)+ SSSRN(3)+ SEDN(3)
      PONITR= PONO3+ PONH4+ POORN
C
C     store reaction fluxes for printout
      ORNMN(1)= SNRXF(3)
      AMIMB(1)= SNRXF(4)
      AMUPB(1)= SNRXF(5)
      NIIMB(1)= SNRXF(6)
      NIUPB(1)= SNRXF(7)
      AMNIT(1)= SNRXF(8)
      DENIF(1)= SNRXF(9)
      AMVOL(1)= SNRXF(10)
      REFRON(1)= SNRXF(11)
      RTLBN(1)= SNRXF(12)
      RTRBN(1)= SNRXF(13)
      AMUPA(1)= SNRXF(14)
      NIUPA(1)= SNRXF(15)
      NFIXFX(1)= SNRXF(16)
      ORNMN(2)= UNRXF(3)
      AMIMB(2)= UNRXF(4)
      AMUPB(2)= UNRXF(5)
      NIIMB(2)= UNRXF(6)
      NIUPB(2)= UNRXF(7)
      AMNIT(2)= UNRXF(8)
      DENIF(2)= UNRXF(9)
      AMVOL(2)= UNRXF(10)
      REFRON(2)= UNRXF(11)
      RTLBN(2)= UNRXF(12)
      RTRBN(2)= UNRXF(13)
      AMUPA(2)= UNRXF(14)
      NIUPA(2)= UNRXF(15)
      NFIXFX(2)= UNRXF(16)
      ORNMN(3)= LNRXF(3)
      AMIMB(3)= LNRXF(4)
      AMUPB(3)= LNRXF(5)
      NIIMB(3)= LNRXF(6)
      NIUPB(3)= LNRXF(7)
      AMNIT(3)= LNRXF(8)
      DENIF(3)= LNRXF(9)
      AMVOL(3)= LNRXF(10)
      REFRON(3)= LNRXF(11)
      RTLBN(3)= LNRXF(12)
      RTRBN(3)= LNRXF(13)
      AMUPA(3)= LNRXF(14)
      NIUPA(3)= LNRXF(15)
      NFIXFX(3)= LNRXF(16)
      ORNMN(4)= ANRXF(3)
      AMIMB(4)= ANRXF(4)
      AMUPB(4)= ANRXF(5)
      NIIMB(4)= ANRXF(6)
      NIUPB(4)= ANRXF(7)
      AMNIT(4)= ANRXF(8)
      DENIF(4)= ANRXF(9)
      AMVOL(4)= ANRXF(10)
      REFRON(4)= ANRXF(11)
      RTLBN(4)= ANRXF(12)
      RTRBN(4)= ANRXF(13)
      AMUPA(4)= ANRXF(14)
      NIUPA(4)= ANRXF(15)
      NFIXFX(4)= ANRXF(16)
      ORNMN(5)= ORNMN(1)+ ORNMN(2)+ ORNMN(3)+ ORNMN(4)
      AMIMB(5)= AMIMB(1)+ AMIMB(2)+ AMIMB(3)+ AMIMB(4)
      AMUPB(5)= AMUPB(1)+ AMUPB(2)+ AMUPB(3)+ AMUPB(4)
      NIIMB(5)= NIIMB(1)+ NIIMB(2)+ NIIMB(3)+ NIIMB(4)
      NIUPB(5)= NIUPB(1)+ NIUPB(2)+ NIUPB(3)+ NIUPB(4)
      AMNIT(5)= AMNIT(1)+ AMNIT(2)+ AMNIT(3)+ AMNIT(4)
      DENIF(5)= DENIF(1)+ DENIF(2)+ DENIF(3)+ DENIF(4)
      AMVOL(5)= AMVOL(1)+ AMVOL(2)+ AMVOL(3)+ AMVOL(4)
      REFRON(5)= REFRON(1)+ REFRON(2)+ REFRON(3)+ REFRON(4)
      RTLBN(5)= RTLBN(1)+ RTLBN(2)+ RTLBN(3)+ RTLBN(4)
      RTRBN(5)= RTRBN(1)+ RTRBN(2)+ RTRBN(3)+ RTRBN(4)
      AMUPA(5)= AMUPA(1)+ AMUPA(2)+ AMUPA(3)+ AMUPA(4)
      NIUPA(5)= NIUPA(1)+ NIUPA(2)+ NIUPA(3)+ NIUPA(4)
      NFIXFX(5)= NFIXFX(1)+ NFIXFX(2)+ NFIXFX(3)+ NFIXFX(4)
C
C     find the totals of nitrogen in soil storage
      TN(1)= SN(1)+ UN(1)+ LN(1)+ AN(1)
      TN(2)= SN(2)+ UN(2)+ LN(2)+ AN(2)
      TN(3)= SN(3)+ UN(3)+ IN(1)+ LN(3)+ AN(3)
      TN(4)= SN(4)+ UN(4)+ IN(2)+ LN(4)+ AN(4)
      TN(6)= SN(6)+ UN(6)+ IN(3)+ LN(6)+ AN(6)
      TN(7)= SN(7)+ UN(7)+ LN(7)+ AN(7)
      TN(8)= SN(8)+ UN(8)+ IN(4)+ LN(8)+ AN(8)
C
C     find the total nitrogen in plant storage
      TN(5)= SN(5)+ UN(5)+ LN(5)+ AN(5)
      IF (ALPNFG .EQ. 1) THEN
C       above-ground compartments being simulated
        TN(5)= TN(5)+ AGPLTN+ LITTRN
      END IF
C
      IF (NUPTFG .EQ. 1) THEN
C       find the total N uptake deficit
        TNDFC= SNDFC+ UNDFC+ LNDFC+ ANDFC
      END IF
C
C     total nitrogen in storage
      TOTNIT= TN(1)+ TN(2)+ TN(3)+ TN(4)+ TN(5)+ TN(6)+ TN(7)+ TN(8)
C
      RETURN
      END
C
C
C
      SUBROUTINE   NITACC
     I                   (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section nitr
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION NITR2 + + +
      INCLUDE    'cplni.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I1,I3,I5,I,J
      REAL       R0
C
C     + + + EXTERNALS + + +
      EXTERNAL   ACCVEC,SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      I3= 3
      I5= 5
      R0= 0.0
C
      CALL ACCVEC (I3,NITIF(1,FRMROW),
     M             NITIF(1,TOROW))
      IF (TOROW .EQ. 3) THEN
C       reset current value of special-action accumulators
        CALL SETVEC (I3,R0,
     O               NITIF(1,1))
      END IF
C
      CALL ACCVEC (I3,NCFX1(1,FRMROW),
     M             NCFX1(1,TOROW))
C
      CALL ACCVEC (I5,NCFX2(1,FRMROW),
     M             NCFX2(1,TOROW))
C
      CALL ACCVEC (I3,NCFX3(1,FRMROW),
     M             NCFX3(1,TOROW))
C
      CALL ACCVEC (I5,NCFX4(1,FRMROW),
     M             NCFX4(1,TOROW))
C
      CALL ACCVEC (I3,NCFX5(1,FRMROW),
     M             NCFX5(1,TOROW))
C
      CALL ACCVEC (I5,NCFX6(1,FRMROW),
     M             NCFX6(1,TOROW))
C
      CALL ACCVEC (I5,NCFX7(1,FRMROW),
     M             NCFX7(1,TOROW))
C
      CALL ACCVEC (I5,NCFX8(1,FRMROW),
     M             NCFX8(1,TOROW))
C
      CALL ACCVEC (I5,NCFX9(1,FRMROW),
     M             NCFX9(1,TOROW))
C
      DO 20 J= 1, 3
        DO 10 I= 1, 2
          NCFX10(J,I,TOROW)= NCFX10(J,I,TOROW)+ NCFX10(J,I,FRMROW)
          NCFX11(J,I,TOROW)= NCFX11(J,I,TOROW)+ NCFX11(J,I,FRMROW)
 10     CONTINUE
 20   CONTINUE
C
      CALL ACCVEC (I5,NCFX12(1,FRMROW),
     M             NCFX12(1,TOROW))
C
      CALL ACCVEC (I5,NCFX13(1,FRMROW),
     M             NCFX13(1,TOROW))
C
      CALL ACCVEC (I3,NCFX14(1,FRMROW),
     M             NCFX14(1,TOROW))
C
      CALL ACCVEC (I5,NCFX15(1,FRMROW),
     M             NCFX15(1,TOROW))
C
      CALL ACCVEC (I3,NCFX16(1,FRMROW),
     M             NCFX16(1,TOROW))
C
      CALL ACCVEC (I5,NCFX17(1,FRMROW),
     M             NCFX17(1,TOROW))
C
      CALL ACCVEC (I5,NCFX18(1,FRMROW),
     M             NCFX18(1,TOROW))
C
      CALL ACCVEC (I5,NCFX19(1,FRMROW),
     M             NCFX19(1,TOROW))
C
      CALL ACCVEC (I5,NCFX20(1,FRMROW),
     M             NCFX20(1,TOROW))
C
      CALL ACCVEC (I5,NCFX21(1,FRMROW),
     M             NCFX21(1,TOROW))
C
      CALL ACCVEC (I5,NCFX22(1,FRMROW),
     M             NCFX22(1,TOROW))
C
      CALL ACCVEC (I5,NCFX23(1,FRMROW),
     M             NCFX23(1,TOROW))
C
      CALL ACCVEC (I1,NCFX24(1,FRMROW),
     M             NCFX24(1,TOROW))
C
      CALL ACCVEC (I3,NCFX25(1,FRMROW),
     M             NCFX25(1,TOROW))
C
      CALL ACCVEC (I3,NCFX26(1,FRMROW),
     M             NCFX26(1,TOROW))
C
      CALL ACCVEC (I5,NCFX27(1,FRMROW),
     M             NCFX27(1,TOROW))
C
      CALL ACCVEC (I5,NCFX28(1,FRMROW),
     M             NCFX28(1,TOROW))
C
      RETURN
      END
C
C
C
      SUBROUTINE   NITPRT
     I                   (LEV,PRINTU,AGMAID,MFACTA,MFACTB,UNITFG)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units, and
C     produce printout
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEV,PRINTU,UNITFG
      REAL        MFACTA,MFACTB
      CHARACTER*8 AGMAID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C     AGMAID - ???
C     MFACTA - ???
C     MFACTB - ???
C     UNITFG - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION NITR2 + + +
      INCLUDE    'cplni.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I1,I3,I4,I5,I8,I,J,N,ADFG
      REAL        MATIN,MATDIF,NSTOR,NSTORS,PSTATA(1),PSTAT(8),
     $            PSTATI(4),TOTAL,PDEFCT(5),PADTOT(3,2),PADALL,
     $            PIFLX(3),PPONH4,PPONIT,PPONO3,PSOSD,PPOORN,PCFLX1(3),
     $            PCFLX2(5),PCFLX3(3),PCFLX4(5),PCFLX5(3),PCFLX6(5),
     $            PCFLX7(5),PCFLX8(5),PCFLX9(5),PCFL10(3,2),
     $            PCFL11(3,2),PCFL12(5),PCFL13(5),PCFL14(3),PCFL15(5),
     $            PCFL16(3),PCFL17(5),PCFL18(5),PCFL19(5),PCFL20(5),
     $            PCFL21(5),PCFL22(5),PCFL23(5),PCFL24(1),PCFL25(3),
     $            PCFL26(3),PCFL27(5),PCFL28(5)
      CHARACTER*8 UNITID
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,BALCHK
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** NITR ***')
 2010 FORMAT (/,'   SEGMENT WIDE VALUES:')
 2020 FORMAT (  '   STATE VARIABLES   ',A8)
 2030 FORMAT (/,5X,'STORAGES BY LAYER',40X,'SOL LABIL ADS LABIL  SOL',
     $             ' REFR  ADS REFR')
 2040 FORMAT (  31X,' NH4-N SOL NH4-N ADS   NO3/2-N ORGANIC N',
     $              ' ORGANIC N ORGANIC N ORGANIC N   PLANT N',13X,
     $              'TOTAL N')
 2050 FORMAT (/,7X,'ABOVE-GROUND PLANT',77X,2F10.3)
 2060 FORMAT (  7X,'LITTER',89X,2F10.3)
 2070 FORMAT (/,7X,'SURFACE LAYER',11X,8F10.3,10X,F10.3)
 2080 FORMAT (  7X,'UPPER PRINCIPAL',9X,8F10.3,10X,F10.3)
 2090 FORMAT (  7X,'UPPER TRANSITORY(INTER) ',F10.3,10X,2F10.3,10X,
     $              F10.3,30X,F10.3)
 2100 FORMAT (  7X,'LOWER LAYER',13X,8F10.3,10X,F10.3)
 2110 FORMAT (  7X,'ACTIVE GROUNDWATER',6X,8F10.3,10X,F10.3)
 2120 FORMAT (/,5X,'TOTALS',20X,8F10.3,10X,F10.3)
 2130 FORMAT (/,5X,'UPTAKE DEFICITS BY LAYER',15X,'SURFACE',10X,
     $          'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2140 FORMAT (  46X,'SNDFC',15X,'UNDFC',15X,'LNDFC',15X,'ANDFC',15X,
     $              'TNDFC')
 2150 FORMAT (  31X,5(10X,1PE10.3))
 2160 FORMAT (/,'   FLUXES',12X,A8)
 2170 FORMAT (/,'     ATMOSPHERIC DEPOSITION    <-------SURFACE',
     #          ' LAYER--------><------UPPER LAYER PRIN------>')
 2180 FORMAT (  31X,'       DRY       WET     TOTAL       DRY',
     #          '       WET     TOTAL')
 2190 FORMAT (  7X,'NH4-N',19X,6(1PE10.3))
 2200 FORMAT (  7X,'NO3-N',19X,6(1PE10.3))
 2210 FORMAT (  7X,'ORGN',20X,6(1PE10.3))
 2220 FORMAT (/,5X,'APPLICATIONS',14X,'   AMMONIA   NITRATE ORGANIC N',
     $             '  NITROGEN')
 2230 FORMAT (  31X,'      INH3      INO3      IORN ALL FORMS')
 2240 FORMAT (  31X,4F10.3)
 2250 FORMAT (/,'     FLOWS OF N IN SOLUTION    <--SURFACE LAYER--->',
     $          '<-UPPER LAYER PRIN-> INTERFLOW',10X,
     $          '<---LOWER LAYER---->',9X,'GROUNDWATER')
 2260 FORMAT (  31X,'   OUTFLOW      PERC      PERC  TO TRANS',
     $          '   OUTFLOW',16X,'PERC DEEP PERC',13X,'OUTFLOW')
 2270 FORMAT (  7X,'NH4-N IN SOLUTION',7X,5(1PE10.3),10X,
     $          2(1PE10.3),10X,(1PE10.3))
 2280 FORMAT (  7X,'NO3(+NO2)-N',13X,5(1PE10.3),10X,
     $          2(1PE10.3),10X,(1PE10.3))
 2290 FORMAT (  7X,'LABILE ORGN',13X,5(1PE10.3),10X,
     $          2(1PE10.3),10X,(1PE10.3))
 2300 FORMAT (  7X,'REFRAC ORGN',13X,5(1PE10.3),10X,
     $          2(1PE10.3),10X,(1PE10.3))
 2310 FORMAT (/,'     OTHER OUTFLOWS',13X,'<-----SEDIMENT ASSOCIATED',
     $          ' OUTFLOW----->',10X,'<------------TOTAL OUTFLOW',
     $          '------------->')
 2320 FORMAT (  31X,'              LABILE    REFRAC')
 2330 FORMAT (  31X,' NH4-N ADS ORGANIC N ORGANIC N     TOTAL',15X,
     $          'NH4-N     NO3-N      ORGN  NITROGEN')
 2340 FORMAT (  31X,'     SDAMA    SDLORN    SDRORN    SOSEDN',15X,
     $          'PONH4     PONO3     POORN ALL FORMS')
 2350 FORMAT (  31X,4(1PE10.3),10X,4(1PE10.3))
 2360 FORMAT (/,'  ABOVE-GROUND PLANT N RETURN      RETAGN')
 2370 FORMAT (  31X,1PE10.3)
 2380 FORMAT (/,'        LITTER PLANT N RETURN',15X,'SURFACE',10X,
     $          'UPPER PRIN',55X,'TOTAL')
 2390 FORMAT (  '               TO LABILE ORGN',16X,'SRTLLN',14X,
     $          'URTLLN',54X,'TRTLLN')
 2400 FORMAT (  41X,1PE10.3,10X,1PE10.3,50X,1PE10.3)
 2410 FORMAT (  '               TO REFRAC ORGN',16X,'SRTRLN',14X,
     $          'URTRLN',54X,'TRTRLN')
 2420 FORMAT (/,'  BELOW-GROUND PLANT N RETURN',15X,'SURFACE',10X,
     $          'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2430 FORMAT (  '               TO LABILE ORGN',16X,'SRTLBN',14X,
     $          'URTLBN',14X,'LRTLBN',14X,'ARTLBN',14X,'TRTLBN')
 2440 FORMAT (  '               TO REFRAC ORGN',16X,'SRTRBN',14X,
     $          'URTRBN',14X,'LRTRBN',14X,'ARTRBN',14X,'TRTRBN')
 2450 FORMAT (/,'  ABOVE-GRND NH3 PLANT UPTAKE',15X,'SURFACE',10X,
     $          'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2460 FORMAT (  45X,'SAMUPA',14X,'UAMUPA',14X,'LAMUPA',14X,'AAMUPA',
     $          14X,'TAMUPA')
 2470 FORMAT (/,'  BELOW-GRND NH3 PLANT UPTAKE',15X,'SURFACE',10X,
     $          'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2480 FORMAT (  45X,'SAMUPB',14X,'UAMUPB',14X,'LAMUPB',14X,'AAMUPB',
     $          14X,'TAMUPB')
 2490 FORMAT (/,'  ABOVE-GRND NO3 PLANT UPTAKE',15X,'SURFACE',10X,
     $          'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2500 FORMAT (  45X,'SNIUPA',14X,'UNIUPA',14X,'LNIUPA',14X,'ANIUPA',
     $          14X,'TNIUPA')
 2510 FORMAT (/,'  BELOW-GRND NO3 PLANT UPTAKE',15X,'SURFACE',10X,
     $          'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2520 FORMAT (  45X,'SNIUPB',14X,'UNIUPB',14X,'LNIUPB',14X,'ANIUPB',
     $          14X,'TNIUPB')
 2530 FORMAT (/,'              DENITRIFICATION',15X,'SURFACE',10X,
     $          'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2540 FORMAT (  46X,'SDENI',15X,'UDENI',15X,'LDENI',15X,'ADENI',
     $          15X,'TDENI')
 2550 FORMAT (/,'            NH3 NITRIFICATION',15X,'SURFACE',10X,
     $          'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2560 FORMAT (  45X,'SAMNIT',14X,'UAMNIT',14X,'LAMNIT',14X,'AAMNIT',
     $          14X,'TAMNIT')
 2570 FORMAT (/,'           NH3 IMMOBILIZATION',15X,'SURFACE',10X,
     $          'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2580 FORMAT (  45X,'SAMIMB',14X,'UAMIMB',14X,'LAMIMB',14X,'AAMIMB',
     $          14X,'TAMIMB')
 2590 FORMAT (/,'          ORGN MINERALIZATION',15X,'SURFACE',10X,
     $          'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2600 FORMAT (  45X,'SORNMN',14X,'UORNMN',14X,'LORNMN',14X,'AORNMN',
     $          14X,'TORNMN')
 2610 FORMAT (/,'           NO3 IMMOBILIZATION',15X,'SURFACE',10X,
     $          'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2620 FORMAT (  45X,'SNIIMB',14X,'UNIIMB',14X,'LNIIMB',14X,'ANIIMB',
     $          14X,'TNIIMB')
 2630 FORMAT (/,'  LABILE/REFR ORGN CONVERSION',15X,'SURFACE',10X,
     $          'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2640 FORMAT (  45X,'SREFON',14X,'UREFON',14X,'LREFON',14X,'AREFON',
     $          14X,'TREFON')
 2650 FORMAT (/,'            NITROGEN FIXATION',15X,'SURFACE',10X,
     $          'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2660 FORMAT (  45X,' SFIXN',14X,' UFIXN',14X,' LFIXN',14X,' AFIXN',
     $          14X,' TFIXN')
 2670 FORMAT (/,'           NH3 VOLATILIZATION',15X,'SURFACE',10X,
     $          'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2680 FORMAT (  45X,'SAMVOL',14X,'UAMVOL',14X,'LAMVOL',14X,'AAMVOL',
     $          14X,'TAMVOL')
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      I3= 3
      I4= 4
      I5= 5
      I8= 8
C
C     print section header
      WRITE (PRINTU,2000)
C
C     state variables
C
C     print heading
      WRITE (PRINTU,2010)
      WRITE (PRINTU,2020)  AGMAID
      WRITE (PRINTU,2030)
      WRITE (PRINTU,2040)
C
      IF (ALPNFG .EQ. 1) THEN
C       above-ground and litter compartments simulated
C
C       above-ground layer
        CALL TRNVEC (I1,AGPLTN,MFACTA,MFACTB,
     O               PSTATA)
        WRITE (PRINTU,2050)  PSTATA
C
C       litter layer
        CALL TRNVEC (I1,LITTRN,MFACTA,MFACTB,
     O               PSTATA)
        WRITE (PRINTU,2060)  PSTATA
      END IF
C
C     surface layer
      CALL TRNVEC (I8,SN,MFACTA,MFACTB,
     O             PSTAT)
      TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)+ PSTAT(4)+ PSTAT(5)+
     $                 PSTAT(6)+ PSTAT(7)+ PSTAT(8)
      WRITE (PRINTU,2070) PSTAT(3),PSTAT(2),PSTAT(4),PSTAT(6),PSTAT(1),
     $                    PSTAT(8),PSTAT(7),PSTAT(5),TOTAL
C
C     upper layer
      CALL TRNVEC (I8,UN,MFACTA,MFACTB,
     O             PSTAT)
      TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)+ PSTAT(4)+ PSTAT(5)+
     $                 PSTAT(6)+ PSTAT(7)+ PSTAT(8)
      WRITE (PRINTU,2080) PSTAT(3),PSTAT(2),PSTAT(4),PSTAT(6),PSTAT(1),
     $                    PSTAT(8),PSTAT(7),PSTAT(5),TOTAL
C
C     interflow layer
      CALL TRNVEC (I4,IN,MFACTA,MFACTB,
     O             PSTATI)
      TOTAL= PSTATI(1)+ PSTATI(2)+ PSTATI(3)+ PSTATI(4)
      WRITE (PRINTU,2090)  PSTATI, TOTAL
C
C     lower layer
      CALL TRNVEC (I8,LN,MFACTA,MFACTB,
     O             PSTAT)
      TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)+ PSTAT(4)+ PSTAT(5)+
     $                 PSTAT(6)+ PSTAT(7)+ PSTAT(8)
      WRITE (PRINTU,2100) PSTAT(3),PSTAT(2),PSTAT(4),PSTAT(6),PSTAT(1),
     $                    PSTAT(8),PSTAT(7),PSTAT(5),TOTAL
C
C     groundwater layer
      CALL TRNVEC (I8,AN,MFACTA,MFACTB,
     O             PSTAT)
      TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)+ PSTAT(4)+ PSTAT(5)+
     $                 PSTAT(6)+ PSTAT(7)+ PSTAT(8)
      WRITE (PRINTU,2110) PSTAT(3),PSTAT(2),PSTAT(4),PSTAT(6),PSTAT(1),
     $                    PSTAT(8),PSTAT(7),PSTAT(5),TOTAL
C
C     total of all layers
      CALL TRNVEC (I8,TN,MFACTA,MFACTB,
     O             PSTAT)
      TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)+ PSTAT(4)+ PSTAT(5)+
     $                 PSTAT(6)+ PSTAT(7)+ PSTAT(8)
      WRITE (PRINTU,2120) PSTAT(3),PSTAT(2),PSTAT(4),PSTAT(6),PSTAT(1),
     $                    PSTAT(8),PSTAT(7),PSTAT(5),TOTAL
C
      IF (NUPTFG .EQ. 1) THEN
C       print out deficits from yield-based plant uptake algorithm
        WRITE (PRINTU,2130)
        WRITE (PRINTU,2140)
        CALL TRNVEC (I5,NDFCT,MFACTA,MFACTB,
     O               PDEFCT)
        WRITE (PRINTU,2150)  PDEFCT
      END IF
C
C     fluxes
C
C     print heading
      WRITE (PRINTU,2160)  AGMAID
C
C     atmospheric deposition fluxes
C
C     determine if needed
      ADFG= 0
      DO 20 J= 1, 3
        DO 10 I= 1, 2
          N= 4*(J-1)+ 2*(I-1)+ 1
          IF ( (NIADFG(N) .NE. 0) .OR. (NIADFG(N+1) .NE. 0) ) THEN
            ADFG= 1
          END IF
 10     CONTINUE
 20   CONTINUE
C
      PADALL= 0.0
      IF (ADFG .EQ. 1) THEN
C       print atmospheric deposition fluxes
C
C       print header
        WRITE (PRINTU,2170)
        WRITE (PRINTU,2180)
C
C       compute amounts
        DO 40 J= 1, 3
          DO 30 I= 1, 2
            N= 4*(J-1)+ 2*(I-1)+ 1
            IF ( (NIADFG(N) .NE. 0) .OR. (NIADFG(N+1) .NE. 0) ) THEN
              IF (NIADFG(N) .NE. 0) THEN
                PCFL10(J,I)= NCFX10(J,I,LEV)*MFACTA+ MFACTB
              ELSE
                PCFL10(J,I)= 0.0
              END IF
              IF (NIADFG(N+1) .NE. 0) THEN
                PCFL11(J,I)= NCFX11(J,I,LEV)*MFACTA+ MFACTB
              ELSE
                PCFL11(J,I)= 0.0
              END IF
              PADTOT(J,I)= PCFL10(J,I)+ PCFL11(J,I)
              PADALL= PADALL+ PADTOT(J,I)
            END IF
 30       CONTINUE
 40     CONTINUE
C
C       print fluxes
        WRITE (PRINTU,2190) PCFL10(2,1),PCFL11(2,1),PADTOT(2,1),
     #                      PCFL10(2,2),PCFL11(2,2),PADTOT(2,2)
        WRITE (PRINTU,2200) PCFL10(1,1),PCFL11(1,1),PADTOT(1,1),
     #                      PCFL10(1,2),PCFL11(1,2),PADTOT(1,2)
        WRITE (PRINTU,2210) PCFL10(3,1),PCFL11(3,1),PADTOT(3,1),
     #                      PCFL10(3,2),PCFL11(3,2),PADTOT(3,2)
C
      END IF
C
C     input application flux
      CALL TRNVEC (I3,NITIF(1,LEV),MFACTA,MFACTB,
     O             PIFLX)
      TOTAL= PIFLX(1)+ PIFLX(2)+ PIFLX(3)
      IF (TOTAL .GT. 0.0) THEN
C       print application amount
        WRITE (PRINTU,2220)
        WRITE (PRINTU,2230)
        WRITE (PRINTU,2240)  PIFLX(2), PIFLX(1), PIFLX(3), TOTAL
      END IF
C
C     solution fluxes
C
C     print header
      WRITE (PRINTU,2250)
      WRITE (PRINTU,2260)
C
C     solution ammonia
      CALL TRNVEC (I5,NCFX2(1,LEV),MFACTA,MFACTB,
     O             PCFLX2)
      CALL TRNVEC (I3,NCFX3(1,LEV),MFACTA,MFACTB,
     O             PCFLX3)
      WRITE (PRINTU,2270)  PCFLX2, PCFLX3
C
C     solution nitrate
      CALL TRNVEC (I5,NCFX4(1,LEV),MFACTA,MFACTB,
     O             PCFLX4)
      CALL TRNVEC (I3,NCFX5(1,LEV),MFACTA,MFACTB,
     O             PCFLX5)
      WRITE (PRINTU,2280)  PCFLX4, PCFLX5
C
C     solution labile organic nitrogen
      CALL TRNVEC (I5,NCFX13(1,LEV),MFACTA,MFACTB,
     O             PCFL13)
      CALL TRNVEC (I3,NCFX14(1,LEV),MFACTA,MFACTB,
     O             PCFL14)
      WRITE (PRINTU,2290)  PCFL13, PCFL14
C
C     solution refractory organic nitrogen
      CALL TRNVEC (I5,NCFX15(1,LEV),MFACTA,MFACTB,
     O             PCFL15)
      CALL TRNVEC (I3,NCFX16(1,LEV),MFACTA,MFACTB,
     O             PCFL16)
      WRITE (PRINTU,2300)  PCFL15, PCFL16
C
C     sediment and total fluxes
C
C     print header
      WRITE (PRINTU,2310)
      WRITE (PRINTU,2320)
      WRITE (PRINTU,2330)
      WRITE (PRINTU,2340)
C
C     print sediment and total fluxes
      CALL TRNVEC (I3,NCFX1(1,LEV),MFACTA,MFACTB,
     O             PCFLX1)
      PSOSD = PCFLX1(1)+ PCFLX1(2)+ PCFLX1(3)
      PPONH4= PCFLX1(2)+ PCFLX2(1)+ PCFLX2(5)+ PCFLX3(3)
      PPONO3= PCFLX4(1)+ PCFLX4(5)+ PCFLX5(3)
      PPOORN= PCFLX1(1)+ PCFL13(1)+ PCFL13(5)+ PCFL14(3)+
     $        PCFLX1(3)+ PCFL15(1)+ PCFL15(5)+ PCFL16(3)
      PPONIT= PPONH4+ PPONO3+ PPOORN
      WRITE (PRINTU,2350)  PCFLX1(2),PCFLX1(1),PCFLX1(3),PSOSD,PPONH4,
     $                     PPONO3,PPOORN,PPONIT
C
C     reaction fluxes
C
      IF (ALPNFG .EQ. 1) THEN
C       above-ground and litter layer simulated
C
C       above-ground plant return to litter
        WRITE (PRINTU,2360)
        CALL TRNVEC (I1,NCFX24(1,LEV),MFACTA,MFACTB,
     O               PCFL24)
        WRITE (PRINTU,2370)  PCFL24
C
C       litter return to labile organic n
        WRITE (PRINTU,2380)
        WRITE (PRINTU,2390)
        CALL TRNVEC (I3,NCFX25(1,LEV),MFACTA,MFACTB,
     O               PCFL25)
        WRITE (PRINTU,2400)  PCFL25
C
C       litter return to refractory organic n
        WRITE (PRINTU,2380)
        WRITE (PRINTU,2410)
        CALL TRNVEC (I3,NCFX26(1,LEV),MFACTA,MFACTB,
     O               PCFL26)
        WRITE (PRINTU,2400)  PCFL26
      END IF
C
C     below-ground plant return to labile organic n
      WRITE (PRINTU,2420)
      WRITE (PRINTU,2430)
      CALL TRNVEC (I5,NCFX27(1,LEV),MFACTA,MFACTB,
     O             PCFL27)
      WRITE (PRINTU,2150)  PCFL27
C
C     below-ground plant return to refractory organic n
      WRITE (PRINTU,2420)
      WRITE (PRINTU,2440)
      CALL TRNVEC (I5,NCFX28(1,LEV),MFACTA,MFACTB,
     O             PCFL28)
      WRITE (PRINTU,2150)  PCFL28
C
      IF (ALPNFG .EQ. 1) THEN
C       above-ground ammonia plant uptake
        WRITE (PRINTU,2450)
        WRITE (PRINTU,2460)
        CALL TRNVEC (I5,NCFX21(1,LEV),MFACTA,MFACTB,
     O               PCFL21)
        WRITE (PRINTU,2150)  PCFL21
      END IF
C
C     below-ground ammonia plant uptake
      WRITE (PRINTU,2470)
      WRITE (PRINTU,2480)
      CALL TRNVEC (I5,NCFX23(1,LEV),MFACTA,MFACTB,
     O             PCFL23)
      WRITE (PRINTU,2150)  PCFL23
C
      IF (ALPNFG .EQ. 1) THEN
C       above-ground nitrate plant uptake
        WRITE (PRINTU,2490)
        WRITE (PRINTU,2500)
        CALL TRNVEC (I5,NCFX20(1,LEV),MFACTA,MFACTB,
     O               PCFL20)
        WRITE (PRINTU,2150)  PCFL20
      END IF
C
C     below-ground ammonia plant uptake
      WRITE (PRINTU,2510)
      WRITE (PRINTU,2520)
      CALL TRNVEC (I5,NCFX22(1,LEV),MFACTA,MFACTB,
     O             PCFL22)
      WRITE (PRINTU,2150)  PCFL22
C
C     denitrification
      WRITE (PRINTU,2530)
      WRITE (PRINTU,2540)
      CALL TRNVEC (I5,NCFX6(1,LEV),MFACTA,MFACTB,
     O             PCFLX6)
      WRITE (PRINTU,2150)  PCFLX6
C
C     nitrification
      WRITE (PRINTU,2550)
      WRITE (PRINTU,2560)
      CALL TRNVEC (I5,NCFX7(1,LEV),MFACTA,MFACTB,
     O             PCFLX7)
      WRITE (PRINTU,2150)  PCFLX7
C
C     ammonia immobilization
      WRITE (PRINTU,2570)
      WRITE (PRINTU,2580)
      CALL TRNVEC (I5,NCFX8(1,LEV),MFACTA,MFACTB,
     O             PCFLX8)
      WRITE (PRINTU,2150)  PCFLX8
C
C     orrganic n mineralization to ammonia
      WRITE (PRINTU,2590)
      WRITE (PRINTU,2600)
      CALL TRNVEC (I5,NCFX9(1,LEV),MFACTA,MFACTB,
     O             PCFLX9)
      WRITE (PRINTU,2150)  PCFLX9
C
C     nitrate immobilization
      WRITE (PRINTU,2610)
      WRITE (PRINTU,2620)
      CALL TRNVEC (I5,NCFX17(1,LEV),MFACTA,MFACTB,
     O             PCFL17)
      WRITE (PRINTU,2150)  PCFL17
C
C     refractory conversion
      WRITE (PRINTU,2630)
      WRITE (PRINTU,2640)
      CALL TRNVEC (I5,NCFX19(1,LEV),MFACTA,MFACTB,
     O             PCFL19)
      WRITE (PRINTU,2150)  PCFL19
C
      IF (FIXNFG .EQ. 1) THEN
C       nitrogen fixation is turned on
        WRITE (PRINTU,2650)
        WRITE (PRINTU,2660)
        CALL TRNVEC (I5,NCFX12(1,LEV),MFACTA,MFACTB,
     O               PCFL12)
        WRITE (PRINTU,2150)  PCFL12
      END IF
C
      IF (AMVOFG .EQ. 1) THEN
C       ammonia volatilization is turned on
        WRITE (PRINTU,2670)
        WRITE (PRINTU,2680)
        CALL TRNVEC (I5,NCFX18(1,LEV),MFACTA,MFACTB,
     O               PCFL18)
        WRITE (PRINTU,2150)  PCFL18
      END IF
C
C     nitrogen balance check and report
      IF (UNITFG .EQ. 1) THEN
C       english
        UNITID= '   LB/AC'
      ELSE
C       metric
        UNITID= '   KG/HA'
      END IF
C
C     convert storages to external units for balance
      NSTORS= TOTN(LEV)*MFACTA+ MFACTB
      NSTOR = TOTN(1)*MFACTA+ MFACTB
C
C     find the net output of nitrogen from the pls
      MATIN= PADALL+ PIFLX(1)+ PIFLX(2)+ PIFLX(3)
      IF (FIXNFG .EQ. 1) THEN
C       nitrogen fixation is turned on
        MATIN= MATIN+ PCFL12(5)
      END IF
      MATDIF= MATIN- PPONIT- PCFLX6(5)- PCFLX3(2)- PCFLX5(2)-
     $               PCFL14(2)- PCFL16(2)
      IF (AMVOFG .EQ. 1) THEN
C       ammonia volatilization is turned on
        MATDIF= MATDIF- PCFL20(5)
      END IF
C
      CALL BALCHK (I1,LSNO,DATIM,MESSU,PRINTU,MSGFL,
     I             NSTORS,NSTOR,MATIN,MATDIF,UNITID,I1,
     M             NWCNT(1))
C
      RETURN
      END
C
C
C
      SUBROUTINE   NITRPB
C
C     + + + PURPOSE + + +
C     ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION NITR2 + + +
      INCLUDE    'cplni.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section nitr
      DO 10 J= 1,3
        IF (SDNFP(J).GE.1)  PAD(SDNFP(J)+IVL1)  = SEDN(J)
 10   CONTINUE
C
      DO 20 J= 1,5
        IF (TSAMSX(J).GE.1)  PAD(TSAMSX(J)+IVL1)= TSAMS(J)
        IF (TSNO3X(J).GE.1)  PAD(TSNO3X(J)+IVL1)= TSNO3(J)
        IF (TSSLNX(J).GE.1)  PAD(TSSLNX(J)+IVL1)= TSSLN(J)
        IF (TSSRNX(J).GE.1)  PAD(TSSRNX(J)+IVL1)= TSSRN(J)
        IF (NFIXFP(J).GE.1)  PAD(NFIXFP(J)+IVL1)= NFIXFX(J)
        IF (AMVOLX(J).GE.1)  PAD(AMVOLX(J)+IVL1)= AMVOL(J)
 20   CONTINUE
C
      DO 40 J= 1,3
        IF (SSAMSX(J).GE.1)  PAD(SSAMSX(J)+IVL1)= SSAMS(J)
        IF (SSNO3X(J).GE.1)  PAD(SSNO3X(J)+IVL1)= SSNO3(J)
        IF (SSSLNX(J).GE.1)  PAD(SSSLNX(J)+IVL1)= SSSLN(J)
        IF (SSSRNX(J).GE.1)  PAD(SSSRNX(J)+IVL1)= SSSRN(J)
        DO 30 I= 1, 2
          IF (NIADDX(J,I) .GE. 1) THEN
            PAD(NIADDX(J,I)+IVL1)= NIADDR(J,I)
          END IF
          IF (NIADWX(J,I) .GE. 1) THEN
            PAD(NIADWX(J,I)+IVL1)= NIADWT(J,I)
          END IF
 30     CONTINUE
 40   CONTINUE
C
      DO 50 J= 1, 4
        IF (NUPTGX(J).GE.1)  PAD(NUPTGX(J)+IVL1)= NUPTG(J)
 50   CONTINUE
C
      IF (OSDNFP.GE.1)  PAD(OSDNFP+IVL1)= SOSEDN
      IF (PONIFP.GE.1)  PAD(PONIFP+IVL1)= PONO3
      IF (POAMFP.GE.1)  PAD(POAMFP+IVL1)= PONH4
      IF (POONFP.GE.1)   PAD(POONFP +IVL1)= POORN
      IF (PONFP.GE.1)   PAD(PONFP +IVL1)= PONITR
      IF (TDNIFP.GE.1)  PAD(TDNIFP+IVL1)= DENIF(5)
C
      RETURN
      END
C
C
C
      SUBROUTINE   NITRPT
C
C     + + + PURPOSE + + +
C     ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION NITR2 + + +
      INCLUDE    'cplni.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    J
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section nitr
C
      IF (AGPLNX.GE.1)  PAD(AGPLNX+IVL1)= AGPLTN
      IF (LITRNX.GE.1)  PAD(LITRNX+IVL1)= LITTRN
C
      DO 10 J= 1, 8
        IF (SNFP(J).GE.1)  PAD(SNFP(J)+IVL1)= SN(J)
        IF (UNFP(J).GE.1)  PAD(UNFP(J)+IVL1)= UN(J)
        IF (LNFP(J).GE.1)  PAD(LNFP(J)+IVL1)= LN(J)
        IF (ANFP(J).GE.1)  PAD(ANFP(J)+IVL1)= AN(J)
        IF (TNFP(J).GE.1)  PAD(TNFP(J)+IVL1)= TN(J)
 10   CONTINUE
C
      DO 20 J= 1,4
        IF (INFP(J).GE.1)  PAD(INFP(J)+IVL1)= IN(J)
 20   CONTINUE
C
      DO 30 J= 1, 5
        IF (NDFCTX(J).GE.1)  PAD(NDFCTX(J)+IVL1)= NDFCT(J)
 30   CONTINUE
C
      IF (TNITFP.GE.1)  PAD(TNITFP+IVL1)= TOTNIT
C
      RETURN
      END
C
C
C
      SUBROUTINE   NITRST
     I                  (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables
C     used in material balance check for section nitr
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION NITR2 + + +
      INCLUDE  'cplni.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I1,I3,I5,I,J
C
C     + + + EXTERNALS + + +
      EXTERNAL  SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      I3= 3
      I5= 5
C
C     set flux accumulators to zero
C
      CALL SETVEC (I3,0.0,
     O             NITIF(1,LEV))
C
      CALL SETVEC (I3,0.0,
     O             NCFX1(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX2(1,LEV))
C
      CALL SETVEC (I3,0.0,
     O             NCFX3(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX4(1,LEV))
C
      CALL SETVEC (I3,0.0,
     O             NCFX5(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX6(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX7(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX8(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX9(1,LEV))
C
      DO 20 J= 1, 3
        DO 10 I= 1, 2
          NCFX10(J,I,LEV)= 0.0
          NCFX11(J,I,LEV)= 0.0
 10     CONTINUE
 20   CONTINUE
C
      CALL SETVEC (I5,0.0,
     O             NCFX12(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX13(1,LEV))
C
      CALL SETVEC (I3,0.0,
     O             NCFX14(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX15(1,LEV))
C
      CALL SETVEC (I3,0.0,
     O             NCFX16(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX17(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX18(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX19(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX20(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX21(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX22(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX23(1,LEV))
C
      CALL SETVEC (I1,0.0,
     O             NCFX24(1,LEV))
C
      CALL SETVEC (I3,0.0,
     O             NCFX25(1,LEV))
C
      CALL SETVEC (I3,0.0,
     O             NCFX26(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX27(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             NCFX28(1,LEV))
C
C     keep storage in state variable used for
C     material balance check
C
      TOTN(LEV)= TOTN(1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   NITRXN
     I                   (LSNO,MSGFL,DATIM,MESSU,ITMAXA,GNPM,BRXNFG,
     I                    CRXNFG,FORAFG,TMP,MOISTM,SOILM,NPM,LAYID,
     I                    KPLN,NUPTFG,FIXNFG,ALPNFG,AMVOFG,NUPTG,NMXRAT,
     I                    WILTPT,ORNPM,KVOL,THVOL,TRFVOL,KSAT,CSAT,
     I                    KRET,KRETF,ANUTF,SMST,
     M                    NDEFC,NWCNT,NECNT,AGPLTN,NIT,NITRXF  ! )
     x                   , pwsoildd, uunits,fldcp)  ! pw
C
C     + + + PURPOSE + + +
C     Perform reactions on nitrogen forms
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LSNO,MSGFL,DATIM(5),MESSU,ITMAXA,BRXNFG,CRXNFG,FORAFG,
     $            NUPTFG,FIXNFG,ALPNFG,AMVOFG,NWCNT(6),NECNT(1)
      REAL        GNPM(11),TMP,MOISTM,SOILM,NPM(11),KPLN,NUPTG,NMXRAT,
     $            WILTPT,ORNPM(4),KVOL,THVOL,TRFVOL,KSAT(4),CSAT(4),
     $            KRET,KRETF,ANUTF,SMST,NDEFC,AGPLTN,NIT(8),NITRXF(16)
     x           , pwsoildd, pwmoistD,fldcp       ! pw
c    x           , pwwiltpt, pwsoild, pwsoilv     ! pw  pwmoistD=SMST
      CHARACTER*4 LAYID
      integer     uunits  ! pw 
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LSNO   - land segment number of PERLND
C     MSGFL  - fortran unit number of HSPF message file
C     DATIM  - date and time of day
C     MESSU  - ftn unit no. to be used for printout of messages
C     ITMAXA - maximum number of iterations allowed for convergence of
C              Freundlich method for ammonia adsorption/desorption
C     GNPM   - general nitrogen parameters
C     BRXNFG - flag indicating whether biological reaction fluxes are
C              recalculated this interval
C     CRXNFG - flag indicating whether chemical reaction fluxes are
C              recalculated this interval (adsorption/desorption)
C     FORAFG - flag indicating which method is used to calculate adsorption/
C              desorption - 1: first-order rates; 2: single-valued Freundlich
C     TMP    - soil temperature in this layer
C     MOISTM - soil moisture in this layer (lb or kg)
C     SOILM  - soil mass of this layer (lb or kg)
C     NPM    - nitrogen parameters for this layer
C     LAYID  - character identifier for this layer
C     KPLN   - first-order plant-uptake parameter for this layer
C     NUPTFG - flag indicating which method is used to calculate plant uptake
C              0: first-order rate; 1: yield-based algorithm;
C              2 or -2: half-saturation (Michaelis-Menton) kinetics
C     FIXNFG - flag to turn on/off nitrogen fixation (NUPTFG= 1 only)
C     ALPNFG - flag to turn on/off above-ground and litter n compartments
C     AMVOFG - flag to turn on/off ammonia volatilization
C     NUPTG  - plant uptake target for this layer (NUPTFG= 1)
C     NMXRAT - ratio of maximum plant uptake to target uptake (NUPTFG= 1)
C     WILTPT - wilting point: soil moisture cutoff for plant uptake for this
C              layer (NUPTFG= 1)
C     ORNPM  - organic n parameters for this layer
C     KVOL   - first-order ammonia volatilization rate for this layer
C     THVOL  - temperature correction coefficient for ammonia volatilization
C     TRFVOL - reference temperature for ammonia volatilization
C     KSAT   - max rate parameters for half-saturation kinetics for this layer
C     CSAT   - half-saturation constants for this layer
C     KRET   - plant N return rate for this layer
C     KRETF  - refractory fraction of plant N return
C     ANUTF  - above-ground plant uptake fraction
C     SMST   - soil moisture storage in inches
C     NDEFC  - cumulative plant uptake deficit for this layer (NUPTFG= 1)
C     NWCNT  - warning counts
C     NECNT  - error count
C     AGPLTN - above-ground plant n storage
C     NIT    - storages of each species of nitrogen in this layer
C     NITRXF - current reaction fluxes for this layer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I4,SCLU,SGRP
      REAL        NO3UTF,NH4UTF,THPLN,THKDSA,THKADA,THKIMN,THKAM,THKDNI,
     $            THKNI,THKIMA,CMAXAM,KDSAM,KADAM,KIMNI,KAM,KDNI,KNI,
     $            KIMAM,XFIXAM,XMAXAM,KF1AM,N1IAM,KLON,KRON,KONLR,
     $            THNLR,KSUNI,KSUAM,KSINI,KSIAM,CSUNI,CSUAM,CSINI,CSIAM,
     $            PLON,AMAD,AMSU,NO3,PLTN,SLON,PRON,SRON,ADSAM,DESAM,
     $            AMMIF,IMMAM,UTAM,IMMNI,UTNI,NITRF,DENI,AMVO,RFON,
     $            PRETL,PRETR,UTAMA,UTNIA,NFIX,UTNTOT,TNH4,AMCY,DIF35,
     $            KDNIK,KIMAMK,KIMNIK,KAMK,KPLNK,KNIK,KONLRK,KSUNIK,
     $            KSUAMK,KSINIK,KSIAMK,KVOLK,PRET,MAXUPT,FRAC,TFRAC,
     $            TPLON,TAMAD,TNO3,TAMSU,TPLTN,UTAMAB,AMSULO,AMDEFC,
     $            UTAMFR,TLON,TRON,UTNACT,AGUTF,NICONC,AMCONC
      CHARACTER*4 NH4ID(5),CHSTR
      character*80 testout
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL    FIRORD,SV,OMSTR,OMSTC,OMSTD,OMSTI,OMSG
C
C     + + + DATA INITIALIZATIONS + + +
      DATA       NH4ID/'AMMO','NIUM','    ','    ','    '/
C
       testout='/model/w510/output/testout.out'
       open(98, file=testout, status='unknown') 
c      write(98,*) YEAR, MD, DAY, SMST
       write(98,*) SMST, pwsoildd, wiltpt, fldcp 
C     + + + END SPECIFICATIONS + + +
c  ! pw adding  
c !         goto 550          
         if(uunits .eq. 1) then
            CFINMA = 2.264E05
         else
            CFINMA = 2.54E05
         endif 
c no! er   pwwiltpt = wiltpt  * cfinma    ! for forest only
         pwmoistD= moistm  /  cfinma   ! correct, mass -> volume (inch)
c   !  should be = SMST?? soil moisture in inches (after mstley). 
c no     pwsoilv  = pwsoild * cfinma                     
c  ! pw end added
550        continue 
C
      I4    = 4
      SCLU  = 310
C     assign values to local variables where necessary
C     general parameters
      NO3UTF= GNPM(1)
      NH4UTF= GNPM(2)
      THPLN = GNPM(3)
      THKDSA= GNPM(4)
      THKADA= GNPM(5)
      THKIMN= GNPM(6)
      THKAM = GNPM(7)
      THKDNI= GNPM(8)
      THKNI = GNPM(9)
      THKIMA= GNPM(10)
      CMAXAM= GNPM(11)
C
C     layer specific parameters
      KDSAM = NPM(1)
      KADAM = NPM(2)
      KIMNI = NPM(3)
      KAM   = NPM(4)
      KDNI  = NPM(5)
      KNI   = NPM(6)
      KIMAM = NPM(7)
      XFIXAM= NPM(8)
      XMAXAM= NPM(9)
      KF1AM = NPM(10)
      N1IAM = NPM(11)
C
C     organic nitrogen parameters
      KLON= ORNPM(1)
      KRON= ORNPM(2)
      KONLR= ORNPM(3)
      THNLR= ORNPM(4)
C
C     saturation kinetics parameters
      KSUNI= KSAT(1)
      KSUAM= KSAT(2)
      KSINI= KSAT(3)
      KSIAM= KSAT(4)
      CSUNI= CSAT(1)
      CSUAM= CSAT(2)
      CSINI= CSAT(3)
      CSIAM= CSAT(4)
C
C     layer specific storages of nitrogen
      PLON= NIT(1)
      AMAD= NIT(2)
      AMSU= NIT(3)
      NO3 = NIT(4)
      PLTN= NIT(5)
      SLON= NIT(6)
      PRON= NIT(7)
      SRON= NIT(8)
C
C     layer specific reaction fluxes
      ADSAM= NITRXF(1)
      DESAM= NITRXF(2)
      AMMIF= NITRXF(3)
      IMMAM= NITRXF(4)
      UTAM = NITRXF(5)
      IMMNI= NITRXF(6)
      UTNI = NITRXF(7)
      NITRF= NITRXF(8)
      DENI = NITRXF(9)
      AMVO = NITRXF(10)
      RFON = NITRXF(11)
      PRETL= NITRXF(12)
      PRETR= NITRXF(13)
      UTAMA= NITRXF(14)
      UTNIA= NITRXF(15)
      NFIX = NITRXF(16)
C
C     set other needed local variables
      UTNTOT= UTNI+ UTAM
      IF (ALPNFG .EQ. 1) THEN
C       above-ground and litter compartments being simulated
        AGUTF= ANUTF
      ELSE
C       no above-ground plant uptake
        AGUTF= 0.0
      END IF
C
      IF (CRXNFG .EQ. 1) THEN
C       chemical (adsorption/desorption) fluxes are
C       recalculated this interval
        IF (FORAFG .NE. 1) THEN
C         ammonium is adsorbed/desorbed by first order kinetics
C         with this method the adsorption/desorption fluxes are
C         calculated every cnumn intervals in units of the basic
C         simulation interval (mass/area-ivl); the updating of the
C         storages is done every interval
C
          CALL FIRORD (TMP,MOISTM,KDSAM,KADAM,THKDSA,THKADA,
     I                 AMSU,AMAD,
     O                 ADSAM,DESAM)
        ELSE
C         ammonium is adsorbed/desorbed using the single value
C         freundlich method
C         with this method the adsorption/desorption is instantaneous
C         and is done every cnumn intervals.  because this method is
C         instantaneous, no updating of the storages is done during
C         intermediate intervals
C
C         total ammonium
          TNH4= AMAD+ AMSU
C
          CALL SV (MOISTM,SOILM,TNH4,XFIXAM,CMAXAM,XMAXAM,KF1AM,
     I             N1IAM,LSNO,MESSU,MSGFL,DATIM,
     I             ITMAXA,NH4ID,LAYID,
     M             AMSU,NECNT(1),
     O             AMCY,AMAD)
C
C         zero fluxes since this method is based on
C         instantaneous equilibrium
          ADSAM= 0.0
          DESAM= 0.0
C
C         any crystalline ammonium formed is considered adsorbed
          AMAD= AMAD+ AMCY
C
        END IF
C
      END IF
C
      IF (BRXNFG .EQ. 1) THEN
C       biochemical transformation fluxes are recalculated
C       this interval
C
        IF ( (TMP .GT. 4.0) .AND. (MOISTM .GT. 100.0) ) THEN
C         there is sufficient soil layer temperature (in deg c)
C         and moisture for biochemical transformations to occur
C
          IF (TMP .LT. 35.0) THEN
C           soil layer temperature in deg c is less than
C           optimum, modify inputted first order reaction rates
C           decrease the inputted first order reaction rates
C           by the modified arrhenius equation
            DIF35 = TMP- 35.0
            KDNIK = KDNI*THKDNI**DIF35
            KIMAMK= KIMAM*THKIMA**DIF35
            KIMNIK= KIMNI*THKIMN**DIF35
            KAMK  = KAM*THKAM**DIF35
            IF (NUPTFG .EQ. 0) THEN
C             first order plant uptake
              KPLNK = KPLN*THPLN**DIF35
            END IF
            KNIK  = KNI*THKNI**DIF35
            KONLRK= KONLR*THNLR**DIF35
            IF ( (NUPTFG .EQ. 2) .OR. (NUPTFG .EQ. -2) ) THEN
C             max rates for saturation kinetics
              KSUNIK= KSUNI*THPLN**DIF35
              KSUAMK= KSUAM*THPLN**DIF35
              KSINIK= KSINI*THKIMN**DIF35
              KSIAMK= KSIAM*THKIMA**DIF35
c                goto 555  ! pw 
c ! pw 8/9/06 adding, because input max is mass/area/day for per depth soil. 
              KSUNIK = KSUNIK * pwsoildd !* 0.1  ! otherwise, input *0.01  
              KSUAMK = KSUAMK * pwsoildd !* 0.1  ! otherwise, input *0.01  
              KSINIK = KSINIK * pwsoildd ! * 0.1  ! otherwise, input *0.01  
              KSIAMK = KSIAMK * pwsoildd ! * 0.1  ! otherwise, input *0.01  
c ! pw end added 
555            continue 
            ELSE
C             zero rates
              KSUNIK= 0.0
              KSUAMK= 0.0
              KSINIK= 0.0
              KSIAMK= 0.0
            END IF
          ELSE
C           soil layer temperature in deg c is at optimum,
C           use inputted first order reaction rates
            KDNIK = KDNI
            KIMAMK= KIMAM
            KIMNIK= KIMNI
            KAMK  = KAM
            IF (NUPTFG .EQ. 0) THEN
C             first order plant uptake
              KPLNK = KPLN
            END IF
            KNIK  = KNI
            KONLRK= KONLR
            IF ( (NUPTFG .EQ. 2) .OR. (NUPTFG .EQ. -2) ) THEN
C             max rates for saturation kinetics
              KSUNIK= KSUNI
              KSUAMK= KSUAM
              KSINIK= KSINI
              KSIAMK= KSIAM
cccc            goto 556  ! pw 
c ! pw 8/9/06 adding, because input max is mass/area/day for per depth soil. 
              KSUNIK = KSUNIK * pwsoildd !* 0.1  ! otherwise, input *0.01  
              KSUAMK = KSUAMK * pwsoildd !* 0.1  ! otherwise, input *0.01  
              KSINIK = KSINIK * pwsoildd ! * 0.1  ! otherwise, input *0.01  
              KSIAMK = KSIAMK * pwsoildd ! * 0.1  ! otherwise, input *0.01  
c ! pw end added 
556            continue 
            ELSE
C             zero rates
              KSUNIK= 0.0
              KSUAMK= 0.0
              KSINIK= 0.0
              KSIAMK= 0.0
            END IF
          END IF
C
          IF (AMVOFG .EQ. 1) THEN
C           temperature correction for ammonia volatilization
            KVOLK = KVOL*THVOL**(TMP-TRFVOL)
          ELSE
C           zero rate
            KVOLK= 0.0
          END IF
C
C         recompute transformation fluxes.  this is done every
C         bnumn intervals in units of the basic simulation
C         interval (mass/area-ivl); however, the updating
C         of the storages is done every interval;  the exception
C         is nitrogen fixation, which is recomputed every interval
C
C         nitrification
          NITRF= AMSU*KNIK
C
C         denitrification
          DENI = NO3*KDNIK
C
C         organic nitrogen ammonification
          AMMIF= PLON*KAMK
C
C         ammonia volatilization
          AMVO= AMSU*KVOLK
C
C         conversion of particulate labile organic n to refractory
          RFON= PLON*KONLRK
C
C         plant nitrogen return - not temperature-adjusted
          PRET= PLTN*KRET
          PRETR= PRET* KRETF
          PRETL= PRET- PRETR
C
          IF ( (NUPTFG .EQ. 0) .OR. (NUPTFG .EQ. 1) ) THEN
C           immobilization is first-order
C
C           nitrate immobilization
            IMMNI= NO3*KIMNIK
C
C           ammonium immobilization
            IMMAM= AMSU*KIMAMK
C
          ELSE IF ( (NUPTFG .EQ. 2) .OR. (NUPTFG .EQ. -2) ) THEN
C           immobilization is half-saturation kinetics
C
C           compute current concentrations
caa           goto 331 ! pw
            NICONC= NO3/MOISTM*1.0E6
            AMCONC=AMSU/MOISTM*1.0E6
331          continue ! pw
             goto 332
c! pw out   AMCONC= AMSU/MOISTM*1.0E6
c ! pw  adding
c          if(pwmoistD .lt. 0.2*pwsoildd) then 
           if( SMST    .lt. fldcp)then ! 0.2*pwsoildd = fldcp 
            NICONC = (  SMST  /(fldcp))*NO3/MOISTM*1.0E6 
            AMCONC = (  SMST  /(fldcp))*AMSU/MOISTM*1.0E6 
           endif
c  actually, it equals NICOCN=NO3/(0.2*soilM) *1.0E6  !!!
c ! pw end-added 
C
332         continue ! pw 
C           nitrate immobilization
            IF (NICONC .LE. 0.0) THEN
C             no nitrate present
              IMMNI= 0.0
            ELSE
              goto 333  ! pw
C             nitrate present
              IMMNI= KSINIK*NICONC/(CSINI+ NICONC)
              IMMNI= IMMNI*MOISTM/1.0E6
c ! pw out    IMMNI= KSINIK*NICONC/(CSINI+ NICONC)
c ! pw out    IMMNI= IMMNI*MOISTM/1.0E6
333           continue  ! pw 
c !!          goto 334
c ! pw adding !!!!
cbb          if(SMST .lt. wiltpt ) then   ! compare depth of water
c cbb         if(pwmoistD .lt. wiltpt ) then   ! compare depth of water
cbb            immni  = 0.0 
cbb          else          ! greater than wilting point 
cbb            if( SMST    .le. fldcp) then ! less than field capacity
c !                             or  0.1??? 
caa         NICONC = (  SMST  /(fldcp))*NO3/MOISTM*1.0E6 
caa             KSINIKe=KSINIK* SMST   /(fldcp) !effective mass max immobil
caa            immni = KSINIKe *NICONC/(NICONC+CSUNI)
caa            else   ! greater than field capacity 
                 NICONC = NO3/MOISTM*1.0E6
cbb         NICONC = (  SMST  /(fldcp))*NO3/MOISTM*1.0E6 
                 immni= KSINIK*NICONC / (NICONC + CSUNI)
              IMMNI= IMMNI*MOISTM/1.0E6
cbb            else
cbb              NICONC = NO3/MOISTM*1.0E6
cbb              immni= KSINIK*NICONC / (NICONC + CSUNI)
cbb            endif 
cbb          endif
c  ! pw end added   
334         continue 
            END IF
C
C           ammonium immobilization
            IF (AMCONC .LE. 0.0) THEN
C             no solution ammonia present
              IMMAM= 0.0
            ELSE
c !             goto 335   ! pw 
C             solution ammonia present
cbb           IMMAM= KSIAMK*AMCONC/(CSIAM+ AMCONC)
cbb           IMMAM= IMMAM*MOISTM/1.0E6
cbb             goto 336   ! pw
c! pw out     IMMAM= KSIAMK*AMCONC/(CSIAM+ AMCONC)
c! pw out     IMMAM= IMMAM*MOISTM/1.0E6
335              continue 
cbb          if(SMST .lt. wiltpt ) then   ! compare depth of water
c             if(pwmoistD .lt. wiltpt ) then   ! compare depth of water
cbb            immam  = 0.0 
cbb          else          ! greater than wilting point 
cbb            if( SMST    .le. fldcp) then ! less than field capacity
c !                             or  0.1??? 
caa         AMCONC = (  SMST  /(fldcp))*AMSU/MOISTM*1.0E6 
caa             KSIAMKe=KSIAMK* SMST   /(fldcp) !effective mass max immobil
caa            immam = KSIAMKe *AMCONC/(AMCONC+CSUAM)
caa            else   ! greater than field capacity 
                 AMCONC =AMSU/MOISTM*1.0E6
cbb         AMCONC = (  SMST  /(fldcp))*AMSU/MOISTM*1.0E6 
                 immam= KSIAMK*AMCONC / (AMCONC + CSUAM)
              IMMAM= IMMAM*MOISTM/1.0E6
cbb            else 
cbb              AMCONC =AMSU/MOISTM*1.0E6
cbb              immam= KSIAMK*AMCONC / (AMCONC + CSUAM)
cbb            endif 
cbb          endif
c  ! pw end added   
336          continue 
            END IF
C
          END IF
C
          IF (NUPTFG .EQ. 0) THEN
C           plant uptake is first-order
C
C           plant uptake of nitrate
            UTNI= NO3*KPLNK*NO3UTF
C
C           plant uptake of ammonium
            UTAM= AMSU*KPLNK*NH4UTF
C
          ELSE IF (NUPTFG .EQ. 1) THEN
C           plant uptake is yield-based
C
            IF (SMST .GE. WILTPT) THEN
C             soil moisture is at or above wilting point
c ! pw adding for moisture affect 
               if(smst .le. fldcp) then ! less than field capacity
c !                             or  0.1??? 
c plan        utntot = (nuptg + ndefc) * 
c pla.n        (   (smst-wiltpt)/(pwsoildd*0.2 - wiltpt)  )
c             linear 0 - 1.0  
c    .         (0.5+(0.5 * (smst-wiltpt)/(pwsoildd*0.2 - wiltpt) ))
c!   .         (0.5 * (1.0 + (smst-wiltpt)/(pwsoildd*0.2 - wiltpt) ) )
c    the above are   linear: 0.5 ~ 1.0 from wiltpt to field capacity 
cpw    the following one is using, but taken out temperaly for linear!! 
cpw              utntot = (nuptg + ndefc) * 
cpw     .         (        ((smst-wiltpt)/(fldcp - wiltpt) )**0.25)
c the next is for linear 
                 utntot = (nuptg + ndefc)*(smst-wiltpt)/(fldcp-wiltpt)
c                    exponential: 0.0~1.0
c    .         (0.5+0.5*((smst-wiltpt)/(pwsoildd*0.2 - wiltpt) )**0.25)
c                    exponential: 0.5~1.0
               else   ! greater than field capacity 
c! pw out     UTNTOT= NUPTG+ NDEFC
              utntot = (nuptg + ndefc) * 
     .     ( - 0.1*(smst-fldcp)*(smst-fldcp) -0.05*(smst-fldcp) +1.)
               endif 
c            endif
c ! pw end added   
C
C             try to take up optimum target plus seasonal deficit
c!pw out!!    UTNTOT= NUPTG+ NDEFC
C
C             make sure maximum rate is not exceeded
              MAXUPT= NUPTG*NMXRAT
              IF (UTNTOT .GT. MAXUPT) THEN
C               reduce to maximum rate
                UTNTOT= MAXUPT
              END IF
            ELSE
C             soil moisture is below wilting point
              UTNTOT= 0.0
            END IF
C
C           divide total target between nitrate and ammonia
            UTNI= UTNTOT*NO3UTF
            UTAM= UTNTOT*NH4UTF
C           
          ELSE IF ( (NUPTFG .EQ. 2) .OR. (NUPTFG .EQ. -2) ) THEN
C           plant uptake uses saturation kinetics
C
C           nitrate
cbb         NICONC = (  SMST  /(fldcp))*NO3/MOISTM*1.0E6 
cbb         AMCONC = (  SMST  /(fldcp))*AMSU/MOISTM*1.0E6 
            NICONC =                    NO3/MOISTM*1.0E6 
            AMCONC =                    AMSU/MOISTM*1.0E6 
c           NICONC = (  SMST  /(fldcp))*NICONC           
c           NICONC = (  SMST  /(fldcp))*NICONC           
c           AMCONC = (  SMST  /(fldcp))*AMCONC            
            IF (NICONC .LE. 0.0) THEN
C             no nitrate present
              UTNI= 0.0
            ELSE
C             nitrate present
               goto 441   ! pw 
              UTNI= KSUNIK*NICONC/(CSUNI+ NICONC)
              UTNI= UTNI*MOISTM/1.0E6
441           continue 
c !!!          goto 442 ! pw 
c! pw out     UTNI= KSUNIK*NICONC/(CSUNI+ NICONC)
c! pw out     UTNI= UTNI*MOISTM/1.0E6
c ! pw adding !!!!
             if(smst .lt. wiltpt ) then   ! compare depth of water
                 utni = 0.0 
             else          ! greater than wilting point 
               if(smst .le. fldcp) then ! less than field capacity
c !                             or  0.1??? 
            NICONC =                    NO3/MOISTM*1.0E6 ! tests 
            NICONC = (  SMST  /(fldcp))*NO3/MOISTM*1.0E6 
                KSUNIKe=KSUNIK*smst/(fldcp) ! effective mass max uptake
                utni = KSUNIKe *NICONC/(NICONC+CSUNI)
caa            else   ! greater than field capacity 
cee              NICONC = NO3/MOISTM*1.0E6
cee         NICONC = (  SMST  /(fldcp))*NO3/MOISTM*1.0E6  ! cbb  
cee              utni = KSUNIK*NICONC / (NICONC + CSUNI)
cff           UTNI= UTNI*MOISTM/1.0E6      ! out this getting more uptake 
               else
                 NICONC = NO3/MOISTM*1.0E6
                 utni = KSUNIK*NICONC / (NICONC + CSUNI)
               endif 
             endif
c  ! pw end added 
442           continue   ! pw 
            END IF
C
C           ammonia
            IF (AMCONC .LE. 0.0) THEN
C             no solution ammonia present
              UTAM= 0.0
            ELSE
              goto 443 ! pw 
C             solution ammonia present
              UTAM= KSUAMK*AMCONC/(CSUAM+ AMCONC)
              UTAM= UTAM*MOISTM/1.0E6
c  !!!         goto 444
c! pw out     UTAM= KSUAMK*AMCONC/(CSUAM+ AMCONC)
c! pw out     UTAM= UTAM*MOISTM/1.0E6
c ! pw adding !!!!
443             continue 
             if(smst .lt.   wiltpt ) then   ! compare depth of water
                 utam = 0.0 
             else          ! greater than wilting point 
               if(smst .le. fldcp) then ! less than field capacity
c !                             or  0.1??? 
            AMCONC = (  SMST  /(fldcp))*AMSU/MOISTM*1.0E6 
                KSUAMKe=KSUAMK*smst /(fldcp) ! effective mass max uptake
                utam = KSUAMKe *AMCONC/(AMCONC+CSUAM)
caa            else   ! greater than field capacity 
cee              AMCONC =AMSU/MOISTM*1.0E6
cee      AMCONC = (  SMST  /(fldcp))*AMSU/MOISTM*1.0E6 
cee              utam = KSUAMK*AMCONC / (AMCONC + CSUAM)
cff           UTAM= UTAM*MOISTM/1.0E6
               else 
                 AMCONC =AMSU/MOISTM*1.0E6
                 utam = KSUAMK*AMCONC / (AMCONC + CSUAM)
               endif 
             endif
c  ! pw end added   
444           continue 
            END IF
C
          END IF
C
          IF (ALPNFG .EQ. 1) THEN
C           divide plant uptake into above- and below-ground
            UTAMA= UTAM* AGUTF
            UTAM= UTAM- UTAMA
            UTNIA= UTNI* AGUTF
            UTNI= UTNI- UTNIA
          END IF
        ELSE
C         there are no biochemical transformations occurring due
C         to either low temperatures or low moisture
C         zero fluxes
          DENI = 0.0
          IMMNI= 0.0
          AMMIF= 0.0
          IMMAM= 0.0
          UTNI = 0.0
          NITRF= 0.0
          UTAM = 0.0
          AMVO = 0.0
          RFON = 0.0
          PRETL= 0.0
          PRETR= 0.0
          UTAMA= 0.0
          UTNIA= 0.0
          UTNTOT= 0.0
C
        END IF
      END IF
C
C     update all storages to account for fluxes - done every
C     interval; check and fix any storages that may be negative
C
C     initalize the fraction used to change any negative storages
C     that may have been computed; frac also acts as a flag
C     indicating negative storages were projected (when < 1.0)
      FRAC= 1.0
C
C     calculate temporary particulate labile organic nitrogen in storage
      TPLON= PLON- AMMIF+ IMMAM+ IMMNI+ PRETL- RFON
C
      IF (TPLON .LT. 0.0) THEN
C       negative storage value is unrealistic
C       calculate that fraction of the flux that is
C       needed to make the storage zero
        FRAC= PLON/(PLON-TPLON)
C
C       write a warning that the organic nitrogen value will
C       be fixed up so that it does not go negative
C
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (FRAC)
        CALL OMSTR (PLON)
        CALL OMSTR (TPLON)
        CALL OMSTR (AMMIF)
        CALL OMSTR (IMMAM)
        CALL OMSTR (IMMNI)
        CALL OMSTR (PRETL)
        CALL OMSTR (RFON)
        CHSTR = LAYID
        CALL OMSTC (I4,CHSTR1)
        SGRP = 2
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             NWCNT(3))
C
      END IF
C
C     calculate temporary adsorbed ammonium in storage
      TAMAD= AMAD- DESAM+ ADSAM
C
      IF (TAMAD .LT. 0.0) THEN
C
C       negative storage value is unrealistic
C       calculate that fraction of the flux that is
C       needed to make the storage zero
        TFRAC= AMAD/(AMAD-TAMAD)
C
C       keep the smaller fraction; the smaller fraction
C       of the fluxes will make all the storages either zero
C       or positive
        IF (TFRAC.LT.FRAC)  FRAC= TFRAC
C
C       write a warning that the adsorbed value of ammonium will
C       be fixed up so that it does not go negative
C
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (FRAC)
        CALL OMSTR (AMAD)
        CALL OMSTR (TAMAD)
        CALL OMSTR (ADSAM)
        CALL OMSTR (DESAM)
        CHSTR = LAYID
        CALL OMSTC (I4,CHSTR1)
        SGRP = 3
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             NWCNT(4))
C
      END IF
C
C     calculate temporary nitrate in storage
      TNO3= NO3+ NITRF- (IMMNI+DENI+UTNI+UTNIA)
C
      IF (TNO3 .LT. 0.0) THEN
C       negative storage value is unrealistic
C       calculate that fraction of the flux that is
C       needed to make the storage zero
        TFRAC= NO3/(NO3-TNO3)
C
C       keep the smaller fraction; the smaller fraction
C       of the fluxes will make all the storages either zero
C       or positive
        IF (TFRAC.LT.FRAC)  FRAC= TFRAC
C
C       write a warning that the value of nitrate in storage will
C       be fixed up so that it does not go negative
C
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (FRAC)
        CALL OMSTR (NO3)
        CALL OMSTR (TNO3)
        CALL OMSTR (NITRF)
        CALL OMSTR (IMMNI)
        CALL OMSTR (DENI)
        CALL OMSTR (UTNI)
        CALL OMSTR (UTNIA)
        CHSTR = LAYID
        CALL OMSTC (I4,CHSTR1)
        SGRP = 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             NWCNT(2))
C
      END IF
C
C     calculate temporary solution ammonium in storage
      TAMSU= AMSU+ DESAM+ AMMIF- (ADSAM+IMMAM+UTAM+UTAMA+NITRF+AMVO)
C
      IF (NUPTFG .EQ. 1) THEN
C       set a threshold value for solution ammonium in storage
        AMSULO= 0.001
        UTAMAB= UTAM+ UTAMA
        IF ( (TAMSU .LE. AMSULO) .AND. (UTAMAB .GT. 0.0)) THEN
C         determine reduction
          AMDEFC= AMSULO- TAMSU
          IF ( AMDEFC .GE. UTAMAB) THEN
C           shut off uptake completely
            TAMSU= TAMSU+ UTAMAB
            UTAM= 0.0
            UTAMA= 0.0
          ELSE
C           prorate reduction
            UTAMFR= AMDEFC/UTAMAB
            TAMSU= TAMSU+ (UTAMFR*UTAMAB)
            UTAM= UTAM*(1.0- UTAMFR)
            UTAMA= UTAMA*(1.0- UTAMFR)
          END IF
        END IF
      END IF
C
      IF (TAMSU .LT. 0.0) THEN
C       negative storage value is unrealistic
C       write a warning that the solution value of ammonium will
C       be fixed up so that it does not go negative
        TFRAC= AMSU/(AMSU-TAMSU)
C
C       keep the smaller fraction; the smaller fraction
C       of the fluxes will make all the storages either zero
C       or positive
        IF (TFRAC.LT.FRAC)  FRAC= TFRAC
C
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (FRAC)
        CALL OMSTR (AMSU)
        CALL OMSTR (TAMSU)
        CALL OMSTR (ADSAM)
        CALL OMSTR (DESAM)
        CALL OMSTR (AMMIF)
        CALL OMSTR (IMMAM)
        CALL OMSTR (NITRF)
        CALL OMSTR (UTAM)
        CALL OMSTR (UTAMA)
        CALL OMSTR (AMVO)
        CHSTR = LAYID
        CALL OMSTC (I4,CHSTR1)
        SGRP = 4
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             NWCNT(5))
C
      END IF
C
C     calculate temporary below-ground plant n in storage
      TPLTN= PLTN+ UTNI+ UTAM- PRETL- PRETR
C
      IF (TPLTN .LT. 0.0) THEN
C       negative storage value is unrealistic
C       calculate that fraction of the flux that is
C       needed to make the storage zero
        TFRAC= PLTN/(PLTN-TPLTN)
C
C       keep the smaller fraction; the smaller fraction
C       of the fluxes will make all the storages either zero
C       or positive
        IF (TFRAC.LT.FRAC)  FRAC= TFRAC
      END IF
C
      IF (FRAC .GE. 1.0) THEN
C       no storages have gone negative; use the temporary values
        PLON= TPLON
        AMAD= TAMAD
        NO3 = TNO3
        AMSU= TAMSU
        PLTN= TPLTN
      ELSE
C       at least one of the storages has gone negative
C       use frac to adjust the fluxes to make all the storages
C       zero or positive
        FRAC = FRAC*0.9999
        ADSAM= ADSAM*FRAC
        DESAM= DESAM*FRAC
        AMMIF= AMMIF*FRAC
        IMMAM= IMMAM*FRAC
        UTAM = UTAM*FRAC
        IMMNI= IMMNI*FRAC
        UTNI = UTNI*FRAC
        NITRF= NITRF*FRAC
        DENI = DENI*FRAC
        AMVO = AMVO*FRAC
        RFON = RFON*FRAC
        PRETL= PRETL*FRAC
        PRETR= PRETR*FRAC
        UTAMA= UTAMA*FRAC
        UTNIA= UTNIA*FRAC
C
C       recalculate the storages
        PLON = PLON- AMMIF+ IMMAM+ IMMNI+ PRETL- RFON
        AMAD = AMAD- DESAM+ ADSAM
        NO3  = NO3+ NITRF- (IMMNI+DENI+UTNI+UTNIA)
        AMSU = AMSU+ DESAM+ AMMIF- (ADSAM+IMMAM+UTAM+UTAMA+NITRF+AMVO)
        PLTN= PLTN+ UTAM+ UTNI- PRETL- PRETR
C
      END IF
C
C     add converted organic n and plant return to refractory storage
      PRON= PRON+ RFON+ PRETR
C
C     partition particulate and solution fractions of organic n storages
C
C     labile
      TLON= PLON+ SLON
      IF (KLON .GT. 1.0E15) THEN
C       infinite paritition coefficient for labile
        SLON= 0.0
      ELSE
C       partition labile
        SLON= TLON/(KLON+ 1.0)
      END IF
      PLON= TLON- SLON
C
C     refractory
      TRON= PRON+ SRON
      IF (KRON .GT. 1.0E15) THEN
C       infinite paritition coefficient for labile
        SRON= 0.0
      ELSE
C       partition labile
        SRON= TRON/(KRON+ 1.0)
      END IF
      PRON= TRON- SRON
C
      IF (ALPNFG .EQ. 1) THEN
C       above-ground compartment simulated
        AGPLTN= AGPLTN+ UTAMA+ UTNIA
      END IF
C
C     calculate nitrogen fixation every interval
      NFIX= 0.0
      IF ( (NUPTFG .EQ. 1) .AND. (FIXNFG .EQ. 1) ) THEN
C       see if there is still unsatisfied demand for uptake
        IF ( (TMP .GT. 4.0) .AND. (MOISTM .GT. 100.0) ) THEN
C         there is sufficient soil layer temperature (in deg c)
C         and moisture for biochemical transformations to occur
          IF (SMST .GE. WILTPT) THEN
C           soil moisture is at or above wilting point
            UTNACT= UTNI+ UTAM+ UTNIA+ UTAMA
            IF (UTNTOT .GT. UTNACT) THEN
C             still unsatisfied plant uptake demand
              NFIX= UTNTOT- UTNACT
              PLTN= PLTN+ NFIX
            END IF
          END IF
        END IF
      END IF
C
      IF (NUPTFG .EQ. 1) THEN
C       accumulate any deficit
        NDEFC= NDEFC+ NUPTG- UTAM- UTNI- UTAMA- UTNIA- NFIX
        IF (NDEFC .LT. 1.0E-06) THEN
C         deficit has been erased
          NDEFC= 0.0
        END IF
      END IF
C
C     reassign storages to "permanent" array
      NIT(1)= PLON
      NIT(2)= AMAD
      NIT(3)= AMSU
      NIT(4)= NO3
      NIT(5)= PLTN
      NIT(6)= SLON
      NIT(7)= PRON
      NIT(8)= SRON
C
C     reassign fluxes to "permanent" array
      NITRXF(1)= ADSAM
      NITRXF(2)= DESAM
      NITRXF(3)= AMMIF
      NITRXF(4)= IMMAM
      NITRXF(5)= UTAM
      NITRXF(6)= IMMNI
      NITRXF(7)= UTNI
      NITRXF(8)= NITRF
      NITRXF(9)= DENI
      NITRXF(10)= AMVO
      NITRXF(11)= RFON
      NITRXF(12)= PRETL
      NITRXF(13)= PRETR
      NITRXF(14)= UTAMA
      NITRXF(15)= UTNIA
      NITRXF(16)= NFIX
C
      RETURN
      END
