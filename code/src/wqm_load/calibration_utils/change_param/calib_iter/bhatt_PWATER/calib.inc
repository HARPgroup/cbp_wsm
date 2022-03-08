************************************************************************
** variables used frequently in the calibration parameter change prog **
**                                                                    **  
** the order of land uses must match extactly the orders specified in **
** ../../../../lib/inc/land_use.inc                                   **
************************************************************************
      include '../../../../lib/inc/rsegs.inc'
      include '../../../../lib/inc/lsegs.inc'
      include '../../../../lib/inc/land_use.inc'

      character*25 calscen    ! set of specifications for this
      integer lencalscen      ! calibration run

      integer numlu,nl  ! number of land uses to act on
      parameter (numlu=25)
      integer lus(numlu) ! index of land uses to act on

      data lus /lfor,lhvf,lhwm,llwm,lhom,lalf,lhyw,lhyo,lpas,
     .          ltrp,lurs,lnhi,lnlo,lnho,lnal,lnhy,lnpa,lrpd,
     .          lnpd,lcpd,lrcn,lccn,lrex,lnex,lcex/

************** uncalibrated land seg liked to surrogate
******************** calibrated land segments
      integer maxorphans,norphans  
      parameter (maxorphans = 100) 
      character*6 orphans(maxorphans)  
      character*6 surrogates(maxorphans)

************ Adjustment factors by calibration site
      real facLandEvap(maxrsegs)  
      real facLZSN(maxrsegs)
      real facINFILT(maxrsegs)
      real facIRC(maxrsegs)
      real facAGWR(maxrsegs)
      real facINTFW(maxrsegs)
      real facAGWETP(maxrsegs)
      real facKVARY(maxrsegs)

      real facLZETP(maxrsegs)
      real facUZSN(maxrsegs)
      
********** Weighting Factor
      integer maxR2L           ! number of river segments having
      parameter (maxR2L = 20)     ! influence on a land seg

      integer nR2L(maxlsegs)  ! # rivers influencing each lseg
      integer R2L(maxlsegs,maxR2L)  ! rivers      
      real weight(maxlsegs,maxR2L)

******************* parameter values
      real LZSN(nlu),AGWR(nlu),IRC(nlu),INTFW(nlu),
     .     INFILT(nlu),AGWETP(nlu),KVARY(nlu),tUZSN,
     .     UZS,
     .     VLEFG(nlu),LZETP(nlu,12),
     .     UZSN(nlu)

************************* Variables To Contain Parameter Factors
      real lufacLZSN(nlu)  ! factor relativer to hom
      real lufacINFILT(nlu)
      real lufacIRC(nlu)
      real lufacAGWR(nlu)
      real lufacINTFW(nlu)
      real lufacAGWETP(nlu)
      real lufacKVARY(nlu)

      integer columnorder(nlu)  ! order to read table

******************* limits
      real limitsLandEvap(4)  ! min, max, HSPFmin, HSPFmax
      real limitsLZSN(4)
      real limitsINFILT(4)
      real limitsIRC(4)
      real limitsAGWR(4)
      real limitsINTFW(4)
      real limitsAGWETP(4)
      real limitsKVARY(4)

      real UZSNfac(nlu)    ! uzsn:lzsn ratio

      logical monthlyUZSN(nlu)   ! does this land use have montly UZSN
      real monthlyUZSNfac(nlu,12)! uzsn monthly factor, if applicable

************ Surface runoff
      real SURO(maxlsegs,nlu)

      real SUROtargets(2)  ! min, max for SURO     

