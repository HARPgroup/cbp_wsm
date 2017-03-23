************************************************************************
** variables used frequently in the calibration parameter change prog **
************************************************************************
      include '../../../../lib/inc/rsegs.inc'
      include '../../../../lib/inc/lsegs.inc'
      include '../../../../lib/inc/land_use.inc'

      character*25 calscen    ! set of specifications for this
      integer lencalscen      ! calibration run

      integer numlu,nl  ! number of land uses to act on
      parameter (numlu=25)
      integer lus(numlu) ! index of land uses to act on
      data lus/lfor,lhvf,lpas,ltrp,lurs,lalf,lhyw,lhyo,
     .         lhom,lhwm,llwm,lnhy,lnpa,lnal,lnho,lnhi,
     .         lnlo,lrpd,lnpd,lcpd,lrcn,lccn,lrex,lnex,lcex/ 

************** uncalibrated land seg liked to surrogate
******************** calibrated land segments
      integer maxorphans,norphans  
      parameter (maxorphans = 100) 
      character*6 orphans(maxorphans)  
      character*6 surrogates(maxorphans)

************ Adjustment factors by calibration site and month
      real facASLT(maxrsegs,12)
      real facULTP1(maxrsegs,12)
      real facLGTP1(maxrsegs,12)
      

********** Weighting Factor
      integer maxR2L           ! number of river segments having
      parameter (maxR2L = 20)     ! influence on a land seg

      integer nR2L(maxlsegs)  ! # rivers influencing each lseg
      integer R2L(maxlsegs,maxR2L)  ! rivers      
      real weight(maxlsegs,maxR2L)


******************* parameter values
      real ASLT(nlu,12),ULTP1(nlu,12),LGTP1(nlu,12)

      real BSLT(nlu),ULTP2(nlu),LGTMP(nlu)

************************* Variables To Contain Parameter Factors
      integer columnorder(nlu)  ! order to read table

******************* limits
      real limitsLGTP1(4)  ! min, max, HSPFmin, HSPFmax
      real limitsASLT(4)
      real limitsULTP1(4)

      character*12 module  ! variable specifiers
      integer lenmod

