************************************************************************
** variables used frequently in the calibration parameter change prog **
************************************************************************
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include '../../../../lib/inc/lsegs.inc'
      include '../../../../lib/inc/land_use.inc'

      character*25 calscen    ! set of specifications for this
      integer lencalscen      ! calibration run

************ Adjustment factors by calibration site
      real targets(maxlsegs)
      real facKSER(maxlsegs)  
      real facNVSI,oldNVSI
************ Variables To Contain Parameter Rules
      real lufacAFFIX  
      real lufacNVSI
      real lufacKSKR
      real NVSIfrac

******************* parameter values
      real AFFIX
      real NVSI,KRER,KSER,oldKSER
      integer columnorder  ! order to read table

******************* limits
      real limitsKRER(4)  ! min, max, HSPFmin, HSPFmax
      real limitsKSER(4)

