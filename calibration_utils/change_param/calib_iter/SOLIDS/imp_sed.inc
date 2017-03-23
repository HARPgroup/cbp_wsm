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
      real facKEIM(maxlsegs)  

******************* parameter values
      real KEIM
      
      integer columnorder  ! order to read table

