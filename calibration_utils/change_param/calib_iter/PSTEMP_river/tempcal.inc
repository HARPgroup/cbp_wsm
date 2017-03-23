************************************************************************
** variables used frequently in the calibration parameter change prog **
************************************************************************
      include '../../../../lib/inc/rsegs.inc'

      character*25 calscen    ! set of specifications for this
      integer lencalscen      ! calibration run

************ Adjustment factor for each parameter
      real facKATRAD

******************* limits
      real limitsKATRAD(4)  ! min, max, HSPFmin, HSPFmax

      character*12 module  ! variable specifiers
      character*12 lmodule ! variable specifiers
      integer lenmod
      integer lenlmod

      integer maxitnum,itnum
      parameter (maxitnum=100)
      real efficiency(maxitnum)
      real KATRAD(maxitnum)

      character*13 calseg

