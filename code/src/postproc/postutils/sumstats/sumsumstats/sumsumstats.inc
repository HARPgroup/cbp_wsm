      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'

      integer i   ! index

      integer maxsites,ns    ! maximum calibration sites
      parameter (maxsites = 300)

      integer nparms,np     ! number of parameters
      parameter (nparms=14)

      real values(nparms,maxsites)
      integer nvalues,nv

      character*13, title
