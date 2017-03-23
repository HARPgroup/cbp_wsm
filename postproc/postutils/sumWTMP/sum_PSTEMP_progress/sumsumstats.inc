      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'

      integer i   ! index

      integer maxsites,ns    ! maximum calibration sites
      parameter (maxsites = 300)

      integer nparms,np     ! number of parameters
      parameter (nparms=24)

      real values(maxsites)
      integer nvalues,nv
      real allvalues(nparms,maxsites)
      integer nallvalues(nparms)

      character*20, title,scenario

      character*4 version


