      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'

      integer i   ! index
      character*300 longline

      integer maxsites,ns    ! maximum calibration sites
      parameter (maxsites = 300)

      integer nm

      character*13 segment(maxsites)

      integer nparms,np     ! number of parameters
      parameter (nparms=2)
      character*4 parname(nparms)  ! names of parameters
      data parname /'quik','base'/
      integer fnum(nparms)  ! file number
      data fnum /11,12/

      real values(nparms,maxsites,12)

