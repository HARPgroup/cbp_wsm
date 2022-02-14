      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/wdm.inc'
      include '../lib/inc/masslinks.inc'

      integer maxcons
      parameter (maxcons = 10)            ! maximum number of constituents in a load
      integer nloadmax                    ! 
      parameter (nloadmax = 20)           ! maximum number of loads
      integer nloads                      ! number of loads
      character*4 loadname(nloadmax)      ! name of each load (e.g. 'totn', 'flow')
      character*4 unit(nloadmax)          ! units for load (e.g. mg/l cfs )
      integer ncons(nloadmax)             ! number of constituents to each load
      character*4 con(nloadmax,maxcons)   ! concentration variable to load
      real confactor(nloadmax,maxcons)    ! factor for the variable

      integer sdate(ndate), edate(ndate)  ! start and end dates in wdm format

      integer pltfil
      parameter (pltfil=40)         ! file number for loadting

      character*25 label(10)     ! load labels

      character*13 conseg  ! confluence segment
