      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/wdm.inc'
      include '../../../lib/inc/masslinks.inc'

      integer maxcons
      parameter (maxcons = 10)            ! maximum number of constituents in a plot
      integer nplotmax                    ! 
      parameter (nplotmax = 20)           ! maximum number of plots
      integer nplots                      ! number of plots
      character*4 plotname(nplotmax)      ! name of each plot (e.g. 'totn', 'flow')
      character*4 unit(nplotmax)          ! units for plot (e.g. mg/l cfs )
      character*4 norm(nplotmax)          ! normalization variable (usually volume of water)
      integer ncons(nplotmax)             ! number of constituents to each plot
      character*4 con(nplotmax,maxcons)   ! concentration variable to plot
      real confactor(nplotmax,maxcons)    ! factor for the variable

      integer sdate(ndate), edate(ndate)  ! start and end dates in wdm format

      integer pltfil
      parameter (pltfil=40)         ! file number for plotting

      character*25 label(10)     ! plot labels

      character*13 conseg         !paper file segment
