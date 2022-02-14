      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/wdm.inc'
      include '../../../lib/inc/masslinks.inc'

      integer maxcons                ! max # of constituents in a load
      parameter (maxcons = 10)       ! or concentration 

      integer nconcmax,nconcs        ! number of concs
      parameter (nconcmax = 20)      ! max number of concentrations
      character*4 concname(nconcmax) ! name of each conc (e.g. 'totn')
      character*4 cunit(nconcmax)     ! units for conc (e.g. mg/l cfs )
      integer nccons(nconcmax)        ! # of constituents to each conc
      character*4 ccon(nconcmax,maxcons) ! conc constituent variable
      real cconfactor(nconcmax,maxcons)  ! factor for the variable
      character*4 norm(nconcmax)     ! normalization variable 
                                     !  (usually volume of water)

      character*25 obscen
      integer lenobscen

********* the following flow variables are paired sim-obs days
********  they exist for days within the averaging period, but may
*******  not be sequential, the date variables hold the dates
      real obflow(ndaymax)
      real bobs(ndaymax)
      real qobs(ndaymax)
      integer obyear(ndaymax)      
      integer obmonth(ndaymax)
      integer obday(ndaymax)
      integer obhour(ndaymax)
      logical obLOD(ndaymax)  ! is this observation at LOD

      real obfl(1984:2005,12,31)     ! obs flows convenient format
      real bofl(1984:2005,12,31)  ! base obs
      real qofl(1984:2005,12,31)  ! quick obs

      integer pltfil
      parameter (pltfil=40)         ! file number for load

      character*25 label(10)     ! load labels

      character*13 conseg         !paper segment

      integer window     ! number of hours before and after measurement
                         ! to check simulated for agreement, 
                         ! accuracy is limited by flow, 
                         ! so same for each constituent

      logical FloObsExist   ! do flow observations exist

