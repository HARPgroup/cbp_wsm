************************************************************************
** include file for basingen fortran version of basingen program      **
************************************************************************
      implicit none
      include '../../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../../code/src/lib/inc/rsegs.inc'
      include '../../../../../../code/src/lib/inc/lsegs.inc'

*********** scenario info
      character*25 calscen,obscen
      integer lencalscen,lenobscen
*********** river variables
      integer maxcsegs  ! maximum calibration stations
      parameter (maxcsegs = 300)

      integer maxrservoir
      parameter (maxrservoir = 50)

      integer ncsegs
      character*13 csegs(maxrsegs)  ! calibration segments
                               ! can include 0003 segs
*********** output variables
      real weight(maxlsegs,maxrsegs),sumweight,stationarea

*********** river variables
      integer allup(maxcsegs,maxrsegs) ! index of all upstream segments
      integer nallup(maxrsegs)         ! number of all upstream segments
      integer upstream(maxrsegs),nup

      logical calsite(supermax)  ! is this a calibration site
      logical double(supermax)  ! is this a double

      logical comment,found,foundnew,sorted
      external comment

********* info about the rsegs
      real basinsize(maxrsegs),lsegpect(maxrsegs,maxlsegs)

      integer clseglist(maxlsegs) ! indices of lsegs in current rseg
      real clarea(maxlsegs)   ! area of intersection
      integer nclseglist ! number of segments upstream of current rseg

      integer obs(maxrsegs) ! number of obs for cseg/conc pair
      integer goodobs(maxrsegs) ! number of obs above LOD

************** uncalibrated land seg liked to surrogate
******************** calibrated land segments
      integer maxorphans,norphans
      parameter (maxorphans = 100)
      character*6 orphans(maxorphans)
      character*6 surrogates(maxorphans)

      character*12 module  ! variable specifiers
      integer lenmod

*************** land variables
      integer allland(maxrsegs,maxL2R)  ! index of lsegs in each rseg
      integer nallland(maxrsegs)     ! number of land segs in each rseg
      real acres(maxrsegs,maxL2R)   ! area of land/river intersection

