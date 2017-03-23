************************************************************************
** include file for basingen fortran version of basingen program      **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/upstream.inc'

*********** river variables
      integer maxrsegs,supermax,nrsegs,ns
      parameter (maxrsegs=1000,supermax=9999)

      integer uniqid(maxrsegs)           ! 4 digit ID
      integer dsid(maxrsegs)   ! 4 digit downstream ID
      character*13 rsegs(maxrsegs) ! 13 character ID
      integer uniqindex(supermax) !returns segments index for uniqid

      integer allup(maxrsegs,maxrsegs) ! index of all upstream segments
      integer nallup(maxrsegs)         ! number of all upstream segments

C      logical calsite(supermax)  ! is this a calibration site

******** land variables
      integer maxlsegs,nlsegs,nl
      parameter (maxlsegs=330)
      character*6 lsegs(maxlsegs)  ! land seg identifier
      real lacres(maxlsegs)   ! total acres in a lseg

******** land to water variables
      integer allland(maxrsegs,maxL2R)  ! index of lsegs in each rseg
      integer nallland(maxrsegs)     ! number of land segs in each rseg
      real acres(maxrsegs,maxL2R)   ! area of land/river intersection

      integer blseglist(maxlsegs)! indices of all lsegs in base rseg
      real blarea(maxlsegs)   ! area of intersection 
      integer nblseglist  ! number of segments upstream of base rseg

      integer clseglist(maxlsegs) ! indices of lsegs in current rseg
      real clarea(maxlsegs)   ! area of intersection 
      integer nclseglist ! number of segments upstream of current rseg

******** end important variables

      integer n1,n2,n3,n1end
      integer ns1,nu,ic,nr

      integer oneseg  ! user input

******* end working variables

      logical comment,found,foundnew,sorted
      external comment

      common rsegs,lsegs,nrsegs,uniqid,dsid,uniqindex,allup,nallup,
     .       nlsegs,lacres,allland,nallland,acres,oneseg
C     .      ,calsite
