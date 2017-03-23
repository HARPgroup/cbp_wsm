************************************************************************
** include file for basingen fortran version of basingen program      **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/upstream.inc'
      include '../../lib/inc/rsegs.inc'
      include '../../lib/inc/lsegs.inc'

*********** river variables
      integer nrproc  ! number of rsegs to process
      integer rproc(maxrsegs)  ! list of indices of rsegs to process
      integer ns

      integer allup(maxrsegs,maxrsegs) ! index of all upstream segments
      integer nallup(maxrsegs)         ! number of all upstream segments

      logical calsite(supermax)  ! is this a calibration site
      character*13 calsegs(maxrsegs)
      integer ncal

******** land variables
      integer nl
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
     .      ,calsite
