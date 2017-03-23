************************************************************************
** include file for basingen fortran version of basingen program      **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/upstream.inc'
      include '../../lib/inc/rsegs.inc'

*********** river variables
      integer ns

      integer allup(maxrsegs,maxrsegs) ! index of all upstream segments
      integer nallup(maxrsegs)         ! number of all upstream segments

      logical calsite(supermax)  ! is this a calibration site
      logical double(supermax)  ! is this a double

******** end important variables

      integer n1,n2,n3,n1end
      integer ns1,nu,ic,nr

      character*13 oneseg  ! user input

******* end working variables

      logical comment,found,foundnew,sorted
      external comment

      common rsegs,nrsegs,uniqid,dsid,uniqindex,allup,nallup,
     .       calsite
