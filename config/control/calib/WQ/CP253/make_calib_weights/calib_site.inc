************************************************************************
** include file for basingen fortran version of basingen program      **
************************************************************************
      implicit none
      include '../../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../../code/src/lib/inc/rsegs.inc'
      include '../weights.inc'

*********** river variables
      integer allup(maxcsegs,maxrsegs) ! index of all upstream segments
      integer nallup(maxcsegs)         ! number of all upstream segments

      logical calsite(supermax)  ! is this a calibration site
      logical double(supermax)  ! is this a double

      logical comment,found,foundnew,sorted
      external comment

********* info about the rsegs
      real rsegarea(maxrsegs) ! size of the segment
      integer obs(maxcsegs,nconcmax) ! number of obs for cseg/conc pair
      integer goodobs(maxcsegs,nconcmax) ! number of obs above LOD


