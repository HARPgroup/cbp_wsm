************************************************************************
** reads in a .calib seglist and determines which ones do not have    **
**   any of the segments listed in the same seglist that are upstream **
** Useful for domain decomposition                                    **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/rsegs.inc'

      integer ncsegs ! calibration segments. Can include 0003 segs
      character*13 csegs(maxrsegs)  ! name
      integer csegindex(maxrsegs)   ! index for rsegs
      logical csegcon(maxrsegs)     ! is this a confluence?

      integer nfirstorder
      character*13 firstorder(maxrsegs)

      character*100 seglist

      integer csegid

      integer upsegs(maxrsegs),nup

      logical foundup

      integer nc,nc2,n  ! indices

*********************** END DECLARATIONS ******************************
      read*,rscen,seglist
      call lencl(rscen,lenrscen)

******** read in all rsegs
      call getrsegs(
     I              rscen,lenrscen,
     O              rsegs,uniqid,dsid,uniqindex,nrsegs)

*********** read in calibration stations from the supplied seglist
      call readCalibSeglist(
     I                      seglist,
     O                      csegs,ncsegs)

      nfirstorder = 0
********* check each cseg for first-orderness
*********** determine the list of upstream rsegs for each cseg and 
******** determine if any uniqid in the upstream rsegs matches the 
******** uniqid of another cseg in the list

********** get the index of each cseg and determine if it is a
********** confluence
      do nc = 1,ncsegs
        read(csegs(nc)(5:8),'(i4)') csegid 
        csegindex(nc) = uniqindex(csegid)
        csegcon(nc) = .false.
        if (csegs(nc)(10:13).eq.'0003') csegcon(nc) = .true.
      end do

********* loop over csegs and determine firstorder-ness
      do nc = 1,ncsegs

********* populate upstream and nup for each cseg
        call GetAllUpStream(
     I                      csegindex(nc),dsid,uniqid,nrsegs,
     O                      upsegs,nup)

        if (csegcon(nc)) then  ! remove cseg from upstream if conseg
          do n = 1,nup-1
            upsegs(n) = upsegs(n+1)
          end do
          nup = nup - 1
        end if 

*********** check if firstorder
        foundup = .false.        
        do nc2 = 1,ncsegs
          if (nc2.eq.nc) cycle ! do not find self
          do n = 1,nup ! search upstream and find match
            if (upsegs(n).eq.csegindex(nc2)) then 
              foundup = .true.
              exit
            end if
          end do
          if (foundup) exit
        end do
        if (.not.foundup) then
          nfirstorder = nfirstorder + 1
          firstorder(nfirstorder) = csegs(nc)
        end if
      end do

*********** write out results
      seglist = 'FirstOrderOnly_'//seglist
      call WriteCalibSeglist(
     I                       seglist,
     O                       firstorder,nfirstorder)

      stop
999   call stopreport(report)

      end

