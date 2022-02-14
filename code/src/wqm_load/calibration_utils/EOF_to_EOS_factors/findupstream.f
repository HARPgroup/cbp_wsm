************************************************************************
** subroutine to find the upstream river segs from a flux station     **
***  without including anything upstream of another flux station      **
************************************************************************
      subroutine findupstream(
     I                        station,nloads,nl,
     I                        maxstations,staname,nstations,
     I                        rsegs,uniqid,dsid,uniqindex,nrsegs,
     O                        upsegs,nupsegs,
     O                        upstations,nupstations)

      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/rsegs.inc'

      integer nloads,nl
**************** input variables about the stations
      integer maxstations,nstations
      character*13 station   ! the station for this call of the routine
      character*13 staname(maxstations,nloads) ! all stations

************** output variables
      integer nupstations            ! number of upstream stations
      integer upstations(maxstations)
      integer nupsegs  ! number of upstream segs (DS of other stations)
      integer upsegs(maxrsegs) ! upstream segs indices

*************** other variables
      integer ns,n1,n2,n1end,Tuniqid
      logical found, foundnew, foundsta, IsStation

*************** END DECLARATIONS

      nupstations = 0

      read(station(5:8),'(i4)') Tuniqid
      nupsegs = 1        ! station segment counts as first segment
      upsegs(nupsegs) = uniqindex(Tuniqid)
      foundnew = .true.

      do while (foundnew)

        foundnew = .false.

************** loop over segs in list and see if any upstream match
        n1end = nupsegs
        do n1 = 1,n1end   ! n1 is index for segments in upstream list
          do n2 = 1,nrsegs  ! n2 is index for all segments to search
            if (dsid(n2).eq.uniqid(upsegs(n1))) then 
                     

**************** found upstream match, check to see if already in list
              found = .false.  ! found = already in list
              do n3 = 1,n1end
                if (n2.eq.upsegs(n3)) found = .true.
              end do

              if (.not.found) then

**************** check to see if this is an upstream station
                IsStation = .false.
                do ns = 1,nstations
                  read(staname(ns,nl)(5:8),'(i4)') Tuniqid 
                  if (uniqid(n2).eq.Tuniqid) then   ! station
                    IsStation = .true.

********************** check to see if already have station
                    foundsta = .false.
                    do n4 = 1,nupstations
                      if (upstations(n4).eq.ns) foundsta = .true.
                    end do
                    if (.not.foundsta) then
                      nupstations = nupstations + 1
                      upstations(nupstations) = ns
                    end if

                  end if

                end do
                      
                if (.not.IsStation) then
                  nupsegs = nupsegs + 1
                  upsegs(nupsegs) = n2
                  foundnew = .true.
                end if
              end if
            end if
          end do
        end do

      end do

****************** add upstream 0003 segments to upsegs
      do ns = 1,nupstations
        if (staname(upstations(ns),nl)(10:13).eq.'0003') then
          nupsegs = nupsegs + 1
          read(staname(upstations(ns),nl)(5:8),'(i4)') Tuniqid
          upsegs(nupsegs) = uniqindex(Tuniqid)
        end if
      end do

**************** if downstream station is an 0003 seg, take it out
      if (station(10:13).eq.'0003') then
        nupsegs = nupsegs - 1
        do nr = 1,nupsegs
          upsegs(nr) = upsegs(nr+1)
        end do
      end if

      end

