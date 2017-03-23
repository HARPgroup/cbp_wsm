************************************************************************
** subroutine to find the indices of upstream segments from the index **
**  of a single segment                                               **
**   the basins will not be sorted                                    **
************************************************************************
      subroutine FindUpstreamIndices(
     I                               ns,dsid,uniqid,nrsegs,
     O                               allup,nallup)
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/rsegs.inc'

      integer allup(maxrsegs) ! indices of upstream segments
      integer nallup  ! number of upstream segments
      integer ns,n1,n2,n3,n1end

      logical foundnew,found

******** find list of all upstream segments for each segment
      nallup = 1  ! segment ns counts as first segment
      allup(nallup) = ns
      foundnew = .true.

      do while (foundnew)

        foundnew = .false.

        n1end = nallup
        do n1 = 1,n1end   ! n1 is index for segments in upstream list
          do n2 = 1,nrsegs  ! n2 is index for all segments to search
            if (dsid(n2).eq.uniqid(allup(n1))) then !check and add
              found = .false.  ! found = already in list
              do n3 = 1,n1end
                if (n2.eq.allup(n3)) found = .true.
              end do
              if (.not.found) then
                nallup = nallup + 1
                allup(nallup) = n2
                foundnew = .true.
              end if
            end if
          end do
        end do

      end do

      end

