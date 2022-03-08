************************************************************************
** subroutine to find the upstream river segs from a point            **
***  without including anything upstream of an upstream reservoir     **
************************************************************************
      subroutine FindResBasin(
     I                        ns,dsid,uniqid,lakeflags,nrsegs,
     O                        allup,nallup,resup,nresup)
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/rsegs.inc'

      integer allup(maxrsegs) ! indices of upstream segments
      integer nallup  ! number of upstream segments
      integer resup(maxrsegs) ! indices of upstream reservoirs
      integer nresup  ! number of upstream reservoirs

      integer ns,n1,n2,n3,n1end,nres
      integer lakeflags(maxrsegs)  ! 0 if not reservoir

      logical foundnew,found

******** find list of all upstream segments for each segment

      nallup = 1  ! segment ns counts as first segment
      allup(nallup) = ns

      nresup = 0  ! no upstream reservoirs yet

      foundnew = .true.
      print *, dsid

      do while (foundnew)

        foundnew = .false.

        n1end = nallup
        do n1 = 1,n1end   ! n1 is index for segments in upstream list
          do n2 = 1,nrsegs  ! n2 is index for all segments to search

            if (dsid(n2).eq.uniqid(allup(n1))) then !check and add

              if (lakeflags(n2).ne.0) then  ! reservoir

                found = .false.  ! check to see if in list
                do nres = 1,nresup
                  if (resup(nres).eq.n2) found = .true.
                end do
                if (found) cycle  ! already got this reservoir

                nresup = nresup + 1   ! passed test, add reservoir
                resup(nresup) = n2
                cycle ! reservoir - do not add

              end if

              found = .false.  ! found = already in list
              do n3 = 1,n1end
                if (n2.eq.allup(n3)) found = .true.
              end do
              if (found) cycle   ! already there

              nallup = nallup + 1   ! passed tests, add this seg
              allup(nallup) = n2
              foundnew = .true.

            end if
          end do
        end do

      end do

      end

