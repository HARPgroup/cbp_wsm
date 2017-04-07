************************************************************************
************************************************************************
** subroutine organize land to river data and get areas               **
************************************************************************
      subroutine getlseglist(
     I                       ns,nallup,allup,nallland,allland,acres,
     O                       blseglist,blarea,nblseglist)

      include 'flow_weight.inc'
      integer nsup                  ! index to upstream segment
      integer blseglist(maxlsegs)   ! indices of all lsegs in base rseg
      real blarea(maxlsegs)         ! area of intersection
      integer nblseglist            ! number of segments upstream of base rseg
      integer ns,nr,nl,n2
**************END OF DECLARATION ***************************************

      do nl = 1,maxlsegs
        blseglist(nl) = 0
        blarea(nl) = 0.0
      end do

      nblseglist = 0

      do nr = 1,nallup(ns)    ! loop over all upstream segments

        nsup = allup(ns,nr)   ! get index of current upstream segment

        do nl = 1,nallland(nsup)  ! loop over all land in up segs

          found = .false.
          do n2 = 1,nblseglist   ! loop over already found lsegs
            if (allland(nsup,nl).eq.blseglist(n2)) then   ! if found
              blarea(n2) = blarea(n2) + acres(nsup,nl)   ! add to acres
              found = .true.
              exit
            end if
          end do

          if (.not.found) then  ! if not found, add new land segment
            nblseglist = nblseglist + 1
            blseglist(nblseglist) = allland(nsup,nl)
            blarea(nblseglist) = acres(nsup,nl)
          end if

        end do
      end do

      end


