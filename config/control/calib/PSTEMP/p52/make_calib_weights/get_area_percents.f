************************************************************************
** river info program.  reads in a uniqid and gives the upstream      **
**  rivers, the size of the counties in the entire upstream watershed **
**   and the other calibration sites that are affected by these       **
**   counties                                                         **
************************************************************************
      subroutine get_area_percents(
     I                             ncsegs,csegs,nlsegs,lsegs,
     I                             nallup,allup,nallland,allland,acres,
     O                             lsegpect)

      include 'wtmp_weight.inc'

      integer ns,nl,nr,n2,nc  ! indices
      integer nsup           ! index to upstream segment

      integer blseglist(maxlsegs)   ! indices of all lsegs in base rseg
      real blarea(maxlsegs)         ! area of intersection
      integer nlsg(maxlsegs),nblseglist  ! number of segments upstream of base rseg
      integer nl2,nl3
************** END DECLARATION *****************************************
      do ns = 1,maxrsegs
       basinsize(ns) = 0.0
       do nl =1, maxlsegs
        blseglist(nl) = 0
        blarea(nl) = 0.0
        lsegpect(ns,nl) = 0.0
       end do
      end do

      do ns = 1,ncsegs

       nblseglist = 0
       do nr = 1,nallup(ns)                  ! loop over all upstream segments
        nsup = allup(ns,nr)                  ! get index of current upstream segment

        do nl = 1,nallland(nsup)             ! loop over all land in up segs
         do nl2 =1, nlsegs
         if (lsegs(allland(nsup,nl)) .eq. lsegs(nl2)) then  
          found = .false. 
           do n2 = 1,nblseglist              ! loop over already found lsegs
            if (allland(nsup,nl).eq.blseglist(n2)) then    ! if found
              blarea(n2) = blarea(n2) + acres(nsup,nl)    ! add to acres
              found = .true.
              exit
            end if
           end do

           if (.not.found) then  ! if not found, add new land segment
            nblseglist = nblseglist + 1
            blseglist(nblseglist) = allland(nsup,nl)
            blarea(nblseglist) = acres(nsup,nl)
            nlsg(nblseglist) = nl2
           end if

          end if
         end do
        end do
       end do

       do nl3 = 1, nblseglist
        basinsize(ns) = basinsize(ns) + blarea(nl3)
       end do

       do nl3 = 1, nblseglist
        lsegpect(ns,nlsg(nl3)) = blarea(nl3)/basinsize(ns)
       end do
      
      end do

      return

      end

