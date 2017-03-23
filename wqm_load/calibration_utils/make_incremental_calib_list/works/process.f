************************************************************************
** subroutines to read all of the data, share common blocks with main **
************************************************************************

************************************************************************
** subroutine to initialize all important variables                   **
************************************************************************
      subroutine initialize
      include 'basingen.inc'

      print*,'initializing'    ! initialize
      do ns = 1,maxrsegs
        nallup(ns) = 0
        dsid(ns) = 0
        uniqid(ns) = 0
        rsegs(ns) = ' '
        do ns1 = 1,maxrsegs
          allup(ns,ns1) = 0
        end do
        do ns1 = 1,maxL2R
          allland(ns,ns1) = 0
          acres(ns,ns1) = 0.0
        end do
        nallland(ns) = 0
      end do
      do nl = 1,maxlsegs
        lacres(nl) = 0.0
      end do
      do ns = 1,supermax
        uniqindex(ns) = 0
C        calsite(ns) = .false.
      end do
        

      end

************************************************************************
** subroutine organize land to river data and get areas               **
************************************************************************
      subroutine getlseglist(ns,
     O                       blseglist,blarea,nblseglist)
      include 'basingen.inc'
      integer nsup  ! index to upstream segment

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


************************************************************************
** subroutine organize land to river data and get areas               **
************************************************************************
      subroutine findupstream(ns)
      include 'basingen.inc'

******** find list of all upstream segments for each segment
      nallup(ns) = 1  ! segment ns counts as first segment
      allup(ns,nallup(ns)) = ns
      foundnew = .true.

      do while (foundnew)

        foundnew = .false.

        n1end = nallup(ns)
        do n1 = 1,n1end   ! n1 is index for segments in upstream list
          do n2 = 1,nrsegs  ! n2 is index for all segments to search
            if (dsid(n2).eq.uniqid(allup(ns,n1))) then !check and add
              found = .false.  ! found = already in list
              do n3 = 1,n1end
                if (n2.eq.allup(ns,n3)) found = .true.
              end do
              if (.not.found) then
                nallup(ns) = nallup(ns) + 1
                allup(ns,nallup(ns)) = n2
                foundnew = .true.
              end if
            end if
          end do
        end do

      end do

      sorted = .false.
      do while (.not.sorted)
        sorted = .true.
        do n1 = 1,nallup(ns)-1
          do n2 = n1+1,nallup(ns)
            if (dsid(allup(ns,n2)).eq.uniqid(allup(ns,n1))) then
              n3 = allup(ns,n1)
              allup(ns,n1) = allup(ns,n2)
              allup(ns,n2) = n3
              sorted = .false.
            end if
          end do
        end do
      end do

C      print*,'nallup(ns) ' , nallup(ns)

      end




