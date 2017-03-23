************************************************************************
** subroutines to read all of the data, share common blocks with main **
************************************************************************

************************************************************************
** subroutine to initialize all important variables                   **
************************************************************************
      subroutine initialize
      include 'basingen.inc'

C      print*,'initializing'    ! initialize
      do ns = 1,maxrsegs
        nallup(ns) = 0
        dsid(ns) = 0
        uniqid(ns) = 0
        rsegs(ns) = ' '
        do ns1 = 1,maxrsegs
          allup(ns,ns1) = 0
        end do
      end do
      do ns = 1,supermax
        uniqindex(ns) = 0
        calsite(ns) = .false.
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

      end




