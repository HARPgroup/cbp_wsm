************************************************************************
** subroutines to read all of the data, share common blocks with main **
************************************************************************

************************************************************************
** subroutine organize land to river data and get areas               **
************************************************************************
      subroutine findupstream(
     I                        Tseg,
     I                        nrsegs,uniqindex,uniqid,dsid,
     O                        upstream,nup)
      include 'calib_site.inc'
      integer Tid
      integer n1,n2,n3,n1end
      integer upstream(maxrsegs),nup


******** find list of all upstream segments for each segment
      nup = 1  ! current segment counts as first segment

      read(Tseg(5:8),'(i4)') Tid 
      upstream(nup) = uniqindex(Tid)

      foundnew = .true.

      do while (foundnew)

        foundnew = .false.

        n1end = nup
        do n1 = 1,n1end   ! n1 is index for segments in upstream list
          do n2 = 1,nrsegs  ! n2 is index for all segments to search
            if (dsid(n2).eq.uniqid(upstream(n1))) then !check and add
              found = .false.  ! found = already in list
              do n3 = 1,n1end
                if (n2.eq.upstream(n3)) found = .true.
              end do
              if (.not.found) then
                nup = nup + 1
                upstream(nup) = n2
                foundnew = .true.
              end if
            end if
          end do
        end do

      end do

      sorted = .false.
      do while (.not.sorted)
        sorted = .true.
        do n1 = 1,nup-1
          do n2 = n1+1,nup
            if (dsid(upstream(n2)).eq.uniqid(upstream(n1))) then
              n3 = upstream(n1)
              upstream(n1) = upstream(n2)
              upstream(n2) = n3
              sorted = .false.
            end if
          end do
        end do
      end do

      end


