************************************************************************
** routine to determine the all of the upstream segments for an rseg  **
**  goes with the particular naming convention of phase5              **
************************************************************************
      subroutine GetAllUpstream(
     I                          nr,dsid,uniqid,nrsegs,
     O                          upsegs,nup)
      implicit none
      include '../inc/standard.inc'
      include '../inc/rsegs.inc'

      integer i         ! index

      logical foundnew,found

      integer upsegs(maxrsegs)  ! index to upstream segments
      integer nup   ! number of upsegs

      integer n1end,n1,n2,n3,nr         ! indices

      nup = 1  ! segment nr counts as first segment
      upsegs(nup) = nr
      
      foundnew = .true.

      do while (foundnew)

        foundnew = .false.

        n1end = nup
        do n1 = 1,n1end   ! n1 is index for segments in upstream list
          do n2 = 1,nrsegs  ! n2 is index for all segments to search
            if (dsid(n2).eq.uniqid(upsegs(n1))) then !check and add
              found = .false.  ! found = already in list
              do n3 = 1,n1end
                if (n2.eq.upsegs(n3)) found = .true.
              end do
              if (.not.found) then
                nup = nup + 1
                upsegs(nup) = n2
                foundnew = .true.
              end if
            end if
          end do
        end do

      end do

      return

***********    ERROR  SPACE        *************************************

991   report(1) = 'could not open file'
      report(2) =    fnam
      write(report(3),*) 'error = ',err
      go to 999

997   report(1) = 'End of file reached '
      report(2) =    fnam
      report(3) = '  without finding literal => end'
      go to 999

998   report(1) = 'Problem with reading file'
      report(2) =    fnam
      report(3) = ' near line '//line
      go to 999

999   call stopreport(report)

1234  format (a100)

      end
