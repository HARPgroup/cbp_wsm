************************************************************************
** routine to determine the segments immediately upstream of the      **
**  current segment.  Goes with the naming convention of phase5       **
************************************************************************
      subroutine getupstream(
     I                       rseg,rscen,lenrscen,
     O                       upstream,nup)

      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/upstream.inc'

      integer i         ! index

      character*4 uniqid  ! unique ID for the current river

      logical comment,scompare

      uniqid = rseg(5:8)

      do i = 1,nupmax    ! initialization
        upstream(i) = '0'
      end do

      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      fnam=catdir//'geo/'//geoscen(:lengeoscen)//'/rivernames.csv'
      open(dfile,file=fnam,status = 'old',iostat=err)
      if (err.ne.0) go to 991

      nup = 0
      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,1234,err=998,end=997) line
        call d2x(line,last)
        if (.not.comment(line)) then
          call findcomma(line,last)
          line = line(:last-1)
          call trims(line,last)
          if (line(10:13).eq.uniqid) then
            nup = nup + 1
            upstream(nup) = line(:last)
          end if
        end if
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
