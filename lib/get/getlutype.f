************************************************************************
** finds pervious or impervious from the scen file                    **
************************************************************************
      subroutine getlutype(lscen,lenlscen,clu,perlnd,implnd)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'

      character*3 clu             ! land use

      logical perlnd,implnd,comment

      integer i             ! index

      perlnd = .false.
      implnd = .false.

      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a100)')line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line).and.line(:11).eq.'PER MODULES') then
          read(dfile,'(a100)')line
          call d2x(line,last)
          i = 1
          do while (line(i:i+2).ne.'   ')
            if (line(i:i+2).eq.clu) perlnd = .true.
            i = i + 4 
          end do
        end if
        if (.not.comment(line).and.line(:11).eq.'IMP MODULES') then
          read(dfile,'(a100)')line
          call d2x(line,last)
          i = 1
          do while (line(i:i+2).ne.'   ')
            if (line(i:i+2).eq.clu) implnd = .true.
            i = i + 4 
          end do
        end if
        read(dfile,'(a100)')line
        call d2x(line,last)
      end do
      close(dfile)

      if (.not.(perlnd.or.implnd)) go to 992

      if (perlnd.and.implnd) go to 993

      return

************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(3) = 'land use '//clu//' not specified as PERLND or IMPLND'
      go to 998
993   report(3)='land use '//clu//' specified as both PERLND and IMPLND'
998   report(1) = ' Problem in file '
      report(2) = fnam
      go to 999

999   call stopreport(report)

      end


