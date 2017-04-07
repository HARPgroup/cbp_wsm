************************************************************************
**  subroutine to add double (0003) calibration sites to the list     **
************************************************************************
      subroutine adddoubles(
     I                      rscen,lenrscen,
     M                      csegs,ncsegs)
      include 'calib_site.inc'
      character*13 Tdouble,Tdoubup(3)
      integer nd,ndoubup
      logical foundall

      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)
      fnam = catdir//'geo/'//geoscen(:lengeoscen)//'/doubles.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a100)',err=996,end=997) line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (comment(line)) then
          read(dfile,'(a100)',err=996,end=997) line
          call d2x(line,last)
          cycle
        end if
        read(line,*,err=997,end=997)Tdouble
        ndoubup = 0
        do nd = 1,3
          call shift(line)
          call d2x(line,last)
          if (last.ne.0) then
            read(line,*,err=997,end=997) Tdoubup(nd)
            ndoubup = ndoubup + 1
          end if
        end do

        foundall = .true.  ! run tests

        do nd = 1,ndoubup  ! check for upstream segments
          found = .false.
          do nc = 1,ncsegs
            if (Tdoubup(nd).eq.csegs(nc)) then
              found = .true.
              exit
            end if
          end do
          if (.not.found) then
            foundall = .false.
            exit
          end if
        end do

        found = .false.  ! check for current segment
        do nc = 1,ncsegs
          if (csegs(nc)(:8).eq.Tdouble(:8)) found = .true.
        end do
        if (.not.found) foundall = .false.

        if (foundall) then
          ncsegs = ncsegs + 1
          csegs(ncsegs) = Tdouble
        end if
          
        read(dfile,'(a100)',err=996,end=997) line
        call d2x(line,last)
      end do
      return
************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

996   report(1) = 'problem reading doubles file'
      report(2) = fnam
      report(3) = ' '
      go to 999

997   report(1) = 'Problem reading file:'
      report(2) = fnam
      report(3) = 'literal '//char(39)//'end'//char(39)//' not found'
      go to 999

999   call stopreport(report)

      end

