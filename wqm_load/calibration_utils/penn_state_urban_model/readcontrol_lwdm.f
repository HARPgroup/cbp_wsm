************************************************************************
** given the land scenario, finds the scenario names for the land wdms**
************************************************************************
      subroutine readcontrol_lwdm(
     I                            lscen,
     O                            pradscen)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

      logical comment
      character*25 pradscen

      logical findprad

      data findprad
     .     /.false./

      call lencl(lscen,lenlscen)

      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      open (dfile,file=fnam,status='old')

      line = 'GO HOOS'
      do while (line(:3).ne.'end')

        read(dfile,'(a100)',err=1001)line
        if (.not.comment(line)) then

          else if (line(:23).eq.'PRECIP ATMOS DEPOSITION') then
            findprad = .true.
            read(dfile,'(a25)') pradscen
            call d2x(pradscen,last)
            read(dfile,'(a100)',err=1001)line
            do while (comment(line))
              read(dfile,'(a100)',err=1001)line
            end do
            if (line(:27).ne.'END PRECIP ATMOS DEPOSITION') then
              print*,' control file ',fnam,
     .          ' only allowed one row in table PRECIP ATMOS DEPOSITION'
              call stopit(fnam,'PRECIP ATMOS DEPOSITION')
            end if

          end if
        end if

      end do
      close (dfile)

      if (.not.findprad) call stopit(fnam,'PRECIP ATMOS DEPOSITION')

      return

*********** ERROR SPACE
1001  report(1) = 'Error reading file after line: '
      report(2) = fnam
      report(3) = line(:64)
      call stopreport(report)

      end


