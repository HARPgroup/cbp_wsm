************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  scenario for each wdm                                    **
************************************************************************
      subroutine readcontrol_wdm(
     I                           rscen,lenrscen,
     O                           pradscen,
     O                           doatdep)
      implicit none

      include 'data.inc'

      logical comment

      character*(*) pradscen
C      character*(*) psscen, sepscen
      logical doatdep,dops,dosep

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (11,file=fnam,status='old')

      line = 'GO HOOS'
      doatdep = .false.
      dops = .false.
      dosep = .false.
      do while (line(:3).ne.'end')
        read(11,'(a100)',err=991)line
        if (.not.comment(line)) then

          if (line(:23).eq.'PRECIP ATMOS DEPOSITION') then
            doatdep = .true.
            read(dfile,'(a100)',err=991)line
            call d2x(line,last)
            do while (comment(line))
              read(dfile,'(a100)',err=991)line
              call d2x(line,last)
            end do
            pradscen = line(:last)
            read(dfile,'(a100)',err=991)line
            call d2x(line,last)
            if (line(:27).ne.'END PRECIP ATMOS DEPOSITION') go to 992

          end if
        end if
      end do

      close (11)

      return

*********** ERROR SPACE
991   report(1) = 'ERROR reading line in file '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

992   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3)=' only allowed one row in table PRECIP ATMOS DEPOSITION'
      go to 999

993   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3)=' only allowed one row in table POINT SOURCE'
      go to 999

994   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3)=' only allowed one row in table SEPTIC'
      go to 999

999   call stopreport(report)

      end
