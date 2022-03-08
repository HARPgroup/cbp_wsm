************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  scenario for each wdm                                    **
************************************************************************
      subroutine readcontrol_rpa(
     I                           rscen,lenrscen,
     O                           rpascen,
     O                           dorpa)
      implicit none

      include '../wqmlib/transfer_wdm.inc'

      logical comment

      character*(*) rpascen
      logical dorpa

      fnam = controldir//'/river/'//rscen(:lenrscen)//'.con'
      open (11,file=fnam,status='old')

      line = 'GO HOOS'
      dorpa = .false.
      do while (line(:3).ne.'end')
        read(11,'(a100)',err=991)line
        if (.not.comment(line)) then

          if (line(:9).eq.'RPA LOADS') then
            dorpa = .true.
            read(dfile,'(a100)',err=991)line
            call d2x(line,last)
            do while (comment(line))
              read(dfile,'(a100)',err=991)line
              call d2x(line,last)
            end do
            rpascen = line(:last)
            read(dfile,'(a100)',err=991)line
            call d2x(line,last)
            if (line(:13).ne.'END RPA LOADS') go to 992
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
      report(3)=' only allowed one row in table RPA LOADS'
      go to 999

999   call stopreport(report)

      end

