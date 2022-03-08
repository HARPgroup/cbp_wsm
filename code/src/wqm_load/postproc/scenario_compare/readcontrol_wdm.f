************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  scenario for each wdm                                    **
************************************************************************
      subroutine readcontrol_wdm(
     I                         rscen,lenrscen,
     O                         pradscen,psscen,sepscen,ribscen,rpascen,
     O                         doatdep,dops,dosep,dorib,dorpa)
      implicit none

      include 'scencompare.inc'

      logical comment

      character*(*) pradscen, psscen, sepscen, ribscen, rpascen
      logical doatdep,dops,dosep,dorib,dorpa

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (11,file=fnam,status='old')

      line = 'GO HOOS'
      doatdep = .false.
      dops = .false.
      dosep = .false.
      dorib = .false.
      dorpa = .false.
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

          else if (line(:12).eq.'POINT SOURCE') then
            dops = .true.
            read(dfile,'(a100)',err=991)line
            call d2x(line,last)
            do while (comment(line))
              read(dfile,'(a100)',err=991)line
              call d2x(line,last)
            end do
            psscen = line(:last)
            read(dfile,'(a100)',err=991)line
            call d2x(line,last)
            if (line(:16).ne.'END POINT SOURCE') go to 993

          else if (line(:6).eq.'SEPTIC') then
            dosep = .true.
            read(dfile,'(a100)',err=991)line
            call d2x(line,last)
            do while (comment(line))
              read(dfile,'(a100)',err=991)line
              call d2x(line,last)
            end do
            sepscen = line(:last)
            read(dfile,'(a100)',err=991)line
            call d2x(line,last)
            if (line(:10).ne.'END SEPTIC') go to 994

          else if (line(:9).eq.'RIB LOADS') then
            dorib = .true.
            read(dfile,'(a100)',err=991)line
            call d2x(line,last)
            do while (comment(line))
              read(dfile,'(a100)',err=991)line
              call d2x(line,last)
            end do
            ribscen = line(:last)
            read(dfile,'(a100)',err=991)line
            call d2x(line,last)
            if (line(:13).ne.'END RIB LOADS') go to 994

          else if (line(:9).eq.'RPA LOADS') then
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
            if (line(:13).ne.'END RPA LOADS') go to 994

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
