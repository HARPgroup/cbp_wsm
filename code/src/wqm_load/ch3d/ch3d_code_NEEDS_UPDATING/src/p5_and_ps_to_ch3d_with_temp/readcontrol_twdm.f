************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  scenario for each wdm                                    **
************************************************************************
      subroutine readcontrol_wdm(
     I                           rscen,lenrscen,
     O                           pradscen,psscen,sepscen,
     O                           doatdep,dops,dosep)
      implicit none

      include 'transfer_wdm.inc'

      logical comment

      character*(*) pradscen, psscen, sepscen
      logical doatdep,dops,dosep

      fnam = tree//'run/control/river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old')

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

          end if
        end if
      end do

      close (dfile)

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

************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  start and end times (T1's and T2's)                      **
**           scenario for each land use (LandScen)                    **
************************************************************************
      subroutine readcontrol_twdm(rscen,lenrscen,
     .                            LandScen)
      implicit none

      include 'transfer_wdm.inc'

      character*3 clu                            ! character land use

      logical comment

      integer l,n

      fnam = tree//'run/control/river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old')

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=1001)line
        if (.not.comment(line)) then

          if (line(:13).eq.'LAND SCENARIO') then
            read(dfile,'(a100)',err=1001)line
            do while (line(:17).ne.'END LAND SCENARIO')
              if (.not.comment(line)) then
                clu = line(:3)
                do l = 1,nlu
                  if (clu.eq.luname(l)) LandScen(l) = line(5:29)
                end do
              end if
              read(dfile,'(a100)',err=1001)line
            end do

          end if
        end if
      end do

      close (dfile)

      return

1234  format(i4,i3,i3,1x,a10)

*********** ERROR SPACE
1001  report(1) = 'Error reading line in file:  line:'
      report(2) = fnam
      report(3) = line(:64)

      call stopreport(report)
      return

      end
