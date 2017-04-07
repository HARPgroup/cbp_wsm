************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  start and end times (T1's and T2's)                      **
**           scenario for each land use (LandScen)                    **
************************************************************************
      subroutine readcontrol_LandScen(rscen,lenrscen,LandScen)
      implicit none

      include 'transfer_wdm.inc'

      character*3 clu                            ! character land use

      logical comment

      integer l,n

      external comment

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
