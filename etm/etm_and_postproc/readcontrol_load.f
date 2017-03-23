************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  start and end times                                      **
**           number of Land Breaks and Bmp Breaks (nLB's and nBB's)   **
**           Days and Files of Breaks (LB's and BB's)                 **
**           scenario for each land use (LandScen)                    **
************************************************************************
      subroutine readcontrol_load(
     I                            rscen,lenrscen,
     O                            nLB,LByear,LBfile)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
 
      integer maxTimeBreaks
      parameter (maxTimeBreaks = 30)
      integer LByear(maxTimeBreaks)             ! year of break
      integer LBmonth(maxTimeBreaks)            ! month
      integer LBday(maxTimeBreaks)              ! day
      character*40 LBfile(maxTimeBreaks)

      character*100 templine(maxTimeBreaks)

      logical comment

      integer nLB,l,n,i

      logical findload

*************** END OF DECLARATION *************************************

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      findload = .false.
      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=1001)line
        call d2x(line,last)
        if (.not.comment(line)) then

          if (line(:13).eq.'AFO CFO LOADS') then
            findload = .true.
            nLB = 0
            read(dfile,'(a100)',err=1001)line
            do while (line(:17).ne.'END AFO CFO LOADS')
              if (.not.comment(line)) then
                nLB = nLB + 1
                templine(nLB) = line
              end if
              read(dfile,'(a100)',err=1001)line
            end do
            do n = 1,nLB
              read(templine(n),1234)
     .              LByear(n),LBmonth(n),LBday(n),LBfile(n)
            end do
          end if

        end if
      end do

      close (dfile)

      if (.not.findload) go to 992

1234  format(i4,i3,i3,1x,a40)

      return
*********** ERROR SPACE ************************************************
991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'did not find afo/cafo load line in control file'
      report(2) = fnam
      report(3) = ' '
      go to 999

1001  report(1) = 'Error reading file after line: '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

999   call stopreport(report)

      end


