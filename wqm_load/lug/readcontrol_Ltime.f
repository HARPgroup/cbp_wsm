************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  start and end times (T1's and T2's)                      **
**           number of Land Breaks and Bmp Breaks (nLB's and nBB's)   **
**           Days and Files of Breaks (LB's and BB's)                 **
**           scenario for each land use (LandScen)                    **
************************************************************************
      subroutine readcontrol_Ltime(
     I                             lscen,lenlscen,
     O                             startY,startM,startD,
     O                             endY,endM,endD)
      implicit none

      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'

      logical comment
      external comment

      integer l,n

      integer startY,startM,startD,endY,endM,endD

      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      open (11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 993

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(11,'(a100)',err=991)line
        if (.not.comment(line)) then

          if (line(:4).eq.'TIME') then
            read(11,1234) startY,startM,startD
            read(11,1234) endY,endM,endD
            read(11,'(a100)',err=991)line
            if (line(:8).ne.'END TIME') go to 992
            return
          end if
        end if
      end do

      close (11)

1234  format(i4,i3,i3,1x,a10)

*********** ERROR SPACE
991   report(1) = 'ERROR reading line in file '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

992   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3) = ' only allowed two rows in table TIME'
      go to 999

993   report(1) = 'could not open land control file'
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end
