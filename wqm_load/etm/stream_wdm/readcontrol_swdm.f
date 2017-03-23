************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  start and end times (T1's and T2's)                      **
**           number of Land Breaks and Bmp Breaks (nLB's and nBB's)   **
**           Days and Files of Breaks (LB's and BB's)                 **
**           scenario for each land use (LandScen)                    **
************************************************************************
      subroutine readcontrol_swdm(rscen,lenrscen,sdate,edate)
      implicit none

      include 'stream_wdm.inc'

      integer T1year, T1month, T1day, T2year, T2month, T2day

      logical comment

      integer l,n

      integer julian
      external julian,comment

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=993)line
        if (.not.comment(line)) then

          if (line(:4).eq.'TIME') then
            read(dfile,1234) T1year, T1month, T1day
            read(dfile,1234) T2year, T2month, T2day
            read(dfile,'(a100)',err=993)line
            if (line(:8).ne.'END TIME') go to 992
          end if
        end if
      end do

      close (dfile)

********* GET START AND END DATE IN WDM FORMAT
      sdate(1) = T1year
      sdate(2) = T1month
      sdate(3) = T1day
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = T2year
      edate(2) = T2month
      edate(3) = T2day
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0


      return

1234  format(i4,i3,i3,1x,a10)

*********** ERROR SPACE
991   report(1) = '  Problem opening file'
      report(2) = fnam
      report(3) = '  error code = '
      write(report(3)(16:18),'(i3)') err
      go to 999

992   report(1) = ' table TIME should only have two rows in file'
      report(2) = fnam
      report(3) = ' '
      go to 999

993   report(1) =  'Error reading line: in file:'
      report(2) = line
      report(3) = fnam
      go to 999

999   call stopreport(report)

      end
