************************************************************************
** one-off program to read a wdm and create an hourly ascii file      **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'

      integer sdate(ndate),edate(ndate)
      integer wdmget
      parameter (wdmget=13)

      integer lenwdm

      integer year,month,day,hour,n,i

      character*4 cdsn

      data sdate /1984,1,1,0,0,0/
      data edate /2005,12,31,24,0,0/

      double precision annave, hourave

******************** END SPECIFICATIONS ********************************

      print*,' program to write hourly ascii output from a wdm'
      print*,'wdm name, start year, end year, dsn'

      read(*,*,err=992,end=992) wdmfnam, sdate(1), edate(1), dsn

********* open and read wdm
      call wdbopn(wdmget,wdmfnam,1,err)  ! open read only
      if (err.ne.0) go to 997

      call gethourdsn(wdmget,sdate,edate,dsn,nvals,hval)

********* name and open ascii file
      call lencl(wdmfnam,lenwdm)
      write(cdsn,'(i4)') dsn
      do i = 1,3
        if (cdsn(i:i).eq.' ') cdsn(i:i) = '0'
      end do
      fnam = wdmfnam(:lenwdm-4)//'_'//cdsn//'.csv'

      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

******** loop over all time and write file
      annave = 0
      year = sdate(1)
      month = sdate(2)
      day = sdate(3)
      hour = sdate(4)
      do n = 1,nvals
        write(dfile,*) year,',',month,',',day,',',hour,',',hval(n)
        annave = annave + hval(n)
        call onehour(year,month,day,hour)
      end do
      close(dfile)

********* print annual average for check
      hourave = annave / real(nvals)
      annave = annave / real(edate(1)-sdate(1)+1)
      print*,'hourly average = ',hourave
      print*,'annual average = ',annave

      stop

********************************* ERROR SPACE **************************
991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'input not in correct format'
      report(2) = ' '
      report(3) = ' '
      go to 999

997   report(1) = wdmfnam
      if (err.eq.0) then
        report(2) = ' is not a wdm file'
      else if (err.eq.-2) then
        report(2) = ' does not exist'
      else
        report(2) = 'Error: opening wdm= '
        write(report(2)(22:24),'(i3)')err
      end if
      report(3) = ' '
      go to 999


999   call stopreport(report)

      end


