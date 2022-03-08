      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'

      character*64 wdmgname,wdmpname,wdm0103,wdm0405

      integer sdate(ndate),edate(ndate)
      integer wdmget,wdmput
      parameter (wdmget=13,wdmput=12)

      integer n,nd,dsnget

      integer metregion    ! 10, 20, ... 70
      character*1 mregion2     ! 1,2,3, ... 7

      integer sdate03(ndate),sdate05(ndate)
      integer edate03(ndate),edate05(ndate)

      real dv2(ndaymax),hv2(ndaymax*24)
      integer nv2

      integer missing

      integer julian
      external julian

      integer starthour,endhour

******************** END SPECIFICATIONS ********************************

      read*,lseg,sdate(1),edate(1)

      wdmpname = 'met_'//lseg//'.wdm'

      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      wdmgname = dummyWDMname

      call wdbopn(wdmget,wdmgname,0,err)  ! open read/write 
      if (err.ne.0) go to 997

      call wdbopn(wdmput,wdmpname,0,err)  ! open read/write 
      if (err.ne.0) go to 998

*********** get the hours from start date
********* starts on the first hour of 1/18/1996
********* ends on the last hour of 1/21/1996
      starthour = julian(sdate(1),sdate(2),sdate(3),1996,1,17)
      endhour = julian(sdate(1),sdate(2),sdate(3),1996,1,21)
      starthour = starthour*24 + 1
      endhour = endhour*24

*********** ATMP **********
      dsn = 1004
      print*,lseg,' ',dsn,' ATMP'
      call gethourdsn(wdmput,sdate,edate,dsn,nvals,hval)
      do n = starthour,endhour  ! 1/18/1996 to 1/21/1996
        hval(n) = hval(n) + 10.0
      end do
      call puthourdsn(wdmput,sdate,edate,dsn,nvals,hval)
      call wdflcl (wdmput,err)
      if (err.ne.0) go to 996

      stop

********************************* ERROR SPACE **************************
991   report(1) = 'too many missing values'
      report(2) = 'stopping program'
      report(3) = ' '
      go to 999

994   report(1) = wdm0103
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
995   report(1) = wdm0405
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

996   report(1) = 'Error: closing wdm = '
      write(report(1)(22:24),'(i3)')err
      report(2) = ' '
      report(3) = ' '
      go to 999

997   report(1) = wdmgname
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

998   report(1) = wdmpname
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


