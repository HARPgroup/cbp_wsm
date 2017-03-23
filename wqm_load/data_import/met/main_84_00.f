      implicit none
      include '../../../inc/standard.inc'
      include '../../../inc/locations.inc'
      include '../../../inc/wdm.inc'

      character*64 wdmgname,wdmpname

      integer sdate(ndate),edate(ndate)
      integer wdmget,wdmput
      parameter (wdmget=12,wdmput=13)

      integer n,nd,dsnget

      integer metregion    ! 10, 20, ... 70

******************** END SPECIFICATIONS ********************************

      read*,lseg

      wdmgname = 'p4met00.wdm'
      wdmpname = 'met_'//lseg//'.wdm'

      call getmetreg(tree,lseg,metregion)
      print*,lseg,metregion

      sdate(1) = 1984
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = 2000
      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      call wdbopnlong(wdmget,wdmgname,1,err)  ! open read only
      if (err.ne.0) go to 997
      call wdbopnlong(wdmput,wdmpname,0,err)  ! open read/write 
      if (err.ne.0) go to 998

C*********** EVAP **********
C      dsn = 1000
C      dsnget = metregion
C      print*,lseg,' ',dsn,dsnget,' EVAP'
C      call getdailydsn(wdmget,sdate,edate,dsnget,nvals,dval)
C      do n = 1,nvals
C        do nd = 1,24
C          hval((n-1)*24+nd) = dval(n)/24.0
C        end do
C      end do
C      nvals = nvals * 24
C      call puthourdsn(wdmput,sdate,edate,dsn,nvals,hval)

*********** DEWP **********
      dsn = 1001
      dsnget = metregion + 1
      print*,lseg,' ',dsn,dsnget,' DEWP'
      call getdailydsn(wdmget,sdate,edate,dsnget,nvals,dval)
      call putdailydsn(wdmput,sdate,edate,dsn,nvals,dval)

*********** CLDC **********
      dsn = 1005
      dsnget = metregion + 2
      print*,lseg,' ',dsn,dsnget,' CLDC'
      call getdailydsn(wdmget,sdate,edate,dsnget,nvals,dval)
      call putdailydsn(wdmput,sdate,edate,dsn,nvals,dval)

*********** WNDH **********
      dsn = 1002
      dsnget = metregion + 4
      print*,lseg,' ',dsn,dsnget,' WNDH'
      call gethourdsn(wdmget,sdate,edate,dsnget,nvals,hval)
      call puthourdsn(wdmput,sdate,edate,dsn,nvals,hval)

*********** RADH **********
      dsn = 1003
      dsnget = metregion + 6
      print*,lseg,' ',dsn,dsnget,' RADH'
      call gethourdsn(wdmget,sdate,edate,dsnget,nvals,hval)
      call puthourdsn(wdmput,sdate,edate,dsn,nvals,hval)

C*********** ATMP **********
C      dsn = 1004
C      dsnget = metregion + 9
C      print*,lseg,' ',dsn,dsnget,' ATMP'
C      call gethourdsn(wdmget,sdate,edate,dsnget,nvals,hval)
C      call puthourdsn(wdmput,sdate,edate,dsn,nvals,hval)

      call wdflcl (wdmget,err)
      if (err.ne.0) go to 996
      call wdflcl (wdmput,err)
      if (err.ne.0) go to 996

      stop

********************************* ERROR SPACE **************************

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


