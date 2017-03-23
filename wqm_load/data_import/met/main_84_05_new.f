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
      
C DIFFERENT SDATE AND EDATE FOR WNDH AND RADH 
      integer sdate03WDRAD(ndate), sdate05WDRAD(ndate)
      integer edate03WDRAD(ndate), edate05WDRAD(ndate)
 
      real dv2(ndaymax),hv2(ndaymax*24)
      integer nv2

      integer missing

******************** END SPECIFICATIONS ********************************

      read*,lseg

      call getmetreg(tree,lseg,metregion)
      print*,lseg,metregion
      write(mregion2,'(i1)') metregion/10

      wdmgname = tree//'input/unformatted/p4met/p4met00.wdm'
      wdm0103 = tree//'input/unformatted/p4met/met0105/mt0103r'
     .          //mregion2//'d.wdm'
      wdm0405 = tree//'input/unformatted/p4met/met0105/mt0405r'
     .          //mregion2//'d.wdm'
      wdmpname = 'met_'//lseg//'.wdm'

      sdate(1) = 1984
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      sdate03(1) = 2001
      sdate03(2) = 1
      sdate03(3) = 1
      sdate03(4) = 0
      sdate03(5) = 0
      sdate03(6) = 0

      sdate05(1) = 2004
      sdate05(2) = 1
      sdate05(3) = 1
      sdate05(4) = 0 
      sdate05(5) = 0
      sdate05(6) = 0

      edate(1) = 2000
      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      edate03(1) = 2003
      edate03(2) = 12
      edate03(3) = 31
      edate03(4) = 24
      edate03(5) = 0
      edate03(6) = 0

      edate05(1) = 2005
      edate05(2) = 12
      edate05(3) = 31
      edate05(4) = 24
      edate05(5) = 0
      edate05(6) = 0

      call wdbopn(wdmget,wdmgname,1,err)  ! open read only
      if (err.ne.0) go to 997
      call wdbopn(wdmget+1,wdm0103,1,err)  ! open read only
      if (err.ne.0) go to 994
      call wdbopn(wdmget+2,wdm0405,1,err)  ! open read only
      if (err.ne.0) go to 995
      call wdbopn(wdmput,wdmpname,0,err)  ! open read/write 
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
      call getdailydsn(wdmget+1,sdate03,edate03,dsnget,nv2,dv2)
      do n = 1,nv2
        dval(nvals+n) = dv2(n)
      end do
      nvals = nvals + nv2
      call getdailydsn(wdmget+2,sdate05,edate05,dsnget,nv2,dv2)
      do n = 1,nv2
        dval(nvals+n) = dv2(n)
      end do
      nvals = nvals + nv2
      call fixmiss(dval,nvals,
     O             missing)
      print*,'   ',missing,' missing values'
      if (missing.gt.20) go to 991
      call putdailydsn(wdmput,sdate,edate05,dsn,nvals,dval)

*********** CLDC **********
      dsn = 1005
      dsnget = metregion + 2
      print*,lseg,' ',dsn,dsnget,' CLDC'
      call getdailydsn(wdmget,sdate,edate,dsnget,nvals,dval)
      call getdailydsn(wdmget+1,sdate03,edate03,dsnget,nv2,dv2)
      do n = 1,nv2
        dval(nvals+n) = dv2(n)
      end do
      nvals = nvals + nv2
      call getdailydsn(wdmget+2,sdate05,edate05,dsnget,nv2,dv2)
      do n = 1,nv2
        dval(nvals+n) = dv2(n)
      end do
      nvals = nvals + nv2
      call fixmiss(dval,nvals,
     O             missing)
      print*,'   ',missing,' missing values'
      if (missing.gt.20) go to 991
      call putdailydsn(wdmput,sdate,edate05,dsn,nvals,dval)

*********** WNDH **********
C     WNDH and RADH HAVE DIFFERENT SDATE AND EDATE for 03 and 05 
      sdate03WDRAD(1) = 2001      
      sdate03WDRAD(2) = 1
      sdate03WDRAD(3) = 1
      sdate03WDRAD(4) = 1
      sdate03WDRAD(5) = 0
      sdate03WDRAD(6) = 0

      edate03WDRAD(1) = 2004
      edate03WDRAD(2) = 1
      edate03WDRAD(3) = 1
      edate03WDRAD(4) = 1
      edate03WDRAD(5) = 0
      edate03WDRAD(6) = 0

      sdate05WDRAD(1) = 2004
      sdate05WDRAD(2) = 1
      sdate05WDRAD(3) = 1
      sdate05WDRAD(4) = 1
      sdate05WDRAD(5) = 0
      sdate05WDRAD(6) = 0

      edate05WDRAD(1) = 2006
      edate05WDRAD(2) = 1
      edate05WDRAD(3) = 1
      edate05WDRAD(4) = 1
      edate05WDRAD(5) = 0
      edate05WDRAD(6) = 0 
C     DATES IN P4met WDM FOR WNDH AND RADH
     
      dsn = 1002
      dsnget = metregion + 4
      print*,lseg,' ',dsn,dsnget,' WNDH'
      call gethourdsn(wdmget,sdate,edate,dsnget,nvals,hval)
      call gethourdsn(wdmget+1,sdate03WDRAD,edate03WDRAD,dsnget,nv2,hv2)
      do n = 1,nv2
        hval(nvals+n) = hv2(n)
      end do
      nvals = nvals + nv2
      call gethourdsn(wdmget+2,sdate05WDRAD,edate05WDRAD,dsnget,nv2,hv2)
      do n = 1,nv2
        hval(nvals+n) = hv2(n)
      end do
      nvals = nvals + nv2
      call fixmiss(hval,nvals,
     O             missing)
      print*,'   ',missing,' missing values'
      if (missing.gt.20) go to 991
      call puthourdsn(wdmput,sdate,edate05,dsn,nvals,hval)

*********** RADH **********
      dsn = 1003
      dsnget = metregion + 6
      print*,lseg,' ',dsn,dsnget,' RADH'
      call gethourdsn(wdmget,sdate,edate,dsnget,nvals,hval)
      call gethourdsn(wdmget+1,sdate03WDRAD,edate03WDRAD,dsnget,nv2,hv2)
      do n = 1,nv2
        hval(nvals+n) = hv2(n)
      end do
      nvals = nvals + nv2
      call gethourdsn(wdmget+2,sdate05WDRAD,edate05WDRAD,dsnget,nv2,hv2)
      do n = 1,nv2
        hval(nvals+n) = hv2(n)
      end do
      nvals = nvals + nv2
      call fixmiss(hval,nvals,
     O             missing)
      print*,'   ',missing,' missing values'
      if (missing.gt.20) go to 991
      call puthourdsn(wdmput,sdate,edate05,dsn,nvals,hval)

C*********** ATMP **********
C      dsn = 1004
C      dsnget = metregion + 9
C      print*,lseg,' ',dsn,dsnget,' ATMP'
C      call gethourdsn(wdmget,sdate,edate,dsnget,nvals,hval)
C      call puthourdsn(wdmput,sdate,edate,dsn,nvals,hval)

      call wdflcl (wdmget,err)
      if (err.ne.0) go to 996

      call wdflcl (wdmget+1,err)
      if (err.ne.0) go to 996

      call wdflcl (wdmget+2,err)
      if (err.ne.0) go to 996

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


