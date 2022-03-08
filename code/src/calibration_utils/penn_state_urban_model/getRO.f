************************************************************************
**  subroutine to get the runoff out of the appropriate land seg,     **
**    land use, and scenario wdm and store in a variable              **
************************************************************************
      subroutine getRO(lseg,lscen,
     .              T1year,T1month,T1day,
     .              T2year,T2month,T2day,
     .              imppercent,
     .              hRO)

      implicit none
      include 'psu.inc'
      include '../../lib/inc/wdm.inc'
      include '../../lib/inc/land_use.inc'

      character*6 fips
      character*64 wdmgname

      integer sdate(ndate),edate(ndate)
      integer wdmget

      call lencl(lseg,lenlseg)
      call lencl(lscen,lenlscen)

      wdmfnam = outwdmdir//'land/pur/'//
     .                lscen(:lenlscen)//'/pur'//
     .                lseg(:lenlseg)//'.wdm'
      call findopen(wdmget)

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

      call wdbopnlong(wdmget,wdmfnam,1,err)  ! open read only
      if (err.ne.0) go to 997

      dsn = 2000
      print*,fips,' ',dsn
      call gethourdsn(wdmget,sdate,edate,dsn,nvals,hrain)

      call wdflcl (wdmget,err)
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

999   call stopreport(report)

      end



