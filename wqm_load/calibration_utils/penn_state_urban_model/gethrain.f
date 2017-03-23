************************************************************************
**  subroutine to get the rainfall out of the appropriate land seg    **
**    and scenario wdm and store in a variable                        **
************************************************************************
      subroutine gethrain(lseg,lscen,
     .              T1year,T1month,T1day,
     .              T2year,T2month,T2day,
     .              hrain)

      implicit none
      include 'psu.inc'
      include '../../lib/inc/wdm.inc'

      character*6 fips
      character*64 wdmgname,wdmpname

      integer sdate(ndate),edate(ndate)
      integer wdmget

      call readcontrol_lwdm(lscen,pradscen)

      call lencl(lseg,lenlseg)
      call lencl(pradscen,lenlscen)

      wdmgname = ScenDatDir//'climate/prad/'//pradscen(:lenlscen)//'/'//
     .           '/prad_'//lseg(:lenlseg)//'.wdm'

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

      call wdbopnlong(wdmget,wdmgname,1,err)  ! open read only
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



