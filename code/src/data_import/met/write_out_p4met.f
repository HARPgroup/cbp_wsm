      implicit none
      include '../../../inc/standard.inc'
      include '../../../inc/wdm.inc'

      character*64 wdmgname

      integer sdate(ndate),edate(ndate)
      integer wdmget
      parameter (wdmget=12)

      integer n,nd,dsnget

      integer metregion    ! 10, 20, ... 70

******************** END SPECIFICATIONS ********************************

      wdmgname = '/work/p5_big_dataset_storage/p4met/allp4met.wdm'

      sdate(1) = 1984
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = 1997
      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      call wdbopnlong(wdmget,wdmgname,1,err)  ! open read only
      if (err.ne.0) go to 997

      do metregion = 10,70,10
*********** EVAP **********
        dsnget = metregion
        print*,dsnget,' EVAP'
        call getdailydsn(wdmget,sdate,edate,dsnget,nvals,dval)
        call writedailydsn(sdate,edate,dsnget,nvals,dval)

*********** DEWP **********
        dsnget = metregion + 1
        print*,dsnget,' DEWP'
        call getdailydsn(wdmget,sdate,edate,dsnget,nvals,dval)
        call writedailydsn(sdate,edate,dsnget,nvals,dval)

*********** CLDC **********
        dsnget = metregion + 2
        print*,dsnget,' CLDC'
        call getdailydsn(wdmget,sdate,edate,dsnget,nvals,dval)
        call writedailydsn(sdate,edate,dsnget,nvals,dval)

*********** WNDH **********
        dsnget = metregion + 4
        print*,dsnget,' WNDH'
        call gethourdsn(wdmget,sdate,edate,dsnget,nvals,hval)
        call writehourdsn(sdate,edate,dsnget,nvals,hval)

*********** RADH **********
        dsnget = metregion + 6
        print*,dsnget,' RADH'
        call gethourdsn(wdmget,sdate,edate,dsnget,nvals,hval)
        call writehourdsn(sdate,edate,dsnget,nvals,hval)

*********** ATMP **********
        dsnget = metregion + 9
        print*,dsnget,' ATMP'
        call gethourdsn(wdmget,sdate,edate,dsnget,nvals,hval)
        call writehourdsn(sdate,edate,dsnget,nvals,hval)

      end do

      call wdflc1 (wdmget,err)
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


