************************************************************************
**  reads from a file and writes a new wdm with similar name          **
************************************************************************
      include 'qstd.inc'

      real mgd2cfs
      parameter (mgd2cfs=1000000./24./3600./7.479)
      integer nvals2,nv
      real annval(maxdsns,12,31)
      integer targetyear,year,month,day,ndaysinyear,startyear,endyear
      external ndaysinyear
      logical done

      ndsns = 2
      dsns(1) = 3007
      dsns(2) = 3008

************* open wdm and read annual data
      read*, wdmfnam,startyear,endyear,targetyear
      print*,wdmfnam
      call wdbopnlong(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 9911

      sdate(1) = targetyear
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = targetyear
      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      do i = 1,ndsns
        dsn = dsns(i)
        call getdailydsn(wdmfil,sdate,edate,dsn,
     .                                nvals,dval)
        year = targetyear
        month = 1
        day = 1
        do nv = 1,ndaysinyear(year)
          annval(i,month,day) = dval(nv)
          call tomorrow(year,month,day)
        end do
        if (ndaysinyear(targetyear).eq.365) then
          annval(i,2,29) = annval(i,2,28)
        end if
      end do

      sdate(1) = startyear
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = endyear
      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      do i = 1,ndsns
        year = sdate(1)
        month = sdate(2)
        day = sdate(3)

        nv = 1
        done = .false.
        do while (.not.done)
          dval(nv) = annval(i,month,day)
          call tomorrow(year,month,day)
          nv = nv + 1
          if (year.gt.edate(1)) done = .true.
          if (year.eq.edate(1).and.month.gt.edate(2)) done = .true.
          if (year.eq.edate(1).and.month.eq.edate(2)
     .                        .and.day.gt.edate(3)) done = .true.
        end do
        nvals = nv - 1

        dsn = dsns(i)
        call putdailydsn(wdmfil,sdate,edate,dsn,
     .                                nvals,dval)
      end do

      call wdflc1(wdmfil,err)
      if (err.ne.0) go to 995

      return

********************************* ERROR SPACE **********************************
991   report(1) = 'WDM file cannot previously exist'
      report(2) = 'remove or rename wdm:'
      report(3) = wdmfnam
      go to 999

9911  report(1) = 'WDM file must previously exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
      go to 999

992   report(1) = 'could not open file'
      report(2) = fnam
      report(3) = ' '
      go to 999

995   report(1) = 'Error: closing wdm = '
      write(report(1)(28:30),'(i3)')err
      report(2) = ' wdm name =  '
      report(3) = wdmfnam
      go to 999

999   call stopreport(report)

      end

