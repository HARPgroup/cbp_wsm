************************************************************************
**  reads from a file and writes a new wdm with similar name          **
************************************************************************
      include 'qstd.inc'

      real mgd2cfs
      parameter (mgd2cfs=1000000./24./3600./7.479)

************* open datafile and create new wdm
      read(*,'(a)') fnam

      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 992

      i = 1
      wdmfnam = ' '
      do while (fnam(i:i).ne.'.')
        wdmfnam(i:i) = fnam(i:i)
        i = i + 1
      end do
      wdmfnam(i:i+3) = '.wdm'

      call wdbopnlong(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 9911

******************** get and check the data set
      call readstdin(hvals,dvals,ndsns,dsns,tcode,sdate,edate)

      close (dfile)

************** write to wdm
      print*,'writing to ',wdmfnam
      do i = 1,ndsns
        dsn = dsns(i)
        if (tcode.eq.3) then
          do nvals = 1, ndaymax*24
            hval(nvals) = hvals(nvals,i)
          end do
          call puthourdsn(wdmfil,sdate,edate,dsn,
     .                                nvals,hval)
        end if
        if (tcode.eq.4) then
          do nvals = 1, ndaymax
            dval(nvals) = abs(dvals(nvals,i))*mgd2cfs
          end do
          call putdailydsn(wdmfil,sdate,edate,dsn,
     .                                 nvals,dval)
        end if
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

