************************************************************************
**  reads from a file and writes a new wdm with similar name          **
************************************************************************
      include 'qstd.inc'
      integer nv

************* open datafile and create new wdm
      read(*,'(a)') fnam

      wdmfnam = fnam

      call wdbopnlong(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 9911

      sdate(1) = 1984
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0
      edate(1) = 2003
      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0
************** write to wdm
      do dsn = 3007,3008

        call getdailydsn(wdmfil,sdate,edate,dsn,nvals,dval)
        do nv = 1,nvals
          dval(nv) = -dval(nv)
        end do
        call putdailydsn(wdmfil,sdate,edate,dsn,nvals,dval)
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

