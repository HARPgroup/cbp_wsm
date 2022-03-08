************************************************************************
**  reads from a file and writes a new wdm with similar name          **
************************************************************************
      include 'qstd.inc'
      include '../../lib/inc/locations.inc'

      real mgd2cfs
      parameter (mgd2cfs=1000000./24./3600./7.479)

      real ind,ag  ! industrial and ag diversions

      integer ny,nm,nd,ndaysinmonth,nvals2
      character*13  seg
      character*14  dummy
      character*30  DivScen
      character*4   YearC
      integer year, lenDivScen

************* open datafile and create new wdm
      read*,seg,DivScen,year
      !print*, 'Bhatt 1a ',DivScen(:lenDivScen)

      call lencl(DivScen,lenDivScen)
      write(YearC,'(I4)') year
      !print*, 'Bhatt 1b ',DivScen(:lenDivScen)

      fnam = tree//'input/unformatted/diversions/'//
     . DivScen(:lenDivScen)//'/Supply3007/'//YearC//'/'//seg//'.csv'
C      print*,'Bhatt 2a ',fnam,DivScen(:lenDivScen)

      call lencl(DivScen,lenDivScen)
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 992

      fnam = tree//'input/unformatted/diversions/'//
     . DivScen(:lenDivScen)//'/Irrigation3008/'//YearC//'/'//seg//'.csv'

C      print*,'Bhatt 2b ',fnam,DivScen(:lenDivScen)

      call lencl(DivScen,lenDivScen)
      open(dfile+1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 992

      wdmfnam = 'DIV_'//seg//'.wdm'
C      print*,wdmfnam
      !go to 992
        ! temporary location of working directory on tautog 4/05

      call wdbopnlong(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 9911

******************** get and check the data set
C      call readstdin(dvals,sdate,edate)
      ndsns = 2
      dsns(1) = 3007
      dsns(2) = 3008
      tcode = 4    ! daily data
      sdate(1) = year
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0
      edate(1) = year
      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      nvals = 0
      read(dfile,'(a100)',end=111)line ! read header
      read(dfile+1,'(a100)',end=111)line ! read header
      do 
        read(dfile,*, end=111) dummy,ny,nm,ind
        read(dfile+1,*, end=111) dummy,ny,nm,ag
        !read(dfile,'(a100)',end=111)line
        !call d2x(line,last)
        !read(line(2:14),'(a14)')dummy
        !read(line(17:20),'(i4)')ny
        !read(line(22:23),'(i2)')nm
        !read(line(7:8),'(i4)')nd
        !read(line,*) ind
C        print*,dummy,',',ny,',',nm,',',ind
C        print*,dummy,',',ny,',',nm,',',ag

        do i = 1,ndaysinmonth(ny,nm)
          nvals = nvals + 1
          dvals(nvals,1) = ind !*mgd2cfs
          dvals(nvals,2) = ag  !*mgd2cfs
        end do
      end do

111   close(dfile)
     
      call timdif(sdate,edate,tcode,1,nvals2)
      if (nvals2.ne.nvals) go to 993


************** write to wdm
C      print*,'writing to ',wdmfnam
      do i = 1,ndsns
        dsn = dsns(i)
        do nd = 1,nvals
          dval(nd) = dvals(nd,i)
        end do
        call putdailydsn(wdmfil,sdate,edate,dsn,
     .                                 nvals,dval)
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

993   report(1) = 'number of values found does not match start and end'
      report(2) = 'dates.  Check for completeness'
      write(report(3),*) 'nvals = ',nvals,' nvals2 = ',nvals2
      go to 999

995   report(1) = 'Error: closing wdm = '
      write(report(1)(28:30),'(i3)')err
      report(2) = ' wdm name =  '
      report(3) = wdmfnam
      go to 999

999   call stopreport(report)

      end

