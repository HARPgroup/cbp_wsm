************************************************************************
**  reads from a file and writes a new wdm with similar name          **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'
      integer wdmfil
      parameter (wdmfil=dfile+10)

      character *1 answer
      character*6 tunits

      integer i,sdate(ndate),edate(ndate),tstep

      integer firstchar,lastchar ! first and last character column numbers
      integer year,month,day,hour,alltime,nvals2
      integer time(ndaymax*24,4)  ! year,month,day,hour

      integer ny,oldyear

      double precision dacc

      character*100 datasource,version ! location of data
      integer lendatasource,lenversion

      character*200 dfnam

************* open datafile and new wdm
      read(*,*) lseg, datasource, version
      call lencl(datasource,lendatasource)
      call lencl(version,lenversion)
      print*,lseg

************* open wdm
      wdmfnam = 'met_'//lseg(:6)//'.wdm'
      call wdbopn(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 9911

**************** TEMPERATURE FIRST
************* open datafile 
      dfnam = tree//'input/unformatted/precip_some_met/'//
     .       datasource(:lendatasource)//'/'//version(:lenversion)//
     .       '/OUTPUT/FIPSAB/'//lseg//'.TMP'
      open(dfile,file=dfnam,status='old',iostat=err)
      if (err.ne.0) go to 992

      tcode = 3    ! hourly data

      dsn = 1004   ! air temperature dsn

***********   ********* read in the data to local variables
      nvals = 0
      do
        read(dfile,'(a100)',end=222)line
        call d2x(line,last)
        nvals = nvals + 1
        read(line,*) (time(nvals,i),i=1,4),hval(nvals)
      end do

222   close (dfile)

************ check the data for completeness
      do i = 1,3                       ! populate sdate and edate
        sdate(i) = time(1,i)
        edate(i) = time(nvals,i)
      end do
      sdate(4) = 0
      edate(4) = 24
      do i = 5,6
        sdate(i) = 0
        edate(i) = 0
      end do
 
      call timdif(sdate,edate,tcode,1,nvals2)
      if (nvals2.ne.nvals) go to 994

******** convert from C to F
      do i = 1,nvals
        hval(i) = hval(i)*9.0/5.0+32.0
      end do

********** check the data for reasonable values of temperature
      do i = 1,nvals
        if (hval(i).lt.-50.0.or.hval(i).gt.110.0) go to 996
      end do

************** write to wdm
      print*,'writing to ',wdmfnam
      call puthourdsn(wdmfil,sdate,edate,dsn,nvals,hval)

**************** PET NEXT
************* open datafile 
      dfnam = tree//'input/unformatted/precip_some_met/'//
     .       datasource(:lendatasource)//'/'//version(:lenversion)//
     .       '/OUTPUT/FIPSAB/'//lseg//'.PET'
      open(dfile,file=dfnam,status='old',iostat=err)
      if (err.ne.0) go to 992

      tcode = 3    ! hourly data

      dsn = 1000   ! PET dsn

***********   ********* read in the data to local variables
      nvals = 0
      do
        read(dfile,'(a100)',end=333)line
        call d2x(line,last)
        nvals = nvals + 1
        read(line,*) (time(nvals,i),i=1,4),hval(nvals)
      end do

333   close (dfile)

************ check the data for completeness
      do i = 1,3                       ! populate sdate and edate
        sdate(i) = time(1,i)
        edate(i) = time(nvals,i)
      end do
      sdate(4) = 0
      edate(4) = 24
      do i = 5,6
        sdate(i) = 0
        edate(i) = 0
      end do
 
      call timdif(sdate,edate,tcode,1,nvals2)
      if (nvals2.ne.nvals) go to 994

********** check the data for negative values
      do i = 1,nvals
        if (hval(i).lt.-0.001) go to 995
      end do

********* check for reasonable values of PET
      oldyear = time(1,1)
      dacc = 0.0
      do i = 1,nvals
        if (oldyear.ne.time(i,1)) then
          print*,lseg,',',oldyear,',',dacc
          if (dacc.lt.20.or.dacc.gt.55) go to 997
          oldyear = time(i,1)
          dacc = 0.0
        end if
        dacc = dacc + hval(i)
      end do
      print*,lseg,',',oldyear,',',dacc

************** write to wdm
      print*,'writing to ',wdmfnam
      call puthourdsn(wdmfil,sdate,edate,dsn,nvals,hval)


************ close wdm
      call wdflc1(wdmfil,err)
      if (err.ne.0) go to 993

      return

********************************* ERROR SPACE **************************
991   report(1) = 'WDM file cannot previously exist'
      report(2) = 'remove or rename wdm:'
      report(3) = wdmfnam
      go to 999

9911  report(1) = 'WDM file must previously exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
      go to 999

992   print*,dfnam
      report(1) = 'could not open file'
      report(2) = dfnam
      report(3) = ' '
      go to 999

993   report(1) = 'Error: closing wdm = '
      write(report(1)(28:30),'(i3)')err
      report(2) = ' wdm name =  '
      report(3) = wdmfnam
      go to 999

994   report(1) = 'number of values found does not match start and end'
      report(2) = 'dates.  Check for completeness'
      write(report(3),*) 'nvals = ',nvals,' nvals2 = ',nvals2
      go to 999

995   report(1) = 'there should be no negative values in this dataset'
      report(2) = ' '
      report(3) = ' '
      go to 999

996   report(1) = 'temperature out of bounds'
      write(report(2),*)hval(i),' degrees on day '
      write(report(3),*)time(i,1),' ',time(i,2),' ',
     .                  time(i,3),' ',time(i,4)
      go to 999

997   report(1) = 'PET out of bounds'
      report(2) = ' '
      report(3) = ' '
      go to 999

999   call stopreport(report)


      end

