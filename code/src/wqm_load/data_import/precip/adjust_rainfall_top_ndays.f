      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'

      integer StartYear,StopYear
      parameter (StartYear=1984,StopYear=2014)

      integer NumDaysMax
      parameter ( NumDaysMax = (StopYear - StartYear + 1) * 366 )

      real hPRC(NumDaysMax*24)

      real dPRC

      integer wdmfil
      parameter (wdmfil=dfile+10)

      integer i,sdate(ndate),edate(ndate),tstep

      integer year,month,day,hour
      integer time(NumDaysMax*24,4)  ! year,month,day,hour

      integer ny,nm,nd,nh
      integer iy,im,id
      integer nhours

      integer NumDays
      integer ndaysinmonth,  ndaysinyear, julian
      external ndaysinmonth, ndaysinyear, julian

      character*100 datasource,version ! location of data
      integer lendatasource,lenversion

      character*200 dfnam

      real factor, dthreshold
************* modify LSEG rain in DATASOURCE using the FACTOR above DTHRESHOLD
      read(*,*) lseg,datasource,factor,dthreshold

      call lencl(version,lenversion)
      call lencl(datasource,lendatasource)

************* Start Stop Time
      sdate(1) = StartYear
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = StopYear
      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      NumDays = julian(sdate(1),sdate(2),sdate(3),
     .               edate(1),edate(2),edate(3))

      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+1,wdmfnam,0,err)
      if (err.ne.0) go to 992

************* open wdm
      wdmfnam = tree//'input/scenario/climate/prad/'//
     .          datasource(:lendatasource)//'/'//
     .          'prad_'//lseg(:6)//'.wdm'
      print*,wdmfnam
      call wdbopnlong(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 991


      dsn = 2000   ! precip
      call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hPRC)

      iy  = sdate(1)
      im  = sdate(2)
      id  = sdate(3)
      nhours = 0
      do nd = 1,NumDays
        dPRC = 0.0
        do nh = 1,24
          nhours = nhours + 1
          dPRC = dPRC + hPRC(nhours)
        end do
        if ( dPRC.ge.dthreshold) then
          print*,'modifying ',lseg,dPRC,' in',iy,im,id,' by',factor
          nhours = nhours - 24
          do nh = 1,24
            hPRC(nhours) = hPRC(nhours) * factor
            nhours = nhours + 1
          end do
        end if
        call tomorrow(iy,im,id)
      end do

************* write back to wdm
      call puthourdsn(wdmfil,sdate,edate,dsn,nvals,hPRC)

************* close wdm
      call wdflcl(wdmfil,err)
      if (err.ne.0) go to 993

      return

991   report(1) = 'WDM file must previously exist'
      report(2) = 'Missing WDM File Name: '
      report(3) = wdmfnam
      go to 999

992   report(1) = 'Could not open file for writing'
      report(2) = fnam
      report(3) = ' '
      go to 999

993   report(1) = 'Error 993 Problem closing wdm file: '
      report(2) = wdmfnam
      report(3) = ' '
      go to 999

999   call stopreport(report)
      end
