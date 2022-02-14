      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'

      integer StartYear,StopYear
      parameter (StartYear=1984,StopYear=2014)

      integer NumDaysMax
      parameter ( NumDaysMax = (StopYear - StartYear + 1) * 366 )

      real hDAT(NumDaysMax*24)
      real rhDAT(366*24)

      real yDAT,ryDAT

      integer wdmfil
      parameter (wdmfil=dfile+10)

      integer i,sdate(ndate),edate(ndate),tstep

      integer year,month,day,hour
      integer time(NumDaysMax*24,4)  ! year,month,day,hour

      integer ny,nm,nd,nh
      integer iy,im,id,ih
      integer nhours,rhours
      integer ndays,rdays

      integer NumDays
      integer ndaysinmonth,  ndaysinyear, julian
      external ndaysinmonth, ndaysinyear, julian

      character*100 datasource,version ! location of data
      integer lendatasource,lenversion

      character*200 dfnam

      real factor, dthreshold

      integer mdays
      integer ryear ! year used for repeating data
      integer rsdate(ndate)
      integer redate(ndate)

      data rsdate /0,1,1,0,0,0/
      data redate /0,12,31,24,0,0/

      integer rvals
      integer rFdays

      integer dcount

************* modify LSEG rain in DATASOURCE using the FACTOR above DTHRESHOLD
      read(*,*) lseg,datasource,ryear

      rsdate(1) = ryear
      redate(1) = ryear

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
      wdmfnam = tree//'input/scenario/climate/met/'//
     .          datasource(:lendatasource)//'/'//
     .          'met_'//lseg(:6)//'.wdm'
      print*,wdmfnam
      call wdbopnlong(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 991



      ! **** H TEMPERATURE **** !
      print*,'... TEMPERATURE'
      dsn = 1004
      call gethourdsn(wdmfil,rsdate,redate,dsn,rvals,rhDAT)
      call gethourdsn(wdmfil, sdate, edate,dsn,nvals,hDAT)

      rFdays = ndaysinmonth(ryear,2)
      nhours = 1
      do iy = sdate(1),edate(1)
         dcount = ndaysinyear(iy) * 24
         yDAT = 0
         ryDAT = 0
         rhours = 1
         do im = 1,12
            ndays = ndaysinmonth(iy,im)
            do id = 1,ndays
               if ( im.eq.2 .and. id.eq.29 .and. rFdays.ne.29 ) then
                  rhours = rhours - 24
               end if
               do ih = 1,24
                  yDAT = yDAT + hDAT(nhours)/dcount
                  hDAT(nhours) = rhDAT(rhours)
                  ryDAT = ryDAT + hDAT(nhours)/dcount
                  nhours = nhours + 1
                  rhours = rhours + 1
               end do
               if ( im.eq.2 .and. id.eq.28 .and. ndays.ne.29 ) then
                  if (rFdays .eq. 29 ) then
                     rhours = rhours + 24
                  end if
               end if
            end do
         end do
         print*,'... replacing ',lseg,iy,yDAT,ryDAT
      end do
        
      call puthourdsn(wdmfil,sdate,edate,dsn,nvals,hDAT)
      ! ************************************************* !
      



      ! **** H POTENTIAL EVAPOTRANSPIRATION  **** !
      print*,'... POTENTIAL EVAPOTRANSPIRATION'
      dsn = 1000 
      call gethourdsn(wdmfil,rsdate,redate,dsn,rvals,rhDAT)
      call gethourdsn(wdmfil, sdate, edate,dsn,nvals,hDAT)

      rFdays = ndaysinmonth(ryear,2)
      nhours = 1
      do iy = sdate(1),edate(1)
         dcount = 1
         yDAT = 0
         ryDAT = 0
         rhours = 1
         do im = 1,12
            ndays = ndaysinmonth(iy,im)
            do id = 1,ndays
               if ( im.eq.2 .and. id.eq.29 .and. rFdays.ne.29 ) then
                  rhours = rhours - 24
               end if
               do ih = 1,24
                  yDAT = yDAT + hDAT(nhours)/dcount
                  hDAT(nhours) = rhDAT(rhours)
                  ryDAT = ryDAT + hDAT(nhours)/dcount
                  nhours = nhours + 1
                  rhours = rhours + 1
               end do
               if ( im.eq.2 .and. id.eq.28 .and. ndays.ne.29 ) then
                  if (rFdays .eq. 29 ) then
                     rhours = rhours + 24
                  end if
               end if
            end do
         end do
         print*,'... replacing ',lseg,iy,yDAT,ryDAT
      end do

      call puthourdsn(wdmfil,sdate,edate,dsn,nvals,hDAT)
      ! ************************************************* !
      !go to 900



      ! **** H RADIATION **** !
      print*,'... RADIATION'
      dsn = 1003
      call gethourdsn(wdmfil,rsdate,redate,dsn,rvals,rhDAT)
      call gethourdsn(wdmfil, sdate, edate,dsn,nvals,hDAT)

      rFdays = ndaysinmonth(ryear,2)
      nhours = 1
      do iy = sdate(1),edate(1)
         dcount = ndaysinyear(iy) * 24
         yDAT = 0
         ryDAT = 0
         rhours = 1
         do im = 1,12
            ndays = ndaysinmonth(iy,im)
            do id = 1,ndays
               if ( im.eq.2 .and. id.eq.29 .and. rFdays.ne.29 ) then
                  rhours = rhours - 24
               end if
               do ih = 1,24
                  yDAT = yDAT + hDAT(nhours)/dcount
                  hDAT(nhours) = rhDAT(rhours)
                  ryDAT = ryDAT + hDAT(nhours)/dcount
                  nhours = nhours + 1
                  rhours = rhours + 1
               end do
               if ( im.eq.2 .and. id.eq.28 .and. ndays.ne.29 ) then
                  if (rFdays .eq. 29 ) then
                     rhours = rhours + 24
                  end if
               end if
            end do
         end do
         print*,'... replacing ',lseg,iy,yDAT,ryDAT
      end do

      call puthourdsn(wdmfil,sdate,edate,dsn,nvals,hDAT)
      ! ************************************************* !
      !go to 900



      ! **** H WIND **** !
      print*,'... WIND VELOCITY'
      dsn = 1002
      call gethourdsn(wdmfil,rsdate,redate,dsn,rvals,rhDAT)
      call gethourdsn(wdmfil, sdate, edate,dsn,nvals,hDAT)

      rFdays = ndaysinmonth(ryear,2)
      nhours = 1
      do iy = sdate(1),edate(1)
         dcount = ndaysinyear(iy) * 24
         yDAT = 0
         ryDAT = 0
         rhours = 1
         do im = 1,12
            ndays = ndaysinmonth(iy,im)
            do id = 1,ndays
               if ( im.eq.2 .and. id.eq.29 .and. rFdays.ne.29 ) then
                  rhours = rhours - 24
               end if
               do ih = 1,24
                  yDAT = yDAT + hDAT(nhours)/dcount
                  hDAT(nhours) = rhDAT(rhours)
                  ryDAT = ryDAT + hDAT(nhours)/dcount
                  nhours = nhours + 1
                  rhours = rhours + 1
               end do
               if ( im.eq.2 .and. id.eq.28 .and. ndays.ne.29 ) then
                  if (rFdays .eq. 29 ) then
                     rhours = rhours + 24
                  end if
               end if
            end do
         end do
         print*,'... replacing ',lseg,iy,yDAT,ryDAT
      end do

      call puthourdsn(wdmfil,sdate,edate,dsn,nvals,hDAT)
      ! ************************************************* !
      !go to 900



      ! **** D DEW POINT **** !
      print*,'... DEW POINT'
      dsn = 1001
      call getdailydsn(wdmfil,rsdate,redate,dsn,rvals,rhDAT)
      call getdailydsn(wdmfil, sdate, edate,dsn,nvals,hDAT)

      rFdays = ndaysinmonth(ryear,2)
      ndays = 1
      do iy = sdate(1),edate(1)
         dcount = ndaysinyear(iy)
         yDAT = 0
         ryDAT = 0
         rdays = 1
         do im = 1,12
            mdays = ndaysinmonth(iy,im)
            do id = 1,mdays
               if ( im.eq.2 .and. id.eq.29 .and. rFdays.ne.29 ) then
                  rdays = rdays - 1
               end if
                  yDAT = yDAT + hDAT(ndays)/dcount
                  hDAT(ndays) = rhDAT(rdays)
                  ryDAT = ryDAT + hDAT(ndays)/dcount
                  ndays = ndays + 1
                  rdays = rdays + 1
               if ( im.eq.2 .and. id.eq.28 .and. mdays.ne.29 ) then
                  if (rFdays .eq. 29 ) then
                     rdays = rdays + 1
                  end if
               end if
            end do
         end do
         print*,'... replacing ',lseg,iy,yDAT,ryDAT
      end do

      call putdailydsn(wdmfil,sdate,edate,dsn,nvals,hDAT)
      ! ************************************************* !
      !go to 900




      ! **** D CLOUD COVER **** !
      print*,'... CLOUD COVER'
      dsn = 1005
      call getdailydsn(wdmfil,rsdate,redate,dsn,rvals,rhDAT)
      call getdailydsn(wdmfil, sdate, edate,dsn,nvals,hDAT)

      rFdays = ndaysinmonth(ryear,2)
      ndays = 1
      do iy = sdate(1),edate(1)
         dcount = ndaysinyear(iy)
         yDAT = 0
         ryDAT = 0
         rdays = 1
         do im = 1,12
            mdays = ndaysinmonth(iy,im)
            do id = 1,mdays
               if ( im.eq.2 .and. id.eq.29 .and. rFdays.ne.29 ) then
                  rdays = rdays - 1
               end if
                  yDAT = yDAT + hDAT(ndays)/dcount
                  hDAT(ndays) = rhDAT(rdays)
                  ryDAT = ryDAT + hDAT(ndays)/dcount
                  ndays = ndays + 1
                  rdays = rdays + 1
               if ( im.eq.2 .and. id.eq.28 .and. mdays.ne.29 ) then
                  if (rFdays .eq. 29 ) then
                     rdays = rdays + 1
                  end if
               end if
            end do
         end do
         print*,'... replacing ',lseg,iy,yDAT,ryDAT
      end do

      call putdailydsn(wdmfil,sdate,edate,dsn,nvals,hDAT)
      ! ************************************************* !


















************* close wdm
900   call wdflcl(wdmfil,err)
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
