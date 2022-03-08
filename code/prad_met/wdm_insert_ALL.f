************************************************************************
***   Program reads NLDAS-2 time series data and writes WDMs         ***
***   Author: Gopal Bhatt made major revisions on 02/05/15           ***
************************************************************************

      implicit none
C     ERROR CODES       
      integer err, retcod
C     FILE AND INPUT NAMES
      character*6   lseg
      character*64  wdmfname,msgfname
      character*200 csvfname
      character*100 datasource, version, period
      character*300 longline
      integer last
C     SIZE OF INPUT NAMES
      integer lendatasource, lenversion, lenperiod
C     SIZE OF DATE ARRAYS
      integer ndate
      parameter (ndate=6)
C     START AND END DATES IN WDM FORMAT         
      integer sdate(ndate), edate(ndate)
C     WDM IO PARAMS
      integer HCODE, DCODE ! Hourly, Daily
      parameter (HCODE=3, DCODE=4)
      integer TSTEP, dtran, qualfg, dtovwr
      parameter (TSTEP=1, dtran=0, qualfg=0, dtovwr=1)
C     FILE UNIT NUMBERS      
      integer csvfile, wdmfile, msgfile
      parameter (csvfile=13, wdmfile=12,msgfile=9)
C     INTEGER COUNTER AND DATA SET NUMBER
      integer i, j, dsn
      integer tIMM, tIDD, tIHH
      real    tRData
C     VARIABLES FOR CHECKING CLIMATE INSERT DATA
      integer oldyear
      real    dacc
      integer ndacc
C     ARRAY SIZES AND NUMBER OF VALUES IN TIMESERIES 
      integer   NDAYSMAX
      parameter (NDAYSMAX=16836) !(2025-1980+1)*366
      real      csvdata(NDAYSMAX*24,5),hval(NDAYSMAX*24)
      real      RNMAX(12,31,24), PRECIP(NDAYSMAX*24)
      integer   SunnyHrs
      integer   csvndata, nvals
      real      hmin,hmax,ymin,ymax
      integer   YEAR1, YEAR2
      integer   HPRC, HTMP, HPET, HRAD, HWND, DDPT, DCLC
******************** END SPECIFICATIONS ********************************
      read(*,*) lseg, datasource, version, period, YEAR1, YEAR2,
     .          HPRC, HTMP, HPET, HRAD, HWND, DDPT, DCLC
      call lencl(datasource,lendatasource)
      call lencl(version,lenversion)
      call lencl(period, lenperiod)
      print*, lseg
**********start and end dates for entire timeseries*********************
      sdate(1) = YEAR1
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0
      
      edate(1) = YEAR2
      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

*********** Open WDM and Data file *************************************
      msgfname= './message.wdm'
      call wdbopn(msgfile,msgfname,1,retcod)  ! open msgfile read only
      if (retcod.ne.0) stop 'ERROR opening message wdm'





************************************************************************
************************ ##### HPRC START ##### ************************
************************************************************************
      if ( HPRC .eq. 1 ) then
      wdmfname = 'prad_'//lseg//'.wdm'
      call wdbopn(wdmfile,wdmfname,0,retcod)  ! open read/write 
      if (retcod.ne.0) then
             print*, 'retcod = ', retcod 
             stop 'ERROR opening wdm'
      end if
      hmin = 0.0
      hmax = 2.1
      ymin = 20.0
      ymax = 78.0
      csvfname = '../../../'//
     .        'input/unformatted/'//
     .       datasource(:lendatasource)//'/'//version(:lenversion)//
     .       '/'//period(:lenperiod)//'/'//lseg//'.PRC'
      print*,csvfname
      open(csvfile,file=csvfname,status='old',iostat=err)

      if (err.ne.0) stop 'ERROR opening .PRC file '
*************read in the data to local variables************************
      csvndata = 1
      do
           read (csvfile,'(a300)',end=2000) longline
           call d2x(longline,last)
           read(longline,*,end=994,err=994)
     .     csvdata(csvndata,1),csvdata(csvndata,2),
     .     csvdata(csvndata,3),csvdata(csvndata,4),
     .     csvdata(csvndata,5)

           !print*,csvdata(csvndata,1),csvdata(csvndata,2)

           if ( csvdata(csvndata,1).ge.sdate(1)
     .           .and.csvdata(csvndata,1).le.edate(1) .and.
     .          csvdata(csvndata,2).ge.sdate(2)
     .           .and.csvdata(csvndata,2).le.edate(2) .and.
     .          csvdata(csvndata,3).ge.sdate(3)
     .           .and.csvdata(csvndata,3).le.edate(3) .and.
     .          csvdata(csvndata,4).ge.sdate(4)
     .           .and.csvdata(csvndata,4).le.edate(4) ) then
                csvndata=csvndata+1
           end if
      end do
 2000 continue
      close (csvfile)
      csvndata = csvndata - 1
*************check if data needed is there*********
      call timdif(
     I            sdate,edate,HCODE,TSTEP,
     O            nvals)

      if (nvals .ne. csvndata) then
           print*,'nval != csvndata',nvals, csvndata 
           stop 'PROBLEM ERROR with input CSV file, missing data'
      end if
************check for valid PRC range in all data***********************
      do i = 1, nvals
           if(csvdata(i,5).lt.hmin.or.csvdata(i,5).gt.hmax) then
                 print*, 'data= ', csvdata(i,5)
                 print*, 'PROBLEM ERROR Hourly data outside valid range'
           end if
      end do
************check for valid range using annual sum and print************
      oldyear = csvdata(1,1)
      dacc = 0.0
      do i = 1,csvndata
           if(oldyear.ne.csvdata(i,1)) then
                   print*,'HPRC, ',lseg,',',oldyear,',',dacc
                   if(dacc.lt.ymin.or.dacc.gt.ymax) then
                        print*, 'PROBLEM ERROR Annual data out of range'
                   end if
                   oldyear = csvdata(i,1)
                   dacc = 0.0
           end if
           dacc = dacc + csvdata(i,5)
      end do
      print*,'HPRC, ',lseg,',',oldyear,',',dacc
************open wdm and read existing data*****************************
      dsn = 2000
      print*,lseg,' ',dsn,' HPRC'
      call wdtget(
     I            wdmfile,dsn,TSTEP,sdate,nvals,
     I            dtran, qualfg, HCODE,
     O            hval, retcod)
      if (retcod.ne.0) stop 'PROBLEM ERROR getting wdm timeseries'
************copy climate data from csv array to insert hval ************
      do i = 1, nvals
           hval(i)=csvdata(i,5)
      end do
************write back to the wdm file**********************************
      print*,'writing to ', wdmfname
      call wdtput(
     I            wdmfile,dsn,TSTEP,sdate,nvals,
     I            dtovwr, qualfg, HCODE,hval,
     O            retcod)
      if (retcod.ne.0) then
            print*, 'retcod = ', retcod
            stop 'ERROR writing wdm'
      end if 
************write back to the wdm file**********************************
      call wdflcl(
     I            wdmfile,
     O            retcod)
      if (retcod.ne.0) stop 'ERROR closing wdm'
      end if
************************************************************************
************************ ##### HPRC FINISH ##### ***********************
************************************************************************









************************************************************************
************************ ##### HTMP START ##### ************************
************************************************************************
      if ( HTMP .eq. 1) then
      wdmfname = 'met_'//lseg//'.wdm'
      call wdbopn(wdmfile,wdmfname,0,retcod)  ! open read/write 
      if (retcod.ne.0) then
             print*, 'retcod = ', retcod 
             stop 'ERROR opening wdm'
      end if
      hmin = -50.0
      hmax = 130.0
      ymin = 0.0
      ymax = 80.0
      csvfname = '../../../'//
     .        'input/unformatted/'//
     .       datasource(:lendatasource)//'/'//version(:lenversion)//
     .       '/'//period(:lenperiod)//'/'//lseg//'.TMP'
      print*,csvfname
      open(csvfile,file=csvfname,status='old',iostat=err)

      if (err.ne.0) stop 'ERROR opening .TMP file '
*************read in the data to local variables************************
      csvndata = 1
      do
           read (csvfile,'(a300)',end=1004) longline
           call d2x(longline,last)
           read(longline,*,end=994,err=994)
     .     csvdata(csvndata,1),csvdata(csvndata,2),
     .     csvdata(csvndata,3),csvdata(csvndata,4),
     .     csvdata(csvndata,5)

           csvdata(csvndata,5) = 1.80 * csvdata(csvndata,5) + 32.0

           !print*,csvdata(csvndata,1),csvdata(csvndata,2)

           if ( csvdata(csvndata,1).ge.sdate(1)
     .           .and.csvdata(csvndata,1).le.edate(1) .and.
     .          csvdata(csvndata,2).ge.sdate(2)
     .           .and.csvdata(csvndata,2).le.edate(2) .and.
     .          csvdata(csvndata,3).ge.sdate(3)
     .           .and.csvdata(csvndata,3).le.edate(3) .and.
     .          csvdata(csvndata,4).ge.sdate(4)
     .           .and.csvdata(csvndata,4).le.edate(4) ) then
                csvndata=csvndata+1
           end if
      end do
 1004 continue
      close (csvfile)
      csvndata = csvndata - 1
*************check if data needed is there*********
      call timdif(
     I            sdate,edate,HCODE,TSTEP,
     O            nvals)

      if (nvals .ne. csvndata) then
           print*,'nval != csvndata',nvals, csvndata 
           stop 'PROBLEM ERROR with input CSV file, missing data'
      end if
************check for valid PRC range in all data***********************
      do i = 1, nvals
           if(csvdata(i,5).lt.hmin.or.csvdata(i,5).gt.hmax) then
                 print*, 'data= ', csvdata(i,5)
                 print*, 'PROBLEM ERROR Hourly data outside valid range'
           end if
      end do
************check for valid range using annual sum and print************
      oldyear = csvdata(1,1)
      dacc = 0.0
      ndacc = 0
      do i = 1,csvndata
           if(oldyear.ne.csvdata(i,1)) then
                   dacc = dacc / ndacc
                   print*,'HTMP, ',lseg,',',oldyear,',',dacc
                   if(dacc.lt.ymin.or.dacc.gt.ymax) then
                        print*, 'PROBLEM ERROR Annual data out of range'
                   end if
                   oldyear = csvdata(i,1)
                   dacc = 0.0
                   ndacc = 0
           end if
           dacc  = dacc + csvdata(i,5)
           ndacc = ndacc + 1
      end do
      dacc = dacc / ndacc
      print*,'HTMP, ',lseg,',',oldyear,',',dacc
************open wdm and read existing data*****************************
      dsn = 1004
      print*,lseg,' ',dsn,' ATMP'
      call wdtget(
     I            wdmfile,dsn,TSTEP,sdate,nvals,
     I            dtran, qualfg, HCODE,
     O            hval, retcod)
      if (retcod.ne.0) stop 'PROBLEM ERROR getting wdm timeseries'
************copy climate data from csv array to insert hval ************
      do i = 1, nvals
           hval(i)=csvdata(i,5)
      end do
************write back to the wdm file**********************************
      print*,'writing to ', wdmfname
      call wdtput(
     I            wdmfile,dsn,TSTEP,sdate,nvals,
     I            dtovwr, qualfg, HCODE,hval,
     O            retcod)
      if (retcod.ne.0) then
            print*, 'retcod = ', retcod
            stop 'ERROR writing wdm'
      end if 
************write back to the wdm file**********************************
      call wdflcl(
     I            wdmfile,
     O            retcod)
      if (retcod.ne.0) stop 'ERROR closing wdm'
      end if
************************************************************************
************************ ##### HTMP FINISH ##### ***********************
************************************************************************











************************************************************************
************************ ##### HPET START ##### ************************
************************************************************************
      if ( HPET .eq. 1 ) then
      wdmfname = 'met_'//lseg//'.wdm'
      call wdbopn(wdmfile,wdmfname,0,retcod)  ! open read/write 
      if (retcod.ne.0) then
             print*, 'retcod = ', retcod 
             stop 'ERROR opening wdm'
      end if
      hmin = 0.0
      hmax = 1.0
      ymin = 20.0
      ymax = 60.0
      csvfname = '../../../'//
     .        'input/unformatted/'//
     .       datasource(:lendatasource)//'/'//version(:lenversion)//
     .       '/'//period(:lenperiod)//'/'//lseg//'.PET'
      print*,csvfname
      open(csvfile,file=csvfname,status='old',iostat=err)

      if (err.ne.0) stop 'ERROR opening .PET file '
*************read in the data to local variables************************
      csvndata = 1
      do
           read (csvfile,'(a300)',end=1000) longline
           call d2x(longline,last)
           read(longline,*,end=994,err=994)
     .     csvdata(csvndata,1),csvdata(csvndata,2),
     .     csvdata(csvndata,3),csvdata(csvndata,4),
     .     csvdata(csvndata,5)

           !print*,csvdata(csvndata,1),csvdata(csvndata,2)

           if ( csvdata(csvndata,1).ge.sdate(1)
     .           .and.csvdata(csvndata,1).le.edate(1) .and.
     .          csvdata(csvndata,2).ge.sdate(2)
     .           .and.csvdata(csvndata,2).le.edate(2) .and.
     .          csvdata(csvndata,3).ge.sdate(3)
     .           .and.csvdata(csvndata,3).le.edate(3) .and.
     .          csvdata(csvndata,4).ge.sdate(4)
     .           .and.csvdata(csvndata,4).le.edate(4) ) then
                csvndata=csvndata+1
           end if
      end do
 1000 continue
      close (csvfile)
      csvndata = csvndata - 1
*************check if data needed is there*********
      call timdif(
     I            sdate,edate,HCODE,TSTEP,
     O            nvals)

      if (nvals .ne. csvndata) then
           print*,'nval != csvndata',nvals, csvndata 
           stop 'PROBLEM ERROR with input CSV file, missing data'
      end if
************check for valid PRC range in all data***********************
      do i = 1, nvals
           if(csvdata(i,5).lt.hmin.or.csvdata(i,5).gt.hmax) then
                 print*, 'data= ', csvdata(i,5)
                 print*, 'PROBLEM ERROR Hourly data outside valid range'
           end if
      end do
************check for valid range using annual sum and print************
      oldyear = csvdata(1,1)
      dacc = 0.0
      do i = 1,csvndata
           if(oldyear.ne.csvdata(i,1)) then
                   print*,'HPET, ',lseg,',',oldyear,',',dacc
                   if(dacc.lt.ymin.or.dacc.gt.ymax) then
                        print*, 'PROBLEM ERROR Annual data out of range'
                   end if
                   oldyear = csvdata(i,1)
                   dacc = 0.0
           end if
           dacc = dacc + csvdata(i,5)
      end do
      print*,'HPET, ',lseg,',',oldyear,',',dacc
************open wdm and read existing data*****************************
      dsn = 1000
      print*,lseg,' ',dsn,' EVAP'
      call wdtget(
     I            wdmfile,dsn,TSTEP,sdate,nvals,
     I            dtran, qualfg, HCODE,
     O            hval, retcod)
      if (retcod.ne.0) stop 'PROBLEM ERROR getting wdm timeseries'
************copy climate data from csv array to insert hval ************
      do i = 1, nvals
           hval(i)=csvdata(i,5)
      end do
************write back to the wdm file**********************************
      print*,'writing to ', wdmfname
      call wdtput(
     I            wdmfile,dsn,TSTEP,sdate,nvals,
     I            dtovwr, qualfg, HCODE,hval,
     O            retcod)
      if (retcod.ne.0) then
            print*, 'retcod = ', retcod
            stop 'ERROR writing wdm'
      end if 
************write back to the wdm file**********************************
      call wdflcl(
     I            wdmfile,
     O            retcod)
      if (retcod.ne.0) stop 'ERROR closing wdm'
      end if
************************************************************************
************************ ##### HPET FINISH ##### ***********************
************************************************************************












************************************************************************
************************ ##### HRAD START ##### ************************
************************************************************************
      if ( HRAD .eq. 1 ) then
      wdmfname = 'met_'//lseg//'.wdm'
      call wdbopn(wdmfile,wdmfname,0,retcod)  ! open read/write 
      if (retcod.ne.0) then
             print*, 'retcod = ', retcod 
             stop 'ERROR opening wdm'
      end if
      hmin = 0.0
      hmax = 100.0
      ymin = 0.0
      ymax = 50.0
      csvfname = '../../../'//
     .        'input/unformatted/'//
     .       datasource(:lendatasource)//'/'//version(:lenversion)//
     .       '/'//period(:lenperiod)//'/'//lseg//'.RAD'
      print*,csvfname
      open(csvfile,file=csvfname,status='old',iostat=err)

      if (err.ne.0) stop 'ERROR opening .RAD file '
*************read in the data to local variables************************
      csvndata = 1
      do
           read (csvfile,'(a300)',end=1003) longline
           call d2x(longline,last)
           read(longline,*,end=994,err=994)
     .     csvdata(csvndata,1),csvdata(csvndata,2),
     .     csvdata(csvndata,3),csvdata(csvndata,4),
     .     csvdata(csvndata,5)

           !print*,csvdata(csvndata,1),csvdata(csvndata,2)

           if ( csvdata(csvndata,1).ge.sdate(1)
     .           .and.csvdata(csvndata,1).le.edate(1) .and.
     .          csvdata(csvndata,2).ge.sdate(2)
     .           .and.csvdata(csvndata,2).le.edate(2) .and.
     .          csvdata(csvndata,3).ge.sdate(3)
     .           .and.csvdata(csvndata,3).le.edate(3) .and.
     .          csvdata(csvndata,4).ge.sdate(4)
     .           .and.csvdata(csvndata,4).le.edate(4) ) then
                csvndata=csvndata+1
           end if
      end do
 1003 continue
      close (csvfile)
      csvndata = csvndata - 1
*************check if data needed is there*********
      call timdif(
     I            sdate,edate,HCODE,TSTEP,
     O            nvals)

      if (nvals .ne. csvndata) then
           print*,'nval != csvndata',nvals, csvndata 
           stop 'PROBLEM ERROR with input CSV file, missing data'
      end if
************check for valid PRC range in all data***********************
      do i = 1, nvals
           if(csvdata(i,5).lt.hmin.or.csvdata(i,5).gt.hmax) then
                 print*, 'data= ', csvdata(i,5)
                 print*, 'PROBLEM ERROR Hourly data outside valid range'
           end if
      end do
************check for valid range using annual sum and print************
      oldyear = csvdata(1,1)
      dacc = 0.0
      ndacc = 0
      do i = 1,csvndata
           if(oldyear.ne.csvdata(i,1)) then
                   dacc = dacc / ndacc
                   print*,'HRAD, ',lseg,',',oldyear,',',dacc
                   if(dacc.lt.ymin.or.dacc.gt.ymax) then
                        print*, 'PROBLEM ERROR Annual data out of range'
                   end if
                   oldyear = csvdata(i,1)
                   dacc = 0.0
                   ndacc = 0
           end if
           dacc  = dacc + csvdata(i,5)
           ndacc = ndacc + 1
      end do
      dacc = dacc / ndacc
      print*,'HRAD, ',lseg,',',oldyear,',',dacc
************open wdm and read existing data*****************************
      dsn = 1003
      print*,lseg,' ',dsn,' RADH'
      call wdtget(
     I            wdmfile,dsn,TSTEP,sdate,nvals,
     I            dtran, qualfg, HCODE,
     O            hval, retcod)
      if (retcod.ne.0) stop 'PROBLEM ERROR getting wdm timeseries'
************copy climate data from csv array to insert hval ************
      do i = 1, nvals
           hval(i)=csvdata(i,5)
      end do
************write back to the wdm file**********************************
      print*,'writing to ', wdmfname
      call wdtput(
     I            wdmfile,dsn,TSTEP,sdate,nvals,
     I            dtovwr, qualfg, HCODE,hval,
     O            retcod)
      if (retcod.ne.0) then
            print*, 'retcod = ', retcod
            stop 'ERROR writing wdm'
      end if 
************write back to the wdm file**********************************
      call wdflcl(
     I            wdmfile,
     O            retcod)
      if (retcod.ne.0) stop 'ERROR closing wdm'
      end if
************************************************************************
************************ ##### HRAD FINISH ##### ***********************
************************************************************************














************************************************************************
************************ ##### HWND START ##### ************************
************************************************************************
      if ( HWND .eq. 1 ) then
      wdmfname = 'met_'//lseg//'.wdm'
      call wdbopn(wdmfile,wdmfname,0,retcod)  ! open read/write 
      if (retcod.ne.0) then
             print*, 'retcod = ', retcod 
             stop 'ERROR opening wdm'
      end if
      hmin = 0.0
      hmax = 120.0
      ymin = 0.0
      ymax = 20.0
      csvfname = '../../../'//
     .        'input/unformatted/'//
     .       datasource(:lendatasource)//'/'//version(:lenversion)//
     .       '/'//period(:lenperiod)//'/'//lseg//'.WND'
      print*,csvfname
      open(csvfile,file=csvfname,status='old',iostat=err)

      if (err.ne.0) stop 'ERROR opening .WND file '
*************read in the data to local variables************************
      csvndata = 1
      do
           read (csvfile,'(a300)',end=1002) longline
           call d2x(longline,last)
           read(longline,*,end=994,err=994)
     .     csvdata(csvndata,1),csvdata(csvndata,2),
     .     csvdata(csvndata,3),csvdata(csvndata,4),
     .     csvdata(csvndata,5)

           !print*,csvdata(csvndata,1),csvdata(csvndata,2)

           if ( csvdata(csvndata,1).ge.sdate(1)
     .           .and.csvdata(csvndata,1).le.edate(1) .and.
     .          csvdata(csvndata,2).ge.sdate(2)
     .           .and.csvdata(csvndata,2).le.edate(2) .and.
     .          csvdata(csvndata,3).ge.sdate(3)
     .           .and.csvdata(csvndata,3).le.edate(3) .and.
     .          csvdata(csvndata,4).ge.sdate(4)
     .           .and.csvdata(csvndata,4).le.edate(4) ) then
                csvndata=csvndata+1
           end if
      end do
 1002 continue
      close (csvfile)
      csvndata = csvndata - 1
*************check if data needed is there*********
      call timdif(
     I            sdate,edate,HCODE,TSTEP,
     O            nvals)

      if (nvals .ne. csvndata) then
           print*,'nval != csvndata',nvals, csvndata 
           stop 'PROBLEM ERROR with input CSV file, missing data'
      end if
************check for valid PRC range in all data***********************
      do i = 1, nvals
           if(csvdata(i,5).lt.hmin.or.csvdata(i,5).gt.hmax) then
                 print*, 'data= ', csvdata(i,5)
                 print*, 'PROBLEM ERROR Hourly data outside valid range'
           end if
      end do
************check for valid range using annual sum and print************
      oldyear = csvdata(1,1)
      dacc = 0.0
      ndacc = 0
      do i = 1,csvndata
           if(oldyear.ne.csvdata(i,1)) then
                   dacc = dacc / ndacc
                   print*,'HWND, ',lseg,',',oldyear,',',dacc
                   if(dacc.lt.ymin.or.dacc.gt.ymax) then
                        print*, 'PROBLEM ERROR Annual data out of range'
                   end if
                   oldyear = csvdata(i,1)
                   dacc = 0.0
                   ndacc = 0
           end if
           dacc  = dacc + csvdata(i,5)
           ndacc = ndacc + 1
      end do
      dacc = dacc / ndacc
      print*,'HWND, ',lseg,',',oldyear,',',dacc
************open wdm and read existing data*****************************
      dsn = 1002
      print*,lseg,' ',dsn,' WNDH'
      call wdtget(
     I            wdmfile,dsn,TSTEP,sdate,nvals,
     I            dtran, qualfg, HCODE,
     O            hval, retcod)
      if (retcod.ne.0) stop 'PROBLEM ERROR getting wdm timeseries'
************copy climate data from csv array to insert hval ************
      do i = 1, nvals
           hval(i)=csvdata(i,5)
      end do
************write back to the wdm file**********************************
      print*,'writing to ', wdmfname
      call wdtput(
     I            wdmfile,dsn,TSTEP,sdate,nvals,
     I            dtovwr, qualfg, HCODE,hval,
     O            retcod)
      if (retcod.ne.0) then
            print*, 'retcod = ', retcod
            stop 'ERROR writing wdm'
      end if 
************write back to the wdm file**********************************
      call wdflcl(
     I            wdmfile,
     O            retcod)
      if (retcod.ne.0) stop 'ERROR closing wdm'
      end if
************************************************************************
************************ ##### HWND FINISH ##### ***********************
************************************************************************















************************************************************************
************************ ##### DDPT START ##### ************************
************************************************************************
      if ( DDPT .eq. 1) then
      wdmfname = 'met_'//lseg//'.wdm'
      call wdbopn(wdmfile,wdmfname,0,retcod)  ! open read/write 
      if (retcod.ne.0) then
             print*, 'retcod = ', retcod 
             stop 'ERROR opening wdm'
      end if
      hmin = -50.0
      hmax = 130.0
      ymin = 0.0
      ymax = 80.0
      csvfname = '../../../'//
     .        'input/unformatted/'//
     .       datasource(:lendatasource)//'/'//version(:lenversion)//
     .       '/'//period(:lenperiod)//'/'//lseg//'.DPT'
      print*,csvfname
      open(csvfile,file=csvfname,status='old',iostat=err)

      if (err.ne.0) stop 'ERROR opening .DPT file '
*************read in the data to local variables************************
      csvndata = 1
      do
           read (csvfile,'(a300)',end=1001) longline
           call d2x(longline,last)
           read(longline,*,end=994,err=994)
     .     csvdata(csvndata,1),csvdata(csvndata,2),
     .     csvdata(csvndata,3),csvdata(csvndata,4),
     .     csvdata(csvndata,5)

           csvdata(csvndata,5) = 1.80 * csvdata(csvndata,5) + 32.0

           !print*,csvdata(csvndata,1),csvdata(csvndata,2)

           if ( csvdata(csvndata,1).ge.sdate(1)
     .           .and.csvdata(csvndata,1).le.edate(1) .and.
     .          csvdata(csvndata,2).ge.sdate(2)
     .           .and.csvdata(csvndata,2).le.edate(2) .and.
     .          csvdata(csvndata,3).ge.sdate(3)
     .           .and.csvdata(csvndata,3).le.edate(3) .and.
     .          csvdata(csvndata,4).ge.sdate(4)
     .           .and.csvdata(csvndata,4).le.edate(4) ) then
                csvndata=csvndata+1
           end if
      end do
 1001 continue
      close (csvfile)
      csvndata = csvndata - 1
*************check if data needed is there*********
      call timdif(
     I            sdate,edate,HCODE,TSTEP,
     O            nvals)

      if (nvals .ne. csvndata) then
           print*,'nval != csvndata',nvals, csvndata 
           stop 'PROBLEM ERROR with input CSV file, missing data'
      end if
************check for valid PRC range in all data***********************
      do i = 1, nvals
           if(csvdata(i,5).lt.hmin.or.csvdata(i,5).gt.hmax) then
                 print*, 'data= ', csvdata(i,5)
                 print*, 'PROBLEM ERROR Hourly data outside valid range'
           end if
      end do
************check for valid range using annual sum and print************
      oldyear = csvdata(1,1)
      dacc = 0.0
      ndacc = 0
      do i = 1,csvndata
           if(oldyear.ne.csvdata(i,1)) then
                   dacc = dacc / ndacc
                   print*,'DDPT, ',lseg,',',oldyear,',',dacc
                   if(dacc.lt.ymin.or.dacc.gt.ymax) then
                        print*, 'PROBLEM ERROR Annual data out of range'
                   end if
                   oldyear = csvdata(i,1)
                   dacc = 0.0
                   ndacc = 0
           end if
           dacc  = dacc + csvdata(i,5)
           ndacc = ndacc + 1
      end do
      dacc = dacc / ndacc
      print*,'DDPT, ',lseg,',',oldyear,',',dacc
************open wdm and read existing data*****************************
      dsn = 1001
      print*,lseg,' ',dsn,' DEWP'
      nvals = nvals/24
      call wdtget(
     I            wdmfile,dsn,TSTEP,sdate,nvals,
     I            dtran, qualfg, DCODE,
     O            hval, retcod)
      if (retcod.ne.0) stop 'PROBLEM ERROR getting wdm timeseries'
************copy climate data from csv array to insert hval ************
      do i = 1, nvals
           hval(i) = 0
           do j=1,24
               hval(i)=hval(i) + csvdata((i-1)*24+j,5)/24
           end do
      end do
************write back to the wdm file**********************************
      print*,'writing to ', wdmfname
      call wdtput(
     I            wdmfile,dsn,TSTEP,sdate,nvals,
     I            dtovwr, qualfg, DCODE,hval,
     O            retcod)
      if (retcod.ne.0) then
            print*, 'retcod = ', retcod
            stop 'ERROR writing wdm'
      end if 
************write back to the wdm file**********************************
      call wdflcl(
     I            wdmfile,
     O            retcod)
      if (retcod.ne.0) stop 'ERROR closing wdm'
      end if
************************************************************************
************************ ##### DDPT FINISH ##### ***********************
************************************************************************












************************************************************************
************************ ##### DCLC START ##### ************************
************************************************************************
      if ( DCLC .eq. 1) then
      wdmfname = 'met_'//lseg//'.wdm'
      call wdbopn(wdmfile,wdmfname,0,retcod)  ! open read/write 
      if (retcod.ne.0) then
             print*, 'retcod = ', retcod 
             stop 'ERROR opening wdm'
      end if

      csvfname = '../../../'//
     .        'input/unformatted/'//
     .       datasource(:lendatasource)//'/'//version(:lenversion)//
     .       '/'//'RNMax'//'/'//lseg//'.RNMax'
      open(csvfile,file=csvfname,status='old',iostat=err)
      if (err.ne.0) stop 'ERROR opening .RNMax file '

      do
           read (csvfile,'(a300)',end=10050) longline
           call d2x(longline,last)
           read(longline,*,end=994,err=994)
     .       tIMM, tIDD, tIHH, tRData
           RNMAX(tIMM,tIDD,tIHH) = tRData
      end do
10050 continue
      close (csvfile)

      csvfname = '../../../'//
     .        'input/unformatted/'//
     .       datasource(:lendatasource)//'/'//version(:lenversion)//
     .       '/'//period(:lenperiod)//'/'//lseg//'.PRC'
      print*,csvfname
      open(csvfile,file=csvfname,status='old',iostat=err)

      if (err.ne.0) stop 'ERROR opening .PRC file '
*************read in the data to local variables************************
      csvndata = 1
      do
           read (csvfile,'(a300)',end=10051) longline
           call d2x(longline,last)
           read(longline,*,end=994,err=994)
     .     csvdata(csvndata,1),csvdata(csvndata,2),
     .     csvdata(csvndata,3),csvdata(csvndata,4),
     .     PRECIP(csvndata)

           if ( csvdata(csvndata,1).ge.sdate(1)
     .           .and.csvdata(csvndata,1).le.edate(1) .and.
     .          csvdata(csvndata,2).ge.sdate(2)
     .           .and.csvdata(csvndata,2).le.edate(2) .and.
     .          csvdata(csvndata,3).ge.sdate(3)
     .           .and.csvdata(csvndata,3).le.edate(3) .and.
     .          csvdata(csvndata,4).ge.sdate(4)
     .           .and.csvdata(csvndata,4).le.edate(4) ) then
                csvndata=csvndata+1
           end if
      end do
10051 continue
      close (csvfile)
      csvndata = csvndata - 1
*************check if data needed is there*********
      call timdif(
     I            sdate,edate,HCODE,TSTEP,
     O            nvals)

      if (nvals .ne. csvndata) then
           print*,'nval != csvndata',nvals, csvndata 
           stop 'PROBLEM ERROR with input CSV file, missing data'
      end if

*******************************************
      hmin = 0.0
      hmax = 10.0
      ymin = 0.0
      ymax = 10.0
      csvfname = '../../../'//
     .        'input/unformatted/'//
     .       datasource(:lendatasource)//'/'//version(:lenversion)//
     .       '/'//period(:lenperiod)//'/'//lseg//'.RAD'
      print*,csvfname
      open(csvfile,file=csvfname,status='old',iostat=err)

      if (err.ne.0) stop 'ERROR opening .RAD file '
*************read in the data to local variables************************
      csvndata = 1
      do
           read (csvfile,'(a300)',end=10052) longline
           call d2x(longline,last)
           read(longline,*,end=994,err=994)
     .     csvdata(csvndata,1),csvdata(csvndata,2),
     .     csvdata(csvndata,3),csvdata(csvndata,4),
     .     csvdata(csvndata,5)

           !print*,csvdata(csvndata,1),csvdata(csvndata,2)

           if ( csvdata(csvndata,1).ge.sdate(1)
     .           .and.csvdata(csvndata,1).le.edate(1) .and.
     .          csvdata(csvndata,2).ge.sdate(2)
     .           .and.csvdata(csvndata,2).le.edate(2) .and.
     .          csvdata(csvndata,3).ge.sdate(3)
     .           .and.csvdata(csvndata,3).le.edate(3) .and.
     .          csvdata(csvndata,4).ge.sdate(4)
     .           .and.csvdata(csvndata,4).le.edate(4) ) then

                tIMM = csvdata(csvndata,2)
                tIDD = csvdata(csvndata,3)
                tIHH = csvdata(csvndata,4)

                if      ( PRECIP(csvndata) .gt. 0 ) then
                     csvdata(csvndata,5) = 10
                else if ( RNMAX(tIMM,tIDD,tIHH) .eq. 0 ) then
                     csvdata(csvndata,5) = 0
                else
                     csvdata(csvndata,5) = 10 * (1-csvdata(csvndata,5) /
     .                   RNMax(tIMM,tIDD,tIHH) )
                end if
                csvndata=csvndata+1
           end if
      end do
10052 continue
      close (csvfile)
      csvndata = csvndata - 1
*************check if data needed is there*********
      call timdif(
     I            sdate,edate,HCODE,TSTEP,
     O            nvals)

      if (nvals .ne. csvndata) then
           print*,'nval != csvndata',nvals, csvndata 
           stop 'PROBLEM ERROR with input CSV file, missing data'
      end if
************check for valid PRC range in all data***********************
      do i = 1, nvals
           if(csvdata(i,5).lt.hmin.or.csvdata(i,5).gt.hmax) then
                 print*, 'data= ', csvdata(i,5)
                 print*, 'PROBLEM ERROR Hourly data outside valid range'
           end if
      end do
************check for valid range using annual sum and print************
      oldyear = csvdata(1,1)
      dacc = 0.0
      ndacc = 0
      do i = 1,csvndata
           if(oldyear.ne.csvdata(i,1)) then
                   dacc = dacc / ndacc
                   print*,'DCLC, ',lseg,',',oldyear,',',dacc
                   if(dacc.lt.ymin.or.dacc.gt.ymax) then
                        print*, 'PROBLEM ERROR Annual data out of range'
                   end if
                   oldyear = csvdata(i,1)
                   dacc = 0.0
                   ndacc = 0
           end if
           dacc  = dacc + csvdata(i,5)
           ndacc = ndacc + 1
      end do
      dacc = dacc / ndacc
      print*,'DCLC, ',lseg,',',oldyear,',',dacc
************open wdm and read existing data*****************************
      dsn = 1005
      print*,lseg,' ',dsn,' CLDC'
      nvals = nvals/24
      call wdtget(
     I            wdmfile,dsn,TSTEP,sdate,nvals,
     I            dtran, qualfg, DCODE,
     O            hval, retcod)
      if (retcod.ne.0) stop 'PROBLEM ERROR getting wdm timeseries'
************copy climate data from csv array to insert hval ************
      do i = 1, nvals
           hval(i) = 0
           SunnyHrs = 0
           do j=1,24
               hval(i)=hval(i) + csvdata((i-1)*24+j,5)
               if ( csvdata((i-1)*24+j,5) .gt. 0 ) then
                    SunnyHrs = SunnyHrs + 1
               end if
           end do
           if ( SunnyHrs > 0 ) then
                hval(i) = hval(i) / SunnyHrs
           else
                hval(i) = 10
           end if
      end do
************write back to the wdm file**********************************
      print*,'writing to ', wdmfname
      call wdtput(
     I            wdmfile,dsn,TSTEP,sdate,nvals,
     I            dtovwr, qualfg, DCODE,hval,
     O            retcod)
      if (retcod.ne.0) then
            print*, 'retcod = ', retcod
            stop 'ERROR writing wdm'
      end if 
************write back to the wdm file**********************************
      call wdflcl(
     I            wdmfile,
     O            retcod)
      if (retcod.ne.0) stop 'ERROR closing wdm'
      end if
************************************************************************
************************ ##### DCLC FINISH ##### ***********************
************************************************************************











      close (msgfile)
      return

994   print*,'Problem reading file:',csvfname
      goto 999

999   continue

      end

