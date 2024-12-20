************************************************************************
**  main calling program for the calculation of statistics            **
************************************************************************
      subroutine doflows(
     I                   rscen,rseg,year1,year2,nvals,flowvals,
     I                   sdate, postfix)
      implicit none
      include 'Rstats.inc'

      integer ndays       ! test dates for checking wdm
      integer ifl

      character(100) pfname,obfnam 

      integer i,ny,nm,nd,nh,nmin  ! indices
      integer year1,year2             ! first and last year to average

      real flowvals(ndaymax*24)          ! flow
      real tflo            ! daily flow                        

      character*200 filenameWYR, filenameN2A, filenameM2O
      character*200 filename7dMin, filename1dMax
      integer fileptrWYR, fileptrN2A, fileptrM2O
      integer fileptr7dMin, fileptr1dMax
      integer lenfilenameWYR, lenfilenameN2A, lenfilenameM2O
      integer lenfilename7dMin, lenfilename1dMax
      parameter(fileptrWYR=11, fileptrN2A=12, fileptrM2O=13)
      parameter(fileptr7dMin=14, fileptr1dMax=15)

      character*10 postfix
      integer lenpostfix
      integer hour1, hour2
      integer oldyear
      real flowWYR, flowN2A, flowM2O ! flow water.year, nov-apr, may-oct
      real flow7dMin, flow1dMax
      real tmpflow7dMin, tmpflow1dMax
      integer flagN2A, flagM2O
      integer HoursCount, tmpI, AVG_DAYS
******************* END DECLARATIONS ***********************************

********************Calculate flow statistics 
*********** code to re-arrange the flow to get hourly 
      ny = sdate(1)
      nm = sdate(2)
      nd = sdate(3)
      nh = max(sdate(4),1)
      i = 1
      do while (ny.le.year2)
        simfl(ny,nm,nd,nh) = flowvals(i)   ! unit: ac-ft/hr
        i = i + 1
        call onehour(ny,nm,nd,nh)
      end do

************ check for flow obs.  If available, send to flowstats
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      call lencl(postfix, lenpostfix)     


      filenameWYR = outwdmdir//'river/'//rscen(:lenrscen)//
     .            '/stream/'//rscen(:lenrscen)//'_'//
     .            postfix(:lenpostfix)//'_FLO'//'.stats.csv'
      filenameN2A = outwdmdir//'river/'//rscen(:lenrscen)//
     .            '/stream/'//rscen(:lenrscen)//'_'//
     .            'Nov-Apr'//'_FLO'//'.stats.csv'
      filenameM2O = outwdmdir//'river/'//rscen(:lenrscen)//
     .            '/stream/'//rscen(:lenrscen)//'_'//
     .            'May-Oct'//'_FLO'//'.stats.csv'
      filename7dMin = outwdmdir//'river/'//rscen(:lenrscen)//
     .            '/stream/'//rscen(:lenrscen)//'_'//
     .            '7dMinCY'//'_FLO'//'.stats.csv'
      filename1dMax = outwdmdir//'river/'//rscen(:lenrscen)//
     .            '/stream/'//rscen(:lenrscen)//'_'//
     .            '1dMaxWY'//'_FLO'//'.stats.csv'
      print*, filenameWYR
      call lencl(filenameWYR, lenfilenameWYR)
      call lencl(filenameN2A, lenfilenameN2A)
      call lencl(filenameM2O, lenfilenameM2O)
      call lencl(filename7dMin, lenfilename7dMin)
      call lencl(filename1dMax, lenfilename1dMax)
      open(fileptrWYR,file=filenameWYR(:lenfilenameWYR),ACCESS='APPEND',
     .     status='UNKNOWN',
     .     iostat=err)
      print*,err
      if (err.ne.0) stop 'error opening FLO WYR STAT file '

      open(fileptrN2A,file=filenameN2A(:lenfilenameN2A),ACCESS='APPEND',
     .     status='UNKNOWN',
     .     iostat=err)
      print*,err
      if (err.ne.0) stop 'error opening FLO N2A STAT file '

      open(fileptrM2O,file=filenameM2O(:lenfilenameM2O),ACCESS='APPEND',
     .     status='UNKNOWN',
     .     iostat=err)
      print*,err
      if (err.ne.0) stop 'error opening FLO M2O STAT file '

      open(fileptr7dMin,file=filename7dMin(:lenfilename7dMin),
     .     ACCESS='APPEND',
     .     status='UNKNOWN',
     .     iostat=err)
      print*,err
      if (err.ne.0) stop 'error opening FLO 7 Day Min STAT file '

      open(fileptr1dMax,file=filename1dMax(:lenfilename1dMax),
     .     ACCESS='APPEND',
     .     status='UNKNOWN',
     .     iostat=err)
      print*,err
      if (err.ne.0) stop 'error opening FLO 1 Day Max STAT file '
*********** CALCULATE WATER YEAR : START  ****************************
      ny = sdate(1)
      nm = sdate(2)
      nd = sdate(3)
      nh = max(sdate(4),1)
      print*, 'WY a ', ny, nm, nd, nh
      call gethours(sdate,year1,year2,hour1,hour2)
      do i = 1, hour1-1
           call onehour(ny,nm,nd,nh)
      end do
      print*, 'WY b ', ny, nm, nd, nh
      do while (nm.ne.10)
           call onehour(ny,nm,nd,nh)
      end do
      print*, 'WY c ', ny, nm, nd, nh
      
      write(fileptrWYR,'(a13,a,a13,a,$)') rscen(:lenrscen),',',
     .       rseg(:lenrseg),','

      oldyear = ny
      HoursCount = 0
      do while (ny.le.year2)
         flowWYR = flowWYR + simfl(ny,nm,nd,nh)
         HoursCount = HoursCount + 1

         call onehour(ny,nm,nd,nh)

         if(oldyear.ne.ny.and.nm.eq.10) then
              flowWYR = (flowWYR/HoursCount)
              flowWYR = (43560 * flowWYR) / 3600
              write (fileptrWYR,'(e14.7,a,$)') flowWYR,','
c              write (*,'(a6,$)') 'hello '
              print*, 'WY $ ', ny, nm, nd, nh
              flowWYR = 0
              HoursCount = 0
              oldyear = ny
         end if
      end do
      write(fileptrWYR,*)
*********** CALCULATE WATER YEAR : END  ****************************

*********** CALCULATE NOV-APR : START  ****************************
      ny = sdate(1)
      nm = sdate(2)
      nd = sdate(3)
      nh = max(sdate(4),1)
      print*, 'NA a ', ny, nm, nd, nh
      call gethours(sdate,year1,year2,hour1,hour2)
      do i = 1, hour1-1
           call onehour(ny,nm,nd,nh)
      end do
      print*, 'NA b ', ny, nm, nd, nh
      do while (nm.ne.11)
           call onehour(ny,nm,nd,nh)
      end do
      print*, 'NA c ', ny, nm, nd, nh

      write(fileptrN2A,'(a13,a,a13,a,$)') rscen(:lenrscen),',',
     .       rseg(:lenrseg),','

      flagN2A = 0
      HoursCount = 0
      do while (ny.le.year2)
         if(nm.gt.10.or.nm.lt.5) then
             flowN2A = flowN2A + simfl(ny,nm,nd,nh)
             HoursCount = HoursCount + 1
             flagN2A = 1
         end if

         call onehour(ny,nm,nd,nh)

         if(flagN2A.eq.1.and.nm.eq.5) then
              flowN2A = (flowN2A/HoursCount)
              flowN2A = (43560 * flowN2A) / 3600
              write (fileptrN2A,'(e14.7,a,$)') flowN2A,','
              print*, 'NA $ ', ny, nm, nd, nh
              flagN2A = 0 !Setting Zero ensures wait for next period until write
              flowN2A = 0
              HoursCount = 0
         end if
      end do

      write(fileptrN2A,*)
*********** CALCULATE NOV-APR : END  ****************************

*********** CALCULATE MAY-OCT : START ****************************
      ny = sdate(1)
      nm = sdate(2)
      nd = sdate(3)
      nh = max(sdate(4),1)
      print*, 'MO a ', ny, nm, nd, nh
      call gethours(sdate,year1,year2,hour1,hour2)
      do i = 1, hour1-1
           call onehour(ny,nm,nd,nh)
      end do
      print*, 'MO b ', ny, nm, nd, nh
      do while (nm.ne.5)
           call onehour(ny,nm,nd,nh)
      end do
      print*, 'MO c ', ny, nm, nd, nh

      write(fileptrM2O,'(a13,a,a13,a,$)') rscen(:lenrscen),',',
     .       rseg(:lenrseg),','

      flagM2O = 0
      HoursCount = 0
      do while (ny.le.year2)
         if(nm.gt.4.and.nm.lt.11) then
c             print*, '#', ny, nm, nd, nh
             flowM2O = flowM2O + simfl(ny,nm,nd,nh)
             HoursCount = HoursCount + 1
             flagM2O = 1
         end if

         call onehour(ny,nm,nd,nh)

         if(flagM2O.eq.1.and.nm.eq.11) then
              flowM2O = (flowM2O/HoursCount)
              flowM2O = (43560 * flowM2O) / 3600
              write (fileptrM2O,'(e14.7,a,$)') flowM2O,','
              print*, 'MO $ ', ny, nm, nd, nh
              flagM2O = 0
              flowM2O = 0
              HoursCount = 0
         end if
      end do

      write(fileptrM2O,*)
*********** CALCULATE SEASONAL : END  ****************************


*********** CALCULATE 1-DAY-MAX-WATER-YEAR : START  ****************************
      AVG_DAYS = 1
      ny = sdate(1)
      nm = sdate(2)
      nd = sdate(3)
      nh = max(sdate(4),1)
      print*, '1D a ', ny, nm, nd, nh
      call gethours(sdate,year1,year2,hour1,hour2)
      tmpI = 1
      do i = 1, hour1-1
           call onehour(ny,nm,nd,nh)
           tmpI = tmpI + 1
      end do
      print*, '1D b ', ny, nm, nd, nh
      do while (nm.ne.10)
           call onehour(ny,nm,nd,nh)
           tmpI = tmpI + 1
      end do
      print*, '1D c ', ny, nm, nd, nh
         
      write(fileptr1dMax,'(a13,a,a13,a,$)') rscen(:lenrscen),',',
     .       rseg(:lenrseg),','
              
      oldyear = ny
      flow1dMax    = 0
      do while (ny.le.year2)
         if(nh == 12) then
              tmpflow1dMax = 0
              do i=(tmpI-11+24*(AVG_DAYS-1)),(tmpI+12+24*(AVG_DAYS-1)) ! TODO: Fix Potential Bug
                  tmpflow1dMax = tmpflow1dMax + flowvals(i)
              end do
              if(tmpflow1dMax.gt.flow1dMax) then
                   flow1dMax = tmpflow1dMax
              end if
         end if

         call onehour(ny,nm,nd,nh)
         tmpI = tmpI + 1

         if(oldyear.ne.ny.and.nm.eq.10) then
              flow1dMax = flow1dMax/(24*AVG_DAYS)
              flow1dMax = (43560 * flow1dMax) / 3600
              write (fileptr1dMax,'(e14.7,a,$)') flow1dMax,','
c              write (*,'(a6,$)') 'hello '
              print*, '1D $ ', ny, nm, nd, nh
              flow1dMax = 0
              oldyear = ny
         end if
      end do
      write(fileptr1dMax,*)
*********** CALCULATE 1-DAY-MAX-WATER-YEAR : END  ****************************


*********** CALCULATE 7-DAY-MIN-CLIMATE-YEAR : START  ****************************
      AVG_DAYS = 7
      ny = sdate(1)
      nm = sdate(2)
      nd = sdate(3)
      nh = max(sdate(4),1)
      print*, '7D a ', ny, nm, nd, nh
      call gethours(sdate,year1,year2,hour1,hour2)
      tmpI = 1
      do i = 1, hour1-1
           call onehour(ny,nm,nd,nh)
           tmpI = tmpI + 1
      end do
      print*, '7D b ', ny, nm, nd, nh
      do while (nm.ne.4)
           call onehour(ny,nm,nd,nh)
           tmpI = tmpI + 1
      end do
      print*, '7D c ', ny, nm, nd, nh

      write(fileptr7dMin,'(a13,a,a13,a,$)') rscen(:lenrscen),',',
     .       rseg(:lenrseg),','

      oldyear = ny
      flow7dMin    = 1E99
      do while (ny.le.year2)
         if(nh == 12) then
              tmpflow7dMin = 0
              do i=(tmpI-11+24*(AVG_DAYS-1)),(tmpI+12+24*(AVG_DAYS-1)) ! TODO: Fix Potential Bug
                  tmpflow7dMin = tmpflow7dMin + flowvals(i)
              end do
              if(tmpflow7dMin.lt.flow7dMin) then
                   flow7dMin = tmpflow7dMin
              end if
         end if

         call onehour(ny,nm,nd,nh)
         tmpI = tmpI + 1

         if(oldyear.ne.ny.and.nm.eq.4) then
              flow7dMin = flow7dMin/(24*AVG_DAYS)
              flow7dMin = (43560 * flow7dMin) / 3600
              write (fileptr7dMin,'(e14.7,a,$)') flow7dMin,','
c              write (*,'(a6,$)') 'hello '
              print*, '7D $ ', ny, nm, nd, nh
              flow7dMin = 1E99
              oldyear = ny
         end if
      end do
      write(fileptr7dMin,*)
*********** CALCULATE 7-DAY-MIN-CLIMATE-YEAR : END  ****************************
      
      return
  
************************ ERROR REPORTING 

993   report(1) = 'Error in PART program'
      if (err.eq.993) then
        report(2) = 'could not find segment '//rseg//' in file:'
        report(3) = catdir//'geo/'//geoscen(:lengeoscen)//
     .              '/watershed_area.csv'
      else if (err.eq.994) then
        report(2) = 'trouble reading file:'
        report(3) = catdir//'geo/'//geoscen(:lengeoscen)//
     .              '/watershed_area.csv'
      else if (err.eq.996) then
        report(2) = 'could not find segment '//rseg//' in file:'
        report(3) = catdir//'geo/'//geoscen(:lengeoscen)//
     .              '/watershed_area.csv'
      else
        report(2) = 'unspecified error, check file'
        report(3) = './pp/src/postproc/river/part/part_sub.f'
      end if
      go to 999

999   call stopreport(report)

      end

