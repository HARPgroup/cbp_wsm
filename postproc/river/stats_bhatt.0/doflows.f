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
      integer fileptrWYR, fileptrN2A, fileptrM2O
      integer lenfilenameWYR, lenfilenameN2A, lenfilenameM2O
      parameter(fileptrWYR=11, fileptrN2A=12, fileptrM2O=13)

      character*10 postfix
      integer lenpostfix
      integer hour1, hour2
      integer oldyear
      real flowWYR, flowN2A, flowM2O ! flow water.year, nov-apr, may-oct
      integer flagN2A, flagM2O
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
      print*, filenameWYR
      call lencl(filenameWYR, lenfilenameWYR)
      call lencl(filenameN2A, lenfilenameN2A)
      call lencl(filenameM2O, lenfilenameM2O)
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
*********** CALCULATE WATER YEAR : START  ****************************
      ny = sdate(1)
      nm = sdate(2)
      nd = sdate(3)
      nh = max(sdate(4),1)
      print*, 'a ', ny, nm, nd, nh
      call gethours(sdate,year1,year2,hour1,hour2)
      do i = 1, hour1-1
           call onehour(ny,nm,nd,nh)
      end do
      print*, 'b ', ny, nm, nd, nh
      do while (nm.ne.10)
           call onehour(ny,nm,nd,nh)
      end do
      print*, 'c ', ny, nm, nd, nh
      
      write(fileptrWYR,'(a8,a,a13,a,$)') rscen(:lenrscen),',',
     .       rseg(:lenrseg),','
      write(fileptrN2A,'(a8,a,a13,a,$)') rscen(:lenrscen),',',
     .       rseg(:lenrseg),','
      write(fileptrM2O,'(a8,a,a13,a,$)') rscen(:lenrscen),',',
     .       rseg(:lenrseg),','

      oldyear = ny
      do while (ny.le.year2)
         flowWYR = flowWYR + simfl(ny,nm,nd,nh)

         call onehour(ny,nm,nd,nh)

         if(oldyear.ne.ny.and.nm.eq.10) then
              write (fileptrWYR,'(e14.7,a,$)') flowWYR,','
c              write (*,'(a6,$)') 'hello '
              print*, '$', ny, nm, nd, nh
              flowWYR = 0
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
      print*, 'a ', ny, nm, nd, nh
      call gethours(sdate,year1,year2,hour1,hour2)
      do i = 1, hour1-1
           call onehour(ny,nm,nd,nh)
      end do
      do while (nm.ne.11)
           call onehour(ny,nm,nd,nh)
      end do
      flagN2A = 0
      do while (ny.le.year2)
         if(nm.gt.10.or.nm.lt.5) then
             flowN2A = flowN2A + simfl(ny,nm,nd,nh)
             flagN2A = 1
         end if

         call onehour(ny,nm,nd,nh)

         if(flagN2A.eq.1.and.nm.eq.5) then
              write (fileptrN2A,'(e14.7,a,$)') flowN2A,','
              flagN2A = 0
              flowN2A = 0
         end if
      end do

      write(fileptrN2A,*)
*********** CALCULATE NOV-APR : END  ****************************

*********** CALCULATE MAY-OCT : START ****************************
      ny = sdate(1)
      nm = sdate(2)
      nd = sdate(3)
      nh = max(sdate(4),1)
      print*, 'a ', ny, nm, nd, nh
      call gethours(sdate,year1,year2,hour1,hour2)
      do i = 1, hour1-1
           call onehour(ny,nm,nd,nh)
      end do
      do while (nm.ne.5)
           call onehour(ny,nm,nd,nh)
      end do
      flagM2O = 0
      do while (ny.le.year2)
         if(nm.gt.4.and.nm.lt.11) then
c             print*, '#', ny, nm, nd, nh
             flowM2O = flowM2O + simfl(ny,nm,nd,nh)
             flagM2O = 1
         end if

         call onehour(ny,nm,nd,nh)

         if(flagM2O.eq.1.and.nm.eq.11) then
              write (fileptrM2O,'(e14.7,a,$)') flowM2O,','
              flagM2O = 0
              flowM2O = 0
         end if
      end do

      write(fileptrM2O,*)
*********** CALCULATE SEASONAL : END  ****************************

      
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

