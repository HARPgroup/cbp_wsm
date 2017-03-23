************************************************************************
** Subroutine to split water quality data into Calibration and        **
**   validation data sets specifically for the Patuxent               **
**                                                                    **
** MDE and ICPRB expressed concern in an email from Ross Mandel on    **
**   7/7/2008 that the Patuxent was not well calibrated due to the    **
**   phase 5 model's inability to capture the large decrease in       **
**   nutrient concentrations observed in the late 1980's              **
**                                                                    **
**   In general, then calibration is low during the 1980s and high    **
**   during the 1991-2000 period, which is the selected hydrologic    **
**   averaging period for the TMDL.                                   **
**                                                                    **
**   The Patuxent above Bowie will only be calibrated to 1991-2000    **
**   data with all other data going into the validation data set      **
**                                                                    **
************************************************************************
      implicit none
      include 'WQdata.inc'

      character*110 dline
      character*100 cfnam,vfnam  ! calibration and validation file names
  
      integer year1, year2
      data year1,year2 /1985,2005/

      integer Calyear1, Calyear2   
      data Calyear1,Calyear2 /1991,2000/     ! calibration period

      integer nr,n,nf
      integer nobs,no

********* variables to read and store data
      integer obyear(ndaymax),year
      integer obmonth(ndaymax),month
      integer obday(ndaymax),day
      integer obhour(ndaymax),hour
      integer obmint(ndaymax),mint
      real obdata(ndaymax),val
      character*1 Qflag(ndaymax),Q
      character*10 station(ndaymax),sta

************ handle confluence (0003) stations
      integer ntype,nt  ! 2 types, regular and confluence
      parameter (ntype = 2)

************************* END DECLARATIONS *****************************
           
********** get river segs
      call getrsegs(nrsegs,rsegs)

***********get constituent names
      call getconcname(nconcs,concname) 

******* Loop over all segments          
      do nr = 1, nrsegs

        if (rsegs(nr)(:2).ne.'XU') cycle  ! only for upper Patuxent

********* loop over segment types
        do nt = 1,ntype
                                     
          if (nt.eq.1) tseg = rsegs(nr)  ! set segment names
          if (nt.eq.2) tseg = rsegs(nr)(:9)//'0003'  ! confluence seg

**************** determine if this is a validation segment
          print*,' splitting out validation data set for '//tseg
          call ttyput('   ')

******* loop over all constituents
          do nc = 1, nconcs 

***********Open existing files to read data
            fnam = tree//'pp/observed/alldata/'//concname(nc)//
     .              '/'//tseg//'.O'//concname(nc)
            open (unit=dfile,file=fnam,status='old',iostat=err)

            if (err.eq.2) cycle  ! file doesn't exist
            if (err.ne.0) go to 990

            call ttyput(' ')
            call ttyput(concname(nc))

************** file exists, read into memory
            nobs = 0
            
            do 
              read(dfile,'(a110)',err=994,end=222) dline
              read(dline,*,err=995,end=111)
     .              year,month,day,hour,mint,val,Q,sta
111           continue
              if (abs(val+9).lt.0.01) cycle  ! missing value
              if (val.lt.0.001) then  ! cal routine can not handle negs
                val = 0.001
                Q = '<'
              end if
              if (year.ge.year1 .and. year.le.year2) then
                nobs = nobs + 1
                if (nobs .ge. ndaymax) go to 991
                obyear(nobs) = year
                obmonth(nobs)= month
                obday(nobs)  = day
                obhour(nobs) = hour
                obmint(nobs) = mint
                obdata(nobs) = val
                Qflag(nobs)  = Q
                station(nobs)= sta
              end if
              val = -9
              Q = ''
              sta = ''
            end do 

222         close(dfile)

            if (nobs.eq.0) cycle  ! if no data in date range


**************** open the calibration and validation files
            cfnam =tree//'pp/observed/calib/'//concname(nc)//
     .             '/'//tseg//'.O'//concname(nc)
            open (unit=dfile+1,file=cfnam,status='unknown',iostat=err)
            if (err.ne.0) go to 992

            vfnam =tree//'pp/observed/valid/'//concname(nc)//
     .              '/'//tseg//'.O'//concname(nc)
            open (dfile+2,file=vfnam,status='unknown',iostat=err)
            if (err.ne.0) go to 993
             
************* loop over all obs and place in the correct file 
            do n = 1,nobs

              nf = 2
              if(obyear(n).ge.Calyear1.and.obyear(n).le.Calyear2) nf = 1
              write(dfile+nf,1001) obyear(n),obmonth(n),obday(n),
     .                             obhour(n),obmint(n),obdata(n),
     .                             Qflag(n),station(n)
            end do  
            close(dfile+1)
            close(dfile+2)

************* close loops
          end do        ! loop over concentration types
          print*,' '
        end do        !loop over segment types
      end do        !loop over segments

      stop 

1001  format(i4,',',i2,',',i2,',',i2,',',i2,',',e12.5,',',a1,',',a10)  

************************ ERROR SPACE
990   report(1) = 'Problem opening alldata file, '
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = fnam
      go to 999

991   report(1) = 'the total numbers greater than max specified'
      report(2) = ' need to check array boundary     '
      report(3) = ' '
      go to 999

992   report(1) = 'Problem opening calibration data file, '
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = cfnam
      go to 999

993   report(1) = 'Problem opening validation data file, '
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = vfnam
      go to 999

994   report(1) = 'Problem reading alldata file: near line:'
      report(2) = fnam
      write(report(3),*) year,month,day,hour,mint,val,Q,sta
      go to 999

995   report(1) = 'Problem reading alldata line: '
      report(2) = fnam
      report(3) = dline
      go to 999

999   call stopreport(report)

      end 
