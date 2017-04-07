************************************************************************
**  Program to calculate long-term statistics from a flow record and  **
**   calculate the same statistics for various-length spans between   **
**   1985 and 2005.  This information will be used in the selection   **
**   of the representative hydrologic period for the bay TMDL         **
************************************************************************
      implicit none
      include '../../../src/lib/inc/standard.inc'

*********** date variables
      integer firstyear,lastyear  ! for setting indices
      parameter (firstyear = 1900, lastyear = 2008)

      integer simyear1,simyear2
      parameter (simyear1=1985,simyear2=2005)

      integer datayear1,datayear2  ! range of data
      integer modelyear1,modelyear2  ! range of model
      integer year1,year2    ! ad hoc limits
      integer year,day
      integer ny,i

      integer duration   ! length of proposed averaging period
      integer nperiods,np  ! number of possible periods for a duration

********** file name variables
      character*100 matlabfn
      character*4 cy1,cy2  ! character version of years

*********** flow variables
      real flow(firstyear:lastyear,366)
      real logflow(firstyear:lastyear,366)
      integer nobs(firstyear:lastyear)  ! number of obs in that years

      real annflow(firstyear:lastyear)

      logical fullyear(firstyear:lastyear)  ! year has all records?
      logical doyear(firstyear:lastyear)  ! year used in analysis
      logical skipperiod   ! true if no full years during period
      integer nfullyears   !number of full years

*********** reading variables
      character*4 USGS  ! the characters 'USGS' in the first column
      integer station
      character*10 date
      real tempflow
      character*1 flag

*********** statistics
      real longmean,longvar,longlogvar  ! long term stats
      real annlongmean,annlongvar,annlonglogvar ! long term annual stats
      real mean,var,logvar  ! short term stats
      real annmean,annvar,annlogvar  ! short term annual stats
      real ksK  ! K statistic for the Kolmogorov-smirnov test
      real annksK  ! annual k stat
      real logksK  ! log k stat

*********** utilities
      integer ndaysinyear
      external ndaysinyear

      real average,variance
      external average,variance

      integer lenbasin

      integer maxvals,maxyears ! maximum number of flow records & years
      parameter (maxyears = lastyear-firstyear+1)
      parameter (maxvals = maxyears*366)
      integer nvals  ! number of values for an analysis
      integer longvals  ! number of long-term values
      integer logvals  ! number of log long-term values
      integer annlongvals  ! number of years for long-term record

      real x(maxvals)  ! vector to evaluate for mean, var, etc.
      real longx(maxvals)  ! vector to evaluate for mean, var, etc.
      real logx(maxvals)  ! vector to evaluate for mean, var, etc.
      real annlongx(maxyears) ! vector of long term flows

******************* END DECLARATIONS ***********************************

******** open data file
      read*, fnam
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

************* read data file, store flow and number of obs per year
      do year = firstyear,lastyear
        nobs(year) = 0
      end do
      do 
        read(11,*,end=111,err=222) USGS,station,date,tempflow,flag
        read(date(:4),'(i4)')year
        nobs(year) = nobs(year) + 1
        flow(year,nobs(year)) = tempflow
        logflow(year,nobs(year)) = log10(tempflow)
222     continue
      end do
111   close(11)

******** open write file
      do i = 1,len(fnam)
        if (fnam(i:i+3).eq.'flow') then
          lenbasin = i-2
          fnam(i:i+3) = 'csv '
          exit
        end if
      end do
      open(12,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

********** open matlab script for running KStest
      matlabfn = 'matlab_scripts/'//fnam(:lenbasin)//'.m'
      open(14,file=matlabfn,status='unknown',iostat=err)
      if (err.ne.0) go to 995
      write(14,*) 'clear all'

*********** determine which years are full years of data
      do year = firstyear,lastyear
        fullyear(year) = .false.
      end do
      nfullyears = 0
      do year = firstyear,lastyear
        if (nobs(year).eq.ndaysinyear(year)) then
          nfullyears = nfullyears + 1
          fullyear(year) = .true.
        end if
      end do
      write(12,*)'there are ',nfullyears,' full years in ',fnam
      open(15,file='full_years/'//fnam(:lenbasin)//'_full_years.csv',
     .     status='unknown')
      do year = firstyear,lastyear
        if (fullyear(year)) then
          write(15,*) fnam(:lenbasin),year
        end if
      end do
      close(15)

********** get long term mean and variance
      call vectorize(
     I               flow,fullyear,
     I               firstyear,lastyear,maxvals,
     O               longx,longvals)
      longmean = average(longx,longvals,maxvals,err)
      if (err.ne.0) go to 993
      longvar = variance(longx,longvals,maxvals,err)
      if (err.ne.0) go to 994

      matlabfn = 'matlab_data/'//fnam(:lenbasin)//'_daily_long.csv'
      call writematlab(matlabfn,longx,longvals,maxvals)
      write(14,*) 'long = csvread('//char(39)//'../matlab_data/'//
     .             fnam(:lenbasin)//'_daily_long.csv'//char(39)//');'

********* get long term log variance
      call vectorize(
     I               logflow,fullyear,
     I               firstyear,lastyear,maxvals,
     O               logx,logvals)
      longlogvar = variance(logx,logvals,maxvals,err)
      if (err.ne.0) go to 994

      matlabfn = 'matlab_data/'//fnam(:lenbasin)//'_log_daily_long.csv'
      call writematlab(matlabfn,logx,logvals,maxvals)
      write(14,*) 'loglong = csvread('//char(39)//'../matlab_data/'//
     .            fnam(:lenbasin)//'_log_daily_long.csv'//char(39)//');'

************ get long term annual mean and variance
      call annvector(
     I               flow,fullyear,
     I               firstyear,lastyear,maxyears,
     O               annlongx,annlongvals)
      annlongmean = average(annlongx,annlongvals,maxyears,err)
      if (err.ne.0) go to 993
      annlongvar = variance(annlongx,annlongvals,maxyears,err)
      if (err.ne.0) go to 994

      matlabfn = 'matlab_data/'//fnam(:lenbasin)//'_annual_long.csv'
      call writematlab(matlabfn,annlongx,annlongvals,maxyears)
      write(14,*) 'annlong = csvread('//char(39)//'../matlab_data/'//
     .             fnam(:lenbasin)//'_annual_long.csv'//char(39)//');'

********** initialize output file
      write(12,'(3a)') 'duration,year1,year2,mean,variance,',
     .                 'variance of logs,annual mean,',
     .                 'annual variance,ksK,log ksK, annual ksK'

      write(12,1234)lastyear-firstyear+1,firstyear,lastyear,
     .       longmean,longvar,longlogvar,annlongmean,annlongvar
*********** loop over durations and periods, get the means and variances
********** for each consecutive year stretch in each duration
      do duration = 1,simyear2 - simyear1 + 1

************ calculate number of periods
        nperiods = simyear2 - simyear1 - duration + 2
        do np = 1,nperiods

************ calculate first and last years for this period and duration
          year1 = simyear1 + np - 1
          year2 = year1 + duration - 1

          write(cy1,'(i4)') year1
          write(cy2,'(i4)') year2

************* set years to calculate
          do year = firstyear,lastyear
            doyear(year) = .false.
          end do
          skipperiod = .true.     ! check for any full years
          do year = year1,year2
            if (fullyear(year)) then
              doyear(year) = .true.
              skipperiod = .false.
            end if
          end do
          if (skipperiod) cycle  ! skip period if no full years

*************** calculate log variance 
          call vectorize(
     I                   logflow,doyear,
     I                   firstyear,lastyear,maxvals,
     O                   x,nvals)
          logvar = variance(x,nvals,maxvals,err)
          if (err.ne.0) go to 994

************* write matlab data and scripts for KS test
          matlabfn = 'matlab_data/'//fnam(:lenbasin)//'_log_daily_'//
     .                cy1//'_'//cy2//'.csv'
          call writematlab(matlabfn,x,nvals,maxvals)
          write(14,*) 'short = csvread('//char(39)//'../matlab_data/'//
     .                 fnam(:lenbasin)//'_log_daily_'//cy1//'_'//cy2//
     .                 '.csv'//char(39)//');'
          write(14,*) '[h(',year1-1984,',',year2-1984,',3),p(',
     .                      year1-1984,',',year2-1984,',3)]',
     .                ' = kstest2(loglong,short);'
          write(14,*) 'cdfplot(loglong)'
          write(14,*) 'hold on'
          write(14,*) 'cdfplot(short,'//char(39)//'r'//char(39)//')'
          write(14,*) 'legend('//char(39)//'long term'//char(39)//','
     .                //char(39)//'short term'//char(39)//')'
          write(14,*) 'title('//char(39)//fnam(:lenbasin)//' log daily '
     .                //cy1//'-'//cy2//char(39)//')'
          write(14,*) 'xlabel('//char(39)//'log flow'//char(39)//')'
          write(14,*) 'ylabel('//char(39)//'frequency'//char(39)//')'
          write(14,*) 'print -dpsc ../matlab_ps_files/'//
     .                 fnam(:lenbasin)//'_log_daily_'//
     .                 cy1//'_'//cy2
          write(14,*) 'hold off'

************ calculate KS stats
          call KStest(
     I                x,logx,maxvals,nvals,logvals,
     O                logksK)

*************** calculate base statistics
          call vectorize(
     I                   flow,doyear,
     I                   firstyear,lastyear,maxvals,
     O                   x,nvals)
          mean = average(x,nvals,maxvals,err)
          if (err.ne.0) go to 993
          var = variance(x,nvals,maxvals,err)
          if (err.ne.0) go to 994

************* write matlab data and scripts for KS test
          matlabfn = 'matlab_data/'//fnam(:lenbasin)//'_daily_'//
     .                cy1//'_'//cy2//'.csv'
          call writematlab(matlabfn,x,nvals,maxvals)
          write(14,*) 'short = csvread('//char(39)//'../matlab_data/'//
     .                 fnam(:lenbasin)//
     .                 '_daily_'//cy1//'_'//cy2//'.csv'//char(39)//');'
          write(14,*) '[h(',year1-1984,',',year2-1984,',1),p(',
     .                      year1-1984,',',year2-1984,',1)]',
     .                ' = kstest2(long,short);'
          write(14,*) 'cdfplot(long)'
          write(14,*) 'hold on'
          write(14,*) 'cdfplot(short,'//char(39)//'r'//char(39)//')'
          write(14,*) 'legend('//char(39)//'long term'//char(39)//','
     .                //char(39)//'short term'//char(39)//')'
          write(14,*) 'title('//char(39)//fnam(:lenbasin)//' daily '
     .                //cy1//'-'//cy2//char(39)//')'
          write(14,*) 'xlabel('//char(39)//'flow'//char(39)//')'
          write(14,*) 'ylabel('//char(39)//'frequency'//char(39)//')'
          write(14,*) 'print -dpsc ../matlab_ps_files/'//
     .                 fnam(:lenbasin)//'_daily_'//
     .                 cy1//'_'//cy2
          write(14,*) 'hold off'


************ calculate KS stats
          call KStest(
     I                x,longx,maxvals,nvals,longvals,
     O                ksK)

************* calculate annual stats
          call annvector(
     I                   flow,doyear,
     I                   firstyear,lastyear,maxyears,
     O                   x,nvals)
          annmean = average(x,nvals,maxvals,err)
          if (err.ne.0) go to 993
          if (nvals.gt.1) then
            annvar = variance(x,nvals,maxvals,err)
            if (err.ne.0) go to 994
          else
            annvar = 0.0
          end if

************* write matlab data and scripts for KS test
          matlabfn = 'matlab_data/'//fnam(:lenbasin)//'_annual_'//
     .                cy1//'_'//cy2//'.csv'
          call writematlab(matlabfn,x,nvals,maxvals)
          if (nvals.gt.2) then
            write(14,*) 'short = csvread('//char(39)//'../matlab_data/'
     .                   //fnam(:lenbasin)//
     .                 '_annual_'//cy1//'_'//cy2//'.csv'//char(39)//');'
            write(14,*) '[h(',year1-1984,',',year2-1984,',2),p(',
     .                        year1-1984,',',year2-1984,',2)]',
     .                  ' = kstest2(annlong,short);'
            write(14,*) 'cdfplot(annlong)'
            write(14,*) 'hold on'
            write(14,*) 'cdfplot(short,'//char(39)//'r'//char(39)//')'
            write(14,*) 'legend('//char(39)//'long term'//char(39)//','
     .                  //char(39)//'short term'//char(39)//')'
            write(14,*) 'title('//char(39)//fnam(:lenbasin)//' annual '
     .                  //cy1//'-'//cy2//char(39)//')'
            write(14,*) 'xlabel('//char(39)//'flow'//char(39)//')'
            write(14,*) 'ylabel('//char(39)//'frequency'//char(39)//')'
            write(14,*) 'print -dpsc ../matlab_ps_files/'//
     .                   fnam(:lenbasin)//'_annual_'//
     .                   cy1//'_'//cy2
            write(14,*) 'hold off'

          end if

************ calculate annual KS stats
          call KStest(
     I                x,annlongx,maxvals,nvals,annlongvals,
     O                annksK)

          write(12,1234)duration,year1,year2,mean,var,logvar,annmean,
     .                  annvar,ksK,logksK,annksK

        end do ! over periods
      end do ! over durations

      write(14,*) 'csvwrite('//char(39)//'../matlab_matrices_out/'//
     .             fnam(:lenbasin)//
     .            '_H_daily.csv'//char(39)//',h(:,:,1))'
      write(14,*) 'csvwrite('//char(39)//'../matlab_matrices_out/'//
     .             fnam(:lenbasin)//
     .            '_P_daily.csv'//char(39)//',p(:,:,1))'
      write(14,*) 'csvwrite('//char(39)//'../matlab_matrices_out/'//
     .             fnam(:lenbasin)//
     .            '_H_annual.csv'//char(39)//',h(:,:,2))'
      write(14,*) 'csvwrite('//char(39)//'../matlab_matrices_out/'//
     .             fnam(:lenbasin)//
     .            '_P_annual.csv'//char(39)//',p(:,:,2))'
      write(14,*) 'csvwrite('//char(39)//'../matlab_matrices_out/'//
     .             fnam(:lenbasin)//
     .            '_H_log.csv'//char(39)//',h(:,:,3))'
      write(14,*) 'csvwrite('//char(39)//'../matlab_matrices_out/'//
     .             fnam(:lenbasin)//
     .            '_P_log.csv'//char(39)//',p(:,:,3))'
      write(14,*) 'clear all'
      close(14)
      stop
1234  format(i3,2(',',i4),8(',',e14.7))
************************ ERROR SPACE
991   report(1) = 'Problem opening data file, '//fnam
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

992   report(1) = 'Problem reading data file, '//fnam
      report(2) = ' near line'
      report(3) = USGS//' '//date//' '//flag
      go to 999

993   report(1) = 'could not calculate average'
      report(2) = 'divide by zero'
      report(3) = ' '
      go to 999

994   report(1) = 'could not calculate variance'
      report(2) = 'divide by zero'
      report(3) = ' '
      go to 999

995   report(1) = 'Problem opening data file, '//matlabfn
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end 
