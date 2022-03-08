************************************************************************
**  reads data from a file and organizes the output                   **
************************************************************************
      subroutine readstdin(hvals,dvals,ndsns,dsns,tcode,sdate,edate)
      include 'qstd.inc'

      integer firstchar,lastchar ! first and last character column numbers
      integer year,month,day,hour,alltime,nvals2
      real value(maxdsns)

      integer time(ndaymax*24,4)  ! year,month,day,hour

      integer n

      tcode = 4    ! daily data

*********** find number of dsns and data set numbers
      read(dfile,'(a100)')line
      call d2x(line,last)
      if (line(:3).eq.'198') backspace dfile
      line = '2 3007 3008'
      read(line,*) ndsns
      if (ndsns.gt.maxdsns) go to 993
      read(line,*) ndsns,(dsns(i),i=1,ndsns)

***********   ********* read in the data to local variables
      nvals = 0
      do
        read(dfile,'(a100)',end=111)line
        call d2x(line,last)
        read(line,*) alltime,(value(i),i=1,ndsns)
        nvals = nvals + 1
        time(nvals,1) = alltime/10000
        time(nvals,2) = (alltime-time(nvals,1)*10000)/100
        time(nvals,3) = alltime-time(nvals,1)*10000-time(nvals,2)*100
        do i = 1,ndsns
          dvals(nvals,i) = value(i)
        end do
      end do

111   continue

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
      if (nvals2.ne.nvals) go to 992
 
*****************  data only went from 1985 through 2000
*************** use 1985 for 1984 and 2000 for 2001 and 2002
      if (sdate(1).ne.1985.or.sdate(2).ne.1.or.sdate(3).ne.1.or.
     .   edate(1).ne.2000.or.edate(2).ne.12.or.edate(3).ne.31) go to 994
      sdate(1) = 1984
      edate(1) = 2002

      do n = nvals,1,-1  ! move all dvals 366 spaces for 1984
        do i = 1,ndsns
          dvals(n+366,i) = dvals(n,i)
        end do
      end do
      do n = 366,60,-1  ! days 1-59 are fine
        do i = 1,ndsns
          dvals(n,i) = dvals(n-1,i)
        end do
      end do
      nvals = nvals+366

      do n = nvals-364,nvals   ! add 2001 and 2002, all 3 years have 365 days
        do i = 1,ndsns
          dvals(n+365,i) = dvals(n,i)
          dvals(n+365+365,i) = dvals(n,i)
        end do
      end do
      nvals = nvals+365+365

      return

********************************* ERROR SPACE **************************

992   report(1) = 'number of values found does not match start and end'
      report(2) = 'dates.  Check for completeness'
      write(report(3),*) 'nvals = ',nvals,' nvals2 = ',nvals2
      go to 999

993   report(1) = 'number of dsns in data file'
      write(report(2),*) ' greater than maximum allowable = ',maxdsns
      report(3) = 'modify ./pp/lib/cbw/src/create_standard/main.f'
      go to 999

994   report(1) = 'dates should be 1/1/1985 - 12/31/2000'
      write(report(2),*)'start date ',(sdate(i),i=1,3)
      write(report(3),*)'end date ',(edate(i),i=1,3)

999   call stopreport(report)


      end

