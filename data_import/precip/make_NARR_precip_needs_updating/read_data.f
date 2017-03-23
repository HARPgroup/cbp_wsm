************************************************************************
**  reads from USGS files to get hourly data                          **
************************************************************************
      subroutine readUSGS(
     I                    lseg,
     O                    nvals,hval,dval,sdate,edate)

      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/wdm.inc'

      integer i,sdate(ndate),edate(ndate)
      integer nday,nhour,nvals2
      integer time(ndaymax*24,4)  ! year,month,day,hour
      integer ny,oldyear

      real Tdval
      double precision dacc

************* open datafile 
      fnam = '/model/p5_big_dataset_storage/xyz_lauren_hay_model/'//
     .       'xyz_2006_11_29/OUTPUT/FIPSAB/'//lseg(:6)//'.PRC'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

*********** read in hourly USGS data to local variables
      nvals = 0
      do
        read(dfile,'(a100)',end=222)line
        call d2x(line,last)
        nvals = nvals + 1
        read(line,*) (time(nvals,i),i=1,4),hval(nvals)
      end do

222   close (dfile)

************ check the data for completeness
      tcode = 3    ! hourly data

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
      nvals2 = nvals2 + 1     ! add start date value
      if (nvals2.ne.nvals) go to 992

********** check the data for negative values
      do i = 1,nvals
        if (hval(i).lt.-0.001) go to 993
      end do

********* check for reasonable values of rainfall
      oldyear = time(1,1)
      dacc = 0.0
      do i = 1,nvals
        if (oldyear.ne.time(i,1)) then
          if (dacc.lt.20.or.dacc.gt.78) go to 994
            oldyear = time(i,1)
            dacc = 0.0
          end if
        dacc = dacc + hval(i)
      end do

********* get daily USGS rainfall
      nhour = 0
      nday  = 0
      Tdval = 0.0

      do i = 1,nvals
        nhour = nhour + 1 
        Tdval = Tdval + hval(i)
        if (nhour.eq.24) then
          nday = nday + 1
          dval(nday) = Tdval
          nhour  = 0
          Tdval = 0.
        end if
      end do

      return

********************************* ERROR SPACE **************************
991   print*,fnam
      report(1) = 'could not open file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'number of values found does not match start and end'
      report(2) = 'dates.  Check for completeness'
      write(report(3),*) 'nvals = ',nvals,' nvals2 = ',nvals2
      go to 999

993   report(1) = 'there should be no negative values in this dataset'
      report(2) = ' '
      report(3) = ' '
      go to 999

994   report(1) = 'rainfall out of bounds'
      report(2) = ' '
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end


************************************************************************
**  reads in daily NARR data                                          **
************************************************************************
      subroutine readNARR(
     I                    lseg,
     O                    ndays,Ndval)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/wdm.inc'

      character*6 Tlseg
      character*10 Tdate
      integer FIPS2

      integer i,sdate(ndate),edate(ndate),tstep
      integer time(ndaymax*24,4)  ! year,month,day,hour
      integer ny,oldyear
      integer ndays,ndays2

      real Ndval(ndaymax),Tvalue
      real dacc

********* read in daily NARR data
      fnam = '/model/p5_big_dataset_storage/atdep/grimm/model'
     .       //'/areastats/NARR_daily.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

********* read in the data to local variables
      ndays = 0
      read(dfile,'(a100)',end=333)line   ! read header line
      
      do
        read(dfile,'(a100)',end=333)line
        call d2x(line,last)
        read(line,*) FIPS2,Tlseg,Tdate,Tvalue
     
        if (Tlseg(:6).eq.lseg(:6)) then         ! find the land segment
          ndays = ndays + 1
          read(Tdate(1:4),*)  time(ndays,1)
          read(Tdate(5:6),*)  time(ndays,2)
          read(Tdate(7:8),*)  time(ndays,3)
          read(Tdate(9:10),*) time(ndays,4)
          Ndval(ndays) = Tvalue
        end if

      end do
333   close (dfile)
      
************ check the data for completeness
      tcode = 4
      do i = 1,3                       ! populate sdate and edate
        sdate(i) = time(1,i)
        edate(i) = time(ndays,i)
      end do
      sdate(4) = 0
      edate(4) = 24
      do i = 5,6
        sdate(i) = 0
        edate(i) = 0
      end do

      call timdif(sdate,edate,tcode,1,ndays2)
      ndays2 = ndays2 + 1     ! add start date value
      if (ndays2.ne.ndays) go to 992

********** check the data for negative values
      do i = 1,ndays
        if (Ndval(i).lt.-0.001) go to 993
      end do

********* check for reasonable values of rainfall
      oldyear = time(1,1)
      dacc = 0.0
      do i = 1,ndays
        if (oldyear.ne.time(i,1)) then
          print*,oldyear,' ',dacc
          if (dacc.lt.15 .or. dacc.gt.78) go to 994
          oldyear = time(i,1)
          dacc = 0.0
        end if
        dacc = dacc + Ndval(i)
      end do

      return

********************************* ERROR SPACE **************************
991   print*,fnam
      report(1) = 'could not open file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'number of values found does not match start and end'
      report(2) = 'dates.  Check for completeness'
      write(report(3),*) 'ndays = ',ndays,' ndays2 = ',ndays2
      go to 999

993   report(1) = 'there should be no negative values in this dataset'
      report(2) = ' '
      report(3) = ' '
      go to 999

994   report(1) = 'rainfall out of bounds'
      report(2) = ' '
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end



