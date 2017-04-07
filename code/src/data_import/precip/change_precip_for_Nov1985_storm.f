      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'

      integer sdate(ndate), edate(ndate), tdate(ndate)  ! start and end dates in wdm format
      integer wdmfil
      parameter (wdmfil=12)         ! file number for wdm

      integer year,day,month,hour,oldyear
      double precision dacc

      integer i,j

      real critrain,daily
      parameter (critrain=1.) ! inches per day
      real mult,factor
      parameter (mult=2.5)  ! multiplier for rain above critrain

      read*,lseg,sdate(1),edate(1)  ! get segment and dates

      dsn = 2000

C      sdate(1) = 1984
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0
C      edate(1) = 2000
      edate(2) = 12
      edate(3) = 31 
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      do i = 1,6
        tdate(i) = sdate(i)
      end do

      wdmfnam = 'prad_'//lseg//'.wdm'
      call wdbopn(wdmfil,wdmfnam,0,err)     ! open main wdm read/write
      call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)
      
      do i = 1,nvals-24,24
        if (tdate(1).eq.1985) then
          if (tdate(2).eq.11) then
            if (tdate(3).le.4) then
              daily = 0
              do j = 1,24
                daily = daily + hval(i+j-1)
              end do
              if (daily.gt.critrain) then
                factor = ((daily-critrain)*mult+critrain)/daily
                do j = 1,24
                  hval(i+j-1) = hval(i+j-1) * factor
                end do
              end if
            end if
          end if
        end if
        call tomorrow(tdate(1),tdate(2),tdate(3))
        if (tdate(1).eq.1986) exit
      end do

********* check for reasonable values of rainfall
      print*,' '
      print*,lseg
      year = sdate(1)
      month = 1
      day = 1
      hour = 1
      dacc = 0.0
      oldyear = year
      do i = 1,nvals
        if (oldyear.ne.year) then
          print*,lseg,',',oldyear,',',dacc
          oldyear = year
          dacc = 0.0
        end if
        dacc = dacc + hval(i)
        call onehour(year,day,month,hour)
      end do

      call puthourdsn(wdmfil,sdate,edate,dsn,nvals,hval)

      call wdflc1(wdmfil,err)

      end

 
