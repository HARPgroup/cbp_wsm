      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
c      include '../../lib/inc/wdm.inc'

      integer   NDAYSMAX
      parameter (NDAYSMAX=16836) !(2025-1980+1)*366
      real      hval(NDAYSMAX*24)
      character*200 wdmfnam,msgfname
      integer ndate
      parameter (ndate = 6)
      integer dsn,tcode,nvals
      integer TSTEP, dtran, qualfg, dtovwr
      parameter (TSTEP=1, dtran=0, qualfg=0, dtovwr=1)
      integer HCODE, DCODE ! Hourly, Daily
      parameter (HCODE=3, DCODE=4)
      integer retcod

      integer sdate(ndate), edate(ndate), tdate(ndate)  ! start and end dates in wdm format
      integer wdmfil,msgfile
      parameter (wdmfil=12,msgfile=9)         ! file number for wdm

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

      msgfname= './message.wdm'
      call wdbopn(msgfile,msgfname,1,retcod)  ! open msgfile read only
      if (retcod.ne.0) stop 'ERROR opening message wdm'

      wdmfnam = 'prad_'//lseg//'.wdm'
      call wdbopn(wdmfil,wdmfnam,0,retcod)     ! open main wdm read/write
      if (retcod.ne.0) then
             print*, 'retcod = ', retcod
             stop 'ERROR opening wdm'
      end if
c      call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)
      call timdif(
     I            sdate,edate,HCODE,TSTEP,
     O            nvals)
      call wdtget(
     I            wdmfil,dsn,TSTEP,sdate,nvals,
     I            dtran, qualfg, HCODE,
     O            hval, retcod)
      
      print*,retcod
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

c      call puthourdsn(wdmfil,sdate,edate,dsn,nvals,hval)
      call wdtput(
     I            wdmfil,dsn,TSTEP,sdate,nvals,
     I            dtovwr, qualfg, HCODE,hval,
     O            retcod)

c      call wdflc1(wdmfil,err)
      if (retcod.ne.0) then
            print*, 'retcod = ', retcod
            stop 'ERROR writing wdm'
      end if
************write back to the wdm file**********************************
      call wdflcl(
     I            wdmfil,
     O            retcod)
      if (retcod.ne.0) stop 'ERROR closing wdm'

      end

 
