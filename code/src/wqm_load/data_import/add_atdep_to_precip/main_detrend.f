      implicit none

      include 'detrend.inc'
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'

      integer dyear !detrend year

      integer sdate(ndate),edate(ndate)

      data sdate /SWDMYEAR, 1, 1, 0,0,0/
      data edate /EWDMYEAR,12,31,24,0,0/

      integer iparam
      integer NPARAM
      parameter ( NPARAM = 4 )
      
      character*4 param(NPARAM)
      data param /'NO23','NO3D','NH4A','NH4D'/

      integer dsns(NPARAM)
      data dsns /2001,2003,2002,2004/

      integer doparam(NPARAM)
      data doparam /1,1,1,1/

      real trendfac(SWDMYEAR:EWDMYEAR)

      integer  ndaysinyear
      external ndaysinyear

      integer idays,ndays
      integer idd,iyr

      integer wdmfil
      parameter (wdmfil=dfile+10)

      read*,lseg,dyear

************* Dummy WDM
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+1,wdmfnam,0,err)
      if (err.ne.0) go to 991

************* open wdm and read rainfall and loads
      wdmfnam = 'prad_'//lseg//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 991 

      do iparam = 1,NPARAM

         print*,lseg//' '//param(iparam)
         if ( doparam(iparam) .eq. 0 ) cycle
         print*,'... processing'

         call gettrendfactor(lseg,dyear,param(iparam),trendfac)

         call getdailydsn(wdmfil,sdate,edate,dsns(iparam),ndays,dval)

         idays = 1
         do iyr = sdate(1),edate(1)
            print*,'...',iyr,trendfac(iyr)
            do idd = 1,ndaysinyear(iyr)
               dval(idays) = dval(idays) * trendfac(iyr)
               idays = idays + 1
            end do
         end do

         call putdailydsn(wdmfil,sdate,edate,dsns(iparam),ndays,dval)

      end do

      return


991   report(1) = 'WDM file must previously exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
      go to 999


999   call stopreport(report)

      end











      subroutine gettrendfactor(
     I            lseg,dyear,param,
     O            dfactors)

      implicit none

      include 'detrend.inc'
      include '../../lib/inc/standard.inc'

      integer syear, eyear
      integer dyear
      character*4 param
      integer years(MAXCOLS)
      real dfactors(SWDMYEAR:EWDMYEAR)

      real tdata(SMAXYEAR:EMAXYEAR)

      character*6 tlseg
      integer lentlseg

      integer nyr,iyr
      character*2000 longline

      logical found

      fnam = '../../../input/unformatted/atdep/WSMDATA/'//
     .       'detrend/PRC20170731'//'_'//param//'_ann.csv'
      if (DEBUG) print*,'0->',fnam

      open(dfile+2,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile+2,'(a2000)',err=992)longline
      if (DEBUG) print*,'1->',longline
      call d2x(longline,last)
      if (DEBUG) print*,'2->',longline
c      print*,last

      nyr = 0
      do
         if ( nyr .eq. 0 ) then
           read(longline,*) tlseg
           call shift(longline)
           if (DEBUG) print*,'3->',nyr,longline
           if (DEBUG) print*,nyr,tlseg
         else
           read(longline,*,err=992,end=101) years(nyr)
           call shift(longline)
           if (DEBUG) print*,'4->',years(nyr)
         end if
         nyr = nyr + 1
      end do

101   if (DEBUG) print*,'num of year cols = ',nyr

      found = .false.
      do
         read(dfile+2,'(a2000)',end=102)longline
c         print*,longline
         call d2x(longline,last)

         read(longline,*) tlseg
         call shift(longline)

         call lencl(tlseg,lentlseg)
         if ( tlseg(:lentlseg)  .eq. lseg ) then
            if (DEBUG) print*,'lseg found'
            found = .true.
            exit
         end if
      end do

102   close(dfile+2)

      if (.not.found) goto 993

      do iyr = 1,nyr-1
         read(longline,*) tdata(years(iyr))
         call shift(longline)
         if (DEBUG) print*,'data ',iyr,years(iyr),tdata(years(iyr))
      end do

      do iyr = SWDMYEAR,EWDMYEAR
         dfactors(iyr) = tdata(dyear)/tdata(iyr)
         if (DEBUG) print*,'factors ',iyr,dfactors(iyr)
      end do

      return

991   report(1) = 'Problem with opening detrend factor file'
      report(2) = fnam
      write(report(3),*) err
      goto 999

992   report(1) = 'Problem reading file'
      report(2) = fnam
      report(3) = ' '
      goto 999

993   report(1) = 'Lseg not found in file'
      report(2) = lseg
      report(3) = fnam
      goto 999


999   call stopreport(report)

      end
