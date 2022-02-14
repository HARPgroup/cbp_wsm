************************************************************************
**  calls the lib3.2 subroutine wdtget which gets a time series from  **
**    a wdm.  This is necessary because the array must be dimensioned **
**    before the call, but after the number of values is determined   **
************************************************************************
      subroutine callwdtget(wdmfil,dsn,sdate,nvals,dtran,tcode,
     O                      hval)
      implicit none

      include '../inc/standard.inc'
      include '../inc/wdm.inc'
      integer wdmfil
      integer i,sdate(ndate)

      real val(ndaymax*24)

      call wdtget(wdmfil,dsn,1,sdate,nvals,dtran,0,tcode,val,err)

      if (err .ne. 0) go to 991

      do i = 1,nvals
        hval(i) = val(i)
      end do

      return

************* ERROR SPACE **********************************************

991   report(1) = 'Problem retrieving data for data set :'
      report(2) = ' '
      write(report(2),'(i6)')dsn
      if (err.eq.-8) then
        write(report(3),*)'Invalid Date'
      else if (err.eq.-20) then
        write(report(3),*)
     .  'Problem with GPFLG, DXX, NVAL, QUALVL, LTSTEP, LTUNIT'
      else if (err.eq.-21) then
        write(report(3),*)'date from WDM does not match expected date'
      else if (err.eq.-81) then
        write(report(3),*)'data set ',dsn,' does not exist'
      else if (err.eq.-82) then
        write(report(3),*)'data set exists, but is wrong DSTYP'
      else if (err.eq.-84) then
        write(report(3),*)'data set number out of range'
      else
        write(report(3),*)
     .     'check lib3.2/src/wdm/wdtms1.f, error code',err
      end if
      go to 999

999   call stopreport(report)

      end



************************************************************************
**  calls the lib3.2 subroutine wdtput which puts a time series to    **
**    a wdm.  This is necessary because the array must be dimensioned **
**    before the call, but after the number of values is determined   **
************************************************************************
      subroutine callwdtput(wdmfil,dsn,sdate,nvals,dtran,tcode,
     O                      hval)
      implicit none

      include '../inc/standard.inc'
      include '../inc/wdm.inc'
      integer wdmfil
      integer i,sdate(ndate)

      real val(ndaymax*24)
      
      do i = 1,nvals
        val(i) = hval(i)
      end do

      call wdtput(wdmfil,dsn,1,sdate,nvals,1,0,tcode,val,err)

      if (err .ne. 0) go to 991

      return

************* ERROR SPACE **********************************************
991   write(report(1),*) 'Problem writing data for data set ',dsn
      if (err.eq.-8) then
        report(2) = 'Invalid Date'
      else if (err.eq.-20) then
        report(2) = 'Problem with DTOVWR, NVAL, QUALFG, TUNITS, DELT'
      else if (err.eq.-21) then
        report(2) = 'date from WDM does not match expected date'
      else if (err.eq.-81) then
        write(report(2),*) 'data set ',dsn,' does not exist'
      else if (err.eq.-82) then
        report(2) = 'data set exists, but is wrong DSTYP'
      else if (err.eq.-84) then
        report(2) = 'data set number out of range'
      else if (err.eq.-85) then
        report(2) = 'Can not write to read only data set'
      else
        write(report(2),*)
     .        'error code ',err,' check lib3.2/src/wdm/wdtms1.f'
      end if
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end



      subroutine getdailydsn(wdmfil,sdate,edate,dsn,
     O                       nvals,dval)
      implicit none
!
!   Based on code written in F90 by J. Thorpe of CCWR 10/02/2000
!   Dumbed down to f77 by G. Shenk of US EPA on 11/30/2000
!   modified by G. Shenk to be useful as a subroutine
! 
!  exports datasets from a wdm file to a variable
!
!  dataset(n) (I5) - inddividual data sets to export. 1 to number of data sets
!  start date (I4,5I2) - yyyymmddhhmmss start date of export
!  end date (I4,5I2) - yyyymmddhhmmss end date of export
!  tcode (I1) - time step of export (1=sec,2=min,3=hour,4=day,5=mon,6=year)  
!  data transformation code (I1) - 0=ave/same 1=sum/div 2=max 3=min
!  output file path (A64) - name of file to write export to
!

      include '../inc/standard.inc'
      include '../inc/wdm.inc'

      integer wdmfil

      integer sdate(ndate),edate(ndate),asdate(ndate),aedate(ndate)
      integer i

!------------- function definitions
      integer timchk
      external timchk

      tcode = 4    ! for days
! timdif gets the number of tcode intervals between sdate and edate
      call timdif(sdate,edate,tcode,1,nvals)
! make interval inclusive of enddate

      dtran = 0

! find start and end date in data set
      call wtdate(wdmfil,1,dsn,2,asdate,aedate,err)
      if (err .ne. 0) go to 991

! check to see if dates are ok
      if (timchk(sdate,asdate).eq.1) go to 992
      if (timchk(edate,aedate).eq.-1) go to 993

      call callwdtget(wdmfil,dsn,sdate,nvals,dtran,tcode,hval)

!  daily values stored in large-dimension variable, convert back
      do i = 1,nvals
        dval(i) = hval(i)
      end do

      return

************* ERROR SPACE **********************************************

991   write(report(1),*) "error getting start and end of existing data"
      write(report(2),*) "error code=",err
      write(report(3),*) 'dsn = ',dsn
      go to 999

992   write(report(1),*)
     .          'requested start date earlier than DSN start date ',dsn
      write(report(2),*)'requested date ',sdate
      write(report(3),*)'DSN start date ',asdate
      go to 999
        
993   write(report(1),*)
     .          'requested end date later than DSN end date ',dsn
      write(report(2),*)'requested date ',edate
      write(report(3),*)'DSN endA date ',aedate
      go to 999

999   call stopreport(report)

      end





      subroutine gethourdsn(wdmfil,sdate,edate,dsn,
     O                      nvals,hval)
      implicit none
!
!   Based on code written in F90 by J. Thorpe of CCWR 10/02/2000
!   Dumbed down to f77 by G. Shenk of US EPA on 11/30/2000
!   modified by G. Shenk to be useful as a subroutine
! 
!  exports datasets from a wdm file to a variable
!
!  wdm path name (A64) - full path of existing wdm file from which to export
!  no. of data sets (I5) - integer number of data sets to export
!  dataset(n) (I5) - inddividual data sets to export. 1 to number of data sets
!  start date (I4,5I2) - yyyymmddhhmmss start date of export
!  end date (I4,5I2) - yyyymmddhhmmss end date of export
!  tcode (I1) - time step of export (1=sec,2=min,3=hour,4=day,5=mon,6=year)  
!  data transformation code (I1) - 0=ave/same 1=sum/div 2=max 3=min
!  output file path (A64) - name of file to write export to
!

      include '../inc/standard.inc'
      include '../inc/wdm.inc'

      integer wdmfil

      integer sdate(ndate),edate(ndate),asdate(ndate),aedate(ndate)
      integer i

!------------- function definitions
      integer timchk
      external timchk

      tcode = 3    ! for hours
! timdif gets the number of tcode intervals between sdate and edate
      call timdif(sdate,edate,tcode,1,nvals)
! make interval inclusive of enddate

      dtran = 0

! find start and end date in data set
      call wtdate(wdmfil,1,dsn,2,asdate,aedate,err)
      !print*,'BHATT ',asdate,' # ',aedate,' # ',err
      if (err .ne. 0) go to 991

! check to see if dates are ok
      if (timchk(sdate,asdate).eq.1) go to 992
      if (timchk(edate,aedate).eq.-1) go to 993

      call callwdtget(wdmfil,dsn,sdate,nvals,dtran,tcode,hval)

      return

************* ERROR SPACE **********************************************
991   write(report(1),*)'error getting start and end of existing data'
      write(report(2),*)'error code=',err
      write(report(3),*)'dsn = ',dsn
      go to 999

992   write(report(1),*)
     .      'requested start date earlier than DSN start date ',dsn
      write(report(2),*)'requested date ',sdate
      write(report(3),*)'DSN start date ',asdate
      go to 999

993   write(report(1),*)
     .      'requested end date later than DSN end date ',dsn
      write(report(2),*)'requested date ',edate
      write(report(3),*)'DSN endB date ',aedate
      go to 999

999   call stopreport(report)

      end





      subroutine putdailydsn(wdmfil,sdate,edate,dsn,nvals,dval)
      implicit none
!
!   Based on code written in F90 by J. Thorpe of CCWR 10/02/2000
!   Dumbed down to f77 by G. Shenk of US EPA on 11/30/2000
!   modified by G. Shenk to be useful as a subroutine
! 
!  exports datasets from a wdm file to a variable
!
!  wdm path name (A64) - full path of existing wdm file from which to export
!  no. of data sets (I5) - integer number of data sets to export
!  dataset(n) (I5) - inddividual data sets to export. 1 to number of data sets
!  start date (I4,5I2) - yyyymmddhhmmss start date of export
!  end date (I4,5I2) - yyyymmddhhmmss end date of export
!  tcode (I1) - time step of export (1=sec,2=min,3=hour,4=day,5=mon,6=year)  
!  data transformation code (I1) - 0=ave/same 1=sum/div 2=max 3=min
!  output file path (A64) - name of file to write export to
!

      include '../inc/standard.inc'
      include '../inc/wdm.inc'

      integer wdmfil

      integer sdate(ndate),edate(ndate),asdate(ndate),aedate(ndate)
      integer i

!------------- function definitions
      integer timchk
      external timchk

      tcode = 4    ! for days
! timdif gets the number of tcode intervals between sdate and edate
      call timdif(sdate,edate,tcode,1,nvals)
! make interval inclusive of enddate

      dtran = 0

!  store daily values in large-dimension variable
      do i = 1,nvals
        hval(i) = dval(i)
      end do


      call callwdtput(wdmfil,dsn,sdate,nvals,dtran,tcode,hval)

!  daily values stored in large-dimension variable, convert back
      do i = 1,nvals
        dval(i) = hval(i)
      end do

      end





      subroutine puthourdsn(wdmfil,sdate,edate,dsn,nvals,hval)
      implicit none
!
!   Based on code written in F90 by J. Thorpe of CCWR 10/02/2000
!   Dumbed down to f77 by G. Shenk of US EPA on 11/30/2000
!   modified by G. Shenk to be useful as a subroutine
! 
!  exports datasets from a wdm file to a variable
!
!  wdm path name (A64) - full path of existing wdm file from which to export
!  no. of data sets (I5) - integer number of data sets to export
!  dataset(n) (I5) - inddividual data sets to export. 1 to number of data sets
!  start date (I4,5I2) - yyyymmddhhmmss start date of export
!  end date (I4,5I2) - yyyymmddhhmmss end date of export
!  tcode (I1) - time step of export (1=sec,2=min,3=hour,4=day,5=mon,6=year)  
!  data transformation code (I1) - 0=ave/same 1=sum/div 2=max 3=min
!  output file path (A64) - name of file to write export to
!

      include '../inc/standard.inc'
      include '../inc/wdm.inc'

      integer wdmfil

      integer sdate(ndate),edate(ndate),asdate(ndate),aedate(ndate)
      integer i

!------------- function definitions
      integer timchk
      external timchk

      tcode = 3    ! for hours
! timdif gets the number of tcode intervals between sdate and edate
      call timdif(sdate,edate,tcode,1,nvals)
! make interval inclusive of enddate

      dtran = 0

      call callwdtput(wdmfil,dsn,sdate,nvals,dtran,tcode,hval)

      end

************************************************************************
** subroutine to translate the time code for output to the screen     **
************************************************************************
      subroutine transtcode(tcode,tunits)
      implicit none
      integer tcode
      character*(*) tunits
      if (tcode.eq.1) tunits = 'second'
      if (tcode.eq.2) tunits = 'minute'
      if (tcode.eq.3) tunits = 'hour'
      if (tcode.eq.4) tunits = 'day'
      if (tcode.eq.5) tunits = 'month'
      if (tcode.eq.6) tunits = 'year'
      end

************************************************************************
**  subroutine to report error based on error number                  **
************************************************************************
      subroutine wdmerr(err,
     O                  errdescribe)
      implicit none
      integer err
      character*(*) errdescribe

      if (err.eq.  -84) then
        errdescribe = 'data set number out of range'
      else if (err.eq.-85) then
        errdescribe = 'trying to write to a read-only data set'
      else if (err.eq.-82) then
        errdescribe = 'data set exists, but is wrong DSTYP'
      else if (err.eq.-81) then
        errdescribe = 'data set does not exist'
      else if (err.eq.-8) then
        errdescribe = 'invalid date'
      else if (err.eq.-20) then
        errdescribe = 'Problem with DTOVWR, NVAL, QUALFG, TUNITS, DELT'
      else if (err.eq.-21) then
        errdescribe = 'date from WDM does not match expected date'
      else
        write(errdescribe,*) 'error = ',err
      end if
      end
