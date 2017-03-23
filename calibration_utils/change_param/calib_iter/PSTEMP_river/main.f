************************************************************************
** program to change parameters for calibration.  Automatically reads **
**   csv files and performs functions on the variables                **
************************************************************************
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include 'tempcal.inc'

      integer ns

      integer nlinemax,nlines  ! number of parameter lines in code
      parameter (nlinemax=1000)
      character*2000 parline(nlinemax),
     .                       varline,tableline,
     .                       vkeep,tkeep

      character*2000 segline

      integer vcKATRAD

      logical found

      integer i 
      integer iline

      character*2 citnum  ! iteration number from shell script

      character*4 cy1,cy2   ! years one and 2

      logical converge

      character*200 lock  ! name of lock file to prevent stale data
      integer lfile
      parameter (lfile = dfile + 1)
      logical fileexist
      integer locktry, maxlocktries  ! number of attempts at opening
      parameter (maxlocktries = 20) ! shouldn't take long

      character*203 deletecommand

*************** END DECLARATIONS ***************************************


************ SPECIFICATION ENTRY SECTION
      read*,calscen,rscen,calseg,itnum,cy1,cy2
      if (itnum.gt.maxitnum) go to 998
      call lencl(rscen,lenrscen)

      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

      module = 'HTRCH'
      lmodule = 'PSTEMP'
      call uppercase(module)
      call lencl(module,lenmod)
      call lencl(lmodule,lenlmod)
      call lencl(calscen,lencalscen)
      call lencl(rscen,lenrscen)
     
************* HOUSEKEEPING
********** get segments
      fnam = seglistdir//calscen(:lencalscen)//'_PSTEMP_'//
     .       calseg//'.riv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      read(dfile,'(a2000)') segline
      c = '('
      call shiftchar(segline,c)
      nrsegs = 0
      do while (segline(1:1).ne.')')
        nrsegs = nrsegs + 1
        read(segline,*) rsegs(nrsegs)
        call spaceshift(segline,last)
      end do

      print*,'optimizing segments'
      do i = 1,nrsegs
        print*,rsegs(i)
      end do
      print*,' run number ',itnum

************  READ IN LAND USE RATIOS
      call zipall( 
     M            limitsKATRAD)

      call getrivfacs(module,lenmod,calscen,lencalscen,
     M                limitsKATRAD)

C      call printall(   ! uncomment this to check input
C     I            limitsLGTP1,limitsASLT,limitsULTP1,BSLT,ULTP2)

**********           FIND FIRST FREE FILE NAME FOR ALL LAND USES
      write(citnum,'(i2)')itnum
      if (itnum.lt.10) citnum(1:1) = '0'

      if (itnum.ne.1) then
        call getoldstats(
     I                   lmodule,calscen,calseg,itnum,
     O                   KATRAD,efficiency)
      end if

************ READ IN current RIVER STATISTICS
      call getefficiency(
     I                   rscen,calscen,calseg,cy1,cy2,
     O                   efficiency(itnum))

********** OPEN PARAMETER FILE
********** CHECK TO SEE IF PARAMETER FILE IS ALREADY OPEN
      fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .         '/'//module(:lenmod)//'.csv'
      lock = pardir//'river/'//paramscen(:lenparamscen)//
     .         '/'//module(:lenmod)//'.csv.lock'
      do locktry = 1,maxlocktries
        inquire(file=lock,exist=fileexist)
        if (fileexist) then
          call sleep(1)
          print*,'waiting for file access ',locktry,' of ',maxlocktries
          print*,fnam
        else
          open(lfile,file=lock,status='new',iostat=err)
          write(lfile,*) 'locked'
          close(lfile)
          if (err.ne.0) go to 991
          exit
        end if
        if (locktry.eq.maxlocktries) go to 989
      end do
************ open the file, it may take a few tries
********** with many parallel operations
      print*,fnam(:75)
      do locktry = 1,maxlocktries
        open(dfile,file=fnam,status='old',iostat=err)
        if (err.eq.0) exit
        print*,'waiting for file to exist, try ',locktry
        call sleep(1)
      end do
      if (err.ne.0) go to 991

**********            READ HEADER LINES
      read(dfile,'(a2000)',err=994) tableline
      call d2x(tableline,last)
      tkeep = tableline
      read(dfile,'(a2000)',err=994) varline
      call d2x(varline,last)
      vkeep = varline

**********            READ WHOLE FILE INTO MEMORY
      nlines = 1
      read(dfile,'(a2000)',err=996,end=997)parline(nlines)
      do while (parline(nlines)(:3).ne.'end')
        nlines = nlines + 1
        if (nlines.gt.nlinemax) go to 990
        read(dfile,'(a2000)',err=996,end=997)parline(nlines)
      end do
      close(dfile)
          
      call allcolumns( 
     I                tkeep,vkeep,module,
     O                vcKATRAD)

********** GET CURRENT PARAMETER VALUE
      found = .false.
      do i = 1,nlines
        read(parline(i),*,err=992,end=992) tseg
        if (tseg.eq.rsegs(nrsegs)) then
          iline = i
          found = .true.
          exit
        end if
      end do
      if (.not.found) go to 993
      call getvar(parline(iline),vcKATRAD,KATRAD(itnum))

********** CALCULATE NEW PARAMETER VALUE
      call updateparm(
     I                  efficiency,maxitnum,itnum,limitsKATRAD,
     M                  KATRAD,
     O                  converge)

********** updated parameter in the file
      do ns = 1,nrsegs  
        found = .false.
        do i = 1,nlines
          read(parline(i),*,err=992,end=992) tseg
          if (tseg.eq.rsegs(ns)) then
            iline = i
            found = .true.
            exit
          end if
        end do
        if (.not.found) go to 995

        call putvar(parline(iline),vcKATRAD,KATRAD(itnum+1))

      end do   

**********          REWRITE MODS TO ORIGINAL FILE NAME
      fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .         '/'//module(:lenmod)//'.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      call rytdos(tkeep,dfile)
      call rytdos(vkeep,dfile)
      do i = 1,nlines
        call rytdos(parline(i),dfile)
      end do
      close(dfile)

************ DELETE LOCK FILE
      deletecommand = 'rm '//lock
      call system(deletecommand)
     
************ WRITE STATS
      call putoldstats(
     I                 lmodule,calscen,calseg,itnum,
     O                 KATRAD,efficiency)

      if (converge) then
        fnam = tree//'run/calibration/PSTEMP/'//calscen(:lencalscen)//
     .         '_'//rscen(:lenrscen)//'_'//calseg//'.converge'
        open(dfile,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(dfile,*,err=951) 'converged'
        close(dfile)
      end if

      stop

********************* ERROR SPACE **************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

989   report(1) = 'waiting to access file, but lockfile exists'
      report(2) = fnam
      report(3) = lock
      go to 999

990   report(1) = 'more lines in file than allowed in program'
      report(2) = fnam
      report(3) = ' increase variable nlinemax'
      go to 999

991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'did not find '//rsegs(ns)
      report(2) = 'in file'
      report(3) = fnam
      go to 999

993   report(1) = 'problem with '//module//'.csv file for river'
      report(2) = ' river segment '//calseg//' not found'
      report(3) = ' '
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'error occured in first two lines'
      go to 999

995   report(1) = 'problem with '//module//'.csv file'
      report(2) = 'river segment '//rsegs(ns)//' not found'
      report(3) = ' '
      go to 999

996   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = parline(nlines-1)(:100)
      go to 999

997   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = 'file ended before literal '//char(39)//'end'//
     .             char(39)//' found'
      go to 999

998   report(1) = 'maximum iteration surpassed'
      report(2) = ' increase maxitnum in tempcal include file'
      report(3) = ' ./pp/src/calibration_utils/change_param/'//
     .            'calib_iter/PSTEMP_river/tempcal.inc'
      go to 999

999   call stopreport(report)

      end
