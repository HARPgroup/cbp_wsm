************************************************************************
**  reads from a file and writes a new wdm with similar name          **
************************************************************************
      include 'qstd.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/rsegs.inc'

      real supply,irrigation  ! supply and ag diversions

      character*25 DivScenSrc,DivScenDest,OneSeg
      integer lenDivScenSrc,lenDivScenDest,lenOneSeg

      integer year1,year2,ny,nm,nr
      character*4 YearC

      real average
      integer ndaysinmonth
      external average,ndaysinmonth

      character*200 fnam7,fnam8

******************** set variables
      data ndsns,dsns(1),dsns(2) /2,3007,3008/
      data sdate /0,1,1,0,0,0/
      data edate /0,12,31,24,0,0/

************* get and process input
      read*,DivScenSrc,DivScenDest,OneSeg,year1,year2
      ! print*,DivScenSrc,DivScenDest,OneSeg,year1,year2,tree

      sdate(1) = year1
      edate(1) = year1

      call lencl(DivScenSrc,lenDivScenSrc)
      call lencl(DivScenDest,lenDivScenDest)
      call lencl(OneSeg,lenOneSeg)

************* open writing files and write header
      write(YearC,"(I4)") year2
      ! print*,'>',YearC,'<'
      fnam8 = tree//'input/unformatted/diversions/'
     .       //DivScenDest(:lenDivScenDest)//
     .  '/Irrigation3008/'//YearC//'/'//OneSeg(:lenOneSeg)//'.csv'
      ! print*,fnam8

      open(dfile+8,file=fnam8,status='unknown',iostat=err)
      if (err.ne.0) go to 992

      fnam7 = tree//'input/unformatted/diversions/'
     .       //DivScenDest(:lenDivScenDest)//
     .  '/Supply3007/'//YearC//'/'//OneSeg(:lenOneSeg)//'.csv'
      ! print*,fnam7
      open(dfile+7,file=fnam7,status='unknown',iostat=err)
      if (err.ne.0) go to 992

      write(dfile+7,*,err=951) 'rseg,year,month,supply_cfs'
      write(dfile+8,*,err=951) 'rseg,year,month,irrigation_cfs'

********* open the dummy wdm
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+1,wdmfnam,0,err)
      if (err.ne.0) go to 991


********* loop over segments; open, read, close wdm; write flows
CBHATT      do nr = 1,nrsegs

CBHATT        print*,rsegs(nr)
        !print*,OneSeg(:lenOneSeg)

        wdmfnam = ScenDatDir//'river/div/'//DivScenSrc(:lenDivScenSrc)//
     .            '/DIV_'//OneSeg(:lenOneSeg)//'.wdm'
        call wdbopnlong(wdmfil,wdmfnam,0,err)
        if (err.ne.0) go to 991

************* loop over all months, read wdm, average, check, and write
        do ny = year1,year1
          do nm = 1,12
            sdate(1) = ny
            sdate(2) = nm
            edate(1) = ny
            edate(2) = nm
            edate(3) = ndaysinmonth(ny,nm)

            call getdailydsn(wdmfil,sdate,edate,dsns(1),
     .                       nvals,dval)
            if (nvals.ne.ndaysinmonth(ny,nm)) go to 993
            supply = average(dval,nvals,ndaymax,err)
            if (err.ne.0) go to 994
            write(dfile+7,*,err=951) OneSeg(:lenOneSeg),',',year2,','
     .             ,nm,',',supply
            
            call getdailydsn(wdmfil,sdate,edate,dsns(2),
     .                       nvals,dval)
            if (nvals.ne.ndaysinmonth(ny,nm)) go to 993
            irrigation = average(dval,nvals,ndaymax,err)
            if (err.ne.0) go to 994
            if (err.ne.0) go to 994
            write(dfile+8,*,err=951) 
     .           OneSeg(:lenOneSeg),',',year2,',',nm,',',irrigation
          end do
        end do

        call wdflcl(wdmfil,err)
        if (err.ne.0) go to 995

CBHATT      end do  ! loop over segments   

      close(dfile+7)
      close(dfile+8)
      !print*, 'finished writing files '
      !print*,fnam7
      !print*,fnam8

      stop

********************************* ERROR SPACE **********************************

951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'WDM file must previously exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
      go to 999

992   report(1) = 'could not open file'
      report(2) = fnam
      report(3) = ' '
      go to 999

993   report(1) = 'number of days in month not correct'
      report(2) = wdmfnam
      write(report(3),*) ny,' ',nm,' ',nvals
      go to 999

994   report(1) = 'problem with average'
      report(2) = wdmfnam
      write(report(3),*) ny,' ',nm
      go to 999

995   report(1) = 'Error: closing wdm = '
      write(report(1)(28:30),'(i3)')err
      report(2) = ' wdm name =  '
      report(3) = wdmfnam
      go to 999

999   call stopreport(report)

      end

