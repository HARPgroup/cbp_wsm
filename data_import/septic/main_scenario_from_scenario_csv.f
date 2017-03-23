************************************************************************
**  reads from a file and writes a new wdm with similar name          **
************************************************************************
      include 'septic.inc'

      integer maxBreaks,nbreaks
      parameter (maxBreaks=30)

      real septic(maxBreaks)  ! septic break points 
      integer BJday(maxBreaks) ! julian day of the breaks from start

      character*100 filename

      integer ndays,year,nb,j
      real factor,denom

      integer julian
      external julian

      data sdate /0,1,1,0,0,0/
      data edate /0,12,31,24,0,0/

      real daily(ndaymax) ! daily values
      real value

      real Tseptic
      character*1 dummy

      character*300 command 

      character*50 dfnam,sepscen   ! base data file and septic scenario
      integer lendfnam,lensepscen

      logical found,riverfirst

**************** END DECLARATIONS **************************************
      read*,sdate(1),edate(1),dfnam,sepscen
      call lencl(dfnam,lendfnam)
      call lencl(sepscen,lensepscen)

******** housekeeping
      filename = dummyWDMname
      call wdbopnlong(wdmfil+1,filename,0,err)
      if (err.ne.0) go to 992

      ndays = julian(sdate(1),1,1,edate(1),12,31)
      if (ndays.gt.ndaymax) go to 994
      dsn = 3010     ! dsn for septic nitrate
     
************ read data file
      fnam = tree//'input/unformatted/septic/'//dfnam(:lendfnam)
      write(*,*) 'reading data file ',fnam
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      
      read(dfile,*) dummy  ! process header
      riverfirst = .false.
      if (dummy.eq.'r'.or.dummy.eq.'R') riverfirst = .true.
      do    ! loop over all lines in files

        read(dfile,'(a100)',err=993,end=111) line
        call d2x(line,last)
        if (riverfirst) then
          read(line,*,err=993,end=993) rseg,lseg,Tseptic
        else
          read(line,*,err=993,end=993) lseg,rseg,Tseptic
        end if

        wdmfnam = ScenDatDir//'river/septic/'//sepscen(:lensepscen)//
     .            '/septic_'//lseg//'_to_'//rseg//'.wdm'
        command = 'cp -v '//tree//'config/blank_wdm/blank_septic.wdm '//
     .            wdmfnam
        print*, command
        call system(command)
        call wdbopnlong(wdmfil,wdmfnam,0,err)
        if (err.ne.0) go to 992

        do j = 1,ndays
          daily(j) = Tseptic
        end do

************** write to wdm
        print*,'writing ',j,' values to ',wdmfnam
        call putdailydsn(wdmfil,sdate,edate,dsn,j,daily)
        call wdflcl(wdmfil,err)   ! close wdm file
        if (err.ne.0) go to 995

      end do

111   close(dfile)
      print*,'done making all wdms'

      stop

************************* ERROR SPACE **********************************
991   report(1) = 'could not open file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'WDM file must previously exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
      go to 999

993   report(1) = 'error reading file, near line:'
      report(2) = fnam
      report(3) = line
      go to 999

994   report(1) = 'start and stop dates provided to the program result'
      report(2) = '  in more than the allowable days in standard.inc'
      write(report(3),*) ' ',sdate(1),' ',edate(1),' ',ndays,' ',ndaymax
      go to 999

995   report(1) = 'Error: closing wdm = '
      write(report(1)(28:30),'(i3)')err
      report(2) = ' wdm name =  '
      report(3) = wdmfnam
      go to 999

997   report(1) = 'unexpected year reading file'
      report(2) = fnam
      report(3) = ' alter data or extend program indices'
      go to 999

999   print*,report(1)
      print*,report(2)
      print*,report(3)
      call stopreport(report)
      end

