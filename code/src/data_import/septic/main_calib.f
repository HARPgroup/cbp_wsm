************************************************************************
**  reads from a file and writes a new wdm with similar name          **
************************************************************************
      include 'septic.inc'

      integer maxBreaks,nbreaks
      parameter (maxBreaks=30)

      real septic(maxBreaks)  ! septic break points 
      integer BJday(maxBreaks) ! julian day of the breaks from start

      character*100 filename

      integer ndays,year
      real factor,denom

      integer j,indx1,indx2
      logical lastindx

      integer julian
      external julian

      data sdate /0,1,1,0,0,0/
      data edate /0,12,31,24,0,0/

      real daily(ndaymax) ! daily values

      real Tseptic
      character*1 dummy

      character*300 command 

      character*50 dfnam,sepscen   ! base data file and septic scenario
      integer lendfnam,lensepscen

      integer maxnlrsegs,nlrsegs,nlr  ! number of lrsegs
      parameter (maxnlrsegs = 3000) 
      character*19 lrsegs(maxnlrsegs)

      integer lowyear,highyear
      parameter (lowyear = 1981, highyear = lowyear + maxBreaks)
      real annseptic(lowyear:highyear,maxnlrsegs)

      logical found

**************** END DECLARATIONS **************************************
      read*,sdate(1),edate(1),dfnam,sepscen
      call lencl(dfnam,lendfnam)
      call lencl(sepscen,lensepscen)

******** housekeeping
      do nlr = 1,maxnlrsegs
        do year = sdate(1),edate(1)
          annseptic(year,nlr) = -999.0
        end do
      end do

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

      read(dfile,*) dummy  ! get rid of header
      nlrsegs = 0
      do    ! loop over all lines in files

        read(dfile,*,err=993,end=111) year,rseg,lseg,Tseptic

        if (year.lt.lowyear.or.year.gt.highyear) go to 997

        found = .false.  ! get nlr
        do nlr = 1,nlrsegs
          if (lrsegs(nlr).eq.lseg//rseg) then
            found = .true.
            exit
          end if
        end do
        if (.not.found) then
          nlrsegs = nlrsegs + 1
          if (nlrsegs.gt.maxnlrsegs) go to 990
          nlr = nlrsegs
          lrsegs(nlr) = lseg//rseg
        end if

        annseptic(year,nlr) = Tseptic

      end do

111   close(dfile)

*********** got all data, now loop over nlrsegs found
************ copy blank file, open file, create breaks, and interpolate
      do nlr = 1,nlrsegs

        wdmfnam = ScenDatDir//'river/septic/'//sepscen(:lensepscen)//
     .            '/septic_'//lrsegs(nlr)(:6)//'_to_'//
     .            lrsegs(nlr)(7:19)//'.wdm'
        command = 'cp '//tree//'config/blank_wdm/blank_septic.wdm '//
     .            wdmfnam
        call system(command)
        call wdbopnlong(wdmfil,wdmfnam,0,err)
        if (err.ne.0) go to 992

        nbreaks = 0
        do year = sdate(1),edate(1)   ! convert to breaks
          if (annseptic(year,nlr).gt.-.01) then ! not missing
            nbreaks = nbreaks + 1
            BJday(nbreaks) = julian(sdate(1),1,1,year,7,1)
            septic(nbreaks) = annseptic(year,nlr)
          end if
        end do

************* sort in case the data file was out of order
        call irbubble2vector(BJday,septic,nbreaks,maxBreaks)

        if (nbreaks.eq.0) go to 998 ! all data missing

        if (nbreaks.eq.1) then  ! create second break
          nbreaks = 2
          BJday(2) = BJday(1) + 1000
          septic(2) = septic(1)
        end if

********* interpolate 
        j = 1
        indx1 = 1
        indx2 = 2
        lastindx = .false.

        do while (j.le.ndays)
          denom = real(BJday(indx2) - BJday(indx1))
          factor = (septic(indx2)-septic(indx1))/denom

          do while ((j.le.BJday(indx2).or.lastindx).and.j.le.ndays)
            daily(j) = septic(indx1) + factor*real(j-BJday(indx1))
C            if (daily(j).lt. -0.1) go to 996
            daily(j) = max(daily(j),0.0)
            j = j + 1
          end do

          if (indx2.eq.nbreaks) then
            lastindx = .true.
          else
            indx2 = indx2+1
            indx1 = indx1+1
          end if

        end do

************** write to wdm
        print*,'writing ',j,' values to ',wdmfnam
        call putdailydsn(wdmfil,sdate,edate,dsn,j,daily)
        call wdflcl(wdmfil,err)   ! close wdm file
        if (err.ne.0) go to 995

      end do

      print*,'done making all wdms'

      stop

************************* ERROR SPACE **********************************
990   report(1) = 'too many lrsegs in file'
      report(2) = fnam
      report(3) = ' increase parameter maxlrsegs and recompile '
      go to 999

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
      write(report(3),*)year,',',lseg,',',rseg,',',Tseptic
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

996   report(1) = 'unexpected error interpolating data from file:'
      report(2) = fnam
      report(3) = lrsegs(nlr)//' resulted in a negative amount'
      go to 999

997   report(1) = 'unexpected year reading file'
      report(2) = fnam
      report(3) = ' alter data or extend program indices'
      go to 999

998   report(1) = 'Problem with file: could not find any data for lrseg'
      report(2) = fnam
      report(3) = lrsegs(nlr)
      go to 999

999   print*,report(1)
      print*,report(2)
      print*,report(3)
      call stopreport(report)
      end

