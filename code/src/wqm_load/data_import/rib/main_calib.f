************************************************************************
**  reads from a file and writes a new wdm with similar name          **
************************************************************************
      include 'rib.inc'

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

      character*1 dummy

      character*300 command 

      character*50 dfnam,ribscen   ! base data file and rib scenario
      integer lendfnam,lenribscen

c      integer maxnlrsegs,nlrsegs,nlr  ! number of lrsegs
c      parameter (maxnlrsegs = 3000) 
c      character*19 lrsegs(maxnlrsegs)

      integer ivar,ivar1,ivar2

c      real annrib(lowyear:highyear,maxnlrsegs)

      real CVP2N, CVN2BOD

      logical found

**************** END DECLARATIONS **************************************
      read*,sdate(1),edate(1),dfnam,ribscen,s2rfnam
      call lencl(dfnam,lendfnam)
      call lencl(ribscen,lenribscen)
      call lencl(rib,lenrib)

******** housekeeping
      do nlr = 1,maxnlrsegs
       do year = sdate(1),edate(1)
        do ivar=1,nvars_inp
          annload(year,ivar,nlr) = -999.0
        end do
        do ivar=1,nvars_out
          annrib(year,ivar,nlr) = -999.0
        end do
       end do
      end do

      filename = dummyWDMname
      call wdbopnlong(wdmfil+1,filename,0,err)
      if (err.ne.0) go to 992

      ndays = julian(sdate(1),1,1,edate(1),12,31)
      if (ndays.gt.ndaymax) go to 994
c      dsn = 3010     ! dsn for rib nitrate

************ read data file
      fnam = tree//'input/unformatted/rib/'//dfnam(:lendfnam)
      write(*,*) 'reading data file ',fnam
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,*,err=993) dummy,dummy,dummy,
     .                      (tmpnm(ivar1),ivar1=1,nvars_inp)
      do ivar1=1,nvars_inp
        found = .false.
        do ivar2=1,nvars_inp
          if(tmpnm(ivar1).eq.loadnm(ivar2)) then
            toindx(ivar1) = ivar2
            found = .true.
          end if
        end do
        if (.not.found ) go to 881
      end do
      print*,(toindx(ivar),ivar=1,nvars_inp)

      nlrsegs = 0
      do    ! loop over all lines in files

        read(dfile,*,err=993,end=111)
     .             year,rseg,lseg,(Trib(toindx(ivar)),ivar=1,nvars_inp)

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

        do ivar=1,nvars_inp
          annload(year,ivar,nlr) = Trib(ivar)
        end do

      end do

111   close(dfile)



      !! >> READ S2R FACTORS
c      nconx     = 1
c      C_conx(1) = 'no3n'
c      call read_s2r(s2rfnam,lrsegs,C_conx,nlrsegs,nconx,s2rfac)
      call read_s2r(s2rfnam,lrsegs,loadnm,nlrsegs,nvars_inp,s2rfac)


      fnam = ScenDatDir//'river/rib/'//ribscen(:lenribscen)//
     .            '/'//rib(:lenrib)//'_lrseg_ann.csv'
      open (dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991


      !! >> APPLY S2R FACTORS
      do nlr = 1,nlrsegs
       print*,'S2R APPLY ', lrsegs(nlr),
     .               (s2rfac(nlr,ivar),ivar=1,nvars_inp)
       do year = sdate(1),edate(1)
        do ivar = 1,nvars_inp
         Trib(ivar) = annload(year,ivar,nlr)
         if ( annload(year,ivar,nlr) .gt. 0 )
     .    annload(year,ivar,nlr)=annload(year,ivar,nlr)*s2rfac(nlr,ivar)
        end do
        write(dfile,*)lrsegs(nlr)(:6),',',lrsegs(nlr)(7:19),',',
     .   year,',B',(',',Trib(ivar),ivar=1,nvars_inp)
        write(dfile,*)lrsegs(nlr)(:6),',',lrsegs(nlr)(7:19),',',
     .   year,',A',(',',annload(year,ivar,nlr),ivar=1,nvars_inp)
       end do
      end do

      close(dfile)


******** >> ASSIGN TO LOAD VARS TO RIB VARS
      CVP2N   = 7.23
      CVN2BOD = 22.9
      do nlr = 1,nlrsegs
        do year = sdate(1),edate(1)

           NH3X = annload(year,1,nlr)
           NO3X = annload(year,2,nlr)
           ORNX = annload(year,3,nlr)
           PO4X = annload(year,4,nlr)
           ORPX = annload(year,5,nlr)

           BODX = -999.0
           if ( ORNX.ge.0 .or. ORPX.ge.0 ) then
            BODX = min(ORNX,ORPX*CVP2N) * CVN2BOD
            ORNX = ORNX - BODX/CVN2BOD
            ORPX = ORPX - (BODX/CVN2BOD)/CVP2N
           end if

           annrib(year,1,nlr) = NH3X
           annrib(year,2,nlr) = NO3X
           annrib(year,3,nlr) = ORNX
           annrib(year,4,nlr) = PO4X
           annrib(year,5,nlr) = ORPX
           annrib(year,6,nlr) = BODX

        end do
      end do


*********** got all data, now loop over nlrsegs found
************ copy blank file, open file, create breaks, and interpolate
      do nlr = 1,nlrsegs

        wdmfnam = ScenDatDir//'river/rib/'//ribscen(:lenribscen)//
     .            '/'//rib(:lenrib)//'_'//lrsegs(nlr)(:6)//'_to_'//
     .            lrsegs(nlr)(7:19)//'.wdm'
        command = 'cp '//tree//'config/blank_wdm/blank_rib.wdm '//
     .            wdmfnam
        call system(command)
        call wdbopnlong(wdmfil,wdmfnam,0,err)
        if (err.ne.0) go to 992

       do ivar = 1,nvars_out
       !{
        nbreaks = 0
        do year = sdate(1),edate(1)   ! convert to breaks
          print*,lrsegs(nlr)(:6)//lrsegs(nlr)(7:19),
     .      ivar,annrib(year,ivar,nlr)
          if (annrib(year,ivar,nlr).gt.-.01) then ! not missing
            nbreaks = nbreaks + 1
            BJday(nbreaks) = julian(sdate(1),1,1,year,7,1)
            nbrib(nbreaks) = annrib(year,ivar,nlr)
          end if
        end do

************* sort in case the data file was out of order
        call irbubble2vector(BJday,nbrib,nbreaks,maxBreaks)

        if (nbreaks.eq.0) go to 998 ! all data missing

        if (nbreaks.eq.1) then  ! create second break
          nbreaks = 2
          BJday(2) = BJday(1) + 1000
          nbrib(2) = nbrib(1)
        end if

********* interpolate 
        j = 1
        indx1 = 1
        indx2 = 2
        lastindx = .false.

        do while (j.le.ndays)
          denom = real(BJday(indx2) - BJday(indx1))
          factor = (nbrib(indx2)-nbrib(indx1))/denom

          do while ((j.le.BJday(indx2).or.lastindx).and.j.le.ndays)
            daily(j) = nbrib(indx1) + factor*real(j-BJday(indx1))
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
        call putdailydsn(wdmfil,sdate,edate,ribdsn(ivar),j,daily)
       !}
       end do

       call wdflcl(wdmfil,err)   ! close wdm file
       if (err.ne.0) go to 995

      end do

      print*,'done making all wdms'

      stop

************************* ERROR SPACE **********************************
881   report(1) = 'error parsing header variables'
      report(1) = fnam
      report(3) = tmpnm(ivar1)
      go to 999

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
      write(report(3),*)year,',',lseg,',',rseg,',',Trib
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

