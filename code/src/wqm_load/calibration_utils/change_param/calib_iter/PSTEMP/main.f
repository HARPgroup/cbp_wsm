************************************************************************
** program to change parameters for calibration.  Automatically reads **
**   csv files and performs functions on the variables                **
************************************************************************
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include 'tempcal.inc'

      integer ns,nr,nm

      character*25 paramscens(nlu),lscens(nlu)
      integer lenparamscens(nlu),lenlscens(nlu)

      character*25 basin
      integer lenbasin

      integer nlinemax,nlines(nlu)  ! number of parameter lines in code
      parameter (nlinemax=400)
      character*2000 parline(nlinemax,nlu),
     .                       varline,tableline,
     .                       vkeep(nlu),tkeep(nlu)

      integer vcASLT(nlu),vcULTP1(nlu),vcLGTP1(nlu) ! column of variable
      integer vcBSLT(nlu),vcULTP2(nlu),vcLGTMP(nlu)

      logical found

      integer i,j,l  ! indices
      integer iline(nlu)
      character*4 version ! version number
      logical fileexist,anyexist  ! if file already exists

      real aggfactor ! variable to aggregate river factors for each lseg

*************** END DECLARATIONS ***************************************


************ SPECIFICATION ENTRY SECTION
      read*,calscen,rscen

      call readcontrol_lscen(rscen,lscens)

      do nl = 1,numlu
        call lencl(lscens(lus(nl)),lenlscens(lus(nl)))
        call readcontrol_Lparamscen(
     I                              lscens(lus(nl)),lenlscens(lus(nl)),
     O                              paramscens(lus(nl)))
        call lencl(paramscens(lus(nl)),lenparamscens(lus(nl)))
      end do

      module = 'PSTEMP'
      call uppercase(module)
      call lencl(module,lenmod)
      call lencl(calscen,lencalscen)

      basin = calscen(:lencalscen)//'_'//module
************* PRINT SKIPPED LAND USES
      do i = 1,nlu ! total number of land uses
        found = .false.
        do j = 1,numlu ! number of optimized land uses
          if (lus(j).eq.i) found = .true.
        end do
        if (.not.found) print*,'not optimizing ',luname(i)
      end do

**********HOUSE KEEPING ************************
      call readLandSeglist(basin,
     O                     lsegs,nlsegs)

      call readRiverSeglist(
     I                      basin,
     O                      rsegs,nrsegs)

      call getorphans(module,lenmod,calscen,lencalscen,
     O                orphans,surrogates,norphans,fnam,err)
      if (err.eq.991) go to 991
      if (err.ne.0) go to 998

*********** populate uniqid, dsid, uniqindex
      do ns = 1,supermax
        uniqindex(ns) = 0
      end do
      do nr = 1,nrsegs
        read(rsegs(nr)(5:8),'(i4)') uniqid(nr)
        read(rsegs(nr)(10:13),'(i4)') dsid(nr)
        uniqindex(uniqid(nr)) = nr
      end do
   
*********** READ IN LAND-RIVER WEIGHTS
      call getR2L(module,lenmod,calscen,lencalscen,
     I            lsegs,nlsegs,uniqindex,
     O            weight,R2L,nR2L,fnam,err)
      if (err.eq.991) go to 991
      if (err.ne.0) go to 998

************  READ IN LAND USE RATIOS
      call zipall( 
     M            limitsLGTP1,limitsASLT,limitsULTP1,BSLT,ULTP2)

      call getlufacs(module,lenmod,calscen,lencalscen,
     M            limitsLGTP1,limitsASLT,limitsULTP1,BSLT,ULTP2)

C      call printall(   ! uncomment this to check input
C     I            limitsLGTP1,limitsASLT,limitsULTP1,BSLT,ULTP2)

**********           FIND FIRST FREE FILE NAME FOR ALL LAND USES
      do i = 0,9999
        write(version,'(i4)') i
        do j = 1,4
          if (version(j:j).eq.' ') version(j:j) = '0'
        end do
        anyexist = .false.
          do nl = 1,numlu
            fnam =pardir//luname(lus(nl))//'/'//paramscens(lus(nl))
     .            (:lenparamscens(lus(nl)))
     .            //'/'//module(:lenmod)//'_'//version//'.csv'
            inquire (file=fnam,exist=fileexist)
            if (fileexist) anyexist = .true.
          end do
        if (.not.anyexist) exit
      end do

************ READ IN ALL RIVER STATISTICS
      call getRstats(
     I               rscen,uniqindex,version,R2L,nR2L,nlsegs,rsegs,
     O               facASLT,facULTP1,facLGTP1)

      print*,'all accounting'

************* LOOP OVER LAND USES, FIND THE FILE AND MAKE THE CHANGES

      do nl = 1,numlu

**********            OPEN FILES
        fnam = pardir//luname(lus(nl))//'/'//paramscens(lus(nl))
     .            (:lenparamscens(lus(nl)))//
     .         '/'//module(:lenmod)//'.csv'
        open(dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

**********            READ HEADER LINES
        read(dfile,'(a2000)',err=994) tableline
        call d2x(tableline,last)
        tkeep(lus(nl)) = tableline
        read(dfile,'(a2000)',err=994) varline
        call d2x(varline,last)
        vkeep(lus(nl)) = varline

**********            READ WHOLE FILE INTO MEMORY
        nlines(lus(nl)) = 1
        read(dfile,'(a2000)',err=996,end=997)
     .               parline(nlines(lus(nl)),lus(nl))
        do while (parline(nlines(lus(nl)),lus(nl))(:3).ne.'end')
          nlines(lus(nl)) = nlines(lus(nl)) + 1
          read(dfile,'(a2000)',err=996,end=997) 
     .                 parline(nlines(lus(nl)),lus(nl))
        end do
        close(dfile)
          
**********           WRITE FILE TO NEW FILE NAME
        fnam = pardir//luname(lus(nl))//'/'//paramscens(lus(nl))
     .            (:lenparamscens(lus(nl)))//
     .         '/'//module(:lenmod)//'_'//version//'.csv'
        open(dfile,file=fnam,status='new',iostat=err)
        if (err.ne.0) go to 991
        call rytdos(tkeep(lus(nl)),dfile)
        call rytdos(vkeep(lus(nl)),dfile)
        do i = 1,nlines(lus(nl))
          call rytdos(parline(i,lus(nl)),dfile)
        end do
        close(dfile)

      end do

      call allcolumns( 
     I       tkeep,vkeep,lus,numlu,module,
     O       vcASLT,vcULTP1,vcLGTP1,vcBSLT,vcULTP2,vcLGTMP)

      do ns = 1,nlsegs  ! LOOP OVER ALL LAND SEGS AND MAKE MODS

C        if (mod(ns,5).eq.0) 
        print*,'modifying ',ns,' out of ',nlsegs,' segments'
C        end if
        found = .false.
        do i = 1,norphans
          if (orphans(i).eq.lsegs(ns)) found = .true.
        end do
        if (found) cycle  ! if an orphan, ignore

        do nl = 1,numlu     ! GET ALL LINE NUMBERS
          found = .false.
          do i = 1,nlines(lus(nl))
            read(parline(i,lus(nl)),*,err=992,end=992) tseg
            if (tseg.eq.lsegs(ns)) then
              iline(lus(nl)) = i
              found = .true.
              exit
            end if
          end do
          if (.not.found) go to 995
        end do      

        do i = 1,numlu
          nl = lus(i)
          do nm = 1,12
            call getvar(parline(iline(nl),nl),
     .                  vcASLT(nl)+nm-1,ASLT(nl,nm))
            call getvar(parline(iline(nl),nl),
     .                  vcULTP1(nl)+nm-1,ULTP1(nl,nm))
            call getvar(parline(iline(nl),nl),
     .                  vcLGTP1(nl)+nm-1,LGTP1(nl,nm))
          end do
        end do

******************* apply factors to all variables
        do nm = 1,12
          aggfactor = 0.0
          do nr = 1,nR2L(ns)
            aggfactor = aggfactor + weight(ns,nr)*facASLT(R2L(ns,nr),nm)
          end do
          do nl = 1,numlu
            ASLT(lus(nl),nm) = ASLT(lus(nl),nm) + aggfactor
          end do
        end do

        do nm = 1,12
          aggfactor = 0.0
          do nr = 1,nR2L(ns)
            aggfactor = aggfactor+weight(ns,nr)*facULTP1(R2L(ns,nr),nm)
          end do
          do nl = 1,numlu
            ULTP1(lus(nl),nm) = ULTP1(lus(nl),nm) + aggfactor
          end do
        end do

        do nm = 1,12
          aggfactor = 0.0
          do nr = 1,nR2L(ns)
            aggfactor = aggfactor+weight(ns,nr)*facLGTP1(R2L(ns,nr),nm)
          end do
          do nl = 1,numlu
            LGTP1(lus(nl),nm) = LGTP1(lus(nl),nm) + aggfactor
          end do
        end do

        do nl = 1,numlu
          LGTMP(lus(nl)) = LGTP1(lus(nl),1)
        end do
          
***************** check for max and min
        call minmax (
     M               ASLT,ULTP1,LGTP1,
     I               limitsASLT,limitsULTP1,limitsLGTP1)
  
*************** put variables in files in memory
        do i = 1,numlu
          nl = lus(i)
          do nm = 1,12
            call putvar(
     .           parline(iline(nl),nl),vcASLT(nl)+nm-1,ASLT(nl,nm))
            call putvar(
     .           parline(iline(nl),nl),vcULTP1(nl)+nm-1,ULTP1(nl,nm))
            call putvar(
     .           parline(iline(nl),nl),vcLGTP1(nl)+nm-1,LGTP1(nl,nm))
            call putvar(
     .           parline(iline(nl),nl),vcBSLT(nl)+nm-1,BSLT(nl))
            call putvar(
     .           parline(iline(nl),nl),vcULTP2(nl)+nm-1,ULTP2(nl))
          end do
          call putvar(parline(iline(nl),nl),vcLGTMP(nl),LGTMP(nl))
        end do

**************** if this is a surrogate, change the orphans as well
        do i = 1,norphans
          if (surrogates(i).eq.lsegs(ns)) then
            do l = 1,numlu
              nl = lus(l)
              found = .false.
              do j = 1,nlines(nl)
                read(parline(j,nl),*,err=992,end=992) tseg
                if (tseg.eq.orphans(i)) then
                  found = .true.
                  do nm = 1,12
                    call putvar(
     .                   parline(j,nl),vcASLT(nl)+nm-1,ASLT(nl,nm))
                    call putvar(
     .                   parline(j,nl),vcULTP1(nl)+nm-1,ULTP1(nl,nm))
                    call putvar(
     .                   parline(j,nl),vcLGTP1(nl)+nm-1,LGTP1(nl,nm))
                    call putvar(
     .                   parline(j,nl),vcBSLT(nl)+nm-1,BSLT(nl))
                    call putvar(
     .                   parline(j,nl),vcULTP2(nl)+nm-1,ULTP2(nl))
                  end do
                  call putvar(parline(j,nl),vcLGTMP(nl),LGTMP(nl)) 
                  exit
                end if
              end do
              if (.not.found) go to 993
            end do
          end if
        end do

      end do    ! ALL MODIFICATIONS FINISHED


**********          REWRITE MODS TO ORIGINAL FILE NAME
      do nl = 1,numlu
        fnam= pardir//luname(lus(nl))//'/'//paramscens(lus(nl))
     .            (:lenparamscens(lus(nl)))//
     .        '/'//module(:lenmod)//'.csv'
        open(dfile,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991

        call rytdos(tkeep(lus(nl)),dfile)
        call rytdos(vkeep(lus(nl)),dfile)
        do i = 1,nlines(lus(nl))
          call rytdos(parline(i,lus(nl)),dfile)
        end do
        close(dfile)

      end do

      stop

********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'did not find '//lsegs(ns)
      report(2) = 'in file'
      report(3) = fnam
      go to 999

9921  report(1) = 'extra segment '//tseg
      report(2) = 'in file'
      report(3) = fnam
      go to 999

993   report(1) = 'orphan segment '//orphans(i)// 'not found'
      report(2) = 'for land use '//luname(nl)
      report(3) = ' '
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'error occured in first two lines'
      go to 999

995   report(1) = 'problem with '//module//'.csv file for land use '
     .             //luname(lus(nl))
      report(2) = 'land segment '//lsegs(ns)//' not found'
      report(3) = ' '
      go to 999

996   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = parline(nlines(lus(nl))-1,lus(nl))(:100)
      go to 999

997   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = 'file ended before literal '//char(39)//'end'//
     .             char(39)//' found'
      go to 999

998   report(1) = 'problem reading file '
      report(2) = fnam
      report(3) = ' '
      if (err.eq.993) report(3) = 'file ended before literal '//char(39)
     .                             //'end'//char(39)//' found'
      if (err.eq.994) report(3) = 'problem reading river seg'
      if (err.eq.995) report(3) = 'problem reading land seg'
      if (err.eq.996) report(3) = 'weights do not add to one'
      if (err.eq.997) report(3) = 'problem reading weights'
      go to 999

999   call stopreport(report)

      end
