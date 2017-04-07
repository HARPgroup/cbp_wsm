************************************************************************
** program to change parameters for calibration.  Automatically reads **
**   csv files and performs functions on the variables                **
************************************************************************
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include 'calib.inc'

      integer ns,nr

      character*12 module  ! variable specifiers
      integer lenmod

      character*25 paramscens(nlu),lscens(nlu)
      integer lenparamscens(nlu),lenlscens(nlu)

      character*25 basin
      integer lenbasin

      integer nlinemax,nlines(nlu)  ! number of parameter lines in code
      parameter (nlinemax=500)
      character*2000 parline(nlinemax,nlu),
     .                       varline,tableline,
     .                       vkeep(nlu),tkeep(nlu)
      character*100 LEline(nlinemax)  ! storage of the land evap file
      integer nLElines
              ! the 3 lines of the file with relevant info

      integer varcolumn  ! column that the variable of interest occupies
      integer vcLZSN(nlu),vcAGWR(nlu),vcIRC(nlu),vcINTFW(nlu),
     .        vcINFILT(nlu),vcAGWETP(nlu),vcKVARY(nlu),vcUZSN(nlu),
     .        vcUZSNJAN(nlu)

      logical found

      integer i,j,k,l,m  ! indices
      integer iline(nlu)
      character*4 version ! version number
      logical fileexist,anyexist  ! if file already exists

      real landevap  ! land evap factor

      real aggfactor ! variable to aggregate river factors for each lseg
      real avsuro    ! average surface runoff SURO

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

      module = 'PWATER'
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

      call getorphans(calscen,
     O                orphans,surrogates,norphans,fnam,err)
      if (err.eq.991) go to 991
      if (err.ne.0) go to 998
  
********** POPULATE UNIQINDEX
      do nr = 1,nrsegs
        Tseg = rsegs(nr)
        read(Tseg(5:8),'(i4)')uniqid(nr)
        uniqindex(uniqid(nr))=nr
      end do
      
*********** READ IN LAND-RIVER WEIGHTS
      call getR2L(
     I            calscen,lsegs,nlsegs,uniqindex,orphans,norphans,
     O            weight,R2L,nR2L,fnam,err)
      if (err.eq.991) go to 991
      if (err.ne.0) go to 998

************  READ IN LAND USE RATIOS
      call zipall(
     O      lufacLZSN,lufacINFILT,lufacIRC,lufacAGWR,
     O      lufacINTFW,lufacAGWETP,lufacKVARY,columnorder,
     O      limitsLandEvap,limitsLZSN,limitsINFILT,limitsIRC,limitsAGWR,
     O      limitsINTFW,limitsAGWETP,limitsKVARY,UZSNfac,monthlyUZSN,
     O      monthlyUZSNfac,SUROtargets)

      call getlufacs(calscen,
     O      lufacLZSN,lufacINFILT,lufacIRC,lufacAGWR,
     O      lufacINTFW,lufacAGWETP,lufacKVARY,columnorder,
     O      limitsLandEvap,limitsLZSN,limitsINFILT,limitsIRC,limitsAGWR,
     O      limitsINTFW,limitsAGWETP,limitsKVARY,UZSNfac,monthlyUZSN,
     O      monthlyUZSNfac,SUROtargets)
     
C      call printall(   ! uncomment this to check input
C     O      lufacLZSN,lufacINFILT,lufacIRC,lufacAGWR,
C     O      lufacINTFW,lufacAGWETP,lufacKVARY,columnorder,
C     O      limitsLandEvap,limitsLZSN,limitsINFILT,limitsIRC,limitsAGWR,
C     O      limitsINTFW,limitsAGWETP,limitsKVARY,UZSNfac,monthlyUZSN,
C     O      monthlyUZSNfac,SUROtargets)

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
      call getRstats(rscen,uniqindex,version,R2L,nR2L,nlsegs,rsegs,
     O               facLandEvap,facLZSN,facINFILT,facIRC,facAGWR,
     O               facINTFW,facAGWETP,facKVARY)
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
     O        vcLZSN,vcAGWR,vcIRC,vcINTFW,vcINFILT,vcAGWETP,vcKVARY,
     O        vcUZSN,vcUZSNJAN)

      do ns = 1,nlsegs  ! LOOP OVER ALL LAND SEGS AND MAKE MODS
        print*, lsegs(ns)
        if (mod(ns,5).eq.0) 
     .          print*,'modifying ',ns,' out of ',nlsegs,' segments'

        found = .false.
        do i = 1,norphans
          if (orphans(i).eq.lsegs(ns)) found = .true.
        end do
        if (found) cycle  ! if an orphan, ignore

        do nl = 1,numlu     ! GET ALL LINE NUMBERS
          do i = 1,nlines(lus(nl))
            read(parline(i,lus(nl)),*,err=992,end=992) tseg
            if (tseg.eq.lsegs(ns)) then
              iline(lus(nl)) = i
              exit
            end if
          end do
        end do      

        nl = lhom  ! all other land uses ratioed against hom
        call getvar(parline(iline(nl),nl),vcLZSN(nl),LZSN(nl))
        call getvar(parline(iline(nl),nl),vcAGWR(nl),AGWR(nl))
        call getvar(parline(iline(nl),nl),vcIRC(nl),IRC(nl))
        call getvar(parline(iline(nl),nl),vcINTFW(nl),INTFW(nl))
        call getvar(parline(iline(nl),nl),vcINFILT(nl),INFILT(nl))
        call getvar(parline(iline(nl),nl),vcAGWETP(nl),AGWETP(nl))
        call getvar(parline(iline(nl),nl),vcKVARY(nl),KVARY(nl))

        call getSURO(luname(lus(nl)),lsegs(ns),paramscens(lus(nl)),
     O               avsuro)                                       ! get SURO for a segment/land use   

        SURO(ns,lus(nl)) = avsuro
  
******************* apply factors to all variables
        aggfactor = 0.0
        do nr = 1,nR2L(ns)
          aggfactor = aggfactor + weight(ns,nr)*facLZSN(R2L(ns,nr))
        end do
        LZSN(lhom) = LZSN(lhom) * aggfactor
        aggfactor = 0.0
        do nr = 1,nR2L(ns)
          aggfactor = aggfactor + weight(ns,nr)*facAGWR(R2L(ns,nr))
        end do
        AGWR(lhom) = AGWR(lhom) * aggfactor
        aggfactor = 0.0
        do nr = 1,nR2L(ns)
          aggfactor = aggfactor + weight(ns,nr)*facIRC(R2L(ns,nr))
        end do
        IRC(lhom) = IRC(lhom) * aggfactor
        aggfactor = 0.0
        do nr = 1,nR2L(ns)
          aggfactor = aggfactor + weight(ns,nr)*facINTFW(R2L(ns,nr))
        end do
        INTFW(lhom) = INTFW(lhom) * aggfactor
        aggfactor = 0.0
        do nr = 1,nR2L(ns)
          aggfactor = aggfactor + weight(ns,nr)*facINFILT(R2L(ns,nr))
        end do
        INFILT(lhom) = INFILT(lhom) * aggfactor
        aggfactor = 0.0
        do nr = 1,nR2L(ns)
          aggfactor = aggfactor + weight(ns,nr)*facAGWETP(R2L(ns,nr))
        end do
        AGWETP(lhom) = AGWETP(lhom) * aggfactor
        aggfactor = 0.0
        do nr = 1,nR2L(ns)
          aggfactor = aggfactor + weight(ns,nr)*facKVARY(R2L(ns,nr))
        end do
        KVARY(lhom) = KVARY(lhom) * aggfactor
    
*************** make sure that all factors are positive by 
************ enforcing minumum, this corrects an error where a parameter of
************* zero would produce a corrupt parameter file.  maxima and minima
************* are reset for all land uses in the subroutine minmax below
        LZSN(lhom) = max(LZSN(lhom),limitsLZSN(1))
        AGWR(lhom) = max(AGWR(lhom),limitsAGWR(1))
        IRC(lhom) = max(IRC(lhom),limitsIRC(1))
        INTFW(lhom) = max(INTFW(lhom),limitsINTFW(1))
        INFILT(lhom) = max(INFILT(lhom),limitsINFILT(1))
        AGWETP(lhom) = max(AGWETP(lhom),limitsAGWETP(1))
        KVARY(lhom) = max(KVARY(lhom),limitsKVARY(1))
**************** apply factors to all land uses
        do nl = 1,numlu
          if (lus(nl).eq.lhom) cycle
          LZSN(lus(nl)) = LZSN(lhom)*lufacLZSN(lus(nl))
          AGWR(lus(nl)) = AGWR(lhom)*lufacAGWR(lus(nl))
          IRC(lus(nl)) = IRC(lhom)*lufacIRC(lus(nl))
          INTFW(lus(nl)) = INTFW(lhom)*lufacINTFW(lus(nl))
          INFILT(lus(nl)) = INFILT(lhom)*lufacINFILT(lus(nl))
          AGWETP(lus(nl)) = AGWETP(lhom)*lufacAGWETP(lus(nl))
          KVARY(lus(nl)) = KVARY(lhom)*lufacKVARY(lus(nl))
        end do

***************** check for max and min
        call minmax (
     M               LZSN,AGWR,IRC,INTFW,
     M               INFILT,AGWETP,KVARY,
     I               limitsLZSN, limitsINFILT, limitsIRC, limitsAGWR,
     I               limitsINTFW, limitsAGWETP, limitsKVARY)
  
*************** adjust intfw so that surface runoff is available
        if (SURO(ns,lhom).lt.SUROtargets(1)) then
          if (SURO(ns,lhom).gt.0.00001) then
            do nl = 1,numlu
              INTFW(lus(nl))=INTFW(lus(nl))*SURO(ns,lhom)/SUROtargets(1)
            end do
          else
            INTFW(lhom) = limitsINTFW(1)
            do nl = 1,numlu
              if (lus(nl).eq.lhom) cycle
              INTFW(lus(nl)) = INTFW(lhom)*lufacINTFW(lus(nl))
            end do
          end if
        end if
        if (SURO(ns,lhom).gt.SUROtargets(2)) then
          do nl = 1,numlu
            INTFW(lus(nl))=INTFW(lus(nl))*SURO(ns,lhom)/SUROtargets(2)
          end do 
        end if
        
*************** put variables in files in memory
        do i = 1,numlu
          nl = lus(i)
          call putvar(parline(iline(nl),nl),vcLZSN(nl),LZSN(nl))
          call putvar(parline(iline(nl),nl),vcAGWR(nl),AGWR(nl))
          call putvar(parline(iline(nl),nl),vcIRC(nl),IRC(nl))
          call putvar(parline(iline(nl),nl),vcINTFW(nl),INTFW(nl))
          call putvar(parline(iline(nl),nl),vcINFILT(nl),INFILT(nl))
          call putvar(parline(iline(nl),nl),vcAGWETP(nl),AGWETP(nl))
          call putvar(parline(iline(nl),nl),vcKVARY(nl),KVARY(nl))
          if (monthlyUZSN(nl)) then
            do j = 1,12
              UZSN = LZSN(nl) * UZSNfac(nl) * monthlyUZSNfac(nl,j)
              call putvar(parline(iline(nl),nl),vcUZSNJAN(nl)+j-1,UZSN)
            end do
          else
            UZSN = LZSN(nl) * UZSNfac(nl)
            call putvar(parline(iline(nl),nl),vcUZSN(nl),UZSN)
          end if
        end do

**************** if this is a surrogate, change the orphan as well
        do i = 1,norphans

          if (surrogates(i).eq.lsegs(ns)) then

            do l = 1,numlu
              nl = lus(l)
              found = .false.
              do j = 1,nlines(nl)
                read(parline(j,nl),*,err=992,end=992) tseg
                if (tseg.eq.orphans(i)) then
                  found = .true.
                  call putvar(parline(j,nl),vcLZSN(nl),LZSN(nl))
                  call putvar(parline(j,nl),vcAGWR(nl),AGWR(nl))
                  call putvar(parline(j,nl),vcIRC(nl),IRC(nl))
                  call putvar(parline(j,nl),vcINTFW(nl),INTFW(nl))
                  call putvar(parline(j,nl),vcINFILT(nl),INFILT(nl))
                  call putvar(parline(j,nl),vcAGWETP(nl),AGWETP(nl))
                  call putvar(parline(j,nl),vcKVARY(nl),KVARY(nl))
                  if (monthlyUZSN(nl)) then
                    do k = 1,12
                      UZSN = LZSN(nl) * UZSNfac(nl)*monthlyUZSNfac(nl,k)
                      m = vcUZSNJAN(nl)+k-1
                      call putvar(parline(j,nl),m,UZSN)
                    end do
                  else
                    UZSN = LZSN(nl) * UZSNfac(nl)
                    call putvar(parline(j,nl),vcUZSN(nl),UZSN)
                  end if
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

************** LAND EVAP PORTION
      fnam = pardir//'common/'//paramscens(lhom)
     .            (:lenparamscens(lhom))//'/land_evap.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nLElines = 1
      call d2x(LEline(nLElines),last)
      read(dfile,'(a100)',err=997,end=997)LEline(nLElines)
      do while (LEline(nLElines)(:3).ne.'end')
        nLElines = nLElines + 1
        read(dfile,'(a100)',err=997,end=997)LEline(nLElines)
        call d2x(LEline(nLElines),last)
      end do
      close(dfile)  ! file read into memory

      fnam = pardir//'common/'//paramscens(lhom)  ! write to new name
     .            (:lenparamscens(lhom))//'/land_evap_'//version//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      do i = 1,nLElines
        call ryt(LEline(i),dfile)
      end do
      close(dfile)

      i  = 2  ! start at line2
      read(LEline(i),*) tseg
      do while (LEline(i)(:3).ne.'end')
        do ns = 1,nlsegs
          if (tseg(:6).eq.lsegs(ns)) then
            read(LEline(i),*)tseg,landevap
            aggfactor = 0.0
            do nr = 1,nR2L(ns)
              aggfactor=aggfactor+weight(ns,nr)*facLandEvap(R2L(ns,nr))
            end do
            landevap = landevap * aggfactor
            landevap = max(limitsLandEvap(1),landevap)
            landevap = min(limitsLandEvap(2),landevap)
            write(LEline(i),*)tseg(:6),',',landevap
            exit
          end if
        end do

        i = i + 1
        read(LEline(i),*) tseg
      end do
      
      fnam = pardir//'common/'//paramscens(lhom)
     .            (:lenparamscens(lhom))//'/land_evap.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      do i = 1,nLElines
        call ryt(LEline(i),dfile)
      end do
      close(dfile)

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
