************************************************************************
**  program to generate the input file for the 57k Bay model          **
**    loops over all cells in water quality model                     **
**      loops over all inputs                                         **
**        creates one variable with all constituents                  **
************************************************************************

      implicit none

      include '../wqmlib/wqm_load.inc'

********* date variables
      integer year,month,day
      integer year1,year2,month1,month2,day1,day2
      parameter (month1=1,day1=1)
      parameter (month2=12,day2=31)
      character*4 cy1,cy2

      include 'date.inc'

********** wqm cell variables
      integer maxcells,ncells,nc
      parameter (maxcells=2400)
      integer cell(maxcells),Tcell
      character*6 ccell  ! character version of cell
      integer lencell

********* lrseg variables
      character*13 Trseg
      character*6 Tlseg
      integer maxlrsegs,nlrsegs,nlrseg
      parameter(maxlrsegs=2000)
      character*19 lrseg(maxlrsegs)

************ variables to match cells to lrsegs
      integer maxCellsPerLrseg,ncLR
      parameter(maxCellsPerLrseg=50)
      integer CellsPerLrseg(maxlrsegs)
      integer lrsegCellIndex(maxlrsegs,maxCellsPerLrseg)
      real lrsegWeight(maxlrsegs,maxCellsPerLrseg)
      real Tweight

********** rseg variables
      integer maxrsegs,nrsegs,nrseg
      parameter (maxrsegs=200)
      character*13 rsegs(maxrsegs)
     
************ variables to match cells to rsegs
      integer maxCellsPerRseg,ncR
      parameter(maxCellsPerRseg=100)
      integer CellsPerRseg(maxrsegs)
      integer rsegCellIndex(maxrsegs,maxCellsPerRseg)
      real rsegWeight(maxrsegs,maxCellsPerRseg)

*********** output load variables
      real wq(366,year1:year2,maxcells,maxBvar)
      integer nd,ny,nq,Divnq
      real pairwq(366,year1:year2,maxBvar)
      double precision AnnualTotal(year1:year2,maxBvar)  ! print out annual totals
      double precision AnnualCell(year1:year2,maxcells,maxBvar)
      double precision AverageCell(maxBvar)

********** ps, atdep, septic variables
      character*25 pradscen, psscen, sepscen
      integer lenpradscen, lenpsscen, lensepscen
      character*11 ps,sep,atdep         ! atdep, pointsource, or septic
      data ps,sep,atdep /'pointsource','septic','atdep'/
      logical doatdep,dops,dosep
c      character*5 psmethod ! must be either 'hcell, wcell, or lrseg'
      real wateracres(ndaymax)

************* utility variables
      integer ndaysinyear
      external ndaysinyear
      integer julian,jday
      external julian
      logical found,comment
      external comment
      integer Nexits  ! number of exits in this river
                      !  affects the river dsns to output
      integer lakeflag,resflag,timestep  ! variables to send to 
                             ! getflags routine, not used

******** hotstart variables  0 = do nothing, 1 = write , 2 = read
      integer hotstart
      character*19 lastlrseg
      integer hotfile  ! file number for hotstart
      parameter (hotfile = dfile + 80)  ! = 91
      integer wdmfil
      parameter (wdmfil = dfile + 9)
      character*200 Request
      integer lenRequest

******** ps variables
      integer nps,np
      parameter (nps=3)
      character*5 psnam(nps)         ! pointsource
      data psnam /'wwtp','indus','cso'/
      integer lenpsnam(nps)
      logical foundfile

************** PS only variables
      integer npssegs,numsegs,ns
      character*6 psl2r(maxL2R)

************ END DECLARATIONS ******************************************

********* stupid wdm thing
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil,wdmfnam,1,err)     ! open dummy read only
      if (err .ne. 0) go to 998             ! and never close

********** initialize
      do nq = 1,maxBvar ! initialize
        do nc = 1,maxcells
          do ny = year1,year2
            do nd = 1,366
              wq(nd,ny,nc,nq) = 0.0
            end do
          end do
        end do
      end do
      do nq = 1,maxBvar
        do ny = year1,year2
          AnnualTotal(ny,nq) = 0.0
        end do
      end do
      do nq = 1,maxBvar
        do nc = 1,maxcells
          do ny = year1,year2
            AnnualCell(ny,nc,nq) = 0.0
          end do
        end do
      end do

      read*,rscen,hotstart,Request ! get river scenario
      call lencl(rscen,lenrscen)
      call lencl(Request,lenRequest)

******** READ THE CONTROL FILE FOR LAND SCENARIOS
      call readcontrol_lscen(rscen,
     .                       LandScen)

********* READ THE CONTROL FILE FOR DATA SCENARIOS
      call readcontrol_wdm(rscen,lenrscen,
     O                     pradscen,psscen,sepscen,
     O                     doatdep,dops,dosep)
      call lencl(psscen,lenpsscen)

********* read control file for I/O, geo, and param scenario
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call readcontrol_Rgeoscen(
     I                          rscen,lenrscen,
     O                          geoscen)
      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(ioscen,lenioscen)
      call lencl(geoscen,lengeoscen)
      call lencl(paramscen,lenparamscen)

******* POPULATE nRv2Bv, Rname2Bv, Rfactor, nRvar, nBvar, Bname,
********         Rdsn, nLvar, Ldsn, Lname, Lfactor
      call readcontrol_modules(
     I                         rscen,lenrscen,
     O                         modules,nmod)
      call masslink(
     I              ioscen,lenioscen,modules,nmod,
     O              nRvar,Rdsn,Rname,RvarBMP,
     O              nLvar,Ldsn,Lname,Lfactor)

******* POPULATE similar variables for the data types
      if (dops) then
        call getvars(
     I               ioscen,lenioscen,
     I               nRvar,Rname,ps,
     O               nPSvar,PSdsn,PSname,PSfac)
      end if

************** POPULATE variables relating river vars to bay vars
      rbfnam = 'river_to_wqm57kPS'
      call riverbay(
     I              ioscen,lenioscen,rbfnam,
     I              nRvar,Rname,
     O              nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,DivBvar)

********** END OF SETUP, OPEN AND READ LRSEG LINK FILE
      fnam = catdir//'geo/'//geoscen(:lengeoscen)//
     .       '/request/p532_'//Request(:lenRequest)//'_lrsegs_PS.csv'

      open(dfile-1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      print*,'reading ',fnam

      ncells = 0  ! initialize variables
      nlrsegs = 0
      do nlrseg = 1,maxlrsegs
        CellsPerLrseg(nlrseg) = 0
      end do

      do 
        read(dfile-1,'(a100)',end=111,err=992) line 
        call d2x(line,last)
        if (comment(line)) cycle
        read(line,*,end=992,err=992) Tcell,Tlseg,Trseg,Tweight
C        print*,'newline ',Tcell,' ',Tlseg,' ',Trseg,' ',Tweight

        found = .false.   ! find cell index
        do nc = 1,ncells
          if (Tcell.eq.cell(nc)) then
            found = .true.
            exit    ! nc preserved
          end if
        end do

        if (.not.found) then
          ncells = ncells + 1
          if (ncells.gt.maxcells) go to 981
          nc = ncells
          cell(nc) = Tcell
        end if

        found = .false.  ! find lrseg index
        do nlrseg = 1,nlrsegs
          if (lrseg(nlrseg).eq.Tlseg//Trseg) then
            found = .true.
            exit     ! nlrseg preserved
          end if
        end do

        if (.not.found) then
          nlrsegs = nlrsegs + 1
          if (nlrsegs.gt.maxlrsegs) go to 982
          nlrseg = nlrsegs
          lrseg(nlrseg) = Tlseg//Trseg
        end if
C        print*,'cell ',nc,'   lrseg ',nlrseg
        
        CellsPerLrseg(nlrseg) = CellsPerLrseg(nlrseg) + 1
C        print*,' cells per ',CellsPerLrseg(nlrseg)
        if (CellsPerLrseg(nlrseg).gt.maxCellsPerLrseg) go to 983
        lrsegCellIndex(nlrseg,CellsPerLrseg(nlrseg)) = nc
        lrsegWeight(nlrseg,CellsPerLrseg(nlrseg)) = Tweight

      end do
111   close(dfile-1)

****************** loop over all lrsegs
      if (dops)then
        
        do np = 1, nps
          call lencl(psnam(np),lenpsnam(np))

          do nlrseg = 1,nlrsegs
            Tlseg = lrseg(nlrseg)(:6)
            Trseg = lrseg(nlrseg)(7:)

          print*,'Processing LRseg ',nlrseg,' of ',nlrsegs,' LRsegs ',
     .          Tlseg,' ',Trseg

            wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .              '/'//psnam(np)(:lenpsnam(np))//'_'//
     .              Tlseg//'_to_'//Trseg//'.wdm'

            inquire(file=wdmfnam,exist=foundfile)
            if (foundfile) then

            print*,'file exists'

              call readdailydat(
     I                      year1,month1,day1,year2,month2,day2,
     I                      nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                      wdmfnam,nRvar,Rname,Rdsn,
     I                      nPSvar,PSdsn,PSname,PSfac,
     O                      pairwq)
              do ncLR = 1,CellsPerLrseg(nlrseg)
                nc = lrsegCellIndex(nlrseg,ncLR)

              print*,'adding lrseg ps: ',wdmfnam,' to cell:',cell(nc)

                do nq = 1,nBvar  ! add to big storage variables
                  do ny = year1,year2
                    do nd = 1,ndaysinyear(ny)
                      wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)
     .                            + pairwq(nd,ny,nq)
     .                            * lrsegWeight(nlrseg,ncLR)
                      AnnualTotal(ny,nq) = AnnualTotal(ny,nq)
     .                               + pairwq(nd,ny,nq)
     .                               * lrsegWeight(nlrseg,ncLR)
                      AnnualCell(ny,nc,nq) = AnnualCell(ny,nc,nq)
     .                                 + pairwq(nd,ny,nq)
     .                                 * lrsegWeight(nlrseg,ncLR)
                    end do
                  end do
                end do
              end do

            end if

          end do  ! end loop over lrsegs

        end do  ! end loop ps names 

      end if

******** DEAL WITH POINT SOURCE ONLY SEGMENTS

c          call getl2r(Trseg,rscen,lenrscen,
c     O                numsegs,l2r)
c          call getpsonlyl2r(
c     I                      Trseg,rscen,lenrscen,numsegs,l2r,
c     O                      npssegs,psl2r)
c            print*,'non physical rseg: ',npssegs,psl2r

****************** ALL DATA PROCESSED, NOW DO OUTPUTS
*********** Make Concentrations from Loads for some variables
      call ttyput(' making concentrations ')
      do nq = 1,nBvar  

        if (DivBvar(nq).eq.'    ') cycle  ! no division necessary

        call ttyput(Bname(nq))
        call ttyput(' ')
        found = .false.
        do Divnq = 1,nBvar  ! find divisor variable
          if (Bname(Divnq).eq.DivBvar(nq)) then
            found = .true.
            exit
          end if
        end do
        if (.not.found) go to 995

        do nc = 1,ncells  ! divide
          do ny = year1,year2
            do nd = 1,ndaysinyear(ny)
              if (wq(nd,ny,nc,Divnq).gt.0.000001) then
                wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)/wq(nd,ny,nc,Divnq)
              else
                wq(nd,ny,nc,nq) = 0.0
              end if
            end do
          end do
        end do
        do nc = 1,ncells
          do ny = year1,year2
            AnnualCell(ny,nc,nq) = AnnualCell(ny,nc,nq) 
     .                           / AnnualCell(ny,nc,Divnq)
          end do
        end do
        do ny = year1,year2
          AnnualTotal(ny,nq) = AnnualTotal(ny,nq)/AnnualTotal(ny,Divnq)
        end do
      end do
      print*,' '

********** write out subset
      do ny = year1,year2

        fnam = 'wsm57k_wsm_ps.YY'           ! open file
        write(fnam(13:16),'(i4)') ny
        fnam(13:14) = 's.'
        fnam = outdir//'wqm_input/'//Request(:lenRequest)//'/'//fnam
        open(dfile,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        print*,'writing file ',fnam(:50)

        year = ny
        month = 1
        day = 1
        do nd = 1,ndaysinyear(ny)
          do nc = 1,ncells
            write(dfile,1234)
     .            cell(nc),year,month,day,(wq(nd,ny,nc,nq),nq=1,nBvar)
          end do
          call tomorrow(year,month,day)
        end do
        close(dfile)
      end do

********* write out annual totals
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2
      fnam = outdir//'wqm_input/'//Request(:lenRequest)//
     .       '/annual_out_wqm57_ps_'//cy1//'_'//cy2//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      print*,'writing file ',fnam(:60)

      write(dfile,'(a4,22(a1,a4))')'year',(',',Bname(nq),nq=1,nBvar)
      do ny = year1,year2
        write(dfile,'(i4,22(a1,e10.3))')
     .               ny,(',',AnnualTotal(ny,nq),nq=1,nBvar)
      end do
      close(dfile)

********* write out annual totals by cell
      fnam = outdir//'wqm_input/'//Request(:lenRequest)//
     .       '/annual_out_wqm57_ps_'//cy1//'_'//cy2//'_by_cell.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      print*,'writing file ',fnam(:60)

      write(dfile,'(a9,22(a1,a4))')
     .        'year,cell',(',',Bname(nq),nq=1,nBvar)
      do nc = 1,ncells
        do ny = year1,year2
          write(dfile,'(i4,a1,i6,22(a1,e10.3))')
     .          ny,',',cell(nc),(',',AnnualCell(ny,nc,nq),nq=1,nBvar)
        end do
      end do
      close(dfile)

********* write out average annual by cell
      fnam = outdir//'wqm_input/'//Request(:lenRequest)//
     .       '/average_out_wqm57_ps_'//cy1//'_'//cy2//'_by_cell.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      print*,'writing file ',fnam(:60)

      write(dfile,'(a9,22(a1,a4))')
     .        'cell',(',',Bname(nq),nq=1,nBvar)
      do nc = 1,ncells
        do nq = 1,nBvar
          AverageCell(nq) = 0.0
          do ny = year1,year2
            AverageCell(nq) = AverageCell(nq) + AnnualCell(ny,nc,nq)
          end do
            AverageCell(nq) = AverageCell(nq)/real(year2-year1+1)
        end do
        write(dfile,'(i6,22(a1,e10.3))')
     .          cell(nc),(',',AverageCell(nq),nq=1,nBvar)
      end do
      close(dfile)

      stop
1234  format(i5,2(',',i4),18(',',e10.4))

********************************* ERROR SPACE **************************
981   report(1) = 'too many cells in file '
      report(2) = fnam
      write(report(3),*)'increase variable maxcells to greater than ',
     .                  maxcells
      go to 999

982   report(1) = 'too many lrsegs in file '
      report(2) = fnam
      write(report(3),*)'increase variable maxlrsegs to greater than ',
     .                  maxlrsegs
      go to 999

983   report(1) = 'too many lrsegs-cell linkages in file '
      report(2) = fnam
      write(report(3),*)'increase variable maxCellsPerLrseg ',
     .                  'to greater than ',maxCellsPerLrseg
      go to 999

984   report(1) = 'too many rsegs in file '
      report(2) = fnam
      write(report(3),*)'increase variable maxrsegs to greater than ',
     .                  maxrsegs
      go to 999

985   report(1) = 'too many rsegs-cell linkages in file '
      report(2) = fnam
      write(report(3),*)'increase variable maxCellsPerRseg ',
     .                  'to greater than ',maxCellsPerRseg
      go to 999

989   report(1) = 'error closing wdm file'
      report(2) = wdmfnam
      report(3) = ' '
      go to 999

991   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'Problem reading following file near line'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'Variable psmethod specifed as input must be:'
      report(2) = 'hcell, wcell, or lrseg'
      report(3) = ' for point sources by ch3d cell, wqm cell, or lrseg'
      go to 999

994   report(1) = 'Problem with linkage file:  Too many pairs on line:'
      report(2) = fnam
      report(3) = line
      go to 999

995   report(1) = 'Attempting to make concentrations'
      report(2) = 'can not find time series for variable '//DivBvar(nq)
      report(3) = 'requested for bay variable '//Bname(nq)
      go to 999

996   report(1) = 'should not reach this point in the program'
      report(2) = ' ps and nps are done separately '
      report(3) = ' '
      go to 999

998   if (err.lt.0) then
        report(1) = 'Error: opening wdm= '
        write(report(1)(22:24),'(i3)')err
        report(2) = wdmfnam
      else
        report(1) = wdmfnam
        report(2) = ' is not a wdm file'
      end if
      report(3) = ' '
      go to 999
 
999   call stopreport(report)

      end

