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

      character*5 psmethod ! must be either 'hcell, wcell, or lrseg'

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

      read*,rscen,psmethod,hotstart    ! get river scenario
      call lencl(rscen,lenrscen)
      if (.not.(psmethod.eq.'hcell'.or.psmethod.eq.'wcell'.or.
     .          psmethod.eq.'lrseg')) go to 993

******** READ THE CONTROL FILE FOR LAND SCENARIOS
      call readcontrol_lscen(rscen,
     .                       LandScen)

********* READ THE CONTROL FILE FOR DATA SCENARIOS
      call readcontrol_wdm(rscen,lenrscen,
     O                     pradscen,psscen,sepscen,
     O                     doatdep,dops,dosep)
      call lencl(psscen,lenpsscen)
      call lencl(sepscen,lensepscen)
      call lencl(pradscen,lenpradscen)
      dops = .false.  ! done in separate program

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

      if (dosep) then
        call getvars(
     I               ioscen,lenioscen,
     I               nRvar,Rname,sep,
     O               nSEPvar,SEPdsn,SEPname,SEPfac)
      end if

      if (doatdep) then
        call getvars(
     I               ioscen,lenioscen,
     I               nRvar,Rname,atdep,
     O               nATDEPvar,ATDEPdsn,ATDEPname,ATDEPfac)
      end if

************** POPULATE variables relating river vars to bay vars
      rbfnam = 'river_to_wqm57kNPS'
      call riverbay(
     I              ioscen,lenioscen,rbfnam,
     I              nRvar,Rname,
     O              nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,DivBvar)

********** END OF SETUP, OPEN AND READ LRSEG LINK FILE
      fnam = catdir//'geo/'//geoscen(:lengeoscen)//
     .       '/p5_to_57k_wqm_lrsegs.csv'
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

********** OPEN AND READ RSEG LINK FILE
      fnam = catdir//'geo/'//geoscen(:lengeoscen)//
     .       '/p5_to_57k_wqm_rsegs.csv'
      open(dfile-1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      print*,'reading ',fnam

      nrsegs = 0  ! initialize variables
      do nrseg = 1,maxrsegs
        CellsPerRseg(nrseg) = 0
      end do

      do
        read(dfile-1,'(a100)',end=222,err=992) line
        call d2x(line,last)
        if (comment(line)) cycle
        read(line,*,end=992,err=992) Tcell,Trseg,Tweight

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

        found = .false.  ! find rseg index
        do nrseg = 1,nrsegs
          if (rsegs(nrseg).eq.Trseg) then
            found = .true.
            exit     ! nrseg preserved
          end if
        end do
        if (.not.found) then
          nrsegs = nrsegs + 1
          if (nrsegs.gt.maxrsegs) go to 984
          nrseg = nrsegs
          rsegs(nrseg) = Trseg
        end if

        CellsPerRseg(nrseg) = CellsPerRseg(nrseg) + 1
        if (CellsPerRseg(nrseg).gt.maxCellsPerRseg) go to 985
        rsegCellIndex(nrseg,CellsPerRseg(nrseg)) = nc
        rsegWeight(nrseg,CellsPerRseg(nrseg)) = Tweight

      end do
222   close(dfile-1)

************ CHECK FOR HOT START FILE
      if (hotstart.eq.2) then
        print*,'getting output for hotstart'
        fnam = outdir//'wqm_input/'//rscen(:lenrscen)//'/'//
     .         'hotstart_wqm57_nps.bin'
        open(hotfile,file=fnam,form='unformatted',
     .       status='unknown',iostat=err)
        if (err.ne.0) go to 991
        read(hotfile)wq,AnnualCell,AnnualTotal,lastlrseg
        close(hotfile)
      end if

****************** loop over all lrsegs
************** read the EOS, atdep, and septic.
      do nlrseg = 1,nlrsegs

        Tlseg = lrseg(nlrseg)(:6)
        Trseg = lrseg(nlrseg)(7:)
        print*,'Processing LRseg ',nlrseg,' of ',nlrsegs,' LRsegs ',
     .          Tlseg,' ',Trseg

        if (hotstart.eq.2) then
          if (lrseg(nlrseg).eq.lastlrseg) then
            hotstart = 1
          end if
          cycle
        end if

*********************** loop over cells in Lrseg
        call readpair(rscen,Tlseg,Trseg,
     I                ioscen,lenioscen,
     I                year1,month1,day1,year2,month2,day2,
     I                nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                nRvar,Rname,RvarBMP,
     I                nLvar,Ldsn,Lname,Lfactor,LandScen,
     O                pairwq)  ! get the data
        do ncLR = 1,CellsPerLrseg(nlrseg)
          nc = lrsegCellIndex(nlrseg,ncLR)
          do nq = 1,nBvar  ! add to big storage variables
            do ny = year1,year2
              do nd = 1,ndaysinyear(ny)
                wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq) 
     .                          + pairwq(nd,ny,nq) 
     .                          * lrsegWeight(nlrseg,ncLR)
                AnnualTotal(ny,nq) = AnnualTotal(ny,nq)
     .                             + pairwq(nd,ny,nq)
     .                             * lrsegWeight(nlrseg,ncLR)
                AnnualCell(ny,nc,nq) = AnnualCell(ny,nc,nq)
     .                               + pairwq(nd,ny,nq)
     .                               * lrsegWeight(nlrseg,ncLR)
              end do
            end do
          end do
        end do

        if (dosep) then
          write(*,'(" septic",$)')
          wdmfnam = ScenDatDir//'river/septic/'//
     .              sepscen(:lensepscen)//
     .              '/septic_'//Tlseg//'_to_'//Trseg//'.wdm'
          call readdailydat(
     I                      year1,month1,day1,year2,month2,day2,
     I                      nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                      wdmfnam,nRvar,Rname,Rdsn,
     I                      nSEPvar,SEPdsn,SEPname,SEPfac,
     O                      pairwq)
          do ncLR = 1,CellsPerLrseg(nlrseg)
            nc = lrsegCellIndex(nlrseg,ncLR)
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

        if (dops.and.psmethod.eq.'lrseg') then
          go to 996
          write(*,'(" ps",$)')
          wdmfnam = ScenDatDir//'river/ps/'//
     .              psscen(:lenpsscen)//'/ps_'//
     .                Tlseg//'_to_'//Trseg//'.wdm'
          call readdailydat(
     I                      year1,month1,day1,year2,month2,day2,
     I                      nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                      wdmfnam,nRvar,Rname,Rdsn,
     I                      nPSvar,PSdsn,PSname,PSfac,
     O                      pairwq)
          do ncLR = 1,CellsPerLrseg(nlrseg)
            nc = lrsegCellIndex(nlrseg,ncLR)
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

        if (doatdep) then
          write(*,'(" atdep",$)')
          wdmfnam = ScenDatDir//'climate/prad/'//
     .              pradscen(:lenpradscen)//
     .              '/prad_'//Tlseg//'.wdm'

          call getwateracres(
     I                       rscen,lenrscen,Trseg,Tlseg,
     I                       year1,month1,day1,
     I                       year2,month2,day2,
     O                       wateracres)
          call readdailydat(
     I                      year1,month1,day1,year2,month2,day2,
     I                      nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                      wdmfnam,nRvar,Rname,Rdsn,
     I                      nATDEPvar,ATDEPdsn,ATDEPname,ATDEPfac,
     O                      pairwq)
          do ncLR = 1,CellsPerLrseg(nlrseg)
            nc = lrsegCellIndex(nlrseg,ncLR)
            do nq = 1,nBvar  ! add to big storage variables
              jday = 1
              do ny = year1,year2
                do nd = 1,ndaysinyear(ny)
                  wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)
     .                            + pairwq(nd,ny,nq)
     .                            * lrsegWeight(nlrseg,ncLR)
     .                            * wateracres(jday)
                  AnnualTotal(ny,nq) = AnnualTotal(ny,nq)
     .                               + pairwq(nd,ny,nq)
     .                               * lrsegWeight(nlrseg,ncLR)
     .                               * wateracres(jday)
                  AnnualCell(ny,nc,nq) = AnnualCell(ny,nc,nq)
     .                                 + pairwq(nd,ny,nq)
     .                                 * lrsegWeight(nlrseg,ncLR)
     .                                 * wateracres(jday)
                  jday = jday + 1
                end do
              end do
            end do
          end do
        end if
        print*,'   ' 


        if (hotstart.eq.1) then !   store output for hot start
          print*,'storing output for hotstart'
          fnam = outdir//'wqm_input/'//rscen(:lenrscen)//'/'//
     .           'hotstart_wqm57_nps.bin'
          open(hotfile,file=fnam,form='unformatted',
     .         status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(hotfile)wq,AnnualCell,AnnualTotal,lrseg(nlrseg)
          close(hotfile)
        end if

      end do  ! end loop over lrsegs

****************** loop over all rsegs
*********** read just the river output
      do nrseg = 1,nrsegs

        Trseg = rsegs(nrseg)
        print*,'Processing Rseg ',Trseg

        call getrflags(
     I                 paramscen,lenparamscen,Trseg,
     O                 Nexits,lakeflag,resflag,timestep)

        call Rmasslink(
     I                 ioscen,lenioscen,
     I                 Nexits,modules,nmod,
     O                 nRvarOut,RdsnOut,RnameOut)

        call readriver(
     I                 rscen,Trseg,
     I                 year1,month1,day1,year2,month2,day2,
     I                 nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                 nRvarOut,RdsnOut,RnameOut,
     O                 pairwq) ! get the data
        do ncR = 1,CellsPerRseg(nrseg)
          nc = rsegCellIndex(nrseg,ncR)
          do nq = 1,nBvar  ! add to big storage variables
            do ny = year1,year2
              do nd = 1,ndaysinyear(ny)
                wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)
     .                          + pairwq(nd,ny,nq) 
     .                          * rsegWeight(nrseg,ncR)
                AnnualTotal(ny,nq) = AnnualTotal(ny,nq)
     .                             + pairwq(nd,ny,nq) 
     .                             * rsegWeight(nrseg,ncR)
                AnnualCell(ny,nc,nq) = AnnualCell(ny,nc,nq)
     .                               + pairwq(nd,ny,nq) 
     .                               * rsegWeight(nrseg,ncR)
              end do
            end do
          end do
        end do
      end do


************* old code for running PS if not in separate file
      if (dops.and.psmethod.ne.'lrseg') then
        go to 996
        write(*,'(" ps",$)')
        write(ccell,'(i6)') cell(nc)
        call trims(ccell,lencell)
        if (psmethod.eq.'hcell') then
          wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .              '/bay_models/ps_ch3d57k_'//ccell(:lencell)//'.wdm'
        else if (psmethod.eq.'wcell') then
          wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .              '/bay_models/ps_wqm57k_'//ccell(:lencell)//'.wdm'
        end if
        call readdailydat(
     I                    year1,month1,day1,year2,month2,day2,
     I                    nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                    wdmfnam,nRvar,Rname,Rdsn,
     I                    nPSvar,PSdsn,PSname,PSfac,
     O                    pairwq)
        do nq = 1,nBvar  ! add to big storage variables
          do ny = year1,year2
            do nd = 1,ndaysinyear(ny)
              wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)+ pairwq(nd,ny,nq) 
              AnnualTotal(ny,nq) = AnnualTotal(ny,nq)+pairwq(nd,ny,nq)
              AnnualCell(ny,nc,nq) = AnnualCell(ny,nc,nq)
     .                             + pairwq(nd,ny,nq)
            end do
          end do
        end do
      end if
          

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

******************** got all data, now write to file
      do ny = year1,year2

        fnam = 'wsm57k_wsm_nps.YY'           ! open file
        write(fnam(14:17),'(i4)') ny
        fnam(14:15) = 's.'
        fnam = outdir//'wqm_input/'//rscen(:lenrscen)//'/'//fnam
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
      fnam = outdir//'wqm_input/'//rscen(:lenrscen)//
     .       '/annual_out_wqm57_nps_'//cy1//'_'//cy2//'.csv'
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
      fnam = outdir//'wqm_input/'//rscen(:lenrscen)//
     .       '/annual_out_wqm57_nps_'//cy1//'_'//cy2//'_by_cell.csv'
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
      fnam = outdir//'wqm_input/'//rscen(:lenrscen)//
     .       '/average_out_wqm57_nps_'//cy1//'_'//cy2//'_by_cell.csv'
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
1234  format(i5,3(',',i4),18(',',e10.4))

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

