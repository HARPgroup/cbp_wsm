************************************************************************
**  program to generate the input file for the 57k Bay model          **
**    loops over all cells in water quality model                     **
**      loops over all inputs                                         **
**        creates one variable with all constituents                  **
**  This is the point source version                                  **
**    Major differences:  monthly, rather than daily output           **
**                        riverbay_ps rather than riverbay link file  **
************************************************************************

      implicit none

      include '../wqmlib/transfer_wdm.inc'

********* date variables
      integer year,month,day
      integer year1,year2,month1,month2,day1,day2
      parameter (month1=1,day1=1)
      parameter (month2=12,day2=31)
      character*4 cy1,cy2

      integer Y1,Y2
      parameter (Y1=1980,Y2=2025)

C BHATT      include 'date.inc'

********** wqm cell variables
      integer maxcells,ncells,nc
      parameter (maxcells=500)
      integer cell(maxcells)
      logical writecell(maxcells)
      character*6 ccell  ! character version of cell
      integer lencell
      integer maxSurfCells
      parameter (maxSurfCells=20000)

********* lrseg variables
      character*13 trseg
      character*6 tlseg

      integer npairs,np,maxpairs ! variables to read line
      parameter(maxpairs=10)
      real weight(maxpairs)
      integer fips(maxpairs), riverid(maxpairs)
      real pairweight  ! fraction of this LRseg or rseg going to cell
*********** output load variables
      real wq(366,Y1:Y2,maxcells,maxBvar)
      integer nd,ny,nq,Divnq
      real pairwq(366,Y1:Y2,maxBvar)
      double precision AnnualTotal(Y1:Y2,maxBvar)  ! print out annual totals
      double precision AnnualCell(Y1:Y2,maxcells,maxBvar)
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
      real eps  ! very small number
      parameter (eps = 0.00001)

      integer ndaysinyear
      external ndaysinyear

      integer julian,jday
      external julian

      logical found

      character*300 bigline  ! file reading variable

      integer Nexits  ! number of exits in this river
                      !  affects the river dsns to output

      integer lakeflag,resflag,timestep  ! variables to send to 
                             ! getflags routine, not used

      integer hotstart, lastcell

      integer wdmfil
      parameter (wdmfil = dfile + 9)

************ END DECLARATIONS ******************************************
********* stupid wdm thing
C BHATT Added new Inputs
      read*,rscen,psmethod,hotstart,year1,year2,rbfnam    ! get river scenario

      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil,wdmfnam,1,err)     ! open dummy read only
      if (err .ne. 0) go to 998             ! and never close


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

C BHATT      read*,rscen,psmethod,hotstart    ! get river scenario
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
C BHATT      rbfnam = 'river_to_wqm57kPS'
      call riverbay(
     I              ioscen,lenioscen,rbfnam,
     I              nRvar,Rname,
     O              nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,DivBvar)

********** get river information
      call getriver(
     I              geoscen,lengeoscen,
     O              rsegs,uniqindex,uniqid,dsid,nrsegs,fnam,err)

      tlseg(1:1) = 'A'
********** END OF SETUP
      if (dops.and.psmethod.eq.'lrseg') then

        go to 996
        fnam = catdir//'geo/'//geoscen(:lengeoscen)//'/'
        open(dfile-1,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read(dfile-1,'(a300)') bigline    ! get rid of header lines

        read(dfile-1,'(a300)',err = 992,end=992) bigline    
        call d2x(bigline,last)


****************** loop over cells in file
        nc = 0
        do while (bigline(:3).ne.'end')
          nc = nc + 1  ! increment number of cells
          read(bigline,*,err=992,end=992)
     .     cell(nc),npairs,(weight(np),fips(np),riverid(np),np=1,npairs)
          if (npairs.gt.maxpairs) go to 994
  
*********************** loop over Land-river pairs in cell
          do np = 1,npairs

            trseg = rsegs(uniqindex(riverid(np)))  ! get river name

            if (fips(np).ne.0) then  ! not river may be point sources

              print*,'   ' 
              call ttyput(trseg)
              call ttyput(' ')
              write(tlseg(2:6),'(i5)')fips(np)  ! convert to character
              call ttyput(tlseg)

              call ttyput(' ps')
              wdmfnam = ScenDatDir//'river/ps/'//
     .                  psscen(:lenpsscen)//'/ps_'//
     .                    tlseg//'_to_'//trseg//'.wdm'
              call readdailydat(
     I                          year1,month1,day1,year2,month2,day2,
     I                          nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                          wdmfnam,nRvar,Rname,Rdsn,
     I                          nPSvar,PSdsn,PSname,PSfac,
     O                          pairwq)
              do nq = 1,nBvar  ! add to big storage variables
                do ny = year1,year2
                  do nd = 1,ndaysinyear(ny)
                    wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)
     .                     + pairwq(nd,ny-(year1-1980),nq) * weight(np)
                    AnnualTotal(ny,nq) = AnnualTotal(ny,nq)
     .                     + pairwq(nd,ny-(year1-1980),nq) * weight(np)
                    AnnualCell(ny,nc,nq) = AnnualCell(ny,nc,nq)
     .                     + pairwq(nd,ny-(year1-1980),nq) * weight(np)
                  end do
                end do
              end do
            end if

            print*,'   ' 


          end do  ! end loop over pairs

        end do  ! loop over lines in file

        ncells = nc
        close(dfile-1)
      end if  ! end if method = lrseg

************ method:  loop over all possible cell names 
*********** and look for existing point source files
      if (dops.and.psmethod.eq.'hcell') go to 997

      if (dops.and.psmethod.eq.'wcell') then
        ncells = 0
        do nc = 1,maxSurfCells
          write(ccell,'(i6)') nc
          call trims(ccell,lencell)
          wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .              '/bay_models/ps_wqm57k_'//ccell(:lencell)//'.wdm'
          
          call wdbopnlong(dfile+1,wdmfnam,1,err)     ! open land read only
          if (err .ne. 0) cycle  ! cell not loaded
          call wdflcl (dfile+1,err)
          if (err.ne.0) go to 989

          ncells = ncells + 1
          if (ncells.gt.maxcells) go to 988
          cell(ncells) = nc

          call readdailydat(
     I                      year1,month1,day1,year2,month2,day2,
     I                      nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                      wdmfnam,nRvar,Rname,Rdsn,
     I                      nPSvar,PSdsn,PSname,PSfac,
     O                      pairwq)
          do nq = 1,nBvar  ! add to big storage variables
            do ny = year1,year2
              do nd = 1,ndaysinyear(ny)
                wq(nd,ny,ncells,nq) = wq(nd,ny,ncells,nq)
     .                              + pairwq(nd,ny-(year1-1980),nq) 
                AnnualTotal(ny,nq) = AnnualTotal(ny,nq)
     .                              + pairwq(nd,ny-(year1-1980),nq)
                AnnualCell(ny,ncells,nq) = AnnualCell(ny,ncells,nq)
     .                              + pairwq(nd,ny-(year1-1980),nq)
              end do
            end do
          end do

        end do
          
      end if


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
        do ny = year1,year2
          AnnualTotal(ny,nq) = AnnualTotal(ny,nq)/AnnualTotal(ny,Divnq)
        end do
        do nc = 1,ncells
          do ny = year1,year2
            AnnualCell(ny,nc,nq) = AnnualCell(ny,nc,nq)
     .                           / AnnualCell(ny,nc,Divnq)
          end do
        end do
      end do
      print*,' '

******************** got all data, now write to file

C********** get subset of cells to write
C      do nc = 1,ncells
C        writecell(nc) = .false.
C        do ny = year1,year2
C          do nd = 1,ndaysinyear(ny)
C            do nq = 1,nBvar
C              if (wq(nd,ny,nc,nq).gt.eps) then
C                writecell(nc) = .true.
C                exit
C              end if
C            end do
C            if (writecell(nc)) exit
C          end do
C          if (writecell(nc)) exit
C        end do
C      end do

********** write out subset
      do ny = year1,year2

        fnam = 'wsm57k_wsm_ps.YY'           ! open file
        write(fnam(13:16),'(i4)') ny
        fnam(13:14) = 's.'
        fnam = outdir//'wqm_input/'//rscen(:lenrscen)//'/'//fnam
        open(dfile,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        print*,'writing file ',fnam(:50)

        year = ny
        month = 1
        day = 1
        do nd = 1,ndaysinyear(ny)
          if (day.eq.1) then
            do nc = 1,ncells
C              if (writecell(nc)) then
                write(dfile,1234)
     .                cell(nc),year,month,(wq(nd,ny,nc,nq),nq=1,nBvar)
C              end if
            end do
          end if
          call tomorrow(year,month,day)
        end do
        close(dfile)
      end do

********* write out annual totals
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2
      fnam = outdir//'wqm_input/'//rscen(:lenrscen)//
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
      fnam = outdir//'wqm_input/'//rscen(:lenrscen)//
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
      fnam = outdir//'wqm_input/'//rscen(:lenrscen)//
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
988   report(1) = 'number of cells with point sources greater than'
      report(2) = 'allowable number, increase maxcells'
      report(3) = ' '
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
      report(3) = bigline(:64)
      go to 999

993   report(1) = 'Variable psmethod specifed as input must be:'
      report(2) = 'hcell, wcell, or lrseg'
      report(3) = ' for point sources by ch3d cell, wqm cell, or lrseg'
      go to 999

994   report(1) = 'Problem with linkage file:  Too many pairs on line:'
      report(2) = fnam
      report(3) = bigline(:64)
      go to 999

995   report(1) = 'Attempting to make concentrations'
      report(2) = 'can not find time series for variable '//DivBvar(nq)
      report(3) = 'requested for bay variable '//Bname(nq)
      go to 999

996   report(1) = 'the lrseg option is not availble without a rewrite'
      report(2) = ' of the code.'
      report(3) = ' '
      go to 999

997   report(1) = 'the hcell option is not availble without a rewrite'
      report(2) = ' of the code.'
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

