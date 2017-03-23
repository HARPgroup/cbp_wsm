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
      integer year1,year2
      character*4 cy1,cy2

***************** variables to read modification file
      integer nmodcellsmax,nmodcells,nmodcell
      parameter (nmodcellsmax = 3000)
      integer modcells(nmodcellsmax)
      real cellfactor(nmodcellsmax,maxBvar)
      integer ncols
      integer nBvarForColumn(maxBvar*2) ! can be extraneous columns
      character*4 TBname

********* wq variable
      real wq(maxBvar)

      integer ny,nq
      double precision AnnualTotal(EarliestYear:LatestYear,maxBvar)

      character*40 modscen  ! specifications on how to modify
      integer lenmodscen    ! the scenario points to a directory in
                            ! ./input/sceanrio/wqm/WqmFactors/

************* utility variables
      character*300 longline

      integer cell,nc

      logical found

************ END DECLARATIONS ******************************************
********** initialize
      do nq = 1,maxBvar
        do year = EarliestYear,LatestYear
          AnnualTotal(year,nq) = 0.0
        end do
      end do

*********** get inputs
      read*,rscen,modscen,year1,year2    ! get river scenario

      call lencl(rscen,lenrscen)
      call lencl(modscen,lenmodscen)

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
      rbfnam = 'river_to_wqm57kNPS'
      call riverbay(
     I              ioscen,lenioscen,rbfnam,
     I              nRvar,Rname,
     O              nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,DivBvar)

************ populate cell mod data
      fnam= ScenDatDir//'wqm/WqmFactors/'//modscen(:lenmodscen)//
     .      '/wqmfactors.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      
********* find the bvar number for each column
      read(dfile,'(a300)',err=992,end=992) longline
      call d2x(longline,last)
      if (longline(300-3:300).ne.'    ') go to 993
      call shift(longline)  ! get rid of first column
      ncols = 0
      do ! parse line
        if (longline(:10).eq.'          ') exit
        read(longline,*,err=992,end=992) TBname
        ncols = ncols + 1
        nBvarForColumn(ncols) = 0
        do nq = 1,nBvar
          if (TBname.eq.Bname(nq)) nBvarForColumn(ncols) = nq
        end do
        call shift(longline)
      end do

************ store the data by cell
      nmodcells = 0
      do
        read(dfile,'(a300)',err=992,end=111) longline     ! read a line
        call d2x(longline,last)
        if (longline(300-3:300).ne.'    ') go to 993

        nmodcells = nmodcells + 1
        if (nmodcells.gt.nmodcellsmax) go to 994
        read(longline,*,err=992,end=992) modcells(nmodcells),
     .        (cellfactor(nmodcells,nBvarForColumn(nc)),nc=1,ncols)
      end do
111   close(dfile)

******************** read from file and write to another
      do ny = year1,year2

************* open both files
        fnam = 'wsm57k_wsm_nps.YY'           ! open read file
        write(fnam(14:17),'(i4)') ny
        fnam(14:15) = 's.'
        fnam = outdir//'wqm_input/'//rscen(:lenrscen)
     .                             //'/'//fnam
        open(dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991
        print*,'reading file ',fnam(:80)

        fnam = 'wsm57k_wsm_nps.YY'           ! open write file
        write(fnam(14:17),'(i4)') ny
        fnam(14:15) = 's.'
        fnam = outdir//'wqm_input/'//rscen(:lenrscen)//'_'//
     .         modscen(:lenmodscen)//'/'//fnam
        open(dfile+1,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        print*,'writing file ',fnam(:80)

************ loop over all cells and day
*********** read, modify, and write
        do 
          read(dfile,*,err=995,end=222)
     .            cell,year,month,day,(wq(nq),nq=1,nBvar)

          found = .false.
          do nmodcell = 1,nmodcells
            if (cell.eq.modcells(nmodcell)) then
              found = .true.
              do nq = 1,nBvar
                wq(nq) = wq(nq) * cellfactor(nmodcell,nq) 
                AnnualTotal(year,nq)=AnnualTotal(year,nq) + wq(nq)
             end do
              exit
            end if
          end do
          if (.not.found) go to 997

          write(dfile+1,1234)
     .            cell,year,month,day,(wq(nq),nq=1,nBvar)
        end do
222     close(dfile)
        close(dfile+1)
      end do

********* write out annual totals
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2
      fnam = outdir//'wqm_input/'//rscen(:lenrscen)//'_'//
     .       modscen(:lenmodscen)//
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
      
      stop
1234  format(i5,3(',',i4),18(',',e10.4))

********************************* ERROR SPACE **************************
991   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'Problem reading following file near line'
      report(2) = fnam
      report(3) = longline
      go to 999

993   report(1) = 'Problem reading following file:  line too long:'
      report(2) = fnam
      report(3) = longline
      go to 999

994   report(1) = 'Problem with linkage file:  Too many entries'
      report(2) = fnam
      report(3) = 'increase nmodcellsmax'
      go to 999

995   report(1) = 'Problem reading following file near line'
      report(2) = fnam
      write(report(3),*) cell,',',year,',',month,',',day
      go to 999

997   write(report(1),*) 'cell ',cell
      report(2) = 'not found in wqmfactor file'
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

