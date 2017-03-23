************************************************************************
**  program to generate the input file for the 57k Bay model          **
**    loops over all cells in water quality model                     **
**      loops over all inputs                                         **
**        creates one variable with all constituents                  **
************************************************************************

      implicit none

      include '../wqmlib/wqm_load.inc'

********* date variables
      integer year,month
      integer year1,year2
      character*4 cy1,cy2

***************** variables to read modification file
      integer nmodcellsmax,nmodcells,nmodcell
      parameter (nmodcellsmax = 3000)
      integer modcells(nmodcellsmax)
      integer scenid(nmodcellsmax,maxBvar)
      integer ncols
      integer nBvarForColumn(maxBvar*2) ! can be extraneous columns
      character*4 TBname

********* wq variable
      real wq(nmodcellsmax,12,maxBvar),Twq(maxBvar)
      real wq1(nmodcellsmax,12,maxBvar),wq2(nmodcellsmax,12,maxBvar)
      real sumcell(nmodcellsmax) 

      integer ny,nm,nq
      double precision AnnualTotal(EarliestYear:LatestYear,maxBvar)

      character*40 wqmscen1,wqmscen2     ! scenarios combined to make a new one
      integer lenwqmscen1,lenwqmscen2    ! specificatio of the scenarios in ./input/sceanrio/wqm/Combine2Scens/

************* utility variables
      character*300 longline
      character*300 dfnam

      integer cell,nc

      logical found

      integer ndaysinmonth
      external ndaysinmonth

************ END DECLARATIONS ******************************************
********** initialize
      do nc = 1,nmodcellsmax
        do nm = 1, 12
          do nq = 1,maxBvar
            wq(nc,nm,nq) = 0.0
            wq1(nc,nm,nq) = 0.0
            wq2(nc,nm,nq) = 0.0
          end do
        end do
      end do

      do nq = 1,maxBvar
        do year = EarliestYear,LatestYear
          AnnualTotal(year,nq) = 0.0
        end do
      end do

*********** get inputs
      read*,rscen,wqmscen1,wqmscen2,year1,year2    ! get river scenario

      call lencl(rscen,lenrscen)
      call lencl(wqmscen1,lenwqmscen1)
      call lencl(wqmscen2,lenwqmscen2)

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
      rbfnam = 'river_to_wqm57kPS'
      call riverbay(
     I              ioscen,lenioscen,rbfnam,
     I              nRvar,Rname,
     O              nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,DivBvar)

************ populate cell mod data
      dfnam= ScenDatDir//'wqm/Com2Scens/'//rscen(:lenrscen)//
     .      '/cellscens.csv'
      open(dfile,file=dfnam,status='old',iostat=err)
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
     .        (scenid(nmodcells,nBvarForColumn(nc)),nc=1,ncols)
      end do
111   close(dfile)

******************** read from file and write to another
      do ny = year1,year2

************* open first file to read
        fnam = 'wsm57k_wsm_ps.YY'          
        write(fnam(13:16),'(i4)') ny
        fnam(13:14) = 's.'
        fnam = outdir//'wqm_input/'//wqmscen1(:lenwqmscen1)//'/'//fnam
        open(dfile+1,file=fnam,status='old',iostat=err)
        print*, fnam
        if (err.ne.0) go to 991
        print*,'reading file ',fnam(:80)
       
        do
          read(dfile+1,*,err=995,end=222)
     .            cell,year,month,(Twq(nq),nq=1,nBvar)

          found = .false.
          do nmodcell = 1,nmodcells
            if (modcells(nmodcell).eq.cell) then
              found = .true.
              do nq  = 1,nBvar
                wq1(nmodcell,month,nq)= Twq(nq)
              end do
              exit
            end if
          end do

          if (.not.found) go to 997
 
        end do    ! END LOOP ALL RECORDS

222     close(dfile+1)
       
************* open second file to read
        fnam = 'wsm57k_wsm_ps.YY'           
        write(fnam(13:16),'(i4)') ny
        fnam(13:14) = 's.'
        fnam = outdir//'wqm_input/'//wqmscen2(:lenwqmscen2)//'/'//fnam
        open(dfile+2,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991
        print*,'writing file ',fnam(:80)

        do
          read(dfile+2,*,err=995,end=333)
     .            cell,year,month,(Twq(nq),nq=1,nBvar)

          found = .false.
          do nmodcell = 1,nmodcells
            if (modcells(nmodcell).eq.cell) then
              found = .true.
              do nq  = 1,nBvar
                wq2(nmodcell,month,nq)= Twq(nq)
              end do
              exit
            end if
          end do

          if (.not.found) go to 997

        end do    ! END LOOP ALL RECORDS

333     close(dfile+2)
        
************* open write file
        fnam = 'wsm57k_wsm_ps.YY'           ! open write file
        write(fnam(13:16),'(i4)') ny
        fnam(13:14) = 's.'
        fnam = outdir//'wqm_input/'//rscen(:lenrscen)//'/'//fnam
        open(dfile,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        print*,'writing file ',fnam(:80)

************ loop over all cells and month to determine and write
        do nmodcell = 1,nmodcells
          sumcell(nmodcell) = 0.0
          do nq = 1,nBvar
            do nm = 1, 12
              if (scenid(nmodcell,nq) .eq. 1) 
     .             wq(nmodcell,nm,nq) = wq1(nmodcell,nm,nq)  ! use scenario ID to determine which scanrio
              if (scenid(nmodcell,nq) .eq. 2) 
     .             wq(nmodcell,nm,nq) = wq2(nmodcell,nm,nq)  
      
              AnnualTotal(ny,nq) = AnnualTotal(ny,nq)
     .                           + wq(nmodcell,nm,nq)
     .                           *real(ndaysinmonth(ny,nm))
              sumcell(nmodcell)= sumcell(nmodcell)+wq(nmodcell,nm,nq) 
            end do
          end do      ! end loop over all constituents
        end do        ! end loop over all cells
         
********** write to a new file
        do nm = 1,12
          do nmodcell = 1,nmodcells         ! Don't write cells w/o loading
            if (sumcell(nmodcell) .eq. 0.0) cycle
            write(dfile,1234) modcells(nmodcell),ny,nm,
     .                       (wq(nmodcell,nm,nq),nq=1,nBvar)
          end do       
        end do        ! end loop over all months

        close(dfile)
      end do         ! end loop all years

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
      
      stop

1234  format(i5,2(',',i4),18(',',e10.4))

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
      write(report(3),*) cell,',',year,',',month
      go to 999

997   write(report(1),*) 'cell ',cell
      report(2) = 'not found in cellscen file'
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

