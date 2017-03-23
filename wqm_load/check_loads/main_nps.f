************************************************************************
**  program to generate the input file for the 57k Bay model          **
**    loops over all cells in water quality model                     **
**      loops over all inputs                                         **
**        creates one variable with all constituents                  **
************************************************************************

      implicit none

      include '../wqmlib/transfer_wdm.inc'

********* date variables
      integer year,month,day
      integer iyear,imonth,iday
      integer year1,year2,month1,month2,day1,day2
      parameter (month1=1,day1=1)
      parameter (month2=12,day2=31)
      character*4 cy1,cy2

********** wqm cell variables
      integer maxcells,ncells,nc
      parameter (maxcells=2400)
      integer cell(maxcells),icell

*********** output load variables
      double precision wq(maxcells,maxBvar)
      double precision Twq(maxBvar)  ! print out annual totals
      real denom
      integer nd,ny,nq

************* utility variables
      integer ndaysinyear
      external ndaysinyear

      character*300 bigline  ! file reading variable

      character*25 wqmscen
      integer lenwqmscen

************ END DECLARATIONS ******************************************
      read*,wqmscen,rscen,year1,year2
      call lencl(rscen,lenrscen)
      call lencl(wqmscen,lenwqmscen)

********** initialize
      do nq = 1,maxBvar ! initialize
        do nc = 1,maxcells
          wq(nc,nq) = 0.0
        end do
      end do

********* read control file for I/O, geo, and param scenario
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call readcontrol_Rgeoscen(
     I                          rscen,lenrscen,
     O                          geoscen)
      call lencl(ioscen,lenioscen)
      call lencl(geoscen,lengeoscen)

******* POPULATE nRv2Bv, Rname2Bv, Rfactor, nRvar, nBvar, Bname,
********         Rdsn, nLvar, Ldsn, Lname, Lfactor
      call readcontrol_modules(
     I                         rscen,lenrscen,
     O                         modules,nmod)
      call masslink(
     I              ioscen,lenioscen,modules,nmod,
     O              nRvar,Rdsn,Rname,RvarBMP,
     O              nLvar,Ldsn,Lname,Lfactor)

************** POPULATE variables relating river vars to bay vars
      rbfnam = 'river_to_wqm57kNPS'
      call riverbay(
     I              ioscen,lenioscen,rbfnam,
     I              nRvar,Rname,
     O              nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,DivBvar)

********** END OF SETUP, OPEN FILE AND LOOP OVER CELLS
      fnam = catdir//'geo/'//geoscen(:lengeoscen)//'/p5_to_57k_wqm.prn'
      open(dfile-1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile-1,'(a300)') bigline    ! get rid of header lines

      read(dfile-1,'(a300)',err = 992,end=992) bigline 
      call d2x(bigline,last)   

****************** loop over cells in file
      nc = 0
      do while (bigline(:3).ne.'end')
        nc = nc + 1  ! increment number of cells
        read(bigline,*,err=992,end=992) cell(nc)
        read(dfile-1,'(a300)',err = 992,end=992) bigline    
        call d2x(bigline,last)   

      end do      ! end loop over cells
      close(dfile-1)

********** store total number of cells
      ncells = nc

******************** read in data
      do ny = year1,year2

        fnam = 'wsm57k_wsm_nps.YY'           ! open file
        write(fnam(14:17),'(i4)') ny
        fnam(14:15) = 's.'
        fnam = outdir//'wqm_input/'//wqmscen(:lenwqmscen)//'/'//fnam
        open(dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991
        print*,'reading file ',fnam(:50)

        year = ny
        month = 1
        day = 1
        do nd = 1,ndaysinyear(ny)
          do nc = 1,ncells
            read(dfile,1234)
     .            icell,iyear,imonth,iday,(Twq(nq),nq=1,nBvar)
            if (icell.ne.cell(nc).or.iyear.ne.year.or.
     .          imonth.ne.month.or.iday.ne.day) go to 993
            do nq = 1,nBvar
              wq(nc,nq) = wq(nc,nq) + Twq(nq)
            end do
          end do
          call tomorrow(year,month,day)
        end do
        close(dfile)
      end do

********* write out segment-wise totals
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2
      fnam = outdir//'wqm_input/'//wqmscen(:lenwqmscen)//
     .       '/average_out_wqm57_nps_by_cell_'//cy1//'_'//cy2//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      print*,'writing file ',fnam(:100)

      write(dfile,'(a4,22(a1,a4))')'cell',(',',Bname(nq),nq=1,nBvar)

      denom = real(year2-year1+1)
      do nc = 1,ncells
        write(dfile,'(i5,22(a1,e10.3))')
     .               cell(nc),(',',wq(nc,nq)/denom,nq=1,nBvar)
      end do

      do nq = 1,nBvar
        do nc = 2,ncells
          wq(1,nq) = wq(1,nq) + wq(nc,nq)
        end do
      end do
      write(dfile,'(a5,22(a1,e14.7))')
     .               'total',(',',wq(1,nq)/denom,nq=1,nBvar)

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
      report(3) = bigline(:64)
      go to 999

993   report(1) = 'problem reading wqm intput file, wrong order'
      report(2) = fnam
      report(3) = bigline(:64)
      go to 999

999   call stopreport(report)

      end

