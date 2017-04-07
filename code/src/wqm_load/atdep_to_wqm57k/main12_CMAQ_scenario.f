************************************************************************
**  generate the atmospheric input file for the 57k cell Bay model    **
**    loops over all cells in water quality model                     **
**      loops over all inputs                                         **
**        creates one variable with all constituents                  **
**                                                                    **
**  modified for a scenario based on CMAQ                             **
**   CMAQ scenarios are presented as a percent reduction from the     **
**    emissionyear base case                                          **
**   Similarly to the back-casting of dry deposition, the trend in    **
**     wet is used to center the deposition on the base case          **
**     then a reduction is taken using the CMAQ scenario file         **
************************************************************************
      implicit none
      include 'inc_atdep.f'

      integer ncells,nc
      parameter (ncells=11064)
C      parameter (ncells=1)

      real wq(366,year1:year2,ncells,nBvar)

      character*10 Cyears

************ END DECLARATIONS ******************************************
      read*,atdepscen,linkdir,
     .      CMAQbase,CbaseDir2,CbaseSuperLong,
     .      CMAQscen,CscenDir2,CscenSuperLong
      call lencl(atdepscen,lenscen)
      call lencl(linkdir,lenlink)
      call lencl(CMAQbase,lenCbase)
      call lencl(Cbasedir2,lenCb2)
      call lencl(CbaseSuperLong,lenCbLong)
      call lencl(CMAQscen,lenCscen)
      call lencl(Cscendir2,lenCs2)
      call lencl(CscenSuperLong,lenCsLong)

      do nq = 1,nBvar ! initialize
        do nc = 1,ncells
          do ny = year1,year2
            do nd = 1,366
              wq(nd,ny,nc,nq) = 0.0
            end do
          end do
        end do
      end do

********* open summary file and write headers
      Cyears = '_yyyy_yyyy'
      write(Cyears(2:5),'(i4)') year1
      write(Cyears(7:10),'(i4)') year2
      fnam = outdir//'wqm_input/atdep/'//atdepscen(:lenscen)//
     .       '/annual_deposition'//Cyears//'.csv'
      open(sumfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      fnam = outdir//'wqm_input/atdep/'//atdepscen(:lenscen)//
     .       '/monthly_deposition'//Cyears//'.csv'
      open(sumfile+1,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      fnam = outdir//'wqm_input/atdep/'//atdepscen(:lenscen)//
     .       '/aveann_deposition'//Cyears//'.csv'
      open(sumfile+2,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      write(sumfile,'(2a)')'seg,year,precip,wetno3,wetnh3,dryno3,',
     .                      'drynh3,wetorn,wetpo4,wetorp'
      write(sumfile+1,'(2a)')'seg,month,precip,wetno3,wetnh3,dryno3,',
     .                      'drynh3,wetorn,wetpo4,wetorp'
      write(sumfile+2,'(2a)')'seg,precip,wetno3,wetnh3,dryno3,',
     .                      'drynh3,wetorn,wetpo4,wetorp'

****************** loop over cells in file  ! create atdep time series
      do nc = 1,ncells

************ find atdep for that cell
        call getatdep12CMAQscen(
     I                          CMAQbase,lenCbase,
     I                          CbaseDir2,lenCb2,
     I                          CbaseSuperLong,lenCbLong,
     I                          CMAQscen,lenCscen,
     I                          CscenDir2,lenCs2,
     I                          CscenSuperLong,lenCsLong,
     I                          nc,linkdir,lenlink,
     O                          cellwq)
        print*,'cell ',nc
        do nq = 1,nBvar
          do ny = year1,year2
            do nd = 1,ndaysinyear(ny)
              wq(nd,ny,nc,nq) = cellwq(nd,ny,nq)
            end do
          end do
        end do

      end do      ! end loop over cells

      do ny = year1,year2

        fnam = 'atdep_opt.YYYY'           ! open file
        write(fnam(11:14),'(i4)') ny
        fnam = outdir//'wqm_input/atdep/'//atdepscen(:lenscen)//'/'
     .       //fnam
        open(dfile,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        print*,'writing file ',fnam(:64)

        year = ny
        month = 1
        day = 1
        do nd = 1,ndaysinyear(ny)
          do nc = 1,ncells
            write(dfile,1234)
     .            nc,year,month,day,(wq(nd,ny,nc,nq),nq=1,nBvar)
          end do
          call tomorrow(year,month,day)
        end do
        close(dfile)
      end do

      close(sumfile)
      close(sumfile+1)
      close(sumfile+2)
      close(sumfile+3)

      stop
1234  format(i5,3(',',i4),5(',',e10.4))

********************************* ERROR SPACE **************************
991   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end
