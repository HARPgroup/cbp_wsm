************************************************************************
**  generate the atmospheric input file for the 57k cell Bay model    **
**    loops over all cells in water quality model                     **
**      loops over all inputs                                         **
**        creates one variable with all constituents                  **
************************************************************************
      implicit none
      include 'inc_atdep.f'

      integer ncells,nc
      parameter (ncells=11064)
C      parameter (ncells=1)

      real wq(366,year1:year2,ncells,nBvar)

      character*10 Cyears

      integer DYEAR

************ END DECLARATIONS ******************************************
      read*,atdepscen,DYEAR,linkdir,
     .      CMAQbase,CbaseDir2,CbaseSuperLong
      call lencl(atdepscen,lenscen)
      call lencl(linkdir,lenlink)
      call lencl(CMAQbase,lenCbase)
      call lencl(Cbasedir2,lenCb2)
      call lencl(CbaseSuperLong,lenCbLong)

      do ny = year1,year2
         fCWNOX(ny) = 1.0
         fCWNHX(ny) = 1.0
         fLDNOX(ny) = 1.0
         fLDNHX(ny) = 1.0
         fLORGN(ny) = 1.0
         fLORGP(ny) = 1.0
         fLPO4X(ny) = 1.0
      end do
      call gettrendfactor(DYEAR,'CWNOX',fCWNOX)
      call gettrendfactor(DYEAR,'CWNHX',fCWNHX)
      call gettrendfactor(DYEAR,'LDNOX',fLDNOX)
      call gettrendfactor(DYEAR,'LDNHX',fLDNHX)
      do ny = year1,year2
         print*,ny,fCWNOX(ny),fCWNHX(ny),fLDNOX(ny),fLDNHX(ny),
     .         fLORGN(ny),fLORGP(ny),fLPO4X(ny)
      end do
c      return

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
     .       '/monthlyavg_deposition'//Cyears//'.csv'
      open(sumfile+1,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      fnam = outdir//'wqm_input/atdep/'//atdepscen(:lenscen)//
     .       '/aveann_deposition'//Cyears//'.csv'
      open(sumfile+2,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      fnam = outdir//'wqm_input/atdep/'//atdepscen(:lenscen)//
     .       '/monthly_deposition'//Cyears//'.csv'
      open(sumfile+3,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      write(sumfile,'(2a)')'seg,year,precip,wetno3,wetnh3,dryno3,',
     .                      'drynh3,wetorn,wetpo4,wetorp,area_ha'
      write(sumfile+1,'(2a)')'seg,month,precip,wetno3,wetnh3,dryno3,',
     .                      'drynh3,wetorn,wetpo4,wetorp,area_ha'
      write(sumfile+2,'(2a)')'seg,precip,wetno3,wetnh3,dryno3,',
     .                      'drynh3,wetorn,wetpo4,wetorp,area_ha'
      write(sumfile+3,'(2a)')'seg,year,mm,precip,wetno3,wetnh3,dryno3,',
     .                      'drynh3,wetorn,wetpo4,wetorp,area_ha'

****************** loop over cells in file  ! create atdep time series
      do nc = 1,ncells

************ find atdep for that cell
        call getatdep12(
     I                  fCWNOX,fCWNHX,fLDNOX,fLDNHX,
     I                  fLORGN,fLORGP,fLPO4X,
     I                  CMAQbase,lenCbase,
     I                  CbaseDir2,lenCb2,
     I                  CbaseSuperLong,lenCbLong,
     I                  nc,linkdir,lenlink,
     O                  cellwq)
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
        fnam = outdir//'wqm_input/atdep/'//atdepscen(:lenscen)//
     .          '/'//fnam
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




      subroutine gettrendfactor(
     I            DYEAR,PARAM,DFACTORS)

      implicit none
      include 'inc_atdep.f'

      integer     DYEAR
      character*5 PARAM,tPARAM
      integer     iPARAM
      real        tdata(1984:2050,4)
      real        DFACTORS(year1:year2)

      integer     iyear
      integer     ic

      character*2000 longline

      do iyear = year1,year2
         DFACTORS(iyear) = 1.0
      end do

      if ( DYEAR .le. 0 ) then
         print*,'... Calibration Mode = No Detrending'
         return
      end if

      fnam = tree//'input/unformatted/atdep/WQMDATA/'
     .       //'AD20160919/trend/20170731_WQSTM_trend.csv'

      open(dfile+2,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile+2,'(a2000)',err=992)longline
      call d2x(longline,last)

      iPARAM = 1
      do
         call d2x(longline,last)
         read(longline,*,err=992) tPARAM
         if ( tPARAM .eq. PARAM ) then
            print*,PARAM,' found at column #',iPARAM  
            goto 101
         end if
         call shift(longline)
         iPARAM = iPARAM + 1
      end do
101   print*,'... Column #',iPARAM

      do
         read(dfile+2,*,err=993,end=102)iyear,(tdata(iyear,ic),ic=1,4)
      end do
      
102   close(dfile+2)

      do iyear = year1,year2
         DFACTORS(iyear) = tdata(DYEAR,iPARAM-1)/tdata(iyear,iPARAM-1)
      end do
      

      return

991   report(1) = 'Problem with opening detrend factor file'
      report(2) = fnam
      write(report(3),*) err
      goto 999

992   report(1) = 'Problem reading file, column not found'
      report(2) = fnam
      report(3) = PARAM
      goto 999

993   report(1) = 'Problem reading file'
      report(2) = fnam
      report(3) = ' '
      goto 999

999   call stopreport(report)

      end
