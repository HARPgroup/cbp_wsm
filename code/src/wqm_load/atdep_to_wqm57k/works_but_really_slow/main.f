************************************************************************
**  generate the atmospheric input file for the 50k cell Bay model    **
**    loops over all cells in water quality model                     **
**      loops over all inputs                                         **
**        creates one variable with all constituents                  **
************************************************************************

      implicit none

      include 'transfer_wdm.inc'

      integer year,month,day
      integer year1,year2,month1,month2,day1,day2
      parameter (year1=1993,month1=1,day1=1)
      parameter (year2=2002,month2=12,day2=31)
      integer maxcells,ncells,nc
C      parameter (maxcells=11064)
      parameter (maxcells=1)


      character*13 trseg
      character*6 tlseg

      character*300 bigline

      real wq(366,year1:year2,maxcells,maxBvar)
      integer nd,ny,nq
      real cellwq(366,year1:year2,maxBvar)

      integer ndaysinyear
      external ndaysinyear

      integer julian
      external julian

      integer hotstart, lastcell

************ END DECLARATIONS ******************************************
      read*,hotstart    ! get hotstart flag

      do nq = 1,maxBvar ! initialize
        do nc = 1,maxcells
          do ny = year1,year2
            do nd = 1,366
              wq(nd,ny,nc,nq) = 0.0
            end do
          end do
        end do
      end do

      call masslink(
     O              nRvar,Rdsn,Rname,nAvar,Adsn,Aname,Afactor)
      call riverbay(
     I              nRvar,Rname,
     O              nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,DivRvar)

      ncells = maxcells   ! for atdep, all cells get the load

************ CHECK FOR HOT START FILE
      if (hotstart.ne.0) then
        print*,'getting output for hotstart'
        fnam = '../out/atdep/atdep_hotstart.bin'
        open(dfile+30,file=fnam,form='unformatted',
     .       status='unknown',iostat=err)
        if (err.ne.0) go to 991
        read(dfile+30)wq,lastcell
        close(dfile+30)
      else
        lastcell = 1
      end if
        

****************** loop over cells in file  ! create atdep time series
      do nc = lastcell,ncells  ! maxcells = ncells

************ find atdep for that cell
        call getatdep(
     I                nc,year1,year2,
     I                nRvar,Rdsn,Rname,nAvar,Adsn,Aname,Afactor,
     I                nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,DivRvar,
     O                cellwq)
        do nq = 1,nBvar
          do ny = year1,year2
            do nd = 1,ndaysinyear(ny)
              wq(nd,ny,nc,nq) = cellwq(nd,ny,nq)
            end do
          end do
        end do

**************** store output for hot start
        print*,'storing output for hotstart'
        fnam = '../out/atdep/atdep_hotstart.bin'
        open(dfile+30,file=fnam,form='unformatted',
     .       status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(dfile+30)wq,nc
        close(dfile+30)

      end do      ! end loop over cells

      do ny = year1,year2

        fnam = 'atdep_opt.YY'           ! open file
        write(fnam(9:12),'(i4)') ny
        fnam(9:10) = 't.'
        fnam = '../out/atdep/'//fnam
        open(dfile,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        print*,'writing file ',fnam(:50)

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

      stop
1234  format(i5,3(',',i4),15(',',e10.4))

********************************* ERROR SPACE **************************
991   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'Problem reading following file near line'
      report(2) = fnam
      report(3) = bigline(:64)
      go to 999

993   report(1) = 'Problem with linkage file:  Too many pairs on line:'
      report(2) = fnam
      report(3) = bigline(:64)
      go to 999

999   call stopreport(report)

      end

