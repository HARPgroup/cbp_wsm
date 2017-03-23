************************************************************************
** subroutine get land EOF results                                    **
************************************************************************
      subroutine getKSERfac(
     I                      lscen,version,lsegs,nlsegs,targets,clu,
     O                      facKSER)
      implicit none
      include 'calib_sed.inc'

      character*6 Tlseg
      character*3 Tlu,clu

      character*300 dline
 
      logical found

      integer ns
      character*(*) version

      real simEOF(maxlsegs)

      logical foundlseg(maxlsegs)

************* END DECLARATION *****************************************

      do ns = 1,maxlsegs    ! set to -9 to test that calib data exists
        foundlseg(ns) = .false.
      end do
      
      call lencl(lscen,lenlscen)
      fnam = outdir//'pltgen/summary/'//lscen(:lenlscen)//
     .             '/'//clu//'_WSSD_tons_'//version//'.csv'
      open(dfile+1,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      fnam = outdir//'pltgen/summary/'//lscen(:lenlscen)//
     .          '/'//clu// '_WSSD_tons.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 996
         
************ Save the results for each run      
      do
        read(dfile,'(a110)',err=992,end=111)dline
        call d2x(dline,last)
        call ryt(dline,dfile+1)
        
        call shift(dline)   ! get rid of long first column

        read (dline,*,err=997,end=997) Tlu  ! read land use
        if (Tlu.ne.clu) cycle              ! ignore line if not clu
        call shift(dline)

        read (dline,*,err=997,end=997) Tlseg   ! read lseg
        found = .false.
        do ns = 1,nlsegs
         if (Tlseg(:6).eq.lsegs(ns)) then
           foundlseg(ns) = .true.
           found = .true.
           exit
         end if
        end do
        if (.not.found) cycle  ! not doing this land seg
                 
        call shift(dline)   ! lseg
        call shift(dline)   ! pltgen type
        call shift(dline)   ! dates

        read (dline,*,err=997,end=997) simEOF(ns)  ! EOF simulated
      end do

111   close(dfile)
      close(dfile+1)

      do ns = 1,nlsegs
        if (.not.foundlseg(ns)) go to 994
      end do

      do ns = 1, nlsegs
        if (simEOF(ns) .lt. 0.0001) then
          facKSER(ns) = 2.0 ! if very small, double KSER and KRER
        else
          facKSER(ns) = targets(ns)/simEOF(ns)
        end if
      end do

      return

************ ERROR SPACE **************************************
991   report(1) = 'error opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'error reading file near line'
      report(2) = fnam
      report(3) = dline
      go to 999

994   report(1) = 'land segment '//lsegs(ns)//' not found in file'
      report(2) = fnam
      report(3) = ''
      go to 999

996   report(1) = 'Could not find output file for landuse '//clu
      report(2) = fnam
      report(3) = ' '
      go to 999

997   report(1) = 'error parsing file near line'
      report(2) = fnam
      report(3) = dline
      go to 999

999   call stopreport(report)
      end



