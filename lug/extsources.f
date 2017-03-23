************************************************************************
** Makes the External Sources block                                   **
************************************************************************
      subroutine extsources(ioscen,lenioscen,perlnd,implnd,evap)
      implicit none
      include 'lug.inc'

      logical perlnd,implnd,found          ! type of land use this uci is for

      real evap           ! pan evaporation multiplier

      if (perlnd) fnam = catdir//'iovars/'//
     .            ioscen(:lenioscen)//'/perlnd_extsources'
      if (implnd) fnam = catdir//'iovars/'//
     .            ioscen(:lenioscen)//'/implnd_extsources'
      open(dfile,file=fnam,status='old',iostat=err)     ! open
      if (err.ne.0) go to 991

      read(dfile,'(a100)') line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (line(12:15).eq.'EVAP') write(line(29:38),'(f10.3)') evap
        call ryt(line,uci)
        read(dfile,'(a100)')line
        call d2x(line,last)
      end do

      close (dfile)

      return

************** ERROR SPACE *********************************************
991   report(1) = 'Could not open file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:10),'(i2)') err
      go to 999

999   call stopreport(report)

      end
