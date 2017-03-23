************************************************************************
** subroutine get river data                                          **
************************************************************************
      subroutine getrsegs(
     I                    rscen,lenrscen,
     O                    rsegs,uniqid,dsid,uniqindex,nrsegs)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/rsegs.inc'

      logical comment
      external comment
      integer ic

      do nrsegs = 1,supermax
        uniqindex(nrsegs) = -9
      end do
      
      nrsegs = 0
      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      fnam = catdir//'geo/'//geoscen(:lengeoscen)//'/rivernames.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a100)')line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line)) then
          call findcomma(line,ic)
          Tseg = line(:ic-1)
          call trims(Tseg,last)
          if (last.ne.13) then 
            err = 992
            return
          end if
          nrsegs = nrsegs + 1
          read(Tseg(5:8),'(i4)')uniqid(nrsegs)
          read(Tseg(10:13),'(i4)')dsid(nrsegs)
          rsegs(nrsegs) = Tseg
          uniqindex(uniqid(nrsegs))=nrsegs
        end if
        read(dfile,'(a100)')line
        call d2x(line,last)
      end do
      close (dfile)

      return

********************** ERROR SPACE *************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

999   call stopreport(report)
      end
