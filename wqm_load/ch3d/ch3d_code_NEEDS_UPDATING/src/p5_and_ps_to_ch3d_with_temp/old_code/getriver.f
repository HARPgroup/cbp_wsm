************************************************************************
** subroutine get river data                                          **
************************************************************************
      subroutine getriver(
     O           rsegs,uniqindex,uniqid,dsid,nrsegs,fnam,err)
      implicit none
      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/rsegs.inc'
      logical comment
      external comment
      integer ic

      nrsegs = 0
C      print*,'getting river data'
      fnam = catdir//'connect/rivernames.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) then
        err = 991
        return
      end if

      read(dfile,'(a100)',err=992,end=993)line
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
        read(dfile,'(a100)',err=992,end=993)line
        call d2x(line,last)
      end do
      close (dfile)
      return

992   err = 992
      return
993   err = 993
      return

      end

