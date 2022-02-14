************************************************************************
** subroutines to read all of the data, share common blocks with main **
************************************************************************

************************************************************************
** subroutine get river data                                          **
************************************************************************
      subroutine getcalriver(calscen,module,fnam,err)
      include 'basingen.inc'
      character*(*) calscen,module
      integer lenc,lenm

      call lencl(calscen,lenc)
      call lencl(module,lenm)

      fnam = controldir//'calib/'//module(:lenm)//'/'//
     .       calscen(:lenc)//'/rsegs.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) then 
        err = 991
        return
      end if

      nrsegs = 0
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
      end

************************************************************************
** subroutine get land data                                           **
************************************************************************
      subroutine getcalland(calscen,module,fnam,err)
      include 'basingen.inc'
      character*(*) calscen,module
      integer lenc,lenm

      call lencl(calscen,lenc)
      call lencl(module,lenm)

      fnam = controldir//'calib/'//module(:lenm)//'/'//
     .       calscen(:lenc)//'/lsegs.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) then 
        err = 991
        return
      end if

      nlsegs = 0
      read(dfile,'(a100)')line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line)) then
          call findcomma(line,ic)
          Tseg = line(:ic-1)
          call trims(Tseg,last)
          if (last.ne.6) then 
            err = 993
            return
          end if
          nlsegs = nlsegs + 1
          lsegs(nlsegs) = Tseg
        end if
        read(dfile,'(a100)')line
        call d2x(line,last)
      end do
      close (dfile)
      end


