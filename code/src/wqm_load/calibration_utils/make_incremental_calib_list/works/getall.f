************************************************************************
** subroutines to read all of the data, share common blocks with main **
************************************************************************

************************************************************************
** subroutine get river data                                          **
************************************************************************
      subroutine getriver(fnam,err)
      include 'basingen.inc'

      nrsegs = 0
      print*,'getting river data'
      fnam = catdir//'geo/'//geoscen(:lengeoscen)//'/rivernames.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) then 
        err = 991
        return
      end if

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
      subroutine getland(fnam,err)
      include 'basingen.inc'

      nlsegs = 0
      print*,'getting land data'
      fnam = catdir//'geo/'//geoscen(:lengeoscen)//'/landnames.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) then 
        err = 991
        return
      end if

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

************************************************************************
** subroutine get calibration sites                                   **
************************************************************************
C      subroutine getcalibsites(fnam,err)
C      include 'basingen.inc'
C      integer tempid
C
C      nrsegs = 0
C      print*,'getting calibration sites'
C      fnam = catdir//'geo/'//geoscen(:lengeoscen)//'/calibsites.csv'
C      open(dfile,file=fnam,status='old',iostat=err)
C      if (err.ne.0) then 
C        err = 991
C        return
C      end if
C
C      read(dfile,'(a100)')line
C      call d2x(line,last)
C      do while (line(:3).ne.'end')
C        if (.not.comment(line)) then
C          call findcomma(line,ic)
C          Tseg = line(:ic-1)
C          call trims(Tseg,last)
C          if (last.ne.13) then 
C            err = 992
C            return
C          end if
C          read(Tseg(5:8),'(i4)') tempid
C          calsite(tempid) = .true.
C        end if
C        read(dfile,'(a100)')line
C        call d2x(line,last)
CC      end do
C      close (dfile)
C      end
C
************************************************************************
** subroutine get land to river data                                  **
************************************************************************
      subroutine getlandtoriver(fnam,err)
      include 'basingen.inc'
      integer keepnl

      print*,'getting land to river area'
      fnam= catdir//'geo/'//geoscen(:lengeoscen)//'/land_water_area.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) then 
        err = 991
        return
      end if

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
          read(Tseg(5:8),'(i4)') ns
          ns = uniqindex(ns)
          nallland(ns) = nallland(ns) + 1

          call shift(line)
          call findcomma(line,ic)
          Tseg = line(:ic-1)
          call trims(Tseg,last)
          if (last.ne.6) then 
            err = 993
            return
          end if
          found = .false.
          do nl = 1,nlsegs
            if (Tseg.eq.lsegs(nl)) then
              found = .true.
              keepnl = nl
              allland(ns,nallland(ns)) = nl
              exit
            end if
          end do
          if (.not.found) then 
            err = 994
            return
          end if
          

          call shift(line)
          read(line,*) acres(ns,nallland(ns))  ! save acres in this xsect

          lacres(keepnl) = lacres(keepnl) + acres(ns,nallland(ns))
                      ! save sum of land acres

        end if
        read(dfile,'(a100)')line
        call d2x(line,last)
      end do
      close (dfile)
      end

