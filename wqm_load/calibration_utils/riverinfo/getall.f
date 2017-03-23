************************************************************************
** subroutines to read all of the data, share common blocks with main **
************************************************************************

************************************************************************
** subroutine get river data                                          **
************************************************************************
      subroutine getriver(rscen,lenrscen,fnam,err)
      include 'basingen.inc'

      nrsegs = 0
C      print*,'getting river data'
      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

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
      subroutine getland(rscen,lenrscen,fnam,err)
      include 'basingen.inc'

      nlsegs = 0
C      print*,'getting land data'
      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

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
      subroutine getcalibsites(rscen,lenrscen,fnam,err)
      include 'basingen.inc'
      integer tempid

C      print*,'getting calibration sites'
      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      fnam = catdir//'geo/'//geoscen(:lengeoscen)//'/calibsites.csv'
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
          read(Tseg(5:8),'(i4)') tempid
          calsite(tempid) = .true.
        end if
        read(dfile,'(a100)')line
        call d2x(line,last)
      end do
      close (dfile)
      end

***********************************************************************
** subroutine get land to river data                                  **
************************************************************************
      subroutine getlandtoriver(rscen,lenrscen,fnam,err)
      include 'basingen.inc'
      integer keepnl

C      print*,'getting land to river area'
      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      fnam= catdir//'geo/'//geoscen(:lengeoscen)//'/land_water_area.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) then 
        err = 991
        return
      end if

      read(dfile,'(a100)')line
      call d2x(line,last)
      do 
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
          if (ns.eq.0) then
            err = 995
            return
          end if
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
        read(dfile,'(a100)',err=996,end=111)line
        call d2x(line,last)
      end do
111   close (dfile)

      return
996   err = 996
      return

      end

