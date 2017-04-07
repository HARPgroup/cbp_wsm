***********************************************************************
***Subroutine get land to river data                                ***
***********************************************************************
      subroutine getlandtoriver(nrsegs,rsegs,nlsegs,lsegs,
     I                          rscen,lenrscen,
     O                          nallland,allland,acres)

      include 'flow_weight.inc'
    
      integer nl,ns,keepnl,ic
      real lacres(maxL2R)
************** END DECLARATION ***************************************

      do ns = 1,nrsegs
       do nl = 1,nlsegs
          allland(ns,nl) = 0
          acres(ns,nl) = 0.0
        end do
        nallland(ns) = 0
      end do

      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      fnam = catdir//'geo/'//geoscen(:lengeoscen)//
     .               '/land_water_area.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a100)')line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line)) then
          call findcomma(line,ic)
          Tseg = line(:ic-1)
          call trims(Tseg,last)
          
          do ns = 1, nrsegs 
           if(Tseg(:last) .eq. rsegs(ns)) then  ! found river segment
            nallland(ns) = nallland(ns) + 1
            call shift(line)
            call findcomma(line,ic)
            Tseg = line(:ic-1)
            call trims(Tseg,last)
            found = .false.
            do nl = 1,nlsegs
             if (Tseg.eq.lsegs(nl)) then
              found = .true.
              keepnl = nl
              allland(ns,nallland(ns)) = nl
              exit
             end if
            end do
            if (.not.found) go to 992

            call shift(line)
            read(line,*) acres(ns,nallland(ns))  ! save acres in this xsect
            lacres(keepnl) = lacres(keepnl) + acres(ns,nallland(ns))   ! save sum of land acres
           exit
          end if
         end do       ! loop over all calibrate sites

        end if
        read(dfile,'(a100)')line
        call d2x(line,last)
      end do
      close (dfile)
      return

************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'something wrong with program logic'
      report(2) = 'good luck'
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

