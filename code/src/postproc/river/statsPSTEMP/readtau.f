************************************************************************
**  main calling program for the calculation of statistics            **
************************************************************************
      subroutine readtau(rscen,rseg,year1,year2,
     O                   dailytau)
    
      implicit none
      include 'Rstats.inc'

      integer ifl

      integer ny,nm,nd,nh,nmin        ! indices
      integer year1,year2             ! first and last year to average

      real dailytau(EarliestYear:LatestYear,12,31),tau  ! daily tau value
                   
****************** END DECLARATIONS ***********************************

***************** Get daily critical sheer stress from Pltgen file   ../pltgen/river/$scenario/*.tau
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      fnam = outdir//'pltgen/river/'//rscen(:lenrscen)//
     .               '/'// rseg(:lenrseg)//'.tau'                       

      call findopen(ifl)
      open(ifl,file=fnam,status='old',iostat=err)
       
      if (err .eq. 0) then  ! if exist, read daily tau values
        read(ifl,'(A26)') line
        if (line(6:26).eq.'HSPF FILE FOR DRIVING') then  ! origan pltgen from HSPF simulation
           call plt2cal(ifl,fnam)                      ! modify original pltgen file
           read(ifl,'(A26)') line                      ! read the first line of modified file
        end if

        do
          read(ifl,*, end=222) ny,nm,nd,nh,nmin,tau
          if (ny.ge.year1.and.ny.le.year2) then
            dailytau(ny,nm,nd) = tau           ! get daily crtitcal stress
          end if
        end do

      else   ! no file
        print*,'Shear Stress Pltgen for river ',rseg,' was not created'
        do ny = year1,year2  ! set daily crtitcal stress to zero
          do nm = 1,12
            do nd = 1,31    
              dailytau(ny,nm,nd) = 0. 
            end do
          end do
        end do
      end if
                     
222   close(ifl)

      return
  
      end

