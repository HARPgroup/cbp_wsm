************************************************************************
** subroutine get surface runoff from the land                        **
************************************************************************
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'

      character*140 outline
      character*3 clu
      character*6 thislseg
      character*25 parmscen
      integer lenparmscen

      integer startyear,nyears,ny

      real annsuro(nyearmax)
      real sumsuro, avsuro

*************** END DECLARATIONS ***************************************
    
      read*,clu,thislseg,parmscen 
     
      call lencl(parmscen,lenparmscen)

**********            OPEN FILE
      fnam = outdir//'hspf/land/out/'//
     .       clu//'/'//parmscen(:lenparmscen)//'/'//
     .             clu//thislseg(:6)//'.out'

      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      print*, fnam   
**********     loop over lines look for meaningful lines
       nyears = 0
       do
        read(dfile,'(a140)',err=992,end=111) outline
        if (err.ne.0) go to 992

        if (outline(2:9).eq.'PERVIOUS') then
          nyears = nyears + 1
          if (nyears.eq.1) read(outline(125:128),'(i4)')startyear
        end if

        if (outline(6:29).eq.'EXTNL INFLOWS & OUTFLOWS') then     ! if find this line in PWATER section
          read(dfile,'(a140)',err=992,end=111) outline
          if (outline(48:51).eq.'SURO') then
           read(dfile,'(a140)',err=992,end=111) outline
           read(dfile,'(a140)',err=992,end=111) outline
           read(outline(42:51),*) annsuro(nyears)
           print*, clu, thislseg, nyears, annsuro(nyears)
          end if
        end if
        
       end do

111   close(dfile)

********** get average annual SURO
      sumsuro = 0.

      do ny = 2, nyears
       sumsuro = sumsuro + annsuro(ny)
      end do

      avsuro = sumsuro/real(nyears-1)
      print*, 'average',avsuro
      stop

********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'near line:'//outline
      go to 999

999   call stopreport(report)
 
      end
