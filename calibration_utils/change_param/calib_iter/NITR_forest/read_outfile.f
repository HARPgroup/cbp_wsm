************************************************************************
** program to read the .out file, which is sometimes easier to deal   **
**   with than pltgens                                                **
************************************************************************
      subroutine read_outfile(
     I                        clu,thislseg,lscen,
     O                        lorgn,plantn,agplant,litter,years)
  
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'

      character*140 outline
      character*3 clu
      character*6 thislseg

      integer startyear,years,ny

      real lorgn(4,0:nyearmax),plantn(4,0:nyearmax)  ! eXport Refractory Orgn Surface
      real agplant(0:nyearmax),litter(0:nyearmax)
      integer nl,ly
*************** END DECLARATIONS ***************************************

      call lencl(lscen,lenlscen)
       
**********            OPEN FILE
      fnam = outdir//'hspf/land/out/'//clu//'/'//lscen(:lenlscen)//
     .           '/'//clu//thislseg(:6)//'.out'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

**********     loop over lines look for meaningful lines
       ny = 0
       do  
        read(dfile,'(a140)',err=992,end=111) outline 
        if (err.ne.0) go to 992

        if (outline(2:9).eq.'PERVIOUS') then
          ny = ny + 1
          if (ny.eq.1) read(outline(125:128),'(i4)')startyear
        end if

        if (outline(33:51).eq.'NH4-N SOL NH4-N ADS') then     ! if find this line, read till total line
          read(dfile,'(a140)',err=992,end=111) outline
          read(dfile,'(a140)',err=992,end=111) outline
           if ( outline(110:112).eq.'nan' ) then                 ! read above-ground and litter plant N
            agplant(ny) = -1
           else
            read(outline(102:111),*) agplant(ny)
           end if

          read(dfile,'(a140)',err=992,end=111) outline
           if ( outline(110:112).eq.'nan' ) then                 ! read above-ground and litter plant N
            litter(ny) = -1
           else
            read(outline(102:111),*) litter(ny)
           end if

*********   read labile organic storage and plant N at each layer            
          read(dfile,'(a140)',err=992,end=111) outline         
          do ly = 1, 2
           read(dfile,'(a140)',err=992,end=111) outline
           if ( outline(79:81).eq.'nan' ) then
            lorgn(ly,ny) = -1
            plantn(ly,ny) =-1
           else
            read(outline(72:81),*) lorgn(ly,ny)
            read(outline(102:111),*) plantn(ly,ny)
           end if
          end do
  
          read(dfile,'(a140)',err=992,end=111) outline
       
          do ly = 3, 4    
           read(dfile,'(a140)',err=992,end=111) outline
           if ( outline(79:81).eq.'nan' ) then
            lorgn(ly,ny) = -1
            plantn(ly,ny) = -1 
           else
            read(outline(72:81),*) lorgn(ly,ny)
            read(outline(102:111),*) plantn(ly,ny)
           end if
          end do
          
        end if

       end do
       
111   close(dfile)

      years = ny

      return

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



