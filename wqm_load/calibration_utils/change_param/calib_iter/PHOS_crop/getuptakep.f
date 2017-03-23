************************************************************************
** subroutine to get observed crop uptake                             **
************************************************************************
      subroutine gettaruptk(
     I                      tarscen,clu,lsegs,nlsegs,
     O                      taruptk)

      implicit none
      include 'calib_phos.inc'

      character*300 dline
      character*13 ltemp
      integer order                ! indexed to the column, not land use
      logical found
      integer i,ns,ic,nl,nc
      character*13 Tlseg
      character*4 uptakenam 
 
      real taruptk(maxlsegs)

      logical foundseg(maxlsegs)
**************END DECLARATION ******************************************

      do nl = 1, maxlsegs             ! initialize Target data
        taruptk(nl) = 0.0
      end do
       
************ open EOF targets file
      uptakenam = 'TP'
      call lencl(tarscen,lentarscen)
      call lencl(uptakenam,last)

      fnam=calibdir//'target/'//tarscen(:lentarscen)//'/'
     .        //uptakenam(:last)//'_uptake.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      
      read(dfile,'(a300)',err=996)dline            ! read header line
      call d2x(dline,i)
      call shift(dline)

      order = 1        ! find order
      do while (dline(:3).ne.clu)
        order = order + 1
        call shift(dline)
        if (dline(:3).eq.'   ') go to 992
      end do

      do ns = 1, nlsegs
        foundseg(ns) = .false.
      end do

*************** loop over all segments in file, look for active segs
      read(dfile,'(a300)',err=996)dline            ! read header line
      call d2x(dline,i)
      do while (dline(:3).ne.'end')
        read(dline,*) Tseg
        do ns = 1,nlsegs
          if (lsegs(ns).eq.Tseg) then
            foundseg(ns) = .true.
            if (dline(len(dline)-3:len(dline)).ne.'    ') go to 995
            do nc = 1,order
              call shift(dline)
            end do
            read(dline,*,err=993) taruptk(ns)
            exit
          end if
        end do
        read(dfile,'(a300)',err=996,end=997)dline    ! read next line
        call d2x(dline,i)
      end do

      close (dfile)

      do ns = 1, nlsegs
        if (.not.foundseg(ns)) go to 994
      end do


      return

**************** ERROR SPACE *******************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'land use '//ltemp(:3)// 'is not valid'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'segment '//Tseg(:6)// 'is not valid'
      report(2) = fnam
      report(3) = line
      go to 999

994   report(1) = 'did not find segment '//lsegs(ns)//' land use '//
     .             clu
      report(2) = fnam
      report(3) = line
      go to 999

995   report(1) = 'data line too long, modify '
      report(2) = fnam
      report(3) = ' or adjust dline in ./pp/src/lug/readvalues.f'
      go to 999

996   report(1) = 'did not find segment '//lsegs(ns)//' land use '//
     .             clu
      report(2) = fnam
      report(3) = line
      go to 999

997   report(1) = 'did not find segment '//lsegs(ns)//' land use '//
     .             clu
      report(2) = fnam
      report(3) = line
      go to 999

999   call stopreport(report)
      end 


************************************************************************
** program to read the .out file, which is sometimes easier to deal   **
**   with than pltgens                                                **
************************************************************************
      subroutine read_uptake(
     I                       clu,thislseg,lscen,
     O                       simuptk)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'

      character*140 outline
      character*3 clu
      character*6 thislseg

      integer years,startyear,ny

      real PO4uptk(0:nyearmax)  ! eXport Refractory Orgn Surface
      real simuptk

      integer i
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
        read(dfile,'(a140)',err=994,end=111) outline
        if (err.ne.0) go to 994

        if (outline(2:9).eq.'PERVIOUS') then
          ny = ny + 1
          if (ny.eq.1) read(outline(125:128),'(i4)')startyear
        end if

        if (outline(6:9).eq.'PHOS')  then
         do i = 1, 12
          read(dfile,'(a140)',err=994,end=111) outline            !get rid of junk lines
         end do
         read(dfile,'(a140)',err=994,end=111) outline      !read the line with Plant P storage 
         read(outline(104:111),*) PO4uptk(ny)
        end if

      end do

111   close(dfile)

      years = ny

      simuptk = (PO4uptk(ny)-PO4uptk(1))/real(years-1)

      return

********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'near line:'//outline
      go to 999

999   call stopreport(report)

      end






