************************************************************************
** subroutine to get observed crop uptake                             **
************************************************************************
      subroutine gettaruptk(
     I                      tarscen,clu,lsegs,nlsegs,
     O                      taruptk)

      implicit none
      include 'calib_nitr.inc'

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

      uptakenam = 'TN'

      do i = 1, maxlsegs             ! initialize Target data
        taruptk(i) = 0.0
      end do
       
************ open EOF targets file
      call lencl(tarscen,lentarscen)
      call lencl(uptakenam,last)

      fnam = calibdir//'target/'//tarscen(:lentarscen)//'/'
     .          //uptakenam(:last)//'_uptake.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a300)',err=992)dline            ! read header line
      call d2x(dline,i)
      call shift(dline)

      order = 1        ! find order
      do while (dline(:3).ne.clu)
        order = order + 1
        call shift(dline)
        if (dline(:3).eq.'   ') go to 993
      end do

      do ns = 1, nlsegs
        foundseg(ns) = .false.
      end do

*************** loop over all segments in file, look for active segs
      read(dfile,'(a300)',err=992)dline            ! read header line
      call d2x(dline,i)
      do while (dline(:3).ne.'end')
        read(dline,*) Tseg
        do ns = 1,nlsegs
          if (lsegs(ns).eq.Tseg) then
            foundseg(ns) = .true.
            if (dline(len(dline)-3:len(dline)).ne.'    ') go to 994
            do nc = 1,order
              call shift(dline)
            end do
            read(dline,*,err=995) taruptk(ns)
            exit
          end if
        end do
        read(dfile,'(a300)',err=992,end=996)dline    ! read next line
        call d2x(dline,i)
      end do

      close (dfile)

      do ns = 1, nlsegs
        if (.not.foundseg(ns)) go to 997
      end do


      return

**************** ERROR SPACE *******************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'error reading file'
      report(2) = fnam
      report(3) = dline
      go to 999

993   report(1) = 'could not find land use '//clu//' in file'
      report(2) = fnam
      report(3) = dline
      go to 999

994   report(1) = 'data line too long, modify '
      report(2) = fnam
      report(3) = ' or adjust dline in ./pp/src/lug/readvalues.f'
      go to 999

995   report(1) = 'error reading target value in file'
      report(2) = fnam
      report(3) = dline
      go to 999

996   report(1) = 'did not literal '//char(39)//'end'//char(39)//' in'
      report(2) = fnam
      report(3) = ' '
      go to 999

997   report(1) = 'did not find segment '//lsegs(ns)//' in file'
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

      real NH4uptk(0:nyearmax),nh4x1, nh4x2  
      real NO3uptk(0:nyearmax),no3x1,no3x2
      real Nfix(0:nyearmax)
      real TNuptk(0:nyearmax)
      real TNSum, simuptk

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
        read(dfile,'(a140)',err=992,end=111) outline
        if (err.ne.0) go to 992

        if (outline(2:9).eq.'PERVIOUS') then
          ny = ny + 1
          if (ny.eq.1) read(outline(125:128),'(i4)')startyear
        end if

*******forest needs to add above-ground uptake
       if (clu .eq. 'for') then                            
         if (outline(3:29) .eq. 'ABOVE-GRND NH3 PLANT UPTAKE') then
           read(dfile,'(a140)',err=992,end=111) outline
           read(dfile,'(a140)',err=992,end=111) outline
           if ( outline(129:131).eq.'NAN' ) then
             nh4x1 = -1.
           else
             read(outline(123:131),*) nh4x1
           end if
         end if

         if (outline(3:29) .eq. 'BELOW-GRND NH3 PLANT UPTAKE') then
         read(dfile,'(a140)',err=992,end=111) outline
         read(dfile,'(a140)',err=992,end=111) outline
           if ( outline(129:131).eq.'NAN' ) then
             nh4x2 = -1.
           else
            read(outline(123:131),*) nh4x2
           end if
         end if
         NH4uptk(ny) = nh4x1 + nh4x2

         if (outline(3:29) .eq. 'ABOVE-GRND NO3 PLANT UPTAKE')  then
         read(dfile,'(a140)',err=992,end=111) outline
         read(dfile,'(a140)',err=992,end=111) outline
           if ( outline(129:131).eq.'NAN' ) then
             no3x1 = -1.
           else
             read(outline(123:131),*) no3x1
           end if
         end if

         if (outline(3:29) .eq. 'BELOW-GRND NO3 PLANT UPTAKE')  then
         read(dfile,'(a140)',err=992,end=111) outline
         read(dfile,'(a140)',err=992,end=111) outline
           if ( outline(129:131).eq.'NAN' ) then
             no3x2 = -1.
           else
             read(outline(123:131),*) no3x2
           end if
         end if
         NO3uptk(ny) = no3x1 + no3x2

         if (nh4x1 .eq. -1. .or. nh4x2 .eq. -1. .or. 
     .                       no3x1 .eq. -1. .or. no3x2 .eq. -1.) then
           TNuptk(ny)= -1. 
         else
         TNuptk(ny) = NH4uptk(ny)+ NO3uptk(ny)
         end if

******** GET UPTAKE FOR NON-FOREST
       else

        if (outline(14:29) .eq. 'NH3 PLANT UPTAKE')  then
         read(dfile,'(a140)',err=992,end=111) outline
         read(dfile,'(a140)',err=992,end=111) outline
         read(outline(123:131),*) NH4uptk(ny)
        end if

        if (outline(14:29) .eq. 'NO3 PLANT UPTAKE')  then
         read(dfile,'(a140)',err=992,end=111) outline
         read(dfile,'(a140)',err=992,end=111) outline
         read(outline(123:131),*) NO3uptk(ny)
        end if

        TNuptk(ny) = NH4uptk(ny)+ NO3uptk(ny)
      end if
              
        if (clu .eq. 'alf') then
          if (outline(13:29) .eq. 'NITROGEN FIXATION')  then
            read(dfile,'(a140)',err=992,end=111) outline
            read(dfile,'(a140)',err=992,end=111) outline
            read(outline(123:131),*) Nfix(ny)
          end if
          TNuptk(ny) = NH4uptk(ny)+ NO3uptk(ny)+ Nfix(ny)
        end if

      end do

111   close(dfile)

      years = ny

      TNSum = 0.
      do i = 2, years
       if (TNuptk(i).eq. -1.0) then
        TNSum = -1.0
        exit
       end if
       TNSum = TNSum + TNuptk(i)
      end do
     
      if (TNSum .eq. -1.0) then
      simuptk = -1.0
      else
      simuptk = TNSum/real(years-1)
      end if

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






