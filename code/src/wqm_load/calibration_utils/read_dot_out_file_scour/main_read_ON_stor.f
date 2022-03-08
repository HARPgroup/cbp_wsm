************************************************************************
** program to read the .out file, which is sometimes easier to deal   **
**   with than pltgens                                                **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

      character*3 clu
      character*140 outline
      integer years, startyear, ny

      real XronS(0:nyearmax)  ! eXport Refractory Orgn Surface
      real XronU(0:nyearmax)
      real XronG(0:nyearmax)
      real XlonS(0:nyearmax)
      real XlonU(0:nyearmax)
      real XlonG(0:nyearmax)

      real Frac3ronS,Frac3ronU,Frac3ronG
      real Frac3lonS,Frac3lonU,Frac3lonG  
           ! fraction of this parameter in the first three years
      real Frac3
      external Frac3
*************** END DECLARATIONS ***************************************


************ SPECIFICATION ENTRY SECTION

      read*,lseg,clu,lscen
      call lencl(lscen,lenlscen)

      print*, 'lu,lseg,Frac3ronS,Frac3ronU,Frac3ronG,',
     .                    'Frac3lonS,Frac3lonU,Frac3lonG,target'
**********            OPEN FILE
      fnam = outdir//'hspf/land/out/'//
     .       clu//'/'//lscen(:lenlscen)//'/'//
     .             clu//lseg(:6)//'.out'

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

        if (outline(8:18).eq.'LABILE ORGN') then
          read(outline(32:41),*) XlonS(ny)
          read(outline(72:81),*) XlonU(ny)
          read(outline(112:131),*) XlonG(ny)
        end if

        if (outline(8:18).eq.'REFRAC ORGN') then
          read(outline(32:41),*) XronS(ny)
          read(outline(72:81),*) XronU(ny)
          read(outline(112:131),*) XronG(ny)
        end if

      end do
111   close(dfile)
      years = ny

      Frac3ronS = Frac3(XronS,years)
      Frac3ronU = Frac3(XronU,years)
      Frac3ronG = Frac3(XronG,years)
      Frac3lonS = Frac3(XlonS,years)
      Frac3lonU = Frac3(XlonU,years)
      Frac3lonG = Frac3(XlonG,years)

      print 1234,clu,lseg,Frac3ronS,Frac3ronU,Frac3ronG,
     .           Frac3lonS,Frac3lonU,Frac3lonG,3.0/real(years-1)
      stop
1234  format(a3,',',a6,7(',',f9.4))

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

      function Frac3(x,years)
      implicit none
      integer years,ny
      real x(0:years),frac3
      
      x(0) = 0.0
      do ny = 2,years
        x(0) = x(0) + x(ny)
      end do
      if (x(0).le.0.00001) then
        Frac3 = 1.0
      else
        Frac3 = (x(2)+x(3)+x(4))/x(0)
      end if
      end

