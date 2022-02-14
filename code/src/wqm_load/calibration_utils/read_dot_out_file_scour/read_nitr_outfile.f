************************************************************************
** program to read the .out file, which is sometimes easier to deal   **
**   with than pltgens                                                **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

      character*140 outline
      character*3 clu
      character*6 thislseg

      integer years,ny,i
      integer year1,year2
      parameter(year1=1985,year2=1999) 
      logical found(year1:year2)

      real appNH4(year1:year2),appNO3(year1:year2)
      real atmosNH4(year1:year2),atmosNO3(year1:year2)
      real uptakeNH4(year1:year2),uptakeNO3(year1:year2)
      real imbNH4(year1:year2),imbNO3(year1:year2)
      real denit(year1:year2),nitr(year1:year2)
      real mine(year1:year2),volt(year1:year2)
      real stNH4(year1:year2),stNO3(year1:year2)
      real NH4out(year1:year2),NO3out(year1:year2)

      real totappNH4,totappNO3
      real totatmosNH4,totatmosNO3 
      real totuptkNH4,totuptkNO3
      real totdenit,totnitr,totvolt
      real totimbNH4,totimbNO3,totmin
      real totoutNH4, totoutNO3

      real avappNH4,avappNO3
      real avatmosNH4,avatmosNO3
      real avuptkNH4,avuptkNO3
      real avdenit,avnitr,avvolt
      real avimbNH4,avimbNO3,avmin
      real avoutNH4,avoutNO3

      real nh4uptk1, nh4uptk2
      real no3uptk1, no3uptk2
      real s1,s2
*************** END DECLARATIONS ***************************************

      print*, 'please input scenario, landuse, landseg'
      read*, lscen,clu,thislseg

      do ny = year1,year2
        found(ny) = .false.
      end do
      call lencl(lscen,lenlscen)
       
**********            OPEN FILE
      fnam = outdir//'hspf/land/out/'//clu//'/'//lscen(:lenlscen)//
     .           '/'//clu//thislseg(:6)//'.out'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

**********     loop over lines look for meaningful lines
      totappNH4   = 0.
      totappNO3   = 0.
      totatmosNH4 = 0.
      totatmosNO3 = 0.
      totuptkNH4  = 0.
      totuptkNO3  = 0.
      totdenit    = 0.
      totnitr     = 0.
      totimbNH4   = 0.
      totimbNO3   = 0. 
      totmin      = 0.
      totvolt     = 0.
      totoutNH4   = 0.
      totoutNO3   = 0. 

      do  
        read(dfile,'(a140)',err=992,end=111) outline 

        if (outline(113:116).ne.'YEAR') cycle
        read(outline(125:128),'(i4)') ny

        if (ny.lt.year1.or.ny.gt.year2) cycle

        found(ny) = .true.
     
        do while (outline(1:1).ne.'1')
          read(dfile,'(a140)',err=992,end=111) outline

*********   storage
          if (outline(33:51).eq.'NH4-N SOL NH4-N ADS') then
             do i =1, 8
               read(dfile,'(a140)',err=992,end=111) outline
             end do
             read(outline(33:41),*) s1
             read(outline(43:51),*) s2
             stNH4(ny) = s1 + s2
             read(outline(53:61),*) stNO3(ny)
          end if  

*********   Atmospheric deposition/applications   
          if (outline(6:27).eq.'ATMOSPHERIC DEPOSITION') then     ! if find this line, read till total line
            read(dfile,'(a140)',err=992,end=111) outline
            read(dfile,'(a140)',err=992,end=111) outline
            read(outline(83:91),*) atmosNH4(ny)
            totatmosNH4 = totatmosNH4 + atmosNH4(ny)

            read(dfile,'(a140)',err=992,end=111) outline
            read(outline(83:91),*) atmosNO3(ny)
            totatmosNO3 = totatmosNO3 + atmosNO3(ny)

            if (clu .eq. 'hwm') then          ! read applications
               read(dfile,'(a140)',err=992,end=111) outline
               read(dfile,'(a140)',err=992,end=111) outline
               read(dfile,'(a140)',err=992,end=111) outline
               read(dfile,'(a140)',err=992,end=111) outline
               read(dfile,'(a140)',err=992,end=111) outline
               read(outline(33:41),*) appNH4(ny)
               read(outline(43:51),*) appNO3(ny)
               totappNH4 = totappNH4 + appNH4(ny)
               totappNO3 = totappNO3 + appNO3(ny)
            end if
          end if

*********   Outflow
          if (outline(87:101).eq.'PONH4     PONO3') then     ! if find this line, read till total line
            read(dfile,'(a140)',err=992,end=111) outline
            read(outline(83:91),*) NH4out(ny)
            read(outline(93:101),*) NO3out(ny) 
            totoutNH4 = totoutNH4 + NH4out(ny)
            totoutNO3 = totoutNO3 + NO3out(ny)
          end if

*********   Plant uptake            
          if (outline(3:29).eq.'ABOVE-GRND NH3 PLANT UPTAKE') then     ! if find this line, read till total line
            read(dfile,'(a140)',err=992,end=111) outline
            read(dfile,'(a140)',err=992,end=111) outline
            read(outline(123:131),*) nh4uptk1
          end if 

          if (outline(3:29).eq.'BELOW-GRND NH3 PLANT UPTAKE') then             
            read(dfile,'(a140)',err=992,end=111) outline
            read(dfile,'(a140)',err=992,end=111) outline
            read(outline(123:131),*) nh4uptk2
            uptakeNH4(ny) = nh4uptk1 + nh4uptk2
            totuptkNH4 = totuptkNH4 + uptakeNH4(ny) 
          end if

           if (outline(3:29).eq.'ABOVE-GRND NO3 PLANT UPTAKE') then     ! if find this line, read till total line
             read(dfile,'(a140)',err=992,end=111) outline
             read(dfile,'(a140)',err=992,end=111) outline
             read(outline(123:131),*) no3uptk1
           end if
           
           if (outline(3:29).eq.'BELOW-GRND NO3 PLANT UPTAKE') then
             read(dfile,'(a140)',err=992,end=111) outline
             read(dfile,'(a140)',err=992,end=111) outline
             read(outline(123:131),*) no3uptk2
             uptakeNO3(ny) = no3uptk1 + no3uptk2
             totuptkNO3 = totuptkNO3 + uptakeNO3(ny)
           end if

*********   Denitrification
           if (outline(15:29).eq.'DENITRIFICATION') then     
             read(dfile,'(a140)',err=992,end=111) outline
             read(dfile,'(a140)',err=992,end=111) outline
             read(outline(123:131),*) denit(ny)
             totdenit = totdenit + denit(ny)  
           end if

*********   Nitrification
           if (outline(13:29).eq.'NH3 NITRIFICATION') then
             read(dfile,'(a140)',err=992,end=111) outline
             read(dfile,'(a140)',err=992,end=111) outline
             read(outline(123:131),*) nitr(ny)
             totnitr = totnitr + nitr(ny)
           end if

*********   NH3 IMMOBILIZATION
           if (outline(12:29).eq.'NH3 IMMOBILIZATION') then
             read(dfile,'(a140)',err=992,end=111) outline
             read(dfile,'(a140)',err=992,end=111) outline
             read(outline(123:131),*) imbNH4(ny)
             totimbNH4 = totimbNH4 + imbNH4(ny)
           end if

*********   ORGN MINERALIZATION
           if (outline(11:29).eq.'ORGN MINERALIZATION') then
             read(dfile,'(a140)',err=992,end=111) outline
             read(dfile,'(a140)',err=992,end=111) outline
             read(outline(123:131),*) mine(ny)
             totmin = totmin + mine(ny)
           end if

*********   Nitrification
           if (outline(12:29).eq.'NO3 IMMOBILIZATION') then
             read(dfile,'(a140)',err=992,end=111) outline
             read(dfile,'(a140)',err=992,end=111) outline
             read(outline(123:131),*) imbNO3(ny)
             totimbNO3 = totimbNO3 + imbNO3(ny)
           end if

*********   Volatilization
           if (outline(12:29).eq.'NH3 VOLATILIZATION') then
             read(dfile,'(a140)',err=992,end=111) outline
             read(dfile,'(a140)',err=992,end=111) outline
             read(outline(123:131),*) volt(ny)
             totvolt = totvolt + volt(ny)
           end if

         end do
       end do
       
111   close(dfile)

**********   check for completeness
      do ny = year1,year2
        if (.not.found(ny)) go to 992
      end do
      years = year2-year1+1

**********   get average value
      avappNH4   = totappNH4/years
      avappNO3   = totappNO3/years
      avatmosNH4 = totatmosNH4/years
      avatmosNO3 = totatmosNO3/years
      avuptkNH4  = totuptkNH4/years
      avuptkNO3  = totuptkNO3/years
      avdenit    = totdenit/years
      avnitr     = totnitr/years
      avimbNH4   = totimbNH4/years
      avimbNO3   = totimbNO3/years
      avmin      = totmin/years
      avvolt      = totvolt/years      
      avoutNH4   = totoutNH4/years
      avoutNO3   = totoutNO3/years

*********  output file

      fnam = outdir//'hspf/land/out/'//clu//'/'//lscen(:lenlscen)//
     .           '/'//'outfile.out'
      open(dfile,file=fnam,status='new',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,1211,err=951) 'SEGMNT','appNH4','appNO3',
     .                  'atmosNH4','atmosNO3','uptkNH4','uptkNO3',
     .                  'denit','nitr','mine','volat',
     .                  'imbNH4','imbNO3','outNH4','outNO3',
     .                  '1stNH4','edNH4','1stNO3','edNO3'
      write(dfile,1222,err=951) thislseg,avappNH4,avappNO3,
     .                  avatmosNH4,avatmosNO3,avuptkNH4,avuptkNO3,
     .                  avdenit,avnitr,avmin,avvolt,
     .                  avimbNH4,avimbNO3,avoutNH4,avoutNO3,
     .                  stNH4(1985),stNH4(1999),stNO3(1985),stNO3(1999) 
      close(dfile)

1211  format(a6,18(',',a8))
1222  format(a6,18(',',f8.3))
      
      stop

********************* ERROR SPACE **************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

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



