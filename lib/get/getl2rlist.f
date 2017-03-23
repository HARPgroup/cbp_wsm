************************************************************************
** sub to get land segments for a river segment                       **
************************************************************************
      subroutine getl2rlist(
     I                      rseg,rscen,lenrscen,
     O                      numsegs,l2r)

      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'

      integer nl,numsegs,np
      logical found,comment
      external comment

      integer nps
      parameter (nps=3)
      character*5 psnam(nps)         ! pointsource
      data psnam /'wwtp','indus','cso'/
      integer lenpsnam(nps)

      character*100 dfnam
      integer lendfnam

      character*35 psscen            ! ps data scenario for wdm location
      integer lenpsscen

      character*35 Tdummy            
      character*20 Tlrseg

      character*13 Trseg
      character*6 Tlseg

      integer lwc  ! open file
       
**************** END DECLARATIONS **************************************
      
******** READ THE CONTROL FILE FOR SCENARIOS
      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)
 
      call readcontrol_Rpsscen(rscen,lenrscen,
     O                         psscen)
      call lencl(psscen,lenpsscen)
      
******** first get land-river segments from geo file
      fnam = catdir//'geo/'//geoscen(:lengeoscen)//
     .       '/land_water_area.csv'
      call findopen(lwc)
      open (lwc,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(lwc,'(a100)') line  ! ditch header

      numsegs = 0
      found = .false.
      do
        read(lwc,'(a100)',end=111,err=992) line
        call d2x(line,last)
        if (comment(line)) cycle
        read(line,*,err=993,end=993) Tseg
        if (Tseg .eq. rseg) then
          found = .true.
          numsegs = numsegs + 1
          if (numsegs.gt.maxL2R) go to 996
          read(line,*,err=993,end=993) Tseg,l2r(numsegs)
        end if
      end do

111   close(lwc)

      if (.not.found) go to 994

******** add zero-acre PS segments to the segment list
      do np = 1, nps
        call lencl(psnam(np),lenpsnam(np))
        dfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//'/'//
     .          psnam(np)(:lenpsnam(np))//'_lrsegs.csv'
        open (lwc,file=dfnam,status='old',iostat=err)
        if (err.ne.0) go to 995
      
        do
          read(lwc,'(a100)',end=222,err=992) line
          call d2x(line,last)
          if (comment(line)) cycle
          read(line,*,err=993,end=993) Tdummy,Trseg,Tlseg
          call trims(Trseg,last)
         
          if (Trseg(:last) .eq. rseg) then
            found = .false.
            do nl = 1,numsegs
              if (Tlseg (:6) .eq. l2r(nl)) then
                found = .true.
                exit  
              end if
            end do
 
            if (.not. found) then
              numsegs = numsegs + 1
              if (numsegs.gt.maxL2R) go to 996
              l2r(numsegs) = Tlseg (:6)
            end if
          end if    ! end if find the rseg
                       
        end do      ! end reading lines int the file

222   close(lwc)
      end do         ! end of loop for three PS files

      return

******************* ERROR SPACE ****************************************
991   report(1) = 'error opening file'
      report(2) = fnam
      write(report(3),*)' error = ',err
      go to 999

992   report(1) = 'error reading file,near line'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'error parsing line: in file:'
      report(2) = line
      report(3) = fnam
      go to 999

994   write(report(1),'(a23,a13,a9)')
     .     'Could not find segment ',rseg,' in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

995   report(1) = 'error opening PS file'
      report(2) = dfnam
      write(report(3),*)' error = ',err
      go to 999

996   report(1) = 'too many land segments for river segment'//rseg
      report(2) = 'increase the maxL2R variable in inc/lib/standard.inc'
      report(3) = ' and recompile all code.'

999   call stopreport(report)

      end

