************************************************************************
** sub to get land segments for a river segment                       **
************************************************************************
      subroutine getpsonlyl2r(
     I                        rseg,rscen,lenrscen,numsegs,l2r,
     O                        npssegs,psl2r)

      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/ps_septic_atdep.inc'

      integer nl,numsegs,np,npssegs
      character*6 psl2r(maxL2R)
      logical found,comment
      external comment

      integer nps
      parameter (nps=3)
      character*3 psnam(nps)         ! pointsource
c      data psnam /'wwtp','indus','cso'/
      integer lenpsnam(nps)

      character*35 psscen            ! ps data scenario for wdm location
      integer lenpsscen

      character*10 Tdummy            
      character*13 Trseg
      character*6 Tlseg
       
**************** END DECLARATIONS **************************************

      psnam(1) = wwtp
      psnam(2) = indus
      psnam(3) = cso      
******** READ THE CONTROL FILE FOR SCENARIOS
      call readcontrol_Rpsscen(rscen,lenrscen,
     O                         psscen)
      call lencl(psscen,lenpsscen)
    
******** add zero-acre PS segments to the segment list
      npssegs = 0 
      do np = 1, nps
        call lencl(psnam(np),lenpsnam(np))
        fnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//'/'//
     .          psnam(np)(:lenpsnam(np))//'_lrsegs.csv'
        open (dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991
      
        read(dfile,'(a100)',end=111,err=992) line   ! read header
        do
          read(dfile,'(a100)',end=111,err=992) line
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
              npssegs = npssegs + 1
              psl2r(npssegs) = Tlseg (:6)
            end if
          end if   ! end if find rseg
             
        end do      ! end reading lines int the file

111   close(dfile)
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

999   call stopreport(report)

      end

