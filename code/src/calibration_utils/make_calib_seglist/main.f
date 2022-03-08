************************************************************************
** Program to create generate weights for each land segments at each  **
**   calibration sites.  The sum of all calibration-site weights for  **
**   each segment must equal 1                                        **
** NOTE, nrsegs in this program means the number of segments that are **
**   in the calibration, not the total in the model as in most        **
**   programs.  other routines are slightly different as well.        **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/rsegs.inc'
      include '../../lib/inc/lsegs.inc'

*********** river variables
      integer ncsegs
      character*13 csegs(maxrsegs)  ! calibration segments
                               ! can include 0003 segs
      integer obs(maxrsegs) ! number of obs for cseg/conc pair
      integer goodobs(maxrsegs) ! number of obs above LOD
      character*12 module  ! variable specifiers
      integer lenmod

      character*25 calscen,basin,obscen
      integer lencalscen,lenbasin,lenobscen

      integer i,nl,nr1,nr2,ncsegs2,nr,ns,nt
      integer npoints,nonLOD  ! number of data points found
      logical foundall,anyobs(maxrsegs)

      integer year1,year2
      integer ny,nm,nd,nh,nmin   ! variables to read observed file

      real value
      character*1 qflag
*********** END DECLARATIONS ******************************************

      print*,' determining the calibration sites'

************ SPECIFICATION ENTRY SECTION
      read*,calscen,obscen,module,year1,year2

      call lencl(calscen,lencalscen)
      call uppercase(module)
      call lencl(module,lenmod)
      call lencl(obscen,lenobscen)

      basin = calscen(:lencalscen)//'_'//module

************* HOUSEKEEPING
      call readRiverSeglist(
     I                      basin,
     O                      rsegs,nrsegs)

      ncsegs = 0  
******** loop over simulated segments  
********* keep those segments that have data
      do nr = 1,nrsegs

********* loop over types, regular and double
******** do double first as it is upstream if both exist
        do nt = 1,2
          tseg = rsegs(nr)
          if (nt.eq.1) tseg(10:13) = '0003'  
          npoints = 0
          nonLOD = 0
          fnam = calibdir//'observed/'//obscen(:lenobscen)//'/WTMP/'
     .           //tseg//'.OWTMP'
          open (dfile,file=fnam,status='old',iostat=err)

          if (err.ne.0) cycle
 
          do
            read(dfile,*,err=992,end=333) ny,nm,nd,nh,nmin,value,qflag
            if (ny.ge.year1 .and. ny.le.year2) then
              npoints = npoints + 1
              if (qflag.ne.'<') nonLOD = nonLOD + 1
            end if
          end do
333       close(dfile)           ! close observed load file

          if (npoints.ge.24) then
            ncsegs = ncsegs + 1
            csegs(ncsegs) = tseg
          end if

        end do  ! end loop over 2 types
      end do          ! end loop of simulated segments
       
********* write the calib site to the .calib file
      fnam = seglistdir//calscen(:lencalscen)//'_'//
     .       module(:lenmod)//'.calib'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      write(dfile,1233,err=951) 'set segments = ( ',
     .                   (csegs(ns),' ',ns=1,ncsegs),')'
      close(dfile)

1233  format(2000a)

      stop

************* ERROR SPACE ****************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Problem reading file:'
      report(2) = fnam
      write(report(3),*) ny,' ',nm,' ',nd
      go to 999

999   call stopreport(report)

      end

