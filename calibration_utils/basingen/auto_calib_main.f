************************************************************************
** fortran version of the matlab basingen program.  Also gets the     **
**  relative size of counties in the watershed and watershed in the   **
**   counties,  and other calibration sites that have this county     **
**  STRATEGY:                                                         **
**   first, get all calibration sites and couties in them             **
************************************************************************
      include 'basingen.inc'

      integer nsoneseg  ! index of base input segment
      integer nu1  ! indices
      character*4 segORbasin
      integer lenbasin
      logical singleseg

      character*25 calscen,module
      integer lenc,lenm

      read*,calscen, module   

******** read in all necessary data
      call initialize
      call getcalriver(calscen,module,fnam,err)
      if (err.ne.0) go to 990
      call getcalland(calscen,module,fnam,err)
      if (err.ne.0) go to 990

      if (nrsegs.ge.999.or.nlsegs.ge.999) go to 995

******** find list of all upstream segments for each segment
      do ns = 1,nrsegs
        call findupstream(ns)
      end do

******** get rid of upstream basins 
********** if the current seg is found upstream of another seg, set the
********** nallup of the current segment to zero
      do ns = 1,nrsegs
        do ns1 = 1,nrsegs  ! loop over other segs
          if (ns1.ne.ns) then
            do nu1 = 1,nallup(ns1)
              if (allup(ns1,nu1).eq.ns) 
     .            nallup(ns) = 0  
            end do
          end if
        end do
      end do
 
************* write out rivers
      call lencl(calscen,lenc)
      call lencl(module,lenm)

      fnam = seglistdir//calscen(:lenc)//'_'//module(:lenm)
     .       //'.riv'
      open (dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,1233,err=951) 'set segments = ( ',
     .  ((rsegs(allup(ns,ns1)),' ',ns1=1,nallup(ns)),
     .                                    ns=1,nrsegs),')'

      close(dfile)

************* write out land 
      fnam = seglistdir//calscen(:lenc)//'_'//module(:lenm)
     .       //'.land'
      open (dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,1233,err=951) 'set segments = ( ',
     .     (lsegs(ns1),' ',ns1=1,nlsegs),')'
      close(dfile)

1233  format(2000a)


      stop
************* ERROR SPACE ****************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

990   if (err.eq.991) go to 991
      if (err.eq.992) go to 992
      if (err.eq.993) go to 992
      if (err.eq.994) go to 992
      report(1) = 'unspecified error'
      report(2) = 'check code'
      report(3) = ' '
      go to 999

991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Problem in file '
      report(2) = fnam
      report(3) = 'river segment must have exactly 13 characters'
      go to 999

993   report(1) = 'Problem in file '
      report(2) = fnam
      report(3) = 'land segment must have exactly 6 characters'
      go to 999

994   report(1) = 'segment '//Tseg//' exists in file:'
      report(2) = fnam
      report(3) = 'but not in file catdir//landnames.csv'
      go to 999

995   report(1) = 'too many segments requested to make seglists file'
      report(2) = 'alter write statement in '
      report(3) = ' ./pp/..../basingen/auto_calib_main.f'
      go to 999

999   call stopreport(report)

      end

