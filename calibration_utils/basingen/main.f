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

      read*,segORbasin,rscen ! get starting segment
      call lencl(rscen,lenrscen)
******** read in all necessary data
      call initialize
      call getriver(rscen,lenrscen,fnam,err)
      if (err.ne.0) go to 990
      call getland(rscen,lenrscen,fnam,err)
      if (err.ne.0) go to 990
      call getlandtoriver(rscen,lenrscen,fnam,err)
      if (err.ne.0) go to 990
      call getcalibsites(rscen,lenrscen,fnam,err)
      if (err.ne.0) go to 990

******* process input; determine number of segments to process
      call procinput(
     I               segORbasin,
     O               singleseg,nrproc,rproc)

******** find list of all upstream segments for each segment
      do ns = 1,nrproc
        call findupstream(rproc(ns))
      end do

******** get rid of upstream basins from rproc (rivers to process)
********** if the current seg is found upstream of another seg, set the
********** nallup of the current segment to zero
      do ns = 1,nrproc    
        do ns1 = 1,nrproc  ! loop over other segs
          if (ns1.ne.ns) then
            do nu1 = 1,nallup(rproc(ns1))
              if (allup(rproc(ns1),nu1).eq.rproc(ns)) 
     .            nallup(rproc(ns)) = 0  
            end do
          end if
        end do
      end do
 
************* write out rivers
      if (singleseg) then
        fnam=tree//'config/seglists/'//rsegs(rproc(1))//'.riv'
      else
        call lencl(segORbasin,lenbasin)
        fnam=tree//'config/seglists/'//segORbasin(:lenbasin)//'.riv'
      end if
      open (dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,1233,err=951) 'set segments = ( ',
     .  ((rsegs(allup(rproc(ns),ns1)),' ',ns1=1,nallup(rproc(ns))),
     .                                    ns=1,nrproc),')'

      close(dfile)

*********** write out calibration sites
      if (singleseg) then
        fnam=tree//'config/seglists/'//rsegs(rproc(1))//'.calib'
      else
        call lencl(segORbasin,lenbasin)
        fnam=tree//'config/seglists/'//segORbasin(:lenbasin)//'.calib'
      end if
      open (dfile,file=fnam,status='unknown',iostat=err)
      
      if (err.ne.0) go to 991

      ncal = 0
      do ns = 1,nrproc
        do ns1 = 1,nallup(rproc(ns))
          if (calsite(uniqid(allup(rproc(ns),ns1)))) then
            ncal = ncal + 1
            calsegs(ncal) = rsegs(allup(rproc(ns),ns1))
          end if
        end do
      end do

      write(dfile,1233,err=951) 'set segments = ( ',
     .     (calsegs(ns1),' ',ns1=1,ncal),')'

      close(dfile)


************ get lsegs and area of base watershed
      call getlseglist(
     I                 rproc,nrproc,
     O                 blseglist,nblseglist)

********** write out total area
      if (singleseg) then
        fnam=tree//'config/seglists/'//rsegs(rproc(1))//'.land'
      else
        call lencl(segORbasin,lenbasin)
        fnam=tree//'config/seglists/'//segORbasin(:lenbasin)//'.land'
      end if
      open (dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,1233,err=951) 'set segments = ( ',
     .     (lsegs(blseglist(ns1)),' ',ns1=1,nblseglist),')'
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
      if (err.eq.993) go to 993
      if (err.eq.994) go to 994
      if (err.eq.995) go to 995
      if (err.eq.996) go to 996
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

995   report(1) = 'number of river segments in file exceeded max'
      report(2) = fnam
      report(3) = 'fix file or increase max in ./code/src/lib/rsegs.inc'
      go to 999

996   report(1) = 'Problem in file '
      report(2) = fnam
      report(3) = 'trouble reading line'
      go to 999

999   call stopreport(report)

      end

