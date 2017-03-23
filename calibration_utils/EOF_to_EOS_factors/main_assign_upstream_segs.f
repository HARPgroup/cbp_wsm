************************************************************************
** program to assign subgrid transport factors from downstream        **
**   stations to upstream segments.  subgrid transport factors        **
**   convert EOF to EOS for nitrogen and phosphorous.                 **
**                                                                    **
**    in the ./run/control/calib/subgridtf/$calscen.con  file         **
**    which references file in the ./pp/data/transport/calc_factors/  **
**    directory.                                                      **
**                                                                    **
**  this always runs on the whole model domain because the chance of  **
**    errors is too great when passing a segment list                 **
************************************************************************
** This approach is flawed
***  The bias correction is applied to all segments between two stations
***  regardless of the bias at the upper station
**** need to do a mass balance approach, get the load bias rather than
**** concentration bias, then follow through like the estimator routine
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/rsegs.inc'

********** input info
      character*20 calscen ! file under ./run/control/calib/calc_factors
      integer lenscen 

********** specifications for data sets
      character*20 datatype, spec
      character*20 wqparam, biasMethod
      integer lenwq, lenbias

********** output subgrid transport factor
      real sgtf(maxrsegs)
      real sgtfbasin

********** calibration station variables
      integer maxstations, nstations, ns  ! estimator flux points
      parameter (maxstations = 300)
      character*13 staname(maxstations)
      real bias(maxstations)
      integer nupstations,nups      ! number of upstream stations
      integer upstations(maxstations)
      integer nupsegs  ! number of upstream segs (DS of other stations)
      integer upsegs(maxrsegs) ! upstream segs indices


******************  useful variables
      integer nr

********** utilities
      logical comment
      external comment

************* END DECLARATIONS *****************************************

********* GET INPUT FROM USER
      read*,calscen,rscen  ! specifier for whole run
      call lencl(calscen,lenscen)
      call lencl(rscen,lenrscen)

C      print*, 'reading data files'
*********** GET RSEGS FROM CATALOG FILE
      call getrsegs(rscen,lenrscen,
     O              rsegs,nrsegs,uniqid,dsid,uniqindex)

************ READ CONTROL FILE
      fnam = controldir//'calib/subgridTF/'//
     .       calscen(:lenscen)//'.con'
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      wqparam = '-'
      biasMethod = '-'
      do
        read(11,'(a100)',end=111,err=992) line
        call d2x(line,last)
        if (comment(line)) cycle
        if (line(1:4).eq.'    ') cycle
        read (line,*) datatype,spec
        if (datatype(1:7).eq.'WQparam') then
          wqparam = spec
        else if (datatype(1:11).eq.'bias_method') then
          biasMethod = spec
        else
          go to 993
        end if
      end do

111   close(11)

      if (wqparam.eq.'-') go to 9941
      if (biasMethod.eq.'-') go to 9945

      call lencl(wqparam,lenwq)
      call lencl(biasMethod,lenbias)

************ READ DATA FILES
*********** if river not supplied in data files, 'no data -9' is kept
********** flux
      fnam = pardir//'transport/calc_factors/'//
     .       'bias_method_'//WQparam(:lenwq)//'_'//
     .       biasMethod(:lenbias)//'.csv'
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nstations = 1
      do
        read(11,'(a100)',err=992,end=222) line
        if (comment(line)) cycle
        read(line,*,err=995,end=995) staname(nstations),bias(nstations)
        nstations = nstations + 1
        if (nstations.gt.maxstations) go to 996
      end do
222   close(11)
      nstations = nstations - 1


************* DETERMINE SGTF *******************************************
********** open output file and transport factor file
      fnam = pardir//'transport/calc_factors/'//
     .       calscen(:lenscen)//'_sgtf.csv'
      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      do nr = 1,nrsegs
        sgtf(nr) = -999.0
      end do

************* loop over estimated bias points
      do ns = 1,nstations

C        call ttyput(staname(ns))
C        call ttyput(' ')

************ get upstream list non-inclusive of other estimator points
        call findupstream(
     I                    staname(ns),
     I                    maxstations,staname,nstations,
     I                    rsegs,uniqid,dsid,uniqindex,nrsegs,
     O                    upsegs,nupsegs,
     O                    upstations,nupstations)

************** calculate factor and store for each segment
        sgtfbasin = 1.0/(1.0+bias(ns))
        sgtfbasin = max(sgtfbasin,0.25)
        sgtfbasin = min(sgtfbasin,4.0)
        
        do nr = 1,nupsegs
          sgtf(upsegs(nr)) = sgtfbasin
        end do

      end do

      write(11,*,err=951)'seg, sgtf'
      do nr = 1,nrsegs
        write(11,*,err=951)rsegs(nr),',',sgtf(nr)
      end do
      close(11)

      print*,'output file written:'
      print*,fnam

      stop
***************** ERROR SPACE ******************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Problem reading file: near line: '
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'Problem in file '
      report(2) = fnam
      report(3) = 'data type '//datatype//' not recognized'
      go to 999

9941  report(1) = 'Problem in file '
      report(2) = fnam
      report(3) = 'data type '//char(39)//'wqparam'
     .            //char(39)//' not found'
      go to 999
9942  report(1) = 'Problem in file '
      report(2) = fnam
      report(3) = 'data type '//char(39)//'EOF'
     .            //char(39)//' not found'
      go to 999
9943  report(1) = 'Problem in file '
      report(2) = fnam
      report(3) = 'data type '//char(39)//'EOS'
     .            //char(39)//' not found'
      go to 999
9944  report(1) = 'Problem in file '
      report(2) = fnam
      report(3) = 'data type '//char(39)//'attenuation'
     .            //char(39)//' not found'
      go to 999
9945  report(1) = 'Problem in file '
      report(2) = fnam
      report(3) = 'data type '//char(39)//'estimator_data'
     .            //char(39)//' not found'
      go to 999

995   report(1) = 'Problem in file: can not parse line into data'
      report(2) = fnam
      report(3) = line
      go to 999

996   report(1) = 'number of stations in file is greater than allowed'
      report(2) = 'adjust maxstation parameter or delete stations'
      report(3) = fnam
      go to 999

9971  report(1) = 'Need to supply attenuation information'
      report(2) = ' for segment '//rsegs(upsegs(nr))
      report(3) = ' to evaluate station '//staname(ns)
      go to 999
9972  report(1) = 'Need to supply EOF information'
      report(2) = ' for segment '//rsegs(upsegs(nr))
      report(3) = ' to evaluate station '//staname(ns)
      go to 999
9973  report(1) = 'Need to supply EOS information'
      report(2) = ' for segment '//rsegs(upsegs(nr))
      report(3) = ' to evaluate station '//staname(ns)
      go to 999

999   call stopreport(report)
      end
