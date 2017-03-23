************************************************************************
** program to make subgrid transport factors to convert EOF to EOS    **
**  for nitrogen and phosphorous.                                     **
**                                                                    **
** This is based on the following:                                    **
**  The flux at any point in the watershed can be though of as the    **
**    sum of the upstream edge of stream loads times the transport    **
**    within the rivers down to the flux point.  The EOS loads are    **
**    edge of field loads time the sub-grid transport factor          **
**  The fluxes are known at certain points through estimator          **
**  The EOF loads are known through targets                           **
**  The riverine transport is known through sparrow                   **
**  Other EOS (point source, septic) is known                         **
**  The only unknown value is the sub-grid transport factor, which    **
**    will be assumed to be equal for all segments upstream of a flux **
**    point and downstream of any other flux point.                   **
**                                                                    **
**  The subgrid transport factor can be calculated as follows:        **
**   FluxOut - sum(FluxIn*(pi(TFs))) - sum(EOS*(pi(TFs)))             **
**   ---------------------------------------------------              **
**                 sum(EOF*(pi(TFs)))                                 **
**                                                                    **
**  The user specifies which data sets are used in the calculation    **
**    in the ./run/control/calib/subgridtf/$calscen.con  file         **
**    which references file in the ./pp/data/transport/calc_factors/  **
**    directory.                                                      **
**                                                                    **
**  this always runs on the whole model domain because the chance of  **
**    errors is too great when passing a segment list                 **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/rsegs.inc'

********** input info
      character*20 calscen ! file under ./run/control/calib/calc_factors
      integer lenscen 

********** specifications for data sets
      character*20 datatype, spec
      character*20 wqparam, EOFscen, EOSscen, attenscen, estscen
      integer lenwq, leneof, leneos, lenatt, lenest

********** output subgrid transport factor
      real sgtf(maxrsegs)
      real numerator, denominator, dstf

******** output fraction of EOF -- measure of accuracy
      real fracEOF(maxrsegs)
      real numf, denomf

********** flux point variables
      integer maxstations, nstations, ns  ! estimator flux points
      parameter (maxstations = 100)
      character*13 staname(maxstations)
      real flux(maxstations)
      real upflux  ! upstream flux
      integer nupstations,nups      ! number of upstream stations
      integer upstations(maxstations)
      integer nupsegs  ! number of upstream segs (DS of other stations)
      integer upsegs(maxrsegs) ! upstream segs indices


********* properties of each river
      real attenuation(maxrsegs)  ! attenuation factor
      real EOF(maxrsegs)   ! eof land loads
      real EOS(maxrsegs)   ! other eos (atdep, pointsource, septic)

******************  useful variables
      integer nr
      real value 
      integer Tuniqid

********** utilities
      logical comment
      external comment

      real stationdf,segdf
      external stationdf,segdf

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
      EOFscen = '-'
      EOSscen = '-'
      attenscen = '-'
      estscen = '-'
      do
        read(11,'(a100)',end=111,err=992) line
        call d2x(line,last)
        if (comment(line)) cycle
        if (line(1:4).eq.'    ') cycle
        read (line,*) datatype,spec
        if (datatype(1:7).eq.'WQparam') then
          wqparam = spec
        else if (datatype(1:3).eq.'EOF') then
          EOFscen = spec
        else if (datatype(1:3).eq.'EOS') then
          EOSscen = spec
        else if (datatype(1:11).eq.'attenuation') then
          attenscen = spec
        else if (datatype(1:14).eq.'estimator_data') then
          estscen = spec
        else
          go to 993
        end if
      end do

111   close(11)

      if (wqparam.eq.'-') go to 9941
      if (EOFscen.eq.'-') go to 9942
      if (EOSscen.eq.'-') go to 9943
      if (attenscen.eq.'-') go to 9944
      if (estscen.eq.'-') go to 9945

      call lencl(wqparam,lenwq)
      call lencl(EOFscen,leneof)
      call lencl(EOSscen,leneos)
      call lencl(attenscen,lenatt)
      call lencl(estscen,lenest)

************ READ DATA FILES
*********** if river not supplied in data files, 'no data -9' is kept
********** flux
      fnam = pardir//'transport/calc_factors/'//
     .       'estimator_'//WQparam(:lenwq)//'_'//
     .       estscen(:lenest)//'.csv'
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nstations = 1
      do
        read(11,'(a100)',err=992,end=222) line
        if (comment(line)) cycle
        read(line,*,err=995,end=995) staname(nstations),flux(nstations)
        nstations = nstations + 1
      end do
222   close(11)
      nstations = nstations - 1
      if (nstations.gt.maxstations) go to 996


********** river attenuation
      fnam = pardir//'transport/calc_factors/'//
     .       'attenuation_'//WQparam(:lenwq)//'_'//
     .       attenscen(:lenatt)//'.csv'
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do nr = 1,nrsegs
        attenuation(nr) = -9.0
      end do

      do
        read(11,'(a100)',err=992,end=333) line
        if (comment(line)) cycle
        read(line,*,err=995,end=995) Tseg,value
        read(Tseg(5:8),'(i4)') Tuniqid  ! get seg unique id
        attenuation(uniqindex(Tuniqid)) = value
      end do
333   close(11)

********** river EOF land loads
      fnam = pardir//'transport/calc_factors/'//
     .       'EOF_'//WQparam(:lenwq)//'_'//
     .       EOFscen(:leneof)//'.csv'
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do nr = 1,nrsegs
        EOF(nr) = -9.0
      end do

      do
        read(11,'(a100)',err=992,end=444) line
        if (comment(line)) cycle
        read(line,*,err=995,end=995) Tseg,value
        read(Tseg(5:8),'(i4)') Tuniqid  ! get seg unique id
        EOF(uniqindex(Tuniqid)) = value
      end do
444   close(11)


********** river EOS 'extra' loads
      fnam = pardir//'transport/calc_factors/'//
     .       'EOS_'//WQparam(:lenwq)//'_'//
     .       EOSscen(:leneos)//'.csv'
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do nr = 1,nrsegs
        EOS(nr) = -9.0
      end do

      do
        read(11,'(a100)',err=992,end=555) line
        if (comment(line)) cycle
        read(line,*,err=995,end=995) Tseg,value
        read(Tseg(5:8),'(i4)') Tuniqid  ! get seg unique id
        EOS(uniqindex(Tuniqid)) = value
      end do
555   close(11)


************* DETERMINE SGTF *******************************************
********** open output file and transport factor file
      fnam = pardir//'transport/calc_factors/'//
     .       calscen(:lenscen)//'_sgtf.csv'
      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      fnam = pardir//'transport/calc_factors/'//
     .       calscen(:lenscen)//'_dfs.csv'
      open(12,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      do nr = 1,nrsegs
        sgtf(nr) = -999.0
      end do

************* loop over estimator flux points
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

       write(12,*,err=951) staname(ns)
       write(12,*,err=951) nupsegs,(',',rsegs(upsegs(nr)),nr=1,nupsegs)
       write(12,*,err=951) nupstations,
     .             (',',staname(upstations(nr)),nr=1,nupstations)

        do nr = 1,nupsegs
          if (attenuation(upsegs(nr)).lt.-8.0) go to 9971
          if (EOF(upsegs(nr)).lt.-8.0) go to 9972
          if (EOS(upsegs(nr)).lt.-8.0) go to 9973
        end do

        numerator = 0.0
        denominator = 0.0
        numf = 0.0
        denomf = 0.0

************* calculate estimator portion
        numerator = flux(ns)  
        do nups = 1,nupstations
          dstf = stationdf(staname(upstations(nups)),staname(ns),
     I                     attenuation,rsegs,maxrsegs,
     I                     upsegs,nupsegs)
          write(12,*,err=951)'station2station',',',staname(ns),
     .           ',',staname(upstations(nups)),',',dstf
          numerator = numerator - flux(upstations(nups))*dstf
          denomf = denomf + flux(upstations(nups))*dstf
        end do

************** calculate eos and eof portion
        do nr = 1,nupsegs
          dstf = segdf(rsegs(upsegs(nr)),staname(ns),
     I                     attenuation,rsegs,maxrsegs,
     I                     upsegs,nupsegs)
          write(12,*,err=951)'seg2station,',staname(ns),',',
     .                       rsegs(upsegs(nr)),',',dstf
          denominator = denominator + EOF(upsegs(nr))*dstf
          numerator = numerator - EOS(upsegs(nr))*dstf
          denomf = denomf + EOS(upsegs(nr))*dstf
          denomf = denomf + EOF(upsegs(nr))*dstf
          numf = numf + EOF(upsegs(nr))*dstf
        end do

************** calculte sgtf and store for each segment
        do nr = 1,nupsegs
          sgtf(upsegs(nr)) = numerator / denominator
          fracEOF(upsegs(nr)) = numf / denomf
        end do

      end do

      write(11,*,err=951)'seg, sgtf, fracEOF'
      do nr = 1,nrsegs
        write(11,*,err=951)rsegs(nr),',',sgtf(nr),',',fracEOF(nr)
      end do
      close(11)
      close(12)

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
