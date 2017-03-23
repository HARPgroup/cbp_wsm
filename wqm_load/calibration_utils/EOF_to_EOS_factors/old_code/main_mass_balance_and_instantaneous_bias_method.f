************************************************************************
** program to make subgrid transport factors to convert EOF to EOS    **
**  for nitrogen and phosphorous.                                     **
**                                                                    **
**  Previous Efforts                                                  **
**   a priori estimates from ESTIMATOR and sparrow have not worked    **
**     likely due to combined errors from those two models, phase 5,  **
**     and the unaccounted regional biases, which we are trying to    **
**     address with this method.                                      **
**   Correcting using the median concentration bias for each station  **
**     worked reasonably well as an interim method, but lacked a full **
**     coherent treatment of the effect of correcting the bias of     **
**     upstream stations and the effect of point source, atmospheric  **
**     deposition, and septic.                                        **
**                                                                    **
** This method is based on the following:                             **
**  To correct a load bias at a station, assuming the delivery        **
**    factors do not change, the total loads input to the rivers must **
**    change by the inverse of the bias.  This ignores the relative   **
**    placement in the upstream watershed of the different load       **
**    sources, but this assumption probably does not cause large      **
**    problems with the overall analysis.                             **
**                                                                    **
**  A mass balance is performed around a sub-watershed, which is      **
**   defined as the watershed between a calibration site and any      **
**   upstream calibration sites                                       **
**                                                                    **
** Variables:                                                         **
**   Si  = Simulated load at any upstream points                      **
**   Bsi = Bias of sum of instantaneous load at upstream points       **
**   Bso = Bias of outlet point ( 1 = no bias )                       **
**   P   = Point sources in the sub-watershed                         **
**   A   = Direct atmospheric deposition to water in the sub-watershed**
**   S   = Septic in the sub-watershed                                **
**   E   = Sum of existing edge-of-stream loads in the sub-watershed  **
**   T   = Subgrid transport factor (output of program)               **
**                                                                    **
** Logic:                                                             **
**   If observed bias at the outlet is Bso, then, assuming equal      **
**     delivery factors, we would need to divide all inputs by Bso to **
**     acheive a balance.  Since the existing EOS loads (E) are the   **
**     only modified item, the math works out like so:                **
**                                                                    **
**   Needed inputs = (Si + P + A + S + E) / Bso                       **
**   Inputs        = Si/Bsi + P + A + S + E*T                         **
**   Equating and solving for T:                                      **
**    T = (Si*(1-Bso/Bsi) + (P+A+S)(1-Bso) + E) / (E * Bso)           **
**                                                                    **
**  logic test: with no Si,P,A,S, this reduces to 1/Bso               **
**   The best representation of bias was found to be the average bias **
**   of the top four quintiles of the cfd for TN and the top quintile **
**   for TP.                                                          **
************************************************************************
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/rsegs.inc'

********** input info
      integer year1,year2  ! start and stop year for data
      character*25 basin ! basin.riv under seglists directory

********** specifications for data sets
      character*20 datatype, spec
      character*20 wqparam, biasMethod
      integer lenwq, lenbias, lenbasin

*********** load types
      integer TN,TP,nloads,nl
      parameter (nloads=2,TN=1,TP=2)
      character*4 loadname(nloads),tempname,units
      data loadname /'TOTN','TOTP'/

********** calibration station variables
      integer maxstations, nstations(nloads), ns  
      parameter (maxstations = 300)
      character*13 staName(maxstations,nloads),rsegsnr
      real staBias(maxstations,nloads)
      real staLoad(maxstations,nloads)
      real staObs(maxstations,nloads)
      real ksStat  ! just read to see if any data
      integer nbins,nbinsmax,nb
      parameter (nbinsmax=5)
      real log10bias(nbinsmax)  ! bias from file

************* variables to find sub-watersheds
      integer nupstations,nups ! number of upstream stations
      integer upstations(maxstations)
      integer nupsegs  ! number of upstream segs (DS of other stations)
      integer upsegs(maxrsegs) ! upstream segs indices

********** mass balance variables for whole domain
      real EOSps(maxrsegs,nloads) ! Point sources 
      real EOSatdep(maxrsegs,nloads) ! Direct atdep to water
      real EOSsep(maxrsegs,nloads) ! Septic 
      real EOSland(maxrsegs,nloads) ! edge-of-stream from land
      real sgtf(maxrsegs,nloads)

******** accumulator variables for each sub-watershed
      real Si,P,A,S,E,Bso,Bsi,denom
      real sgtfbasin

******************  useful variables
      integer nr,n
      real sim,obs
      character*4 cy1,cy2 ! character representation of year
      character*4 dummy

********** utilities
      integer i
      logical comment
      external comment

************* END DECLARATIONS *****************************************

********* GET INPUT FROM USER
      read*,rscen,basin,year1,year2  ! specifier for whole run
      call lencl(rscen,lenrscen)
      call lencl(basin,lenbasin)

      write(cy1,'(i4)')year1
      write(cy2,'(i4)')year2

C      print*, 'reading data files'
*********** GET RSEGS FROM CATALOG FILE
      print*,'getting river segments'
      call readRiverSeglist(
     I                      basin,
     O                      rsegs,nrsegs)
*********** populate uniqid, dsid, uniqindex
      do ns = 1,supermax
        uniqindex(ns) = 0
      end do
      do nr = 1,nrsegs
        read(rsegs(nr)(5:8),'(i4)') uniqid(nr)
        read(rsegs(nr)(10:13),'(i4)') dsid(nr)
        uniqindex(uniqid(nr)) = nr
      end do

************ READ DATA FILES

************ populate nstations, staname, stabias, staload, staObs
********** from cfd and load files
      print*,'getting river loads and biases'
      do nl = 1,nloads
        nstations(nl) = 0
        do nr = 1,nrsegs
          do i = 1,2     ! once for regular seg, once for double

            rsegsnr = rsegs(nr)
            if (i.eq.2) rsegsnr(10:13) = '0003'

            fnam = outdir//'river/cfd/'//rscen(:lenrscen)//'/'//
     .             rsegsnr//'_'//cy1//'_'//cy2//'.'//
     .             loadname(nl)
            open(11,file=fnam,status='old',iostat=err)
            if (err.ne.0) cycle

            read(11,*,err=993) ksStat
            if (abs(ksStat+999).lt.1) then  ! no data
              close(11)
              cycle
            end if

************ cfd file checks out, store this station info
            nstations(nl) = nstations(nl) + 1
            ns = nstations(nl)
            staName(nstations(nl),nl) = rsegsnr

************* get cfd statistics and number of observations
            read(11,*,err=993) nbins ! number of bins
            if (nbins.gt.nbinsmax) go to 998
            read(11,*,err=993) dummy ! header
            do nb = 1,nbins
              read(11,*,err=993) n,sim,obs,log10bias(nb)
              if (n.ne.nb) go to 993
            end do
            read(11,*,err=993) staObs(ns,nl)

            if (staObs(ns,nl).lt.50) then  ! min number of observations
              nstations(nl) = nstations(nl) - 1
              cycle
            end if



************** for explanation of why the top four quintiles were
************ chosen for nitrogen and the top quintile was chosen for
************ phosphorus, see documentation.  Generally, they were best
************ fit to the independent 'eyeball' assessment
            if (nl.eq.TN) then
              staBias(ns,nl) = 0.0
              do nb = 2,5
                staBias(ns,nl) = staBias(ns,nl) + 10.0**log10bias(nb)
              end do
              staBias(ns,nl) = staBias(ns,nl) / 4.0
            else if (nl.eq.TP) then
              staBias(ns,nl) = 10.0**log10Bias(5)
            end if

            close(11)

******************** get simulated load
            fnam = outdir//'river/aveann/'//rscen(:lenrscen)//'/'//
     .             rsegsnr//'_'//cy1//'_'//cy2//'.ave'
            open(11,file=fnam,status='old',iostat=err)
            if (err.ne.0) go to 991

            read(11,*,err=993) dummy ! header line
            do 
              read(11,*,err=993,end=990) tempname,units,staLoad(ns,nl)
              if (tempname.eq.loadname(nl)) exit
            end do
            close(11)

          end do
        end do
      end do

      do nl = 1,nloads
        do ns = 1,nstations(nl)
          print*,nl,ns,' ',staName(ns,nl),staBias(ns,nl)
        end do
      end do

************ call subroutine to populate EOS from land, point sources
**************** atdep, and septic
      print*,'getting eos loads from eos wdms'
      call getEOSload(
     I                rscen,rsegs,nrsegs,year1,year2,
     I                nloads,loadname,
     O                EOSland,EOSsep,EOSatdep,EOSps)

************* DETERMINE SGTF *******************************************
************* loop over estimated bias points
      print*,'determining SGTF'

      do nl = 1,nloads
        do nr = 1,nrsegs 
          sgtf(nr,nl) = -999.0
        end do
      end do

********** write out stations for check
      fnam = pardir//'transport/calc_factors/'//
     .       rscen(:lenrscen)//'_'//basin(:lenbasin)//
     .       '_sgtf_check_stations.csv'
      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      do nl = 1,nloads
        do ns = 1,nstations(nl)

C        call ttyput(staname(ns))
C        call ttyput(' ')

************ get upstream list non-inclusive of other estimated points
          call findupstream(
     I                      staname(ns,nl),nloads,nl,
     I                      maxstations,staname,nstations,
     I                      rsegs,uniqid,dsid,uniqindex,nrsegs,
     O                      upsegs,nupsegs,
     O                      upstations,nupstations)

***************** write out stations for check
          write(11,*,err=951)'STATION ',staname(ns,nl),
     .                       ' for ',loadname(nl)
          do nups = 1,nupsegs
            write(11,*,err=951)'  upstream segment ',rsegs(upsegs(nups))
     .                 ,' ',staname(ns,nl),' ',loadname(nl)
          end do
          do nups = 1,nupstations
            write(11,*,err=951)'    upstream station ',
     .                 staName(upstations(nups),nl),
     .                 ' ',staname(ns,nl),' ',loadname(nl)

          end do
           

************** calculate factor and store for each segment
********** T = (Si*(1-Bso/Bsi) + (P+A+S)(1-Bso) + E) / (E * Bso)
          Bso = staBias(ns,nl)
          sgtfbasin = 0.0
          do nups = 1,nupstations
            Si = staLoad(upstations(nups),nl)
            Bsi = staBias(upstations(nups),nl)
            sgtfbasin = sgtfbasin + Si*(1.0-Bso/Bsi)
          end do
          do nups = 1,nupsegs
            P = EOSps(upsegs(nups),nl)
            A = EOSatdep(upsegs(nups),nl)
            S = EOSsep(upsegs(nups),nl)
            E = EOSland(upsegs(nups),nl)
            sgtfbasin = sgtfbasin + (P+A+S)*(1.0-Bso) + E
          end do
          denom = 0.0
          do nups = 1,nupsegs
            denom = denom + EOSland(upsegs(nups),nl)*Bso
          end do
          sgtfbasin = sgtfbasin / denom


          sgtfbasin = max(sgtfbasin,0.25)
          sgtfbasin = min(sgtfbasin,4.0)
        
          do nr = 1,nupsegs
            sgtf(upsegs(nr),nl) = sgtfbasin
          end do

        end do
      end do
      close(11)

********** open output file and transport factor file
      fnam = pardir//'transport/calc_factors/'//
     .       rscen(:lenrscen)//'_'//basin(:lenbasin)//
     .       '_sgtf.csv'
      print*,'writing output ',fnam
      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(11,*,err=951)'seg ',(',',loadname(nl),nl=1,nloads)
      do nr = 1,nrsegs
        write(11,1234,err=951)rsegs(nr),sgtf(nr,1),sgtf(nr,2)
      end do
      close(11)

      stop
1234  format(a13,2(',',f10.4))
***************** ERROR SPACE ******************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

990   report(1) = 'Problem reading file:'
      report(2) = fnam
      report(3) = 'file ended before load '//loadname(nl)//' found'
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
      report(3) = 'trouble reading '
      go to 999

994   report(1) = 'Problem in file '
      report(2) = fnam
      report(3) = 'trouble reading loads'
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
      report(3) = ' to evaluate station '//staname(ns,nl)
      go to 999
9972  report(1) = 'Need to supply EOF information'
      report(2) = ' for segment '//rsegs(upsegs(nr))
      report(3) = ' to evaluate station '//staname(ns,nl)
      go to 999
9973  report(1) = 'Need to supply EOS information'
      report(2) = ' for segment '//rsegs(upsegs(nr))
      report(3) = ' to evaluate station '//staname(ns,nl)
      go to 999

998   report(1) = 'cfd postprocessor has more bins than expected'
      report(2) = 'check '//fnam
      report(3) = 'or modify the nbinsmax in this program'
      go to 999

999   call stopreport(report)
      end
