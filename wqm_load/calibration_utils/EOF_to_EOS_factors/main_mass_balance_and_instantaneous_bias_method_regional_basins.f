************************************************************************
** program to make regional adjustment factors to convert EOF to EOS  **
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
**   T   = Regional Adjustment factor (output of program)             **
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
**                                                                    **
** This is the same as run_subgrid_with_mass_balance_and_             **
**      instantaneous_bias_method.csh except that there is a pre-     **
**      selected group of stations where the adjustment occurs        **
**      and the bias is judged by estimator preferentially            **
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
      real EstBias, CFDBias
      real staBias(maxstations,nloads)
      real staLoad(maxstations,nloads)
      real nobs
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
      real Siup(maxstations),Bsiup(maxstations)

******************  useful variables
      integer nr,n
      real sim,obs
      character*4 cy1,cy2 ! character representation of year
      character*4 dummy

********** utilities
      integer i
      logical comment,found
      external comment
      logical foundEst,foundCFD

********* variables to specify which basins could be used, given
******** sufficient data.  The regional adjustment factors are applied 
******* on a wider scale than each station
      integer nRAbasins, nrab
      character*13 RAbasins(maxstations)
      character*25 calscen

      character*2 version
      integer iversion
************* END DECLARATIONS *****************************************

********* GET INPUT FROM USER
      read*,rscen,calscen,basin,year1,year2  ! specifier for whole run
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
********** for the 'big basins' version, use only the stations specified
********** in the list
********* read in the list
      call getRAbasins(
     I                 calscen,maxstations,
     O                 RAbasins,nRAbasins)

************ populate nstations, staname, stabias, staload
********** from cfd, estimator, and load files
******* if it's not a regional adjustment basin, skip it
*******  if there is an estimator file, use it
*******  if no estimator file, then use specific quintiles of the cfd
      print*,'getting river loads and biases'
      print*,'type,seg,bias,estbias,cfdbias,load'
      do nl = 1,nloads
        nstations(nl) = 0
        do nr = 1,nrsegs
          do i = 1,2     ! once for regular seg, once for double

            rsegsnr = rsegs(nr)
            if (i.eq.2) rsegsnr(10:13) = '0003'

**************** check to see if a regional adjustment basin
            found = .false.       ! only use pre-specified 'big basins'
            do nrab = 1,nRAbasins
              if (rsegsnr.eq.RAbasins(nrab)) then
                found = .true.
                exit
              end if
            end do
            if (.not.found) cycle

************** check for estimator file, save EstBias
            fnam = outdir//'river/stats/'//rscen(:lenrscen)//'/'//
     .             rsegsnr//'_'//cy1//'_'//cy2//'_annual_vs_estimator.'
     .             //loadname(nl)
            open(11,file=fnam,status='old',iostat=err)
            if (err.ne.0) then
              foundEst = .false.
              EstBias = -999.
            else
              foundEst = .true.
              do  ! read file
                read(11,'(a100)',err=992,end=994) line              
                if (line(:15).eq.' mean          ') exit
              end do
              read(line,*,err=992,end=995) dummy, sim, obs
              if (obs.gt.0.0001) then
                EstBias = sim/obs
              else
                foundEst = .false.
                EstBias = -999.
              end if
              close(11)
            end if

************** check for cfd file, save CFDBias
************** check CFD file for low obs, invalid ksStat
************** for explanation of why the top four quintiles were
************ chosen for nitrogen and the top quintile was chosen for
************ phosphorus, see documentation.  Generally, they were best
************ fit to the independent 'eyeball' assessment
            fnam = outdir//'river/cfd/'//rscen(:lenrscen)//'/'//
     .             rsegsnr//'_'//cy1//'_'//cy2//'.'//
     .             loadname(nl)
            open(11,file=fnam,status='old',iostat=err)
            if (err.ne.0) then
              foundCFD = .false.
            else
              foundCFD = .true.
              read(11,*,err=993) ksStat
              if (abs(ksStat+999).lt.1) then  ! no data
                foundCFD = .false.
              else  ! cfd file OK, read data
                read(11,*,err=993) nbins ! number of bins
                if (nbins.gt.nbinsmax) go to 998
                read(11,*,err=993) dummy ! header
                do nb = 1,nbins
                  read(11,*,err=993) n,sim,obs,log10bias(nb)
                  if (n.ne.nb) go to 993
                end do
                read(11,*,err=993) nobs
                if (nobs.lt.40) then
                  foundCFD = .false.
                else
                  if (nl.eq.TN) then
                    CFDBias = 0.0
                    do nb = 2,5
                      CFDBias = CFDBias + 10.0**log10bias(nb)
                    end do
                    CFDBias = CFDBias / 4.0
                  else if (nl.eq.TP) then
                    CFDBias = 10.0**log10Bias(5)
                  end if
                end if
              end if
              close(11)
            end if

************ if any bias information available, store this station
*************** prefer estimator
            if (.not.foundCFD .and. .not.foundEst) cycle
            nstations(nl) = nstations(nl) + 1
            ns = nstations(nl)
            staName(nstations(nl),nl) = rsegsnr
            if (foundEst) then
              staBias(ns,nl) = EstBias
            else
              staBias(ns,nl) = CFDBias
            end if

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

            print*,loadname(nl),',',rsegsnr,',',staBias(ns,nl),',',
     .             EstBias,',',CFDBias,',',StaLoad(ns,nl)

          end do
        end do
      end do

************ got all loads and biases

************ call subroutine to populate EOS from land, point sources
**************** atdep, and septic
      print*,'getting eos loads'
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
      do iversion = 0,99
        write(version,'(i2)') iversion
        if (version(1:1).eq.' ') version(1:1) = '0'
        fnam = pardir//'transport/calc_factors/'//
     .       rscen(:lenrscen)//'_'//basin(:lenbasin)//
     .       '_sgtf_check_stations_'//version//'.csv'
        open(11,file=fnam,status='old',iostat=err)
        if (err.ne.0) exit
        close(11)
        if (iversion.eq.99) go to 989
      end do
      open(11,file=fnam,status='new',iostat=err)
      if (err.ne.0) go to 991

*********** open file to write out mass balance constituents by station
      fnam = pardir//'transport/calc_factors/'//
     .       rscen(:lenrscen)//'_'//basin(:lenbasin)//
     .       '_sgtf_constituents_'//version//'.csv'
      open(12,file=fnam,status='new',iostat=err)
      if (err.ne.0) go to 991
      write(12,*,err=951)'LoadType,Station,OutBias,EOS,PS,Sep,Atdep,',
     .           'Inload,InBias,Inload2, etc'


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

          do nr = 1,nupsegs
            sgtf(upsegs(nr),nl) = sgtfbasin
          end do

*********** figure a different way to get the totals
********** temporary code to make sure that it works, then 
********** replace the above with this chunk
**    T = (Si*(1-Bso/Bsi) + (P+A+S)(1-Bso) + E) / (E * Bso)           **

          sgtfbasin = 0.0
          Bso = StaBias(ns,nl)
          do nups = 1,nupstations
            Siup(nups) = staLoad(upstations(nups),nl)
            Bsiup(nups) = staBias(upstations(nups),nl)
          end do
          P = 0.0
          A = 0.0
          S = 0.0
          E = 0.0
          do nups = 1,nupsegs
            P = P + EOSps(upsegs(nups),nl)
            A = A + EOSatdep(upsegs(nups),nl)
            S = S + EOSsep(upsegs(nups),nl)
            E = E + EOSland(upsegs(nups),nl)
          end do
          sgtfbasin = 0.0
          do nups = 1,nupstations
            sgtfbasin = sgtfbasin + Siup(nups)*(1.0-Bso/Bsiup(nups))
          end do
          sgtfbasin = (sgtfbasin + (P+A+S)*(1-Bso) + E) / (E * Bso)
          if (sgtfbasin/sgtf(upsegs(1),nl)-1.0.gt.0.01) go to 988
          write(12,1235,err=951)LoadName(nl),StaName(ns,nl),Bso,E,P,S,A,
     .                  (Siup(nups),Bsiup(nups),nups=1,nupstations)

        end do
      end do
      close(11)
      close(12)

********** open output file and transport factor file
      fnam = pardir//'transport/calc_factors/'//
     .       rscen(:lenrscen)//'_'//basin(:lenbasin)//
     .       '_sgtf_'//version//'.csv'
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
1235  format(a4,',',a13,40(',',e11.4))
***************** ERROR SPACE ******************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

988   report(1) = 'Problem with new sgtf calculations'
      write(report(2),*) 'old calculation ',sgtf(upsegs(1),nl)
      write(report(3),*) 'new calculation ',sgtfbasin
      go to 999

989   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'more than 99 versions exist'
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
