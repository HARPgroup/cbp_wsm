************************************************************************
**  The purpose of this program is to calculate statitstics for       **
**  hyfrological calibration                                          **
**                                                                    **
**  The ststistics are calculated for both base variables and         **
**     log transformed variables                                      **
************************************************************************
      subroutine VAstats(ob,si,qo,qs,bo,bs,
     I                   nday2,rscen,rseg,year1,year2,
     I                   obyear,obmonth,obday,
     I                   npeaks)

      implicit none
      include 'Rstats.inc'

      integer ndays, nday2    ! number of days

      character*4 suffix
      data suffix /'VAst'/

      character*100 pfname

      character*4 cy1,cy2

      integer year1,year2             ! first and last year to average

      integer nd ! index

*******  regression variables
      real obs(ndaymax),sim(ndaymax)   ! all flow
      real ob(ndaymax),si(ndaymax)   ! all flow
      real qo(ndaymax),qs(ndaymax) ! baseflow
      real bo(ndaymax),bs(ndaymax) ! baseflow
      real bias,lbias     ! relative bias
      real olvar,slvar,elvar  ! obs, sim, error variances
      real ovar,svar,evar  ! obs, sim, error variances
      real obar,sbar,olbar,slbar       ! means
      real nsel,nse      ! nash-sutcliff efficiency
      real r,rl  ! correlation of log transforms
      real correl,variance,errorvar,mean
      external correl,variance,errorvar,mean

*******  variables needed to calculate recession indices
      real allqt(ndaymax),allqtplus1(ndaymax)  !  Qt and Qt+1
      integer nqtsim,nqtobs  ! number of days to regress
      real qminus1,q,qplus1  ! temp variables

      real mobs,msim,robs,rsim
      real indexindex  ! simulated index / observed index

      real mobs2,msim2  ! mean of individual indices
      real mobs3,msim3  ! median of individual indices
      real mobs4,msim4  ! mean of individual Winter indices
      real mobs5,msim5  ! median of individual Winter indices
      real mobs6,msim6  ! mean of individual Summer indices
      real mobs7,msim7  ! median of individual Summer indices
      real indind2,indind3 ! sim index / obs index
      real indind4,indind5 ! Winter sim index / obs index
      real indind6,indind7 ! Summer sim index / obs index

******** variables for monthly efficiencies
      real monobs(nyearmax*12),monsim(nyearmax*12)
      real nseM, ovarM, evarM ! monthly 
      integer ny,nm,ndy ! calendar
      integer month,lastmonth

******** variables for biases
      real allobs,allsim
      real allqobs,allqsim

******** variables for seasonal biases
      real winterobs,wintersim,winterbias
      real summerobs,summersim,summerbias
      real summerstormobs,summerstormsim,summerstormbias
      integer nsummerdays,nwinterdays

******** variables for lowest 50% and highest 10% bias
      real low50obs,low50sim,low50bias
      real hi10obs,hi10sim,hi10bias
      integer obsord(ndaymax)
      integer simord(ndaymax)

****** variables for investigation of peaks
      integer npeaksmax, npeaks, npobs, npsim ! peaks to investigate
      parameter (npeaksmax=100)
      real obspeak(npeaksmax), simpeak(npeaksmax)  ! max values
      integer obspeakday(npeaksmax), simpeakday(npeaksmax) ! day

      integer np,np2 ! indices

      integer npmatch  ! number of days that match
      real obsmatch(npeaksmax),simmatch(npeaksmax)  ! value for day
      integer matchday(npeaksmax)                   ! day of match

      integer np1match ! number of days that are one day off
      real obs1match(npeaksmax),sim1match(npeaksmax) ! values
      integer match1day(npeaksmax,2)     ! days (obs,sim)

      integer np2match ! number of days that are one day off
      real obs2match(npeaksmax),sim2match(npeaksmax) ! values
      integer match2day(npeaksmax,2)     ! days (obs,sim)

      integer nNoMatchObs,nNoMatchSim  ! number of days that don't match
      real obsNoMatch(npeaksmax),simNoMatch(npeaksmax)
      real simforobs(npeaksmax),obsforsim(npeaksmax)
      integer obsNoMatchDay(npeaksmax),simNoMatchDay(npeaksmax)

      logical foundobs,foundsim(npeaksmax)  ! found match?

      real allsummerobs(ndaymax),allsummersim(ndaymax)
      real VPobs,VPsim   ! volume of peaks on matching days
      real summerVPobs,summerVPsim   ! volume of peaks on summer days
      real vpbias, summervpbias  ! volumes of peak and summer peak bias
      real Pobs, Psim ! peak heights
      real summerPobs,summerPsim ! peak heights

      integer npeaksmatch, nsummerpeaksmatch  ! storage for npmatch

******************* END DECLARATIONS ***********************************

      ndays = nday2
      do nd = 1,ndays    ! variables will be modified
        obs(nd) = ob(nd)   ! so store them in alternate variables
        sim(nd) = si(nd)
        qobs(nd) = qo(nd)
        qsim(nd) = qs(nd)
        bobs(nd) = bo(nd)
        bsim(nd) = bs(nd)
      end do

*********** sort the observed lowest to highest
********* this will help in hi/low flow
******** and allow for adding without much underflow error
      call qsortr(obsord,ndays,obs) ! sort (does not destroy order)
      call qsortr(simord,ndays,sim) ! sort (does not destroy order)

********* calculate overall and quickflow biases
********** uses sorted data to avoid underflow problem
      allobs = 0.0
      allsim = 0.0
      do nd = 1,ndays
        allobs = allobs + obs(obsord(nd))
        allqobs = allqobs + qobs(obsord(nd))
        allsim = allsim + sim(simord(nd))
        allqsim = allqsim + qsim(simord(nd))
      end do

********* calculate biases for low 50% and high 10% of observed data
      low50obs = 0.0
      low50sim = 0.0
      hi10obs = 0.0
      hi10sim = 0.0
      do nd = 1,ndays/2
        low50obs = low50obs + obs(obsord(nd))
        low50sim = low50sim + sim(simord(nd))
      end do
      if (low50obs.gt.0) then
        low50bias = (low50sim-low50obs)/low50obs
      else
        low50bias = -9
      end if
      do nd = 9*ndays/10,ndays
        hi10obs = hi10obs + obs(obsord(nd))
        hi10sim = hi10sim + sim(simord(nd))
      end do
      if (hi10obs.gt.0) then
        hi10bias = (hi10sim-hi10obs)/hi10obs
      else
        hi10bias = -9
      end if

*********** calculate winter bias
      nwinterdays = 0
      winterobs = 0.0
      wintersim = 0.0
      do nd = 1,ndays
        if (obmonth(nd).eq.12.or.obmonth(nd).le.2) then
          nwinterdays = nwinterdays + 1
          winterobs = winterobs + obs(nd)
          wintersim = wintersim + sim(nd)
        end if
      end do
      if (winterobs.le..0001) then
        winterbias = -9.0
      else
        winterbias = (wintersim-winterobs)/winterobs
      end if

*********** calculate summer bias
      nsummerdays = 0
      summerobs = 0.0
      summersim = 0.0
      do nd = 1,ndays
        if (obmonth(nd).ge.6.and.obmonth(nd).le.8) then
          nsummerdays = nsummerdays + 1
          summerobs = summerobs + obs(nd)
          summersim = summersim + sim(nd)
          summerstormobs = summerstormobs + qobs(nd)
          summerstormsim = summerstormsim + qsim(nd)
        end if
      end do
      if (summerobs.le..0001) then
        summerbias = -9.0
      else
        summerbias = (summersim-summerobs)/summerobs
      end if
      if (summerstormobs.le..0001) then
        summerstormbias = -9.0
      else
        summerstormbias = (summerstormsim-summerstormobs)/summerstormobs
      end if

************ calculate efficiency
      ovar = variance(obs,ndays,ndaymax,err)
      if (err.ne.0) continue
      evar = errorvar(obs,sim,ndays,ndaymax,err)
      if (err.ne.0) continue

      if (ovar.le.0.0001) then
        nse = -9
      else
        nse = (ovar-evar)/ovar
      end if

**************  calculate recession indices
      obar = allobs/real(ndays)
      nqtobs = 0      ! observed first
      do nd = 2,ndays-1
        qplus1  = bobs(nd+1)
        q       = bobs(nd)
        qminus1 = bobs(nd-1)
        if (q.lt.qminus1) then  ! not peak and falling
          if (qplus1.lt.q) then  ! still decending
            if (qplus1.gt.0.01*obar) then  ! not at rock bottom
              nqtobs = nqtobs + 1 
              allqt(nqtobs) = q
              allqtplus1(nqtobs) = qplus1
            end if
          end if
        end if
      end do
      if (nqtobs.gt.0) then
        call calcRC(allqt,allqtplus1,nqtobs,ndaymax,
     O              mobs2,mobs3,err)
        if (err.ne.0) go to 993
      else
        mobs2 = -9.0
        mobs3 = -9.0
      end if

      sbar = allsim/real(ndays)
      nqtsim = 0      ! simulated
      do nd = 2,ndays-1
        qplus1  = bsim(nd+1)
        q       = bsim(nd)
        qminus1 = bsim(nd-1)
        if (q.lt.qminus1) then  ! not peak and falling
          if (qplus1.lt.q) then  ! still decending
            if (qplus1.gt.0.01*sbar) then  ! not at rock bottom
              nqtsim = nqtsim + 1 
              allqt(nqtsim) = q
              allqtplus1(nqtsim) = qplus1
            end if
          end if
        end if
      end do
      if (nqtsim.gt.0) then
        call calcRC(allqt,allqtplus1,nqtsim,ndaymax,
     O              msim2,msim3,err)
        if (err.ne.0) go to 994
      else
        msim2  = -9.0
        msim3  = -9.0
      end if

      if (mobs2.le.0.) then
        indind2 = -9.0
      else
        indind2 = (msim2-mobs2)/mobs2
      end if
      if (mobs3.le.0.) then
        indind3 = -9.0
      else
        indind3 = (msim3-mobs3)/mobs3
      end if

****************** find the peaks of the simulated and oberved    **
      call getpeaks(obs,ndays,ndaymax,npeaks,npeaksmax,
     O              obspeak,obspeakday,npobs)

      call getpeaks(sim,ndays,ndaymax,npeaks,npeaksmax,
     O              simpeak,simpeakday,npsim)


****************** find matching days ************
      do np2 = 1,npsim
        foundsim(np2) = .false.
      end do

      npmatch = 0
      Psim = 0.0
      Pobs = 0.0
      do np = 1,npobs
        do np2 = 1,npsim
          if (obspeakday(np).eq.simpeakday(np2)) then !update match vars
            npmatch = npmatch + 1
            obsmatch(npmatch) = obspeak(np)
            simmatch(npmatch) = simpeak(np2)
            Pobs = Pobs + obspeak(np)
            Psim = Psim + simpeak(np)
            matchday(npmatch) = obspeakday(np)
          end if
        end do
      end do
 
      if (npmatch.gt.0) then
        call calcVApbias(
     I                   obs,sim,qobs,qsim,
     I                   npeaksmax,matchday,npmatch,ndays,
     O                   VPobs,VPsim)
        vpbias = (VPsim-VPobs)/VPobs
      else
        vpbias = -9
      end if

      npeaksmatch = npmatch
      
****************** find summer storms only ************
      do nd = 1,ndays
        if (obmonth(nd).ge.6.and.obmonth(nd).le.8) then
          allsummerobs(nd) = obs(nd)  ! store for peaks
          allsummersim(nd) = sim(nd)
        else
          allsummerobs(nd) = 0.1
          allsummersim(nd) = 0.1
        end if
      end do
****************** find the peaks of the simulated and oberved    **
      call getpeaks(allsummerobs,nsummerdays,ndaymax,npeaks,npeaksmax,
     O              obspeak,obspeakday,npobs)

      call getpeaks(allsummersim,nsummerdays,ndaymax,npeaks,npeaksmax,
     O              simpeak,simpeakday,npsim)

      do np2 = 1,npsim
        foundsim(np2) = .false.
      end do

      npmatch = 0
      summerPsim = 0.0
      summerPobs = 0.0
      do np = 1,npobs
        do np2 = 1,npsim
          if (obspeakday(np).eq.simpeakday(np2)) then !update match vars
            npmatch = npmatch + 1
            obsmatch(npmatch) = obspeak(np)
            simmatch(npmatch) = simpeak(np2)
            summerPobs = summerPobs + obspeak(np)
            summerPsim = summerPsim + simpeak(np)
            matchday(npmatch) = obspeakday(np)
          end if
        end do
      end do
 
      if (npmatch.gt.0) then
        call calcVApbias(
     I                   obs,sim,qobs,qsim,
     I                   npeaksmax,matchday,npmatch,ndays,
     O                   summerVPobs,summerVPsim)
        summervpbias = (summerVPsim-summerVPobs)/summerVPobs
      else
        summervpbias = -9
      end if
      
      nsummerpeaksmatch = npmatch

***************** Jeff Load file
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2
      pfname = outdir//'river/stats/'//rscen(:lenrscen)//'/'//
     .         rseg(:lenrseg)//'_'//cy1//'_'//cy2//'.'//suffix
      open (pltfil,file = pfname, status = 'unknown',iostat = err)
      if (err.ne.0) goto 992

      write(pltfil,*,err=951) suffix//',            sim,        obs',
     .                        ',          ndays'
      write(pltfil,*,err=951) 'Volume (cfs-days),',
     .                 allsim,',',allobs,',',ndays
      write(pltfil,*,err=951) 'Lowest 50% flows (cfs-days),',
     .                 low50sim,',',low50obs,',',ndays/2
      write(pltfil,*,err=951) 'Highest 10% flows (cfs-days),',
     .                 hi10sim,',',hi10obs,',',ndays/10
      write(pltfil,*,err=951) 'Storm Volume (cfs-days),',
     .                 VPsim,',',VPobs,',',npeaksmatch
      write(pltfil,*,err=951) 'Total of Peaks (cfs-days),',
     .                 Psim,',',Pobs,',',npeaksmatch
      write(pltfil,*,err=951) 'Mean Baseflow Recession,',msim2,',',mobs2
      write(pltfil,*,err=951) 'Median Baseflow Recession,',
     .                 msim3,',',mobs3
      write(pltfil,*,err=951) 'Summer Volume (cfs-days),',
     .                 summersim,',',summerobs,',',nsummerdays
      write(pltfil,*,err=951) 'Winter Volume (cfs-days),',
     .                 wintersim,',',winterobs,',',nwinterdays
      write(pltfil,*,err=951) 'Summer Storm Volume (cfs-days),',
     .                 summerVPsim,',',summerVPobs,',',nsummerpeaksmatch
      write(pltfil,*,err=951) 'efficiency,',nse

      close (pltfil)

      return

************************ error reporting
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

992   report(1) = 'Problem opening load files for segment '//rseg
      report(2) = pfname
      report(3) = '  Error =    '
      write(report(3)(11:13),'(i3)') err
      go to 999

993   report(1) = 'Problem with calculating recession index'
      report(2) = ' for observed data'
      report(3) = '  for segment '//rseg
      go to 999

994   report(1) = 'Problem with calculating recession index'
      report(2) = ' for simulated data'
      report(3) = '  for segment '//rseg
      go to 999

997   report(1) = 'problem with finding stats with observed'
      report(2) = '  Divide by zero'
      report(3) = '  for segment '//rseg
      go to 999
 
999   call stopreport(report)

      end

************************************************************************
** function to calculate the total flow area of the peaks that occur  **
**  on the same day.                                                  **
************************************************************************
      subroutine calcVApbias(
     I                       obs,sim,qobs,qsim,
     I                       npeaksmax,matchday,npmatch,ndays,
     O                       VPobs,VPsim)

      implicit none
      include 'Rstats.inc'

      real obs(ndaymax),sim(ndaymax)   ! all flow

      integer ndays    ! number of days

      integer nd,np ! index

      integer year1  ! start year

****** variables for investigation of peaks
      integer npeaksmax

      integer npmatch  ! number of days that match
      integer matchday(npeaksmax)                   ! day of match

      real sumo(npeaksmax),sums(npeaksmax)

      logical descend

      real VPsim, VPobs ! volume of peaks, simulated and observed

******************* END DECLARATIONS ***********************************

************ loop over storm events
************* for each storm, sum the daily flows before and after the
************** peak until qobs increases or is less than 10% of flow
      do np = 1,npmatch
        nd = matchday(np)
        sumo(np) = obs(nd)
        descend = .true.
        do while (descend)
          nd = nd + 1
          sumo(np) = sumo(np) + obs(nd)
          if (qobs(nd+1).gt.qobs(nd)) descend = .false.
          if (qobs(nd+1)/obs(nd+1).lt.0.1) descend = .false.
          if (nd.ge.ndays) descend = .false.
        end do
        descend = .true.
        nd = matchday(np)
        do while (descend)
          nd = nd - 1 
          if (nd.eq.1) exit
          sumo(np) = sumo(np) + obs(nd)
          if (qobs(nd-1).gt.qobs(nd)) descend = .false.
          if (qobs(nd-1)/obs(nd-1).lt.0.1) descend = .false.
          if (nd.le.2) descend = .false.
        end do
      end do

      do np = 1,npmatch
        nd = matchday(np)
        sums(np) = sim(nd)
        descend = .true.
        do while (descend)
          nd = nd + 1
          sums(np) = sums(np) + sim(nd)
          if (qsim(nd+1).gt.qsim(nd)) descend = .false.
          if (qsim(nd+1)/sim(nd+1).lt.0.1) descend = .false.
          if (nd.ge.ndays) descend = .false.
        end do
        descend = .true.
        nd = matchday(np)
        do while (descend)
          nd = nd - 1
          if (nd.eq.1) exit
          sums(np) = sums(np) + sim(nd)
          if (qsim(nd-1).gt.qsim(nd)) descend = .false.
          if (qsim(nd-1)/sim(nd-1).lt.0.1) descend = .false.
          if (nd.le.2) descend = .false.
        end do
      end do

      VPobs = 0.0
      VPsim = 0.0
      do np = 1,npmatch  ! find average, kept them separate for future
        VPobs = VPobs + sumo(np)
        VPsim = VPsim + sums(np)
      end do


      return

      end


      

      

      

