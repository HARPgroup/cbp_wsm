************************************************************************
** This subroutine compares the peaks of the simulated and oberved    **
************************************************************************
      subroutine peaks(obs,sim,qobs,qsim,ndays,rscen,rseg,npeaks,
     .           year1,year2,obyear,obmonth,obday)
      implicit none
      include 'Rstats.inc'

      integer ndays    ! number of days

      integer nd ! index

      integer year1,year2  ! start year
      character*4,cyear1,cyear2

*******  regression variables
      real obs(ndaymax),sim(ndaymax)   ! all flow

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

      real correl,variance,errorvar,mean
      external correl,variance,errorvar,mean

      real r,evar  ! correlation and error variance
      real ovar,svar  ! obs, sim variances
      real obar,sbar       ! means
      real nse   ! nash sutcliffe efficiency

      integer iyear,imonth,iday

      real pbias, vpbias  ! peak bias and volume of peak bias

      real calcvpbias
      external calcvpbias


******************* END DECLARATIONS ***********************************
      if (npeaks.ge.npeaksmax) go to 991

      call getpeaks(obs,ndays,ndaymax,npeaks,npeaksmax,
     O              obspeak,obspeakday,npobs)

      call getpeaks(sim,ndays,ndaymax,npeaks,npeaksmax,
     O              simpeak,simpeakday,npsim)


****************** FIND MATCHING DAYS AND NON-MATCHING DAYS ************
      do np2 = 1,npsim
        foundsim(np2) = .false.
      end do

      npmatch = 0
      np1match = 0
      np2match = 0
      nNoMatchObs = 0
      do np = 1,npobs
        foundobs = .false.
        do np2 = 1,npsim
          if (obspeakday(np).eq.simpeakday(np2)) then !update match vars
            foundobs = .true.
            foundsim(np2) = .true.
            npmatch = npmatch + 1
            obsmatch(npmatch) = obspeak(np)
            simmatch(npmatch) = simpeak(np2)
            matchday(npmatch) = obspeakday(np)
          end if
        end do
        do np2 = 1,npsim
          if (abs(obspeakday(np)-simpeakday(np2)).eq.1) then 
            foundobs = .true.                         !update match vars
            foundsim(np2) = .true.
            np1match = np1match + 1
            obs1match(np1match) = obspeak(np)
            sim1match(np1match) = simpeak(np2)
            match1day(np1match,1) = obspeakday(np)
            match1day(np1match,2) = simpeakday(np2)
          end if
        end do
        do np2 = 1,npsim
          if (abs(obspeakday(np)-simpeakday(np2)).eq.2) then 
            foundobs = .true.                         !update match vars
            foundsim(np2) = .true.
            np2match = np2match + 1
            obs2match(np2match) = obspeak(np)
            sim2match(np2match) = simpeak(np2)
            match2day(np2match,1) = obspeakday(np)
            match2day(np2match,2) = simpeakday(np2)
          end if
        end do
        if (.not.foundobs) then  ! update obs non-match vars
          nNoMatchObs = nNoMatchObs + 1
          obsNoMatch(nNoMatchObs) = obspeak(np)
          simforobs(nNoMatchObs) = sim(obspeakday(np))
          obsNoMatchDay(nNoMatchObs) = obspeakday(np)
        end if
      end do

      nNoMatchSim = 0
      do np2 = 1,npsim
        if (.not.foundsim(np2)) then  ! update sim non-match vars
          nNoMatchSim = nNoMatchSim + 1
          simNoMatch(nNoMatchSim) = simpeak(np2)
          obsforsim(nNoMatchSim) = obs(simpeakday(np2))
          simNoMatchDay(nNoMatchSim) = simpeakday(np2)
        end if
      end do

      vpbias = calcvpbias(qobs,qsim,npeaksmax,matchday,npmatch,ndays)
      
***************** write peaks file
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      write(cyear1,'(i4)') year1
      write(cyear2,'(i4)') year2
      fnam = outdir//'river/peaks/'//rscen(:lenrscen)//'/'//
     .         rseg(:lenrseg)//'_'//cyear1//'_'//cyear2//'.peaks'
      open (pltfil,file = fnam, status = 'unknown',iostat = err)
      if (err.ne.0) goto 992

****************** PLOT EDF ********************************************
      call plotpeaks(obspeak,npobs,simpeak,npsim,npeaksmax,pltfil)

      write(pltfil,*,err=951) 'Searched for ',npeaks,' peaks, found ',
     .                npobs,' observed and ',npsim,' simulated'
      write(pltfil,*,err=951) ' of which ',npmatch,
     .                        ' occurred on the same day'
      write(pltfil,*,err=951)' ',np1match,' more occurred within 1 day'
      write(pltfil,*,err=951)' ',np2match,' more occurred within 2 days'
      write(pltfil,*,err=951) 

      if (npmatch.gt.2) then
        obar = mean(obsmatch,npmatch,npeaksmax,err)
        if (err.ne.0) continue
        sbar = mean(simmatch,npmatch,npeaksmax,err)
        if (err.ne.0) continue
        ovar = variance(obsmatch,npmatch,npeaksmax,err)
        if (err.ne.0) continue
        evar = errorvar(obsmatch,simmatch,npmatch,npeaksmax,err)
        if (err.ne.0) continue
        r = correl(obsmatch,simmatch,npmatch,npeaksmax,err)
        if (err.ne.0) continue
        if (ovar.le.0.0001) then
          nse = -9.0
        else
          nse = (ovar-evar)/ovar
        end if
        if (obar.le.0.0001) then
          pbias = -9.0
        else
          pbias = (sbar-obar)/obar
        end if
        write(pltfil,*,err=951) ' For MATCHING days: , obs    sim' 
        write(pltfil,*,err=951) ' means obs, mean sim,',obar,',',sbar
        write(pltfil,*,err=951) ' correl,  efficiency,',r,',',nse
        write(pltfil,*,err=951) 'peak bias     ,',pbias
        write(pltfil,*,err=951) 'vol peak bias ,',vpbias
      else
        write(pltfil,*,err=951) 'peak bias     ,',-9.0  ! write these stats 
        write(pltfil,*,err=951) 'vol peak bias ,',-9.0  ! so sumstats will work
      end if
      if (npmatch.gt.0) then
        write(pltfil,*,err=951) ' Day , obs    sim' 
        do np = 1,npmatch
          nd = matchday(np)
          write(pltfil,*,err=951) obyear(nd),obmonth(nd),obday(nd),
     .                    obsmatch(np),simmatch(np)
        end do
      end if
        
      write(pltfil,*,err=951) 
      write(pltfil,*,err=951) 

      if (np1match.gt.2) then
        obar = mean(obs1match,np1match,npeaksmax,err)
        if (err.ne.0) continue
        sbar = mean(sim1match,np1match,npeaksmax,err)
        if (err.ne.0) continue
        ovar = variance(obs1match,np1match,npeaksmax,err)
        if (err.ne.0) continue
        evar = errorvar(obs1match,sim1match,np1match,npeaksmax,err)
        if (err.ne.0) continue
        r = correl(obs1match,sim1match,np1match,npeaksmax,err)
        if (err.ne.0) continue
        if (ovar.le.0.0001) then
          nse = -9
        else
          nse = (ovar-evar)/ovar
        end if
        write(pltfil,*,err=951) 'For peaks within 1 day: , obs    sim'
        write(pltfil,*,err=951) 'means obs, mean sim,    ',obar,',',sbar
        write(pltfil,*,err=951) ' correl,  efficiency,     ',r,',',nse
      end if
      if (np1match.gt.0) then
        write(pltfil,*,err=951) ' Day , obs ,   sim, simulated offset' 
        do np = 1,np1match
          nd = match1day(np,1)
          write(pltfil,*,err=951) obyear(nd),obmonth(nd),obday(nd),
     .                    obs1match(np),sim1match(np),
     .                    match1day(np,2)-match1day(np,1)
        end do
      end if
        
      write(pltfil,*,err=951) 
      write(pltfil,*,err=951) 

      if (np2match.gt.2) then
        obar = mean(obs2match,np2match,npeaksmax,err)
        if (err.ne.0) continue
        sbar = mean(sim2match,np2match,npeaksmax,err)
        if (err.ne.0) continue
        ovar = variance(obs2match,np2match,npeaksmax,err)
        if (err.ne.0) continue
        evar = errorvar(obs2match,sim2match,np2match,npeaksmax,err)
        if (err.ne.0) continue
        r = correl(obs2match,sim2match,np2match,npeaksmax,err)
        if (err.ne.0) then
          if (err.eq.2) then
            print*,'warning zero variance'
          else
            continue
          end if
        end if
        if (ovar.le.0.0001) then
          nse = -9
        else
          nse = (ovar-evar)/ovar
        end if
        write(pltfil,*,err=951)' For peaks within 2 days: , obs    sim' 
        write(pltfil,*,err=951)' means obs, mean sim,    ',obar,',',sbar
        write(pltfil,*,err=951) ' correl,  efficiency,     ',r,',',nse
      end if
      if (np2match.gt.0) then
        write(pltfil,*,err=951) ' Day , obs ,   sim, simulated offset' 
        do np = 1,np2match
          nd = match2day(np,1)
          write(pltfil,*,err=951) obyear(nd),obmonth(nd),obday(nd),
     .                    obs2match(np),sim2match(np),
     .                    match2day(np,2)-match2day(np,1)
        end do
      end if
        
      write(pltfil,*,err=951) 
      write(pltfil,*,err=951) 

      write(pltfil,*,err=951) ' For the ',nNoMatchObs,
     .                ' observed peaks which ',
     .                '  did not match simulated:'
      write(pltfil,*,err=951) 

      if (nNoMatchObs.gt.2) then
        obar = mean(obsNoMatch,nNoMatchObs,npeaksmax,err)
        if (err.ne.0) continue
        sbar = mean(simforobs,nNoMatchObs,npeaksmax,err)
        if (err.ne.0) continue
        ovar = variance(obsNoMatch,nNoMatchObs,npeaksmax,err)
        if (err.ne.0) continue
        evar = errorvar(obsNoMatch,simforobs,nNoMatchObs,npeaksmax,err)
        if (err.ne.0) continue
        r = correl(obsNoMatch,simforobs,nNoMatchObs,npeaksmax,err)
        if (err.ne.0) continue
        if (ovar.le.0.0001) then
          nse = -9
        else
          nse = (ovar-evar)/ovar
        end if
        write(pltfil,*,err=951) ' For Non-MATCHING days: , obs    sim' 
        write(pltfil,*,err=951) ' means obs, mean sim,',obar,',',sbar
        write(pltfil,*,err=951) ' correl,  efficiency,',r,',',nse
      end if
      if (nNoMatchObs.gt.0) then
        write(pltfil,*,err=951) ' Day , obs    sim' 
        do np = 1,nNoMatchObs
          nd = obsNoMatchDay(np)
          write(pltfil,*,err=951) obyear(nd),obmonth(nd),obday(nd),
     .                    obsNoMatch(np),simforobs(np)
        end do
      end if
        
      write(pltfil,*,err=951) 
      write(pltfil,*,err=951) 
        
      write(pltfil,*,err=951) ' For the ',nNoMatchObs,
     .                ' simulated peaks which ',
     .                '  did not match observed:'
      write(pltfil,*,err=951) 

      if (nNoMatchSim.gt.2) then
        sbar = mean(simNoMatch,nNoMatchSim,npeaksmax,err)
        if (err.ne.0) continue
        obar = mean(obsforsim,nNoMatchSim,npeaksmax,err)
        if (err.ne.0) continue
        ovar = variance(obsforsim,nNoMatchSim,npeaksmax,err)
        if (err.ne.0) continue
        evar = errorvar(simNoMatch,obsforsim,nNoMatchSim,npeaksmax,err)
        if (err.ne.0) continue
        r = correl(simNoMatch,obsforsim,nNoMatchSim,npeaksmax,err)
        if (err.ne.0) continue
        if (ovar.le.0.0001) then
          nse = -9
        else
          nse = (ovar-evar)/ovar
        end if
        write(pltfil,*,err=951) ' For Non-MATCHING days: , obs    sim' 
        write(pltfil,*,err=951) ' means obs, mean sim,',obar,',',sbar
        write(pltfil,*,err=951) ' correl,  efficiency,',r,',',nse
      end if
      if (nNoMatchObs.gt.0) then
        write(pltfil,*,err=951) ' Day , obs    sim' 
        do np = 1,nNoMatchSim
          nd = simNoMatchDay(np) 
          write(pltfil,*,err=951) obyear(nd),obmonth(nd),obday(nd),
     .                    obsforsim(np),simNoMatch(np)
        end do
      end if
        
      write(pltfil,*,err=951) 
      write(pltfil,*,err=951) 


      close (pltfil)

      return

************************ error reporting
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   write(report(1),*)'only ',npeaksmax-1,' peaks can be investigated'
      write(report(2),*) npeaks,' were requested in run_postproc.com'
      report(3) = 'change run_postproc.com or npeaksmax in code'
      go to 999

992   report(1) = ' problem with opening file'
      report(2) = fnam
      report(3) = '  Error = '
      write(report(3)(11:13),'(i3)') err
      go to 999


997   report(1) = 'problem with finding stats in PEAKS program'
      report(2) = '  Divide by zero'
      report(3) = '  for segment '//rseg
      go to 999
 
999   call stopreport(report)

      end

************************************************************************
**  returns the peak values and the days on which they occur          **
**    sorted highest to lowest                                        **
************************************************************************
      subroutine getpeaks(values,ndays,ndaymax,npeaks,npeaksmax,
     O                    peak,peakday,nps)
      implicit none
      integer ndays,ndaymax,npeaks,npeaksmax,nps
      real values(ndaymax),peak(npeaksmax),temp  ! data
      integer peakday(npeaksmax),itemp    ! days of peaks
      integer np,nd

      do np = 1,npeaksmax  ! initialize
        peak(np) = 0.0
        peakday(np) = 0
      end do

      nps = 0 
      do nd = 2,ndays-1                  ! get observed peaks
        if (values(nd).gt.values(nd-1)) then
          if (values(nd).gt.values(nd+1)) then   ! found a peak
            nps = nps + 1
            peak(npeaks+1) = values(nd)   ! bottom of the list
            peakday(npeaks+1) = nd
            do np = npeaks,1,-1           !  bubble it up
              if (peak(np).lt.peak(np+1)) then   ! switch
                temp = peak(np)
                peak(np) = peak(np+1)
                peak(np+1) = temp
                itemp = peakday(np)
                peakday(np) = peakday(np+1)
                peakday(np+1) = itemp
              end if
            end do
          end if
        end if 
      end do

      if (nps.gt.npeaks) nps = npeaks
               
      end 


************************************************************************
**  plots empirical density function in ASCII **************************
******** THIS SUBROUTINE NEEDS TESTING AND DEBUGGING *******************
********  EDFs ARE EQUAL, BUT NOT NECESSARILY CORRECT ******************
************************************************************************
      subroutine plotpeaks(obspeak,npobs,simpeak,npsim,npeaksmax,ifl)
      implicit none
      integer npobs,npsim,npeaksmax
      real obspeak(npeaksmax),simpeak(npeaksmax)
      integer nrows,ncols
      parameter (nrows = 21,ncols=70)

      character*100 line

      real hi,lo,step

      integer yobs(nrows),ysim(nrows) 
      real xcoord(ncols)

      integer obsspaces,simspaces

      integer i,j

      integer ifl  ! file number
     
********** find y indices
      do i = 1,nrows
        yobs(i)=int(real(npobs)-(real(i)*real(npobs)/real(nrows))+1.5)
        ysim(i)=int(real(npsim)-(real(i)*real(npsim)/real(nrows))+1.5)
      end do

********* find x coordinates
      hi = max(obspeak(1),simpeak(1))
      lo = min(obspeak(npobs),simpeak(npsim))
      step = (hi-lo)/real(ncols-1)
      xcoord(1) = lo
      do i = 2,ncols
        xcoord(i) = xcoord(i-1) + step
      end do

********* PLOT LINEAR
      line = ' LINEAR PLOT '
      call ryt(line,ifl)
      line = ' observed -      simulated |'
      call ryt(line,ifl)
      do i = 1,nrows
        line = ' '
        write(line,'(i3)') 100 - (i-1)*100/(nrows-1)
        do j = 1,ncols
          if (obspeak(yobs(nrows-i+1)).ge.xcoord(j)) obsspaces = j
          if (simpeak(ysim(nrows-i+1)).ge.xcoord(j)) simspaces = j
        end do
        line(obsspaces+4:obsspaces+4) = '-'
        line(simspaces+4:simspaces+4) = '|'
        if (obsspaces.eq.simspaces) line(obsspaces+4:obsspaces+4) = '+'
        call ryt(line,ifl)
      end do
      write(line,'(f10.0)')lo
      write(line(ncols:ncols+10),'(f10.0)')hi
      call ryt(line,ifl)
      line = ' '
      call ryt(line,ifl)
      call ryt(line,ifl)

********* PLOT LOG
********* find x coordinates
      hi = log(hi)
      lo = log(lo)
      step = (hi-lo)/real(ncols-1)
      xcoord(1) = lo
      do i = 2,ncols
        xcoord(i) = xcoord(i-1) + step
      end do

      line = ' LOG PLOT'
      call ryt(line,ifl)
      line = ' observed -      simulated |'
      call ryt(line,ifl)
      do i = 1,nrows
        line = ' '
        write(line,'(i3)') 100 - (i-1)*100/(nrows-1)
        do j = 1,ncols
          if (log(obspeak(yobs(nrows-i+1))).ge.xcoord(j)) obsspaces = j
          if (log(simpeak(ysim(nrows-i+1))).ge.xcoord(j)) simspaces = j
        end do
        line(obsspaces+4:obsspaces+4) = '-'
        line(simspaces+4:simspaces+4) = '|'
        if (obsspaces.eq.simspaces) line(obsspaces+4:obsspaces+4) = '+'
        call ryt(line,ifl)
      end do
      write(line,'(f10.0)') lo
      write(line(ncols:ncols+10),'(f10.0)') hi
      call ryt(line,ifl)
      line = ' '
      call ryt(line,ifl)
      call ryt(line,ifl)

      end

************************************************************************
** function to calculate the quick flow area of the peaks that occur  **
**  on the same day.                                                  **
************************************************************************
      function calcvpbias(qobs,qsim,npeaksmax,matchday,npmatch,ndays)
      implicit none
      include 'Rstats.inc'

      integer ndays    ! number of days

      integer nd,np ! index

      integer year1  ! start year

****** variables for investigation of peaks
      integer npeaksmax

      integer npmatch  ! number of days that match
      integer matchday(npeaksmax)                   ! day of match

      real sumo(npeaksmax),sums(npeaksmax)
      real obsave,simave

      logical descend

      real calcvpbias

******************* END DECLARATIONS ***********************************

      do np = 1,npmatch
        nd = matchday(np)
        sumo(np) = qobs(nd)
        descend = .true.
        do while (descend)
          nd = nd + 1
          sumo(np) = sumo(np) + qobs(nd)
          if (qobs(nd+1).gt.qobs(nd)) descend = .false.
          if (qobs(nd+1).lt.0.01) descend = .false.
          if (nd.ge.ndays) descend = .false.
        end do
        descend = .true.
        nd = matchday(np)
        do while (descend)
          nd = nd - 1
          if (nd.lt.2) exit
          sumo(np) = sumo(np) + qobs(nd)
          if (qobs(nd-1).gt.qobs(nd)) descend = .false.
          if (qobs(nd-1).lt.0.01) descend = .false.
          if (nd.le.2) descend = .false.
        end do
      end do

      do np = 1,npmatch
        nd = matchday(np)
        sums(np) = qsim(nd)
        descend = .true.
        do while (descend)
          nd = nd + 1
          sums(np) = sums(np) + qsim(nd)
          if (qsim(nd+1).gt.qsim(nd)) descend = .false.
          if (qsim(nd+1).lt.0.01) descend = .false.
          if (nd.ge.ndays) descend = .false.
        end do
        descend = .true.
        nd = matchday(np)
        do while (descend)
          nd = nd - 1
          if (nd.lt.2) exit
          sums(np) = sums(np) + qsim(nd)
          if (qsim(nd-1).gt.qsim(nd)) descend = .false.
          if (qsim(nd-1).lt.0.01) descend = .false.
          if (nd.le.2) descend = .false.
        end do
      end do

      obsave = 0.0
      simave = 0.0
      do np = 1,npmatch  ! find average, kept them separate for future
        obsave = obsave + sumo(np)
        simave = simave + sums(np)
      end do

      obsave = obsave / real(npmatch)
      simave = simave / real(npmatch)

      calcvpbias = (simave-obsave)/obsave

      return

      end


