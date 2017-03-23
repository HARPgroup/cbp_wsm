************************************************************************
**  The purpose of this program is to calculate statitstics for       **
**  hyfrological calibration                                          **
**                                                                    **
**  The ststistics are calculated for both base variables and         **
**     log transformed variables                                      **
************************************************************************

      subroutine flowstats(ob,si,nday2,rscen,rseg,year1,year2,suffix,
     .                     obyear,obmonth,obday)
      implicit none
      include 'Rstats.inc'

      integer ndays, nday2    ! number of days

      character*4 suffix

      character*100 pfname

      character*4 cy1,cy2

      integer year1,year2             ! first and last year to average

      integer nd ! index

*******  regression variables
      real obs(ndaymax),sim(ndaymax)   ! all flow
      real ob(ndaymax),si(ndaymax)   ! all flow
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

******** variables for Dec-Feb Bias
      real winterobs,wintersim,winterbias
      real summerobs,summersim,summerbias

******** variables for lowest 10% bias
      real low10obs,low10sim,low10bias,low05obs,low05sim,low05bias   !ICPRB: added low05's

      integer ord(ndaymax)

******************* END DECLARATIONS ***********************************

      ndays = nday2
      do nd = 1,ndays    ! variables will be modified
        obs(nd) = ob(nd)   ! so store them in alternate variables
        sim(nd) = si(nd)
      end do

********* calculate biase for low 10% of observed data
      call qsortr(ord,ndays,obs)  ! sort (does not destroy order of obs)
      low10obs = 0.0
      low10sim = 0.0
      do nd = 1,ndays/10
        low10obs = low10obs + obs(ord(nd))
        low10sim = low10sim + sim(ord(nd))
      end do
      if (low10obs.gt.0) then
        low10bias = (low10sim-low10obs)/low10obs
      else
        low10bias = -9
      end if

********* calculate biase for low 5% of observed data - added by ICPRB
      low05obs = 0.0
      low05sim = 0.0
      do nd = 1,ndays/20
        low05obs = low05obs + obs(ord(nd))
        low05sim = low05sim + sim(ord(nd)) ! paired lowflows - like lo10
      end do
      if (low05obs.gt.0) then
        low05bias = (low05sim-low05obs)/low05obs
      else
        low05bias = -9
      end if

*********** calculate winter bias
      winterobs = 0.0
      wintersim = 0.0
      do nd = 1,ndays
        if (obmonth(nd).eq.12.or.obmonth(nd).le.2) then
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
      summerobs = 0.0
      summersim = 0.0
      do nd = 1,ndays
        if (obmonth(nd).ge.6.and.obmonth(nd).le.8) then
          summerobs = summerobs + obs(nd)
          summersim = summersim + sim(nd)
        end if
      end do
      if (summerobs.le..0001) then
        summerbias = -9.0
      else
        summerbias = (summersim-summerobs)/summerobs
      end if

******** calculate monthly statistics
      do nd = 1,nyearmax*12
        monobs(nd) = 0.0
        monsim(nd) = 0.0
      end do

      nd = 1
      ndy = 1
      nm = 1
      ny = year1
      lastmonth = 0
      month = 0
      do while (nd.le.ndays)
        if (nm.ne.lastmonth) then
          month = month + 1
          lastmonth = nm
        end if
        monobs(month) = monobs(month) + obs(nd)
        monsim(month) = monsim(month) + sim(nd)
        call tomorrow(ny,nm,ndy)
        nd = nd + 1
      end do

      ovarM = variance(monobs,month,nyearmax*12,err)
      if (err.ne.0) continue
      evarM = errorvar(monobs,monsim,month,nyearmax*12,err)
      if (err.ne.0) continue

      if (ovarM.le.0.0001) then
        nseM = -9
      else
        nseM = (ovarM-evarM)/ovarM
      end if


********** calculate basic statistics on untransformed variables
      obar = mean(obs,ndays,ndaymax,err)
      if (err.ne.0) continue
      sbar = mean(sim,ndays,ndaymax,err)
      if (err.ne.0) continue

      if (obar.le.0.0001) then 
        bias = -9
      else
        bias = (sbar-obar)/obar
      end if

      ovar = variance(obs,ndays,ndaymax,err)
      if (err.ne.0) continue
      svar = variance(sim,ndays,ndaymax,err)
      if (err.ne.0) continue
      evar = errorvar(obs,sim,ndays,ndaymax,err)
      if (err.ne.0) continue
      r = correl(obs,sim,ndays,ndaymax,err)
      if (err.ne.0) continue

      if (ovar.le.0.0001) then
        nse = -9
      else
        nse = (ovar-evar)/ovar
      end if

**************  calculate recession indices
      nqtobs = 0      ! observed first
      do nd = 2,ndays-1
        qplus1  = obs(nd+1)
        q       = obs(nd)
        qminus1 = obs(nd-1)
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
        call noIregress(allqt,allqtplus1,nqtobs,ndaymax,
     O                  mobs,robs,err)
        if (err.ne.0) go to 993
        call calcRC(allqt,allqtplus1,nqtobs,ndaymax,
     O              mobs2,mobs3,err)
        if (err.ne.0) go to 993
      else
        mobs = -9.0
        robs = -9.0
        mobs2 = -9.0
        mobs3 = -9.0
      end if

      nqtsim = 0      ! simulated
      do nd = 2,ndays-1
        qplus1  = sim(nd+1)
        q       = sim(nd)
        qminus1 = sim(nd-1)
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
        call noIregress(allqt,allqtplus1,nqtsim,ndaymax,
     O                msim,rsim,err)
        if (err.ne.0) go to 994
        call calcRC(allqt,allqtplus1,nqtsim,ndaymax,
     O              msim2,msim3,err)
        if (err.ne.0) go to 994
      else
        msim  = -9.0
        rsim  = -9.0
        msim2  = -9.0
        msim3  = -9.0
      end if

      if (mobs.le.0.) then
        indexindex = -9.0
      else
        indexindex = msim/mobs
      end if
      if (mobs2.le.0.) then
        indind2 = -9.0
      else
        indind2 = msim2/mobs2
      end if
      if (mobs3.le.0.) then
        indind3 = -9.0
      else
        indind3 = msim3/mobs3
      end if

********* calculate summer and winter recession indices
      nqtsim = 0      ! simulated winter
      do nd = 2,ndays-1
        if (obmonth(nd).eq.12.or.obmonth(nd).le.2) then
          qplus1  = sim(nd+1)
          q       = sim(nd)
          qminus1 = sim(nd-1)
          if (q.lt.qminus1) then  ! not peak and falling
            if (qplus1.lt.q) then  ! still decending
              if (qplus1.gt.0.01*sbar) then  ! not at rock bottom
                nqtsim = nqtsim + 1
                allqt(nqtsim) = q
                allqtplus1(nqtsim) = qplus1
              end if
            end if
          end if
        end if
      end do
      if (nqtsim.eq.0) then
        msim4 = -9.0
        msim5 = -9.0
      else
        call calcRC(allqt,allqtplus1,nqtsim,ndaymax,
     O              msim4,msim5,err)
        if (err.ne.0) go to 994
      end if
      nqtsim = 0      ! simulated summer
      do nd = 2,ndays-1
        if (obmonth(nd).ge.6.and.obmonth(nd).le.8) then
          qplus1  = sim(nd+1)
          q       = sim(nd)
          qminus1 = sim(nd-1)
          if (q.lt.qminus1) then  ! not peak and falling
            if (qplus1.lt.q) then  ! still decending
              if (qplus1.gt.0.01*sbar) then  ! not at rock bottom
                nqtsim = nqtsim + 1
                allqt(nqtsim) = q
                allqtplus1(nqtsim) = qplus1
              end if
            end if
          end if
        end if
      end do
      if (nqtsim.eq.0) then
        msim6 = -9.0
        msim7 = -9.0
      else
        call calcRC(allqt,allqtplus1,nqtsim,ndaymax,
     O              msim6,msim7,err)
        if (err.ne.0) go to 994
      end if
      nqtobs = 0      ! observed winter
      do nd = 2,ndays-1
        if (obmonth(nd).eq.12.or.obmonth(nd).le.2) then
          qplus1  = obs(nd+1)
          q       = obs(nd)
          qminus1 = obs(nd-1)
          if (q.lt.qminus1) then  ! not peak and falling
            if (qplus1.lt.q) then  ! still decending
              if (qplus1.gt.0.01*sbar) then  ! not at rock bottom
                nqtobs = nqtobs + 1
                allqt(nqtobs) = q
                allqtplus1(nqtobs) = qplus1
              end if
            end if
          end if
        end if
      end do
      if (nqtobs.eq.0) then
        mobs4 = -9.0
        mobs5 = -9.0
      else
        call calcRC(allqt,allqtplus1,nqtobs,ndaymax,
     O              mobs4,mobs5,err)
        if (err.ne.0) go to 993
      end if
      nqtobs = 0      ! observed summer
      do nd = 2,ndays-1
        if (obmonth(nd).ge.6.and.obmonth(nd).le.8) then
          qplus1  = obs(nd+1)
          q       = obs(nd)
          qminus1 = obs(nd-1)
          if (q.lt.qminus1) then  ! not peak and falling
            if (qplus1.lt.q) then  ! still decending
              if (qplus1.gt.0.01*sbar) then  ! not at rock bottom
                nqtobs = nqtobs + 1
                allqt(nqtobs) = q
                allqtplus1(nqtobs) = qplus1
              end if
            end if
          end if
        end if
      end do
      if (nqtobs.eq.0) then
        mobs6 = -9.0
        mobs7 = -9.0
      else
        call calcRC(allqt,allqtplus1,nqtobs,ndaymax,
     O              mobs6,mobs7,err)
        if (err.ne.0) go to 993
      end if
      if (msim4.le.0..or.mobs4.le.0.) then
        indind4 = -9.0
      else
        indind4 = msim4/mobs4
      end if
      if (msim4.le.0..or.mobs5.le.0.) then
        indind5 = -9.0
      else
        indind5 = msim5/mobs5
      end if
      if (msim6.le.0..or.mobs6.le.0.) then
        indind6 = -9.0
      else
        indind6 = msim6/mobs6
      end if
      if (msim7.le.0..or.mobs7.le.0.) then
        indind7 = -9.0
      else
        indind7 = msim7/mobs7
      end if

********** log transform data
      do nd = 1,nday2  ! before transform, get rid of non-positives
        if (nd.gt.ndays) exit
        if (obs(nd).lt.0.0001.or.sim(nd).lt.0.0001) then
          do while (obs(ndays).lt.0.0001.or.sim(ndays).lt.0.0001)
            ndays = ndays - 1
          end do
          obs(nd) = obs(ndays)  ! this destroyes the order
          sim(nd) = sim(ndays)  ! but not the pairing
          ndays = ndays - 1
        end if
      end do

      do nd = 1,ndays    ! log transform 
        obs(nd) = log(obs(nd))
        sim(nd) = log(sim(nd))
      end do

********** calculate basic statistics on transformed variables
      olbar = mean(obs,ndays,ndaymax,err)
      if (err.ne.0) continue
      slbar = mean(sim,ndays,ndaymax,err)
      if (err.ne.0) continue

      if (olbar.le.0.0001) then 
        lbias = -9
      else
        lbias = (slbar-olbar)/olbar
      end if

      olvar = variance(obs,ndays,ndaymax,err)
      if (err.ne.0) continue
      slvar = variance(sim,ndays,ndaymax,err)
      if (err.ne.0) continue
      elvar = errorvar(obs,sim,ndays,ndaymax,err)
      if (err.ne.0) continue
      rl = correl(obs,sim,ndays,ndaymax,err)
      if (err.ne.0) continue

      if (olvar.le.0.0001) then
        nse = -9
      else
        nsel = (olvar-elvar)/olvar
      end if

***************** Jeff Load file
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2
      pfname = outdir//'river/stats/'//rscen(:lenrscen)//'/'//
     .         rseg(:lenrseg)//'_'//cy1//'_'//cy2//'.'//suffix
      open (pltfil,file = pfname, status = 'unknown',iostat = err)
      if (err.ne.0) goto 992

      write(pltfil,*,err=951) suffix//',            sim,        obs'
      write(pltfil,*,err=951) 'mean          ,',sbar,',',obar
      write(pltfil,*,err=951) 'bias   n      ,',bias,',',nday2
      write(pltfil,*,err=951) 'correl        ,',r
      write(pltfil,*,err=951) 'efficiency    ,',nse
      write(pltfil,*,err=951) 'variance      ,',svar,',',ovar
      write(pltfil,*,err=951) 'errorvar      ,',evar
      write(pltfil,*,err=951) 'mean       log,',slbar,',',olbar
      write(pltfil,*,err=951) 'bias   n   log,',lbias,',',ndays
      write(pltfil,*,err=951) 'correl     log,',rl
      write(pltfil,*,err=951) 'efficiency log,',nsel
      write(pltfil,*,err=951) 'variance   log,',slvar,',',olvar
      write(pltfil,*,err=951) 'errorvar   log,',elvar
      write(pltfil,*,err=951) 'recession indx,',msim,',',mobs
      write(pltfil,*,err=951) 'ave reces indx,',msim2,',',mobs2
      write(pltfil,*,err=951) 'med reces indx,',msim3,',',mobs3
      write(pltfil,*,err=951) 'r recess  indx,',rsim,',',robs
      write(pltfil,*,err=951) 'n recess  indx,',nqtsim,',',nqtobs
      write(pltfil,*,err=951) 'recession stat,',indexindex
      write(pltfil,*,err=951) 'ave reces stat,',indind2
      write(pltfil,*,err=951) 'med reces stat,',indind3
      write(pltfil,*,err=951) 'Wint ave reces,',indind4
      write(pltfil,*,err=951) 'Wint med reces,',indind5
      write(pltfil,*,err=951) 'Sumr ave reces,',indind6
      write(pltfil,*,err=951) 'Sumr med reces,',indind7
      write(pltfil,*,err=951) 'efficiency mon,',nseM
      write(pltfil,*,err=951) 'winter bias   ,',winterbias
      write(pltfil,*,err=951) 'summer bias   ,',summerbias
      write(pltfil,*,err=951) 'low10 bias    ,',low10bias
      write(pltfil,*,err=951) 'low05 bias    ,',low05bias

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
**  subroutine to get the average and median ratio between today's    **
**    yesterday's flow                                                **
************************************************************************
      subroutine calcRC(allqt,allqtplus1,nqt,ndaymax,
     O                  ave,mdn,err)

      implicit none
      integer nqt,ndaymax
      real allqt(ndaymax),allqtplus1(ndaymax),ratio(ndaymax)
      integer err,n,i

      real ave,mdn

      real mean,median
      external mean,median

      do n = 1,nqt
        ratio(n) = allqtplus1(n)/allqt(n)
      end do

      ave = mean(ratio,nqt,ndaymax,err)
      mdn = median(ratio,nqt,ndaymax,err)

      end

      

      

      

