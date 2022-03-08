************************************************************************
** subroutine to produce averages for sections of a cumulative        **
***  distribution for two vectors and the maximum difference between  **
**   them at for each secion of the cfd.  The max difference can be   **
**   used in the Kolmogorov-Smirnov test                              **
**                                                                    **
** Always performed on the log of concentrations                      **
**                                                                    **
** verified in c:/aafiles/wsm/p5/calib/river_iterative_calibration/   **
**     why_use_hazen_for_quintile_plots.xls                           **
**                                                                    **
** related subroutine noObVodka is just the simulated values when no  **
**  observations exist for this river or time period                  **
************************************************************************
      subroutine vodkatest(
     I                     sim,obs,obLOD,
     I                     nobsmax,nobsin,nbinsmax,nbins,
     O                     simBinAve,obsBinAve,ksKstat,nse,nobsout)
      implicit none
      integer nobsmax,nobs,nbinsmax,nbins  ! observations and bins
      real sim(nobsmax),obs(nobsmax)  ! input variables, do not sort
      real si2(nobsmax),ob2(nobsmax)  ! missing values removed
      real si(nobsmax),ob(nobsmax)  ! sorted variables

      logical obLOD(nobsmax)  ! true if observations is Limit Of Detect

      real simBinAve(nbinsmax),obsBinAve(nbinsmax) ! ave for bin
      real ksKstat  ! KS statistic
      real ovar,evar,nse ! efficiency
 
      integer no,nb,no1,no2,nobsin,nobsout
      integer nobsInBin(nbinsmax)
      integer ord(nobsmax)

      real hazenrank(nobsmax),hazen

      real upperbound,lowerbound

      logical found

      real variance,errorvar,err
      external variance,errorvar

*************** END DECLARATIONS
      nobs = 0
      do no = 1,nobsin  ! take log of sim and observed
        if (obs(no).gt.-8.) then   ! if not missing data

          ob2(no) = max(obs(no),1.0e-4)  ! simulated sometimes is zero
          si2(no) = max(sim(no),1.0e-4)  ! need to count these as well

                ! if limit of detect, count anything below as exact 
          if (obLOD(no)) then  ! match (to the accuracy of the data)
            if (si2(no).lt.ob2(no)) si2(no) = ob2(no)
          end if

          nobs = nobs + 1            ! data OK, take logs
          ob2(nobs) = log10(ob2(no))
          si2(nobs) = log10(si2(no))

        end if
      end do

C      if (nobs.lt.10) return
      nobsout = nobs

      do no = 1,nobs  ! find ranks
        hazenrank(no) = (real(no)-0.5)/real(nobs)
      end do

      call qsortr(ord,nobs,si2)  ! sort both data sets
      do no = 1,nobs
        si(no) = si2(ord(no))
      end do

      call qsortr(ord,nobs,ob2)
      do no = 1,nobs
        ob(no) = ob2(ord(no))
      end do

********** find bin averages
      do nb = 1,nbins
        upperbound = real(nb)/real(nbins)      
        lowerbound = real(nb-1)/real(nbins)      
        simBinAve(nb) = 0.0
        obsBinAve(nb) = 0.0
        nobsInBin(nb) = 0
        do no = 1,nobs
          if (hazenrank(no).lt.upperbound.and.
     .        hazenrank(no).ge.lowerbound) then
            simBinAve(nb) = simBinAve(nb) + si(no)
            obsBinAve(nb) = obsBinAve(nb) + ob(no)
            nobsInBin(nb) = nobsInBin(nb) + 1
          end if
        end do 
        if (nobsInBin(nb) .gt. 0) then
          simBinAve(nb) = simBinAve(nb) / real(nobsInBin(nb))
          obsBinAve(nb) = obsBinAve(nb) / real(nobsInBin(nb))
        end if   ! contain zeros if no obs in Bin
      end do

********** find KS statistic
      ksKstat = -1.0

      do no1 = 1,nobs  ! loop over simulated first
        found = .false.
        do no2 = 2,nobs   ! loop over observed and find first over
          if (ob(no2).gt.si(no1).and.ob(no2-1).lt.si(no1)) then
            found = .true.
            exit
          end if
        end do
        if (found) then
          hazen = hazenrank(no2-1) + 
     .            (hazenrank(no2)-hazenrank(no2-1)) *
     .              (si(no1)-ob(no2-1))/(ob(no2)-ob(no2-1))
          ksKstat = max(ksKstat,abs(hazen-hazenrank(no1)))
        end if
      end do
      do no1 = 1,nobs  ! then over observed
        found = .false.
        do no2 = 2,nobs   ! loop over simulated and find first over
          if (si(no2).gt.ob(no1).and.si(no2-1).lt.ob(no1)) then
            found = .true.
            exit
          end if
        end do
        if (found) then
          hazen = hazenrank(no2-1) + 
     .            (hazenrank(no2)-hazenrank(no2-1)) *
     .              (ob(no1)-si(no2-1))/(si(no2)-si(no2-1))
          ksKstat = max(ksKstat,abs(hazen-hazenrank(no1)))
        end if
      end do

      if (ksKstat.lt.-.9) ksKstat = 1.0  ! no overlap at all

*************** find efficiency
      ovar = variance(obsBinAve,nbins,nbinsmax,err)
      if (err.ne.0) ovar = -9

      evar = errorvar(obsBinAve,simBinAve,nbins,nbinsmax,err)
      if (err.ne.0) evar = -9

      if (ovar.le.0.0001) then
        nse = -9
      else
        nse = (ovar-evar)/ovar
      end if


      end

      subroutine noObVodka(
     I                     sim,obs,nobsmax,nobsin,nbinsmax,nbins,
     O                     simBinAve,obsBinAve,ksKstat,nse)
      implicit none
      integer nobsmax,nobs,nbinsmax,nbins  ! observations and bins
      real sim(nobsmax),obs(nobsmax)  ! input variables, do not sort
      real si2(nobsmax),ob2(nobsmax)  ! missing values removed
      real si(nobsmax),ob(nobsmax)  ! sorted variables
      real simBinAve(nbinsmax),obsBinAve(nbinsmax) ! ave for bin
      real ksKstat  ! KS statistic
      real nse       ! efficiency
 
      integer no,nb,no1,no2,nobsin
      integer nobsInBin(nbinsmax)
      integer ord(nobsmax)

      real hazenrank(nobsmax),hazen

      real upperbound,lowerbound

      logical found

*************** END DECLARATIONS
      do nb = 1,nbins
        obsBinAve(nb) = -999.0
      end do
      ksKstat = -999.0
      nse = -999.0
     
      nobs = nobsin
      do no = 1,nobsin  ! make sure sim is legit
        if (sim(no).gt.1.0e-8) then ! not low or missing
          si2(no) = log10(sim(no))
        else
          si2(no) = -9.0
        end if
      end do

      do no = 1,nobs  ! find ranks
        hazenrank(no) = (real(no)-0.5)/real(nobs)
      end do

      call qsortr(ord,nobs,si2)  ! sort both data sets
      do no = 1,nobs
        si(no) = si2(ord(no))
      end do

********** find bin averages
      do nb = 1,nbins
        upperbound = real(nb)/real(nbins)      
        lowerbound = real(nb-1)/real(nbins)      
        simBinAve(nb) = 0.0
        nobsInBin(nb) = 0
        do no = 1,nobs
          if (hazenrank(no).lt.upperbound.and.
     .        hazenrank(no).ge.lowerbound) then
            simBinAve(nb) = simBinAve(nb) + si(no)
            nobsInBin(nb) = nobsInBin(nb) + 1
          end if
        end do 
        simBinAve(nb) = simBinAve(nb) / real(nobsInBin(nb))
      end do

      end


      subroutine vodkatest_bins(
     I                     sim,obs,obLOD,
     I                     nobsmax,nobsin,nbinsmax,nbins,
     I                     lowerbound,upperbound,
     O                     simBinAve,obsBinAve,ksKstat,nse,nobsout)
      implicit none
      integer nobsmax,nobs,nbinsmax,nbins  ! observations and bins
      real sim(nobsmax),obs(nobsmax)  ! input variables, do not sort
      real si2(nobsmax),ob2(nobsmax)  ! missing values removed
      real si(nobsmax),ob(nobsmax)  ! sorted variables

      logical obLOD(nobsmax)  ! true if observations is Limit Of Detect

      real simBinAve(nbinsmax),obsBinAve(nbinsmax) ! ave for bin
      real ksKstat  ! KS statistic
      real ovar,evar,nse ! efficiency

      integer no,nb,no1,no2,nobsin,nobsout
      integer nobsInBin(nbinsmax)
      integer ord(nobsmax)

      real hazenrank(nobsmax),hazen

      real upperbound(nbinsmax),lowerbound(nbinsmax)

      logical found

      real variance,errorvar,err
      external variance,errorvar

*************** END DECLARATIONS
      nobs = 0
      do no = 1,nobsin  ! take log of sim and observed
        if (obs(no).gt.-8.) then   ! if not missing data

          ob2(no) = max(obs(no),1.0e-4)  ! simulated sometimes is zero
          si2(no) = max(sim(no),1.0e-4)  ! need to count these as well

                ! if limit of detect, count anything below as exact 
          if (obLOD(no)) then  ! match (to the accuracy of the data)
            if (si2(no).lt.ob2(no)) si2(no) = ob2(no)
          end if

          nobs = nobs + 1            ! data OK, take logs
          ob2(nobs) = log10(ob2(no))
          si2(nobs) = log10(si2(no))

        end if
      end do

C      if (nobs.lt.10) return
      nobsout = nobs

      do no = 1,nobs  ! find ranks
        hazenrank(no) = (real(no)-0.5)/real(nobs)
      end do

      call qsortr(ord,nobs,si2)  ! sort both data sets
      do no = 1,nobs
        si(no) = si2(ord(no))
      end do

      call qsortr(ord,nobs,ob2)
      do no = 1,nobs
        ob(no) = ob2(ord(no))
      end do

********** find bin averages
      do nb = 1,nbins
c        upperbound = real(nb)/real(nbins)
c        lowerbound = real(nb-1)/real(nbins)
        simBinAve(nb) = 0.0
        obsBinAve(nb) = 0.0
        nobsInBin(nb) = 0
        do no = 1,nobs
          if (hazenrank(no).lt.upperbound(nb).and.
     .        hazenrank(no).ge.lowerbound(nb)) then
            simBinAve(nb) = simBinAve(nb) + si(no)
            obsBinAve(nb) = obsBinAve(nb) + ob(no)
            nobsInBin(nb) = nobsInBin(nb) + 1
          end if
        end do
        if (nobsInBin(nb) .gt. 0) then
          simBinAve(nb) = simBinAve(nb) / real(nobsInBin(nb))
          obsBinAve(nb) = obsBinAve(nb) / real(nobsInBin(nb))
        end if   ! contain zeros if no obs in Bin
      end do

********** find KS statistic
      ksKstat = -1.0

      do no1 = 1,nobs  ! loop over simulated first
        found = .false.
        do no2 = 2,nobs   ! loop over observed and find first over
          if (ob(no2).gt.si(no1).and.ob(no2-1).lt.si(no1)) then
            found = .true.
            exit
          end if
        end do
        if (found) then
          hazen = hazenrank(no2-1) +
     .            (hazenrank(no2)-hazenrank(no2-1)) *
     .              (si(no1)-ob(no2-1))/(ob(no2)-ob(no2-1))
          ksKstat = max(ksKstat,abs(hazen-hazenrank(no1)))
        end if
      end do
      do no1 = 1,nobs  ! then over observed
        found = .false.
        do no2 = 2,nobs   ! loop over simulated and find first over
          if (si(no2).gt.ob(no1).and.si(no2-1).lt.ob(no1)) then
            found = .true.
            exit
          end if
        end do
        if (found) then
          hazen = hazenrank(no2-1) +
     .            (hazenrank(no2)-hazenrank(no2-1)) *
     .              (ob(no1)-si(no2-1))/(si(no2)-si(no2-1))
          ksKstat = max(ksKstat,abs(hazen-hazenrank(no1)))
        end if
      end do

      if (ksKstat.lt.-.9) ksKstat = 1.0  ! no overlap at all

*************** find efficiency
      ovar = variance(obsBinAve,nbins,nbinsmax,err)
      if (err.ne.0) ovar = -9

      evar = errorvar(obsBinAve,simBinAve,nbins,nbinsmax,err)
      if (err.ne.0) evar = -9

      if (ovar.le.0.0001) then
        nse = -9
      else
        nse = (ovar-evar)/ovar
      end if


      end
