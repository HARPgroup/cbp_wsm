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
      subroutine KStest(
     I                     sim,obs,maxvals,nsim,nobs,
     O                     ksKstat)
      implicit none
      integer maxvals,nsim,nobs
      real sim(maxvals),obs(maxvals)  ! input variables, do not sort

      real si(maxvals),ob(maxvals)  ! sorted logs of input variables
      integer ord(maxvals)      ! variable for sorting

      real ksKstat  ! KS statistic
 
      integer no,ns,nb,no1,no2,nobsin,nobsout

      real simrank(maxvals),obsrank(maxvals),hazen

      logical found

*************** END DECLARATIONS

*********** sort, take logs, and transfer to temp variables
      call qsortr(ord,nsim,sim)  ! sort both data sets
      do ns = 1,nsim
        si(ns) = log10(sim(ord(ns)))
      end do

      call qsortr(ord,nobs,obs)  ! sort both data sets
      do no = 1,nobs
        ob(no) = log10(obs(ord(no)))
      end do

********** find ranks
      do no = 1,nobs  ! find ranks
        obsrank(no) = (real(no)-0.5)/real(nobs)
      end do

      do ns = 1,nsim  ! find ranks
        simrank(ns) = (real(ns)-0.5)/real(nsim)
      end do

********** find KS statistic
      ksKstat = -1.0

      do ns = 1,nsim  ! loop over simulated first
        found = .false.
        do no = 2,nobs   ! loop over observed and find first over
          if (ob(no).ge.si(ns).and.ob(no-1).lt.si(ns)) then
            found = .true.
            exit
          end if
        end do
        if (found) then
          hazen = obsrank(no-1) + 
     .            (obsrank(no)-obsrank(no-1)) *
     .              (si(ns)-ob(no-1))/(ob(no)-ob(no-1))
          ksKstat = max(ksKstat,abs(hazen-simrank(ns)))
        end if
      end do

      if (ksKstat.lt.-.9) ksKstat = 1.0  ! no overlap at all

      end

