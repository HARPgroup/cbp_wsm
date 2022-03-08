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
      subroutine binaves(
     I                  obs,nobs,
     I                  nobsmax,nbinsmax,nbins,
     O                  binave)
      implicit none
      integer nobsmax,nobs,nbinsmax,nbins  ! observations and bins
      real obs(nobsmax)  ! input variables, do not sort
      real si2(nobsmax),ob2(nobsmax)  ! missing values removed
      real si(nobsmax),ob(nobsmax)  ! sorted variables

      real binave(nbinsmax)
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
      do no = 1,nobs  ! take log 
        ob2(no) = max(obs(no),1.0e-4) 
        ob2(no) = log10(ob2(no))
      end do

      do no = 1,nobs  ! find ranks
        hazenrank(no) = (real(no)-0.5)/real(nobs)
      end do

      call qsortr(ord,nobs,ob2)
      do no = 1,nobs
        ob(no) = ob2(ord(no))
      end do

********** find bin averages
      do nb = 1,nbins
        upperbound = real(nb)/real(nbins)      
        lowerbound = real(nb-1)/real(nbins)      
        binave(nb) = 0.0
        nobsInBin(nb) = 0
        do no = 1,nobs
          if (hazenrank(no).lt.upperbound.and.
     .        hazenrank(no).ge.lowerbound) then
            binave(nb) = binave(nb) + ob(no)
            nobsInBin(nb) = nobsInBin(nb) + 1
          end if
        end do 
        if (nobsInBin(nb) .gt. 0) then
          binave(nb) = binave(nb) / real(nobsInBin(nb))
        end if   ! contain zeros if no obs in Bin
      end do

      end
