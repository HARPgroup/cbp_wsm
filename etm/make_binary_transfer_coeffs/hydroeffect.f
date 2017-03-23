************************************************************************
** subroutine to apply the hydrologic effects on BMPs                 **
**  the include file documents the meaning of the parameters          **
**  controlling the hydrologic effect                                 **
************************************************************************
      subroutine hydroeffect(
     I                       nBmpTypes,NoBmpAcres,BmpName,
     I                       indHG,l,ndays,
     I                       BmpHydEffType,BmpHydEffParm,retfreq,
     I                       BMPconname,nbcon,
     M                       DailyIndEff)
      implicit none

      include 'mbtc.f'

      integer l,j,nb  ! land use and day indices
      integer ndays

      real retfreq(ndaymax) ! return frequency of the storm flow
                            ! for days between start and end date
      integer julian
      external julian

************** index to the HGMR of the lrsegs
      integer indHG

******** temporary storage variables to calcuate daily bmps efficiencies
      real DailyIndEff(ndaymax,maxBMPcon,MaxBmpTypes) ! individual bmp 

      logical NoBmpAcres(MaxBmpTypes) ! does this BMP exist in lu & seg

********* temporary variables to calculate the decrement
      real minfreq, halfsat, asymptot

************* END DECLARATIONS *****************************************

      do nBmp = 1,nBmpTypes

        if (NoBmpAcres(nBmp)) cycle

        do nb = 1,nbcon

          if (BmpHydEffType(nBmp,nb,indHG,l).eq.-999) then
            cycle         ! no effect
          else if (BmpHydEffType(nBmp,nb,indHG,l).eq.0) then 
            cycle         ! no effect
          else if (BmpHydEffType(nBmp,nb,indHG,l).eq.1 ) then  ! MM
            minfreq = BmpHydEffParm(nBmp,nb,indHG,l,1)
            halfsat = BmpHydEffParm(nBmp,nb,indHG,l,2)
            asymptot = BmpHydEffParm(nBmp,nb,indHG,l,3)
            do j = 1,ndays
c              print *,'DailyIndEff original= ',DailyIndEff(j,nb,nBmp)  !GY
              if (retfreq(j).gt.minfreq) then
c              print *,'DailyIndEff original= ',DailyIndEff(j,nb,nBmp),
c     .              minfreq,halfsat,asymptot  !GY
                DailyIndEff(j,nb,nBmp) = DailyIndEff(j,nb,nBmp) * 
     .                 (1.0 -  (1.0-asymptot) * (retfreq(j)-minfreq)
     .                      /  (retfreq(j)-minfreq + halfsat-minfreq))
c                print *,'DailyIndEff = ',DailyIndEff(j,nb,nBmp)  !GY
              end if
            end do
          else
            go to 992   ! error
          end if
 
        end do
      end do

      return

************** ERROR SPACE *********************************************
992   report(1) = 'Bmp Hydrologic effect type unknown'
      write(report(2),*)'BMP: ',BmpName(nBmp),' effect type: ',
     .                  BmpHydEffType(nBmp,nb,indHG,l),' land use: ',
     .                  luname(l),' constituent: ',BMPconname(nb)
      report(3) = ' check hydrologic_parameters.csv file'
      go to 999

999   call stopreport(report)

      end
