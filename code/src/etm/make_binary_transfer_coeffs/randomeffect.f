************************************************************************
** subroutine to apply the hydrologic effects on BMPs                 **
**  the include file documents the meaning of the parameters          **
**  controlling the hydrologic effect                                 **
************************************************************************
      subroutine randomeffect(
     I                        nBmpTypes,NoBmpAcres,BmpName,
     I                        indHG,l,ndays,
     I                        BmpEffDistType,BmpEffDistParm,seed,
     I                        BMPconname,nbcon,
     M                        DailyIndEff)
      implicit none

      include 'mbtc.f'

      integer l,j,nb  ! land use and day indices
      integer ndays

      real retfreq(ndaymax) ! return frequency of the storm flow
                            ! for days between start and end date
      integer julian
      external julian

      integer seed  ! random number seed
      real singlerand  ! holding variable for one-time generated random

************** index to the HGMR of the lrsegs
      integer indHG

******** temporary storage variables to calcuate daily bmps efficiencies
      real DailyIndEff(ndaymax,maxBMPcon,MaxBmpTypes) ! individual bmp 

      logical NoBmpAcres(MaxBmpTypes) ! does this BMP exist in lu & seg

********* temporary variables to calculate the decrement
      real Umax,Umin  ! uniform distribution max and min

************* END DECLARATIONS *****************************************
      do nBmp = 1,nBmpTypes
        if (NoBmpAcres(nBmp)) cycle
        do nb = 1,nbcon
          if (BmpEffDistType(nBmp,nb,indHG,l).eq.-999) then
            cycle         ! no effect ( not specified )
          else if (BmpEffDistType(nBmp,nb,indHG,l).eq.0) then 
            cycle         ! no effect  ( specified as no effect )
          else if (BmpEffDistType(nBmp,nb,indHG,l).eq.1) then ! daily U
            Umin = BmpEffDistParm(nBmp,nb,indHG,l,1)
            Umax = BmpEffDistParm(nBmp,nb,indHG,l,2)
            do j = 1,ndays
              seed = seed + 1
              DailyIndEff(j,nb,nBmp) = DailyIndEff(j,nb,nBmp) * 
     .          (Umin + (Umax-Umin)*rand(seed))
            end do
          else if (BmpEffDistType(nBmp,nb,indHG,l).eq.2) then ! daily U
            Umin = DailyIndEff(j,nb,nBmp) *
     .                    BmpEffDistParm(nBmp,nb,indHG,l,1)
            Umax = DailyIndEff(j,nb,nBmp) / 
     .                    BmpEffDistParm(nBmp,nb,indHG,l,1)
            do j = 1,ndays
              seed = seed + 1
              DailyIndEff(j,nb,nBmp) = DailyIndEff(j,nb,nBmp) * 
     .          (Umin + (Umax-Umin)*rand(seed))
            end do
            if (BmpEffDistParm(nBmp,nb,indHG,l,2).eq.1) then
              do j = 1,ndays
                DailyIndEff(j,nb,nBmp) = max(DailyIndEff(j,nb,nBmp),0.)
                DailyIndEff(j,nb,nBmp) = min(DailyIndEff(j,nb,nBmp),1.)
              end do 
            end if

          else if (BmpEffDistType(nBmp,nb,indHG,l).eq.3) then ! daily N
            go to 993

          else if (BmpEffDistType(nBmp,nb,indHG,l).eq.4) then ! single U
            seed = seed + 1
            singlerand = rand(seed)
            Umin = BmpEffDistParm(nBmp,nb,indHG,l,1)
            Umax = BmpEffDistParm(nBmp,nb,indHG,l,2)
            do j = 1,ndays
              DailyIndEff(j,nb,nBmp) = DailyIndEff(j,nb,nBmp) * 
     .          (Umin + (Umax-Umin)*singlerand)
            end do

          else if (BmpEffDistType(nBmp,nb,indHG,l).eq.5) then ! single U
            seed = seed + 1
            singlerand = rand(seed)
            Umin = DailyIndEff(j,nb,nBmp) *
     .                    BmpEffDistParm(nBmp,nb,indHG,l,1)
            Umax = DailyIndEff(j,nb,nBmp) / 
     .                    BmpEffDistParm(nBmp,nb,indHG,l,1)
            do j = 1,ndays
              DailyIndEff(j,nb,nBmp) = DailyIndEff(j,nb,nBmp) * 
     .          (Umin + (Umax-Umin)*singlerand)
            end do
            if (BmpEffDistParm(nBmp,nb,indHG,l,2).eq.1) then
              do j = 1,ndays
                DailyIndEff(j,nb,nBmp) = max(DailyIndEff(j,nb,nBmp),0.0)
                DailyIndEff(j,nb,nBmp) = min(DailyIndEff(j,nb,nBmp),1.0)
              end do 
            end if

          else if (BmpEffDistType(nBmp,nb,indHG,l).eq.6) then ! single N
            go to 993

          else
            go to 992   ! error
          end if

        end do
      end do

      return
************** ERROR SPACE *********************************************
992   report(1) = 'Bmp random effect type unknown'
      write(report(2),*)'BMP: ',BmpName(nBmp),' effect type: ',
     .                  BmpEffDistType(nBmp,nb,indHG,l),' land use: ',
     .                  luname(l),' constituent: ',BMPconname(nb)
      report(3) = ' check hydrologic_parameters.csv file'
      go to 999

993   report(1) = 'Bmp Random effects 3 & 6 (normal distribution) not'
      report(2) = 'yet programmed.'
      report(3) = ''
      go to 999
999   call stopreport(report)

      end
