************************************************************************
** This program updates the parameters based on a system of rules     **
**  responding to simulated and observed quintiles of the paried CFDs **
** The rules are hard-coded into this subroutine to allow for         **
**  maximum flexibility                                               **
**                                                                    **
**  for each type of concentration, calculate the change for each     **
**   station and then apply it to all upstream rivers using the       **
**   weights                                                          **
**                                                                    **
**  in general, sensitivities are calculated as follows               **
**  S = (SimNew-SimOld) / (ParNew-ParOld)   --- Additive              **
**         or                                                         **
**  S = (SimNew-SimOld / (log10(ParNew)-log10(ParOld)) --- Multiplic  **
** so updates are as follows, given you want SimNew = Obs             **
**  ParNew = ParOld + (Obs-Sim)/S  -- additive                        **
**  ParNew = ParOld * 10^((Obs-Sim)/S) -- Multiplicative              **
**    sensitivities should be estimated high to avoid oscillation     **
************************************************************************
************************************************************************
**  General strategy for calibrating sensitivities in this file:      **
**    run small basins and check how the sources and sinks change     **
**    over iterations in the summary file:                            **
**      output/river/summary/$scen/sumout_iter_$basin.csv             **
**    If there is oscillation, then sensitivities are too low         **
**    If there is no movement or not asymptotic, sensitivities are    **
**      too high.  
************************************************************************
      subroutine newPar(
     I                  parUsed,parmin,parmax,sim,obs,ksKstat,
     I                  rsegs,csegs,nrsegs,ncsegs,weight,
     I                  concname,nconcs,itnum,lakeflags,
     I                  paramscen,parAorM,noObsSim,
     I                sandin,sandout,sandtf,
     I                siltin,siltout,silttf,
     I                clayin,clayout,claytf,
     I                sanddiv,siltdiv,claydiv,
     I                tnin,tnout,tntf,tnbrbod,tnbods,tnbenal,
     I                tntamscr,tnbrtam,tntamvol,tnnitden,
     I                tnrefset,tnphyset,tndiv,
     I                tpin,tpout,tptf,tpbrbod,tpbods,tpbenal,
     I                tppo4scr,tpbrpo4,tprefset,tpphyset,tpdiv,
     I                Csandin,Csandout,Csandtf,
     I                Csiltin,Csiltout,Csilttf,
     I                Cclayin,Cclayout,Cclaytf,
     I                Csanddiv,Csiltdiv,Cclaydiv,
     I                Ctnin,Ctnout,Ctntf,Ctnbrbod,Ctnbods,
     I                Ctnbenal,Ctntamscr,Ctnbrtam,Ctntamvol,
     I                Ctnnitden,Ctnrefset,Ctnphyset,Ctndiv,
     I                Ctpin,Ctpout,Ctptf,Ctpbrbod,Ctpbods,
     I                Ctpbenal,Ctppo4scr,Ctpbrpo4,Ctprefset,
     I                Ctpphyset,Ctpdiv,
     I                  kREAK,kCFOREA,kSUPSAT,kBENOD,
     I                  kCTAUCS,kSTAUCS,kCTAUCD,kSTAUCD,
     I                  kSM,kCM,kCW,kSW,kKSAND,kEXPSND,
     I                  kPHYSET,kREFSET,kMALGR,kALDL,kTALGRM,
     I                  kSEED,kMXSTAY,kKTAM20,kKNO320,kDENOXT,
     I                  kBRPO41,kBRPO42,kBRTAM1,kBRTAM2,kANAER,
     I                  kBEDNH4SILT,kBEDNH4CLAY,kBEDNH4SAND,
     I                  kADSNH4SILT,kADSNH4CLAY,kADSNH4SAND,
     I                  kBEDPO4SILT,kBEDPO4CLAY,kBEDPO4SAND,
     I                  kADSPO4SILT,kADSPO4CLAY,kADSPO4SAND,
     M                  parval)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'WQcal.inc'
      include 'params.inc'
      include 'outfile.inc'

      integer nr,it  ! indices

      character*4 tconc

      real bias(maxcsegs,nconcmax,nbinmax,maxitnum)
      real avebias(maxcsegs,nconcmax,maxitnum)
      real sensitivity
      real percentile  ! percentile for TAU calcs

      integer findnc
      real findpercenttau,findtaupercent
      external findnc,findpercenttau,findtaupercent

      real Tmin,Tmax
      real factor,fac1,fac2,fac3,fac4,fac5,oldfac5
      real TNfactor,TPfactor,TOCfactor,PO4factor,NH3factor,NO3factor

      integer ncTN, ncTP, ncTOC, ncPO4, ncNH3, ncNO3, ncDO

      real TNTPbalance(maxcsegs)
      real TNTOCbalance(maxcsegs)

*************** END DECLARATIONS ***************************************

**  find biases for all consitutents at all stations for all iterations
      do it = 1,itnum
        do nc = 1,nconcs
          do ns = 1,ncsegs
            if (ksKstat(ns,nc,it).gt.-1.0) then  ! if enough data
              avebias(ns,nc,it) = 0.0
              do nb = 1,nbinmax
                bias(ns,nc,nb,it) = sim(ns,nc,nb,it)-obs(ns,nc,nb,it)
                avebias(ns,nc,it) = avebias(ns,nc,it)+bias(ns,nc,nb,it)
              end do
              avebias(ns,nc,it) = avebias(ns,nc,it) / real(nbinmax)
            else                 ! no data, set to zero
              do nb = 1,nbinmax
                bias(ns,nc,nb,it) = 0.
              end do
              avebias(ns,nc,it) = 0.
            end if
          end do
        end do
      end do

******** find observed tn tp toc balance for each station
      tconc = 'TOTN'   ! find nc
      ncTN = findnc(concname,nconcs,tconc)
      tconc = 'TOTP'   ! find nc
      ncTP = findnc(concname,nconcs,tconc)
      tconc = 'TOCX'   ! find nc
      ncTOC = findnc(concname,nconcs,tconc)
      tconc = 'PO4X'   ! find nc
      ncPO4 = findnc(concname,nconcs,tconc)
      tconc = 'NH3X'   ! find nc
      ncNH3 = findnc(concname,nconcs,tconc)
      tconc = 'NO3X'   ! find nc
      ncNO3 = findnc(concname,nconcs,tconc)
      tconc = 'DOXX'   ! find nc
      ncDO = findnc(concname,nconcs,tconc)

      do ns = 1,ncsegs
        if (ksKstat(ns,ncTN,itnum).gt.-1.0 .and. 
     .      ksKstat(ns,ncTP,itnum).gt.-1.0) then
          TNTPbalance(ns) = obs(ns,ncTN,3,itnum)/obs(ns,ncTP,3,itnum)
        else
          TNTPbalance(ns) = -9.0
        end if
        if (ksKstat(ns,ncTN,itnum).gt.-1.0 .and. 
     .      ksKstat(ns,ncTOC,itnum).gt.-1.0) then
          TNTOCbalance(ns) = obs(ns,ncTN,3,itnum)/obs(ns,ncTOC,3,itnum)
        else
          TNTOCbalance(ns) = -9.0
        end if
      end do

************* DOXX
********* DO is mainly controlled by REAK for rivers and CFOREA for res
******* Several parameters can alter the seasonal pattern
******** move bins 4 and 5 though SUPSAT between 1.1 and 1.35
********* move bins 1 and 2 through BENOD
********* works pretty well
      tconc = 'DOXX'   ! find nc
      nc = findnc(concname,nconcs,tconc)

      sensitivity = .1  ! assign factor
      do nr = 1,nrsegs  ! loop over segments
        factor = 0.0  ! calculate weighted change figure
        do ns = 1,ncsegs  ! loop over calibration segments
          factor = factor + avebias(ns,nc,itnum) * weight(nr,ns,nc)
        end do

        if (lakeflags(nr).eq.0) np = kREAK  ! assign np
        if (lakeflags(nr).eq.1) np = kCFOREA
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
      end do


      np = kSUPSAT
      do nr = 1,nrsegs 
        factor = 0.0
        fac1 = 0.0    ! test to see if high bias is worse than overall
        do ns = 1,ncsegs
          factor = factor + 
     .     (weight(nr,ns,nc)*(bias(ns,nc,4,itnum)+bias(ns,nc,5,itnum)))
          fac1 = fac1 + weight(nr,ns,nc)*avebias(ns,nc,itnum)
        end do
        factor = factor / 2.0  ! averaging two bins
        if (factor*fac1.gt.0.0) then  ! if bias in same direction
          if (abs(factor).lt.abs(fac1)) cycle  ! do nothing
        end if
        factor = - factor   ! max adjustment of about .2
        call addpar(
     I              factor,parmin(np),parmax(np),   
     M              parval(np,nr))
      end do
         
      np = kBENOD
      sensitivity = -0.005
      do nr = 1,nrsegs 
        factor = 0.0
        fac1 = 0.0  ! test to see if low bias is worse than overall
        do ns = 1,ncsegs
          factor = factor +
     .     (weight(nr,ns,nc)*(bias(ns,nc,1,itnum)+bias(ns,nc,2,itnum)))
          fac1 = fac1 + weight(nr,ns,nc)*avebias(ns,nc,itnum)
        end do
        factor = factor / 2.0  ! averaging two bins
        if (factor*fac1.gt.0.0) then  ! if bias in same direction
          if (abs(factor).lt.abs(fac1)) cycle  ! do nothing
        end if
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
      end do

************ CHLA
********* PHYSET is the main control on Chlorophyll
********  MALGR is also important, especially for higher concs
********* use PHYSET for bins 1-3 and MALGR on 4-5
*********** calculate overall log of bias and adjust malgr if the higher
*********   concs are more out of shape, otherwise adjust physet
********* if algae can't grow, and physet is low and malgr is high
********    , relax LITSED, ALDL and MALGR
************ do not let physet get rid of more than 10% of the N in
*********** any one segment
********** works pretty well
      tconc = 'CHLA'   ! find nc
      nc = findnc(concname,nconcs,tconc)

************* check if no data downstream, if so
*********** put simulation in interquartile range for other segs
******* if not orphan, still maintain within observed max and min
******** if within observed range, calibrate to downstream station
      do nr = 1,nrsegs
        fac1 = 0
        do ns = 1,ncsegs
          fac1 = fac1 + weight(nr,ns,nc)
        end do
        if (fac1.lt.0.1) then  ! orphan segment for chlorophyll
          fac2 = 1.0  ! 75th percentile of bin 5
          fac3 = 0.8  ! 25th percentile of bin 5
          fac4 = 0.45  ! 75th percentile of bins 2,3,4
          fac5 = 0.15 ! 25th percentile of bin 2,3,4
        else  ! has downstream data, constrain to obs max and min
          fac2 = 1.4  ! max of bin 5
          fac3 = 0.45  ! min of bin 5
          fac4 = .72  ! max of bins 2,3,4
          fac5 = 0.0 ! min of bin 2,3,4
        end if

        np = kMALGR ! assign np
        sensitivity = 20.0  ! assign factor
        if (noObsSim(nr,nc,5,itnum).gt.fac2.or.
     .      noObsSim(nr,nc,5,itnum).lt.fac3) then
          factor = noObsSim(nr,nc,5,itnum)-(fac2+fac3)/2.0
          call factorpar(
     I               factor,sensitivity,parAorM(np),
     I               parmin(np),parmax(np),
     M               parval(np,nr))
        else   ! in a good range, constrain with downstream
          factor = 0.0
          do ns = 1,ncsegs
            factor = factor + (weight(nr,ns,nc)*bias(ns,nc,5,itnum))
          end do
          call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
        end if

        np = kPHYSET ! assign np
        sensitivity = -10.  ! assign factor
        factor = (noObsSim(nr,nc,2,itnum) +
     .            noObsSim(nr,nc,3,itnum) +
     .            noObsSim(nr,nc,4,itnum))/3.0
        if (factor.gt.fac4 .or. factor.lt.fac5) then
          factor = factor-(fac4+fac5)/2.0
          np = kPHYSET ! assign np
          sensitivity = -10.  ! assign factor
          call factorpar(
     I               factor,sensitivity,parAorM(np),
     I               parmin(np),parmax(np),
     M               parval(np,nr))
        else
          factor = 0.0
          do ns = 1,ncsegs
            factor = factor + (weight(nr,ns,nc) *
     .    (bias(ns,nc,2,itnum)+bias(ns,nc,3,itnum)+bias(ns,nc,4,itnum)))
          end do
          factor = factor / 3.0  ! averaging three bins
          call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
        end if

      end do


************** other fixes
C        do nb = 1,nbinmax
C          if (sim(nc,nb,itnum).lt.0.05*obs(nc,nb,itnum)) then ! no grow
C            np = kALDL
C            mult = 0.9
C            do nr = 1,nrsegs
C              call multpar(
C     I                     mult,parmin(np),parmax(np),
C     M                     parval(np,nr))
C            end do
C            exit  ! just do this once per iteration
C          end if
C        end do  
C        
C      end if

************* TSSX
********* TSS is controlled by different parameters, depending
******** on the concentration  
**********   bin 5 = SM,  bin 4&5 = CM,
**********   taucs is based on the relationship between bins 3,4,5
**********      if 5 is more biased than 3, lower taucs to even out
**********   low bins are deposition
*********      taucd starts low around 50%
*********      bin1,2 = CW, bin2,3 = SW
*********
**********     if M are near low limits, raise deposition very high
*********        and calibrate settling to overall balance
**********  
*********** if reservoir, set the M values to zero and calibrate them
*********** by hand for individual events, calibrate all reservoir
********** sediment through settling velocities in this program
**********
******** sand is not a big factor, it should have a delivery factor in 
********* the low single digits

      tconc = 'TSSX'   ! find nc
      nc = findnc(concname,nconcs,tconc)

************ RESERVOIRS
      do nr = 1,nrsegs
        if (lakeflags(nr).eq.0) cycle

        if (itnum.eq.1) then  ! set initial
          percentile = 100
          parval(kCTAUCS,nr)=
     .           findtaupercent(rsegs(nr),paramscen,percentile)
          parval(kCTAUCD,nr) = parval(kCTAUCS,nr)
          parval(kSTAUCS,nr) = parval(kCTAUCS,nr)
          parval(kSTAUCD,nr) = parval(kCTAUCS,nr)
          parval(kCM,nr) = 0.0001
          parval(kSM,nr) = 0.0001
          cycle
        end if
          
        factor = 0.0
        do ns = 1,ncsegs
          factor = factor + (weight(nr,ns,nc)*avebias(ns,nc,itnum))
        end do
        np = kCW     ! np
        sensitivity = -.5  ! settling V up, sediment down
        write(99,*)'CW ',parval(kCW,nr),' ',rsegs(nr),' ',factor
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
        write(99,*)'CW ',parval(kCW,nr),' ',rsegs(nr)
        write(99,*)'SW ',parval(kSW,nr),' ',rsegs(nr)
        parval(kSW,nr) = parval(np,nr) * 10.0
        write(99,*)'SW ',parval(kSW,nr),' ',rsegs(nr)

      end do

************ REACHES
      do nr = 1,nrsegs

        if (lakeflags(nr).eq.1) cycle

        fac1 = 0.0
        fac2 = 0.0
        fac3 = 0.0
        fac4 = 0.0
        fac5 = 0.0
        do ns = 1,ncsegs
          fac1 = fac1 + (weight(nr,ns,nc)*bias(ns,nc,1,itnum))
          fac2 = fac2 + (weight(nr,ns,nc)*bias(ns,nc,2,itnum))
          fac3 = fac3 + (weight(nr,ns,nc)*bias(ns,nc,3,itnum))
          fac4 = fac4 + (weight(nr,ns,nc)*bias(ns,nc,4,itnum))
          fac5 = fac5 + (weight(nr,ns,nc)*bias(ns,nc,5,itnum))
        end do

************* TAUCS is related to the balance of bin3 and bin5
        factor = fac5-fac3
        sensitivity = .10 ! lower tau, lower factor
        np = kCTAUCS        ! np
        percentile=findpercenttau(rsegs(nr),paramscen,parval(np,nr))
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 percentile)
        parval(np,nr)=findtaupercent(rsegs(nr),paramscen,percentile)
**************** keep silt taus half as far from 100
        percentile = 100.0 - (100.0-percentile)/2.0  ! silt pctle
        np = kSTAUCS
        parval(np,nr)=findtaupercent(rsegs(nr),paramscen,percentile)


************* TAUCD is related to the balance of bin3 and bin1
        factor = fac3-fac1
        sensitivity = -.30 ! raise tau, lower factor
        np = kCTAUCD        ! np
        percentile=findpercenttau(rsegs(nr),paramscen,parval(np,nr))
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 percentile)
        parval(np,nr)=findtaupercent(rsegs(nr),paramscen,percentile)
**************** keep silt taus half as far from 100
        percentile = 100.0 - (100.0-percentile)/2.0  ! silt pctle
        np = kSTAUCD
        parval(np,nr)=findtaupercent(rsegs(nr),paramscen,percentile)


************ erodibility factors tie silt to bin 5 and clay to bins 4&5
        factor = fac5
        sensitivity = 1.0  ! raise SM, raise bin5
        np = kSM  ! np
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))

        factor = ( fac5+fac4 ) /2.0
        sensitivity = 1.0  ! raise CM, raise bins 4&5
        np = kCM  ! np
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))



**************** SW = 10*CW Use low average for settling
********** if bias5 is persistently high and SM is low,
*********     use overall bias
        if (abs(parval(kSM,nr)-parmin(kSM)).lt.0.0001) then  ! if SM low

          if (itnum.gt.2) then  ! calculate previous high bias
            oldfac5 = 0.0
            do ns = 1,ncsegs
              oldfac5=oldfac5+(weight(nr,ns,nc)*bias(ns,nc,5,itnum-1))
            end do
          else
            oldfac5 = 10.0 * fac5
          end if

          if (fac5.gt.0.0001) then          ! calculate change in bias
            oldfac5 = (oldfac5-fac5)/fac5
          else
            oldfac5 = 1.0
          end if


          if (oldfac5.lt.0.02) then   ! less than 2% change
            factor = (fac1+fac2+fac3+fac4+fac5)/5.0
          else
C            factor = (fac1+fac2+fac3)/3.0
            factor = (fac2+fac3)/2.0
          end if
        else
C          factor = (fac1+fac2+fac3)/3.0
          factor = (fac2+fac3)/2.0
        end if

        np = kCW     ! np
        sensitivity = -10.0  ! settling V up, sediment down
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
        parval(kSW,nr) = parval(np,nr) * 10.0

      end do


************ sand, set initial k to 0.0001 and exp to 4
********* get delivery factor between 0.01 and 0.10
      if (itnum.eq.1) then
        np = kKSAND
        do nr = 1,nrsegs
          parval(np,nr) = 0.0001
        end do
        np = kEXPSND
        do nr = 1,nrsegs
          parval(np,nr) = 4.0
        end do
      else
        np = kKSAND
        do nr = 1,nrsegs
          factor = 0.0
          if (lakeflags(nr).eq.1) then
            if (sandtf(nr).gt.0.01) then
              factor = -1.0
            end if
          else
            if (sandtf(nr).gt.0.10) then
              factor = -1.0
            else if (sandtf(nr).lt.0.01) then
              factor = factor + 1.0
            end if
          end if
          parval(np,nr) = parval(np,nr)*10.0**factor
        end do
      end if


************* for nutrients, do by parameter rather than consituent


*********** REFSET
********** REFSET is a general control TN, TP, and TOC
********* use average of all 3 if agreement
********** find different method for TOC, too much uncertainty
      np = kREFSET
      sensitivity = -.15
      do nr = 1,nrsegs
        nc = ncTP
        factor = 0.0
        do ns = 1,ncsegs
          factor = factor + weight(nr,ns,nc) *
     .             ( 4.0*bias(ns,nc,1,itnum)
     .             + 3.0*bias(ns,nc,2,itnum)
     .             + 2.0*bias(ns,nc,3,itnum)) 
     .             / 9.0 
C     .             / 9.0 * TNTPbalance(ns)
        end do
        TPfactor = factor
       

        nc = ncTN
        factor = 0.0
        do ns = 1,ncsegs
          factor = factor + weight(nr,ns,nc) *
     .             ( 5.0*bias(ns,nc,1,itnum)
     .             + 4.0*bias(ns,nc,2,itnum)
     .             + 3.0*bias(ns,nc,3,itnum)
     .             + 2.0*bias(ns,nc,4,itnum)
     .             + 1.0*bias(ns,nc,5,itnum))
     .             /15.0
        end do
        TNfactor = factor

        nc = ncTOC
        factor = 0.0
        do ns = 1,ncsegs
          factor = factor + weight(nr,ns,nc) *
     .             ( 5.0*bias(ns,nc,1,itnum)
     .             + 4.0*bias(ns,nc,2,itnum)
     .             + 3.0*bias(ns,nc,3,itnum)
     .             + 2.0*bias(ns,nc,4,itnum)
     .             + 1.0*bias(ns,nc,5,itnum))
     .             /15.0 
C     .             /15.0 * TNTOCbalance(ns)
        end do
        TOCfactor = factor

C        factor = (TPfactor + TNfactor + TOCfactor) / 3.0
        factor = (TPfactor + TNfactor ) / 2.0

        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
      end do

************* DENITRIFICATION
********** denitrify if no3 is high and tn is high, or conversely
      np = kKNO320
      sensitivity = -10.
      do nr = 1,nrsegs

        nc = ncTN
        factor = 0.0
        do ns = 1,ncsegs
          factor = factor + (weight(nr,ns,nc) * 
     .            ( 2.0*bias(ns,nc,1,itnum)+bias(ns,nc,2,itnum)))
        end do
        TNfactor = factor / 3.0

        nc = ncNO3
        factor = 0.0
        do ns = 1,ncsegs
          factor = factor + (weight(nr,ns,nc) * 
     .            ( 2.0*bias(ns,nc,1,itnum)+bias(ns,nc,2,itnum)))
        end do
        NO3factor = factor / 3.0

        if (TNfactor*NO3factor.gt.-0.001) then  ! agreement
          call factorpar(
     I                   NO3factor,sensitivity,parAorM(np),
     I                   parmin(np),parmax(np),
     M                   parval(np,nr))
        end if
      end do

      

*************** lake benthic release of nh3
*********** tie to overall mass balance
********** should not be more than 20% of TN
      nc = ncTN
      np = kBRTAM1
      sensitivity = 0.05
      do nr = 1,nrsegs
        if (lakeflags(nr).ne.1) cycle
        do ns = 1,ncsegs
          if (Ctnbrtam(ns).gt. 0.20) then
            factor = factor + weight(nr,ns,nc) * 1.0
          else
            factor = factor + (weight(nr,ns,nc) * avebias(ns,nc,itnum))
          end if
        end do
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
        parval(kBRTAM2,nr) = parval(np,nr) * 5.0
      end do

************* scour of ammonia on sediment
******** This has an effect on the high values of TOTN
      nc = ncTN
      np = kBEDNH4CLAY
      sensitivity = 5.0
      do nr = 1,nrsegs
        if (lakeflags(nr).ne.0) cycle
        do ns = 1,ncsegs
          factor = factor + (weight(nr,ns,nc) * 
     .                ( bias(ns,nc,4,itnum) + bias(ns,nc,5,itnum))/2.0)
        end do
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
        parval(kBEDNH4SILT,nr) = parval(np,nr) / 10.0
        parval(kBEDNH4SAND,nr) = parval(np,nr) / 100.0
      end do

********* benthal release of po4 in reservoirs can help increase
********** totp if needed
      np = kBRPO41
      sensitivity = 0.5
      do nr = 1,nrsegs
        if (lakeflags(nr).ne.1) cycle
        factor = 0.0
        do ns = 1,ncsegs
          factor = factor + weight(nr,ns,nc) *
     .             ( 3.0*bias(ns,nc,1,itnum)
     .             + 2.0*bias(ns,nc,2,itnum)
     .             + 1.0*bias(ns,nc,3,itnum))
        end do
        factor = factor / 6.0
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
        parval(kBRPO42,nr) = parval(np,nr) * 5.0

      end do

******** set anaer halfway between the first and second DO quintiles
      np = kANAER
      nc = ncDO
      do nr = 1,nrsegs
        factor = 0.0
        do ns = 1,ncsegs
          factor = factor + weight(nr,ns,nc) * 
     .             (obs(ns,nc,1,itnum)+obs(ns,nc,2,itnum))/2.0
        end do
        parval(np,nr) = factor
      end do

*************** bed concentrations of PO4 drive the upper
******* portions of the CFD, keep clay = 10*silt = 100*sand 
*********** this part of the CFD also drives the loads       
      nc = ncTP
      np = kBEDPO4CLAY
      sensitivity = 50.
      do nr = 1,nrsegs
        factor = 0.0
        do ns = 1,ncsegs
          factor = factor + weight(nr,ns,nc) * bias(ns,nc,5,itnum)
        end do
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
        parval(kBEDPO4SILT,nr) = parval(np,nr)/10.0
        parval(kBEDPO4SAND,nr) = parval(np,nr)/100.0
      end do

************* adsorption coefficient for PO4
********** Main idea is to balance the dissolved PO4 with the 
*********** PO4 sorbed.  The total PO4 is controlled by BEDCONC
************ the amount in solution is controlled by nut-adsparm
********** with a good portion having to do with chlorophyll
********* Can create po4 by removing from sediment nut-adsparm
************** clay = silt*3 = sand*9
      nc = ncPO4
      np = kADSPO4CLAY
      sensitivity = -1.
      do nr = 1,nrsegs
        factor = 0.0
        do ns = 1,ncsegs
          factor = factor + weight(nr,ns,nc) * avebias(ns,nc,itnum)
        end do
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
        parval(kADSPO4SILT,nr) = parval(np,nr) / 3.0
        parval(kADSPO4SAND,nr) = parval(np,nr) / 9.0
      end do

************* DIN balance
*******  KTAM20 is the best control on the DIN balance
      np = kKTAM20
      sensitivity = -1.
      do nr = 1,nrsegs

        nc = ncNH3
        factor = 0.0
        do ns = 1,ncsegs
          factor = factor + weight(nr,ns,nc) * avebias(ns,nc,itnum)
        end do
        NH3factor = factor

        nc = ncNO3
        factor = 0.0
        do ns = 1,ncsegs
          factor = factor + weight(nr,ns,nc) * avebias(ns,nc,itnum)
        end do
        NO3factor = factor

        factor = NH3factor - NO3factor
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
      end do

************* ORGN
******************* commented out because over-specified
************** when organic scour incorporated, then get this working
********* if PHYSET is controlled by CHLA then use refset
*******  otherwise, use physet
C      tconc = 'CHLA'
C      if (ksKstat(findnc(concname,nconcs,tconc),itnum).lt.-1.0) then
C
C        tconc = 'ORGN'   ! find nc
C        nc = findnc(concname,nconcs,tconc)
C        if (ksKstat(nc,itnum).gt.-1.0) then
C
C          np = kPHYSET
C          avebias = (5.0*(sim(nc,1,itnum)-obs(nc,1,itnum))
C     .            +  4.0*(sim(nc,2,itnum)-obs(nc,2,itnum)) 
C     .            +  3.0*(sim(nc,3,itnum)-obs(nc,3,itnum)) 
C     .            +  2.0*(sim(nc,4,itnum)-obs(nc,4,itnum)) 
C     .            +  1.0*(sim(nc,5,itnum)-obs(nc,5,itnum)) )
C     .                / 15.0   ! bias
C          sensitivity = -.1
C          do nr = 1,nrsegs
C            call factorpar(
C     I                     avebias,sensitivity,parAorM(np),
C     I                     parmin(np),parmax(np),
C     M                     parval(np,nr))
C          end do
C        end if
C
C      else
C
C        tconc = 'ORGN'   ! find nc
C        nc = findnc(concname,nconcs,tconc)
C        if (ksKstat(nc,itnum).gt.-1.0) then
C
C          np = kREFSET
C          avebias = (5.0*(sim(nc,1,itnum)-obs(nc,1,itnum))
C     .            +  4.0*(sim(nc,2,itnum)-obs(nc,2,itnum))
C     .            +  3.0*(sim(nc,3,itnum)-obs(nc,3,itnum))
C     .            +  2.0*(sim(nc,4,itnum)-obs(nc,4,itnum))
C     .            +  1.0*(sim(nc,5,itnum)-obs(nc,5,itnum)) )
C     .                / 15.0   ! bias
C          sensitivity = -.01
C          do nr = 1,nrsegs
C            call factorpar(
C     I                     avebias,sensitivity,parAorM(np),
C     I                     parmin(np),parmax(np),
C     M                     parval(np,nr))
C          end do
C        end if
C
C      end if

      return
********************** ERROR SPACE *************************************


      end

************************************************************************
**  function to find the index of the concentration                   **
************************************************************************
      function findnc(concname,nconcs,tconc)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'WQcal.inc'
      character*(*) tconc
      integer findnc

      do findnc = 1,nconcs
        if (concname(findnc).eq.tconc) return
      end do

      report(1) = 'could not find concentration '//tconc
      report(2) = 'requested by optimization code'
      report(3) = './control/calib/WQ/$calscen/newPar.f'
      call stopreport(report)
      end

************************************************************************
**  subroutine to multiply value by a factor                          **
**  in general, sensitivities are calculated as follows               **
**  S = (SimNew-SimOld) / (ParNew-ParOld)   --- Additive              **
**         or                                                         **
**  S = (SimNew-SimOld / (log10(ParNew)-log10(ParOld)) --- Multiplic  **
** so updates are as follows, given you want SimNew = Obs             **
**  ParNew = ParOld + (Obs-Sim)/S  -- additive                        **
**  ParNew = ParOld * 10^((Obs-Sim)/S) -- Multiplicative              **
**    sensitivities should be estimated high to avoid oscillation     **
************************************************************************
      subroutine factorpar(
     I                     bias,sens,parAorM,
     I                     parmin,parmax,
     M                     parval)
      implicit none
      real bias   ! obs-sim
      real sens  ! sensitivity factor - transition from bias to param
      character*1 parAorM  ! Additive or Multiplicative parameter
      real parmin,parmax   ! min and max allowable values
      real parval    ! parameter value to modify

      if (parAorM.eq.'A') then
        parval = parval - bias/sens
      else if (parAorM.eq.'M') then
        parval = parval / 10.0**(bias/sens)
      end if
      parval = min(parval,parmax)
      parval = max(parval,parmin)
      end
        
************************************************************************
**  subroutine to add a value to a parameter and check for min,max    **
************************************************************************
      subroutine addpar(
     I                  add,parmin,parmax,
     M                  parval)
      implicit none
      real add ! what to add to the factor
      real parmin,parmax   ! min and max allowable values
      real parval    ! parameter value to modify
      parval = parval + add
      parval = min(parval,parmax)
      parval = max(parval,parmin)
      end

************************************************************************
**  subroutine to multiply a parameter by a value, check for min,max  **
************************************************************************
      subroutine multpar(
     I                   mult,parmin,parmax,
     M                   parval)
      implicit none
      real mult ! what to multiply by
      real parmin,parmax   ! min and max allowable values
      real parval    ! parameter value to modify
      parval = parval * mult
      parval = min(parval,parmax)
      parval = max(parval,parmin)
      end

