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
     I                  rsegs,csegs,nrsegs,ncsegs,goodobs,weight,
     I                  concname,nconcs,itnum,lakeflags,rscen,
     I                  paramscen,parAorM,noObsSim,oldparval,
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
     I                overscour,
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

      integer nr,it,j  ! indices

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

      real maxbrtam,maxbrpo4  ! max fraction of segment input due to 
      parameter (maxbrtam = 0.6, maxbrpo4 = 0.6) ! benthal release

      real getsens
      external getsens

      real Ofactor,Ofac1,Ofac2,Ofac3,Ofac4,Ofac5
      real TNOfactor,TPOfactor,PO4Ofactor,NH3Ofactor,NO3Ofactor

      integer ncORGN,ncORGP
      real Nfactor,Pfactor,NOfactor,POfactor

      real REAsen(maxrsegs),SUPSsen(maxrsegs),BENODsen(maxrsegs)
      real MALGRsen(maxrsegs),PHYSETsen(maxrsegs),CMsen(maxrsegs)
      real CWsen(maxrsegs),CTAUCSsen(maxrsegs),CTAUCDsen(maxrsegs)
      real SMsen(maxrsegs),REFSETsen(maxrsegs),KNO320sen(maxrsegs)
      real BRTAMsen(maxrsegs),BDNH4Csen(maxrsegs),KTAM20sen(maxrsegs)
      real BRPO4sen(maxrsegs),BDPO4Csen(maxrsegs),ADPO4Csen(maxrsegs)

      character*2 cnum
      integer lencnum
*************** END DECLARATIONS ***************************************

**  find biases for all consitutents at all stations for all iterations
      do it = 1,itnum
        do nc = 1,nconcs
          do ns = 1,ncsegs
            avebias(ns,nc,it) = 0.0
            if (missing(ksKstat(ns,nc,it),misval)) then  ! no data, set to zero
              do nb = 1,nbinmax
                bias(ns,nc,nb,it) = 0.
              end do
            else                 ! if enough data
              do nb = 1,nbinmax
                bias(ns,nc,nb,it) = sim(ns,nc,nb,it)-obs(ns,nc,nb,it)
                avebias(ns,nc,it) = avebias(ns,nc,it)+bias(ns,nc,nb,it)
              end do
              avebias(ns,nc,it) = avebias(ns,nc,it) / real(nbinmax)
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
      tconc = 'ORGN'   ! find nc
      ncORGN = findnc(concname,nconcs,tconc)
      tconc = 'ORGP'   ! find nc
      ncORGP = findnc(concname,nconcs,tconc)


      do ns = 1,ncsegs
        if (missing(ksKstat(ns,ncTN,itnum),misval) .or. 
     .      missing(ksKstat(ns,ncTP,itnum),misval)) then
          TNTPbalance(ns) = -9.0
        else
          TNTPbalance(ns) = obs(ns,ncTN,3,itnum)/obs(ns,ncTP,3,itnum)
        end if
        if (missing(ksKstat(ns,ncTN,itnum),misval) .or. 
     .      missing(ksKstat(ns,ncTOC,itnum),misval)) then
          TNTOCbalance(ns) = -9.0
        else
          TNTOCbalance(ns) = obs(ns,ncTN,3,itnum)/obs(ns,ncTOC,3,itnum)
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

      do nr = 1,nrsegs  ! loop over segments
        factor = 0.0  ! calculate weighted change figure
        Ofactor = 0.0
        do ns = 1,ncsegs  ! loop over calibration segments
          factor = factor + avebias(ns,nc,itnum) * weight(nr,ns,nc)
          Ofactor= Ofactor+ avebias(ns,nc,itnum-1)*weight(nr,ns,nc)
        end do

        if (lakeflags(nr).eq.0) np = kREAK  ! assign np
        if (lakeflags(nr).eq.1) np = kCFOREA
 
        sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr))
        REAsen(nr) = sensitivity
        sensitivity = min(sensitivity,1.)  ! expected sensitivity of 0.1
        sensitivity = max(sensitivity,.01)
        sensitivity = sensitivity * 10.0  ! under estimate sensitivity
        if (itnum .eq. 1) sensitivity = .1 

        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
      end do

************** no need to adjust sensitivity here
      np = kSUPSAT
      do nr = 1,nrsegs 
        factor = 0.0
        Ofactor = 0.0
        fac1 = 0.0    ! test to see if high bias is worse than overall
        do ns = 1,ncsegs
          factor = factor + (weight(nr,ns,nc)*
     .                    (bias(ns,nc,4,itnum)+bias(ns,nc,5,itnum)))
          Ofactor= Ofactor + (weight(nr,ns,nc)*
     .                    (bias(ns,nc,4,itnum-1)+bias(ns,nc,5,itnum-1)))
          fac1 = fac1 + weight(nr,ns,nc)*avebias(ns,nc,itnum)
        end do

        factor = factor / 2.0  ! averaging two bins
        Ofactor = Ofactor / 2.0  

        if (factor*fac1.gt.0.0) then  ! if bias in same direction
          if (abs(factor).lt.abs(fac1)) cycle  ! do nothing
        end if
 
        if (abs(parval(np,nr)-oldparval(np,nr)).gt.0.001) then
          sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                          parval(np,nr),oldparval(np,nr))
          SUPSsen(nr) = sensitivity
        else
          sensitivity = 1.
        end if
        sensitivity = min(sensitivity,1.)  ! expected sensitivity of 1
        sensitivity = max(sensitivity,.01)
        sensitivity = sensitivity * 10.0  ! under estimate sensitivity
        if (itnum .eq. 1) sensitivity = .1

        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
      end do
         
      np = kBENOD
      sensitivity = -0.005
      do nr = 1,nrsegs 
        factor = 0.0
        Ofactor = 0.0
        fac1 = 0.0  ! test to see if low bias is worse than overall
        do ns = 1,ncsegs
          factor = factor + (weight(nr,ns,nc)*
     .                    (bias(ns,nc,1,itnum)+bias(ns,nc,2,itnum)))
          Ofactor = Ofactor + (weight(nr,ns,nc)*
     .                    (bias(ns,nc,1,itnum-1)+bias(ns,nc,2,itnum-1)))
          fac1 = fac1 + weight(nr,ns,nc)*avebias(ns,nc,itnum)
        end do

        factor = factor / 2.0  ! averaging two bins
        Ofactor = Ofactor / 2.0  
        
        if (factor*fac1.gt.0.0) then  ! if bias in same direction
          if (abs(factor).lt.abs(fac1)) cycle  ! do nothing
        end if
 
        if (abs(parval(np,nr)-oldparval(np,nr)).gt.0.001) then
          sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                          parval(np,nr),oldparval(np,nr))
          BENODsen(nr) = sensitivity
        else
          sensitivity = 1.
        end if
        sensitivity = min(sensitivity,-.05)  ! expected sensitivity of 1
        sensitivity = max(sensitivity,-.0005)
        sensitivity = sensitivity * 10.0  ! under estimate sensitivity
        if (itnum .eq. 1) sensitivity = -0.005

        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
      end do

******** set anaer halfway between the first and second DO quintiles
      np = kANAER
      nc = ncDO
      do nr = 1,nrsegs
        parval(np,nr) = 10**
     .      ((noObsSim(nr,nc,1,itnum) + noObsSim(nr,nc,2,itnum))/2.0)
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
******** $tree/output/river/cfd/wqsT4>grep ',' *.CHLA
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

*********Max algae growth rate
        np = kMALGR ! assign np
        sensitivity = 20.0  ! assign factor
     
        if (noObsSim(nr,nc,5,itnum).gt.fac2.or.
     .      noObsSim(nr,nc,5,itnum).lt.fac3) then
          factor = noObsSim(nr,nc,5,itnum)-(fac2+fac3)/2.0
          Ofactor = noObsSim(nr,nc,5,itnum-1)-(fac2+fac3)/2.0
        else   ! in a good range, constrain with downstream

          factor = 0.0
          Ofactor = 0.0       
          do ns = 1,ncsegs
            factor = factor + (weight(nr,ns,nc)*bias(ns,nc,5,itnum))
            Ofactor = Ofactor + (weight(nr,ns,nc)*bias(ns,nc,5,itnum-1))
          end do
        end if

        sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr))
        MALGRsen(nr) = sensitivity
        sensitivity = min(sensitivity,200.)  ! expected sensitivity of 0.1
        sensitivity = max(sensitivity,2.)
        sensitivity = sensitivity * 10.0  ! under estimate sensitivity
        if (itnum .eq. 1) sensitivity = 20.

        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))

********* Phytoplankton settling rate
        np = kPHYSET ! assign np
        sensitivity = -10.  ! assign factor
        factor = (noObsSim(nr,nc,2,itnum) +
     .            noObsSim(nr,nc,3,itnum) +
     .            noObsSim(nr,nc,4,itnum))/3.0

        Ofactor = (noObsSim(nr,nc,2,itnum-1)+
     .            noObsSim(nr,nc,3,itnum-1) +
     .            noObsSim(nr,nc,4,itnum-1))/3.0

        if (factor.gt.fac4 .or. factor.lt.fac5) then
          factor = factor-(fac4+fac5)/2.0
          Ofactor = Ofactor-(fac4+fac5)/2.0
        else
          factor = 0.0
          Ofactor = 0.0 
         do ns = 1,ncsegs
            factor = factor + (weight(nr,ns,nc) *(bias(ns,nc,2,itnum)
     .                      + bias(ns,nc,3,itnum)+bias(ns,nc,4,itnum)))

            Ofactor =Ofactor+(weight(nr,ns,nc)*(bias(ns,nc,2,itnum-1)
     .                    +bias(ns,nc,3,itnum-1)+bias(ns,nc,4,itnum-1)))

          end do
          factor = factor / 3.0  ! averaging three bins
          Ofactor = Ofactor / 3.0 
        end if

        sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr))
        PHYSETsen(nr) = sensitivity
        sensitivity = min(sensitivity,-100.)  ! expected sensitivity of 0.1
        sensitivity = max(sensitivity,-1.)
        sensitivity = sensitivity * 10.0  ! under estimate sensitivity
        if (itnum .eq. 1) sensitivity = -10.

        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))

      end do


************** other fixes
C        do nb = 1,nbinmax ! if resurrected, need to fix indices
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
*********** 
********** if the out file reader finds overscouring, increase TAUCS

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

******** for clay          
        np = kCW     ! np
        sensitivity = -.5  ! settling V up, sediment down
        factor = 0.0
        Ofactor = 0.0
        do ns = 1,ncsegs
          factor = factor + (weight(nr,ns,nc)*avebias(ns,nc,itnum))
          Ofactor = Ofactor + (weight(nr,ns,nc)*avebias(ns,nc,itnum-1))
        end do

        sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr))
        sensitivity = min(sensitivity,-5.)  
        sensitivity = max(sensitivity,-.05)
        sensitivity = sensitivity * 10.0  ! under estimate sensitivity
        if (itnum .eq. 1) sensitivity = -.5

        write(99,*)'CW ',parval(kCW,nr),' ',rsegs(nr),' ',factor
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))

********* for Silt
        np = kSW     ! np
        factor = 0.0
        Ofactor = 0.0
        do ns = 1,ncsegs
          factor  = factor + weight(nr,ns,nc)*
     .              ((bias(ns,nc,4,itnum)+bias(ns,nc,5,itnum))/2.)
          Ofactor = Ofactor + weight(nr,ns,nc)*
     .              ((bias(ns,nc,4,itnum-1)+bias(ns,nc,5,itnum-1))/2.)
        end do

        sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr))
        sensitivity = min(sensitivity,-5.)
        sensitivity = max(sensitivity,-.05)
        sensitivity = sensitivity * 10.0  ! under estimate sensitivity
        if (itnum .eq. 1) sensitivity = -.5

        write(99,*)'SW ',parval(kSW,nr),' ',rsegs(nr)
        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))

      end do

************ REACHES
      do nr = 1,nrsegs

        if (lakeflags(nr).eq.1) cycle

************ if overscouring, reset TAUCS to higher percentile
*********** half as far from 100
        if (overscour(nr).or.silttf(nr).gt.5.0.or.claytf(nr).gt.5.0)then
          np = kCTAUCS        ! np
          percentile=findpercenttau(rsegs(nr),paramscen,parval(np,nr))
          percentile = 100.0 - (100.0-percentile)/2.0  ! half as far

          parval(np,nr)=findtaupercent(rsegs(nr),paramscen,percentile)

          np = kSTAUCS  ! silt half as far again
          percentile = 100.0 - (100.0-percentile)/2.0  ! silt pctle
          parval(np,nr)=findtaupercent(rsegs(nr),paramscen,percentile)

          cycle  ! run with modified taus only
        end if

        fac1 = 0.0
        fac2 = 0.0
        fac3 = 0.0
        fac4 = 0.0
        fac5 = 0.0

        Ofac1 = 0.0
        Ofac2 = 0.0
        Ofac3 = 0.0
        Ofac4 = 0.0
        Ofac5 = 0.0

        do ns = 1,ncsegs
          fac1 = fac1 + (weight(nr,ns,nc)*bias(ns,nc,1,itnum))
          fac2 = fac2 + (weight(nr,ns,nc)*bias(ns,nc,2,itnum))
          fac3 = fac3 + (weight(nr,ns,nc)*bias(ns,nc,3,itnum))
          fac4 = fac4 + (weight(nr,ns,nc)*bias(ns,nc,4,itnum))
          fac5 = fac5 + (weight(nr,ns,nc)*bias(ns,nc,5,itnum))

          Ofac1 = Ofac1 + (weight(nr,ns,nc)*bias(ns,nc,1,itnum-1))
          Ofac2 = Ofac2 + (weight(nr,ns,nc)*bias(ns,nc,2,itnum-1))
          Ofac3 = Ofac3 + (weight(nr,ns,nc)*bias(ns,nc,3,itnum-1))
          Ofac4 = Ofac4 + (weight(nr,ns,nc)*bias(ns,nc,4,itnum-1))
          Ofac5 = Ofac5 + (weight(nr,ns,nc)*bias(ns,nc,5,itnum-1))
        end do

************* TAUCS is related to the balance of bin3 and bin5
        np = kCTAUCS        ! np
        sensitivity = .10 ! lower tau, lower factor

        percentile=findpercenttau(rsegs(nr),paramscen,parval(np,nr))

        factor = fac5-fac3
        Ofactor = Ofac5-Ofac3

        sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr))
        CTAUCSsen(nr) = sensitivity
        sensitivity = min(sensitivity,1.)  ! expected sensitivity of 0.1
        sensitivity = max(sensitivity,.01)
        sensitivity = sensitivity * 10.0  ! under estimate sensitivity
        if (itnum .eq. 1) sensitivity = .10

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
        np = kCTAUCD        ! np
        sensitivity = -.30 ! raise tau, lower factor

        percentile=findpercenttau(rsegs(nr),paramscen,parval(np,nr))

        factor = fac3-fac1
        Ofactor = Ofac3-Ofac1

        sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr))
        CTAUCDsen(nr) = sensitivity
        sensitivity = min(sensitivity,-3.)  ! expected sensitivity of 0.1
        sensitivity = max(sensitivity,-.03)
        sensitivity = sensitivity * 10.0  ! under estimate sensitivity
        if (itnum .eq. 1) sensitivity = -.3

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
        np = kSM  ! np
        sensitivity = 1.0  ! raise SM, raise bin5

        factor = fac5
        Ofactor = Ofac5      
        sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr))
        SMsen(nr) = sensitivity
        sensitivity = min(sensitivity,10.)  ! expected sensitivity of 0.1
        sensitivity = max(sensitivity,.1)
        sensitivity = sensitivity * 10.0  ! under estimate sensitivity
        if (itnum .eq. 1) sensitivity = .1

        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))

*************for Clay 
        np = kCM  ! np
        sensitivity = 1.0  ! raise CM, raise bins 4&5

        factor = ( fac5+fac4 ) /2.0
        Ofactor = (Ofac5+Ofac4 ) /2.0

        sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr))
        CMsen(nr) = sensitivity
        sensitivity = min(sensitivity,10.)  ! expected sensitivity of 0.1
        sensitivity = max(sensitivity,.1)
        sensitivity = sensitivity * 10.0  ! under estimate sensitivity
        if (itnum .eq. 1) sensitivity = 1.

        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))


**************** SW = 10*CW Use low average for settling
********** if bias5 is persistently high and SM is low,
*********     use overall bias
        np = kCW     ! np
        sensitivity = -10.0  ! settling V up, sediment down

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
            Ofactor = (Ofac1+Ofac2+Ofac3+Ofac4+Ofac5)/5.0
          else
            factor = (fac2+fac3)/2.0
            Ofactor = (Ofac2+Ofac3)/2.0
          end if
        else
          factor = (fac2+fac3)/2.0
          Ofactor = (Ofac2+Ofac3)/2.0
        end if

        sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr))
        CWsen(nr) = sensitivity
        sensitivity = min(sensitivity,-100.)  ! expected sensitivity of 0.1
        sensitivity = max(sensitivity,-1.)
        sensitivity = sensitivity * 10.0  ! under estimate sensitivity
        if (itnum .eq. 1) sensitivity = -10.

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
        factor = 0.0
        Ofactor = 0.0
        do ns = 1,ncsegs
          if (goodobs(ns,ncORGP).gt. 0.5*goodobs(ns,ncTP)) then    ! if observed ORGP data points > half of TP points
            nc = ncORGP                                            ! use ORGP, otherwise, use TP 
          else
            nc = ncTP
          end if
 
          factor = factor + weight(nr,ns,nc) *
     .             ( bias(ns,nc,3,itnum)
     .             + bias(ns,nc,4,itnum)
     .             + bias(ns,nc,5,itnum))/3.0
C     .             ( bias(ns,nc,1,itnum)
C     .             + bias(ns,nc,2,itnum)
C     .             + bias(ns,nc,3,itnum))/3.0 
 
          Ofactor = Ofactor + weight(nr,ns,nc) *
     .             ( bias(ns,nc,3,itnum-1)
     .             + bias(ns,nc,4,itnum-1)
     .             + bias(ns,nc,5,itnum-1))
     .             / 3.0
        end do
        Pfactor  = factor
        POfactor = Ofactor

        factor = 0.0
        Ofactor = 0.0
        do ns = 1,ncsegs! if obs ORGN data points > half of TN points
          if (goodobs(ns,ncORGN).gt. 0.5*goodobs(ns,ncTN)) then
            nc = ncORGN     ! use ORGN, otherwise, use TN
          else
            nc = ncTN
          end if

          factor = factor + weight(nr,ns,nc) * avebias(ns,nc,itnum)
          Ofactor= Ofactor + weight(nr,ns,nc) * avebias(ns,nc,itnum-1)
        end do
        Nfactor  = factor
        NOfactor = Ofactor
        
        nc = ncTOC
        factor = 0.0
        do ns = 1,ncsegs
          factor = factor + weight(nr,ns,nc) * avebias(ns,nc,itnum)
        end do
        TOCfactor = factor

C C       factor = (TPfactor + TNfactor + TOCfactor) / 3.0
        factor = (Pfactor + Nfactor ) / 2.0
        Ofactor= (POfactor + NOfactor ) / 2.0 

        sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr))
        REFSETsen(nr) = sensitivity
        sensitivity = min(sensitivity,-1.5)  ! expected sensitivity of 1
        sensitivity = max(sensitivity,-.015)
        sensitivity = sensitivity * 20.0  ! under-estimate sensitivity to avoid oscillation
        if (itnum .eq. 1) sensitivity = -0.15

        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
      end do

************* DENITRIFICATION
********** denitrify if no3 is high and tn is high, or conversely
      np = kKNO320
      do nr = 1,nrsegs

        nc = ncTN
        factor = 0.0
        do ns = 1,ncsegs
          factor = factor + (weight(nr,ns,nc) * 
     .    (bias(ns,nc,1,itnum)+bias(ns,nc,2,itnum)+bias(ns,nc,3,itnum)))
C     .    (bias(ns,nc,3,itnum)+bias(ns,nc,4,itnum)+bias(ns,nc,5,itnum)))
C     .            ( 2.0*bias(ns,nc,1,itnum)+bias(ns,nc,2,itnum)))
        end do
        TNfactor = factor / 3.0

        nc = ncNO3
        factor = 0.0
        Ofactor = 0.0
        do ns = 1,ncsegs
          factor = factor + (weight(nr,ns,nc) *(bias(ns,nc,1,itnum)
     .              +bias(ns,nc,2,itnum)+bias(ns,nc,3,itnum)))
C     .    (bias(ns,nc,3,itnum)+bias(ns,nc,4,itnum)+bias(ns,nc,5,itnum)))
C     .            ( 2.0*bias(ns,nc,1,itnum)+bias(ns,nc,2,itnum)))
          Ofactor = Ofactor + (weight(nr,ns,nc) *(bias(ns,nc,1,itnum-1)
     .              +bias(ns,nc,2,itnum-1)+bias(ns,nc,3,itnum-1)))
C     .    (bias(ns,nc,3,itnum-1)+bias(ns,nc,4,itnum-1)+bias(ns,nc,5,itnum-1)))
C     .            ( 2.0*bias(ns,nc,1,itnum-1)+bias(ns,nc,2,itnum-1)))
        end do
        NO3factor = factor / 3.0
        Ofactor   = Ofactor / 3.0

        sensitivity = getsens(NO3factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr))
        KNO320sen(nr) = sensitivity
        sensitivity = min(sensitivity,-40.)  ! expected sensitivity of 1
        sensitivity = max(sensitivity,-.4)
        sensitivity = sensitivity * 10.0  ! under-estimate sensitivity to avoid oscillation
        if (itnum .eq. 1) sensitivity = -4.

        if (TNfactor*NO3factor.gt.-0.001) then  ! agreement
          call factorpar(
     I                   NO3factor,sensitivity,parAorM(np),
     I                   parmin(np),parmax(np),
     M                   parval(np,nr))
        end if
      end do

      
*************** lake benthic release of nh3
*********** tie to overall mass balance
********** should not be more than 60% of TN
      np = kBRTAM1

      do nr = 1,nrsegs
        if (lakeflags(nr) .eq. 0) cycle    !for reservoir ONLY


************** check for orphan seg
        fac1 = 0
        do ns = 1,ncsegs
          fac1 = fac1 + weight(nr,ns,nc)
        end do
        if (fac1.lt.0.1) then  ! orphan segment for TN

          factor = tntf(nr) - 0.3
          sensitivity = 10
          if (factor .lt. 0.0 ) then  
            factor = factor - (factor/maxbrtam)*tnbrtam(nr)
          end if

        else

          nc = ncTN
          factor = 0.0
          Ofactor = 0.0
          do ns = 1,ncsegs
            factor = factor + (weight(nr,ns,nc) * avebias(ns,nc,itnum))
            Ofactor = Ofactor +(weight(nr,ns,nc)*avebias(ns,nc,itnum-1))
          end do

*********** if factor is negative, reduce factor so that
********** max ben release is observed
          if (factor .lt. 0.0 ) then  
            factor = factor - (factor/maxbrtam)*tnbrtam(nr)
          end if

          sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                          parval(np,nr),oldparval(np,nr))
          BRTAMsen(nr) = sensitivity
          sensitivity = min(sensitivity,200.)  ! expected sensitivity of 1
          sensitivity = max(sensitivity,2.)
          sensitivity = sensitivity * 10.0  ! under-estimate sensitivity to avoid oscillation
          if (itnum .eq. 1) sensitivity = 20.

        end if

        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
        parval(kBRTAM2,nr) = parval(np,nr) * 5.0
      end do

************* scour of ammonia on sediment
******** This has an effect on the high values of TOTN
      np = kBEDNH4CLAY
      sensitivity = 5.0

      do nr = 1,nrsegs
        if (lakeflags(nr) .eq. 1) cycle      ! for reaches only, NOT reservoir

        nc = ncTN
        factor = 0.0
        Ofactor = 0.0      
        do ns = 1,ncsegs
          factor = factor + (weight(nr,ns,nc) * 
     .                (bias(ns,nc,4,itnum) + bias(ns,nc,5,itnum))/2.0)
          Ofactor = Ofactor + (weight(nr,ns,nc) *
     .              (bias(ns,nc,4,itnum-1)+ bias(ns,nc,5,itnum-1))/2.0)
        end do

        sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr))
        BDNH4Csen(nr) = sensitivity
        sensitivity = min(sensitivity,50.)  ! expected sensitivity of 1
        sensitivity = max(sensitivity,.5)
        sensitivity = sensitivity * 10.0  ! under-estimate sensitivity to avoid oscillation
        if (itnum .eq. 1) sensitivity = 5.

        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
        parval(kBEDNH4SILT,nr) = parval(np,nr) / 10.0
        parval(kBEDNH4SAND,nr) = parval(np,nr) / 100.0
      end do

************* DIN balance
*******  KTAM20 is the best control on the DIN balance
      np = kKTAM20
      sensitivity = -1.
      do nr = 1,nrsegs

        nc = ncNH3
        factor = 0.0
        Ofactor = 0.0
        do ns = 1,ncsegs
          factor = factor + weight(nr,ns,nc) * avebias(ns,nc,itnum)
          Ofactor = Ofactor + weight(nr,ns,nc)*avebias(ns,nc,itnum-1)
        end do
        NH3factor = factor
        NH3Ofactor = Ofactor
        
        nc = ncNO3
        factor = 0.0
        Ofactor = 0.0
        do ns = 1,ncsegs
          factor = factor + weight(nr,ns,nc) * avebias(ns,nc,itnum)
          Ofactor = Ofactor + weight(nr,ns,nc)*avebias(ns,nc,itnum-1)
        end do
        NO3factor = factor
        NO3Ofactor = Ofactor

        factor = NH3factor - NO3factor
        Ofactor = NH3Ofactor - NO3Ofactor

        sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr)) 
        KTAM20sen(nr) = sensitivity 
        sensitivity = min(sensitivity,-10.)  ! expected sensitivity of 1
        sensitivity = max(sensitivity,-.1)
        sensitivity = sensitivity * 10.0  ! under-estimate sensitivity to avoid oscillation
        if (itnum .eq. 1) sensitivity = -1.

        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
      end do

********* benthal release of po4 in reservoirs can help increase
********** totp if needed
      nc = ncTP
      np = kBRPO41
      sensitivity = 10.0
      do nr = 1,nrsegs
        if (lakeflags(nr) .eq. 0) cycle       ! for reservoir ONLY

************** check for orphan seg
        fac1 = 0
        do ns = 1,ncsegs
          fac1 = fac1 + weight(nr,ns,nc)
        end do
        if (fac1.lt.0.1) then  ! orphan segment for TP

          factor = tptf(nr) - 0.3
          sensitivity = 10
          if (factor .lt. 0.0 ) then 
            factor = factor - (factor/maxbrpo4)*tpbrpo4(nr)
          end if

        else
         
          factor = 0.0   ! factor is bias
          Ofactor = 0.0
          do ns = 1,ncsegs
            factor = factor + weight(nr,ns,nc) *
     .               ( bias(ns,nc,1,itnum)
     .               + bias(ns,nc,2,itnum)
     .               + bias(ns,nc,3,itnum)
     .               + bias(ns,nc,4,itnum))
            Ofactor = Ofactor + weight(nr,ns,nc) *
     .               ( bias(ns,nc,1,itnum-1)
     .               + bias(ns,nc,2,itnum-1)
     .               + bias(ns,nc,3,itnum-1)
     .               + bias(ns,nc,4,itnum-1))
          end do
          factor = factor / 4.0
          Ofactor = Ofactor / 4.0
       
*********** if factor is negative, reduce factor so that
********** max ben release is observed
          if (factor .lt. 0.0 ) then  ! if negative
            factor = factor - (factor/maxbrpo4)*tpbrpo4(nr)
          end if

          sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                          parval(np,nr),oldparval(np,nr))
          BRPO4sen(nr) = sensitivity
          sensitivity = min(sensitivity,100.)  ! expected sensitivity of 1
          sensitivity = max(sensitivity,1.)
          sensitivity = sensitivity * 10.0  ! under-estimate sensitivity to avoid oscillation
          if (itnum .eq. 1) sensitivity = 10.

        end if

        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
        parval(kBRPO42,nr) = parval(np,nr) * 5.0

      end do

*************** bed concentrations of PO4 drive the upper
******* portions of the CFD, keep clay = 10*silt = 100*sand 
*********** this part of the CFD also drives the loads       
      nc = ncTP
      np = kBEDPO4CLAY
      sensitivity = 50.

      do nr = 1,nrsegs
        if (lakeflags(nr) .eq. 1) cycle      ! for reaches only, NOT reservoir      

        factor = 0.0
        Ofactor = 0.0
        do ns = 1,ncsegs
          factor = factor + weight(nr,ns,nc) *
     .                 (bias(ns,nc,5,itnum)+ bias(ns,nc,4,itnum))/2.0
          Ofactor = Ofactor + weight(nr,ns,nc) * 
     .                 (bias(ns,nc,5,itnum-1)+ bias(ns,nc,4,itnum-1))/2.0
        end do

        sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr))
        BDPO4Csen(nr) = sensitivity
        sensitivity = min(sensitivity,500.)  ! expected sensitivity of 1
        sensitivity = max(sensitivity,5.)
        sensitivity = sensitivity * 10.0  ! under-estimate sensitivity to avoid oscillation
        if (itnum .eq. 1) sensitivity = 50.

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
        Ofactor = 0.0
        do ns = 1,ncsegs
          factor = factor + weight(nr,ns,nc) *             !  avebias(ns,nc,itnum)
     .                 (bias(ns,nc,5,itnum)+ bias(ns,nc,4,itnum))/2.0
          Ofactor = Ofactor + weight(nr,ns,nc)*            !  avebias(ns,nc,itnum-1)
     .                 (bias(ns,nc,5,itnum-1)+ bias(ns,nc,4,itnum-1))/2.0
        end do

        sensitivity = getsens(factor,Ofactor,parAorM(np),
     I                        parval(np,nr),oldparval(np,nr))
        ADPO4Csen(nr) = sensitivity
        sensitivity = min(sensitivity,-10.)  ! expected sensitivity of 1
        sensitivity = max(sensitivity,-.1)
        sensitivity = sensitivity * 10.0     ! opposite direction to factor
        if (itnum .eq. 1) sensitivity = -1.

        call factorpar(
     I                 factor,sensitivity,parAorM(np),
     I                 parmin(np),parmax(np),
     M                 parval(np,nr))
        parval(kADSPO4SILT,nr) = parval(np,nr) / 3.0
        parval(kADSPO4SAND,nr) = parval(np,nr) / 9.0
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

********* store calculated sensitivity for each parm at each iteration
      write(cnum,'(i2)') itnum
      do j = 1,2
        if (cnum(j:j).eq.' ') cnum(j:j) = '0'
      end do

      call lencl(rscen,lenrscen)        
      fnam = tree//'output/river/summary/'//rscen(:lenrscen)  
     .           //'/sensitivity_'//cnum//'.csv'
        
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
        
      write(dfile,*)'Rseg,REAK,SUPSAT,BENOD,MALGR,PHYSET,CTAUCS,
     . CTAUCD,SM,CM,CW,REFSET,KNO320,BRTAM,BEDNH4CLAY,KTAM20,
     . BRPO4,BEDPO4CLAY,ADSPO4CLAY'
      do nr = 1,nrsegs
        write(dfile,1234) rsegs(nr),REAsen(nr),SUPSsen(nr),
     .           BENODsen(nr),MALGRsen(nr),PHYSETsen(nr),
     .           CTAUCSsen(nr),CTAUCDsen(nr),SMsen(nr),CMsen(nr),
     .           CWsen(nr),REFSETsen(nr),KNO320sen(nr),BRTAMsen(nr),
     .           BDNH4Csen(nr),KTAM20sen(nr),BRPO4sen(nr),
     .           BDPO4Csen(nr),ADPO4Csen(nr)
      end do

      close(dfile)

1234  format(a13,18(',',f8.2))

      return
********************** ERROR SPACE *************************************
991   report(1) = 'problem open file'
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)

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

************************************************************************
**  function to calculate senstivity                                  **
************************************************************************
      function getsens(factor,oldfactor,parAorM,par,oldpar)

      implicit none
      real getsens
      character*1 parAorM
      real factor,oldfactor,par,oldpar

      if (parAorM.eq.'A') then
        if (abs(par-oldpar).le.0.0001) then
          getsens = 100.
        else
          getsens = (factor-oldfactor)/(par-oldpar)
        end if
      else if (parAorM.eq.'M') then
        if (abs(log10(par)-log10(oldpar)).le.0.0001) then
          getsens = 100.
        else
          getsens = (factor-oldfactor)/(log10(par)-log10(oldpar)) 
        end if
      end if
      return
      end
