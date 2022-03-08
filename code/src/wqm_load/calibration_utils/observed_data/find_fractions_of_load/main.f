************************************************************************
** program to determine necessary baseflow/stormflow load ratios in   **
**  the stream network that should be coming from the land uses       **
**                                                                    **
**  The idea is to find the baseflow concentration distribution by    **
**    evaluating the concentration only on days that are >90% baseflow**
**  Once you have the distribution of baseflows, evaluate the storm   **
**    concentration by subtracting out a baseflow load from the       **
**    daily load based on the distribution of baseflows and generate  **
**    distributions of storm flow concentrations.                     **
**                                                                    **
**  Then run the model forward for each flow day to generate a        **
**    distribution of base and storm loads                            **
**  This could be better done with more time using ESTIMATOR          **
**                                                                    **
************************************************************************
      implicit none
      include '/model/p510/pp/src/lib/inc/standard.inc'
      integer maxobs, nobs, nflo, n, nbobs, nqobs
      parameter (maxobs = 10000)
      integer minyear
      parameter (minyear = 1984)
      integer year,month,day,hour,minute

      integer nbins, nb, nb2
      parameter (nbins=9)

      integer flowyear(maxobs)
      integer flowmonth(maxobs)
      integer flowday(maxobs)
      real flow(maxobs), bflw(maxobs), qflw(maxobs)

      integer concyear(maxobs)
      integer concmonth(maxobs)
      integer concday(maxobs)
      real conc(maxobs),concflow(maxobs),concb(maxobs),concq(maxobs)

      real baseconc(maxobs), baseconcpct(nbins)
      real stormconc(maxobs), stormconcpct(nbins,nbins)
      real baseload(nbins),stormload(nbins,nbins)
      real allbase,allstorm
      real allstormload(nbins*nbins)
      real pct

      integer nconcs, nc
      parameter (nconcs = 10)
      character*4 cnames(nconcs)
      data cnames/'NH3X','NO3X','ORGN','ORGP','NO23',
     .            'PO4X','TOCX','TOTN','TOTP','TSSX'/

      real addvector,percentile
      external addvector,percentile

******************** END DECLARATIONS **********************************

      read*,rseg

******** open flow files
      fnam = '/model/p5_observed/FLOW/'//rseg//'.OFLOW'
      open (11,file=fnam,status='old',iostat=err)
      if (err.ne.0) stop

      fnam = '/model/p5_observed/QFLW/'//rseg//'.OQFLW'
      open (12,file=fnam,status='old',iostat=err)
      if (err.ne.0) stop

      fnam = '/model/p5_observed/BFLW/'//rseg//'.OBFLW'
      open (13,file=fnam,status='old',iostat=err)
      if (err.ne.0) stop

*********** read flow file
      year = minyear-1
      do while (year.lt.minyear)
        read(11,*,end=991) year
      end do
      
      backspace 11
      nflo = 1
      do 
        read(11,*,end=111) flowyear(nflo),flowmonth(nflo),flowday(nflo),
     .                     hour,minute,flow(nflo)
        nflo = nflo + 1
      end do
111   close(11)
      nflo = nflo - 1
     
*********** read quick flow file
      year = minyear-1
      do while (year.lt.minyear)
        read(12,*,end=991) year
      end do
      
      backspace 12
      do n = 1,nflo
        read(12,*,end=992) year,month,day,
     .                     hour,minute,qflw(n)
      end do
      read(12,*,end=222) year
      go to 993  ! should not be successful in above read statment

222   close(12)
     
*********** read quick flow file
      year = minyear-1
      do while (year.lt.minyear)
        read(13,*,end=991) year
      end do
      
      backspace 13
      do n = 1,nflo
        read(13,*,end=994) year,month,day,
     .                     hour,minute,bflw(n)
      end do
      read(13,*,end=333) year
      go to 995  ! should not be successful in above read statment

333   close(13)

************ got all flow info, now loop over all possible concs
      do nc = 1,nconcs


******************* read concentration file
        fnam = '/model/p5_observed/'//cnames(nc)//'/'//rseg//
     .         '.O'//cnames(nc)
        open (14,file=fnam,status='old',iostat=err)
        if (err.ne.0) cycle  ! only do concs that exist

        year = minyear-1
        do while (year.lt.minyear)
          read(14,*,end=991) year
        end do

        backspace 14
        nobs = 1
        n = 1
        do   ! read each conc and assign flow, bflw, and qflw
          read(14,*,end=444) concyear(nobs),concmonth(nobs),
     .                       concday(nobs),hour,minute,conc(nobs)
          do while (concyear(nobs).ne.flowyear(n))
            n = n + 1
            if (n.gt.nflo) exit
          end do
          do while (concmonth(nobs).ne.flowmonth(n))
            n = n + 1
            if (n.gt.nflo) exit
          end do
          do while (concday(nobs).ne.flowday(n))
            n = n + 1
            if (n.gt.nflo) exit
          end do
          if (n.gt.nflo) exit
          concflow(nobs) = flow(n)
          concb(nobs) = bflw(n)
          concq(nobs) = qflw(n)
           
          nobs = nobs + 1
        end do
444     close(14)
        nobs = nobs - 1
        if (nobs.lt.30) cycle

************* find distribution of baseflow concentrations
        nbobs = 0
        do n = 1,nobs
          if (concb(n)/concflow(n).gt.0.9) then ! baseflow
            nbobs = nbobs + 1
            baseconc(nbobs) = conc(n)
          end if
        end do


        do nb = 1,nbins
          pct = real(nb)/real(nbins+1)*100.0   ! weibull
          baseconcpct(nb) = percentile(pct,baseconc,nbobs,maxobs,err)

*********** find stormflow concentrations by subtracting
********** out the portion of load from baseflow
**********  perform calculation only when stormflow is over 50%
          nqobs = 0
          do n = 1,nobs
            if (concq(n)/concflow(n).gt.0.5) then ! stormflow
              nqobs = nqobs + 1
              stormconc(nqobs) = (conc(n)*concflow(n)
     .                            -baseconcpct(nb)*concb(n))
     .                                /concq(n)
            end if
          end do

          do nb2 = 1,nbins
            pct = real(nb2)/real(nbins+1)*100.0   ! weibull
            stormconcpct(nb,nb2) = percentile(
     I                             pct,stormconc,nqobs,maxobs,err)
          end do

        end do

********** we now have a very simple model with a base concentration
**********  and a storm concentration
        allbase = addvector(bflw,n,maxobs,err)
        allstorm = addvector(qflw,n,maxobs,err)
        do nb = 1,nbins
          baseload(nb) = allbase*baseconcpct(nb)
          do nb2 = 1,nbins
            stormload(nb,nb2) = allstorm*stormconcpct(nb,nb2)
          end do
        end do

        do nb = 1,nbins
          do nb2 = 1,nbins
            allstormload((nb-1)*nbins+nb2) = stormload(nb,nb2)
          end do
        end do

        nqobs = nbins*nbins
        do nb = 1,nbins
          pct = real(nb)/real(nbins+1)*100.0   ! weibull
          stormload(nb,1)= percentile(pct,allstormload,nqobs,maxobs,err)
        end do

        print 1234,rseg,',',cnames(nc),',base',
     .         (',',baseload(nb),nb=1,nbins)
        print 1234,rseg,',',cnames(nc),',storm',
     .         (',',stormload(nb,1),nb=1,nbins)

      end do
      stop
1234  format(a13,a1,a4,a6,10(a1,f14.0))
991   stop

992   print*,'fewer lines in the quick flow file than in the flow file'
      stop

993   print*,'more lines in the quick flow file than in the flow file'
      stop

994   print*,'fewer lines in the base flow file than in the flow file'
      stop

995   print*,'more lines in the base flow file than in the flow file'
      stop

      end
