************************************************************************
**  The purpose of this program is to calculate the statistics for    **
**  pollutant concentrations                                          **
**                                                                    **
**  The Statistics are calculated for both base variables and         **
**  log transformed variables                                         **
************************************************************************
      subroutine wqstats(
     I                   ob,si,nday2,ndaymax,
     I                   pltfil,suffix,rseg,stat)

      implicit none
   
      integer ndays, nday2         ! number of days

      integer nd                   ! index
      integer ndaymax

      integer pltfil               ! file number
      integer err, stat

      character(4) suffix
      character(13) rseg
      character(64) report(3)

**********  regression variables
      real obs(ndaymax),sim(ndaymax)  ! all vals
      real ob(ndaymax),si(ndaymax)    ! all vals

      real olvar,slvar,elvar          ! obs, sim, error variances
      real ovar,svar,evar             ! obs, sim, error variances
      real obar,sbar,olbar,slbar      ! means
      real omed,smed,olmed,slmed      ! medians
      real nsel,nse                   ! nash-sutcliff efficiency
      real r,rl                       ! correlation of log transforms
      real correl,variance,errorvar,mean,median
      external correl,variance,errorvar,mean,median

******************* END DECLARATIONS ***********************************
  
      ndays = nday2
      do nd = 1,ndays          ! variables will be modified
        obs(nd) = ob(nd)       ! so store them in alternate variables
        sim(nd) = si(nd)
      end do

      do nd = 1,nday2     ! get rid of missing values
        if (nd.gt.ndays) exit
        if (obs(nd).lt.-8.0 .or.sim(nd).lt.-8.0) then
          do while (obs(ndays).lt.-8.0.or.sim(ndays).lt.-8.0)
            ndays = ndays - 1
            if (ndays.lt.nd) exit
          end do
          if (ndays.lt.nd) exit
          obs(nd) = obs(ndays)      ! this destroyes the order
          sim(nd) = sim(ndays)      ! but not the pairing
          ndays = ndays - 1
        end if
      end do

*********** Calculate basic statistics on untransformed variables
      obar = mean(obs,ndays,ndaymax,err)
      if (err.ne.0) obar = -9
      
      sbar = mean(sim,ndays,ndaymax,err)
      if (err.ne.0) sbar = -9

      omed = median(obs,ndays,ndaymax,err)
      if (err.ne.0) omed = -9
      
      smed = median(sim,ndays,ndaymax,err)
      if (err.ne.0) smed = -9

      ovar = variance(obs,ndays,ndaymax,err)
      if (err.ne.0) ovar = -9

      svar = variance(sim,ndays,ndaymax,err)
      if (err.ne.0) svar = -9

      evar = errorvar(obs,sim,ndays,ndaymax,err)
      if (err.ne.0) evar = -9
      
      r = correl(obs,sim,ndays,ndaymax,err)
      if (err.ne.0) r = -9
     
      if (ovar.le.0.0001) then
        nse = -9
      else
        nse = (ovar-evar)/ovar
      end if

*********** log transform data
      do nd = 1,nday2     ! before transform, get rid of non-positives
        if (nd.gt.ndays) exit
        if (obs(nd).lt.1.0e-8 .or.sim(nd).lt.1.0e-8) then
          do while (obs(ndays).lt.1.0e-8.or.sim(ndays).lt.1.0e-8)
            ndays = ndays - 1
            if (ndays.lt.nd) exit
          end do
          if (ndays.lt.nd) exit
          obs(nd) = obs(ndays)      ! this destroyes the order
          sim(nd) = sim(ndays)      ! but not the pairing
          ndays = ndays - 1
        end if
      end do
      
      do nd = 1,ndays               ! log transform 
        obs(nd) = log(obs(nd))
        sim(nd) = log(sim(nd))
      end do

********** calculate basic statistics on transformed variables
      olbar = mean(obs,ndays,ndaymax,err)
      if (err.ne.0) olbar = -9
    
      slbar = mean(sim,ndays,ndaymax,err)
      if (err.ne.0) slbar = -9
             
      olmed = median(obs,ndays,ndaymax,err)
      if (err.ne.0) olmed = -9
    
      slmed = median(sim,ndays,ndaymax,err)
      if (err.ne.0) slmed = -9
             
      olvar = variance(obs,ndays,ndaymax,err)
      if (err.ne.0) olvar = -9
    
      slvar = variance(sim,ndays,ndaymax,err)
      if (err.ne.0) slvar = -9
      
      elvar = errorvar(obs,sim,ndays,ndaymax,err)
      if (err.ne.0) elvar = -9
 
      rl = correl(obs,sim,ndays,ndaymax,err)
      if (err.ne.0) rl = -9

      if (olvar.le.0.0001) then
        nsel = -9
      else
        nsel = (olvar-elvar)/olvar
      end if

************* write statistics to output file
      write(pltfil,100,err=951) suffix
      write(pltfil,200,err=951) 'n             ,',nday2,',',nday2 
      write(pltfil,300,err=951) 'mean          ,',sbar,',',obar
      write(pltfil,300,err=951) 'median        ,',smed,',',omed
      write(pltfil,400,err=951) 'correl        ,',r
      write(pltfil,400,err=951) 'efficiency    ,',nse
      write(pltfil,*,err=951) 'variance      ,',svar,',',ovar
      write(pltfil,*,err=951) 'errorvar      ,',evar
      write(pltfil,200,err=951) 'n          log,',ndays,',',ndays 
      write(pltfil,300,err=951) 'mean       log,',slbar,',',olbar
      write(pltfil,300,err=951) 'median     log,',slmed,',',olmed
      write(pltfil,400,err=951) 'correl     log,',rl
      write(pltfil,400,err=951) 'efficiency log,',nsel
      write(pltfil,300,err=951) 'variance   log,',slvar,',',olvar
      write(pltfil,400,err=951) 'errorvar   log,',elvar
     
100   format(1x,a4, 14x,'Simulated',4x,'Observed') 
200   format(1x,a15,4x,i4,4x,a1,4x,i4)
300   format(1x,a15,1x,e10.4,1x,a1,1x,e10.4)
400   format(1x,a15,1x,f10.3)
    
      return

************************ ERROR SPACE
951   report(1) = 'error writing to file'
      report(2) = ' '
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'problem with finding conc stats with observed'
      report(2) = '  Divide by zero'
      report(3) = '  for segment '//rseg
      go to 999

992   report(1) = 'problem with finding conc stats with simulated'
      report(2) = '  Divide by zero'
      report(3) = '  for segment '//rseg
      go to 999

993   report(1) = 'problem with finding corelations between observed'
      report(2) = '  and simulated'
      report(3) = '  for segment '//rseg
      go to 999

999   call stopreport(report)

      end 


