************************************************************************
**  The purpose of this program is to calculate the statistics for    **
**  pollutant concentrations                                          **
**                                                                    **
**  The Statistics are calculated for both base variables and         **
**  log transformed variables                                         **
************************************************************************
      subroutine avmoneff(obconc,simconc,ndays,ndaymax,obmonth, ! compute model efficient of average monthly concentration
     .                    pltfil,suffix,rseg,stat)

      implicit none
   
      integer ndays,ndaymax    ! number of days
      integer nd,nm            ! index

      integer pltfil               ! file number
      integer err, stat

      character(4) suffix
      character(13) rseg
      character(64) report(3)

************ variables to compute statistics 
      real obconc(ndaymax),simconc(ndaymax) ! daily concentration
      real obmonconc(12),simonconc(12)      ! monthly concentration
      integer npts(12)
      integer obmonth(ndaymax)

**********  regression variables
      real olvar,elvar          ! obs, sim, error variances
      real ovar,evar             ! obs, sim, error variances
      real nsel,nse                   ! nash-sutcliff efficiency
      real variance,errorvar
      external variance,errorvar

      integer maxmon,nmon
      data maxmon /12/
******************* END DECLARATIONS ***********************************
      do nm = 1,12
        npts(nm) = 0
        obmonconc(nm) = 0.
        simonconc(nm) = 0.
      end do

      do nd = 1,ndays
        do nm = 1,12
          if (nm .eq. obmonth(nd)) then
            if (obconc(nd).gt.1.0e-8 .and. simconc(nd).gt.1.0e-8) then  ! valid and pairing data only
              npts(nm)      = npts(nm) + 1
              obmonconc(nm) = obmonconc(nm) + obconc(nd)
              simonconc(nm) = simonconc(nm)+ simconc(nd)
            end if
          end if
        end do
      end do

********* get rid of month without data
      nmon = 12
      do nm = 1,12     
        if (nm .gt. nmon) exit
        if (npts(nm) .eq. 0) then     ! if missing data in this month
          do while (npts(nmon) .eq. 0)
            nmon = nmon - 1
            if (nmon.lt.nm) exit
          end do
          if (nmon.lt.nm) exit
          obmonconc(nm) = obmonconc(nmon)  ! this destroyes the order
          simonconc(nm) = simonconc(nmon)  ! but not the pairing 
          nmon = nmon - 1
        end if
      end do
      
********* calculate average monthly concentration
      do nm = 1,nmon
        obmonconc(nm) = obmonconc(nm)/npts(nm)
        simonconc(nm) = simonconc(nm)/npts(nm)
      end do

*********** Calculate basic statistics on untransformed variables
      ovar = variance(obmonconc,nmon,maxmon,err)
      if (err.ne.0) ovar = -9
       
      evar = errorvar(obmonconc,simonconc,nmon,maxmon,err)
      if (err.ne.0) evar = -9
      
      if (ovar.le.0.0001) then
        nse = -9
      else
        nse = (ovar-evar)/ovar
      end if

*********** log transform data
      do nm = 1,nmon               ! log transform 
        obmonconc(nm) = log(obmonconc(nm))
        simonconc(nm) = log(simonconc(nm))
      end do

********** calculate basic statistics on transformed variables
      olvar = variance(obmonconc,nmon,maxmon,err)
      if (err.ne.0) ovar = -9

      elvar = errorvar(obmonconc,simonconc,nmon,maxmon,err)
      if (err.ne.0) evar = -9

      if (olvar.le.0.0001) then
        nsel = -9
      else
        nsel = (olvar-elvar)/olvar
      end if

************* write statistics to output file
      write(pltfil,400,err=951) 'mon efficiency    ,',nse
      write(pltfil,400,err=951) 'mon efficiency log,',nsel
     
400   format(1x,a19,1x,f10.3)
    
      return
951   report(1) = 'error writing to file'
      report(2) = ' '
      report(3) = 'possible permission problem'
      go to 999

999   call stopreport(report)

      end 


