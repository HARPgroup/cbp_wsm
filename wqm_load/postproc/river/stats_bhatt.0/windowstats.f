************************************************************************
**  program to calculate statistics for 'windowed' concentration data **
**   and output the windowed data for plotting purposes in cal5       **
**  'Windowing' means two things:                                     **
**     Any simulated or observed value below LOD is set to LOD        **
**     The simulated value is set to the closest value to the         **
**        observed that the simulation obtained within the specified  **
**        time window                                                 **
************************************************************************
      subroutine windowstats(rscen,rseg,year1,year2,loadnm,
     I                       simfl,obfl,simconcdate,obconc,
     I                       obyear,obmonth,obday,obhour,obLOD,
     I                       ndays,window,dailytau)
    
      implicit none
      include 'Rstats.inc'

      character*100 pfnam,obldfnam,winfnam
      character*4 loadnm
      character*4 cy1,cy2

      integer i
      integer ny,nm,nd,nh,nh2,nday,ndays   ! indices
      integer year1,year2                   ! first and last year to average
       
******** flow and load variables
      real simconcdate(1984:2005,12,31,24)
      real dailytau(1984:2005,12,31)
      real obconc(ndaymax)
      real hconc,tconc

      real acfthr2cfs
      parameter (acfthr2cfs = 43560.0/3600)
 
******** variables to give statistics for a window of simulated values
      real windowsim(ndaymax)   ! concentration during the window 
                                !  closest to the observation
                                ! if obs=LOD and win<LOD, sim=obs
      real windowobs(ndaymax)   ! observed conc
      real windsimflo(ndaymax)   ! flow associated with sim
      real windobsflo(ndaymax)   ! flow associated with obs
      real windtau(ndaymax)      ! daily critical stress associated with obs
      real minsim,maxsim        ! min and max simulation over the window
      integer winyear(ndaymax)
      integer winmonth(ndaymax)
      integer winday(ndaymax)
      integer winhour(ndaymax)
******************* END DECLARATIONS ***********************************
      do nday = 1,ndays
        ny = obyear(nday)
        nm = obmonth(nday)
        nd = obday(nday)
        nh = obhour(nday)
  
        windtau(nday) = dailytau(ny,nm,nd)
        windobsflo(nday) = obfl(ny,nm,nd)

        do nh2 = 1,window+1  ! go back in time one window-length
          call onehourless(ny,nm,nd,nh)
        end do

        minsim = 99999.0
        maxsim = 0.0
        windsimflo(nday) = 0.
        do nh2 = 1,window*2
          call onehour(ny,nm,nd,nh)
          minsim = min(minsim,simconcdate(ny,nm,nd,nh))
          maxsim = max(maxsim,simconcdate(ny,nm,nd,nh))
          windsimflo(nday) = windsimflo(nday)+simfl(ny,nm,nd,nh)
        end do
        windsimflo(nday) = windsimflo(nday)/real(window*2)*acfthr2cfs
                      ! convert ac-ft/day TO cfs

        windowobs(nday) = obconc(nday)

        if (obLOD(nday)) then
          windowsim(nday) = max(minsim,windowobs(nday))
        else
          if (windowobs(nday).ge.minsim) then
            if (windowobs(nday).le.maxsim) then
              windowsim(nday) = windowobs(nday)
            else
              windowsim(nday) = maxsim
            end if
          else
            windowsim(nday) = minsim
          end if
        end if
         
      end do
      
****************** write out windowed data statistics
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2

      call lencl(rseg,lenrseg)
      call lencl(rscen,lenrscen)
      pfnam = outdir//'river/window/'//rscen(:lenrscen)//'/'
     .         //rseg(:lenrseg)//'_'//cy1//'_'//cy2//'.'//loadnm
      open (pltfil,file = pfnam, status = 'unknown',iostat = err)

      if (err.ne.0) goto 994

      write(pltfil, 300,err=951)loadnm
      write(pltfil,*,err=951)'======================================='

      call wqstats(windowobs,windowsim,ndays,ndaymax,
     .             pltfil,loadnm,rseg,err)

      close(pltfil)

************** write out windowed data set
      winfnam = outdir//'river/window/'//rscen(:lenrscen)//'/'
     .          //rseg(:lenrseg)//'.'//loadnm
      open (pltfil,file = winfnam, status = 'unknown',iostat = err)
      if (err.ne.0) goto 994

      write(pltfil,*,err=951) 'year,month,day,hour,sim_conc,obs_conc,',
     .                  'sim_flow,obs_flow, daily_stress' 
      do nh = 1,ndays
        if (windowsim(nh).gt.1.0E-8.and.windowobs(nh).gt.1.0e-8) then
          write(pltfil,1234,err=951)obyear(nh),obmonth(nh),obday(nh),
     .                   obhour(nh),windowsim(nh),windowobs(nh),
     .                   windsimflo(nh),windobsflo(nh),
     .                   windtau(nh)
        end if
      end do
      close(pltfil)

      return

300   format(3x,'Basic Statistics for',1x,a4,1x,'Conc Window')
1234  format(i4,',',i2,',',i2,',',i2,5(',',e11.4))
  
************************ ERROR REPORTING *******************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

994   report(1) = 'Problem opening output load files for segment '//rseg
      report(2) = pfnam
      report(3) = '  Error =    '
      write(report(3)(11:13),'(i3)') err
      go to 999

995   report(1) = 'Problem writing statistics to file for segment'//rseg
      report(2) = pfnam
      report(3) = '  Error =    '
      write(report(3)(11:13),'(i3)') err
      go to 999

999   call stopreport(report)

      end

