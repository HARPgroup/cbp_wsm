************************************************************************
**  main calling program for the calculation of statistics            **
************************************************************************
      subroutine single(rscen,obscen,rseg,year1,year2,wdmfil,
     .                  npeaks,Nexits,window)
    
      implicit none
      include 'Rstats.inc'

      integer asdate(ndate),aedate(ndate),ndays   ! test dates for wdm

      integer wdmfil
      integer i,np,nc,j,n,Rvar,hour,oldyear,ny,nm,nd,nh,nmin  ! indices
      integer year1,year2             ! first and last year to average
      integer hour1,hour2             ! first and last hour to average

      integer maxWATR,nWATRdsns    ! number of DSNs that make up the 
                                   ! total flow (multiple exits)
      parameter (maxWATR=4)
      integer WATRdsn(maxWATR)        ! dsns for flow variables

      real flowvals(ndaymax*24)          ! flow
      real inteload(ndaymax*24)          ! load
                
      character*4 Tdsn

      integer Nexits
      integer npeaks                          ! peaks to investigate

*******  regression variables
      real simintld(EarliestYear:LatestYear,12,31,24)
      real dailytau(EarliestYear:LatestYear,12,31)
      real simintload(ndaymax)                          

******* variables for a window of simulated values
      integer ndwind            ! number of observations
      integer winyear(ndaymax)
      integer winmonth(ndaymax)
      integer winday(ndaymax)
      integer winhour(ndaymax)
******************* END DECLARATIONS ***********************************

      write(*,*) 'Making flow statistic files for ', rseg

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      call WDMinfo(
     I             rscen,Nexits,
     O             nRvar,Rdsn,Rname) 
c      write(*,*) 'BHATT single ',rscen
c      write(*,*) 'BHATT single nRvar=',nRvar,' Rname= ',Rname
                             ! POPULATE nRvar, Rdsn, Rname

      call readcontrol_time(rscen,lenrscen,sdate,edate)         
                                            ! get start and stop
      wdmfnam = outwdmdir//'river/'//rscen(:lenrscen)//
     .            '/stream/'//rseg(:lenrseg)//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)       ! open main wdm read/write
      if (err .ne. 0) go to 990

      call wtdate(wdmfil,1,Rdsn(1),2,asdate,aedate,err) ! tests date
      if (err .ne. 0) go to 991

      nWATRdsns = 0
      do Rvar = 1,nRvar         ! find which dsns corresponds to water
        if (Rname(Rvar).eq.'WATR') then
          nWATRdsns = nWATRdsns + 1
          if (nWATRdsns.gt.maxWATR) go to 992
          WATRdsn(nWATRdsns) = Rdsn(Rvar)
        end if
      end do

      call checkflow(wdmfnam,wdmfil,0,maxWATR,nWATRdsns,WATRdsn,
     .                     sdate,edate)   ! make sure that flow exists
 
      call gethourdsn(wdmfil,sdate,edate,WATRdsn(1),nvals,flowvals)
                                      ! get nvals from water variable
      do Rvar = 2,nWATRdsns   ! get other flowvals if >1 water variable
        call gethourdsn(wdmfil,sdate,edate,WATRdsn(Rvar),nvals,hval)
        do i = 1,nvals
          flowvals(i) = flowvals(i) + hval(i)
        end do
      end do
      
      call gethours(sdate,year1,year2,hour1,hour2)
          ! hval(hour1) is first averaged, hval(hour2) is the last
          ! user sets averaging period in the run_postproc.com script

********************Calculate statistics for flow
      call doflows(
     I             rscen,obscen,rseg,year1,year2,nvals,flowvals,
     I             sdate,npeaks,
     O             simfl,obfl,bofl,bsfl,qofl,qsfl,FloObsExist)

******* Calculate stats for concs and instantaneous loads
************   in rchres_out_to_conc vs observed
      call doconcs(
     I             rscen,obscen,rseg,year1,year2,window,
     I             nRvar,Rdsn,Rname,wdmfil,sdate,edate,
     I             simfl,obfl,bofl,bsfl,qofl,qsfl,FloObsExist)

******* Calculate statistics for loads in rchres_out_to_load
*********** vs ESTIMATOR loads
      call doloads(
     I             rscen,rseg,year1,year2,
     I             nRvar,Rdsn,Rname,wdmfil,sdate,edate)

      return
  
************************ error reporting

990   report(1) = 'Problem with opening wdm for river segment '//rseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = wdmfnam
      go to 999

991   report(1) = 'Problem getting dates from open wdm '
      report(2) = wdmfnam
      report(3) = '  Error = '
      write(report(3)(11:13),'(i3)') err
      go to 999

992   report(1) = 'too many variables names WATR in catalog file'
      report(2) = 'increase maxWATR variable in'
      report(3) = './pp/postproc/src/river/*/main.f'
      go to 999

999   call stopreport(report)

      end

