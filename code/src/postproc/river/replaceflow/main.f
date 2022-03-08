************************************************************************
**  The purpose of this program is to prepare the plot files for the  **
**    SAS routines for calibration                                    **
**    The Strategy                                                    **
**      1.  get segment and scenario                                  **
**      2.  get info from catalog file                                **
**      3.  read water and store it to find concentrations            **
**      4.  read and write concentrations for each variable           **
************************************************************************

      implicit none
      include 'Rdaily.inc'

      integer asdate(ndate),aedate(ndate)           ! test dates for checking wdm

      integer wdmfil
      parameter (wdmfil=12)         ! file number for wdm

      character*100 pfname

      integer i,np,nc,j,Rvar,hour, h         ! indices

      integer Nexits,idummy,timestep
      character*1 resflag

      integer maxWATR,nWATRdsns    ! number of DSNs that make up the total flow (multiple exits)
      parameter (maxWATR=4)
      integer WATRdsn(maxWATR)      ! dsns for flow variables

      real flowvals(ndaymax*24)        ! flow
      real loadvals(ndaymax*24)        ! load
      real normvals(ndaymax*24)        ! normalize by:  (usually flow)
      real concvals(24)              ! hourly concentration in a day         

      real dnorm,dload,dconc    ! daily normalization (flow),phytoplankton, and load accumulators
      real daymax, daymin     ! daily MAX and MIN values
      
      character*4 Tdsn
      logical founddouble

      integer dyr,dmm,ddd,dhh,dxx
      integer ivals,rvals
      real dfl, sfl ! daily flow, simulated flow
      real factors(ndaymax*24)

***************** END OF DECLARATIONS **********************************

      read*,rscen,rseg     ! variables supplied by pp/run/run_postproc.com
      call lencl(rscen,lenrscen)

c      call getconseg(rseg,rscen,lenrscen,
c     O               conseg,founddouble)   !look for rseg in doubles.csv file

c      if (founddouble) then
c        call maindouble(rscen, conseg,wdmfil+1)
c      end if

      call riverstop(rseg)     ! stop if not a calibration site
      
      call lencl(rseg,lenrseg)

c      print*,'Daily River Concentration ',rscen(:lenrscen),' ',rseg

      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

      call getrflags(
     I               paramscen,lenparamscen,rseg,
     O               Nexits,idummy,resflag,timestep)

      call WDMinfo(rscen,Nexits,nRvar,Rdsn,Rname)  ! POPULATE nRvar, Rdsn, Rname 
      call plotinfo(rscen,lenrscen,nRvar,Rname,
     O              nplots,plotname,unit,norm,ncons,con,confactor)     ! POPULATE plotting variables

      call readcontrol_time(rscen,lenrscen,sdate,edate)  ! get start and stop

c      wdmfnam = outwdmdir//'river/'//rscen(:lenrscen)//
c     .            '/stream/'//rseg(:lenrseg)//'.wdm'
      wdmfnam = rseg(:lenrseg)//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)     ! open main wdm read/write
      
      if (err .ne. 0) go to 991

      call wtdate(wdmfil,1,Rdsn(1),2,asdate,aedate,err)    ! tests the date

      if (err .ne. 0) go to 994

      nWATRdsns = 0
      do Rvar = 1,nRvar                     ! find which dsns corresponds to water
        if (Rname(Rvar).eq.'WATR') then
          nWATRdsns = nWATRdsns + 1
          if (nWATRdsns.gt.maxWATR) go to 995
          WATRdsn(nWATRdsns) = Rdsn(Rvar)
        end if
      end do

      call checkflow(wdmfnam,wdmfil,0,maxWATR,nWATRdsns,WATRdsn,
     .                     sdate,edate)               ! make sure that flow exists

      call gethourdsn(wdmfil,sdate,edate,WATRdsn(1),nvals,flowvals)         ! get nvals from water variable
      do Rvar = 2,nWATRdsns         ! get other flowvals if more than 1 water variables
        call gethourdsn(wdmfil,sdate,edate,WATRdsn(Rvar),nvals,hval)
        do i = 1,nvals
          flowvals(i) = flowvals(i) + hval(i)
        end do
      end do

      do i = 1,ndaymax*24
         factors(i) = 1.0
      end do
      pfname = rseg(:lenrseg)//'.OFLOW'
      open (pltfil,file = pfname, status = 'old',iostat = err)
      if (err.ne.0) goto 992
      ivals = 0
      do
      !{
111      read (pltfil,*,end=121) dyr,dmm,ddd,dhh,dxx,dfl
         !print*,dyr,dmm,ddd,dhh,dxx,dfl
         if (dyr+(dmm+ddd/31)/12 .lt. 
     .       asdate(1)+(asdate(2)+asdate(3)/31)/12 ) go to 111
         do
            if( asdate(1).eq.dyr .and. asdate(2).eq.dmm .and.
     .          asdate(3).eq.ddd ) then
               rvals = ivals
               sfl = 0
               do i = 1,24
                  sfl = sfl + flowvals(rvals+i)*12.10/24
               end do
               do i = 1,24
                  if (sfl .gt. 0) then
                     factors(rvals+i) = (dfl/sfl)
                     print*,dyr,dmm,ddd,dfl,sfl,factors(rvals+i)
                  end if
               end do
               go to 111
            end if
            ivals = ivals + 24
            call tomorrow(asdate(1),asdate(2),asdate(3))
         end do
      !}
      end do
121   close(pltfil)

      call gethourdsn(wdmfil,sdate,edate,WATRdsn(1),nvals,hval)
      do i = 1,nvals
         hval(i) = hval(i) * factors(i)
      end do
      call puthourdsn(wdmfil,sdate,edate,WATRdsn(1),nvals,hval)
      do Rvar = 2,nWATRdsns         ! get other flowvals if more than 1 water variables
        call gethourdsn(wdmfil,sdate,edate,WATRdsn(Rvar),nvals,hval)
        do i = 1,nvals
           hval(i) = hval(i) * factors(i)
        end do
        call puthourdsn(wdmfil,sdate,edate,WATRdsn(Rvar),nvals,hval)
      end do

      call wdflc1(wdmfil,err)
      if (err.ne.0) then
        go to 996
      end if

      print*,' '

      stop

1234  format(i6,',', 2(i3,','), '24,0,', 2(e14.5,','), e14.5)

************************ error reporting
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem with opening wdm for river segment '//rseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

992   report(1) = 'Problem opening plot files for segment '//rseg
      report(2) = pfname
      report(3) = '  Error =    '
      write(report(3)(11:13),'(i3)') err
      go to 999

994   report(1) = 'Problem getting dates from open wdm '
      report(2) = wdmfnam
      report(3) = '  Error = '
      write(report(3)(11:13),'(i3)') err
      go to 999

995   report(1) = 'too many variables names WATR in catalog file'
      report(2) = 'increase maxWATR variable in'
      report(3) = './pp/postproc/src/river/*/main.f'
      go to 999

996   report(1) = ' Could not close current wdm'
      report(2) = '  for segment '//rseg
      report(3) = wdmfnam
      go to 999

997   report(1) = 'normalization choices are nothing and WATR'
      report(2) = 'need to recode postprocessor'
      report(3) = ' or modify rchres_out_to_daily file'
      go to 999

999   call stopreport(report)

      end

