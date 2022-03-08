************************************************************************
**  The purpose of this program is to prepare the plot files for the  **
**    SAS routines for calibration                                    **
**    The Strategy                                                    **
**      1.  get segment and scenario                                  **
**      2.  get info from catalog file                                **
**      3.  read water and store it to find concentrations            **
**      4.  read and write concentrations for each variable           **
************************************************************************
      subroutine maindouble(rscen,conseg,wdmfil)

      implicit none
      include 'Rdaily.inc'

      integer asdate(ndate),aedate(ndate)           ! test dates for checking wdm

      character*64 pfname

      integer wdmfil

      integer i,np,nc,j,Rvar,hour,h         ! indices

      integer Nexits,idummy

      integer maxWATR,nWATRdsns    ! number of DSNs that make up the total flow (multiple exits)
      parameter (maxWATR=4)
      integer WATRdsn(maxWATR)      ! dsns for flow variables

      real flowvals(ndaymax*24)        ! flow
      real loadvals(ndaymax*24)        ! load
      real normvals(ndaymax*24)        ! normalize by:  (usually flow)
      real:: concvals(24)              ! hourly concentration in a day

      real dnorm,dload,dconc    ! daily normalization (flow),phytoplankton, and load accumulators
      real:: daymax, daymin     ! daily MAX and MIN values

      character*4 Tdsn

      logical found,foundall

************* END DECLARATIONS *******************************

      call riverstop(conseg)     ! stop if not a calibration site
      
      print*,'Making daily river concentration files for ', conseg

      call lencl(rscen,lenrscen)
      call lencl(conseg,lenrseg)

      Nexits = 3

      call WDMinfo(rscen,Nexits,nRvar,Rdsn,Rname)                           ! POPULATE nRvar, Rdsn, Rname 
      call plotinfo(rscen,lenrscen,nRvar,Rname,
     O              nplots,plotname,unit,norm,ncons,con,confactor)     ! POPULATE plotting variables
      call readcontrol_time(rscen,lenrscen,sdate,edate)  ! get start and stop

      wdmfnam = outwdmdir//'river/'//rscen(:lenrscen)//
     .            '/stream/'//conseg(:lenrseg)//'.wdm'
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

      do np = 1,nplots
        call ttyput(plotname(np))
        call ttyput(' ')
        do i = 1,nvals                                              ! initialize loadvals
          loadvals(i) = 0.
        end do
        do nc = 1,ncons(np)        ! for each constituent
          do Rvar = 1,nRvar        ! get the right dsn
            if (con(np,nc).eq.Rname(Rvar)) then
              dsn = Rdsn(Rvar)
              call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)        ! get the data
              do i = 1,nvals
                if (hval(i).gt.0.)
     .            loadvals(i) = loadvals(i) + hval(i)*confactor(np,nc)  ! add to temp
              end do
            end if
          end do
        end do

        if (norm(np).eq.'1   ') then      ! no normalization required
          do i = 1,nvals
            normvals(i) = 1.0
          end do
        else if (norm(np).eq.'WATR') then     ! use the flow values already stored
          do i = 1,nvals
            normvals(i) = flowvals(i)
          end do
        else
          go to 997
        end if

*****************plot
        pfname = outdir//'river/daily/'//rscen(:lenrscen)
     .           //'/'//conseg(:lenrseg)//'.'//plotname(np)
        open (pltfil,file = pfname, status = 'unknown',iostat = err)
        if (err.ne.0) goto 992

c	    May want to write a useful header file later JPR
c           call ziplabel(label)
c           label(1) = 'YYYY   (XXXX)            '
c           label(1)(:4) = plotname(np)
c           label(1)(9:12) = unit(np)
      
c           call header(sdate,plotname(np),label)

        do i = 1,6
          asdate(i) = sdate(i)
        end do
  
        hour = 0
        dnorm = 0.
        dload = 0.
        do i = 1,nvals
          hour = hour + 1
          if ( loadvals(i).gt.0.0 .and. normvals(i).gt.0.0) then
          concvals(hour) = loadvals(i)/normvals(i)    ! get hourly concentration
          end if
          dnorm = dnorm + normvals(i)
          dload = dload + loadvals(i)
          if (hour.eq.24) then
            if (dnorm.gt.0.0 .and. dload.gt.0.0) then       ! check to see if allowable value
              if (abs(log10(dnorm)-log10(dload)).lt.9.) then  ! allow the value
                dconc = dload / dnorm
              else
                dconc = 0.
              end if
            else
              dconc = 0.
            end if

**************** find daily max and min value
            daymax = concvals(1)
            daymin = concvals(1)
            do h = 2, 24
              if (concvals(h) .gt. daymax) then
              daymax = concvals(h)
              end if
              if (concvals(h) .lt. daymin) then
              daymin = concvals(h)
              end if
            end do

**************** write daily average, min, and max values into file
            write(pltfil,1234,err=951) 
     .              (asdate(j),j=1,3),dconc,daymin,daymax

            hour = 0
            dnorm = 0.
            dload = 0.
            call tomorrow(asdate(1),asdate(2),asdate(3))
          end if
        end do

        close (pltfil)

      end do            ! end loop over all plots in 'rchres_out_to_daily' file
            
      call wdflc1(wdmfil,err)
      if (err.ne.0) then
        go to 996
      end if

      print*,' '

      return

1234  format(i6,',', 2(i3,','), '24,0,', 2(e14.5,','), e14.5)

************************ error reporting
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem with opening wdm for river segment '//conseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = wdmfnam
      go to 999

992   report(1) = 'Problem opening plot files for segment '//conseg
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
      report(2) = '  for segment '//conseg
      report(3) = wdmfnam
      go to 999

997   report(1) = 'normalization choices are nothing and WATR'
      report(2) = 'need to recode postprocessor'
      report(3) = ' or modify rchres_out_to_daily file'
      go to 999

999   call stopreport(report)

      end

