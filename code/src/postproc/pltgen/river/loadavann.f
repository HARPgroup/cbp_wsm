***********************************************************************
**  main calling program for calculation of average annaul load      **
************************************************************************

      subroutine loadavann(rscen,rseg,year1,year2,wdmfil,Nexits,
     O                 aveload)
      implicit none
      include 'Raveann.inc'

      integer asdate(ndate),aedate(ndate)    ! test dates for checking wdm

      integer wdmfil

      integer i,np,nc,Rvar            ! indices

      integer year1,year2             ! first and last year to average
      integer hour1,hour2             ! first and last hour to average

      integer maxWATR,nWATRdsns       ! number of DSNs that make up the total flow (multiple exits)
      parameter (maxWATR=4)
      integer WATRdsn(maxWATR)        ! dsns for flow variables

      real flowvals(ndaymax*24)       ! flow
      real loadvals(ndaymax*24)       ! load
  
      real:: aveload
   
      integer Nexits

      logical found,foundall

******************* END DECLARATIONS ***********************************

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      call WDMinfo(rscen,Nexits,nRvar,Rdsn,Rname) 
                                                   ! POPULATE nRvar, Rdsn, Rname
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

      call loadinfo(nRvar,Rname,
     O              nloads,loadname,unit,ncons,con,confactor)   
                                                   ! POPULATE loading variables
      call readcontrol_time(rscen,lenrscen,sdate,edate)         
                                                   ! get start and stop

      wdmfnam = outwdmdir//'river/'//rscen(:lenrscen)//
     .            '/stream/riv'//rseg(:lenrseg)//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)            ! open main wdm read/write
      if (err .ne. 0) go to 991

      call wtdate(wdmfil,1,Rdsn(1),2,asdate,aedate,err) ! tests date

      if (err .ne. 0) go to 992

      nWATRdsns = 0
      do Rvar = 1,nRvar                            ! find which dsns corresponds to water
        if (Rname(Rvar).eq.'WATR') then
          nWATRdsns = nWATRdsns + 1
          if (nWATRdsns.gt.maxWATR) go to 993
          WATRdsn(nWATRdsns) = Rdsn(Rvar)
        end if
      end do

      call checkflow(wdmfnam,wdmfil,0,maxWATR,nWATRdsns,WATRdsn,
     .                     sdate,edate)            ! make sure that flow exists

      call gethourdsn(wdmfil,sdate,edate,WATRdsn(1),nvals,flowvals)
                                                   ! get nvals from water variable
      do Rvar = 2,nWATRdsns                        ! get other flowvals if >1 water variable
        call gethourdsn(wdmfil,sdate,edate,WATRdsn(Rvar),nvals,hval)
        do i = 1,nvals
          flowvals(i) = flowvals(i) + hval(i)
        end do
      end do

      call gethours(sdate,year1,year2,hour1,hour2)
                                                   ! hval(hour1) is first averaged, hval(hour2) is the last

      do np = 1,nloads

        if (loadname(np).eq. 'TSED') then  ! sediment only

        do i = 1,nvals                   ! initialize loadvals
          loadvals(i) = 0.
        end do
      
        foundall = .true.
        do nc = 1,ncons(np)              ! for each constituent
          found = .false.                ! found this constituent
          do Rvar = 1,nRvar              ! get the right dsn
            if (con(np,nc).eq.Rname(Rvar)) then
              found = .true.
              dsn = Rdsn(Rvar)
              call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)
              do i = 1,nvals
                if (hval(i).gt.0.)
     .            loadvals(i) = loadvals(i) + hval(i)*confactor(np,nc) 
              end do
            end if
          end do
          if (.not.found) foundall = .false.
        end do

******************** got the loadvals, now make the average

        if (foundall) then
          aveload = 0. 
          do i = hour1,hour2
            aveload = aveload + loadvals(i)
          end do
            aveload = aveload / real(year2-year1+1)
        end if
       
       end if    ! end if for sediment

      end do     ! end loop over all loads in 'rchres_out_to_daily'

      return

************************ error reporting

991   report(1) = 'Problem with opening wdm for river segment '//rseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

992   report(1) = 'Problem getting dates from open wdm '
      report(2) = wdmfnam
      report(3) = '  Error = '
      write(report(3)(11:13),'(i3)') err
      go to 999

993   report(1) = 'too many variables names WATR in catalog file'
      report(2) = 'increase maxWATR variable in'
      report(3) = './pp/postproc/src/river/*/main.f'
      go to 999

999   call stopreport(report)

      end 

