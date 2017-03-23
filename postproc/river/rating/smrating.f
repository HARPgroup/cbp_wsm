**************************************************************************
** main calling program for developing rating curve for simulated data  **
** Strategy - 1. seperate simulated daily flow into ascending,          **
**               descending, and base flow                              **
**            2. get simulated hourly flow and concentration            **
**            3. put hourly data into different part of hydrograph      **
**            4. develop rating curve for each set                      **
**************************************************************************
     
      subroutine smrating(rscen,rseg,year1,year2,wdmfil,Nexits, pltfil)
    
      implicit none
      include 'Rating.inc'

      integer:: wdmfil,ifl,pltfil
      integer:: asdate(ndate),aedate(ndate)       ! test dates for checking wdm

      character(100):: pfname 

      integer:: i,np,nc,n,m,Rvar        ! indices 
      integer:: ny,nm,nd,nh,nmin                   
      integer:: ny2,nm2,nd2,nh2,nmin2
      integer:: year1,year2             ! first and last year to average
      integer:: hour1,hour2             ! first and last hour to average

      integer:: maxWATR,nWATRdsns       ! number of DSNs that make up the total flow (multiple exits)
      parameter (maxWATR=4)
      integer:: WATRdsn(maxWATR)        ! dsns for flow variables

      real:: flowvals(ndaymax*24)       ! flow
      real:: inteload(ndaymax*24)       ! load
      real:: simintld(EarliestYear:LatestYear,12,31,24)

      character(4):: Tdsn
      real:: hconc
      integer:: Nexits

******************* END DECLARATIONS ***********************************

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      call WDMinfo(rscen,Nexits,nRvar,Rdsn,Rname) 
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
     .                     sdate,edate)     ! make sure that flow exists

      call gethourdsn(wdmfil,sdate,edate,WATRdsn(1),nvals,flowvals)
                                            ! get nvals from water variable
      do Rvar = 2,nWATRdsns                 ! get other flowvals if >1 water variable
        call gethourdsn(wdmfil,sdate,edate,WATRdsn(Rvar),nvals,hval)
        do i = 1,nvals
          flowvals(i) = flowvals(i) + hval(i)
        end do
      end do

      call gethours(sdate,year1,year2,hour1,hour2)
                                ! hval(hour1) is first averaged, hval(hour2) is the last
      
*********** code to re-arrange simulated hourly flow and to get daily
        ny = sdate(1)
        nm = sdate(2)
        nd = sdate(3)
        nh = max(sdate(4),1)

        i = 1
        ndays = 0 

        do while (ny.le.year2)
         simfl(ny,nm,nd,nh) = flowvals(i)      ! unit: ac-ft/hour
          if (nh .eq. 24) then
           ndays = ndays + 1
           simflow(ndays)   = 0.
           simdayfl(ny,nm,nd) = 0.
           do nh = 1,24
            if (simfl(ny,nm,nd,nh).lt.0.1) simfl(ny,nm,nd,nh)=0.1
            simflow(ndays) = simflow(ndays) + simfl(ny,nm,nd,nh)    ! unit: ac-ft/day
            simdayfl(ny,nm,nd) = simdayfl(ny,nm,nd)+ simfl(ny,nm,nd,nh)
           end do
            simflow(ndays) = simflow(ndays)*43560.0/3600.0/24.0     ! convert to cfs
            simdayfl(ny,nm,nd) = simdayfl(ny,nm,nd)*43560.0/3600.0/24.0
            simyear(ndays) = ny
            simmonth(ndays) = nm
            simday(ndays) = nd
          end if
          i = i + 1
          call onehour(ny,nm,nd,nh)
        end do

************** partition quick flow and base flow

        if (ndays.gt.2) then
          nm = 1
          nd = 1
          call readcontrol_Rgeoscen(rscen,lenrscen,
     O                              geoscen)
          call lencl(geoscen,lengeoscen)

          fnam = catdir//'geo/'//geoscen(:lengeoscen)//
     .              '/watershed_area.csv'
            
          call PART(simflow,ndaymax,ndays,fnam,year1,nm,nd,rseg,    ! for simulated flow
     O              bsim,qsim,err)
          
         if (err.ne.0) go to 993
         
          call seperation(qsim,bsim,ndays,                   ! seperate ascending/descending rib for simulated flow
     O                    nasdays,ndsdays,asmatch,dsmatch)
       
        end if                                       

************** get concentration infomation

      call caloadinfo(rscen,lenrscen,nRvar,Rname,
     O              nloads,loadname,unit,ncons,con,confactor)  ! POPULATE loading variables
                                          
      do np = 1,nloads

       if (loadname(np) .ne. 'FLOW') then

       write(*,*) 'Making simulated sediment rating curve for ', rseg
******************** get hourly simulated load value
        do i = 1,nvals
         inteload(i) = 0.                         ! initialize intertiment load value
        end do
      
         do nc = 1,ncons(np)                        ! for each constituent

          do Rvar = 1,nRvar                         ! get the right dsn
            if (con(np,nc).eq.Rname(Rvar)) then
              dsn = Rdsn(Rvar)
              call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)
               do i = 1,nvals
                if (hval(i).gt.0.) then
                inteload(i) = inteload(i) + hval(i)*confactor(np,nc)  ! hourly intettiment load value
                end if
               end do
            end if
          end do

        end do
    
*********** code to re-arrange the simulated load to get hourly
        year1 = sdate(1)
        year2 = edate(1)

        ny = sdate(1)
        nm = sdate(2)
        nd = sdate(3)
        nh = max(sdate(4),1)
        i = 1

       do while (ny.le.year2)
          simintld(ny,nm,nd,nh) = inteload(i)

        if (simfl(ny,nm,nd,nh).gt.0.0 .and. simintld(ny,nm,nd,nh)
     .              .gt. 0.0) then                                       ! check to see if allowable value

         if (abs(log10(simfl(ny,nm,nd,nh))-log10(simintld(ny,nm,nd,nh)))
     .             .lt.9.) then                                          ! allow the value
          hconc = simintld(ny,nm,nd,nh)/simfl(ny,nm,nd,nh)             ! get hourly concentration, mg/L
         else
          hconc = 0.
         end if
       else
          hconc = 0.
       end if

       if (loadname(np) .eq.'CHLA') then                 ! simulated hourly concentration, chla unit: uq/L
          simcon(ny,nm,nd,nh) = hconc*1000.
       else
          simcon(ny,nm,nd,nh) = hconc                         ! other: mg/L
       end if

       i = i + 1
       call onehour(ny,nm,nd,nh)
      end do

************** DEVELOPING RATING CURVE ************************

************ for total flow hydrograph
      npts = 0
      do n =1, ndays
        do nh = 1, 24
          npts = npts + 1
          simallfl(npts) = simdayfl(simyear(n),simmonth(n),simday(n))
          simallcon(npts)=simcon(simyear(n),simmonth(n),simday(n),nh)
        end do
      end do

       call transform(npts,simallfl,simallcon)                     ! get ride of non-positive values
       call regress(simallfl,simallcon,npts,nptsmax,
     O             simslp,simint,simr2,limit,err)
      if (err.ne.0) go to 994
      
************ for ascending rib 
      naspts = 0
      do n =1, ndays              
        do m = 1, nasdays
          if (n .eq. asmatch(m)) then    ! found the matching day
           do nh = 1, 24
           naspts = naspts + 1
           simasfl(naspts)=simdayfl(simyear(n),simmonth(n),simday(n))  
           simascon(naspts)=simcon(simyear(n),simmonth(n),simday(n),nh)
           end do 
         end if
        end do
      end do 
    
      call transform(naspts,simasfl,simascon)                     ! get ride of non-positive values
      call regress(simasfl,simascon,naspts,nptsmax,
     O                simaslp,simasint,simasr2,aslimit,err)
       if (err.ne.0) go to 994
   
**************for dscending rib
      ndspts = 0
      do n =1, ndays      
        do m = 1, ndsdays
         if (n .eq. dsmatch(m)) then    ! found the matching day
           do nh = 1, 24
           ndspts = ndspts + 1
           simdsfl(ndspts)=simdayfl(simyear(n),simmonth(n),simday(n))
           simdscon(ndspts)=simcon(simyear(n),simmonth(n),simday(n),nh)
           end do
         end if
        end do
      end do

      call transform(ndspts,simdsfl,simdscon)                       ! get ride of non-positive values
      call regress(simdsfl,simdscon,ndspts,nptsmax,
     O                simdslp,simdsint,simdsr2,dslimit,err)
       if (err.ne.0) go to 994

*********** for base flow
      nbspts = 0 
      do n =1, ndays               
       if (bsim(n) .ne. -9.0) then  ! found base flow 
           do nh = 1, 24
           nbspts = nbspts + 1
           simbsfl(nbspts)=simdayfl(simyear(n),simmonth(n),simday(n))
           simbscon(nbspts)=simcon(simyear(n),simmonth(n),simday(n),nh)
           end do
         end if
      end do

      call transform(nbspts,simbsfl,simbscon)                         ! get ride of non-positive values
      call regress(simbsfl,simbscon,nbspts,nptsmax,
     O                simbslp,simbsint,simbsr2,bslimit,err)
       if (err.ne.0) go to 994

************** write outputs into a file

       write(pltfil,100,err=951) loadname(np)
       write(pltfil,*,err=951)'========================================'
       write(pltfil,200,err=951) 'total','ascend','descend','base'
       write(pltfil,300,err=951) 'data points',npts,naspts,
     .                                             ndspts,nbspts
       write(pltfil,400,err=951) 'slope',simslp,simaslp,
     .                                       simdslp,simbslp
       write(pltfil,400,err=951) 'intercept',simint,simasint,
     .                                           simdsint,simbsint
       write(pltfil,400,err=951) 'correlation',simr2,simasr2,
     .                                             simdsr2,simbsr2
       write(pltfil,400,err=951) 'interval S1',simslp-limit, 
     .           simaslp-aslimit,simdslp-dslimit,simbslp-bslimit
       write(pltfil,400,err=951) 'interval S2',simslp+limit,
     .           simaslp+aslimit,simdslp+dslimit,simbslp+bslimit
       write(pltfil,*,err=951) ' '
       write(pltfil,*,err=951) ' '
       
      end if                    ! end if for non-flow constituents  
  
      end do   ! end loop over all loads in 'rchres_out_to_daily'

100   format(3x,'regression curve for simulated',1x,a4)
200   format(11x,a10,4(1x,a8))
300   format(1x,a11,4(1x,i8))
400   format(1x,a11,4(1x,f8.3))

      return
  
************************ ERROR REPORTING *******************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

990   report(1) = 'Problem with opening wdm for river segment '//rseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
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

993   report(1) = 'Error in PART program'
      report(2) = 'unspecified error, check file'
      report(3) = './pp/src/postproc/river/part/part_sub.f'
      go to 999

994   report(1) ='Problem making simulated regression for segment'//rseg
      report(2) = pfname
      report(3) = '  Error =    '
      write(report(3)(11:13),'(i3)') err
      go to 999

999   call stopreport(report)

      end

