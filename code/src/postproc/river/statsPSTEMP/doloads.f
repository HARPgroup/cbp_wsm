************************************************************************
**  compare loads against WRTDS loads                                 **
**  compare loads against !estimator loads                            **
************************************************************************
      subroutine doloads(
     I                   rscen,rseg,year1,year2,
     I                   nRvar,Rdsn,Rname,wdmfil,sdate,edate)
      implicit none
      include 'Rstats.inc'

      integer ndays,ifl

      character(100) pfname,obldfnam 
      character(4) cy1,cy2

      integer i,ny,nm,nd,nh  ! indices
      integer nl,nc,Rvar

      integer year1,year2             ! first and last year to average

      integer wdmfil  ! wdm file number

********  simulated daily, annual, and monthly load
      real simld(EarliestYear:LatestYear,12,31)
      real simannld(EarliestYear:LatestYear)
      real simmonld(EarliestYear:LatestYear,12)
      real load(ndaymax*24)

      real simdayq(EarliestYear:LatestYear,12,31)
      real simannq(EarliestYear:LatestYear)
      real simmonq(EarliestYear:LatestYear,12)
      real simq(ndaymax)
                    
********  observed annual and monthly loads and confidence intervals
      real obsannld(EarliestYear:LatestYear)
      real obsmonld(EarliestYear:LatestYear,12)
      real upannCI(EarliestYear:LatestYear)
      real loannCI(EarliestYear:LatestYear) ! confidence intervals
      real upmonCI(EarliestYear:LatestYear,12)
      real lomonCI(EarliestYear:LatestYear,12) 
      real tld,tuci,tlci

      real obs(ndaymax),sim(ndaymax)  ! variables to send to loadstats
      real NSE,Bias,RSR,Nbias,Nsd,r,sdd,NRMSD
      real kg2lbs  ! convert from kg to lbs
      parameter (kg2lbs=2.2046)

      logical annexist,monexist  ! do observed exist

      integer ndaysinmonth
      external ndaysinmonth

      character*1 cdum

      logical foundany

******************* END DECLARATIONS ***********************************
      foundany = .false.

      call loadinfo(               ! POPULATE loading variables
     I              rscen,lenrscen,nRvar,Rname,
     O              nloads,loadname,lunit,nlcons,lcon,lconfactor)

******************** loop over all pollutants
      do nl = 1,nloads

        if (loadname(nl).eq.'FLOW') then
           do i = 1,nvals
             load(i) = 0.      ! initialize load value
           end do

           do nc = 1,nlcons(nl)     ! for each constituent

             do Rvar = 1,nRvar       ! get the right dsn
               if (lcon(nl,nc).eq.Rname(Rvar)) then
                 dsn = Rdsn(Rvar)
                 call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)

                 do i = 1,nvals   ! hourly load,lbs/hr (tons/hr for sed)
                   if (hval(i).gt.0.) then
                     load(i)=load(i)+hval(i)*lconfactor(nl,nc)
                   end if
                 end do
               end if
             end do

           end do

*********** code to re-arrange the load to get daily
           ny = sdate(1)
           nm = sdate(2)
           nd = sdate(3)
           i = 1
           do while (ny.le.year2)
             simdayq(ny,nm,nd) = 0.0
             do nh = 1,24
               simdayq(ny,nm,nd) = simdayq(ny,nm,nd) + load(i)
               i = i + 1
             end do
             call tomorrow(ny,nm,nd)
           end do

********** get monthly from daily
           do ny = sdate(1),year2
             do nm = 1,12
              simmonq(ny,nm) = 0.0
              do nd = 1,ndaysinmonth(ny,nm)
               simmonq(ny,nm) = simmonq(ny,nm) + simdayq(ny,nm,nd)
              end do
             end do
           end do

********** get annual from monthly
           do ny = sdate(1),year2
             simannq(ny) = 0.0
             do nm = 1,12
               simannq(ny) = simannq(ny) + simmonq(ny,nm)
             end do
           end do
           cycle   ! flow handled seperately
        end if

*************** check if annual estimator file exist
        call lencl(rseg,lenrseg)

        annexist = .false.
CBHATT        obldfnam=calibdir//'observed/estimator/annual/'
        obldfnam=calibdir//'observed/WRTDS/annual/'
     .           //rseg(:lenrseg)//'_'//loadname(nl)//'_CLD.csv'
        call findopen(ifl)
        open(ifl,file=obldfnam,status='old',iostat=err)
        if (err .eq. 0) then ! if no file exist, try monthly load
          annexist = .true.
     
          do ny = EarliestYear,LatestYear
            obsannld(ny) = -9.0
          end do
 
          read(ifl,*) cdum 
          do
            read(ifl,*,end=333) ny,tld,tlci,tuci
            if (ny.lt.year1 .or. ny.gt.year2) cycle
            obsannld(ny) = tld * kg2lbs
            loannCI(ny) = tlci * kg2lbs
            upannCI(ny) = tuci * kg2lbs
          end do
333       close(ifl)           ! close observed load file
        end if

*************** check if monthly estimator file exist
        monexist = .false.
CBHATT        obldfnam=calibdir//'observed/estimator/monthly/'
        obldfnam=calibdir//'observed/WRTDS/monthly/'
     .           //rseg(:lenrseg)//'_'//loadname(nl)//'_MLD.csv'
        call findopen(ifl)
        open(ifl,file=obldfnam,status='old',iostat=err)
        if (err .eq. 0) then ! if no file exist, try next load
          monexist = .true.
     
          do ny = EarliestYear,LatestYear
            do nm = 1,12
              obsmonld(ny,nm) = -9.0
            end do
          end do
 
          read(ifl,*) cdum 
          do
            read(ifl,*,end=444) ny,nm,tld,tlci,tuci
            if (ny.lt.year1 .or. ny.gt.year2) cycle
            obsmonld(ny,nm) = tld * kg2lbs
            lomonCI(ny,nm) = tlci * kg2lbs
            upmonCI(ny,nm) = tuci * kg2lbs
          end do
444       close(ifl)           ! close observed load file
        end if

        if (.not.monexist.and..not.annexist) cycle ! no obs, ignore load


************ announce this load
        if (.not.foundany) then
          write(*,*) 'Making load statistic file for ', rseg
          foundany = .true.
        end if

        call ttyput(loadname(nl))
        call ttyput(' ')

************* get simulated
******************** get hourly load value
        do i = 1,nvals
          load(i) = 0.      ! initialize load value
        end do

        do nc = 1,nlcons(nl)     ! for each constituent

          do Rvar = 1,nRvar       ! get the right dsn
            if (lcon(nl,nc).eq.Rname(Rvar)) then
              dsn = Rdsn(Rvar)
              call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)

              do i = 1,nvals   ! hourly load,lbs/hr (tons/hr for sed)
                if (hval(i).gt.0.) then
                  load(i)=load(i)+hval(i)*lconfactor(nl,nc)
                end if
              end do
            end if
          end do

        end do

*********** code to re-arrange the load to get daily
        ny = sdate(1)
        nm = sdate(2)
        nd = sdate(3)
        i = 1
        do while (ny.le.year2)
          simld(ny,nm,nd) = 0.0
          do nh = 1,24
            simld(ny,nm,nd) = simld(ny,nm,nd) + load(i)
            i = i + 1
          end do
          call tomorrow(ny,nm,nd)
        end do

********** get monthly from daily
        do ny = sdate(1),year2
          do nm = 1,12
            simmonld(ny,nm) = 0.0
            do nd = 1,ndaysinmonth(ny,nm)
              simmonld(ny,nm) = simmonld(ny,nm) + simld(ny,nm,nd)
            end do
          end do
        end do

********** get annual from monthly
        do ny = sdate(1),year2
          simannld(ny) = 0.0
          do nm = 1,12
            simannld(ny) = simannld(ny) + simmonld(ny,nm)
          end do
        end do


*********** got all simulated
****************** write out load stats
        write(cy1,'(i4)') year1
        write(cy2,'(i4)') year2
        call lencl(rscen,lenrscen)

        if (annexist) then
          pfname = outdir//'river/stats/'//rscen(:lenrscen)//'/'
     .           //rseg(:lenrseg)//'_'//cy1//'_'//cy2//
     .           '_annual_vs_wrtds.'//loadname(nl)
          open (pltfil,file = pfname, status = 'unknown',iostat = err)
          if (err.ne.0) goto 994

          write(pltfil,100,err=951)loadname(nl)
          write(pltfil,*,err=951)'====================================='

          ndays = 0
          do ny = year1,year2
            if (obsannld(ny).gt.-9.0) then
              ndays = ndays + 1
              obs(ndays) = obsannld(ny)
              sim(ndays) = simannld(ny)
              simq(ndays)= simannq(ny)
            end if
          end do
          print*,'Annual-'
          call wqstatsld(
     I       obs,sim,simq,ndays,ndaymax,pltfil,loadname(nl),rseg,err)
          if (err .ne. 0) print*,'995 A'
          if (err .ne. 0) go to 995

          close (pltfil)

        end if

********Annual evaluation statistics****************
        if (annexist) then
          pfname = outdir//'river/stats/'//rscen(:lenrscen)//'/'
     .           //rseg(:lenrseg)//'_'//cy1//'_'//cy2//
     .           '_annual_evaluation.'//loadname(nl)
          open (pltfil,file = pfname, status = 'unknown',iostat = err)
          if (err.ne.0) goto 994
          write(pltfil,300)'Time step','Rseg','n','NSE','BIAS','RSR',
     .           'NBIAS','NSTDEV','r','SIGN','NRMSD','err'

          ndays = 0
          do ny = year1,year2
c            print*,ny,obsannld(ny)
            if (obsannld(ny).gt.-9.0) then
              ndays = ndays + 1
              obs(ndays) = obsannld(ny)
              sim(ndays) = simannld(ny)
            end if
          end do

          call eval(
     I             obs,sim,ndays,ndaymax,
     O          NSE,Bias,RSR,Nbias,Nsd,r,sdd,NRMSD,err)
          if (err .ne. 0) print*,'995 B ',loadname(nl)
          if (err .ne. 0) go to 995
          write(pltfil,400)rseg,ndays,NSE,Bias,RSR,
     .           Nbias,Nsd,r,sdd,NRMSD,err
          close (pltfil)

        end if

        if (monexist) then
          pfname = outdir//'river/stats/'//rscen(:lenrscen)//'/'
     .           //rseg(:lenrseg)//'_'//cy1//'_'//cy2//
     .           '_monthly_vs_wrtds.'//loadname(nl)
          open (pltfil,file = pfname, status = 'unknown',iostat = err)
          if (err.ne.0) goto 994

          write(pltfil,200,err=951)loadname(nl)
          write(pltfil,*,err=951)'====================================='

          ndays = 0
          do ny = year1,year2
            do nm = 1,12
              if (obsmonld(ny,nm).gt.-9.0) then
                ndays = ndays + 1
                obs(ndays) = obsmonld(ny,nm)
                sim(ndays) = simmonld(ny,nm)
                simq(ndays)= simmonq(ny,nm)
              end if
            end do
          end do
          print*,'Monthly-'
          call wqstatsld(
     I       obs,sim,simq,ndays,ndaymax,pltfil,loadname(nl),rseg,err)
          if (err .ne. 0) print*,'995 C'
          if (err .ne. 0) go to 995

          close (pltfil)

        end if 

********Monthly evaluation statistics****************
        if (annexist) then
          pfname = outdir//'river/stats/'//rscen(:lenrscen)//'/'
     .           //rseg(:lenrseg)//'_'//cy1//'_'//cy2//
     .           '_monthly_evaluation.'//loadname(nl)
          open (pltfil,file = pfname, status = 'unknown',iostat = err)
          if (err.ne.0) goto 994
          write(pltfil,300)'Time step','Rseg','n','NSE','BIAS','RSR',
     .           'NBIAS','NSTDEV','r','SIGN','NRMSD','err'

          ndays = 0
          do ny = year1,year2
            do nm = 1,12
              if (obsmonld(ny,nm).gt.-9.0) then
                ndays = ndays + 1
                obs(ndays) = obsmonld(ny,nm)
                sim(ndays) = simmonld(ny,nm)
              end if
            end do
          end do

          call eval(
     I             obs,sim,ndays,ndaymax,
     O          NSE,Bias,RSR,Nbias,Nsd,r,sdd,NRMSD,err)
          if (err .ne. 0) print*,'995 D'
          if (err .ne. 0) go to 995
          write(pltfil,500)rseg,ndays,NSE,Bias,RSR,
     .           Nbias,Nsd,r,sdd,NRMSD,err
          close (pltfil)

        end if

      end do      ! end loop over all loads in 'rchres_out_to_load'

      print*,' '

100   format(3x,'Annual Statistics for',1x,a4,1x,'Load')
200   format(3x,'Monthly Statistics for',1x,a4,1x,'Load')
300   format(A9,',',A4,',',A1,',',A3,',',A4,',',A3,',',A5,',',A6,','
     .       ,A1,',',A4,',',A5,',',A3)
400   format('Annual',',',A13,',',i4,8(',',e14.7),',',i1)
500   format('Monthly',',',A13,',',i4,8(',',e14.7),',',i1)

      return
  
************************ error reporting
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

994   report(1) = 'Problem opening output load files for segment '//rseg
      report(2) = pfname
      report(3) = '  Error =    '
      write(report(3)(11:13),'(i3)') err
      go to 999

995   report(1) ='Problem writing statistics to file for segment '//rseg
      write(report(2),*) pfname
      report(3) = '  Error =    '
      write(report(3)(11:13),'(i3)') err
      go to 999

999   call stopreport(report)

      end

