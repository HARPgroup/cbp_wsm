************************************************************************
**  main calling program for the calculation of statistics            **
************************************************************************
      subroutine doconcs(
     I                   rscen,obscen,rseg,year1,year2,window,
     I                   nRvar,Rdsn,Rname,wdmfil,sdate,edate,
     I                   simfl,obfl,bofl,bsfl,qofl,qsfl,FloObsExist)
      implicit none
      include 'Rstats.inc'

      integer ndays,ifl

      character(100) pfname,obldfnam 
      character(100) winfnam, lodfnam
      character(4) cy1,cy2

      integer i,ny,nm,nd,nh,nmin,np  ! indices
      integer ny2,nm2,nd2,nh2,nmin2,ntime,nc,Rvar,nday

      integer wdmfil

      integer year1,year2             ! first and last year to average
      real tflo, tconc, dconc            ! daily flow and concentration
      real dsum                       

      character*1 QWflag  ! for reading data, test for LOD
 
      real load(ndaymax*24)  ! wdm accumulation variable

********  shear stress for use in windowed comparison.
      real dailytau(EarliestYear:LatestYear,12,31)
      logical gottau

************ date-indexed concentrations and loads
      real simloaddate(EarliestYear:LatestYear,12,31,24)
      real simconcdate(EarliestYear:LatestYear,12,31,24)

************ variables to send into statistics routines
      real obload(ndaymax),simload(ndaymax) ! daily load
      real obconc(ndaymax),simconc(ndaymax) ! daily concentration
    
********** conversion from mg/l*cfs to lb/day
      real mglcfs2lbday
      parameter (mglcfs2lbday = 5.39375)

*********** conversion from lb/acft to mg/l
******** this factor is inherent in rchres_out_to_concs, so 
********  the simulated loads are divided by this factor to get to lbs
      real loadfactor
      parameter (loadfactor = 0.3677)

*********** variables for separating CFD into Bins and calculating KS
      integer nbins,nbinsmax,nb
      parameter (nbinsmax=5)
      real lowerbound(nbinsmax),upperbound(nbinsmax)
      real simBinAve(nbinsmax),obsBinAve(nbinsmax) ! ave for bin
      real ksKstat  ! KS statistic 
      real nse ! nash-sutcliff efficiency
      integer cfdnobs ! number of observations used in cfd

      integer minobs  ! minimum observations for calculating stats
      parameter (minobs = 10)

      real MINFLOW
      parameter ( MINFLOW = 0.1 ) ! 0.1 cfs = 0.198347 ac-ft/d

******************* END DECLARATIONS ***********************************
      write(*,*) 'Making concentration statistic file for ', rseg

      call lencl(rscen,lenrscen)
      call concinfo(               ! POPULATE concentration variables
     I              rscen,lenrscen,nRvar,Rname,
     O              nconcs,concname,cunit,norm,nccons,ccon,cconfactor)
c      write(*,*) 'BHATT nconcs ', nconcs, concname

      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2

      gottau = .false.
******************** loop over all pollutants
c      write(*,*) 'BHATT Begin loop. nconcs= ',nconcs
      do np = 1,nconcs

        if (concname(np).eq.'FLOW') cycle   ! flow handled seperately
        if (norm(np).ne.'WATR') go to 996
        call ttyput(concname(np))
        call ttyput(' ')

************ GET SIMULATED
******************** get hourly simulated load value
        do i = 1,ndaymax*24
          load(i) = 0.      ! initialize load value
        end do

        do nc = 1,nccons(np)     ! for each constituent

          do Rvar = 1,nRvar       ! get the right dsn
            if (ccon(np,nc).eq.Rname(Rvar)) then
              dsn = Rdsn(Rvar)
              call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)

              do i = 1,nvals
                if (hval(i).gt.0.) then
                  load(i)=load(i)+hval(i)*cconfactor(np,nc)
                end if
              end do
            end if
          end do

        end do

************ GET OBSERVED
*************** check if observed file exist
c        call lencl(rscen,lenrscen)
        call lencl(rseg,lenrseg)
        call lencl(obscen,lenobscen)

        obldfnam=calibdir//'observed/'//obscen(:lenobscen)//'/'//
     .           concname(np)//'/'//rseg(:lenrseg)//'.O'//concname(np)
c        write(*,*) 'BHATT A',obldfnam
       
        call findopen(ifl)
        open(ifl,file=obldfnam,status='old',iostat=err)
     
        ndays = 0
************* read observed concentration and calculate daily load 
        if (err.eq.0) then !  observation file exists
c          write(*,*) 'BHATT B',obldfnam
          tconc = 0 ! bhatt
          do
            read(ifl,*,err=992,end=333) ny,nm,nd,nh,nmin,tconc,QWflag
            if (ny.lt.year1 .or. ny.gt.year2) cycle  
  
            if (nmin.gt.30) call onehour(ny,nm,nd,nh)  ! round 
            if (nh.eq.24) then
              nh = 0
              call tomorrow(ny,nm,nd)
            end if

            print*, ndays, ',', tconc
            ndays = ndays + 1
            obconc(ndays) = tconc
            
            if (FloObsExist) then
              obload(ndays) = tconc*obfl(ny,nm,nd)*mglcfs2lbday
            end if
            obyear(ndays) = ny
            obmonth(ndays) = nm
            obday(ndays) = nd
            obhour(ndays) = nh
            obLOD(ndays) = .false.
            if (QWflag.eq.'<') obLOD(ndays) = .true.
          end do
333       close(ifl)           ! close observed load file
        end if

************ if 10 or fewer observed values, calc cfd anyway then cycle
        if (ndays.lt.minobs) then 

          ny = sdate(1)  ! get daily simulated load
          nm = sdate(2)
          nd = sdate(3)
          nh = max(sdate(4),1)
          i = 1
          do while (ny.le.year2)
            simloaddate(ny,nm,nd,nh) = load(i)/loadfactor  ! lb/hr
            i = i + 1
            call onehour(ny,nm,nd,nh)
          end do

          ny = sdate(1)  ! get daily simulated flow and conc
          nm = sdate(2)
          nd = sdate(3)
          ndays = 1
          do while (ny.le.year2)
            simflow(ndays) = 0.0
            simload(ndays) = 0.0
            do nh = 1,24
              simflow(ndays)=simflow(ndays)+simfl(ny,nm,nd,nh) ! ac-ft
              simload(ndays)=simload(ndays)+simloaddate(ny,nm,nd,nh)! lb
            end do
  
                      ! check to see if allowable value
            if(simflow(ndays).gt.MINFLOW.and.simload(ndays).gt.0.0)then
              if (abs(log10(simflow(ndays))
     .               -log10(simload(ndays))).lt.9.) then  
                simconc(ndays)=simload(ndays)/simflow(ndays)*loadfactor
              else
                simconc(ndays) = -9.0
              end if
            else
              simconc(ndays) = -9.0
            end if
               
            ndays = ndays + 1
            call tomorrow(ny,nm,nd)
          end do
          ndays = ndays - 1

          nbins = 5
          call noObVodka(
     I                   simconc,obconc,ndaymax,ndays,nbinsmax,nbins,
     O                   simBinAve,obsBinAve,ksKstat,nse)

          pfname = outdir//'river/cfd/'//rscen(:lenrscen)//'/'
     .             //rseg(:lenrseg)//'_'//cy1//'_'//cy2//'.'//
     .             concname(np)
          open (pltfil,file = pfname, status = 'unknown',iostat = err)
          if (err.ne.0) goto 994
          write(pltfil,*,err=951) ksKstat,', KS statistic'
          write(pltfil,*) nbins,', number of bins'
          write(pltfil,*) 'bin#, Simulated ave, Observed ave'
          do nb = 1,nbins
            write(pltfil,1234)nb,simBinAve(nb),obsBinAve(nb)
          end do
          write(pltfil,*) '0, number of observations'
          close(pltfil)

          cycle  ! go to next conc
        end if

*********** code to re-arrange the load to get hourly simulated
        ny = sdate(1)
        nm = sdate(2)
        nd = sdate(3)
        nh = max(sdate(4),1)
        i = 1
        do while (ny.le.year2)
          simloaddate(ny,nm,nd,nh) = load(i)/loadfactor  ! lb/hr
          if (simfl(ny,nm,nd,nh).gt.0.01) then
            simconcdate(ny,nm,nd,nh) = load(i)/simfl(ny,nm,nd,nh)! mg/l
          else
            simconcdate(ny,nm,nd,nh) = -9.0
          end if
          i = i + 1
          call onehour(ny,nm,nd,nh)
        end do

********** match simulated with observed in straight-line variables
************** for daily averages
        do nday = 1,ndays
          ny = obyear(nday)
          nm = obmonth(nday)
          nd = obday(nday)
          simflow(nday) = 0.0
          simload(nday) = 0.0
          do nh = 1,24
            simflow(nday)=simflow(nday)+simfl(ny,nm,nd,nh) ! ac-ft
            simload(nday)=simload(nday)+simloaddate(ny,nm,nd,nh) ! lb
          end do

                      ! check to see if allowable value
          if (simflow(nday).gt.MINFLOW.and.simload(nday).gt.0.0) then  
            if (abs(log10(simflow(nday))
     .             -log10(simload(nday))).lt.9.) then  
              simconc(nday) = simload(nday)/simflow(nday)*loadfactor
            else
              simconc(nday) = -9.0
            end if
          else
            simconc(nday) = -9.0
          end if
             
        end do

        pfname = outdir//'river/stats/'//rscen(:lenrscen)//'/'
     .           //rseg(:lenrseg)//'_'//cy1//'_'//cy2//'_conc.'//
     .           concname(np)
        open (pltfil,file = pfname, status = 'unknown',iostat = err)

        if (err.ne.0) goto 994

        write(pltfil, 100)concname(np)
        write(pltfil,*)'=========================================='

        call wqstats(obconc,simconc,ndays,ndaymax,
     .               pltfil,concname(np),rseg,err)
        if (err .ne. 0) go to 995

********** write model efficiency of average mothly conc into same file
        call avmoneff(obconc,simconc,ndays,ndaymax,obmonth,           
     .                pltfil,concname(np),rseg,err)
        if (err .ne. 0) go to 995
        close (pltfil)

        if (FloObsExist) then
          pfname = outdir//'river/stats/'//rscen(:lenrscen)//'/'
     .             //rseg(:lenrseg)//'_'//cy1//'_'//cy2//'_load.'//
     .             concname(np)
          open (pltfil,file = pfname, status = 'unknown',iostat = err)

          if (err.ne.0) goto 994

          write(pltfil, 200)concname(np)
          write(pltfil,*)'=========================================='

          call wqstats(obload,simload,ndays,ndaymax,
     .                 pltfil,concname(np),rseg,err)
          if (err .ne. 0) go to 995
      
          close (pltfil)
        end if

************ Calculate statistics for 'windowed' data
        if (window .ge. 0) then        ! if do 'windowed' data

************** get daily tau value
          if (.not.gottau) then
            call readtau(rscen,rseg,year1,year2,
     O                   dailytau)
            gottau = .true.
          end if

          call windowstats(rscen,rseg,year1,year2,concname(np),
     I                     simfl,obfl,simconcdate,obconc,
     I                     obyear,obmonth,obday,obhour,obLOD,
     I                     ndays,window,dailytau)
        end if


***************** Calculate monthly biases for temperature
        if (concname(np).eq.'WTMP') then
          call monthlybias(rscen,rseg,year1,year2,concname(np),
     I                     obconc,obyear,obmonth,obday,obhour,ndays,
     I                     simconc,bofl,bsfl,qofl,qsfl)
        end if

**************** check for bins
        nbins = 5
        do nb=1,nbins
           upperbound(nb) = real(nb)/real(nbins)
           lowerbound(nb) = real(nb-1)/real(nbins)
        end do
        call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
        call lencl(ioscen,lenioscen)
        fnam = catdir//'iovars/'//ioscen(:lenioscen)//
     .         '/binbound_'//concname(np)
        print*,fnam
        open (dfile,file=fnam,status='old',iostat = err)
        if (err.eq.0) then
          read(dfile,*)nbins
          do nb=1,nbins
            read (dfile,*) lowerbound(nb),upperbound(nb)
          end do
        end if
        close(dfile)

*************** calculate KS statistic and cfd bin averages
        if (ndays.ge.minobs) then  ! only if enough obs
c          nbins = 5
c          call vodkatest(
c     I                   simconc,obconc,obLOD,
c     I                   ndaymax,ndays,nbinsmax,nbins,
c     O                   simBinAve,obsBinAve,ksKstat,nse,cfdnobs)
          call vodkatest_bins(
     I                   simconc,obconc,obLOD,
     I                   ndaymax,ndays,nbinsmax,nbins,
     I                   lowerbound,upperbound,
     O                   simBinAve,obsBinAve,ksKstat,nse,cfdnobs)

          pfname = outdir//'river/cfd/'//rscen(:lenrscen)//'/'
     .             //rseg(:lenrseg)//'_'//cy1//'_'//cy2//'.'//
     .             concname(np)
          open (pltfil,file = pfname, status = 'unknown',iostat = err)
          if (err.ne.0) goto 994
          write(pltfil,*) ksKstat,', KS statistic'
          write(pltfil,*) nbins,', number of bins'
          write(pltfil,*) 'bin#, Simulated ave, Observed ave, Bias'
          do nb = 1,nbins
            write(pltfil,1234)nb,simBinAve(nb),obsBinAve(nb),
     .                        simBinAve(nb)-obsBinAve(nb)
          end do
          write(pltfil,*),cfdnobs,', number of observations'
          write(pltfil,*),nse,', efficiency'
          close(pltfil)

        end if   ! end if enough data for cfd calc


      end do      ! end loop over all loads in 'rchres_out_to_conc'
      print*,''


100   format(3x,'Basic Statistics for',1x,a4,1x,'Concentration')
200   format(3x,'Basic Statistics for',1x,a4,1x,'Load')
1234  format(i4,3(',',e14.6))

      return
  
************************ error reporting
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

992   report(1) = 'problem reading observed data file: near line:'
      report(2) = obldfnam
      write(report(3),*) ny,nm,nd,nh,nmin,tconc,QWflag
      go to 999

994   report(1) = 'Problem opening output load files for segment '//rseg
      report(2) = pfname
      report(3) = '  Error =    '
      write(report(3)(11:13),'(i3)') err
      go to 999

995   report(1) = 'Problem writing statistics to file for segment'//rseg
      report(2) = pfname
      report(3) = '  Error =    '
      write(report(3)(11:13),'(i3)') err
      go to 999

996   report(1)='Problem with calculating concentrations: need more'
      report(2)=' coding in pp/src/postproc/river/stats to deal with'
      report(3)=' normalizations other than WATR in rchres_out_to_conc'
      go to 999

999   call stopreport(report)

      end

