************************************************************************
**  main calling program for the calculation of statistics            **
************************************************************************
      subroutine doconcs(
     I                   obscen,rseg,year1,year2,
     I                   obfl,bofl,qofl,
     O                   tobflo,tobdays)
      implicit none
      include 'Rstats.inc'

      integer ndays,ifl,nLOD
      real tobflo(ndaymax)
      integer tobdays

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
      real dailytau(1984:2005,12,31)
      logical gottau

************ variables to send into statistics routines
      real obload(ndaymax) ! daily load
      real obconc(ndaymax) ! daily concentration
    
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
      real afbin(nbinsmax) ! ave for bin
      real ofbin(nbinsmax) ! ave for bin
      real ksKstat  ! KS statistic 
      real nse ! nash-sutcliff efficiency
      integer cfdnobs ! number of observations used in cfd

      integer minobs  ! minimum observations for calculating stats
      parameter (minobs = 10)


******************* END DECLARATIONS ***********************************
      nbins = nbinsmax
      print*, 'Making concentration statistic file for ', rseg
      nconcs = 3
      concname(1) = 'TOTN'
      concname(2) = 'TOTP'
      concname(3) = 'TSSX'

      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2

******************** loop over all pollutants
      do np = 1,nconcs

        if (concname(np).eq.'FLOW') cycle   ! flow handled seperately

************ GET OBSERVED
*************** check if observed file exist
        call lencl(rseg,lenrseg)
        call lencl(obscen,lenobscen)

        obldfnam=calibdir//'observed/'//obscen(:lenobscen)//'/'//
     .           concname(np)//'/'//rseg(:lenrseg)//'.O'//concname(np)
       
        call findopen(ifl)
        open(ifl,file=obldfnam,status='old',iostat=err)
     
        ndays = 0
        nLOD = 0
************* read observed concentration and calculate daily load 
        if (err.eq.0) then !  observation file exists
          do
            read(ifl,*,err=992,end=333) ny,nm,nd,nh,nmin,tconc,QWflag
            if (ny.lt.year1 .or. ny.gt.year2) cycle  
  
            if (nmin.gt.30) call onehour(ny,nm,nd,nh)  ! round 
            if (nh.eq.24) then
              nh = 0
              call tomorrow(ny,nm,nd)
            end if

            if (obfl(ny,nm,nd).lt.-1) cycle  ! no flow data
            if (tconc.lt.0) cycle  ! missing data
            ndays = ndays + 1
            obconc(ndays) = tconc
            obflow(ndays) = obfl(ny,nm,nd)
            obload(ndays) = tconc*obfl(ny,nm,nd)*mglcfs2lbday
            obyear(ndays) = ny
            obmonth(ndays) = nm
            obday(ndays) = nd
            obhour(ndays) = nh
            obLOD(ndays) = .false.
            if (QWflag.eq.'<') then
              obLOD(ndays) = .true.
              nLOD = nLOD + 1
            end if
          end do
333       close(ifl)           ! close observed load file
        end if

****************** find the quintiles of the observed flow and
************* observed flow on the days of observation
        call binaves(
     I               tobflo,tobdays,
     I               ndaymax,nbinsmax,nbins,
     O               afbin)

        call binaves(
     I               obflow,ndays,
     I               ndaymax,nbinsmax,nbins,
     O               ofbin)


*********** write out the quintiles, the number of obs, and nLOD
        print*,'seg,conc,nobs,nLOD,all1,all2,all3,all4,all5,',
     .             'obs1,obs2,obs3,obs4,obs5'

        print 1234,rseg,concname(np),ndays,nLOD,
     .                (afbin(nb),nb=1,nbins),(ofbin(nb),nb=1,nbins)

      end do      ! end loop over all loads in 'rchres_out_to_conc'
      print*,''


100   format(3x,'Basic Statistics for',1x,a4,1x,'Concentration')
200   format(3x,'Basic Statistics for',1x,a4,1x,'Load')
1234  format(a13,',',a4,2(',',i4),10(',',e14.6))

      return
  
************************ error reporting

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

