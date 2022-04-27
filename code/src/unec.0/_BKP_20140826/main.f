
      implicit none

      include 'eofc.inc'


      character*6        C6_tlseg
      integer            I_lentlseg,I_lenfnam
      integer            I_tlus(nlu)
      character*3        C3_tlus(nlu), C3_tquals(I_NQUALS)
      character*10       C10_SpecFlags(nlu)
      character*4        C4_tStr
      integer            I_tInt
      integer            I_tYear, I_tMonth
      integer            I_tquals(I_NQUALS)
      double precision   D_tSval,D_tBval
      double precision   D_tSpecies(I_NAPPSPECIES)
      double precision   D_tSURO,D_tSEDM,D_tIFWO,D_tAGWO
      double precision   D_Sfactor,D_Bfactor
      double precision   D_tSCON,D_tDCON,D_tICON,D_tACON
      double precision   D_AvgAnnApp
      logical            L_found

      integer            I_NOLUTARGETS
      parameter          (I_NOLUTARGETS = 3) ! wat afo cfo

      logical            comment
      external           comment
      
      integer            ndaysinmonth
      external           ndaysinmonth

      double precision   D_MonApp(I_MAXMONTHS)
c      data D_MonApp /I_MAXMONTHS*0/

      integer            I_nMonths

      integer            I_sParamLeadEdge
      integer            I_sParamPeakConc
      integer            I_sParamTrailEdge
      double precision   D_sParamExponent1, D_sParamExponent2
      integer            I_sParamNormalize

      integer            I_dParamLeadEdge
      integer            I_dParamPeakConc
      integer            I_dParamTrailEdge
      double precision   D_dParamExponent1, D_dParamExponent2
      integer            I_dParamNormalize

      integer            I_iParamLeadEdge
      integer            I_iParamPeakConc
      integer            I_iParamTrailEdge
      double precision   D_iParamExponent1, D_iParamExponent2
      integer            I_iParamNormalize

      integer            I_aParamLeadEdge
      integer            I_aParamPeakConc
      integer            I_aParamTrailEdge
      double precision   D_aParamExponent1, D_aParamExponent2
      integer            I_aParamNormalize

      double precision   D_fracS, D_fracD, D_fracI, D_fracA ! Partition of Input between Surface, Detached, Interflow & Groundwater
      ! TODO Implement separate fractions for QUALS, read from file for given land-segment

      read*,lseg,lscen,I_Year1,I_Year2,I_YearC1,I_YearC2

      !>> ASSIGN partition of input loads to transport mechanism
      !   D_fracS + D_fracD + D_fracI + D_fracA = 1
      !   NOTE: This can be time variable as well based on flow regimes
      !   D_fracS & D_fracD : Linear Isotherm is a suitable choice as it falls
      !         between Freundlich and Langmuir Isotherm.
      D_fracS    = 0.45D0
      D_fracD    = 0.10D0
      D_fracI    = 0.25D0
      D_fracA    = 0.25D0

c      D_fracS    = 0.4
c      D_fracD    = 0.5
c      D_fracI    = 0.09
c      D_fracA    = 0.01



      !>> ASSIGN 5 Breakthrough Curve Parameters
      !   TODO Read from CSV File btc_<lu>.csv
      !   A10001,S,sp1,sp2,sp3,sp4,sp5
      !   A10001,D,dp1,dp2,dp3,dp4,dp5
      I_sParamLeadEdge    = 1
      I_sParamPeakConc    = 1
      I_sParamTrailEdge   = 12
      D_sParamExponent1   = 0.1D0
      D_sParamExponent2   = 0.3D0
      I_sParamNormalize   = 1

      I_dParamLeadEdge    = 1
      I_dParamPeakConc    = 1
      I_dParamTrailEdge   = 12
      D_dParamExponent1   = 0.1D0
      D_dParamExponent2   = 0.2D0
      I_dParamNormalize   = 1

      I_iParamLeadEdge    = 2
      I_iParamPeakConc    = 2
      I_iParamTrailEdge   = 25
      D_iParamExponent1   = 0.1D0
      D_iParamExponent2   = 0.1D0
      I_iParamNormalize   = 1

      I_aParamLeadEdge    = 3
      I_aParamPeakConc    = 7
      I_aParamTrailEdge   = 36
      D_aParamExponent1   = 0.10D0
      D_aParamExponent2   = 0.08D0
      I_aParamNormalize   = 1



      !>> COMPUTE "characteristic" Breakthrouh Curve factors
      call BreakThroughFactors(
     .        I_sParamLeadEdge,I_sParamPeakConc,I_sParamTrailEdge,
     .        D_sParamExponent1,D_sParamExponent2,I_sParamNormalize,
     .        s1Factors)
      call BreakThroughFactors(
     .        I_dParamLeadEdge,I_dParamPeakConc,I_dParamTrailEdge,
     .        D_dParamExponent1,D_dParamExponent2,I_dParamNormalize,
     .        d1Factors)
      call BreakThroughFactors(
     .        I_iParamLeadEdge,I_iParamPeakConc,I_iParamTrailEdge,
     .        D_iParamExponent1,D_iParamExponent2,I_iParamNormalize,
     .        i1Factors)
      call BreakThroughFactors(
     .        I_aParamLeadEdge,I_aParamPeakConc,I_aParamTrailEdge,
     .        D_aParamExponent1,D_aParamExponent2,I_aParamNormalize,
     .        a1Factors)



      !>> READ "monthly" inputs from output/input/
      data D_MonApp /I_MAXMONTHS*0.0D0/
c      D_MonApp(1) = 0.0D0
c      D_MonApp(2) = 0.0D0
c      D_MonApp(3) = 0.0D0
c      D_MonApp(4) = 0.0D0
c      D_MonApp(5) = 0.0D0
      D_MonApp(6) = 1.0D0
      D_MonApp(7) = 0.0D0
      D_MonApp(8) = 0.0D0


      call lencl(lseg,lenlseg)
      call lencl(lscen,lenlscen)



      !>> initialize WDM DSNs using perlnd file
      ! OutWDMs [ I_NQUALS ][ I_NSTATES ]
      do i = 1,I_NQUALS
      !{
         do j = 1,I_NSTATES
         !{
            OutWDMs(i,j) = 999
            quals_to_states(i,j) = 0
         !}
         end do
      !}
      end do

      call readcontrol_Lgeoscen(
     I                          lscen,lenlscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      fnam = catdir//'iovars/'//geoscen(:lengeoscen)//'/perlnd'

      do i = 1,I_NQUALS
      !{
         do j = 1,I_NSTATES
         !{
            C4_tStr = states(j)//quals(i)
c            print*,'>>',C4_tStr
            open(dfile,file=fnam,status='old',iostat=err)
            if (err.ne.0) go to 991
            do
            !{
               read(dfile,'(a100)',err=992,end=112) line
               call d2x(line,last)
               if (comment(line)) cycle
               if (line(:20).eq.'                    ') cycle
c               print*,line(26:29),'<>',C4_tStr
               if (line(26:29).eq.C4_tStr) then
               !{
                  read(line(:3),*) OutWDMs(i,j)
                  quals_to_states(i,j) = 1
               !}
               end if
            !}
            end do
112         close (dfile)
         !}
         end do
      !}
      end do
      print*,'\n\n\n'
      print*,'PQUAL Land WDM Strcture (I_NQUALS,I_NSTATES) from iovar/'
     .        //geoscen(:lengeoscen)//'/perlnd'
      write(*,'(A4,4(A4$))') ' ',(states(i),i=1,I_NSTATES)
      write(*,*) ''
      do i=1,I_NQUALS
         print*,quals(i),(OutWDMs(i,j),j=1,I_NSTATES)
      end do

      call FlowDSN(fnam,flow(1),dsnSURO)
      call FlowDSN(fnam,flow(2),dsnSEDM)
      call FlowDSN(fnam,flow(3),dsnIFWO)
      call FlowDSN(fnam,flow(4),dsnAGWO)

      print*,'FLOWDSNs ',dsnSURO,dsnSEDM,dsnIFWO,dsnAGWO

      !>> get PQUAL to Species Mapping
      fnam = 'inputs/quals_to_appspecies.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do i=1,I_NQUALS
      !{
100      read(dfile,'(a100)',err=992,end=114) line
         call d2x(line,last)
         if (comment(line)) goto 100
         if (line(:10).eq.'          ') goto 100
         read(line,*,end=993,err=993)
     .        (quals_to_appspecies(i,k),k=1,I_NAPPSPECIES)
      !}
      end do
114   close(dfile)

      print*,"\n\nQuals to AppSpecies:"
      do i=1,I_NQUALS
         print*,(quals_to_appspecies(i,k),k=1,I_NAPPSPECIES)
      end do



      !>> Get what app(lication) type inputs (flags) are available
      ! Using spec_flags.csv file at the moment that allows basin specific flags
      ! Alternatively "landuse_apptypes.csv" can be used as global descriptor

      call readcontrol_Lspecscen(
     I                           lscen,lenlscen,
     O                           specscen)

      print*,'\n\n',specscen            
      call lencl(specscen,lenspecscen)
      print*,specscen(:lenspecscen)

      fnam = ScenDatDir//'land/spec_flags/'//specscen(:lenspecscen)//
     .       '/spec_flags.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
                     
      read(dfile,'(a300)',err=992,end=115)longline
      call d2x(longline,last)
      read (longline,*,end=995,err=995) C6_tlseg,(C3_tlus(i),i=1,nlu-1)
      do i = 1,nlu-1
      !{    
         j = 1
201      if ( C3_tlus(i).ne.luname(j) ) then
         !{
            j = j + 1
            go to 201
         !}
         end if
         I_tlus(i) = j
      !} 
      end do
      print*,'spec_flags lus: ',(I_tlus(i),i=1,nlu)
      
         
      do    
      !{       
         read(dfile,'(a300)',err=992,end=115)longline
         call d2x(longline,last)
         read(longline,*,end=994,err=994)
     .               C6_tlseg,(C10_SpecFlags(i),i=1,nlu-1)
         if ( C6_tlseg.eq.lseg(:lenlseg) ) then
         !{
c           print*,C10_SpecFlags(3),len(C10_SpecFlags(3))
           do i = 1,nlu
           !{
             do k = 1,I_NAPPTYPES
             !{
                apptypesflag(i,k) = 0
             !}
             end do

             call lencl(C10_SpecFlags(i), I_tInt)
             do j = 1,I_tInt
             !{
               if(C10_SpecFlags(i)(j:j).eq.'Q')C10_SpecFlags(i)(j:j)='A'
c                print*,C10_SpecFlags(i)(j:j)
                do k = 1,I_NAPPTYPES
                !{
c                   print*,'>',C10_SpecFlags(i)(j:j),'#',apptypes(k),'<'
                   if ( C10_SpecFlags(i)(j:j).eq.apptypes(k) ) then
                   !{
                      apptypesflag(I_tlus(i),k) = 1
                   !}
                   end if
                !}
                end do
             !}
             end do
           !}
           end do
           exit
         !}
         end if
      !}
      end do
      do i = 1,nlu
         print*,'nlu= ',i,', ',(apptypesflag(i,k),k=1,I_NAPPTYPES)
      end do
      do k = 1,I_NAPPTYPES
      !{
         if ( forceignore(k).eq.0 ) then
            do i = 1,nlu
               apptypesflag(i,k) = 0
            end do
         end if
      !}
      end do
      print*,'\nForcing A, V=0, P=0, F, M, L, T, U=0, R=0'
      do i = 1,nlu
         print*,'nlu= ',i,', ',(apptypesflag(i,k),k=1,I_NAPPTYPES)
      end do
115   close(dfile)



      call readcontrol_Lcalscen(
     I                          lscen,lenlscen,
     O                          lcalib)
      call lencl(lcalib,lenlcalib)
      print*,'CALIBRATION: ',lcalib(:lenlcalib)



      !>> OPEN Dummy WDM
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+1,wdmfnam,0,err)
      if (err.ne.0) go to 801

      !>> INITIALIZE TARGETS
      !   IF lscen = lcalib
      !       READ TARGETS (S/surface & B/baseflow)
      !   ELSE
      !       Compute Surface & Baseflow exports over I_YearC1 to I_YearC2 in lcalib
      print*,'SCENARIO:    ',lscen(:lenlscen)
      print*,'CALIBRATION: ',lcalib(:lenlcalib)
      if (lscen(:lenlscen).eq.lcalib(:lenlcalib)) then
      !{
         print*,'TARGETS IN ***CALIBRATION*** MODE'
         do i=1,I_NQUALS
         !{
            !>> READ SURFACE TARGETS
c            fnam=calibdir//'target/g533/'//'S'//quals(i)//'_target.csv' TODO
            fnam=calibdir//'target/del/'//'S'//quals(i)//'_target.csv'
            print*,'STARGETS ',fnam
            L_found = .false.
            open(dfile,file=fnam,status='old',iostat=err)
            if (err.ne.0) go to 991
            read(dfile,'(a300)',err=992,end=116) longline ! header line
            call d2x(longline,last)
            read (longline,*,end=995,err=995)
     .                 C6_tlseg,(C3_tlus(j),j=1,nlu-I_NOLUTARGETS)
            do j = 1,nlu-I_NOLUTARGETS
            !{    
               k = 1
203            if ( C3_tlus(j).ne.luname(k) ) then
               !{
                  k = k + 1
                  if (k.gt.nlu+1) goto 996
                  go to 203
               !}
               end if
               I_tlus(j) = k
            !}
            end do

            do
            !{
               read(dfile,'(a300)',err=992,end=116) longline
               call d2x(longline,last)
               read(longline,*,end=995,err=995)
     .           C6_tlseg,(D_sTarget(I_tlus(j),i),j=1,nlu-I_NOLUTARGETS)
               if ( C6_tlseg.eq.lseg(:lenlseg) ) then
                  L_found = .true.
                  exit
               end if
            !}
            end do
116         close (dfile)

            if (L_found .eqv. .false.) goto 997
            print*,'STARGETS: ',lseg(:lenlseg),' '
            do j = 1,nlu
               print*,luname(j),D_sTarget(j,i)
            end do



            !>> READ BASEFLOW TARGETS
c            fnam=calibdir//'target/g533/'//'B'//quals(i)//'_target.csv' TODO
            fnam=calibdir//'target/del/'//'B'//quals(i)//'_target.csv'
            print*,'BTARGETS ',fnam
            L_found = .false.
            open(dfile,file=fnam,status='old',iostat=err)
            if (err.ne.0) go to 991
            read(dfile,'(a300)',err=992,end=117) longline ! header line
            call d2x(longline,last)
            read (longline,*,end=995,err=995)
     .                 C6_tlseg,(C3_tlus(j),j=1,nlu-I_NOLUTARGETS)
            do j = 1,nlu-I_NOLUTARGETS
            !{    
               k = 1
202            if ( C3_tlus(j).ne.luname(k) ) then
               !{
                  k = k + 1
                  if (k.gt.nlu+1) goto 996
                  go to 202
               !}
               end if
               I_tlus(j) = k
            !}
            end do

            do
            !{
               read(dfile,'(a300)',err=992,end=117) longline
               call d2x(longline,last)
               read(longline,*,end=995,err=995)
     .           C6_tlseg,(D_bTarget(I_tlus(j),i),j=1,nlu-I_NOLUTARGETS)
               if ( C6_tlseg.eq.lseg(:lenlseg) ) then
                  L_found = .true.
                  exit
               end if
            !}
            end do
117         close (dfile)

            if (L_found .eqv. .false.) goto 997
            print*,'BTARGETS: ',lseg(:lenlseg)
            do j = 1,nlu
               print*,luname(j),' ',D_bTarget(j,i)
            end do

         !}
         end do
         do i=1,nlu
           do j=1,I_NQUALS
             print*,'ORG TARGET',' ',luname(i),' ',quals(j),' ',
     .                 D_sTarget(i,j),' ',D_bTarget(i,j)
           end do
         end do
      !}
      else
      !{
         !Open Monthly Export File of CALIBRATION (lcalib)
         print*,'TARGETS IN ***SCENARIO*** MODE'

         do i = 1,nlu
         !{
            if ( luname(i).eq.'wat' ) cycle
            if ( luname(i).ne.'hom' ) cycle

            do j = 1,I_NQUALS
            !{
               fnam = outdir//'eof/monthly/'//lcalib(:lenlcalib)//'/'//
     .          luname(i)//'_'//lseg(:lenlseg)//'.'//quals(j)
               call lencl(fnam,I_lenfnam)
               write(*,'(3(A$))'),' Reading ',fnam(:I_lenfnam),' ...'
               print*,''
               open(dfile,file=fnam,status='unknown',iostat=err)
               if (err.ne.0) go to 991

               read(dfile,'(a100)',err=992,end=120) line ! header line

               D_sTarget(i,j) = 0.0D0
               D_bTarget(i,j) = 0.0D0
               
               do
               !{
                  read(dfile,'(a300)',err=992,end=120) longline
                  call d2x(longline,last)
                  read(longline,*,end=995,err=995)
     .               I_tYear,I_tMonth,(D_Meof(k),k=1,I_NSTATES)
                  if (I_tYear.ge.I_YearC1.and.I_tYear.le.I_YearC2) then
                  !{
                     do k = 1,I_NSTATES
                     !{
                        if ( states(k).ne.'A' ) then
                        !{
                           D_sTarget(i,j) = D_sTarget(i,j) + D_Meof(k)
                        !}
                        else
                        !{
                           D_bTarget(i,j) = D_bTarget(i,j) + D_Meof(k)
                        !}
                        end if
                     !}
                     end do
                  !}
                  end if
               !}
               end do
120            close (dfile)

               D_sTarget(i,j) = D_sTarget(i,j) / (I_YearC2-I_YearC1+1)
               D_bTarget(i,j) = D_bTarget(i,j) / (I_YearC2-I_YearC1+1)

               print*,'CAL TARGET',' ',luname(i),' ',quals(j),' ',
     .                 D_sTarget(i,j),' ',D_bTarget(i,j)
            !}
            end do

         !}
         end do

      !}
      end if



      !>> FOR each land-use { use .inc to get lu char key }
      do l = 1,nlu
      !{
         print*,''
         print*,"*** Land Use: "//luname(l)//" ***"

         if ( luname(l).eq.'wat' ) cycle
         if ( luname(l).ne.'hom' ) cycle ! TODO DELETE


         !>> for calibration (1991-2000)
         ! compute inputs from each source and for each appspecies
         ! TODO IN_CALIB[ SOURCE ][ APPSPECIES ]
         ! TODO IN_CALIB_SRC[ SOURCE ]

         do i = 1,I_NAPPTYPES
         !{
            do j = 1,I_NAPPSPECIES
            !{
               do I_tYear = I_Year1,I_Year2
                  do I_tMonth = 1,12
                  !{
                     D_inp_calib_app(i,j,I_tYear,I_tMonth) = 0.0D0
                     D_inp_scen_app(i,j,I_tYear,I_tMonth)  = 0.0D0
                  !}
                  end do
               end do
            !}
            end do
         !}
         end do

         do i = 1,I_NAPPTYPES
         !{
            if (apptypesflag(l,i) == 0) cycle

            call lencl(apptypeslong(i),I_tInt)
cc            print*,">>",t6Char(:I_tInt),"<<"

            fnam = outdir//'input/'//lcalib(:lenlcalib)//'/monthly_'//
     .       apptypeslong(i)(:I_tInt)//'_'//lseg//'_'//luname(l)//'.csv'
            open(dfile,file=fnam,status='old',iostat=err)
            if (err.ne.0) go to 991

            read(dfile,'(a100)',err=992,end=113) line ! header line

            ! *** assuming a fixed column structure -- no mapping required ***
            ! i.e. column sequence in the annual_* files in output/input
            do
            !{
               read(dfile,'(a100)',err=992,end=113) line
               call d2x(line,last)
               read(line,*,end=993,err=993)
     .                I_tYear,I_tMonth,(D_tSpecies(j),j=1,I_NAPPSPECIES)
               if (I_tYear.ge.I_Year1.and.I_tYear.le.I_Year2) then
               !{
                  do j=1,I_NAPPSPECIES
                  !{
                     D_inp_calib_app(i,j,I_tYear,I_tMonth) = 
     .                         D_inp_calib_app(i,j,I_tYear,I_tMonth)
     .                         + D_tSpecies(j)
                  !}
                  end do
               !}
               end if
            !}
            end do
113         close (dfile)
         !}
         end do
         do i = 1,I_NAPPTYPES
         !{
            write(*,'(A6,5(A12))') 'apptypes',
     .                   (appspecies(j),j=1,I_NAPPSPECIES)
            do yy = I_Year1,I_Year2
             do mm = 1,12
                  write(*,'(A6,5(e12.5))')apptypeslong(i),
     .                   (D_inp_calib_app(i,j,yy,mm),j=1,I_NAPPSPECIES)
             end do
            end do
         !}
         end do


         !>> READ Monthly Inputs if running a Scenario
         if (lscen(:lenlscen).ne.lcalib(:lenlcalib)) then
         !{
            do i = 1,I_NAPPTYPES
            !{
               if (apptypesflag(l,i) == 0) cycle

               call lencl(apptypeslong(i),I_tInt)
cc               print*,">>",t6Char(:I_tInt),"<<"

               fnam = outdir//'input/'//lscen(:lenlscen)//'/monthly_'
     .                //apptypeslong(i)(:I_tInt)//
     .                '_'//lseg//'_'//luname(l)//'.csv'
               open(dfile,file=fnam,status='old',iostat=err)
               if (err.ne.0) go to 991
               read(dfile,'(a100)',err=992,end=118) line ! header line

               ! *** assuming a fixed column structure -- no mapping required ***
               ! i.e. column sequence in the annual_* files in output/input
               do
               !{
                  read(dfile,'(a100)',err=992,end=118) line
                  call d2x(line,last)
                  read(line,*,end=993,err=993)
     .                I_tYear,I_tMonth,(D_tSpecies(j),j=1,I_NAPPSPECIES)
                  if (I_tYear.ge.I_Year1.and.I_tYear.le.I_Year2) then
                  !{
                     do j=1,I_NAPPSPECIES
                     !{
                        D_inp_scen_app(i,j,I_tYear,I_tMonth) =
     .                            D_inp_scen_app(i,j,I_tYear,I_tMonth)
     .                            + D_tSpecies(j)
                     !}
                     end do
                  !}
                  end if
               !}
               end do
118            close (dfile)
            !}
            end do
         !}
         end if


         ! >> TODO APPLY SENSITIVITY
         !    Apply sensitivity to quals (based on ground e.g. Total, Inorganic
         !    Organic, etc.) to scale the Surface- and Baseflow Targets
         if (lscen(:lenlscen).ne.lcalib(:lenlcalib)) then
         !{
            do i = 1,I_NAPPTYPES
            !{
               if (apptypesflag(l,i) == 0) cycle

               call lencl(apptypeslong(i),I_tInt)
               fnam = 'inputs/'//luname(l)//'_'//
     .                 apptypeslong(i)(:I_tInt)//'_sensitivity.csv'
               call lencl(fnam,I_lenfnam)
               write(*,'(3(A$))'),' Reading ',fnam(:I_lenfnam),' ...'
               print*,''
               open(dfile,file=fnam,status='old',iostat=err)
               if (err.ne.0) go to 991

               read(dfile,'(a300)',err=992,end=119) longline
c               print*,'$',longline
               call d2x(longline,last)
               read (longline,*,end=993,err=993)
     .                C6_tlseg,(C3_tquals(j),j=1,I_NQUALS)
c               print*,(C3_tquals(j),j=1,I_NQUALS)

               do j = 1,I_NQUALS
               !{
                  k = 1
204               if ( C3_tquals(j).ne.quals(k) ) then
                  !{
                     k = k + 1
                     go to 204
                  !}
                  end if
                  I_tquals(j) = k
               !}
               end do
               print*,(quals(j),j=1,I_NQUALS),' :: ',
     .                       (I_tquals(j),j=1,I_NQUALS)

               do
               !{
                  read (dfile,'(a300)',end=992,err=119) longline
                  call d2x(longline,last)
                  read(longline,*,end=994,err=994),C6_tlseg,
     .                    (C20_sensitivity(j),j=1,I_NQUALS),
     .                    (C2_tsmethod(j),j=1,I_NQUALS)
c                  print*,C6_tlseg
c                  print*,(C20_sensitivity(j),j=1,I_NQUALS)

                  call lencl(C6_tlseg,I_lentlseg)
                  if( C6_tlseg(:I_lentlseg).eq.lseg(:lenlseg) ) then
                  !{
                     do j = 1,I_NQUALS
                     !{
                        read(C20_sensitivity(j)(:1),*,end=996,err=996),
     .                         I_sgroup( I_tquals(j) )
                        read(C20_sensitivity(j)(3:),*,end=996,err=996),
     .                         D_sensitivity( I_tquals(j) )
                        print*,(I_sgroup(k),k=1,I_NQUALS)
                        print*,(D_sensitivity(k),k=1,I_NQUALS)
c                        D_sensitivity( I_tquals(j) ) = C20_sensitivity(j)
                        C2_smethod ( I_tquals(j) ) = C2_tsmethod(j)
                     !}
                     end do
                     exit
                  !}
                  end if
               !}
               end do

               print*,'Sensitivites: ',lseg(:lenlseg)
               write(*,'(5(A4$))') (quals(j),j=1,I_NQUALS)
               print*,''
               print*,'Sensitivity ',(D_sensitivity(j),j=1,I_NQUALS)
               print*,'Groupings   ',(I_sgroup(j),j=1,I_NQUALS)
               print*,'Methodology ',(C2_smethod(j),j=1,I_NQUALS)

119            close(dfile)

               !>> COMPUTE target for the sensitivity group
               !   * this is used for proportionately partitioning the change
               do j = 1,I_NQUALS
                  D_groupTarget(j) = 0.0D0
               end do
               do j = 1,I_NQUALS
               !{
c                  D_qualsTarget(j) = D_sTarget(l,j) + D_bTarget(l,j)
                  k = I_sgroup(j)
                  D_groupTarget(k) = D_groupTarget(k) 
     .                                + D_sTarget(l,j) + D_bTarget(l,j)
               !}
               end do
               do j = 1,I_NQUALS
               !{
                 k = I_sgroup(j)
                 print*,'Group Targets ',j,k,D_groupTarget(k)
               !}
               end do

               do j = 1,I_NQUALS
               !{
                D_tSval = 0.0D0
                D_tBval = 0.0D0
c TODO                if (quals_to_appspecies(j,i) = 0) cycle
                do k = 1,I_NAPPSPECIES
                !{
                 do yy = I_YearC1,I_YearC2
                 !{
                  do mm = 1,12
                  !{
                    if (C2_smethod(j).eq.'D1') then
                    !{
c                  write(*,'(E12.5,I2,E12.5,E12.5,E12.5,E12.5,E12.5,I3)')
c     .     D_tSval,quals_to_appspecies(j,k),
c     .     D_inp_scen_app(i,k,yy,mm),D_inp_calib_app(i,k,yy,mm),
c     .     D_sensitivity(j),D_sTarget(l,j),D_groupTarget(I_sgroup(j)),
c     .     (I_YearC2 - I_YearC1 + 1)
                     D_tSval = D_tSval + quals_to_appspecies(j,k) *
     .                ( D_inp_scen_app(i,k,yy,mm) 
     .                   - D_inp_calib_app(i,k,yy,mm) )
     .                * D_sensitivity(j)
     .                * (D_sTarget(l,j)/D_groupTarget(I_sgroup(j)))
     .                / (I_YearC2 - I_YearC1 + 1)
                     D_tBval = D_tBval + quals_to_appspecies(j,k) *
     .                ( D_inp_scen_app(i,k,yy,mm) 
     .                   - D_inp_calib_app(i,k,yy,mm) )
     .                * D_sensitivity(j)
     .                * (D_bTarget(l,j)/D_groupTarget(I_sgroup(j)))
     .                / (I_YearC2 - I_YearC1 + 1)
                    !}
                    else if (C2_smethod(j).eq.'D2') then
                    !{
                      print*,'PROBLEM METHOD '
     .                      ,C2_smethod(j),' NOT IMPLEMENTED'
                      goto 998
                    !}
                    else
                    !{
                      goto 998
                    !}
                    end if
                  !}
                  end do
                 !}
                 end do
                !}
                end do

                write(*,'(I2,I2,E12.5,E12.5$)') 
     .                    l,j,D_sTarget(l,j),D_tSval
                D_sTarget(l,j) = D_sTarget(l,j) + D_tSval
                write(*,'(E12.5)') D_sTarget(l,j)
                write(*,'(I2,I2,E12.5,E12.5$)') 
     .                    l,j,D_bTarget(l,j),D_tBval
                D_bTarget(l,j) = D_bTarget(l,j) + D_tBval
                write(*,'(E12.5)') D_bTarget(l,j)
               !}
               end do
            !}
            end do
         !}
         end if

         !>> OPEN SCENARIO WDM (w/ PWATER & SEDMNT)

         wdmfnam = luname(l)//lseg(:lenlseg)//'.wdm'
         print*,'Land WDM File: ',wdmfnam
         call wdbopnlong(wdmfil,wdmfnam,0,err)
         if (err.ne.0) go to 801

         sdate(1) = I_Year1
         sdate(2) = 1
         sdate(3) = 1
         sdate(4) = 0
         sdate(5) = 0
         sdate(6) = 0
               
         edate(1) = I_Year2
         edate(2) = 12
         edate(3) = 31
         edate(4) = 24
         edate(5) = 0
         edate(6) = 0

         !>> READ SURO, SEDM, IFWO, AGWO
         call gethourdsn(wdmfil,sdate,edate,dsnSURO,nvals,F_hSURO)
         call gethourdsn(wdmfil,sdate,edate,dsnSEDM,nvals,F_hSEDM)
         call gethourdsn(wdmfil,sdate,edate,dsnIFWO,nvals,F_hIFWO)
         call gethourdsn(wdmfil,sdate,edate,dsnAGWO,nvals,F_hAGWO)

         D_tSURO = 0.0D0
         D_tSEDM = 0.0D0
         D_tIFWO = 0.0D0
         D_tAGWO = 0.0D0
         do i = 1,nvals
            D_tSURO = D_tSURO + F_hSURO(i)
            D_tSEDM = D_tSEDM + F_hSEDM(i)
            D_tIFWO = D_tIFWO + F_hIFWO(i)
            D_tAGWO = D_tAGWO + F_hAGWO(i)
         end do

         print*,'SURO (in/ac/yr) ', D_tSURO / (I_Year2-I_Year1+1)
         print*,'SEDM (tn/ac/yr) ', D_tSEDM / (I_Year2-I_Year1+1)
         print*,'IFWO (in/ac/yr) ', D_tIFWO / (I_Year2-I_Year1+1)
         print*,'AGWO (in/ac/yr) ', D_tAGWO / (I_Year2-I_Year1+1)


         I_nMonths = (I_Year2 - I_Year1 + 1) * 12

         do k = 1,I_NQUALS ! TODO FIX THIS --> k=1,I_NQUALS
         !{
            print*,'\n\nQUAL = ',k
c            write(*,'(A)') 'Break Through Curve'
c            do i = 1,I_nMonths
c               write(*,'(I2, E12.5, E12.5)'), i,s1Factors(i-1),s2Factors(i)
c            end do
            do yy = I_Year1,I_Year2
            !{
               do mm = 1,12
               !{
                  D_MonApp((yy-I_Year1)*12+mm) = 0.0D0
                  do i = 1,I_NAPPTYPES
                  !{
                     do j = 1,I_NAPPSPECIES !TODO FIXED ON 20140815
                     !{
c                        print*,'HERE1 ', quals_to_appspecies(k,j)
                        if (quals_to_appspecies(k,j) .eq. 0) cycle
c                        print*,'HERE2 ', quals_to_appspecies(k,j)

                        if (apptypeslong(i) .ne. 'uptake') then
                           D_MonApp((yy-I_Year1)*12+mm) = 
     .                         D_MonApp((yy-I_Year1)*12+mm)
     .                         + D_inp_calib_app(i,j,yy,mm)
                        else
                           D_MonApp((yy-I_Year1)*12+mm) =
     .                         D_MonApp((yy-I_Year1)*12+mm)
     .                         - D_inp_calib_app(i,j,yy,mm)
                        end if
                     !}
                     end do
                  !}
                  end do
               !}
               end do
            !}
            end do

            print*,"Total Monthly Application/Input"
            !>> FIX NEGATIVE INPUT TODO
            D_AvgAnnApp = 0
            do i = 1,I_nMonths
c??               if ( D_MonApp(i) .lt. 0 ) D_MonApp(i) = 0
               D_AvgAnnApp = D_AvgAnnApp + D_MonApp(i)
               print*,i,D_MonApp(i)
            end do
            D_AvgAnnApp = D_AvgAnnApp / (I_nMonths/12)
            print*,'\n\nQUAL = ',k,' INPUT = ',D_AvgAnnApp

            do j = 1,I_nMonths
               s2Factors(j) = 0.0D0
               d2Factors(j) = 0.0D0
               i2Factors(j) = 0.0D0
               a2Factors(j) = 0.0D0
            end do 
            do i = 1,I_nMonths
            !{
               do j = i,I_nMonths
               !{
                  s2Factors(j) = s2Factors(j) 
     .                           + D_fracS*D_MonApp(i)*s1Factors(j-i+1)
     .                             * quals_to_states(k,1)
                  d2Factors(j) = d2Factors(j)
     .                           + D_fracD*D_MonApp(i)*d1Factors(j-i+1)
     .                             * quals_to_states(k,2)
                  i2Factors(j) = i2Factors(j)
     .                           + D_fracI*D_MonApp(i)*i1Factors(j-i+1)
     .                             * quals_to_states(k,3)
                  a2Factors(j) = a2Factors(j)
     .                           + D_fracA*D_MonApp(i)*a1Factors(j-i+1)
     .                             * quals_to_states(k,4)
               !}
               end do
            !}
            end do
            write(*,'(A)') 'Break Through Curve of Analyte'
            do i = 1,I_nMonths
            !{
               !TODO: Fix Negative Factors (Ensure non -ve export of analyte)
               if ( s2Factors(i).lt.0 ) s2Factors(i) = 0.0D0
               if ( d2Factors(i).lt.0 ) d2Factors(i) = 0.0D0
               if ( i2Factors(i).lt.0 ) i2Factors(i) = 0.0D0
               if ( a2Factors(i).lt.0 ) a2Factors(i) = 0.0D0
c               write(*,'(I2, E12.5, E12.5)'), i,s1Factors(i-1),s2Factors(i)
c               write(*,'(I2,E12.5,E12.5,E12,5,E12.5)'),i,
c     .            s2Factors(i),d2Factors(i),i2Factors(i),a2Factors(i)
               write(*,1237) i,
     .              s2Factors(i),d2Factors(i),i2Factors(i),a2Factors(i)
            !}
            end do

            !>> SCALE SEDM values to SURO, TODO ?? Apply D vs. S weight
            do i = 1,nvals
c               D_hDCON(i) = (D_tSURO/D_tSEDM)*(D_fracD/D_fracS) * F_hSEDM(i)
               D_hDCON(i) = (D_tSURO/D_tSEDM) * F_hSEDM(i)
c               D_hDCON(i) = ( (D_tSURO+D_tIFWO) / D_tSEDM )
c     .                        * F_hSEDM(i)
            end do
            
            !>> COMPUTE YIELDS (Surface,S & Baseflow,B) = FLOW * CONC
            D_tSCON = 0.0D0
            D_tDCON = 0.0D0
            D_tICON = 0.0D0
            D_tACON = 0.0D0
            j = 1  ! ** count hours
            m = 1  ! ** count months
            do yy = I_Year1,I_Year2
            !{
               do mm = 1,12
               !{
                  do i=1,ndaysinmonth(yy,mm)*24
                  !{
                     D_hSCON(j) = F_hSURO(j) * s2Factors(m)
                     D_hDCON(j) = D_hDCON(j) * d2Factors(m) !***
                     D_hICON(j) = F_hIFWO(j) * i2Factors(m)
                     D_hACON(j) = F_hAGWO(j) * a2Factors(m)
                     if( yy.ge.I_YearC1 .and. yy.le.I_YearC2 ) then
                     !{
                        D_tSCON = D_tSCON + D_hSCON(j)
                        D_tDCON = D_tDCON + D_hDCON(j)
                        D_tICON = D_tICON + D_hICON(j)
                        D_tACON = D_tACON + D_hACON(j)
                     !}
                     end if

                     j = j + 1
                  !}
                  end do
                  m = m + 1
               !}
               end do
            !}
            end do

            !>> READ TARGETS (S/surface & B/baseflow)
c            D_sTarget(l,1) = 1.58748 ! SNH3
c            D_bTarget(l,1) = 1.14879 ! BNH3

c            D_sTarget(l,2) = 8.00996
c            D_bTarget(l,2) = 24.2846

            !D_sTarget(1) = 1.08748 ! SNH3
            !D_bTarget(1) = 0.64879 ! BNH3
            print*,'\nSTARGET ',luname(l),' ',quals(k),D_sTarget(l,k),
     .             '\nBTARGET ',luname(l),' ',quals(k),D_bTarget(l,k)

            !>> Compute Scalaing Factor TARGET / YIELD
            D_Sfactor = D_sTarget(l,k) * (I_YearC2-I_YearC1+1)
     .                  / (D_tSCON + D_tDCON + D_tICON)
            D_Bfactor = D_bTarget(l,k) * (I_YearC2-I_YearC1+1)
     .                  / (D_tACON)

            write(*,'(/A F12.5)') 'Factor S = ',D_Sfactor
            write(*,'(A F12.5)' ) 'Factor B = ',D_Bfactor

            !>> ADJUST S/D/I/A-2 factors to get EOF Yields
            D_tSCON = 0.0D0
            D_tDCON = 0.0D0
            D_tICON = 0.0D0
            D_tACON = 0.0D0
            j = 1  ! ** count hours
            do yy = I_Year1,I_Year2
               do mm = 1,12
                  do i=1,ndaysinmonth(yy,mm)*24
                     D_hSCON(j) = D_hSCON(j) * D_Sfactor
                     D_hDCON(j) = D_hDCON(j) * D_Sfactor
                     D_hICON(j) = D_hICON(j) * D_Sfactor
                     D_hACON(j) = D_hACON(j) * D_Bfactor
                     if( yy.ge.I_YearC1.and.yy.le.I_YearC2 ) then
                        D_tSCON = D_tSCON + D_hSCON(j)
                        D_tDCON = D_tDCON + D_hDCON(j)
                        D_tICON = D_tICON + D_hICON(j)
                        D_tACON = D_tACON + D_hACON(j)
                     end if
                     F_hSCON(j) = D_hSCON(j)
                     F_hDCON(j) = D_hDCON(j)
                     F_hICON(j) = D_hICON(j)
                     F_hACON(j) = D_hACON(j)

                     j = j + 1
                  end do
               end do
            end do
            write(*,'(/A F10.5)')"I = ", D_AvgAnnApp
            write(*,'(A F8.5)')  "S = ", D_tSCON/(I_YearC2-I_YearC1+1)
            write(*,'(A F8.5)' ) "D = ", D_tDCON/(I_YearC2-I_YearC1+1)
            write(*,'(A F8.5)' ) "I = ", D_tICON/(I_YearC2-I_YearC1+1)
            write(*,'(A F8.5)' ) "A = ", D_tACON/(I_YearC2-I_YearC1+1)

            fnam = outdir//'eof/monthly/'//lscen(:lenlscen)//'/'//
     .          luname(l)//'_'//lseg(:lenlseg)//'.'//quals(k)
            call lencl(fnam,I_lenfnam)
            write(*,'(3(A$))'),' Writing ',fnam(:I_lenfnam),' ...'
            print*,''
            open(dfile,file=fnam,status='unknown',iostat=err)
            if (err.ne.0) go to 991
            write(dfile,1234) 'year,mm',
     .       ',S',quals(k),',D',quals(k),',I',quals(k),',A',quals(k),','

            !>> COMPUTE monthly yields
            j = 1  ! ** count hours
            m = 1  ! ** count months
            do yy = I_Year1,I_Year2
               do mm = 1,12
                  D_mSCON(m) = 0.0D0
                  D_mDCON(m) = 0.0D0
                  D_mICON(m) = 0.0D0
                  D_mACON(m) = 0.0D0
                  do i=1,ndaysinmonth(yy,mm)*24
                     D_mSCON(m) = D_mSCON(m) + D_hSCON(j)
                     D_mDCON(m) = D_mDCON(m) + D_hDCON(j)
                     D_mICON(m) = D_mICON(m) + D_hICON(j)
                     D_mACON(m) = D_mACON(m) + D_hACON(j)
                     j = j + 1
                  end do
                  write(*,1236) yy,mm,D_MonApp(m),
     .                 D_mSCON(m),D_mDCON(m),D_mICON(m),D_mACON(m)
                  write(dfile,1235) yy,mm,
     .                 D_mSCON(m),D_mDCON(m),D_mICON(m),D_mACON(m)
                  m = m + 1
               end do
            end do
            close(dfile)

            
            !>> WRITE WDM
            print*,'\nWriting WDMs...'
            print*,'\n',quals(k),OutWDMs(k,1)
            if (OutWDMs(k,1) .ne. 999)
     .    call puthourdsn(wdmfil,sdate,edate,OutWDMs(k,1),nvals,F_hSCON)
            print*,'\n',quals(k),OutWDMs(k,2)
            if (OutWDMs(k,2) .ne. 999)
     .    call puthourdsn(wdmfil,sdate,edate,OutWDMs(k,2),nvals,F_hDCON)
            print*,'\n',quals(k),OutWDMs(k,3)
            if (OutWDMs(k,3) .ne. 999)
     .    call puthourdsn(wdmfil,sdate,edate,OutWDMs(k,3),nvals,F_hICON)
            print*,'\n',quals(k),OutWDMs(k,4)
            if (OutWDMs(k,4) .ne. 999)
     .    call puthourdsn(wdmfil,sdate,edate,OutWDMs(k,4),nvals,F_hACON)

         !}
         end do

         !>> CLOSE WDM
         call wdflcl(wdmfil,err)
         if (err.ne.0) go to 803
      !}
      end do

      return

1234  format(A7,A2,A3,A2,A3,A2,A3,A2,A3,A)
1235  format(I4,',',I2,',',e19.12,',',e19.12,',',e19.12,',',e19.12)
1236  format(I4,',',I2,',',e19.12,4(',',e19.12))
1237  format(I4,4(',',e15.8))


801   report(1) = 'WDM file must previously exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
      go to 999

803   report(1) = 'error closing wdm file: '
      report(2) = wdmfnam
      report(3) = ' '
      go to 999

991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Problem reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'Problem parsing line: in file:'
      report(2) = line
      report(3) = fnam
      go to 999

994   report(1) = 'Problem parsing line: in file:'
      report(2) = longline
      report(3) = fnam
      go to 999

995   report(1) = 'Problem parsing line: in file:'
      report(2) = longline
      report(3) = fnam
      go to 999

996   report(1) = 'Problem parsing land use in file:'
      report(2) = fnam
      report(3) = longline
      go to 999

997   report(1) = 'Problem parsing land seg in file:'
      report(2) = fnam
      report(3) = lseg(:lenlseg)
      go to 999

998   report(1) = 'Sensitivity Method Not Implemented'
      report(2) = fnam
      report(3) = C2_smethod(j)
      go to 999

999   call stopreport(report)
      end







***** Following subroutine "BreakThroughFactors" computes the Breakthrough curve
***** based on following *FIVE* parameters.
***** I_paramLeadEdge  = elapsed time to the LEADing EDGE
***** I_paramPeakConc  = elapsed time to the arrival of PEAK CONCentration
***** I_paramTrailEdge = elapsed time to the TRAILing EDGE of the Response Curve
***** D_paramExponent1 = rate of exponential increase to the peak concentration
***** D_paramExponent2 = rate of exponential decay  from the peak concentration

      subroutine BreakThroughFactors(
     I          I_paramLeadEdge,
     I          I_paramPeakConc,
     I          I_paramTrailEdge,
     I          D_paramExponent1,
     I          D_paramExponent2,
     I          I_paramNormalize,
     O          D_tFactors)
      !{

         implicit none
         include 'eofc.inc'

         integer           I_paramLeadEdge
         integer           I_paramPeakConc
         integer           I_paramTrailEdge
         double precision  D_paramExponent1, D_paramExponent2
         integer           I_paramNormalize

         integer           I_temp
         double precision  D_temp

c         integer           I_MAXMONTHS
c         parameter         ( I_MAXMONTHS = 360 ) ! 30 Years x 12
         integer           I_nMonths

c         double precision  D_tFactors(I_MAXMONTHS)
c         data              D_tFactors /I_MAXMONTHS*0/

         double precision  D_total
         data              D_total /1*0.0D0/


         ! PARAMETERS ASSIGN
c         I_paramLeadEdge    = 6
c         I_paramPeakConc    = 12
c         I_paramTrailEdge   = 24
c         D_paramExponent1   = 0.1
c         D_paramExponent2   = 0.2
c         I_paramNormalize   = 0


         write(*,'(A I4 I4 I4$)')
     .        'Param: ',I_paramLeadEdge,I_paramPeakConc,I_paramTrailEdge
         write(*,'(E12.5, E12.5$)') D_paramExponent1,D_paramExponent2
         write(*,'(I2)') I_paramNormalize

         do i = 1,I_MAXMONTHS
            D_tFactors(i) = 0.0D0
         end do

c         I_nMonths = 60 ! TODO

c         if (I_paramTrailEdge .le. 0) I_paramTrailEdge = I_nMonths
         do i = 1,I_paramTrailEdge
         !{
            if      ( i .lt. I_paramLeadEdge ) then
                D_tFactors(i)=0
            else if ( i .lt. I_paramPeakConc ) then
                D_tFactors(i)=EXP( D_paramExponent1*(i-I_paramPeakConc))
            else
                D_tFactors(i)=EXP(-D_paramExponent2*(i-I_paramPeakConc))
            end if
            write(*,'(I2, E12.5)'), i,D_tFactors(i)
         !}
         end do

         if ( I_paramNormalize .eq. 1 ) then
         !{
            D_total = 0.0D0
            do i = 1,I_paramTrailEdge
            !{
               D_total = D_total + D_tFactors(i)
            !}
            end do
            write(*,'(A E12.5)') 'Total = ',D_total
         !}
         else
            D_total = 1.0D0
         end if

         D_temp = 0.0D0
         I_temp = 0
         do i = 1,I_paramTrailEdge
         !{
            D_tFactors(i) = D_tFactors(i)/D_total
            D_temp = D_temp + D_tFactors(i)
            if ( I_paramNormalize.eq.1
c     .          .and. D_temp.gt.0
     .          .and. D_temp.lt.0.5 ) then
               I_temp = I_temp + 1
            end if
            write(*,'(I2, E12.5)'), i,D_tFactors(i)
         !}
         end do
         I_temp = I_temp + 1
         if ( I_paramNormalize.eq.1 ) then
           ! http://www.epa.gov/ncea/pdfs/qtracer/qtracerch03.pdf Section 3.2.1
           write(*,'(A I2 A)'),'Mean Residence Time = ',I_temp,' months'
         end if

         return

      end
      !}


      SUBROUTINE FlowDSN(filename,dsnstr,dsn)

         implicit none
         include '../lib/inc/standard.inc'

         character*200 filename
c         integer       dfile
c         parameter     (dfile=10)
c         integer       err
c         character*200 line

         character*4   dsnstr
         integer       dsn

         logical       comment
         external      comment

         open(dfile,file=filename,status='old',iostat=err)
         if (err.ne.0) go to 991

         do
         !{
            read(dfile,'(a100)',err=992,end=310) line
            call d2x(line,last)
            if (comment(line)) cycle
            if (line(:20).eq.'                    ') cycle
c            print*,line(26:29),'<>',C4_tStr
            if (line(26:29).eq.dsnstr) then
            !{
               read(line(:3),*) dsn
            !}
            end if
         !}
         end do
310      close (dfile)
      
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999
      
992   report(1) = 'Problem reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999
   
999      END