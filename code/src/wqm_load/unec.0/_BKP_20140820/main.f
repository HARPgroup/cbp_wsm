
      implicit none

      include 'eofc.inc'


      character*6   C6_tlseg
      integer       I_lentlseg,I_lenfnam
      integer       I_tlus(nlu)
      character*3   C3_tlus(nlu), C3_tquals(nquals)
      character*10  C10_SpecFlags(nlu)
      character*4   C4_tStr
      integer       I_tInt
      integer       I_tYear, I_tMonth
      integer       I_tquals(nquals)
      real          R_tSval,R_tBval
      real          R_tSpecies(nappspecies)
      real          R_tSURO,R_tWSSD,R_tIFWO,R_tAGWO
      real          R_Sfactor,R_Bfactor
      real          R_tSCON,R_tDCON,R_tICON,R_tACON
      real          R_AvgAnnApp
      logical       found

      integer       NoLuTargets
      parameter     (NoLuTargets = 3) ! wat afo cfo

      logical  comment
      external comment
      
      integer  ndaysinmonth
      external ndaysinmonth

      real R_MonApp(maxMonths)
c      data R_MonApp /maxMonths*0/

      integer nMonths

      integer sParamLeadEdge, sParamPeakConc, sParamTrailEdge
      real    sParamExponent1, sParamExponent2
      integer sParamNormalize

      integer dParamLeadEdge, dParamPeakConc, dParamTrailEdge
      real    dParamExponent1, dParamExponent2
      integer dParamNormalize

      integer iParamLeadEdge, iParamPeakConc, iParamTrailEdge
      real    iParamExponent1, iParamExponent2
      integer iParamNormalize

      integer aParamLeadEdge, aParamPeakConc, aParamTrailEdge
      real    aParamExponent1, aParamExponent2
      integer aParamNormalize

      real    R_fracS, R_fracD, R_fracI, R_fracA ! Partition of Input between Surface, Detached, Interflow & Groundwater


      read*,lseg,lscen,I_Year1,I_Year2,I_YearC1,I_YearC2

      !>> ASSIGN partition of input loads to transport mechanism
      !   R_fracS + R_fracD + R_fracI + R_fracA = 1
      !   NOTE: This can be time variable as well based on flow regimes
      !   R_fracS & R_fracD : Linear Isotherm is a suitable choice as it falls
      !         between Freundlich and Langmuir Isotherm.
      R_fracS    = 0.25
      R_fracD    = 0.25
      R_fracI    = 0.25
      R_fracA    = 0.25

c      R_fracS    = 0.4
c      R_fracD    = 0.5
c      R_fracI    = 0.09
c      R_fracA    = 0.01

      !>> ASSIGN 5 Breakthrough Curve Parameters
      !   Read from CSV File btc_<lu>.csv
      !   A10001,S,sp1,sp2,sp3,sp4,sp5
      !   A10001,D,dp1,dp2,dp3,dp4,dp5
      sParamLeadEdge    = 1
      sParamPeakConc    = 1
      sParamTrailEdge   = 12
      sParamExponent1   = 0.1
      sParamExponent2   = 0.3
      sParamNormalize   = 1

      dParamLeadEdge    = 1
      dParamPeakConc    = 1
      dParamTrailEdge   = 12
      dParamExponent1   = 0.1
      dParamExponent2   = 0.2
      dParamNormalize   = 1

      iParamLeadEdge    = 2
      iParamPeakConc    = 2
      iParamTrailEdge   = 25
      iParamExponent1   = 0.1
      iParamExponent2   = 0.1
      iParamNormalize   = 1

      aParamLeadEdge    = 3
      aParamPeakConc    = 7
      aParamTrailEdge   = 36
      aParamExponent1   = 0.1
      aParamExponent2   = 0.08
      aParamNormalize   = 1

      !>> COMPUTE "characteristic" Breakthrouh Curve factors
      call BreakThroughFactors(
     .        sParamLeadEdge,sParamPeakConc,sParamTrailEdge,
     .        sParamExponent1,sParamExponent2,sParamNormalize,s1Factors)
      call BreakThroughFactors(
     .        dParamLeadEdge,dParamPeakConc,dParamTrailEdge,
     .        dParamExponent1,dParamExponent2,dParamNormalize,d1Factors)
      call BreakThroughFactors(
     .        iParamLeadEdge,iParamPeakConc,iParamTrailEdge,
     .        iParamExponent1,iParamExponent2,iParamNormalize,i1Factors)
      call BreakThroughFactors(
     .        aParamLeadEdge,aParamPeakConc,aParamTrailEdge,
     .        aParamExponent1,aParamExponent2,aParamNormalize,a1Factors)



      !>> READ "monthly" inputs from output/input/
      data R_MonApp /maxMonths*0/
c      R_MonApp(1) = 0
c      R_MonApp(2) = 0
c      R_MonApp(3) = 0
c      R_MonApp(4) = 0
c      R_MonApp(5) = 0
      R_MonApp(6) = 1
      R_MonApp(7) = 0
      R_MonApp(8) = 0


      call lencl(lseg,lenlseg)
      call lencl(lscen,lenlscen)

      !>> initialize WDM DSNs using perlnd file
      ! OutWDMs [ nquals ][ nstates ]
      do i = 1,nquals
      !{
         do j = 1,nstates
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

      do i = 1,nquals
      !{
         do j = 1,nstates
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
      print*,''
      print*,'PQUAL Land WDM Strcture (nquals,nstates) from iovar/'
     .        //geoscen(:lengeoscen)//'/perlnd'
      write(*,'(A4,4(A4$))') ' ',(states(i),i=1,nstates)
      write(*,*) ''
      do i=1,nquals
         print*,quals(i),(OutWDMs(i,j),j=1,nstates)
      end do


      !>> get PQUAL to Species Mapping
      fnam = 'inputs/quals_to_appspecies.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do i=1,nquals
      !{
100      read(dfile,'(a100)',err=992,end=114) line
         call d2x(line,last)
         if (comment(line)) goto 100
         if (line(:10).eq.'          ') goto 100
         read(line,*,end=993,err=993)
     .        (quals_to_appspecies(i,k),k=1,nappspecies)
      !}
      end do
114   close(dfile)

      print*,"Quals to AppSpecies:"
      do i=1,nquals
         print*,(quals_to_appspecies(i,k),k=1,nappspecies)
      end do


      !>> Get what app(lication) type inputs (flags) are available
      ! Using spec_flags.csv file at the moment that allows basin specific flags
      ! Alternatively "landuse_apptypes.csv" can be used as global descriptor

      call readcontrol_Lspecscen(
     I                           lscen,lenlscen,
     O                           specscen)

      print*,specscen            
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
            j = j + 1
            go to 201
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
             do k = 1,napptypes
             !{
                apptypesflag(i,k) = 0
             !}
             end do

             call lencl(C10_SpecFlags(i), I_tInt)
             do j = 1,I_tInt
             !{
               if(C10_SpecFlags(i)(j:j).eq.'Q')C10_SpecFlags(i)(j:j)='A'
c                print*,C10_SpecFlags(i)(j:j)
                do k = 1,napptypes
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
         print*,'nlu= ',i,', ',(apptypesflag(i,k),k=1,napptypes)
      end do
      do k = 1,napptypes
      !{
         if ( forceignore(k).eq.0 ) then
            do i = 1,nlu
               apptypesflag(i,k) = 0
            end do
         end if
      !}
      end do
      print*,'Forcing A, V=0, P=0, F, M, L, T, U=0, R=0'
      do i = 1,nlu
         print*,'nlu= ',i,', ',(apptypesflag(i,k),k=1,napptypes)
      end do
115   close(dfile)



      call readcontrol_Lcalscen(
     I                          lscen,lenlscen,
     O                          lcalib)
      call lencl(lcalib,lenlcalib)
      print*,'CALIBRATION: ',lcalib(:lenlcalib)


      !>> OPEN WDM
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+1,wdmfnam,0,err)
      if (err.ne.0) go to 801

      !>> INITIALIZE TARGETS
      !   IF lscen = lcalib
      !       READ TARGETS (S/surface & B/baseflow)
      !   ELSE
      !       Compute Surface & Baseflow exports over I_YearC1 to I_YearC2 in lcalib
      if (lscen(:lenlscen).eq.lcalib(:lenlcalib)) then
      !{
         print*,'TARGETS IN ***CALIBRATION*** MODE'
         do i=1,nquals
         !{
            !>> READ SURFACE TARGETS
c            fnam=calibdir//'target/g533/'//'S'//quals(i)//'_target.csv' TODO
            fnam=calibdir//'target/del/'//'S'//quals(i)//'_target.csv'
            print*,'STARGETS ',fnam
            found = .false.
            open(dfile,file=fnam,status='old',iostat=err)
            if (err.ne.0) go to 991
            read(dfile,'(a300)',err=992,end=116) longline ! header line
            call d2x(longline,last)
            read (longline,*,end=995,err=995)
     .                 C6_tlseg,(C3_tlus(j),j=1,nlu-NoLuTargets)
            do j = 1,nlu-NoLuTargets
            !{    
               k = 1
203            if ( C3_tlus(j).ne.luname(k) ) then
                  k = k + 1
                  if (k.gt.nlu+1) goto 996
                  go to 203
               end if
               I_tlus(j) = k
            !}
            end do

            do
            !{
               read(dfile,'(a300)',err=992,end=116) longline
               call d2x(longline,last)
               read(longline,*,end=995,err=995)
     .               C6_tlseg,(sTarget(I_tlus(j),i),j=1,nlu-NoLuTargets)
               if ( C6_tlseg.eq.lseg(:lenlseg) ) then
                  found = .true.
                  exit
               end if
            !}
            end do
116         close (dfile)
            if (found .eqv. .false.) goto 997
            print*,'STARGETS: ',lseg(:lenlseg),' '
            do j = 1,nlu
               print*,luname(j),sTarget(j,i)
            end do

            !>> READ BASEFLOW TARGETS
c            fnam=calibdir//'target/g533/'//'B'//quals(i)//'_target.csv' TODO
            fnam=calibdir//'target/del/'//'B'//quals(i)//'_target.csv'
            print*,'BTARGETS ',fnam
            found = .false.
            open(dfile,file=fnam,status='old',iostat=err)
            if (err.ne.0) go to 991
            read(dfile,'(a300)',err=992,end=117) longline ! header line
            call d2x(longline,last)
            read (longline,*,end=995,err=995)
     .                 C6_tlseg,(C3_tlus(j),j=1,nlu-NoLuTargets)
            do j = 1,nlu-NoLuTargets
            !{    
               k = 1
202            if ( C3_tlus(j).ne.luname(k) ) then
                  k = k + 1
                  if (k.gt.nlu+1) goto 996
                  go to 202
               end if
               I_tlus(j) = k
            !}
            end do

            do
            !{
               read(dfile,'(a300)',err=992,end=117) longline
               call d2x(longline,last)
               read(longline,*,end=995,err=995)
     .               C6_tlseg,(bTarget(I_tlus(j),i),j=1,nlu-NoLuTargets)
               if ( C6_tlseg.eq.lseg(:lenlseg) ) then
                  found = .true.
                  exit
               end if
            !}
            end do
117         close (dfile)
            if (found .eqv. .false.) goto 997
            print*,'BTARGETS: ',lseg(:lenlseg)
            do j = 1,nlu
               print*,luname(j),' ',bTarget(j,i)
            end do

         !}
         end do
      !}
      else
      !{
         !Open WDM File of lcalib
         print*,'TARGETS IN ***SCENARIO*** MODE'

         do i = 1,nlu
         !{
            if ( luname(i).eq.'wat' ) cycle
            if ( luname(i).ne.'hom' ) cycle

            wdmfnam = outwdmdir//'land/'//luname(i)//'/'//
     .        lcalib(:lenlcalib)//'/'//luname(i)//lseg(:lenlseg)//'.wdm'
            print*,'Land WDM File: ',wdmfnam
            call wdbopnlong(wdmfil,wdmfnam,0,err)
            if (err.ne.0) go to 801

            sdate(1) = I_YearC1
            sdate(2) = 1
            sdate(3) = 1
            sdate(4) = 0
            sdate(5) = 0
            sdate(6) = 0
              
            edate(1) = I_YearC2
            edate(2) = 12
            edate(3) = 31
            edate(4) = 24
            edate(5) = 0 
            edate(6) = 0

            do j = 1,nquals
            !{
               sTarget(i,j) = 0.0
               do k = 1,nstates
               !{

                  if ( OutWDMs(j,k).eq.999 ) cycle

                  call gethourdsn(wdmfil,sdate,edate,OutWDMs(j,k),
     .                                                   nvals,hDATA)

                  if ( states(k).ne.'A' ) then
                  !{
                     do l = 1,nvals
                        sTarget(i,j) = sTarget(i,j) + hDATA(l)
                     end do
                  !}
                  else
                  !{
                     do l = 1,nvals
                        bTarget(i,j) = bTarget(i,j) + hDATA(l)
                     end do
                  !}
                  end if
               !}
               end do

               sTarget(i,j) = sTarget(i,j) / (I_YearC2-I_YearC1+1)
               bTarget(i,j) = bTarget(i,j) / (I_YearC2-I_YearC1+1)

               print*,'CAL TARGET',' ',luname(i),' ',quals(j),' ',
     .                 sTarget(i,j),' ',bTarget(i,j)
            !}
            end do
                  
            call wdflcl(wdmfil,err)
            if (err.ne.0) go to 803

         !}
         end do

      !}
      end if

      !>> FOR each land-use { use .inc to get lu char key }
      do l = 1,nlu
      !{
         print*,''
         print*,"*** Land Use: "//luname(l)

         if ( luname(l).eq.'wat' ) cycle
         if ( luname(l).ne.'hom' ) cycle ! TODO DELETE


         !>> for calibration (1991-2000)
         ! compute inputs from each source and for each appspecies
         ! TODO IN_CALIB[ SOURCE ][ APPSPECIES ]
         ! TODO IN_CALIB_SRC[ SOURCE ]

         do i = 1,napptypes
         !{
            do j = 1,nappspecies
            !{
               do I_tYear = I_Year1,I_Year2
                  do I_tMonth = 1,12
                     inp_calib_app(i,j,I_tYear,I_tMonth) = 0
                     inp_scen_app(i,j,I_tYear,I_tMonth) = 0
                  end do
               end do
            !}
            end do
         !}
         end do

         do i = 1,napptypes
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
     .                I_tYear,I_tMonth,(R_tSpecies(j),j=1,nappspecies)
               if (I_tYear.ge.I_Year1.and.I_tYear.le.I_Year2) then
               !{
                  do j=1,nappspecies
                  !{
                     inp_calib_app(i,j,I_tYear,I_tMonth) = 
     .                         inp_calib_app(i,j,I_tYear,I_tMonth)
     .                         + R_tSpecies(j)
                  !}
                  end do
               !}
               end if
            !}
            end do
113         close (dfile)
         !}
         end do
         do i = 1,napptypes
         !{
            write(*,'(A6,5(A12))') 'apptypes',
     .                   (appspecies(j),j=1,nappspecies)
            do yy = I_Year1,I_Year2
             do mm = 1,12
                  write(*,'(A6,5(e12.5))')apptypeslong(i),
     .                      (inp_calib_app(i,j,yy,mm),j=1,nappspecies)
             end do
            end do
         !}
         end do

         if (lscen(:lenlscen).ne.lcalib(:lenlcalib)) then
         !{
            do i = 1,napptypes
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
     .                I_tYear,I_tMonth,(R_tSpecies(j),j=1,nappspecies)
                  if (I_tYear.ge.I_Year1.and.I_tYear.le.I_Year2) then
                  !{
                     do j=1,nappspecies
                     !{
                        inp_scen_app(i,j,I_tYear,I_tMonth) =
     .                            inp_scen_app(i,j,I_tYear,I_tMonth)
     .                            + R_tSpecies(j)
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
            do i = 1,napptypes
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
     .                C6_tlseg,(C3_tquals(j),j=1,nquals)
c               print*,(C3_tquals(j),j=1,nquals)

               do j = 1,nquals
               !{
                  k = 1
204               if ( C3_tquals(j).ne.quals(k) ) then
                     k = k + 1
                     go to 204
                  end if
                  I_tquals(j) = k
               !}
               end do
               print*,(quals(j),j=1,nquals),' :: ',
     .                       (I_tquals(j),j=1,nquals)

               do
               !{
                  read (dfile,'(a300)',end=992,err=119) longline
                  call d2x(longline,last)
                  read(longline,*,end=994,err=994),C6_tlseg,
     .                    (C20_sensitivity(j),j=1,nquals),
     .                    (C2_tsmethod(j),j=1,nquals)
c                  print*,C6_tlseg
c                  print*,(C20_sensitivity(j),j=1,nquals)

                  call lencl(C6_tlseg,I_lentlseg)
                  if( C6_tlseg(:I_lentlseg).eq.lseg(:lenlseg) ) then
                  !{
                     do j = 1,nquals
                     !{
                        read(C20_sensitivity(j)(:1),*,end=996,err=996),
     .                         I_sgroup( I_tquals(j) )
                        read(C20_sensitivity(j)(3:),*,end=996,err=996),
     .                         R_sensitivity( I_tquals(j) )
                        print*,(I_sgroup(k),k=1,nquals)
                        print*,(R_sensitivity(k),k=1,nquals)
c                        R_sensitivity( I_tquals(j) ) = C20_sensitivity(j)
                        C2_smethod ( I_tquals(j) ) = C2_tsmethod(j)
                     !}
                     end do
                     exit
                  !}
                  end if
               !}
               end do

               print*,'Sensitivites: ',lseg(:lenlseg)
               write(*,'(5(A4$))') (quals(j),j=1,nquals)
               print*,''
               print*,'Sen ',(R_sensitivity(j),j=1,nquals)
               print*,'Grp ',(I_sgroup(j),j=1,nquals)
               print*,'Mtd ',(C2_smethod(j),j=1,nquals)

119            close(dfile)


               do j = 1,nquals
                  R_groupTarget(j) = 0
               end do
               do j = 1,nquals
c                  R_qualsTarget(j) = sTarget(l,j) + bTarget(l,j)
                  k = I_sgroup(j)
                  R_groupTarget(k) = R_groupTarget(k) 
     .                                + sTarget(l,j) + bTarget(l,j)
               end do
               do j = 1,nquals
                 k = I_sgroup(j)
                 print*,'Grps ',j,k,R_groupTarget(k)
               end do

               do j = 1,nquals
               !{
                R_tSval = 0
                R_tBval = 0
c TODO                if (quals_to_appspecies(j,i) = 0) cycle
                do k = 1,nappspecies
                !{
                 do yy = I_YearC1,I_YearC2
                 !{
                  do mm = 1,12
                  !{
                    if (C2_smethod(j).eq.'D1') then
                    !{
c                  write(*,'(E12.5,I2,E12.5,E12.5,E12.5,E12.5,E12.5,I3)')
c     .     R_tSval,quals_to_appspecies(j,k),
c     .     inp_scen_app(i,k,yy,mm),inp_calib_app(i,k,yy,mm),
c     .     R_sensitivity(j),sTarget(l,j),R_groupTarget(I_sgroup(j)),
c     .     (I_YearC2 - I_YearC1 + 1)
                     R_tSval = R_tSval + quals_to_appspecies(j,k) *
     .                (inp_scen_app(i,k,yy,mm)-inp_calib_app(i,k,yy,mm))
     .                * R_sensitivity(j)
     .                * (sTarget(l,j)/R_groupTarget(I_sgroup(j)))
     .                / (I_YearC2 - I_YearC1 + 1)
                     R_tBval = R_tBval + quals_to_appspecies(j,k) *
     .                (inp_scen_app(i,k,yy,mm)-inp_calib_app(i,k,yy,mm))
     .                * R_sensitivity(j)
     .                * (bTarget(l,j)/R_groupTarget(I_sgroup(j)))
     .                / (I_YearC2 - I_YearC1 + 1)
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

                write(*,'(I2,I2,E12.5,E12.5$)') l,j,sTarget(l,j),R_tSval
                sTarget(l,j) = sTarget(l,j) + R_tSval
                write(*,'(E12.5)') sTarget(l,j)
                write(*,'(I2,I2,E12.5,E12.5$)') l,j,bTarget(l,j),R_tBval
                bTarget(l,j) = bTarget(l,j) + R_tBval
                write(*,'(E12.5)') bTarget(l,j)
               !}
               end do
            !}
            end do
         !}
         end if

         !>> OPEN WDM
c         wdmfnam = dummyWDMname
c         call wdbopnlong(wdmfil+1,wdmfnam,0,err)
c         if (err.ne.0) go to 801
            
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

         !>> READ SURO, WSSD, IFWO, AGWO
         call gethourdsn(wdmfil,sdate,edate,dsnSURO,nvals,hSURO)
         call gethourdsn(wdmfil,sdate,edate,dsnWSSD,nvals,hWSSD)
         call gethourdsn(wdmfil,sdate,edate,dsnIFWO,nvals,hIFWO)
         call gethourdsn(wdmfil,sdate,edate,dsnAGWO,nvals,hAGWO)

         R_tSURO = 0
         R_tWSSD = 0
         R_tIFWO = 0
         R_tAGWO = 0
         do i = 1,nvals
            R_tSURO = R_tSURO + hSURO(i)
            R_tWSSD = R_tWSSD + hWSSD(i)
            R_tIFWO = R_tIFWO + hIFWO(i)
            R_tAGWO = R_tAGWO + hAGWO(i)
         end do

         print*,'SURO (in/ac/yr) ', R_tSURO / (I_Year2-I_Year1+1)
         print*,'WSSD (tn/ac/yr) ', R_tWSSD / (I_Year2-I_Year1+1)
         print*,'IFWO (in/ac/yr) ', R_tIFWO / (I_Year2-I_Year1+1)
         print*,'AGWO (in/ac/yr) ', R_tAGWO / (I_Year2-I_Year1+1)


         nMonths = (I_Year2 - I_Year1 + 1) * 12

         do k = 1,nquals ! TODO FIX THIS --> k=1,nquals
         !{
            print*,'QUAL = ',k
c            write(*,'(A)') 'Break Through Curve'
c            do i = 1,nMonths
c               write(*,'(I2, E12.5, E12.5)'), i,s1Factors(i-1),s2Factors(i)
c            end do
            do yy = I_Year1,I_Year2
            !{
               do mm = 1,12
               !{
                  R_MonApp((yy-I_Year1)*12+mm) = 0.0
                  do i = 1,napptypes
                  !{
                     do j = 1,nappspecies !TODO FIXED ON 20140815
                     !{
c                        print*,'HERE1 ', quals_to_appspecies(k,j)
                        if (quals_to_appspecies(k,j) .eq. 0) cycle
c                        print*,'HERE2 ', quals_to_appspecies(k,j)

                        if (apptypeslong(i) .ne. 'uptake') then
                           R_MonApp((yy-I_Year1)*12+mm) = 
     .                         R_MonApp((yy-I_Year1)*12+mm)
     .                         + inp_calib_app(i,j,yy,mm)
                        else
                           R_MonApp((yy-I_Year1)*12+mm) =
     .                         R_MonApp((yy-I_Year1)*12+mm)
     .                         - inp_calib_app(i,j,yy,mm)
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
            R_AvgAnnApp = 0
            do i = 1,nMonths
c??               if ( R_MonApp(i) .lt. 0 ) R_MonApp(i) = 0
               R_AvgAnnApp = R_AvgAnnApp + R_MonApp(i)
               print*,i,R_MonApp(i)
            end do
            R_AvgAnnApp = R_AvgAnnApp / (nMonths/12)
            print*,'QUAL = ',k,' INPUT = ',R_AvgAnnApp

            do j = 1,nMonths
               s2Factors(j) = 0.0
               d2Factors(j) = 0.0
               i2Factors(j) = 0.0
               a2Factors(j) = 0.0
            end do 
            do i = 1,nMonths
            !{
               do j = i,nMonths
               !{
                  s2Factors(j) = s2Factors(j) 
     .                           + R_fracS*R_MonApp(i)*s1Factors(j-i+1)
     .                             * quals_to_states(k,1)
                  d2Factors(j) = d2Factors(j)
     .                           + R_fracD*R_MonApp(i)*d1Factors(j-i+1)
     .                             * quals_to_states(k,2)
                  i2Factors(j) = i2Factors(j)
     .                           + R_fracI*R_MonApp(i)*i1Factors(j-i+1)
     .                             * quals_to_states(k,3)
                  a2Factors(j) = a2Factors(j)
     .                           + R_fracA*R_MonApp(i)*a1Factors(j-i+1)
     .                             * quals_to_states(k,4)
               !}
               end do
            !}
            end do
            write(*,'(A)') 'Break Through Curve of Analyte'
            do i = 1,nMonths
            !{
               !TODO: Fix Negative Factors
               if ( s2Factors(i).lt.0 ) s2Factors(i) = 0
               if ( d2Factors(i).lt.0 ) d2Factors(i) = 0
               if ( i2Factors(i).lt.0 ) i2Factors(i) = 0
               if ( a2Factors(i).lt.0 ) a2Factors(i) = 0
c               write(*,'(I2, E12.5, E12.5)'), i,s1Factors(i-1),s2Factors(i)
c               write(*,'(I2,E12.5,E12.5,E12,5,E12.5)'),i,
c     .            s2Factors(i),d2Factors(i),i2Factors(i),a2Factors(i)
               print*,i,
     .              s2Factors(i),d2Factors(i),i2Factors(i),a2Factors(i)
            !}
            end do

            !>> SCALE WSSD values to SURO, TODO ?? Apply D vs. S weight
            do i = 1,nvals
c               hDCON(i) = (R_tSURO/R_tWSSD)*(R_fracD/R_fracS) * hWSSD(i)
               hDCON(i) = (R_tSURO/R_tWSSD) * hWSSD(i)
c               hDCON(i) = ( (R_tSURO+R_tIFWO) / R_tWSSD )
c     .                        * hWSSD(i)
            end do
            
            !>> COMPUTE YIELDS (Surface,S & Baseflow,B) = FLOW * CONC
            R_tSCON = 0
            R_tDCON = 0
            R_tICON = 0
            R_tACON = 0
            j = 1  ! ** count hours
            m = 1  ! ** count months
            do yy = I_Year1,I_Year2
               do mm = 1,12
                  do i=1,ndaysinmonth(yy,mm)*24
                     hSCON(j) = hSURO(j) * s2Factors(m)
                     hDCON(j) = hDCON(j) * d2Factors(m) !***
                     hICON(j) = hIFWO(j) * i2Factors(m)
                     hACON(j) = hAGWO(j) * a2Factors(m)
                     if( yy.ge.I_YearC1.and.yy.le.I_YearC2 ) then
                        R_tSCON = R_tSCON + hSCON(j)
                        R_tDCON = R_tDCON + hDCON(j)
                        R_tICON = R_tICON + hICON(j)
                        R_tACON = R_tACON + hACON(j)
                     end if

                     j = j + 1
                  end do
                  m = m + 1
               end do
            end do

            !>> READ TARGETS (S/surface & B/baseflow)
c            sTarget(l,1) = 1.58748 ! SNH3
c            bTarget(l,1) = 1.14879 ! BNH3

c            sTarget(l,2) = 8.00996
c            bTarget(l,2) = 24.2846

            !sTarget(1) = 1.08748 ! SNH3
            !bTarget(1) = 0.64879 ! BNH3
            print*,'STARGET ',luname(l),' ',quals(k),sTarget(l,k)
            print*,'BTARGET ',luname(l),' ',quals(k),bTarget(l,k)

            !>> Compute Scalaing Factor TARGET / YIELD
            R_Sfactor = sTarget(l,k) * (I_YearC2-I_YearC1+1)
     .                  / (R_tSCON + R_tDCON + R_tICON)
            R_Bfactor = bTarget(l,k) * (I_YearC2-I_YearC1+1)
     .                  / (R_tACON)

            write(*,'(/A F12.5)') 'Factor S = ',R_Sfactor
            write(*,'(A F12.5)' ) 'Factor B = ',R_Bfactor

            !>> ADJUST S/D/I/A-2 factors to get EOF Yields
            R_tSCON = 0
            R_tDCON = 0
            R_tICON = 0
            R_tACON = 0
            j = 1  ! ** count hours
            do yy = I_Year1,I_Year2
               do mm = 1,12
                  do i=1,ndaysinmonth(yy,mm)*24
                     hSCON(j) = hSCON(j) * R_Sfactor
                     hDCON(j) = hDCON(j) * R_Sfactor
                     hICON(j) = hICON(j) * R_Sfactor
                     hACON(j) = hACON(j) * R_Bfactor
                     if( yy.ge.I_YearC1.and.yy.le.I_YearC2 ) then
                        R_tSCON = R_tSCON + hSCON(j)
                        R_tDCON = R_tDCON + hDCON(j)
                        R_tICON = R_tICON + hICON(j)
                        R_tACON = R_tACON + hACON(j)
                     end if

                     j = j + 1
                  end do
               end do
            end do
            write(*,'(/A F10.5)') "I = ",R_AvgAnnApp
            write(*,'(/A F8.5)') "S = ",R_tSCON/(I_YearC2-I_YearC1+1)
            write(*,'(A F8.5)' ) "D = ",R_tDCON/(I_YearC2-I_YearC1+1)
            write(*,'(A F8.5)' ) "I = ",R_tICON/(I_YearC2-I_YearC1+1)
            write(*,'(A F8.5)' ) "A = ",R_tACON/(I_YearC2-I_YearC1+1)

            !>> COMPUTE monthly yields
            j = 1  ! ** count hours
            m = 1  ! ** count months
            do yy = I_Year1,I_Year2
               do mm = 1,12
                  mSCON(m) = 0.0
                  mDCON(m) = 0.0
                  mICON(m) = 0.0
                  mACON(m) = 0.0
                  do i=1,ndaysinmonth(yy,mm)*24
                     mSCON(m) = mSCON(m) + hSCON(j)
                     mDCON(m) = mDCON(m) + hDCON(j)
                     mICON(m) = mICON(m) + hICON(j)
                     mACON(m) = mACON(m) + hACON(j)
                     j = j + 1
                  end do
                  print*,
     .              m,R_MonApp(m),mSCON(m),mDCON(m),mICON(m),mACON(m)
                  m = m + 1
               end do
            end do

            !>> WRITE WDM

         !}
         end do

         !>> CLOSE WDM
         call wdflcl(wdmfil,err)
         if (err.ne.0) go to 803
      !}
      end do

      return


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
***** paramLeadEdge  = elapsed time to the LEADing EDGE
***** paramPeakConc  = elapsed time to the arrival of PEAK CONCentration
***** paramTrailEdge = elapsed time to the TRAILing EDGE of the Response Curve
***** paramExponent1 = rate of exponential increase to the peak concentration
***** paramExponent2 = rate of exponential decay  from the peak concentration

      subroutine BreakThroughFactors(
     I          paramLeadEdge,
     I          paramPeakConc,
     I          paramTrailEdge,
     I          paramExponent1,
     I          paramExponent2,
     I          paramNormalize,
     O          tFactors)
      !{

      implicit none
         include 'eofc.inc'

         integer paramLeadEdge
         integer paramPeakConc
         integer paramTrailEdge
         real    paramExponent1, paramExponent2
         integer paramNormalize

         integer tempI1
         real    tempR1

c         integer maxMonths
c         parameter ( maxMonths = 360 ) ! 30 Years x 12
         integer nMonths

c         real    tFactors(maxMonths)
c         data    tFactors /maxMonths*0/

         real    total
         data    total /1*0.0/


         ! PARAMETERS ASSIGN
c         paramLeadEdge    = 6
c         paramPeakConc    = 12
c         paramTrailEdge   = 24
c         paramExponent1   = 0.1
c         paramExponent2   = 0.2
c         paramNormalize   = 0


         write(*,'(A I4 I4 I4$)')
     .        'Param: ',paramLeadEdge,paramPeakConc,paramTrailEdge
         write(*,'(E12.5, E12.5$)') paramExponent1,paramExponent2
         write(*,'(I2)') paramNormalize

         do i = 1,maxMonths
            tFactors(i) = 0.0
         end do

c         nMonths = 60 ! TODO

c         if (paramTrailEdge .le. 0) paramTrailEdge = nMonths
         do i = 1,paramTrailEdge
         !{
            if      ( i .lt. paramLeadEdge ) then
                 tFactors(i) = 0
            else if ( i .lt. paramPeakConc ) then
                 tFactors(i) = EXP( paramExponent1 * (i-paramPeakConc) )
            else
                 tFactors(i) = EXP(-paramExponent2 * (i-paramPeakConc) )
            end if
            write(*,'(I2, E12.5)'), i,tFactors(i)
         !}
         end do

         if ( paramNormalize .eq. 1 ) then
         !{
            total = 0.0
            do i = 1,paramTrailEdge
            !{
               total = total + tFactors(i)
            !}
            end do
            write(*,'(A E12.5)') 'Total = ',total
         !}
         else
            total = 1
         end if

         tempR1 = 0.0
         tempI1 = 0
         do i = 1,paramTrailEdge
         !{
            tFactors(i) = tFactors(i)/total
            tempR1 = tempR1 + tFactors(i)
            if ( paramNormalize.eq.1
c     .          .and. tempR1.gt.0
     .          .and. tempR1.lt.0.5 ) then
               tempI1 = tempI1 + 1
            end if
            write(*,'(I2, E12.5)'), i,tFactors(i)
         !}
         end do
         tempI1 = tempI1 + 1
         if ( paramNormalize.eq.1 ) then
           ! http://www.epa.gov/ncea/pdfs/qtracer/qtracerch03.pdf Section 3.2.1
           write(*,'(A I2 A)'),'Mean Residence Time = ',tempI1,' months'
         end if

         return

      end
      !}

