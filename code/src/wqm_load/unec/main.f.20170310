
      implicit none

      include 'unec.inc'


      character*6        C6_tlseg
      integer            I_lentlseg,I_lenfnam
      integer            I_tlus(nlu)
      character*3        C3_tlus(nlu), C3_tquals(I_NQUALS)
      character*10       C10_SpecFlags(nlu)
      character*4        C4_tStr
      integer            I_tInt
      integer            I_tYear, I_tMonth, I_tDay
      integer            I_tquals(I_NQUALS)
      double precision   D_tDouble
      double precision   D_tSval,D_tBval
      double precision   D_tSpecies(I_NAPPSPECIES)
      double precision   D_tSURO,D_tSEDM,D_tIFWO,D_tAGWO
      double precision   D_Sfactor,D_Bfactor
      double precision   D_tSCON,D_tDCON,D_tICON,D_tACON
      double precision   D_AvgAnnApp
      logical            L_found

c      character*20       C_target
c      integer            lentarget

      integer            I_NOLUTARGETS
      parameter          (I_NOLUTARGETS = 0) ! wat afo cfo

      logical            comment
      external           comment

      logical            first
      
      integer            ndaysinmonth
      external           ndaysinmonth

      double precision   D_MonApp_Pve(I_MAXMONTHS), D_MonApp_Pve_Tot
      double precision   D_MonApp_Nve(I_MAXMONTHS), D_MonApp_Nve_Tot
      double precision   D_MonApp(I_MAXMONTHS), D_MonApp_Tot
      double precision   D_tMonApp
c      data D_MonApp /I_MAXMONTHS*0/

      integer            I_nMonths

      integer            I_ParamLeadEdge(nlu,I_NQUALS,I_NSTATES)
      integer            I_ParamPeakConc(nlu,I_NQUALS,I_NSTATES)
      integer            I_ParamTrailEdge(nlu,I_NQUALS,I_NSTATES)
      double precision   D_ParamExponent1(nlu,I_NQUALS,I_NSTATES)
      double precision   D_ParamExponent2(nlu,I_NQUALS,I_NSTATES)
      integer            I_ParamNormalize(nlu,I_NQUALS,I_NSTATES)

      integer            I_LagTimeSWX
      integer            I_LagTimeIFX
      integer            I_LagTimeSED
      integer            I_LagTimeAGX

c      integer            I_sParamLeadEdge
c      integer            I_sParamPeakConc
c      integer            I_sParamTrailEdge
c      double precision   D_sParamExponent1, D_sParamExponent2
c      integer            I_sParamNormalize

c      integer            I_dParamLeadEdge
c      integer            I_dParamPeakConc
c      integer            I_dParamTrailEdge
c      double precision   D_dParamExponent1, D_dParamExponent2
c      integer            I_dParamNormalize

c      integer            I_iParamLeadEdge
c      integer            I_iParamPeakConc
c      integer            I_iParamTrailEdge
c      double precision   D_iParamExponent1, D_iParamExponent2
c      integer            I_iParamNormalize

c      integer            I_aParamLeadEdge
c      integer            I_aParamPeakConc
c      integer            I_aParamTrailEdge
c      double precision   D_aParamExponent1, D_aParamExponent2
c      integer            I_aParamNormalize

      integer I_SPIN      ! Number of spin-up months

      logical L_rSAS(I_NQUALS,I_NSTATES)
      integer I_rSAS(I_NQUALS,I_NSTATES)

      double precision D_UPTAKE_FACTOR
      parameter ( D_UPTAKE_FACTOR = 1.0 ) ! ** this is a multiplier to uptake data
                                          ! setting it 0 will remote uptake from calculations
      logical L_SET_S2MIN_ZERO            ! ** wheather to set -ve s2 UNEC to 0 or positive
      parameter ( L_SET_S2MIN_ZERO = .false. ) ! default = false, set to positive, not 0
      double precision s2FactorMin,d2FactorMin,i2FactorMin,a2FactorMin

      character*3 tlu
      character*1 tapp
      character*3 tqual
      double precision twgt

      read*,lseg,lscen,I_SPIN,I_Year1,I_Year2,
c     .            C_target,
     .            I_YearC1,I_YearC2,
     .            I_WDM,I_DEBUG0
                 ! I_WDM = 1 => work on WDM in tmp directory
                 ! I_WDM = 0 => overwrite the WDM in land directory

      if (I_Year1.gt.I_YearC1 .or. I_Year2.lt.I_YearC2) goto 990

      !>> ASSIGN partition of input loads to transport mechanism
      !   D_fracS + D_fracD + D_fracI + D_fracA = 1
      !   NOTE: This can be time variable as well based on flow regimes
      !   D_fracS & D_fracD : Linear Isotherm is a suitable choice as it falls
      !         between Freundlich and Langmuir Isotherm.
      
      D_fracS(1)    = 0.45D0
      D_fracD(1)    = 0.10D0
      D_fracI(1)    = 0.25D0
      D_fracA(1)    = 0.25D0

      D_fracS(2)    = 0.15D0
      D_fracD(2)    = 0.05D0
      D_fracI(2)    = 0.15D0
      D_fracA(2)    = 0.65D0

      D_fracS(3)    = 0.25D0
      D_fracD(3)    = 0.25D0
      D_fracI(3)    = 0.25D0
      D_fracA(3)    = 0.25D0

      D_fracS(4)    = 0.25D0
      D_fracD(4)    = 0.20D0
      D_fracI(4)    = 0.25D0
      D_fracA(4)    = 0.25D0

      D_fracS(5)    = 0.15D0
      D_fracD(5)    = 0.65D0
      D_fracI(5)    = 0.15D0
      D_fracA(5)    = 0.05D0
! BHATT ??
      D_fracS(5)    = 0.05D0
      D_fracD(5)    = 0.75D0
      D_fracI(5)    = 0.01D0
      D_fracA(5)    = 0.05D0

      D_fracS(6)    = 0.15D0
      D_fracD(6)    = 0.65D0
      D_fracI(6)    = 0.15D0
      D_fracA(6)    = 0.05D0

      D_fracS(7)    = 0.15D0
      D_fracD(7)    = 0.65D0
      D_fracI(7)    = 0.15D0
      D_fracA(7)    = 0.05D0

      do i=1,I_NQUALS
         do j=1,I_NSTATES
            L_rSAS(i,j) = .false.
         end do
      end do
C      L_rSAS(2,4) = .true.
      

c      D_fracS    = 0.4
c      D_fracD    = 0.5
c      D_fracI    = 0.09
c      D_fracA    = 0.01


      call lencl(lseg,lenlseg)
      call lencl(lscen,lenlscen)

c      call lencl(C_target,lentarget)

c      C25_UNEC_PARAM        = lscen(:lenlscen) ! TODO Add a parameter table in control file and initialize
c      C25_SENSITIVITY = lscen(:lenlscen) ! TODO Add a parameter table in control file and initialize
      call readcontrol_Lunec_Table(lscen,lenlscen,'UNEC PARAM',10,
     .                             C25_UNEC_PARAM)
      call readcontrol_Lunec_Table(lscen,lenlscen,'UNEC PARAM',10,
     .                             C25_SENSITIVITY)
      call readcontrol_Lunec_Table(lscen,lenlscen,'UNEC TARGET',11,
     .                             C25_TARGET)

      call lencl(C25_UNEC_PARAM,lC25_UNEC_PARAM)
      call lencl(C25_SENSITIVITY,lC25_SENSITIVITY)
      call lencl(C25_TARGET,lC25_TARGET)

      !>> ASSIGN 5 Breakthrough Curve Parameters
      !   TODO Read from CSV File btc_<lu>.csv
      !   A10001,S,sp1,sp2,sp3,sp4,sp5
      !   A10001,D,dp1,dp2,dp3,dp4,dp5
      do i=1,nlu
      !{
         if ( luname(i).eq.'wat' ) cycle
c         if ( luname(i).ne.'ohy' ) cycle
         if ( luname(i).ne.'gom' ) cycle
c         if ( luname(i).ne.'trp' ) cycle
c         if ( luname(i).ne.'for' .and. luname(i).ne.'hom' ) cycle

         do j=1,I_NQUALS
         !{
            fnam   =pardir//'unec/'//C25_UNEC_PARAM(:lC25_UNEC_PARAM)//
     .               '/unec_'//luname(i)//'_'//quals(j)//'.csv'
            open(dfile,file=fnam,status='old',iostat=err)
            if (err.ne.0) then
               fnam=pardir//'unec/'//C25_UNEC_PARAM(:lC25_UNEC_PARAM)//
     .               '/unec_'//'default'//'_'//quals(j)//'.csv'
               open(dfile,file=fnam,status='old',iostat=err)
            end if
            if (err.ne.0) go to 991
            read (dfile,'(a300)',end=992,err=121) longline
            L_found = .false.
            do
            !{
               read (dfile,'(a300)',end=992,err=121) longline
               call d2x(longline,last)
               read(longline,*,end=994,err=994),C6_tlseg,
     .                I_ParamLeadEdge(i,j,1),
     .                I_ParamPeakConc(i,j,1),
     .                I_ParamTrailEdge(i,j,1),
     .                D_ParamExponent1(i,j,1),
     .                D_ParamExponent2(i,j,1),
     .                I_ParamNormalize(i,j,1),
     .                I_ParamLeadEdge(i,j,2),
     .                I_ParamPeakConc(i,j,2),           
     .                I_ParamTrailEdge(i,j,2),
     .                D_ParamExponent1(i,j,2),
     .                D_ParamExponent2(i,j,2),
     .                I_ParamNormalize(i,j,2),
     .                I_ParamLeadEdge(i,j,3),
     .                I_ParamPeakConc(i,j,3),           
     .                I_ParamTrailEdge(i,j,3),
     .                D_ParamExponent1(i,j,3),
     .                D_ParamExponent2(i,j,3),
     .                I_ParamNormalize(i,j,3),
     .                I_ParamLeadEdge(i,j,4),
     .                I_ParamPeakConc(i,j,4),           
     .                I_ParamTrailEdge(i,j,4),
     .                D_ParamExponent1(i,j,4),
     .                D_ParamExponent2(i,j,4),
     .                I_ParamNormalize(i,j,4)
c               print*,C6_tlseg
c               print*,(C20_sensitivity(j),j=1,I_NQUALS)

               call lencl(C6_tlseg,I_lentlseg)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,'|',C6_tlseg(:I_lentlseg),'|',lseg(:lenlseg),'|',
     .                luname(i)//"_"//quals(j)
               if( C6_tlseg(:I_lentlseg).eq.lseg(:lenlseg) ) then
               !{
                  L_found = .true.
                  exit
               !}
               end if
            !}
            end do
121         close(dfile)

c            do k=1,I_NSTATES
c            !{
c               call BreakThroughFactors(
c     .            I_ParamLeadEdge(i,j,k),
c     .            I_ParamPeakConc(i,j,k),
c     .            I_ParamTrailEdge(i,j,k),
c     .            D_ParamExponent1(i,j,k),
c     .            D_ParamExponent2(i,j,k),
c     .            I_ParamNormalize(i,j,k),
c     .            s1Factors(i,j,k) )
c            !}
c            end do
 
         !}
         end do
      !}
      end do

      !>> READ D_APP_WEIGHTS(LU,APP,QUAL)
      fnam = pardir//'unec/'//C25_UNEC_PARAM(:lC25_UNEC_PARAM)//
     .        '/apptypes_weights.csv'
      do i = 1,nlu
         do j = 1,I_NAPPTYPES
            do k = 1,I_NQUALS
               D_APP_WEIGHTS(i,j,k) = 1.0D0
            end do
         end do
      end do
      open(dfile,file=fnam,status='old',iostat=err)
      if ( err.ne.0 ) go to 991
      read (dfile,'(a300)',err=992) longline ! header
      do
      !{
         read(dfile,'(a300)',end=130) longline
         print*,'WEIGHTS ',longline
         call d2x(longline,last)
         read(longline,*,end=994,err=994) tlu,tapp,tqual,twgt
         do i = 1,nlu
         !{
            print*,'... ',tlu,' ',luname(i)
            if (tlu.eq.luname(i)) then
            !{
               print*,'WEIGHTS ',tlu
               do j = 1,I_NAPPTYPES
               !{
                  print*,'... ... ',tapp,' ',apptypes(j)
                  if (tapp.eq.apptypes(j)) then
                  !{
                     print*,'WEIGHTS ',tapp
                     do k = 1,I_NQUALS
                     !{
                        print*,'... ... ... ',tqual,' ',C_APP_WEIGHTS(k)
                        if (tqual.eq.C_APP_WEIGHTS(k)) then
                           print*, 'WEIGHTS ',tqual
                           D_APP_WEIGHTS(i,j,k) = twgt
                        end if
                     !}
                     end do
                     go to 131
                  !}
                  end if
               !}
               end do
            !}
            end if
         !}
         end do
131      continue
      !}
      end do
130   close(dfile)
      do i = 1,nlu
         do j = 1,I_NAPPTYPES
            do k = 1,I_NQUALS
               print*,luname(i),' ',apptypes(j),' ',quals(k),' ',
     .                 C_APP_WEIGHTS(k),' ',D_APP_WEIGHTS(i,j,k)
            end do
         end do
      end do
c      stop


      !>> COMPUTE "characteristic" Breakthrouh Curve factors
c      call BreakThroughFactors(
c     .        I_sParamLeadEdge,I_sParamPeakConc,I_sParamTrailEdge,
c     .        D_sParamExponent1,D_sParamExponent2,I_sParamNormalize,
c     .        s1Factors)
c      call BreakThroughFactors(
c     .        I_dParamLeadEdge,I_dParamPeakConc,I_dParamTrailEdge,
c     .        D_dParamExponent1,D_dParamExponent2,I_dParamNormalize,
c     .        d1Factors)
c      call BreakThroughFactors(
c     .        I_iParamLeadEdge,I_iParamPeakConc,I_iParamTrailEdge,
c     .        D_iParamExponent1,D_iParamExponent2,I_iParamNormalize,
c     .        i1Factors)
c      call BreakThroughFactors(
c     .        I_aParamLeadEdge,I_aParamPeakConc,I_aParamTrailEdge,
c     .        D_aParamExponent1,D_aParamExponent2,I_aParamNormalize,
c     .        a1Factors)



      !>> READ "monthly" inputs from output/input/
      data D_MonApp_Pve /I_MAXMONTHS*0.0D0/
      data D_MonApp_Nve /I_MAXMONTHS*0.0D0/
      data D_MonApp     /I_MAXMONTHS*0.0D0/
      data D_MonApp_Pve_Tot /0.0/
      data D_MonApp_Nve_Tot /0.0/
      data D_MonApp_Tot     /0.0/

c      D_MonApp(1) = 0.0D0
c      D_MonApp(2) = 0.0D0
c      D_MonApp(3) = 0.0D0
c      D_MonApp(4) = 0.0D0
c      D_MonApp(5) = 0.0D0
c      D_MonApp(6) = 1.0D0
c      D_MonApp(7) = 0.0D0
c      D_MonApp(8) = 0.0D0


c      call lencl(lseg,lenlseg)
c      call lencl(lscen,lenlscen)



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

      call readcontrol_Lioscen(
     I                          lscen,lenlscen,
     O                          ioscen)
      call lencl(ioscen,lenioscen)

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/perlnd' ! TODO Replace ioscen with iovars parameter

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
      if ( I_DEBUG0 .eq. 1 )
     . print*,'\n\n\n'
      if ( I_DEBUG0 .eq. 1 )
     . print*,'PQUAL Land WDM Strcture (I_NQUALS,I_NSTATES) from iovar/'
     .        //ioscen(:lenioscen)//'/perlnd'
      if ( I_DEBUG0 .eq. 1 )
     . write(*,'(A4,4(A4$))') ' ',(states(i),i=1,I_NSTATES)
      if ( I_DEBUG0 .eq. 1 )
     . write(*,*) ''
      do i=1,I_NQUALS
         if ( I_DEBUG0 .eq. 1 )
     .    print*,quals(i),(OutWDMs(i,j),j=1,I_NSTATES)
      end do

      call FlowDSN(fnam,flow(1),dsnSURO)
      call FlowDSN(fnam,flow(2),dsnSEDM)
      call FlowDSN(fnam,flow(3),dsnIFWO)
      call FlowDSN(fnam,flow(4),dsnAGWO)

      if ( I_DEBUG0 .eq. 1 )
     . print*,'FLOWDSNs ',dsnSURO,dsnSEDM,dsnIFWO,dsnAGWO

      !>> get PQUAL to Species Mapping
      fnam = pardir//'unec/'//C25_UNEC_PARAM(:lC25_UNEC_PARAM)//
     .          '/quals_to_appspecies.dat'
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

      if ( I_DEBUG0 .eq. 1 )
     . print*,"\n\nQuals to AppSpecies:"
      do i=1,I_NQUALS
         if ( I_DEBUG0 .eq. 1 )
     .    print*,(quals_to_appspecies(i,k),k=1,I_NAPPSPECIES)
      end do


      
      !>> get rSAS switches
      fnam = pardir//'unec/'//C25_UNEC_PARAM(:lC25_UNEC_PARAM)//
     .        '/rSAS_quals.dat'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      if ( I_DEBUG0 .eq. 1 )
     . print*, "\n\n*** rSAS Switches ***"
      if ( I_DEBUG0 .eq. 1 )
     . print*,'Quals ',(states(i),i=1,I_NSTATES)
      do i=1,I_NQUALS
      !{
150      read(dfile,'(a100)',err=992,end=122) line
         call d2x(line,last)
         if (comment(line)) goto 150
         if (line(:10).eq.'          ') goto 150
         read(line,*,end=993,err=993)
     .        (I_rSAS(i,k),k=1,I_NSTATES)
         do k=1,I_NSTATES
            if (I_rSAS(i,k) .eq. 1) then
               L_rSAS(i,k) = .true.
            end if
         end do
         if ( I_DEBUG0 .eq. 1 )
     .    print*,quals(i),(L_rSAS(i,k),k=1,I_NSTATES)
      !}
      end do
122   close(dfile)


      !>> Get what app(lication) type inputs (flags) are available
      ! Using spec_flags.csv file at the moment that allows basin specific flags
      ! Alternatively "landuse_apptypes.csv" can be used as global descriptor

      call readcontrol_Lspecscen(
     I                           lscen,lenlscen,
     O                           specscen)

      if ( I_DEBUG0 .eq. 1 )
     . print*,'\n\n',specscen            
      call lencl(specscen,lenspecscen)
      if ( I_DEBUG0 .eq. 1 )
     . print*,specscen(:lenspecscen)

      fnam = ScenDatDir//'land/spec_flags/'//specscen(:lenspecscen)//
     .       '/spec_flags.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
                     
      read(dfile,'(a300)',err=992,end=115)longline
      call d2x(longline,last)
      read (longline,*,end=994,err=994) C6_tlseg,(C3_tlus(i),i=1,nlu-1)
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
      if ( I_DEBUG0 .eq. 1 )
     . print*,'spec_flags lus: ',(I_tlus(i),i=1,nlu)
      
         
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
                if ( I_tlus(i) .eq. 0 ) cycle 
                apptypesflag(I_tlus(i),k) = 0
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
         if ( I_DEBUG0 .eq. 1 )
     .    print*,'nlu= ',i,', ',(apptypesflag(i,k),k=1,I_NAPPTYPES)
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
      if ( I_DEBUG0 .eq. 1 )
     . print*,'\nForcing A, V=0, P=0, F, M, L, T, U=0, R=0'
      do i = 1,nlu
         if ( I_DEBUG0 .eq. 1 )
     .    print*,'nlu= ',i,', ',luname(i),', ',
     .       (apptypesflag(i,k),k=1,I_NAPPTYPES)
      end do
115   close(dfile)



      call readcontrol_Lcalscen(
     I                          lscen,lenlscen,
     O                          lcalib)
      call lencl(lcalib,lenlcalib)
      if ( I_DEBUG0 .eq. 1 )
     . print*,'CALIBRATION: ',lcalib(:lenlcalib)



      !>> OPEN Dummy WDM
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+1,wdmfnam,0,err)
      if (err.ne.0) go to 801

      !>> INITIALIZE TARGETS
      !   IF lscen = lcalib
      !       READ TARGETS (S/surface & B/baseflow)
      !   ELSE
      !       Compute Surface & Baseflow exports over I_YearC1 to I_YearC2 in lcalib
      if ( I_DEBUG0 .eq. 1 )
     . print*,'SCENARIO:    ',lscen(:lenlscen)
      if ( I_DEBUG0 .eq. 1 )
     . print*,'CALIBRATION: ',lcalib(:lenlcalib)
      if (lscen(:lenlscen).eq.lcalib(:lenlcalib)) then
      !{
         if ( I_DEBUG0 .eq. 1 )
     .    print*,'TARGETS IN ***CALIBRATION*** MODE'
         do i=1,I_NQUALS
         !{
            !>> READ SURFACE TARGETS
c            fnam=calibdir//'target/g533/'//'S'//quals(i)//'_target.csv' TODO
            fnam=calibdir//'target/'//C25_TARGET(:lC25_TARGET)//'/'
     .           //'S'//quals(i)//'.csv' ! TODO Fix me
            if ( I_DEBUG0 .eq. 1 )
     .       print*,'STARGETS ',fnam
            L_found = .false.
            open(dfile,file=fnam,status='old',iostat=err)
            if (err.ne.0) go to 991
            read(dfile,'(a500)',err=992,end=116) longline ! header line
            call d2x(longline,last)
            read (longline,*,end=994,err=994)
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
               read(dfile,'(a1000)',err=992,end=116) longline
               call d2x(longline,last)
               read(longline,*,end=994,err=994)
     .           C6_tlseg,(D_sTarget(I_tlus(j),i),j=1,nlu-I_NOLUTARGETS)
               if ( C6_tlseg.eq.lseg(:lenlseg) ) then
                  L_found = .true.
                  exit
               end if
            !}
            end do
116         close (dfile)

            if (L_found .eqv. .false.) goto 997
            if ( I_DEBUG0 .eq. 1 )
     .       print*,'STARGETS: ',lseg(:lenlseg),' '
            do j = 1,nlu
               if ( I_DEBUG0 .eq. 1 )
     .          print*,luname(j),D_sTarget(j,i)
            end do



            !>> READ BASEFLOW TARGETS
c            fnam=calibdir//'target/g533/'//'B'//quals(i)//'_target.csv' TODO 
            fnam=calibdir//'target/'//C25_TARGET(:lC25_TARGET)//'/'
     .            //'B'//quals(i)//'.csv' ! TODO Fix me
            if ( I_DEBUG0 .eq. 1 )
     .       print*,'BTARGETS ',fnam
            L_found = .false.
            open(dfile,file=fnam,status='old',iostat=err)
            if (err.ne.0) go to 991
            read(dfile,'(a300)',err=992,end=117) longline ! header line
            call d2x(longline,last)
            read (longline,*,end=994,err=994)
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
               read(dfile,'(a1000)',err=992,end=117) longline
               call d2x(longline,last)
               read(longline,*,end=994,err=994)
     .           C6_tlseg,(D_bTarget(I_tlus(j),i),j=1,nlu-I_NOLUTARGETS)
               if ( C6_tlseg.eq.lseg(:lenlseg) ) then
                  L_found = .true.
                  exit
               end if
            !}
            end do
117         close (dfile)

            if (L_found .eqv. .false.) goto 997
            if ( I_DEBUG0 .eq. 1 )
     .       print*,'BTARGETS: ',lseg(:lenlseg)
            do j = 1,nlu
               if ( I_DEBUG0 .eq. 1 )
     .          print*,luname(j),' ',D_bTarget(j,i)
            end do

         !}
         end do
         do i=1,nlu
           do j=1,I_NQUALS
             if ( I_DEBUG0 .eq. 1 )
     .        print*,'ORG TARGET',' ',luname(i),' ',quals(j),' ',
     .                 D_sTarget(i,j),' ',D_bTarget(i,j)
           end do
         end do
      !}
      else
      !{
         !Open Monthly Export File of CALIBRATION (lcalib)
         if ( I_DEBUG0 .eq. 1 )
     .    print*,'TARGETS IN ***SCENARIO*** MODE'

         do i = 1,nlu
         !{
            if ( luname(i).eq.'wat' ) cycle
c            if ( luname(i).ne.'ohy' ) cycle
            if ( luname(i).ne.'gom' ) cycle
c         if ( luname(i).ne.'trp' ) cycle
c            if ( luname(i).ne.'for' .and. luname(i).ne.'hom' ) cycle

            do j = 1,I_NQUALS
            !{
               fnam = outdir//'eof/monthly/'//lcalib(:lenlcalib)//'/'//
     .          luname(i)//'_'//lseg(:lenlseg)//'.'//quals(j)
               call lencl(fnam,I_lenfnam)
               if ( I_DEBUG0 .eq. 1 )
     .          write(*,'(3(A$))'),' Reading ',fnam(:I_lenfnam),' ...'
               if ( I_DEBUG0 .eq. 1 )
     .          print*,''
               open(dfile,file=fnam,status='unknown',iostat=err)
               if (err.ne.0) go to 991

               read(dfile,'(a100)',err=992,end=120) line ! header line

               D_sTarget(i,j) = 0.0D0
               D_bTarget(i,j) = 0.0D0
               
               do
               !{
                  read(dfile,'(a300)',err=992,end=120) longline
                  call d2x(longline,last)
                  read(longline,*,end=994,err=994)
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

               if ( I_DEBUG0 .eq. 1 )
     .          print*,'CAL TARGET',' ',luname(i),' ',quals(j),' ',
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
         if ( I_DEBUG0 .eq. 1 )
     .    print*,''
         if ( I_DEBUG0 .eq. 1 )
     .    print*,"*** Land Use: "//luname(l)//" ***"

         if ( luname(l).eq.'wat' ) cycle
c         if ( luname(l).ne.'ohy' ) cycle ! TODO DELETE
         if ( luname(l).ne.'gom' ) cycle
c         if ( luname(l).ne.'trp' ) cycle
c         if ( luname(l).ne.'for' .and. luname(l).ne.'hom' ) cycle

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
                   D_inp_calib_app(i,j,I_tYear,I_tMonth) = 0.00000001D0
                   D_inp_scen_app(i,j,I_tYear,I_tMonth)  = 0.00000001D0
                  !}
                  end do
               end do
            !}
            end do
         !}
         end do

         do i = 1,I_NAPPTYPES
         !{
            if (apptypesflag(l,i) .eq. 0) cycle

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
            if ( I_DEBUG0 .eq. 1 )
     .        write(*,'(A9,5(A12))') 'apptypes ',
     .                   (appspecies(j),j=1,I_NAPPSPECIES)
            do yy = I_Year1,I_Year2
             do mm = 1,12
                  if ( I_DEBUG0 .eq. 1 )
     .             write(*,'(A9,5(e12.5))')apptypeslong(i),
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
               if (apptypesflag(l,i) .eq. 0) cycle

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
               if (apptypesflag(l,i) .eq. 0) cycle

               call lencl(apptypeslong(i),I_tInt)
               fnam = 
     .          pardir//'unec/'//C25_SENSITIVITY(:lC25_SENSITIVITY)//'/'
     .                //'sensitivity_'//luname(l)//'_'//
     .                apptypeslong(i)(:I_tInt)//'.csv'
               call lencl(fnam,I_lenfnam)
               if ( I_DEBUG0 .eq. 1 )
     .          write(*,'(3(A$))'),' Reading ',fnam(:I_lenfnam),' ...'
               if ( I_DEBUG0 .eq. 1 )
     .          print*,''
               open(dfile,file=fnam,status='old',iostat=err)
               if (err.ne.0) then
                  fnam = 
     .          pardir//'unec/'//C25_SENSITIVITY(:lC25_SENSITIVITY)//'/'
     .                //'sensitivity_'//luname(l)//'_'//
     .                'default'//'.csv'
                  call lencl(fnam,I_lenfnam)
                  open(dfile,file=fnam,status='old',iostat=err)
               end if
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
               if ( I_DEBUG0 .eq. 1 )
     .          print*,(quals(j),j=1,I_NQUALS),' :: ',
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
                        if ( I_DEBUG0 .eq. 1 )
     .                   print*,(I_sgroup(k),k=1,I_NQUALS)
                        if ( I_DEBUG0 .eq. 1 )
     .                   print*,(D_sensitivity(k),k=1,I_NQUALS)
c                        D_sensitivity( I_tquals(j) ) = C20_sensitivity(j)
                        C2_smethod ( I_tquals(j) ) = C2_tsmethod(j)
                     !}
                     end do
                     exit
                  !}
                  end if
               !}
               end do

               if ( I_DEBUG0 .eq. 1 )
     .          print*,'Sensitivites: ',lseg(:lenlseg)
               if ( I_DEBUG0 .eq. 1 )
     .          write(*,'(5(A4$))') (quals(j),j=1,I_NQUALS)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,''
               if ( I_DEBUG0 .eq. 1 )
     .          print*,'Sensitivity ',(D_sensitivity(j),j=1,I_NQUALS)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,'Groupings   ',(I_sgroup(j),j=1,I_NQUALS)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,'Methodology ',(C2_smethod(j),j=1,I_NQUALS)

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
                 if ( I_DEBUG0 .eq. 1 )
     .            print*,'Group Targets ',j,k,D_groupTarget(k)
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
                      if ( D_groupTarget(I_sgroup(j)) .ne. 0 ) then
                      !{
                        D_tSval = D_tSval + quals_to_appspecies(j,k) *
     .                   ( D_inp_scen_app(i,k,yy,mm) 
     .                      - D_inp_calib_app(i,k,yy,mm) )
     .                   * D_sensitivity(j)
     .                   * (D_sTarget(l,j)/D_groupTarget(I_sgroup(j)))
     .                   / (I_YearC2 - I_YearC1 + 1)
                        D_tBval = D_tBval + quals_to_appspecies(j,k) *
     .                   ( D_inp_scen_app(i,k,yy,mm) 
     .                      - D_inp_calib_app(i,k,yy,mm) )
     .                   * D_sensitivity(j)
     .                   * (D_bTarget(l,j)/D_groupTarget(I_sgroup(j)))
     .                   / (I_YearC2 - I_YearC1 + 1)
                      !}
                      end if
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

                if ( I_DEBUG0 .eq. 1 )
     .           write(*,'(I2,I2,E12.5,E12.5$)') 
     .                    l,j,D_sTarget(l,j),D_tSval
                if (D_sTarget(l,j)+D_tSval<D_sTarget(l,j)*0.4999) then!TODO Discuss this
                   print*,'WARNING: ',D_tSval,D_sTarget(l,j)
                   D_tSval = -0.5 * D_sTarget(l,j)
                end if
                D_sTarget(l,j) = D_sTarget(l,j) + D_tSval

                if ( I_DEBUG0 .eq. 1 )
     .           write(*,'(E12.5)') D_sTarget(l,j)
                if ( I_DEBUG0 .eq. 1 )
     .           write(*,'(I2,I2,E12.5,E12.5$)') 
     .                    l,j,D_bTarget(l,j),D_tBval
                if (D_bTarget(l,j)+D_tBval<D_bTarget(l,j)*0.4999) then!TODO Discuss this
                   print*,'WARNING: ',D_tBval,D_bTarget(l,j)
                   D_tBval = -0.5 * D_bTarget(l,j)
                end if
                D_bTarget(l,j) = D_bTarget(l,j) + D_tBval
                if ( I_DEBUG0 .eq. 1 )
     .           write(*,'(E12.5)') D_bTarget(l,j)
               !}
               end do
            !}
            end do
         !}
         end if

         !>> OPEN SCENARIO WDM (w/ PWATER & SEDMNT)

         if ( I_WDM .eq. 1) then
            wdmfnam = luname(l)//lseg(:lenlseg)//'.wdm'
         else
            wdmfnam = outwdmdir//'/land/'//luname(l)//'/'//
     .         lscen(:lenlscen)//'/'//luname(l)//lseg(:lenlseg)//'.wdm'
         end if
         if ( I_DEBUG0 .eq. 1 )
     .    print*,'Land WDM File: ',wdmfnam
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
         j = 1  ! ** count hours
         m = 1  ! ** count months
         do yy = I_Year1,I_Year2
            do mm = 1,12
               D_mSURO(m) = 0.0D0
               D_mSEDM(m) = 0.0D0
               D_mIFWO(m) = 0.0D0
               D_mAGWO(m) = 0.0D0
               D_mFLOW(m) = 0.0D0
               do i=1,ndaysinmonth(yy,mm)*24
                     
                  D_mSURO(m) = D_mSURO(m) + F_hSURO(j)
                  D_mSEDM(m) = D_mSEDM(m) + F_hSEDM(j)
                  D_mIFWO(m) = D_mIFWO(m) + F_hIFWO(j)
                  D_mAGWO(m) = D_mAGWO(m) + F_hAGWO(j)
                  D_mFLOW(m) = D_mFLOW(m) + 
     .                         F_hSURO(j) + F_hIFWO(j) + F_hAGWO(j)

                  D_tSURO = D_tSURO + F_hSURO(j)
                  D_tSEDM = D_tSEDM + F_hSEDM(j)
                  D_tIFWO = D_tIFWO + F_hIFWO(j)
                  D_tAGWO = D_tAGWO + F_hAGWO(j)
                  j = j + 1
               end do
               m = m + 1
            end do
         end do

         print*,'SURO (in/ac/yr) ', D_tSURO / (I_Year2-I_Year1+1)
         print*,'SEDM (tn/ac/yr) ', D_tSEDM / (I_Year2-I_Year1+1)
         print*,'IFWO (in/ac/yr) ', D_tIFWO / (I_Year2-I_Year1+1)
         print*,'AGWO (in/ac/yr) ', D_tAGWO / (I_Year2-I_Year1+1)


         I_nMonths = (I_Year2 - I_Year1 + 1) * 12

         do k = 1,I_NQUALS ! TODO FIX THIS --> k=1,I_NQUALS DONE!
         !{
            if ( I_DEBUG0 .eq. 1 )
     .       print*,'\n\nQUAL = ',k
c            write(*,'(A)') 'Break Through Curve'
c            do i = 1,I_nMonths
c               write(*,'(I2, E12.5, E12.5)'), i,s1Factors(i-1),s2Factors(i)
c            end do
            D_MonApp_Pve_Tot = 0.0D0
            D_MonApp_Nve_Tot = 0.0D0
            D_MonApp_Tot     = 0.0D0
            do yy = I_Year1,I_Year2
            !{
               do mm = 1,12
               !{
                  D_MonApp_Pve((yy-I_Year1)*12+mm) = 0.0D0
                  D_MonApp_Nve((yy-I_Year1)*12+mm) = 0.0D0
                  D_MonApp((yy-I_Year1)*12+mm)     = 0.0D0
                  do i = 1,I_NAPPTYPES
                  !{
                     do j = 1,I_NAPPSPECIES !TODO FIXED ON 20140815
                     !{
c                        print*,'HERE1 ', quals_to_appspecies(k,j)
                        if (quals_to_appspecies(k,j) .eq. 0) cycle
c                        print*,'HERE2 ', quals_to_appspecies(k,j)

                        if ( L_APP_WEIGHTS .eqv. .false. ) then
                        !{
                          if (apptypeslong(i) .ne. 'uptake') then
                          !{
                             D_tMonApp = D_inp_calib_app(i,j,yy,mm)
                             D_MonApp_Pve((yy-I_Year1)*12+mm) = 
     .                           D_MonApp_Pve((yy-I_Year1)*12+mm)
     .                           + D_tMonApp
                             D_MonApp_Pve_Tot = D_MonApp_Pve_Tot
     .                           + D_tMonApp
                          !}
                          else
                          !{
                             print*,'BHATT',apptypes(i),' ',
     .                          apptypeslong(i)
                             D_tMonApp = - ( D_inp_calib_app(i,j,yy,mm) 
     .                           * D_UPTAKE_FACTOR )
                             D_MonApp_Nve((yy-I_Year1)*12+mm) =
     .                           D_MonApp_Nve((yy-I_Year1)*12+mm)
     .                           + D_tMonApp
                             D_MonApp_Nve_Tot = D_MonApp_Nve_Tot
     .                           + D_tMonApp
                          !}
                          end if
                          D_MonApp_Tot = D_MonApp_Tot + D_tMonApp
                        !}
                        else
                        !{
                           print*,'BHATT',' ',apptypes(i),' ',
     .                        D_APP_WEIGHTS(l,i,k)
                           D_tMonApp = + D_inp_calib_app(i,j,yy,mm)
     .                          * D_APP_WEIGHTS(l,i,k)
                           if ( D_APP_WEIGHTS(l,i,k) .gt. 0 ) then
c                             D_tMonApp = + D_inp_calib_app(i,j,yy,mm)
c     .                          * D_APP_WEIGHTS(l,i,k)
                             D_MonApp_Pve((yy-I_Year1)*12+mm) = 
     .                          D_MonApp_Pve((yy-I_Year1)*12+mm)
     .                          + D_tMonApp
                             D_MonApp_Pve_Tot = D_MonApp_Pve_Tot
     .                          + D_tMonApp
                           else
c                             D_tMonApp = + D_inp_calib_app(i,j,yy,mm)
c     .                          * D_APP_WEIGHTS(l,i,k)
                             D_MonApp_Nve((yy-I_Year1)*12+mm) =
     .                          D_MonApp_Nve((yy-I_Year1)*12+mm)
     .                          + D_tMonApp
                             D_MonApp_Nve_Tot = D_MonApp_Nve_Tot
     .                          + D_tMonApp
                           end if
                           D_MonApp_Tot = D_MonApp_Tot + D_tMonApp
                        !}
                        end if

                     !}
                     end do
                  !}
                  end do
               !}
               end do
            !}
            end do

            if ( L_APP_BALANCE .eqv. .false. ) then
              D_MonApp_Bal_Fac = 1.0D0
            else
              if ( (D_MonApp_Pve_Tot + D_MonApp_Nve_Tot) .lt. 0.0 ) then
                D_MonApp_Bal_Fac = - D_MonApp_Pve_Tot / D_MonApp_Nve_Tot
              else
                D_MonApp_Bal_Fac = 1.0D0
              end if
            end if
            do yy = I_Year1,I_Year2
            !{
              do mm = 1,12
              !{
                D_MonApp((yy-I_Year1)*12+mm) =
     .            D_MonApp_Pve((yy-I_Year1)*12+mm) + 
     .            D_MonApp_Nve((yy-I_Year1)*12+mm) * D_MonApp_Bal_Fac
              !}
              end do
            !}
            end do

            if ( I_DEBUG0 .eq. 1 )
     .       print*,"Total Monthly Application/Input"
            !>> FIX NEGATIVE INPUT TODO NOTE: DO *NOT* FIX NEGATIVE INPUTS HERE. FIXING NEGATIVE INPUTS WHEN THERE IS NO POSITIVE INPUT ON THAT GIVEN MONTH BUT THERE WERE INPUTS BEFORE THAT. E.g. +40 -10 -10 -10 -10
            D_AvgAnnApp = 0
            do i = 1,I_nMonths
c??               if ( D_MonApp(i) .lt. 0 ) D_MonApp(i) = 0
               D_AvgAnnApp = D_AvgAnnApp + D_MonApp(i)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,i,D_MonApp(i),D_MonApp_Pve(i),D_MonApp_Nve(i)
            end do
            D_AvgAnnApp = D_AvgAnnApp / (I_nMonths/12)
            if ( I_DEBUG0 .eq. 1 )
c     .       print*,
     .       write(*,'(A,A,A,I1,A,F10.3,F10.3,F10.3,F10.3,A,L1,A,L1)')
     .         'LU = ',luname(l),' QUAL = ',k,' INPUT = ',
     .         D_AvgAnnApp,
     .         D_MonApp_Tot/(I_nMonths/12),
     .         D_MonApp_Pve_Tot/(I_nMonths/12),
     .         D_MonApp_Nve_Tot/(I_nMonths/12),
     .         ' ',L_APP_BALANCE,
     .         ' ',L_APP_WEIGHTS

            call BreakThroughFactors(
     .            I_ParamLeadEdge(l,k,1),
     .            I_ParamPeakConc(l,k,1),
     .            I_ParamTrailEdge(l,k,1),
     .            D_ParamExponent1(l,k,1),
     .            D_ParamExponent2(l,k,1),
     .            I_ParamNormalize(l,k,1),
     .            I_DEBUG0,
     .            s1Factors, I_LagTimeSWX )

            call BreakThroughFactors(
     .            I_ParamLeadEdge(l,k,2),
     .            I_ParamPeakConc(l,k,2),
     .            I_ParamTrailEdge(l,k,2),
     .            D_ParamExponent1(l,k,2),
     .            D_ParamExponent2(l,k,2),
     .            I_ParamNormalize(l,k,2),
     .            I_DEBUG0,
     .            d1Factors, I_LagTimeSED )

            call BreakThroughFactors(
     .            I_ParamLeadEdge(l,k,3),
     .            I_ParamPeakConc(l,k,3),
     .            I_ParamTrailEdge(l,k,3),
     .            D_ParamExponent1(l,k,3),
     .            D_ParamExponent2(l,k,3),
     .            I_ParamNormalize(l,k,3),
     .            I_DEBUG0,
     .            i1Factors, I_LagTimeIFX )

            call BreakThroughFactors(
     .            I_ParamLeadEdge(l,k,4),
     .            I_ParamPeakConc(l,k,4),
     .            I_ParamTrailEdge(l,k,4),
     .            D_ParamExponent1(l,k,4),
     .            D_ParamExponent2(l,k,4),
     .            I_ParamNormalize(l,k,4),
     .            I_DEBUG0,
     .            a1Factors, I_LagTimeAGX )

            print*,'Mean Residence Time,',
     .             lseg(:lenlseg),',',luname(l),',',
     .             I_LagTimeSWX,',',I_LagTimeIFX,',',
     .             I_LagTimeSED,',',I_LagTimeAGX

C            return

            do j = 1,I_nMonths
               s2Factors(j) = 0.0D0
               d2Factors(j) = 0.0D0
               i2Factors(j) = 0.0D0
               a2Factors(j) = 0.0D0
            end do 
            !! >> SPIN-UP START
            ! I_SPIN = 0        ! YEARS
            ! I_SPIN = I_SPIN * 12 ! MONTHS
            do i = 1,I_SPIN
            !{
               if ( I_DEBUG0 .eq. 1) print*,'SPIN ',i
               do j = I_SPIN,I_nMonths+I_SPIN-1 ! On 20160730 BHATT changed I_nMonths to I_nMonths+I_SPIN-1
               !{
c                  print*,j-I_SPIN+1,MOD(i-1,12)+1,j-i+1,
c     .               D_MonApp(MOD(i-1,12)+1)
                  s2Factors(j-I_SPIN+1) = s2Factors(j-I_SPIN+1)
     .             + D_fracS(k)*D_MonApp(MOD(i-1,12)+1)*s1Factors(j-i+1)
     .                           * quals_to_states(k,1)
                  d2Factors(j-I_SPIN+1) = d2Factors(j-I_SPIN+1)
     .             + D_fracD(k)*D_MonApp(MOD(i-1,12)+1)*d1Factors(j-i+1)
     .                           * quals_to_states(k,2)
                  i2Factors(j-I_SPIN+1) = i2Factors(j-I_SPIN+1)
     .             + D_fracI(k)*D_MonApp(MOD(i-1,12)+1)*i1Factors(j-i+1)
     .                           * quals_to_states(k,3)
                  a2Factors(j-I_SPIN+1) = a2Factors(j-I_SPIN+1)
     .             + D_fracA(k)*D_MonApp(MOD(i-1,12)+1)*a1Factors(j-i+1)
     .                           * quals_to_states(k,4)

               !}
               end do
            !}
            end do
            !! >> SPIN-UP FINISH
            do i = 1,I_nMonths
            !{
               do j = i,I_nMonths
               !{
c                  if ( s2Factors(j).lt.0 .and. D_MonApp(i).lt.0 ) cycle
c                  if ( d2Factors(j).lt.0 .and. D_MonApp(i).lt.0 ) cycle
c                  if ( i2Factors(j).lt.0 .and. D_MonApp(i).lt.0 ) cycle
c                  if ( a2Factors(j).lt.0 .and. D_MonApp(i).lt.0 ) cycle

                  s2Factors(j) = s2Factors(j) 
     .                         + D_fracS(k)*D_MonApp(i)*s1Factors(j-i+1)
     .                           * quals_to_states(k,1)
                  d2Factors(j) = d2Factors(j)
     .                         + D_fracD(k)*D_MonApp(i)*d1Factors(j-i+1)
     .                           * quals_to_states(k,2)
                  i2Factors(j) = i2Factors(j)
     .                         + D_fracI(k)*D_MonApp(i)*i1Factors(j-i+1)
     .                           * quals_to_states(k,3)
                  a2Factors(j) = a2Factors(j)
     .                         + D_fracA(k)*D_MonApp(i)*a1Factors(j-i+1)
     .                           * quals_to_states(k,4)

                  !if ( s2Factors(j).lt.0 ) s2Factors(j) = 0
                  !if ( d2Factors(j).lt.0 ) d2Factors(j) = 0
                  !if ( i2Factors(j).lt.0 ) i2Factors(j) = 0
                  !if ( a2Factors(j).lt.0 ) a2Factors(j) = 0
               !}
               end do
            !}
            end do
            if ( I_DEBUG0 .eq. 1 )
     .       write(*,'(A)') 'Break Through Curve of Analyte '//quals(k)
            ! *** If L_SET_S2MIN_ZERO is false then find the minimum s2factor
            ! *** And then shift the curve to set positive dynamic equilibrium
            if ( L_SET_S2MIN_ZERO .eqv. .false. ) then
               s2FactorMin = 0.0
               d2FactorMin = 0.0
               i2FactorMin = 0.0
               a2FactorMin = 0.0
               do i = 1,I_nMonths
                  if ( s2Factors(i).lt.0 .and. 
     .                 s2Factors(i).lt.s2FactorMin )
     .                 s2FactorMin = s2Factors(i)-0.00000001D0
                  if ( d2Factors(i).lt.0 .and.
     .                 d2Factors(i).lt.d2FactorMin )
     .                 d2FactorMin = d2Factors(i)-0.00000001D0
                  if ( i2Factors(i).lt.0 .and.
     .                 i2Factors(i).lt.i2FactorMin )
     .                 i2FactorMin = i2Factors(i)-0.00000001D0
                  if ( a2Factors(i).lt.0 .and.
     .                 a2Factors(i).lt.a2FactorMin )
     .                 a2FactorMin = a2Factors(i)-0.00000001D0
               end do
            end if
            if ( L_SET_S2MIN_ZERO .eqv. .false. ) then
            !{
               !TODO: Fix Negative Factors (Ensure non -ve export of analyte)
               if ( s2FactorMin .lt. 0 ) then
                  do i = 1,I_nMonths
                     s2Factors(i) = s2Factors(i) - s2FactorMin
                  end do
               end if
               if ( d2FactorMin .lt. 0 ) then
                  do i = 1,I_nMonths
                     d2Factors(i) = d2Factors(i) - d2FactorMin
                  end do
               end if
               if ( i2FactorMin .lt. 0 ) then
                  do i = 1,I_nMonths
                     i2Factors(i) = i2Factors(i) - i2FactorMin
                  end do
               end if
               if ( a2FactorMin .lt. 0 ) then
                  do i = 1,I_nMonths
                     a2Factors(i) = a2Factors(i) - a2FactorMin
                  end do
               end if
c                  write(*,'(I2, E12.5, E12.5)'), i,s1Factors(i-1),s2Factors(i)
c                  write(*,'(I2,E12.5,E12.5,E12,5,E12.5)'),i,
c     .               s2Factors(i),d2Factors(i),i2Factors(i),a2Factors(i)
               do i = 1,I_nMonths
                  if ( I_DEBUG0 .eq. 1 )
     .              write(*,1237) i,
     .               s2Factors(i),d2Factors(i),i2Factors(i),a2Factors(i)
               end do
            !}
            else
            !{
               do i = 1,I_nMonths
               !{
                  !TODO: Fix Negative Factors (Ensure non -ve export of analyte)
                  if ( s2Factors(i).lt.0 ) s2Factors(i) = 0.0D0
                  if ( d2Factors(i).lt.0 ) d2Factors(i) = 0.0D0
                  if ( i2Factors(i).lt.0 ) i2Factors(i) = 0.0D0
                  if ( a2Factors(i).lt.0 ) a2Factors(i) = 0.0D0
c                  write(*,'(I2, E12.5, E12.5)'), i,s1Factors(i-1),s2Factors(i)
c                  write(*,'(I2,E12.5,E12.5,E12,5,E12.5)'),i,
c     .               s2Factors(i),d2Factors(i),i2Factors(i),a2Factors(i)
                  if ( I_DEBUG0 .eq. 1 )
     .              write(*,1237) i,
     .               s2Factors(i),d2Factors(i),i2Factors(i),a2Factors(i)
               !}
               end do
            !}
            end if

            do i = 1,I_nMonths
            !{
               do j = 1,31
               !{
                  s3Factors(i,j) = s2Factors(i)
                  d3Factors(i,j) = d2Factors(i)
                  i3Factors(i,j) = i2Factors(i)
                  a3Factors(i,j) = a2Factors(i)
               !}
               end do
            !}
            end do

            if ( L_rSAS(k,1) ) then
            !{
               if ( I_DEBUG0 .eq. 1 )
     .          print*,outdir
               if ( I_DEBUG0 .eq. 1 )
     .          print*,lscen(:lenlscen)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,luname(l)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,lseg(:lenlseg)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,quals(k)
               fnam = outdir//'eof/daily/'//lscen(:lenlscen)
     .           //'/rSASO/'//luname(l)//'_'//lseg(:lenlseg)
     .           //'.S'//quals(k)
               write(*,*)' Reading: ',fnam
               open(dfile,file=fnam,status='old',iostat=err)
               if (err.ne.0) go to 991
               first = .true.
106            read(dfile,'(a300)',err=992,end=701)longline ! error if ends before start
               call d2x(longline,last)
               read (longline,*,end=994,err=994)
     .              I_tYear,I_tMonth,I_tDay,D_tDouble !TODO Assuming rsas & simulation time aligns
               if (I_tYear .LT. I_Year1) goto 106 ! ie first line starts sonner -- loop
               if (I_tYear .GT. I_Year1) goto 702 ! ie first line starts later  -- error
               if (I_tMonth .NE. 1 .or. I_tDay .NE. 1) goto 703
               m = 1
               do yy = I_Year1,I_Year2
               !{
                  do mm = 1,12
                  !{
                     do dd=1,ndaysinmonth(yy,mm)
                     !{
!                        s3Factors(m,dd) = D_tDouble

c                        if ( m .le. I_nMonths .and. 
c     .                       dd .lt. ndaysinmonth(yy,mm) ) then
                        if (first .eqv. .false.) then
                           read(dfile,'(a300)',err=992,END=704)longline
                           call d2x(longline,last)
                           read (longline,*,end=994,err=994)
     .                          I_tYear,I_tMonth,I_tDay,D_tDouble
                           if ( I_DEBUG0 .eq. 1 )
     .                      print*,'Read ',I_tYear,I_tMonth,I_tDay
                        end if
c                        else
c                           print*,'SKIPING'
c                        end if
                        s3Factors(m,dd) = D_tDouble
                        first = .false.
                     !}
                     end do
                     m = m + 1
                  !}
                  end do
               !}
               end do
c               call yesterday(I_tYear,I_tMonth,I_tDay)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,I_tYear,I_tMonth,I_tDay
               if (I_tYear.NE.I_Year2 .OR. I_tMonth.NE.12
     .                .OR. I_tDay.NE.31) goto 704
            !}
            end if
            if ( L_rSAS(k,2) ) then
            !{
               if ( I_DEBUG0 .eq. 1 )
     .          print*,outdir
               if ( I_DEBUG0 .eq. 1 )
     .          print*,lscen(:lenlscen)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,luname(l)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,lseg(:lenlseg)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,quals(k)
               fnam = outdir//'eof/daily/'//lscen(:lenlscen)
     .           //'/rSASO/'//luname(l)//'_'//lseg(:lenlseg)
     .           //'.D'//quals(k)
               write(*,*)' Reading: ',fnam
               open(dfile,file=fnam,status='old',iostat=err)
               if (err.ne.0) go to 991
               first = .true.
107            read(dfile,'(a300)',err=992,end=701)longline ! error if ends before start
               call d2x(longline,last)
               read (longline,*,end=994,err=994)
     .              I_tYear,I_tMonth,I_tDay,D_tDouble !TODO Assuming rsas & simulation time aligns
               if (I_tYear .LT. I_Year1) goto 107 ! ie first line starts sonner -- loop
               if (I_tYear .GT. I_Year1) goto 702 ! ie first line starts later  -- error
               if (I_tMonth .NE. 1 .or. I_tDay .NE. 1) goto 703
               m = 1
               do yy = I_Year1,I_Year2
               !{
                  do mm = 1,12
                  !{
                     do dd=1,ndaysinmonth(yy,mm)
                     !{
!                        d3Factors(m,dd) = D_tDouble

c                        if ( m .le. I_nMonths .and. 
c     .                       dd .lt. ndaysinmonth(yy,mm) ) then
                        if (first .eqv. .false.) then
                           read(dfile,'(a300)',err=992,END=704)longline
                           call d2x(longline,last)
                           read (longline,*,end=994,err=994)
     .                          I_tYear,I_tMonth,I_tDay,D_tDouble
                           if ( I_DEBUG0 .eq. 1 )
     .                      print*,'Read ',I_tYear,I_tMonth,I_tDay
                        end if
c                        else
c                           print*,'SKIPING'
c                        end if
                        d3Factors(m,dd) = D_tDouble
                        first = .false.
                     !}
                     end do
                     m = m + 1
                  !}
                  end do
               !}
               end do
c               call yesterday(I_tYear,I_tMonth,I_tDay)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,I_tYear,I_tMonth,I_tDay
               if (I_tYear.NE.I_Year2 .OR. I_tMonth.NE.12
     .                .OR. I_tDay.NE.31) goto 704
            !}
            end if
            if ( L_rSAS(k,3) ) then
            !{
               if ( I_DEBUG0 .eq. 1 )
     .          print*,outdir
               if ( I_DEBUG0 .eq. 1 )
     .          print*,lscen(:lenlscen)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,luname(l)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,lseg(:lenlseg)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,quals(k)
               fnam = outdir//'eof/daily/'//lscen(:lenlscen)
     .           //'/rSASO/'//luname(l)//'_'//lseg(:lenlseg)
     .           //'.A'//quals(k)
               write(*,*)' Reading: ',fnam
               open(dfile,file=fnam,status='old',iostat=err)
               if (err.ne.0) go to 991
               first = .true.
108            read(dfile,'(a300)',err=992,end=701)longline ! error if ends before start
               call d2x(longline,last)
               read (longline,*,end=994,err=994)
     .              I_tYear,I_tMonth,I_tDay,D_tDouble !TODO Assuming rsas & simulation time aligns
               if (I_tYear .LT. I_Year1) goto 108 ! ie first line starts sonner -- loop
               if (I_tYear .GT. I_Year1) goto 702 ! ie first line starts later  -- error
               if (I_tMonth .NE. 1 .or. I_tDay .NE. 1) goto 703
               m = 1
               do yy = I_Year1,I_Year2
               !{
                  do mm = 1,12
                  !{
                     do dd=1,ndaysinmonth(yy,mm)
                     !{
!                        i3Factors(m,dd) = D_tDouble

c                        if ( m .le. I_nMonths .and. 
c     .                       dd .lt. ndaysinmonth(yy,mm) ) then
                        if (first .eqv. .false.) then
                           read(dfile,'(a300)',err=992,END=704)longline
                           call d2x(longline,last)
                           read (longline,*,end=994,err=994)
     .                          I_tYear,I_tMonth,I_tDay,D_tDouble
                           if ( I_DEBUG0 .eq. 1 )
     .                      print*,'Read ',I_tYear,I_tMonth,I_tDay
                        end if
c                        else
c                           print*,'SKIPING'
c                        end if
                        i3Factors(m,dd) = D_tDouble
                        first = .false.
                     !}
                     end do
                     m = m + 1
                  !}
                  end do
               !}
               end do
c               call yesterday(I_tYear,I_tMonth,I_tDay)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,I_tYear,I_tMonth,I_tDay
               if (I_tYear.NE.I_Year2 .OR. I_tMonth.NE.12
     .                .OR. I_tDay.NE.31) goto 704
            !}
            end if
            if ( L_rSAS(k,4) ) then
            !{
               if ( I_DEBUG0 .eq. 1 )
     .          print*,outdir
               if ( I_DEBUG0 .eq. 1 )
     .          print*,lscen(:lenlscen)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,luname(l)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,lseg(:lenlseg)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,quals(k)
               fnam = outdir//'eof/daily/'//lscen(:lenlscen)
     .           //'/rSASO/'//luname(l)//'_'//lseg(:lenlseg)
     .           //'.A'//quals(k)
               write(*,*)' Reading: ',fnam
               open(dfile,file=fnam,status='old',iostat=err)
               if (err.ne.0) go to 991
               first = .true.
109            read(dfile,'(a300)',err=992,end=701)longline ! error if ends before start
               call d2x(longline,last)
               read (longline,*,end=994,err=994) 
     .              I_tYear,I_tMonth,I_tDay,D_tDouble !TODO Assuming rsas & simulation time aligns
               if (I_tYear .LT. I_Year1) goto 109 ! ie first line starts sonner -- loop
               if (I_tYear .GT. I_Year1) goto 702 ! ie first line starts later  -- error
               if (I_tMonth .NE. 1 .or. I_tDay .NE. 1) goto 703
               m = 1
               do yy = I_Year1,I_Year2
               !{
                  do mm = 1,12
                  !{
                     do dd=1,ndaysinmonth(yy,mm)
                     !{
!                        a3Factors(m,dd) = D_tDouble

c                        if ( m .le. I_nMonths .and. 
c     .                       dd .lt. ndaysinmonth(yy,mm) ) then
                        if (first .eqv. .false.) then
                           read(dfile,'(a300)',err=992,END=704)longline
                           call d2x(longline,last)
                           read (longline,*,end=994,err=994)
     .                          I_tYear,I_tMonth,I_tDay,D_tDouble
                           if ( I_DEBUG0 .eq. 1 )
     .                      print*,'Read ',I_tYear,I_tMonth,I_tDay
                        end if
c                        else
c                           print*,'SKIPING'
c                        end if
                        a3Factors(m,dd) = D_tDouble
                        first = .false.
                     !}
                     end do
                     m = m + 1
                  !}
                  end do
               !}
               end do
c               call yesterday(I_tYear,I_tMonth,I_tDay)
               if ( I_DEBUG0 .eq. 1 )
     .          print*,I_tYear,I_tMonth,I_tDay
               if (I_tYear.NE.I_Year2 .OR. I_tMonth.NE.12
     .                .OR. I_tDay.NE.31) goto 704
            !}
            end if

            m = 1
            do yy = I_Year1,I_Year2
            !{
               do mm = 1,12
               !{
                  do dd=1,ndaysinmonth(yy,mm)
                  !{
                     if ( I_DEBUG0 .eq. 1 )
     .                print*,'S3: ',yy,mm,dd,s3Factors(m,dd),
     .                  d3Factors(m,dd),i3Factors(m,dd),a3Factors(m,dd)
                  !}
                  end do
                  m = m + 1
               !}
               end do
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
            hh = 1  ! ** count hours
            m = 1  ! ** count months
            do yy = I_Year1,I_Year2
            !{
               do mm = 1,12
               !{
                  do dd=1,ndaysinmonth(yy,mm)
                  !{
                     do j = 1,24
                     !{
                        D_hSCON(hh) = F_hSURO(hh) * s3Factors(m,dd)
                        D_hDCON(hh) = D_hDCON(hh) * d3Factors(m,dd) !***
                        D_hICON(hh) = F_hIFWO(hh) * i3Factors(m,dd)
                        D_hACON(hh) = F_hAGWO(hh) * a3Factors(m,dd)
                        if( yy.ge.I_YearC1 .and. yy.le.I_YearC2 ) then
                        !{
                           D_tSCON = D_tSCON + D_hSCON(hh)
                           D_tDCON = D_tDCON + D_hDCON(hh)
                           D_tICON = D_tICON + D_hICON(hh)
                           D_tACON = D_tACON + D_hACON(hh)
                        !}
                        end if

                        hh = hh + 1
                     !}
                     end do
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
            if ( (D_tSCON + D_tDCON + D_tICON).eq.0 ) then
               D_Sfactor = 0
            else
               D_Sfactor = D_sTarget(l,k) * (I_YearC2-I_YearC1+1)
     .                  / (D_tSCON + D_tDCON + D_tICON)
            end if
            if (D_tACON .eq. 0) then
               D_Bfactor = 0.0D0
            else
               D_Bfactor = D_bTarget(l,k) * (I_YearC2-I_YearC1+1)
     .                  / (D_tACON)
            end if

            if ( I_DEBUG0 .eq. 1 )
     .       write(*,'(/A F12.5)') 'Factor S = ',D_Sfactor
            if ( I_DEBUG0 .eq. 1 )
     .       write(*,'(A F12.5)' ) 'Factor B = ',D_Bfactor

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
            if ( I_DEBUG0 .eq. 1 )
     .       write(*,'(3(A$))'),' Writing ',fnam(:I_lenfnam),' ...'
            if ( I_DEBUG0 .eq. 1 )
     .       print*,''
            open(dfile,file=fnam,status='unknown',iostat=err)
            if (err.ne.0) go to 991
c            if ( I_DEBUG0 .eq. 1 )
c     .       write(dfile,1234) 'year,mm',
c     .       ',S',quals(k),',D',quals(k),',I',quals(k),',A',quals(k),','
            write(dfile,1239) 'year,mm',
     .       ',S',quals(k),',D',quals(k),',I',quals(k),',A',quals(k),','
     .       ,'SURO,SEDM,IFWO,AGWO'

            !>> COMPUTE monthly yields
            j = 1  ! ** count hours
            m = 1  ! ** count months
            do yy = I_Year1,I_Year2
               do mm = 1,12
                  D_mSCON(m) = 0.0D0
                  D_mDCON(m) = 0.0D0
                  D_mICON(m) = 0.0D0
                  D_mACON(m) = 0.0D0

c                  D_mSURO(m) = 0.0D0
c                  D_mSEDM(m) = 0.0D0
c                  D_mIFWO(m) = 0.0D0
c                  D_mAGWO(m) = 0.0D0
c                  D_mFLOW(m) = 0.0D0

                  do i=1,ndaysinmonth(yy,mm)*24
                     D_mSCON(m) = D_mSCON(m) + D_hSCON(j)
                     D_mDCON(m) = D_mDCON(m) + D_hDCON(j)
                     D_mICON(m) = D_mICON(m) + D_hICON(j)
                     D_mACON(m) = D_mACON(m) + D_hACON(j)

c                     D_mSURO(m) = D_mSURO(m) + F_hSURO(j)
c                     D_mSEDM(m) = D_mSEDM(m) + F_hSEDM(j)
c                     D_mIFWO(m) = D_mIFWO(m) + F_hIFWO(j)
c                     D_mAGWO(m) = D_mAGWO(m) + F_hAGWO(j)
c                     D_mFLOW(m) = D_mFLOW(m) + 
c     .                            F_hSURO(j) + F_hIFWO(j) + F_hAGWO(j)
                     j = j + 1
                  end do
                  if ( I_DEBUG0 .eq. 1 )
     .             write(*,1236) yy,mm,D_MonApp(m),
     .                 D_mSCON(m),D_mDCON(m),D_mICON(m),D_mACON(m),
     .                 D_mFLOW(m)
c                  write(dfile,1235) yy,mm,
c     .                 D_mSCON(m),D_mDCON(m),D_mICON(m),D_mACON(m)
                  write(dfile,1238) yy,mm,
     .                 D_mSCON(m),D_mDCON(m),D_mICON(m),D_mACON(m)
     .                ,D_mSURO(m),D_mSEDM(m),D_mIFWO(m),D_mAGWO(m)
                  m = m + 1
               end do
            end do
            close(dfile)

            
            !>> WRITE WDM
            if ( I_DEBUG0 .eq. 1 )
     .       print*,'\nWriting WDMs...'
            if ( I_DEBUG0 .eq. 1 )
     .       print*,'\n',quals(k),OutWDMs(k,1)
            if (OutWDMs(k,1) .ne. 999)
     .    call puthourdsn(wdmfil,sdate,edate,OutWDMs(k,1),nvals,F_hSCON)
            if ( I_DEBUG0 .eq. 1 )
     .       print*,'\n',quals(k),OutWDMs(k,2)
            if (OutWDMs(k,2) .ne. 999)
     .    call puthourdsn(wdmfil,sdate,edate,OutWDMs(k,2),nvals,F_hDCON)
            if ( I_DEBUG0 .eq. 1 )
     .       print*,'\n',quals(k),OutWDMs(k,3)
            if (OutWDMs(k,3) .ne. 999)
     .    call puthourdsn(wdmfil,sdate,edate,OutWDMs(k,3),nvals,F_hICON)
            if ( I_DEBUG0 .eq. 1 )
     .       print*,'\n',quals(k),OutWDMs(k,4)
            if (OutWDMs(k,4) .ne. 999)
     .    call puthourdsn(wdmfil,sdate,edate,OutWDMs(k,4),nvals,F_hACON)

         !}
         end do

         !>> CLOSE WDM
         call wdflcl(wdmfil,err)
         if (err.ne.0) go to 803
      !}
      end do

      print*,"iRunning UNEC ",
     .     lscen(:lenlscen),lseg(:lenlseg)," Finished"

      return

1234  format(A7,A2,A3,A2,A3,A2,A3,A2,A3,A)
1235  format(I4,',',I2,',',e19.12,',',e19.12,',',e19.12,',',e19.12)
1236  format(I4,',',I2,',',e19.12,5(',',e19.12))
1237  format(I4,4(',',e15.8))
1238  format(I4,',',I2,8(',',e19.12))
1239  format(A7,A2,A3,A2,A3,A2,A3,A2,A3,A,A)

701   report(1) = 'Error 701 Problem with rSAS file:'
      report(2) = fnam
      write(report(3),*)'File ended before Start Year ', I_Year1
      go to 999

702   report(1) = 'Error 702 Problem with rSAS file:'
      report(2) = fnam
      write(report(3),*)'Data missing for Start Year  ', I_Year1
      go to 999

703   report(1) = 'Error 703 Problem with rSAS file:'
      report(2) = fnam
      write(report(3),*)'Data missing ', '\nneeded 1/ 1/ ',I_Year1,
     .  ' but has ',I_tMonth,'/',I_tDay,'/',I_tYear
      go to 999

704   report(1) = 'Error 704 Problem with rSAS file:'
      report(2) = fnam
      write(report(3),*)'File ended before End Year ', I_Year2,
     . ' last read ',I_tMonth,'/',I_tDay,'/',I_tYear
      go to 999

801   report(1) = 'Error 801 WDM file must previously exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
      go to 999

803   report(1) = 'Error 803 Problem closing wdm file: '
      report(2) = wdmfnam
      report(3) = ' '
      go to 999

990   report(1) = 'Error 990 Problem with Simulation Periods'
      write(report(2),*)'Simulation : [',I_Year1,',',I_Year2,']'
      write(report(3),*)'Evaluation : [',I_YearC1,',',I_YearC2,']'
      go to 999

991   report(1) = 'Error 991 Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Error 992 Problem reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'Error 993 Problem parsing line: in file:'
      report(2) = line
      report(3) = fnam
      go to 999

994   report(1) = 'Error 994 Problem parsing longline: in file:'
      report(2) = longline
      report(3) = fnam
      go to 999

995   report(1) = 'Error 995 Problem missing data in file:'
      report(2) = longline
      report(3) = fnam
      go to 999

996   report(1) = 'Error 996 Problem parsing land use in file:'
      report(2) = fnam
      report(3) = longline
      go to 999

997   report(1) = 'Error 997 Problem parsing land seg in file:'
      report(2) = fnam
      report(3) = lseg(:lenlseg)
      go to 999

998   report(1) = 'Error 998 Sensitivity Method Not Implemented'
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

      SUBROUTINE BreakThroughFactors(
     I          I_paramLeadEdge,
     I          I_paramPeakConc,
     I          I_paramTrailEdge,
     I          D_paramExponent1,
     I          D_paramExponent2,
     I          I_paramNormalize,
     I          I_DEBUG0,
     O          D_tFactors,
     O          I_temp)
      !{

         implicit none
         include 'unec.inc'

         integer           I_RISINGLIMB,I_PARABOLIC,I_EXPONENTIAL
         data              I_PARABOLIC   /1/
         data              I_EXPONENTIAL /2/
         data              I_RISINGLIMB  /1/

         logical           L_SETTRAILTOZERO
         data              L_SETTRAILTOZERO /.true./
         
         
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

         double precision  D_peak,D_trail,D_factor


         ! PARAMETERS ASSIGN
c         I_paramLeadEdge    = 6
c         I_paramPeakConc    = 12
c         I_paramTrailEdge   = 24
c         D_paramExponent1   = 0.1
c         D_paramExponent2   = 0.2
c         I_paramNormalize   = 0


         if ( I_DEBUG0 .eq. 1 )
     .    write(*,'(A I4 I4 I4$)')
     .        'Param: ',I_paramLeadEdge,I_paramPeakConc,I_paramTrailEdge
         if ( I_DEBUG0 .eq. 1 )
     .    write(*,'(E12.5, E12.5$)') D_paramExponent1,D_paramExponent2
         if ( I_DEBUG0 .eq. 1 )
     .    write(*,'(I2)') I_paramNormalize

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
              if ( I_RISINGLIMB .eq. I_EXPONENTIAL ) then
                D_tFactors(i)=EXP( D_paramExponent1*(i-I_paramPeakConc))
              end if
              if ( I_RISINGLIMB .eq. I_PARABOLIC ) then
                D_tFactors(i)=SQRT(REAL(i-I_paramLeadEdge)) / 
     .                       SQRT(REAL(I_paramPeakConc-I_paramLeadEdge))
              end if
            else
                D_tFactors(i)=EXP(-D_paramExponent2*(i-I_paramPeakConc))
            end if
            if ( I_DEBUG0 .eq. 1 )
     .       write(*,'(I4, E12.5)'), i,D_tFactors(i)
         !}
         end do

         if ( L_SETTRAILTOZERO ) then
         !{
            D_peak  = D_tFactors(I_paramPeakConc)
            D_trail  = D_tFactors(I_paramTrailEdge)
            D_factor = ( D_peak - D_trail ) / D_peak
            do i = 1,I_paramPeakConc
               D_tFactors(i) = D_tFactors(i) * D_factor
            end do
            do i = I_paramPeakConc+1,I_paramTrailEdge
               if ( D_tFactors(i).gt. 0 ) then
                  D_tFactors(i) = D_tFactors(i) - D_trail
               end if
            end do
         !}
         end if


         if ( I_paramNormalize .eq. 1 ) then
         !{
            D_total = 0.0D0
            do i = 1,I_paramTrailEdge
            !{
               D_total = D_total + D_tFactors(i)
            !}
            end do
            if ( I_DEBUG0 .eq. 1 )
     .       write(*,'(A E12.5)') 'Total = ',D_total
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
            if ( I_DEBUG0 .eq. 1 )
     .       write(*,'(I4, E12.5)'), i,D_tFactors(i)
         !}
         end do
         I_temp = I_temp + 1
         print*,''
         if ( I_paramNormalize.eq.1 ) then
           ! http://www.epa.gov/ncea/pdfs/qtracer/qtracerch03.pdf Section 3.2.1
c           write(*,'(A I4 A)'),'Mean Residence Time = ',I_temp,' months'
         end if

         return

      end
      !}


      SUBROUTINE FlowDSN(filename,dsnstr,dsn)
      !{
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
      
         return

991      report(1) = 'Error 992b Problem opening file:'
         report(2) = fnam
         write(report(3),*)'iostat error = ',err
         go to 999
      
992      report(1) = 'Error 992c Problem reading file: near line:'
         report(2) = fnam
         report(3) = line
         go to 999

999   call stopreport(report)

      !}
      END
