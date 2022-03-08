
      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/wdm.inc'

      include '../lib/inc/land_use.inc'

      integer I_DEBUG0, I_DEBUG1, I_DEBUG2, I_DEBUG3
      integer I_WDM

      integer I_MINYEAR, I_MAXYEAR
      parameter ( I_MINYEAR = 1980 )
      parameter ( I_MAXYEAR = 2017 )

      integer I_MAXMONTHS
      parameter ( I_MAXMONTHS = 12*(I_MAXYEAR-I_MINYEAR+1) ) ! 30 Years x 12

      integer I_MAXDAYS
      parameter ( I_MAXDAYS = I_MAXMONTHS*31 )

      double precision    D_tFactors(I_MAXMONTHS)
c      data    D_tFactors /I_MAXMONTHS*0/

      double precision    s1Factors(I_MAXMONTHS)
      data    s1Factors /I_MAXMONTHS*0/

      double precision    d1Factors(I_MAXMONTHS)
      data    d1Factors /I_MAXMONTHS*0/

      double precision    i1Factors(I_MAXMONTHS)
      data    i1Factors /I_MAXMONTHS*0/

      double precision    a1Factors(I_MAXMONTHS)
      data    a1Factors /I_MAXMONTHS*0/


      double precision    s2Factors(I_MAXMONTHS)
      data    s2Factors /I_MAXMONTHS*0.0/

      double precision    d2Factors(I_MAXMONTHS)
      data    d2Factors /I_MAXMONTHS*0.0/

      double precision    i2Factors(I_MAXMONTHS)
      data    i2Factors /I_MAXMONTHS*0.0/

      double precision    a2Factors(I_MAXMONTHS)
      data    a2Factors /I_MAXMONTHS*0.0/


      double precision    s3Factors(I_MAXMONTHS,31)
      data    s3Factors /I_MAXDAYS*0.0/

      double precision    d3Factors(I_MAXMONTHS,31)
      data    d3Factors /I_MAXDAYS*0.0/

      double precision    i3Factors(I_MAXMONTHS,31)
      data    i3Factors /I_MAXDAYS*0.0/

      double precision    a3Factors(I_MAXMONTHS,31)
      data    a3Factors /I_MAXDAYS*0.0/


      integer i,j,k,l,m,n,yy,mm,dd,hh


      integer      I_Year1,  I_Year2
      integer      I_YearC1, I_YearC2
      character*10 specscen
      integer      lenspecscen
      character*300 longline

      character*25  lcalib
      integer       lenlcalib


      integer I_NQUALS
      parameter (I_NQUALS=5)  ! NH3,NO3,LON,RON,PO4

      character*3 quals(I_NQUALS)
      data quals /'NH3','NO3','LON','RON','PO4'/

      integer I_NSTATES
      parameter (I_NSTATES=4)  ! I,D,I,A or S,D,I,A

      character*1 states(I_NSTATES)
c      data states /'D','S','I','A'/
      data states /'S','D','I','A'/

      integer OutWDMs(I_NQUALS,I_NSTATES) ! Parsed from config/catalog/iovars/p532qual/perlnd

      integer I_NAPPTYPES
      parameter (I_NAPPTYPES=9)  ! A,V,P,F,M,L,T,U,R, #Q

      character*1 apptypes(I_NAPPTYPES)
      data apptypes /'A','V','P','F','M','L','T','U','R'/

      character*6 apptypeslong(I_NAPPTYPES)
      data apptypeslong /'atdep','coverc','plow','fert','manure',
     .                   'legume','uptake','muptake','reforg'/

      integer apptypesflag(nlu,I_NAPPTYPES)

      integer forceignore(I_NAPPTYPES)
      data forceignore /9,0,0,9,9,9,9,0,0/

      integer I_NAPPSPECIES
      parameter (I_NAPPSPECIES=5)  ! nh3n,no3n,orgn,po4p,org

      character*4 appspecies(I_NAPPSPECIES)
      data appspecies /'no3n','nh3n','orgn','po4p','orgp'/


      integer quals_to_states(I_NQUALS,I_NSTATES)
      integer quals_to_appspecies(I_NQUALS,I_NAPPSPECIES)


      ! Partition of Input between Surface, Detached, Interflow & Groundwater
      ! TODO Implement separate fractions for QUALS, read from file for given land-segment
      double precision   D_fracS(I_NQUALS)
      double precision   D_fracD(I_NQUALS)
      double precision   D_fracI(I_NQUALS)
      double precision   D_fracA(I_NQUALS) 

      double precision D_inp_calib_app(I_NAPPTYPES,I_NAPPSPECIES,
     .                                    I_MINYEAR:I_MAXYEAR,12)
      double precision D_inp_scen_app(I_NAPPTYPES,I_NAPPSPECIES,
     .                                    I_MINYEAR:I_MAXYEAR,12)

      character*20      C20_sensitivity(I_NQUALS)
      double precision  D_sensitivity(I_NQUALS)
      character*2       C2_tsmethod(I_NQUALS)
      character*2       C2_smethod(I_NQUALS)
      integer           I_sgroup(I_NQUALS)
      character*25      C25_UNEC        ! Pointer to UNEC Parameter Tables
      character*25      C25_SENSITIVITY ! Pointer to Sensitivity Data Table
      integer           lC25_UNEC,lC25_SENSITIVITY

      character*4 flow(I_NSTATES)
      data flow /'SURO','SEDM','IFWO','AGWO'/

      integer dsnSURO, dsnSEDM, dsnIFWO, dsnAGWO
c      parameter (dsnSURO=111)
c      parameter (dsnSEDM=121)
c      parameter (dsnIFWO=211)
c      parameter (dsnAGWO=411)

c      double precision D_hSURO(I_MAXMONTHS*31*24)
c      double precision D_hSEDM(I_MAXMONTHS*31*24)
c      double precision D_hIFWO(I_MAXMONTHS*31*24)
c      double precision D_hAGWO(I_MAXMONTHS*31*24)
c      double precision hDATA(I_MAXMONTHS*31*24)
      real F_hSURO(I_MAXMONTHS*31*24)
      real F_hSEDM(I_MAXMONTHS*31*24)
      real F_hIFWO(I_MAXMONTHS*31*24)
      real F_hAGWO(I_MAXMONTHS*31*24)
c      real hDATA(I_MAXMONTHS*31*24)

      double precision D_sTarget(nlu,I_NQUALS)
      double precision D_bTarget(nlu,I_NQUALS)
      double precision D_qualsTarget(I_NQUALS)
      double precision D_groupTarget(I_NQUALS)
      double precision D_Meof(I_NSTATES)

      double precision D_hSCON(I_MAXMONTHS*31*24)
      double precision D_hDCON(I_MAXMONTHS*31*24)
      double precision D_hICON(I_MAXMONTHS*31*24)
      double precision D_hACON(I_MAXMONTHS*31*24)

      double precision D_mSCON(I_MAXMONTHS)
      double precision D_mDCON(I_MAXMONTHS)
      double precision D_mICON(I_MAXMONTHS)
      double precision D_mACON(I_MAXMONTHS)

      double precision D_mFLOW(I_MAXMONTHS)

      real F_hSCON(I_MAXMONTHS*31*24)
      real F_hDCON(I_MAXMONTHS*31*24)
      real F_hICON(I_MAXMONTHS*31*24)
      real F_hACON(I_MAXMONTHS*31*24)

      integer wdmfil
      parameter (wdmfil=dfile+10)

      integer sdate(ndate),edate(ndate)
