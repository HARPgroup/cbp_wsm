
      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/wdm.inc'

      include '../lib/inc/land_use.inc'


      integer maxMonths
      parameter ( maxMonths = 360 ) ! 30 Years x 12

      real    tFactors(maxMonths)
c      data    tFactors /maxMonths*0/

      real    s1Factors(maxMonths)
      data    s1Factors /maxMonths*0/

      real    d1Factors(maxMonths)
      data    d1Factors /maxMonths*0/

      real    i1Factors(maxMonths)
      data    i1Factors /maxMonths*0/

      real    a1Factors(maxMonths)
      data    a1Factors /maxMonths*0/


      real    s2Factors(maxMonths)
      data    s2Factors /maxMonths*0.0/

      real    d2Factors(maxMonths)
      data    d2Factors /maxMonths*0.0/

      real    i2Factors(maxMonths)
      data    i2Factors /maxMonths*0.0/

      real    a2Factors(maxMonths)
      data    a2Factors /maxMonths*0.0/

      integer i,j,k,l,m,n,yy,mm


      integer      I_Year1,  I_Year2
      integer      I_YearC1, I_YearC2
      character*10 specscen
      integer      lenspecscen
      character*300 longline

      character*25  lcalib
      integer       lenlcalib


      integer nquals
      parameter (nquals=5)  ! NH3,NO3,LON,RON,PO4

      character*3 quals(nquals)
      data quals /'NH3','NO3','LON','RON','PO4'/

      integer nstates
      parameter (nstates=4)  ! D,O,I,A or D,S,I,A

      character*1 states(nstates)
c      data states /'D','S','I','A'/
      data states /'S','D','I','A'/

      integer OutWDMs(nquals,nstates) ! Parsed from config/catalog/iovars/p532qual/perlnd

      integer napptypes
      parameter (napptypes=9)  ! A,V,P,F,M,L,T,U,R, #Q

      character*1 apptypes(napptypes)
      data apptypes /'A','V','P','F','M','L','T','U','R'/

      character*6 apptypeslong(napptypes)
      data apptypeslong /'atdep','coverc','plow','fert','manure',
     .                   'legume','uptake','muptake','reforg'/

      integer apptypesflag(nlu,napptypes)

      integer forceignore(napptypes)
      data forceignore /9,0,0,9,9,9,9,0,0/

      integer nappspecies
      parameter (nappspecies=5)  ! nh3n,no3n,orgn,po4p,org

      character*4 appspecies(nappspecies)
      data appspecies /'no3n','nh3n','orgn','po4p','orgp'/


      integer quals_to_states(nquals,nstates)
      integer quals_to_appspecies(nquals,nappspecies)


      real inp_calib_app(napptypes,nappspecies,1980:2011,12)
      real inp_scen_app(napptypes,nappspecies,1980:2011,12)

      character*20  C20_sensitivity(nquals)
      real          R_sensitivity(nquals)
      character*2   C2_tsmethod(nquals)
      character*2   C2_smethod(nquals)
      integer       I_sgroup(nquals)

      integer dsnSURO, dsnWSSD, dsnIFWO, dsnAGWO
      parameter (dsnSURO=111)
      parameter (dsnWSSD=121)
      parameter (dsnIFWO=211)
      parameter (dsnAGWO=411)

      real hSURO(maxMonths*31*24)
      real hWSSD(maxMonths*31*24)
      real hIFWO(maxMonths*31*24)
      real hAGWO(maxMonths*31*24)
      real hDATA(maxMonths*31*24)

      real sTarget(nlu,nquals)
      real bTarget(nlu,nquals)
      real R_qualsTarget(nquals)
      real R_groupTarget(nquals)
      real R_Meof(nstates)

      real hSCON(maxMonths*31*24)
      real hDCON(maxMonths*31*24)
      real hICON(maxMonths*31*24)
      real hACON(maxMonths*31*24)

      real mSCON(maxMonths)
      real mDCON(maxMonths)
      real mICON(maxMonths)
      real mACON(maxMonths)

      integer wdmfil
      parameter (wdmfil=dfile+10)

      integer sdate(ndate),edate(ndate)
