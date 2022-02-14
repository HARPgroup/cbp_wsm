
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


      integer      year1, year2
      character*10 specscen
      integer      lenspecscen
      character*300 longline

      character*25  lcalib
      integer       lenlcalib


      integer nquals
      parameter (nquals=5)  ! NH3,NO3,LON,RON,PO4

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

      real inp_calib_app(napptypes,nappspecies,1985:2011,12)
      real inp_scen_app(napptypes,nappspecies)
