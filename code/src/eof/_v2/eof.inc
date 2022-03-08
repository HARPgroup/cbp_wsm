
      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/wdm.inc'

      include '../lib/inc/land_use.inc'

C ****************
C TODO: Most of the mapping info will have to be in the form of
C       external tables rather than static version now

ccccccc
c PQUAL outputs

      integer nquals
      parameter (nquals=5)  ! NH3,NO3,LON,RON,PO4

      character*3 quals(nquals)
      data quals /'NH3','NO3','LON','RON','PO4'/

      integer nstates
      parameter (nstates=4)  ! D,O,I,A or D,S,I,A

      character*1 states(nstates)
      data states /'D','S','I','A'/

      integer OutWDMs(nquals,nstates) ! Parsed from config/catalog/iovars/p532qual/perlnd
c      data    OutWDMs/142,999,141,143,152,
c     .                144,145,146,147,153,
c     .                244,245,246,247,253,
c     .                444,445,446,447,453/

ccccccc
c INPUT dataset

      integer napptypes
      parameter (napptypes=9)  ! A,V,P,F,M,L,T,U,R, #Q

      character*1 apptypes(napptypes)
      data apptypes /'A','V','P','F','M','L','T','U','R'/

      character*6 apptypeslong(napptypes)
      data apptypeslong /'atdep','coverc','plow','fert','manure',
     .                   'legume','uptake','muptake','reforg'/

      integer apptypesflag(nlu,napptypes)
c      data apptypesflag /1,0,0,1,1,1,1,0,0/
      integer forceignore(napptypes)
      data forceignore /9,0,0,9,9,9,9,0,0/

      integer nappspecies
      parameter (nappspecies=5)  ! nh3n,no3n,orgn,po4p,orgp

      character*4 appspecies(nappspecies)
      data appspecies /'no3n','nh3n','orgn','po4p','orgp'/


ccccccc
c PQUAL outputs vs. Input datasets

      integer pquals_vs_inputs(nquals,nstates,nappspecies)


ccccccc
      real inp_calib_app(napptypes,nappspecies)
      real inp_scen_app(napptypes,nappspecies)

      real        sensitivity(nquals)
      character*1 smethod(nquals)
      real        delta_out(nquals,nstates)
      


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer wdmfil
      parameter (wdmfil=dfile+10)

      integer sdate(ndate),edate(ndate)

      integer dsns(nquals)
      real hvals(ndaymax*24) !,maxdsns)
      real pcent_delta,out_calib

      integer i,j,k,l,m

      character*10 specscen
      integer      lenspecscen
