      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'

      integer i   ! index
      character*200 longline

      integer maxsites,ns    ! maximum calibration sites
      parameter (maxsites = 300)

      character*13 segment(maxsites)

      integer nparms,np     ! number of parameters
      parameter (nparms=11)
      character*4 parname(nparms)  ! names of parameters
      data parname /'bias','effy','Leff',
     .              'avri','Meff','Wntr','Sumr',
     .              'Wari','Sari','lo10','lo05'/
      integer fnum(nparms)  ! file number
      data fnum /11,12,13,14,15,16,17,18,19,20,21/

      real values(nparms,maxsites)

*********** special vars for reading peak files
      integer npeakparms     ! number of parameters
      parameter (npeakparms=2)
      character*5 peakparname(npeakparms)  ! names of parameters
      data peakparname /'pbias','vbias'/

      real peakvalues(npeakparms,maxsites)

