************************************************************************
** variables used frequently in the calibration parameter change prog **
************************************************************************
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include '../../../../lib/inc/lsegs.inc'
      include '../../../../lib/inc/land_use.inc'

      character*25 calscen    ! set of specifications for this
      integer lencalscen      ! calibration run

      character*12 module  ! variable specifiers
      integer lenmod
          
      character*3 clu

      integer maxtargets      ! number of targests to be calibrated
      parameter (maxtargets=20)

      integer nlinemax,nlines      ! number of parameter lines in code
      parameter (nlinemax=400)
      character*2000 parline(nlinemax),
     .                       varline,tableline,
     .                       vkeep,tkeep

************ Variable to contain Targets
      integer ntargets
      character*4 tarnam(maxtargets)
      real targets(maxtargets,maxlsegs)

      character*10 tarscen
      integer lentarscen

************ PLTGEN outputs from HSPF model
      integer maxplts                         ! number of targests to be calibrated
      parameter (maxplts=25)

      integer nplts
      character*4 pltnam(maxplts)

************ Parameter limits, min, max, HSPFmin, HSPFmax
      real limitKIMP(4),limitKMP(4),limitXFIX(4),limitK1(4)

************ parameter values
      integer vcKIMP(4),vcKMP(4)
      integer vcXFIX(4),vcK1(4)
      real KIMP(4),KMP(4),XFIX(4),K1(4)
      integer columnorder  ! order to read table

************ simulated annual loads 
      real simEOF(maxtargets,maxlsegs)

      real SPO4tar(maxlsegs),BPO4tar(maxlsegs)
      real SROPtar(maxlsegs)
      real SPO4(maxlsegs),BPO4(maxlsegs)
      real SROP(maxlsegs),SP(maxlsegs)

      character*4 version     ! version number

