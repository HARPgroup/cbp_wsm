************************************************************************
** variables used frequently in the calibration parameter change prog **
************************************************************************
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include '../../../../lib/inc/lsegs.inc'
      include '../../../../lib/inc/land_use.inc'

      character*25 calscen    ! set of specifications for this
      integer lencalscen      ! calibration run

      integer maxtargets      ! number of targests to be calibrated
      parameter (maxtargets=20)

      character*3 clu  ! character land use

      character*12 module  ! variable specifiers
      integer lenmod

      integer nlinemax,nlines  ! number of parameter lines in code
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
      integer maxplts      ! number of targests to be calibrated
      parameter (maxplts=25)

      integer nplts
      character*4 pltnam(maxplts)

************** Parameter limits, min, max, HSPFmin, HSPFmax
      real limitKIMNI(4),limitKAM(4),limitKDNI(4),limitKNI(4)
      real limitKIMAM(4),limitKLON(4),limitKRON(4)

************ parameter values
      real KIMNI(4),KAM(4),KDNI(4),KNI(4),KIMAM(4)
      real KRON(4),KLON(4)
      real SKVOL,UKVOL

************ column order
      integer columnorder  ! order to read table

************ simulated annual loads 
      real simEOF(maxplts,maxlsegs)

