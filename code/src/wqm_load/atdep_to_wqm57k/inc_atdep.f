      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

********** set dates
      integer year,month,day
      integer year1,year2
      include 'date.inc'

*********** summary output file name
      integer sumfile
      parameter(sumfile=12)

*********** definition of bay variables
      integer nBvar        ! number of bay variables
      parameter (nBvar = 5)
      character*4 Bname(nBvar)  ! names of bay variables
      data Bname /'no3x','nh4x','orgn','po4x','orgp'/
      integer kno3,knh3,korn,kpo4,korp
      data kno3,knh3,korn,kpo4,korp /1,2,3,4,5/

      real cellwq(366,year1:year2,nBvar)

      integer nd,ny,nm,nq

      integer ndaysinyear
      external ndaysinyear

*********** Scenario to Create
      character*50 atdepscen
      integer lenscen

      character*50 linkdir  ! name of cellsize directory
      integer lenlink

********** CMAQ scenario and names of CMAQ files
      character*20 CMAQbase, CMAQscen
      character*120 CbaseDir2, Cscendir2, CbaseSuperLong, CscenSuperLong
      integer lenCbase, lenCscen, lenCb2, lenCs2, lenCbLong, lenCsLong


      real fCWNOX(year1:year2)
      real fCWNHX(year1:year2)
      real fLDNOX(year1:year2)
      real fLDNHX(year1:year2)
      real fLORGN(year1:year2)
      real fLORGP(year1:year2)
      real fLPO4X(year1:year2)
