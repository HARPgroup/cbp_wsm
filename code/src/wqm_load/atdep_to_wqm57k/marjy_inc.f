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
      data Bname /'prec','wno3','dno3','wnh4','dnh4'/
      integer kprec,kwno3,kdno3,kwnh4,kdnh4
      data kprec,kwno3,kwnh4,kdno3,kdnh4 /1,2,3,4,5/

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


