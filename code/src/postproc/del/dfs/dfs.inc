      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/land_use.inc'
      include '../../../lib/inc/masslinks.inc'
      include '../../../lib/inc/modules.inc'
      include '../../../lib/inc/rsegs.inc'

      integer nloadmax,nloads      ! number of loads
      parameter (nloadmax = 20)    ! maximum number of loads
      character*4 loadname(nloadmax)  ! name of each load (e.g. 'totn')

      integer ndelmax      ! maximum number of delivery variables
      parameter (ndelmax=10) 
      integer ndel
      character*4 delname(ndelmax)

      integer load2del(nloadmax)    ! delivery surrogate for each load

      integer y1,y2
      parameter (y1=1980,y2=2100)

************ segment specific dfs
      real segDFmon(ndelmax,y1:y2,12)
      real segDFann(ndelmax,y1:y2)
      real segDFave(ndelmax)

************ whole basin dfs
      real basinDFmon(ndelmax,y1:y2,12)
      real basinDFann(ndelmax,y1:y2)
      real basinDFave(ndelmax)

************ reservoir basin specific dfs
      real resDFmon(ndelmax,y1:y2,12)
      real resDFann(ndelmax,y1:y2)
      real resDFave(ndelmax)

      integer sdate(6),edate(6),year1,year2

************ EOS, river output, and tfs from tf files
      real outmon(ndelmax,y1:y2,12),inmon(ndelmax,y1:y2,12)
      real tfmon(ndelmax,y1:y2,12)

      real outann(ndelmax,y1:y2),inann(ndelmax,y1:y2)
      real tfann(ndelmax,y1:y2)

      real outave(ndelmax),inave(ndelmax)
      real tfave(ndelmax)

