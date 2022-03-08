      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/land_use.inc'
      include '../../../lib/inc/upstream.inc'
      include '../../../lib/inc/masslinks.inc'
      include '../../../lib/inc/modules.inc'
      include '../../../lib/inc/rsegs.inc'
      include '../../../lib/inc/lsegs.inc'
      include '../../../lib/inc/ps_septic_atdep.inc'

      integer nloadmax,nloads      ! number of loads
      parameter (nloadmax = 20)    ! maximum number of loads
      character*4 loadname(nloadmax)  ! name of each load (e.g. 'totn')
 
      integer ndelmax,ndel    ! maximum number of delivery variables
      parameter (ndelmax=10)  ! fewer since species of nutrients use
      character*4 delname(ndelmax) ! the same delivery factors

      integer load2del(nloadmax)    ! delivery surrogate for each load

************** variables to hold loads in and loads out for a segment
      integer y1,y2
      parameter (y1=1980,y2=2100)
      real outmon(ndelmax,y1:y2,12),inmon(ndelmax,y1:y2,12)
      real upmon(ndelmax,y1:y2,12)
      real tfmon(ndelmax,y1:y2,12),tempmon(ndelmax,y1:y2,12)

      real outann(ndelmax,y1:y2),inann(ndelmax,y1:y2)
      real upann(ndelmax,y1:y2)
      real tfann(ndelmax,y1:y2),tempann(ndelmax,y1:y2)

      real outave(ndelmax),inave(ndelmax)
      real upave(ndelmax)
      real tfave(ndelmax),tempave(ndelmax)

      real totalin

      integer year1,year2 ! first year and number of years
                          ! tests that all data types are same years

      integer sdate(6),edate(6)

