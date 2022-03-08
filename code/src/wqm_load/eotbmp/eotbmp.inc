      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/rsegs.inc'
      include '../lib/inc/masslinks.inc'
      include '../lib/inc/wdm.inc'

      integer iyr,imm,idd,ihr
      integer syear, eyear
      integer sdate(6),edate(6)

      integer irseg,icon

      integer tuniqid
      integer tindex
      integer I_allup(maxrsegs) ! indices of upstream segments
      character*13 C_allup(maxrsegs)
      integer nallup,iallup

      integer nb
      integer ibcon,irvar
      real AnnEOTLoads(EarliestYear:LatestYear,maxBMPcon)
      real AnnRIVLoads(EarliestYear:LatestYear,maxBMPcon)
      real AnnEOTfactor(EarliestYear:LatestYear,maxBMPcon)
 
      integer I_DBG 
