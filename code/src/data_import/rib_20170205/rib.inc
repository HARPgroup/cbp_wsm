************************************************************************
**  reads from a file and writes a new wdm with similar name          **
************************************************************************
      implicit none

      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'
      include '../../lib/inc/ps_septic_atdep.inc'

      integer wdmfil,maxdsns
      parameter (wdmfil=dfile+10)
      parameter (maxdsns=20)

      character *1 answer
      character*6 tunits

      integer i,sdate(ndate),edate(ndate),tstep,ndsns,dsns(maxdsns)
      real hvals(ndaymax*24,maxdsns)
      real dvals(ndaymax,maxdsns)

      integer maxnlrsegs,nlrsegs,nlr,ilrsegs  ! number of lrsegs
      parameter (maxnlrsegs = 3000)
      character*19 lrsegs(maxnlrsegs)

      character*50  s2rfnam
      integer       lens2rfnam
      real          s2rfac(maxnlrsegs,1)

      integer maxcons
      parameter ( maxcons = 1 )
      character*4 C_conx(maxcons)
      integer     nconx,iconx

      character*300 dline
      character*10 xdline
      integer      lenxdline

      logical      lrsegfound

      character*3  C3lu
      parameter    (C3lu = 'mtg')

      integer      ribdsn
      parameter    (ribdsn = 3015)
