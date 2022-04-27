************************************************************************
**  reads from a file and writes a new wdm with similar name          **
************************************************************************
      implicit none

      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'
      include '../../lib/inc/ps_septic_atdep.inc'

      integer wdmfil,maxdsns,maxcons,maxnyears
      parameter (wdmfil=dfile+10)
      parameter (maxdsns   = 20)
      parameter (maxcons   =  8)
      parameter (maxnyears = 53)

      integer ndsns,idsn

      character *1 answer
      character*6 tunits

      integer i,sdate(ndate),edate(ndate),tstep
      integer imon,iyear
      integer iday,ndays
      integer indx1,indx2

      integer  ndaysinmonth,daysinmonth
      external ndaysinmonth

      character*4 outcon(maxdsns)
      integer outdsn(maxdsns),ncons(maxdsns),icon
      character*4 inpcon(maxdsns,maxcons)
      real        confac(maxdsns,maxcons)
      real        factor

      character*4 C_conx(maxcons)
      integer     I_conx(maxcons)
      integer     nconx,iconx
      logical     conxfound

c      real dvals(maxdsns,ndaymax)
      real dvals(ndaymax)
      real load(maxdsns)

      integer maxBreaks,nbreaks
      parameter (maxBreaks=53)

      data sdate /0,1,1,0,0,0/
      data edate /0,12,31,24,0,0/

      character*100 filename

      character*300 dline

      character*10 xdline
      integer      lenxdline

      logical comment
      external comment

      integer julian
      external julian

      character*50  rpafnam
      integer       lenrpafnam

      character*50  s2rfnam
      integer       lens2rfnam

      integer       maxlrsegs
      parameter     ( maxlrsegs = 3000 )
      integer       nlrsegs,ilrsegs
      character*19  lrsegs(maxlrsegs)
      logical       lrsegfound

      integer       year
      character*3   lux
      character*4   nutx
      real          tload(12)
      logical       nutxfound

      integer       nyears
      integer       years(maxnyears)
      logical       yearfound

      integer       lyear,hyear
      parameter     (lyear = 1980 )
      parameter     (hyear = 2025 )

      real          lrsegloadinp(maxlrsegs,maxcons,lyear:hyear,12)
      real          lrsegloadout(maxlrsegs,maxdsns,lyear:hyear,12)
      real          s2rfac(maxlrsegs,maxdsns)

      character*25  rpascen
      integer       lenrpascen

      character*300 command




      character*4   SPECIES1,SPECIES2
      real          CVP2N, CVN2BOD
      integer       ibodr,ironr,iropr
      character*4   C4_con
      logical       dsnfound


      integer       debug
      parameter     ( debug = 1 )

      character*3  C3lu
      parameter    (C3lu = 'pas')