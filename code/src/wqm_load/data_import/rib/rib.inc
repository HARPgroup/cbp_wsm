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

      integer maxBreaks,nbreaks
      parameter (maxBreaks=53)

      real nbrib(maxBreaks)  ! rib break points 
      integer BJday(maxBreaks) ! julian day of the breaks from start

      integer nvars_inp
      parameter ( nvars_inp = 5 )
      character*4 loadnm(nvars_inp)
      data loadnm /'nh3n','no3n','orgn','po4p','orgp'/
      character*4 tmpnm(nvars_inp)
      integer toindx(nvars_inp)

      integer nvars_out
      parameter ( nvars_out = 6 )
      character*4 ribnm(nvars_out)
      data ribnm /'NH3X','NO3X','ORNX','PO4X','ORPX','BODX'/
      real NH3X,NO3X,ORNX,PO4X,ORPX,BODX

      integer   ribdsn(nvars_out)
      data ribdsn /3012,3013,3014,3015,3016,3017/

      integer lowyear,highyear
      parameter (lowyear = 1981, highyear = lowyear + maxBreaks)
      real Trib(nvars_inp)
      real annload(lowyear:highyear,nvars_inp,maxnlrsegs) ! input loads
      real annrib(lowyear:highyear,nvars_out,maxnlrsegs)  ! RIB wdm loads

      integer maxcons
      parameter ( maxcons = nvars_inp )
      character*4 C_conx(maxcons)
      integer     nconx,iconx

      character*300 dline
      character*10 xdline
      integer      lenxdline

      logical      lrsegfound

      character*50  s2rfnam
      integer       lens2rfnam
      real          s2rfac(maxnlrsegs,nvars_inp)

      character*3  C3lu
      parameter    (C3lu = 'mtg')

