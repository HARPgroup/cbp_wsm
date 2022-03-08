************************************************************************
**  reads from a file and writes a new wdm with similar name          **
************************************************************************
      implicit none

      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'
      integer wdmfil,maxdsns
      parameter (wdmfil=dfile+10)
      parameter (maxdsns=20)

      character *1 answer
      character*6 tunits

      integer minyear,maxyear
      parameter (minyear=1980,maxyear=2014)

      integer i,sdate(ndate),edate(ndate),tstep,ndsns,dsns(maxdsns)

      integer ndmax
      parameter (ndmax=366*maxyear-366*minyear)
      real hvals(ndmax*24,maxdsns)
      real dvals(ndmax,maxdsns)

      logical debug
      parameter(debug=.true.)
