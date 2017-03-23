************************************************************************
**  reads from a file and writes a new wdm with similar name          **
************************************************************************
      implicit none

      include '../../lib/inc/standard.inc'
      include '../../lib/inc/wdm.inc'
      integer wdmfil,maxdsns
      parameter (wdmfil=dfile+10)
      parameter (maxdsns=20)

      character *1 answer
      character*6 tunits

      integer i,sdate(ndate),edate(ndate),tstep,ndsns,dsns(maxdsns)
      real hvals(ndaymax*24,maxdsns)
      real dvals(ndaymax,maxdsns)

