************************************************************************
**  Subroutine to find the downstream segment if one exists.          **
**   returns the name of the downstream segment and a logical         **
**   true or false for whether the passed stream is a pour point      **
************************************************************************
      subroutine DownstreamIndex(
     I                           ns,rsegs,uniqindex,nrsegs,
     O                           ds)

      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/rsegs.inc'

      integer downstreamID,ns,ds

      read(rsegs(ns)(10:13),'(i4)') downstreamID

      ds = uniqindex(downstreamID)

      if (ds.lt.1.or.ds.gt.nrsegs) go to 992

      return

********************************** ERROR SPACE *************************
992   report(1) = 'could not find downstream segment for seg:'
      report(2) = rseg
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end
************************************************************************
**  Subroutine to find the downstream segment if one exists.          **
**   returns the name of the downstream segment and a logical         **
**   true or false for whether the passed stream is a pour point      **
************************************************************************
      subroutine downstream(
     I                      rseg,rsegs,uniqindex,nrsegs,
     O                      downseg)

      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/rsegs.inc'

      character*13 downseg
      integer downstreamID,ns,ds

      read(rseg(10:13),'(i4)') downstreamID

      ds = uniqindex(downstreamID)

      if (ds.lt.1.or.ds.gt.nrsegs) go to 992

      downseg = rsegs(ds)

      return

********************************** ERROR SPACE *************************
992   report(1) = 'could not find downstream segment for seg:'
      report(2) = rseg
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end
