************************************************************************
**  routine to add together one dsn from two wdms and store in the    **
**    first wdm                                                       **
************************************************************************

      subroutine addRwdm(wdmfnam,wdmfil,n,dsnIn,dsnOut,sdate,edate)
      implicit none
      include 'stream_wdm.inc'

      integer wdmfil,n,nv,dsnIn,dsnOut
      integer asdate(ndate),aedate(ndate)

      real uphval(ndaymax*24)                 ! temp variable

      call wtdate(wdmfil+n,1,dsnOut,2,asdate,aedate,err)

      call checktime(wdmfnam,sdate,asdate,edate,aedate,dsnOut)

      call gethourdsn(wdmfil,sdate,edate,dsnIn,nvals,hval)

      call gethourdsn(wdmfil+n,sdate,edate,dsnOut,nvals,uphval)
                      
      do nv = 1,nvals
        hval(nv) = hval(nv) + uphval(nv)
      end do

      call puthourdsn(wdmfil,sdate,edate,dsnIn,nvals,hval)

      end

