************************************************************************
**  routine to check that flow exists in the first month, stop if not **
************************************************************************

      subroutine checkflow(wdmfnam,wdmfil,n,dsn,sdate,edate)
      implicit none
      include 'stream_wdm.inc'

      integer wdmfil,n,nv,nd
      integer aedate(ndate)

      real total

! if less than one month total time skip this subroutine
      if (edate(1).ne.sdate(1).or. edate(2).ne.sdate(2)) then

        do nd = 1,ndate    ! make end date = start date
          aedate(nd) = sdate(nd)
        end do
        aedate(2) = aedate(2) + 1        ! + 1 month

        call gethourdsn(wdmfil+n,sdate,aedate,dsn,nvals,hval)

        total = 0
        do nv = 1,nvals
          total = total + hval(nv)
        end do

        if (total.lt.1.) go to 991

      end if

      return

991   report(1) = 'ERROR:  the wdm file '
      report(2) = wdmfnam
      report(3) = ' Had zero flow for the first month'
      call stopreport(report)

      end

