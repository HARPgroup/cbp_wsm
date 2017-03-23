************************************************************************
**  routine to check that flow exists in the first month, stop if not **
************************************************************************

      subroutine checkflow(wdmfnam,wdmfil,n,maxWATR,nWATRdsns,WATRdsn,
     .                     sdate,edate)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/wdm.inc'

      integer wdmfil,n  ! file number and increment of file number
      integer nv,nd
      integer aedate(ndate),sdate(ndate),edate(ndate)
      real total

      integer maxWATR,nWATRdsns    ! number of DSNs that make up the total flow (multiple exits)
      integer WATRdsn(maxWATR)      ! dsns for flow variables

      integer Rvar

! if less than one month total time skip this subroutine
      if (edate(1).ne.sdate(1).or. edate(2).ne.sdate(2)) return

      do nd = 1,ndate    ! make end date = start date
        aedate(nd) = sdate(nd)
      end do
      aedate(2) = aedate(2) + 1        ! + 1 month

      total = 0.0

      do Rvar = 1,nWATRdsns
        call gethourdsn(wdmfil+n,sdate,aedate,WATRdsn(Rvar),nvals,hval)

        do nv = 1,nvals
          total = total + hval(nv)
        end do
      end do

      if (total.lt.1.) go to 991

      return

991   report(1) = 'ERROR:  the wdm file '
      report(2) = wdmfnam
      report(3) = ' Had zero flow for the first month'
      call stopreport(report)

      end

