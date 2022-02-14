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
c bhatt ??       aedate(2) = aedate(2) + 1        ! + 1 month
c bhatt: start
        aedate(2) = aedate(2) + 4
c bhatt: end
        call gethourdsn(wdmfil+n,sdate,aedate,dsn,nvals,hval)

        total = 0
        do nv = 1,nvals
          total = total + hval(nv)
        end do

c        if (total.lt.1.) go to 991
c bhatt: start ??
        print*, 'BHATT wdmfnam $$ ', wdmfnam
        print*, 'BHATT total   $$ ', total
        print*, 'BHATT dsn     $$ ', dsn
        print*, 'BHATT start   $$ ', sdate(1), sdate(2), sdate(3)
        print*, 'BHATT aend    $$ ', aedate(1), aedate(2), aedate(3)
        if(total.lt.1.) then
          print*, 'total = ', total
          print*, 'start = ', sdate(1), sdate(2), sdate(3)
          print*, 'aend  = ', edate(1), edate(2), edate(3)
          do nv = 1,nvals
c              write(*,('F A $')) hval(nv), ' '
             write(*,'(e14.7,a,$)') hval(nv), ' , '
          end do
          go to 991
        end if
c bhatt: end
      end if

      return

991   report(1) = 'ERROR:  the wdm file '
      report(2) = wdmfnam
      report(3) = ' Had zero flow for the first month'
      call stopreport(report)

      end

