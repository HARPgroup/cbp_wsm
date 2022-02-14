************************************************************************
** routine to make sure that the pair (asdate,aedate) are within the  **
**   pair (sdate,edate)                                               **
************************************************************************

      subroutine checktime(wdmfnam,sdate,asdate,edate,aedate,dsn)

      integer sdate(6),asdate(6),edate(6),aedate(6)
      integer ichk,dsn
      character*64 report(3),wdmfnam

      integer timchk         ! function in lib3.2
      external timchk

      ichk = timchk(sdate,asdate)
      if (ichk.gt.0) then
        report(1)=wdmfnam
        report(2)='NEED yyyymmddhhmmss HAVE yyyymmddhhmmss'
        write(report(2)(6:19),'(i4,5i2)') sdate
        write(report(2)(26:39),'(i4,5i2)') asdate
        report(3)=' for DSN '
        write(report(3)(10:13),'(i4)')dsn
        call stopreport(report)
      end if
      ichk = timchk(edate,aedate)
      if (ichk.lt.0) then
        report(1)=wdmfnam
        report(2)='NEED yyyymmddhhmmss HAVE yyyymmddhhmmss'
        write(report(2)(6:19),'(i4,5i2)') edate
        write(report(2)(26:39),'(i4,5i2)') aedate
        report(3)=' for DSN '
        write(report(3)(10:13),'(i4)')dsn
        call stopreport(report)
      end if

      end
