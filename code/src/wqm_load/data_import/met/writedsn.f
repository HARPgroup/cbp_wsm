      subroutine writedailydsn(sdate,edate,dsn,nvals,dval)
      implicit none
      include '../../../inc/standard.inc'
      include '../../../inc/wdm.inc'

      integer sdate(ndate),edate(ndate)

      integer n,dsnget,ifl,ny,nm,nd,nh,i
      logical done

******************** END SPECIFICATIONS ********************************

      write(fnam,*) 'p4met_',dsn,'.prn'
      call noblanks(fnam,last)

      call findopen(ifl)
      open(ifl,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      ny = sdate(1)
      nm = sdate(2)
      nd = sdate(3)
      nh = 1
      done = .false.
      i = 1
      do while (ny.lt.1995)
        call tomorrow(ny,nm,nd)
        i = i + 1
      end do
      do while (.not.done)
        write(ifl,*,err=951) ny,',',nm,',',nd,',',dval(i)
        call tomorrow(ny,nm,nd)
        i = i + 1
        if (ny.gt.1997) done = .true.
      end do

      close(ifl)

      return
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'error opening file'
      report(2) = fnam
      report(3) = ''
      go to 999
      end

************************************************************************
************************************************************************

      subroutine writehourdsn(sdate,edate,dsn,nvals,hval)
      implicit none
      include '../../../inc/standard.inc'
      include '../../../inc/wdm.inc'

      integer sdate(ndate),edate(ndate)

      integer n,dsnget,ifl,ny,nm,nd,nh,i

      logical done

******************** END SPECIFICATIONS ********************************

      write(fnam,*) 'p4met_',dsn,'.prn'
      call noblanks(fnam,last)

      call findopen(ifl)
      open(ifl,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      ny = sdate(1)
      nm = sdate(2)
      nd = sdate(3)
      nh = 1
      done = .false.
      i = 1
      do while (ny.lt.1995)
        call onehour(ny,nm,nd,nh)
        i = i + 1
      end do
      do while (.not.done)
        write(ifl,*,err=951) ny,',',nm,',',nd,',',nh,',',hval(i)
        call onehour(ny,nm,nd,nh)
        i = i + 1
        if (ny.gt.1997) done = .true.
      end do

      close(ifl)
      return
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'error opening file'
      report(2) = fnam
      report(3) = ''
      go to 999

      end

