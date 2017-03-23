************************************************************************
**  writes out the edge-of-stream loads by year                       **
************************************************************************
      subroutine  writeDATdaily
     .              (rscen,onelseg,rseg,eosfil,sdate,
     .               dothislu,acres,type,
     .               ndays,nloads,loadname,dload)
     
      implicit none
      include 'data.inc'

      integer eosfil
      character*(*) onelseg    ! land segment
      character*(*) type  ! ps, atdep, or sep
      integer lentype

      integer ndays,year,month,day
  
      logical dothislu  ! does this land use have loads?

      real dload(ndaymax,nloadmax)

      real acres(ndaymax)

      integer nl,nd   ! indices

************** END DECLARATION **************************************

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      call lencl(onelseg,lenlseg)
      call lencl(type,lentype)

      fnam = outdir//'eos/daily/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//type(:lentype)//'.day'
      open(eosfil,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      if (.not.dothislu) then   ! if no loads
        write(eosfil,'(a100)',err=951) 'NO LOADS'
        return ! get next land use
      end if

      if (type(:5).eq.'atdep') then
        write(eosfil,1001,err=951) 'YEAR,MONTH,DAY',
     .                             (loadname(nl),nl=1,nloads),'ACRE'
      else
        write(eosfil,1001,err=951) 'YEAR,MONTH,DAY',
     .                             (loadname(nl),nl=1,nloads)
      end if

      year = sdate(1)
      month = sdate(2)
      day = sdate(3)

      if (type(:5).eq.'atdep') then
        do nd = 1,ndays
          write(eosfil,1234,err=951) year,month,day,
     .                             (dload(nd,nl),nl=1,nloads),acres(nd)
          call tomorrow(year,month,day)
        end do
      else
        do nd = 1,ndays
          write(eosfil,1234,err=951) year,month,day,
     .                             (dload(nd,nl),nl=1,nloads)
          call tomorrow(year,month,day)
        end do
      end if
      close (eosfil)

      return

1001  format(a15,',',25(5x,a4,','))
1234  format(2(i5,','),i5,25(',',e14.7))

**************ERROR SPACE ******************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end
