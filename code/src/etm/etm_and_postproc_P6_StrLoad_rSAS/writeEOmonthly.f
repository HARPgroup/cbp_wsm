************************************************************************
**  writes out the edge-of-stream loads by year                       **
************************************************************************
      subroutine  writeEOmonthly
     .              (rscen,onelseg,rseg,eosfil,sdate,
     .               lu,dothislu,acres,
     .               type,ndays,nloads,loadname,dload)
     
      implicit none
      include 'land.inc'

      integer eosfil
      character*(*) onelseg    ! land use segment
      character*(*) type  ! eos or eof
      character*(*) lu   ! land use name

      integer ndays,year,month,day
      integer oldyear,oldmonth
  
      integer sdate(ndate)   ! start date

      logical dothislu  ! does this land use have loads?

      real dload(ndaymax,nloadmax)
      double precision monload(nloadmax)

      real acres(ndaymax)
      double precision monacre

      integer nl,nd   ! indices

      integer ndaysinmonth
      external ndaysinmonth

************** END DECLARATION **************************************

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      call lencl(onelseg,lenlseg)

      fnam = outdir//type//'/monthly/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//lu//'.mon'
      open(eosfil,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      if (.not.dothislu) then   ! if no loads
        write(eosfil,'(a8)',err=951) 'NO LOADS'
        return ! get next land use
      end if

      write(eosfil,1001,err=951) 'YEAR,MONTH    ',
     .                            (loadname(nl),nl=1,nloads),'ACRE'

      year = sdate(1)
      month = sdate(2)
      day = sdate(3)
      oldyear = year
      oldmonth = month
      do nl = 1,nloads
        monload(nl) = 0.0
      end do
      monacre = 0.0

      do nd = 1,ndays

        do nl = 1,nloads
          monload(nl) = monload(nl) + dload(nd,nl)
        end do
        monacre = monacre + acres(nd)

        call tomorrow(year,month,day)

        if (month.ne.oldmonth) then
          monacre = monacre / real(ndaysinmonth(oldyear,oldmonth))
          write(eosfil,1234,err=951) oldyear,oldmonth,
     .                               (monload(nl),nl=1,nloads),monacre
          oldyear = year
          oldmonth = month
          do nl = 1,nloads
            monload(nl) = 0.0
          end do
          monacre = 0.0
        end if

      end do

      close (eosfil)

      return

1001  format(a15,',',25(5x,a4,','))
1234  format(i5,',',i5,25(',',e14.7))

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
