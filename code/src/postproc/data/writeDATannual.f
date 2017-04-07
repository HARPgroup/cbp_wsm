************************************************************************
**  writes out the edge-of-stream loads by year                       **
************************************************************************
      subroutine  writeDATannual
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
      integer oldyear
  
      logical dothislu  ! does this land use have loads?

      real dload(ndaymax,nloadmax)
      double precision annload(nloadmax)

      real acres(ndaymax)
      double precision annacre

      integer nl,nd   ! indices

      integer ndaysinyear
      external ndaysinyear

************** END DECLARATION **************************************

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      call lencl(onelseg,lenlseg)
      call lencl(type,lentype)

      fnam = outdir//'eos/annual/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//type(:lentype)//'.ann'
      open(eosfil,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      if (.not.dothislu) then   ! if no loads
        write(eosfil,'(a100)',err=951) 'NO LOADS'
        return ! get next land use
      end if

      if (type(:5).eq.'atdep') then
        write(eosfil,1001,err=951) 'YEAR          ',
     .                             (loadname(nl),nl=1,nloads),'ACRE'
      else
        write(eosfil,1001,err=951) 'YEAR          ',
     .                              (loadname(nl),nl=1,nloads)
      end if

      year = sdate(1)
      month = sdate(2)
      day = sdate(3)
      oldyear = year
      do nl = 1,nloads
        annload(nl) = 0.0
      end do
      annacre = 0.0

      do nd = 1,ndays

        do nl = 1,nloads
          annload(nl) = annload(nl) + dload(nd,nl)
        end do
        annacre = annacre + acres(nd)

        call tomorrow(year,month,day)

        if (year.ne.oldyear) then
          annacre = annacre / real(ndaysinyear(oldyear))
          if (type(:5).eq.'atdep') then
            write(eosfil,1234,err=951) oldyear,
     .                                 (annload(nl),nl=1,nloads),annacre
          else
            write(eosfil,1234,err=951) oldyear,(annload(nl),nl=1,nloads)
          end if
          oldyear = year
          do nl = 1,nloads
            annload(nl) = 0.0
          end do
          annacre = 0.0
        end if

      end do

      close (eosfil)

      return

1001  format(a15,',',25(5x,a4,','))
1234  format(i5,25(',',e14.7))

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
