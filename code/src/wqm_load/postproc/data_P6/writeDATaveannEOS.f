************************************************************************
**  writes out the edge-of-stream loads by year                       **
************************************************************************
      subroutine  writeDATaveannEOS
     .              (rscen,onelseg,rseg,eosfil,sdate,
     .               dothislu,acres,type,
     .               ndays,nloads,loadname,dload,
     .               year1,year2)
     
      implicit none
      include 'data.inc'

      integer eosfil
      character*(*) onelseg    ! land segment
      character*(*) type  ! ps, atdep, or sep
      integer lentype

      integer year1,year2
      character*4 cyear1,cyear2

      integer ndays,year,month,day
  
      logical dothislu  ! does this land use have loads?

      real dload(ndaymax,nloadmax)
      double precision aveload(nloadmax)

      real acres(ndaymax)
      double precision aveacre

      integer nl,nd   ! indices

      integer denom
      integer ndaysinyear
      external ndaysinyear

************** END DECLARATION **************************************
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      call lencl(onelseg,lenlseg)
      write(cyear1,'(i4)') year1
      write(cyear2,'(i4)') year2
      call lencl(type,lentype)
      
      fnam = outdir//'eos/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//type(:lentype)//'_'//cyear1//'-'//cyear2//'.ave'
      open(eosfil,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      if (.not.dothislu) then   ! if no loads
        write(eosfil,'(a100)',err=951) 'NO LOADS'
        return ! get next land use
      end if

      if (type(:3).eq.atdep) then
        write(eosfil,1001,err=951) 'YEARS         ',
     .                             (loadname(nl),nl=1,nloads),'ACRE'
      else
        write(eosfil,1001,err=951) 'YEARS         ',
     .                             (loadname(nl),nl=1,nloads)
      end if

      year = sdate(1)
      month = sdate(2)
      day = sdate(3)
      nd = 1

      if (year.gt.year1) go to 992

      do nl = 1,nloads
        aveload(nl) = 0.0
      end do
      aveacre = 0.0

      do while (year.lt.year1)
        nd = nd + 1
        call tomorrow(year,month,day)
      end do
        
      do while (year.le.year2)
        do nl = 1,nloads
          aveload(nl) = aveload(nl) + dload(nd,nl)
        end do
        aveacre = aveacre + acres(nd)
        nd = nd + 1
        if (nd.gt.ndays+1) go to 993
        call tomorrow(year,month,day)
      end do
        
      do nl = 1,nloads
        aveload(nl) = aveload(nl) / real(year2-year1+1)
      end do

      denom = 0
      do year = year1,year2
        denom = denom + ndaysinyear(year)
      end do
      aveacre = aveacre / real(denom)

      if (type(:3).eq.atdep) then
        write(eosfil,1234,err=951) year1,year2,
     .                             (aveload(nl),nl=1,nloads),aveacre
      else
        write(eosfil,1234,err=951) year1,year2,(aveload(nl),nl=1,nloads)
      endif

      close (eosfil)

      return

1001  format(a15,',',25(5x,a4,','))
1234  format(i5,'-',i4,25(',',e14.7))

**************ERROR SPACE ******************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   write(report(1),*)'postproc script requested start year ',year1
      write(report(2),*)' but simulation starts in ',year
      report(3) = ' '
      go to 999
     
993   write(report(1),*)'postproc script requested end year ',year2
      write(report(2),*)' but simulation ends in ',year-1
      report(3) = ' '
      go to 999
     
999   call stopreport(report)

      end
