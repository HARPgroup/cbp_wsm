************************************************************************
** reads a time series from a pltgen                                  **
************************************************************************
      subroutine readplt(
     I                   year1,year2,rscen,lenrscen,rseg,lenrseg,suffix,
     O                   values)
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      integer year1,year2,year,month,day,nv
      integer Ty,Tm,Td,T
      character*4 suffix
      real values(nyearmax*366)
      integer ndaysinmonth
      external ndaysinmonth


      fnam = outdir//'river/daily/'//rscen(:lenrscen)//'/'//
     .       rseg(:lenrseg)//'.'//suffix
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

************ read down to data you want
      year = -1
      do while (year.lt.year1)
        read(dfile,*,err=992,end=993) year
      end do
      backspace dfile

      nv = 0
      do year = year1,year2
        do month = 1,12
          do day = 1,ndaysinmonth(year,month)
            nv = nv + 1
            read(dfile,*,err=994,end=995) Ty,Tm,Td,T,T,values(nv)
            if (Ty.ne.year.or.Tm.ne.month.or.Td.ne.day) go to 996
          end do
        end do
      end do
      close(dfile)

      return

*************** ERROR SPACE ********************************************
991   report(1) = 'Problem with opening file'
      report(2) = fnam
      write(report(3),*) '  Error =    ',err
      go to 999

992   report(1) = 'Problem reading file'
      report(2) = fnam
      report(3) = 'error reading year in first column'
      go to 999

993   report(1) = 'Problem reading file'
      report(2) = fnam
      report(3) = 'whole file read before start date reached'
      go to 999

994   report(1) = 'Problem reading file'
      report(2) = fnam
      report(3) = 'error reading date and value'
      go to 999

995   report(1) = 'Problem reading file'
      report(2) = fnam
      report(3) = 'whole file read before end date reached'
      go to 999

996   report(1) = 'Problem with file'
      report(2) = fnam
      report(3) = 'dates inconsistent or out of order'
      go to 999

999   call stopreport(report)

      end

      


