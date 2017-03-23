************************************************************************
** subroutine to write transport factors for one river segment        **
************************************************************************
      subroutine writemondfs(rseg,lenrseg,rscen,lenrscen,
     I                       ndel,delname,sdate,edate,
     I                       segDFmon,BasinDFmon,ResDFmon)
      implicit none
      include 'dfs.inc'
      character*10 segDFname(ndelmax),basinDFname(ndelmax),
     .             resDFname(ndelmax)
      integer ndl,ny,nm,nm1,nm2

      fnam = outdir//'del/dfs/monthly/'//rscen(:lenrscen)//
     .       '/'//rseg(:lenrseg)//'_'//rscen(:lenrscen)//'.txt'
      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      do ndl = 1,ndel
        segDFname(ndl) = 'SEG_'//delname(ndl)
        basinDFname(ndl) = 'BASIN_'//delname(ndl)
        resDFname(ndl) = 'RES_'//delname(ndl)
      end do

      write(11,123,err=951) 'year,month',(segDFname(ndl),ndl=1,ndel),
     .                                   (basinDFname(ndl),ndl=1,ndel),
     .                                   (resDFname(ndl),ndl=1,ndel)
      do ny = sdate(1),edate(1)
        nm1 = 1
        if (ny.eq.sdate(1)) nm1 = sdate(2)
        nm2 = 12
        if (ny.eq.edate(1)) nm2 = edate(2)
        do nm = nm1,nm2
          write(11,1234,err=951) ny,nm,(segDFmon(ndl,ny,nm),ndl=1,ndel),
     .                               (basinDFmon(ndl,ny,nm),ndl=1,ndel),
     .                                 (resDFmon(ndl,ny,nm),ndl=1,ndel)
        end do
      end do
      close(11)

      return

123   format(a11,30(',',a4))
1234  format(i4,',',i3,30(',',e14.7))

************************* ERROR SPACE
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = '  Problem opening '
      report(2) = fnam
      report(3) = 'error code = '
      write(report(3)(14:16),'(i3)')err
      go to 999
999   call stopreport(report)
      end

************************************************************************
      subroutine writeanndfs(rseg,lenrseg,rscen,lenrscen,
     I                       ndel,delname,sdate,edate,
     I                       segDFann,BasinDFann,ResDFann)
      implicit none
      include 'dfs.inc'
      character*10 segDFname(ndelmax),basinDFname(ndelmax),
     .             resDFname(ndelmax)
      integer ndl,ny

      fnam = outdir//'del/dfs/annual/'//rscen(:lenrscen)//
     .       '/'//rseg(:lenrseg)//'_'//rscen(:lenrscen)//'.txt'
      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      do ndl = 1,ndel
        segDFname(ndl) = 'SEG_'//delname(ndl)
        basinDFname(ndl) = 'BASIN_'//delname(ndl)
        resDFname(ndl) = 'RES_'//delname(ndl)
      end do

      write(11,123,err=951) 'year',(segDFname(ndl),ndl=1,ndel),
     .                              (basinDFname(ndl),ndl=1,ndel),
     .                              (resDFname(ndl),ndl=1,ndel)
      do ny = sdate(1),edate(1)
        write(11,1234,err=951) ny,(segDFann(ndl,ny),ndl=1,ndel),
     .                            (basinDFann(ndl,ny),ndl=1,ndel),
     .                            (resDFann(ndl,ny),ndl=1,ndel)
      end do
      close(11)

      return

123   format(a4,5x,30(',',a4))
1234  format(i4,30(',',f14.7))

************************* ERROR SPACE
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = '  Problem opening '
      report(2) = fnam
      report(3) = 'error code = '
      write(report(3)(14:16),'(i3)')err
      go to 999
999   call stopreport(report)
      end

************************************************************************
      subroutine WriteAveDFs(rseg,rscen,lenrscen,
     I                       ndel,delname,year1,year2,
     I                       segDFave,BasinDFave,ResDFave)
      implicit none
      include 'dfs.inc'
      integer ndl,ny
      character*4,cy1,cy2
      character*10 segDFname(ndelmax),basinDFname(ndelmax),
     .             resDFname(ndelmax)

      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2

      fnam = outdir//'del/dfs/aveann/'//rscen(:lenrscen)//
     .       '/'//rseg//'_'//rscen(:lenrscen)//
     .       '_'//cy1//'_'//cy2//'.txt'
      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      do ndl = 1,ndel
        segDFname(ndl) = 'SEG_'//delname(ndl)
        basinDFname(ndl) = 'BASIN_'//delname(ndl)
        resDFname(ndl) = 'RES_'//delname(ndl)
      end do

      write(11,123,err=951) 'years',(segDFname(ndl),ndl=1,ndel),
     .                              (basinDFname(ndl),ndl=1,ndel),
     .                              (resDFname(ndl),ndl=1,ndel)
      write(11,1234,err=951) year1,year2,(segDFave(ndl),ndl=1,ndel),
     .                                   (basinDFave(ndl),ndl=1,ndel),
     .                                   (resDFave(ndl),ndl=1,ndel)
      close(11)

      return

123   format(a5,30(',',a10))
1234  format(i4,'-',i4,30(',',f14.7))

************************* ERROR SPACE
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = '  Problem opening '
      report(2) = fnam
      report(3) = 'error code = '
      write(report(3)(14:16),'(i3)')err
      go to 999
999   call stopreport(report)
      end

