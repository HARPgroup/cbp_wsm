************************************************************************
** subroutine to write transport factors for one river segment        **
************************************************************************
      subroutine writemontfs(rseg,lenrseg,rscen,lenrscen,
     I                       ndel,delname,sdate,edate,
     I                       inmon,outmon,tfmon)
      implicit none
      include 'tfs.inc'
      integer ndl,ny,nm,nm1,nm2,i
      character*7 inname(ndelmax),outname(ndelmax)

      fnam = outdir//'del/tfs/monthly/'//rscen(:lenrscen)//
     .       '/'//rseg(:lenrseg)//'_'//rscen(:lenrscen)//'.txt'
      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      do ndl = 1,ndel
        inname(ndl) = 'IN_'//delname(ndl)
        outname(ndl) = 'OUT'//delname(ndl)
      end do
      write (11,123,err=951) 'year,month',(delname(ndl),ndl=1,ndel),
     .                                    (inname(ndl),ndl=1,ndel),
     .                                    (outname(ndl),ndl=1,ndel)
      do ny = sdate(1),edate(1)
        nm1 = 1
        if (ny.eq.sdate(1)) nm1 = sdate(2)
        nm2 = 12
        if (ny.eq.edate(1)) nm2 = edate(2)
        do nm = nm1,nm2
          write (11,1234,err=951)ny,nm,(tfmon(ndl,ny,nm),ndl=1,ndel),
     .                                 (inmon(ndl,ny,nm),ndl=1,ndel),
     .                                 (outmon(ndl,ny,nm),ndl=1,ndel)
        end do
      end do
      close(11)

      return

123   format(a11,30(',',a7))
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
      subroutine writeanntfs(rseg,lenrseg,rscen,lenrscen,
     I                       ndel,delname,sdate,edate,
     I                       inann,outann,tfann)
      implicit none
      include 'tfs.inc'
      integer ndl,ny,i
      character*7 inname(ndelmax),outname(ndelmax)

      fnam = outdir//'del/tfs/annual/'//rscen(:lenrscen)//
     .       '/'//rseg(:lenrseg)//'_'//rscen(:lenrscen)//'.txt'
      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      do ndl = 1,ndel
        inname(ndl) = 'IN_'//delname(ndl)
        outname(ndl) = 'OUT'//delname(ndl)
      end do
      write(11,123,err=951) 'year',(delname(ndl),ndl=1,ndel),
     .                             (inname(ndl),ndl=1,ndel),
     .                             (outname(ndl),ndl=1,ndel)
      do ny = sdate(1),edate(1)
        write(11,1234,err=951)ny,(tfann(ndl,ny),ndl=1,ndel),
     .                           (inann(ndl,ny),ndl=1,ndel),
     .                           (outann(ndl,ny),ndl=1,ndel)

      end do
      close(11)

      return

123   format(a4,5x,30(',',a7))
1234  format(i4,30(',',e14.7))

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
      subroutine writeavetfs(rseg,lenrseg,rscen,lenrscen,
     I                       ndel,delname,year1,year2,
     I                       inave,outave,tfave)
      implicit none
      include 'tfs.inc'
      integer ndl,ny,i
      character*7 inname(ndelmax),outname(ndelmax)
      character*4,cy1,cy2

      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2

      fnam = outdir//'del/tfs/aveann/'//rscen(:lenrscen)//
     .       '/'//rseg(:lenrseg)//'_'//rscen(:lenrscen)//
     .       '_'//cy1//'_'//cy2//'.txt'
      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      do ndl = 1,ndel
        inname(ndl) = 'IN_'//delname(ndl)
        outname(ndl) = 'OUT'//delname(ndl)
      end do
      write(11,123,err=951) 'years',(delname(ndl),ndl=1,ndel),
     .                              (inname(ndl),ndl=1,ndel),
     .                              (outname(ndl),ndl=1,ndel)
      write(11,1234,err=951) year1,year2,(tfave(ndl),ndl=1,ndel),
     .                                   (inave(ndl),ndl=1,ndel),
     .                                   (outave(ndl),ndl=1,ndel)

      close(11)

      return

123   format(a5,30(',',a7))
1234  format(i4,'-',i4,30(',',e14.7))

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

