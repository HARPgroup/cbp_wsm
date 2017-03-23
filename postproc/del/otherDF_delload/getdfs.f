************************************************************************
** subroutine to read and multiply transport factors for one segment  **
************************************************************************
      subroutine getmondfs(Tseg,lenrseg,rscen,lenrscen,lseg,
     I                      ndel,delname,sdate,edate,
     O                      segDFmon,basinDFmon,resDFmon)
      implicit none
      include 'delload.inc'
      integer ndl,ny,nm,nm1,nm2,Tny,Tnm
      character*4 cdum
************************** 
      call lencl(Tseg,lenrseg)
      fnam = outdir//'del/dfs/monthly/'//rscen(:lenrscen)//'/'//lseg//
     .       '_'//Tseg(:lenrseg)//'_'//rscen(:lenrscen)//'.txt'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read (dfile,*,end=993,err=994) cdum  ! get rid of header

      do ny = sdate(1),edate(1)
        nm1 = 1
        if (ny.eq.sdate(1)) nm1 = sdate(2)
        nm2 = 12
        if (ny.eq.edate(1)) nm2 = edate(2)
        do nm = nm1,nm2
          read(dfile,*,end=993,err=994)Tny,Tnm,
     .                                 (segDFmon(ndl,ny,nm),ndl=1,ndel),
     .                               (basinDFmon(ndl,ny,nm),ndl=1,ndel),
     .                                 (resDFmon(ndl,ny,nm),ndl=1,ndel)
          if (Tny.ne.ny.or.Tnm.ne.nm) go to 992
        end do
      end do
      close(dfile)
      return

************************* ERROR SPACE
991   report(1) = '  Problem opening '
      report(2) = fnam
      report(3) = 'error code = '
      write(report(3)(14:16),'(i3)')err
      print*,fnam
      go to 999

992   report(1) = 'problem reading file'
      report(2) = fnam
      write(report(3),*) 'expecting: ',ny,' ',nm,' read: ',Tny,' ',Tnm
      go to 999

993   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'end of file found unexpectedly'
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'error on read, check code'
      go to 999

999   call stopreport(report)

      end

************************************************************************
************************************************************************
      subroutine getanndfs(Tseg,lenrseg,rscen,lenrscen,lseg,
     I                      ndel,delname,sdate,edate,
     O                      segDFann,basinDFann,resDFann)
      implicit none
      include 'delload.inc'
      integer ndl,ny,Tny
      character*4 cdum
************************** 
      call lencl(Tseg,lenrseg)
      fnam = outdir//'del/dfs/annual/'//rscen(:lenrscen)//'/'//lseg//
     .       '_'//Tseg(:lenrseg)//'_'//rscen(:lenrscen)//'.txt'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read (dfile,*,end=993,err=994) cdum  ! get rid of header

      do ny = sdate(1),edate(1)
        read(dfile,*,end=993,err=994) Tny,
     .                                 (segDFann(ndl,ny),ndl=1,ndel),
     .                               (basinDFann(ndl,ny),ndl=1,ndel),
     .                                 (resDFann(ndl,ny),ndl=1,ndel)
        if (Tny.ne.ny) go to 992
      end do
      close(dfile)
      return

************************* ERROR SPACE
991   report(1) = '  Problem opening '
      report(2) = fnam
      report(3) = 'error code = '
      write(report(3)(14:16),'(i3)')err
      print*,fnam
      go to 999

992   report(1) = 'problem reading file'
      report(2) = fnam
      write(report(3),*) 'expecting: ',ny,' read: ',Tny
      go to 999

993   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'end of file found unexpectedly'
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'error on read, check code'
      go to 999

999   call stopreport(report)

      end

************************************************************************
************************************************************************
      subroutine getavedfs(Tseg,lenrseg,rscen,lenrscen,lseg,
     I                      ndel,delname,year1,year2,
     O                      segDFave,basinDFave,resDFave)
      implicit none
      include 'delload.inc'
      integer ndl
      character*4 cdum
      character*4,cy1,cy2

************************** 
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2
      call lencl(Tseg,lenrseg)
      fnam = outdir//'del/dfs/aveann/'//rscen(:lenrscen)//'/'//lseg//
     .       '_'//Tseg(:lenrseg)//'_'//rscen(:lenrscen)//
     .       '_'//cy1//'_'//cy2//'.txt'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read (dfile,*,end=993,err=994) cdum  ! get rid of header

      read(dfile,*,end=993,err=994) cdum,
     .                                 (segDFave(ndl),ndl=1,ndel),
     .                               (basinDFave(ndl),ndl=1,ndel),
     .                                 (resDFave(ndl),ndl=1,ndel)
      close(dfile)
      return

************************* ERROR SPACE
991   report(1) = '  Problem opening '
      report(2) = fnam
      report(3) = 'error code = '
      write(report(3)(14:16),'(i3)')err
      print*,fnam
      go to 999

993   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'end of file found unexpectedly'
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'error on read, check code'
      go to 999

999   call stopreport(report)

      end

