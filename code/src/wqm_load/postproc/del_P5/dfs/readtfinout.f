************************************************************************
** subroutine to read and multiply transport factors for one segment  **
************************************************************************
      subroutine monreadTFIO(Tseg,rscen,lenrscen,
     I                      sdate,edate,ndel,delname,
     O                      tfmon,inmon,outmon)
      implicit none
      include 'dfs.inc'
      integer ndl,ny,nm,nm1,nm2,Tny,Tnm
      real tf(ndelmax)      ! transport factor for this segment
      character*4 cdum
************************** 
      fnam = outdir//'del/tfs/monthly/'//rscen(:lenrscen)//
     .       '/'//Tseg//'_'//rscen(:lenrscen)//'.txt'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read (dfile,*,end=993,err=994) cdum  ! get rid of header

      do ny = sdate(1),edate(1)
        nm1 = 1
        if (ny.eq.sdate(1)) nm1 = sdate(2)
        nm2 = 12
        if (ny.eq.edate(1)) nm2 = edate(2)
        do nm = nm1,nm2
          read(dfile,*,end=993,err=994) Tny,Tnm,
     .                (tfmon(ndl,ny,nm),ndl=1,ndel),
     .                (inmon(ndl,ny,nm),ndl=1,ndel),
     .                (outmon(ndl,ny,nm),ndl=1,ndel)
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
      subroutine annreadTFIO(Tseg,rscen,lenrscen,
     I                      sdate,edate,ndel,delname,
     O                      tfann,inann,outann)
      implicit none
      include 'dfs.inc'
      integer ndl,ny,Tny
      real tf(ndelmax)      ! transport factor for this segment
      character*4 cdum
************************** 
      fnam = outdir//'del/tfs/annual/'//rscen(:lenrscen)//
     .       '/'//Tseg//'_'//rscen(:lenrscen)//'.txt'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read (dfile,*,end=993,err=994) cdum  ! get rid of header

      do ny = sdate(1),edate(1)
        read(dfile,*,end=993,err=994) Tny,(tfann(ndl,ny),ndl=1,ndel),
     .                                    (inann(ndl,ny),ndl=1,ndel),
     .                                    (outann(ndl,ny),ndl=1,ndel)
        if (Tny.ne.ny) go to 992
      end do
      close(dfile)
      return

************************* ERROR SPACE
991   report(1) = '  Problem opening '
      report(2) = fnam
      report(3) = 'error code = '
      write(report(3)(14:16),'(i3)')err
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
      subroutine avereadTFIO(Tseg,rscen,lenrscen,
     I                       year1,year2,ndel,delname,
     O                       tfave,inave,outave)
      implicit none
      include 'dfs.inc'
      integer ndl
      real tf(ndelmax)      ! transport factor for this segment
      character*4 cdum
      character*4,cy1,cy2

************************** 
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2
      fnam = outdir//'del/tfs/aveann/'//rscen(:lenrscen)//
     .       '/'//Tseg//'_'//rscen(:lenrscen)//
     .       '_'//cy1//'_'//cy2//'.txt'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read (dfile,*,end=993,err=994) cdum  ! get rid of header

      read(dfile,*,end=993,err=994) cdum,(tfave(ndl),ndl=1,ndel),
     .                                   (inave(ndl),ndl=1,ndel),
     .                                   (outave(ndl),ndl=1,ndel)
      close(dfile)
      return

************************* ERROR SPACE
991   report(1) = '  Problem opening '
      report(2) = fnam
      report(3) = 'error code = '
      write(report(3)(14:16),'(i3)')err
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

