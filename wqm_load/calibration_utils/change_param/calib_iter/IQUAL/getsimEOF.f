************************************************************************
** subroutine get simulated EOF from summarized pltgens               **
************************************************************************
      subroutine getsimEOF(
     I                     TName,clu,lscen,version,lsegs,nlsegs,
     O                     Tval)
      implicit none
      include 'iqual.inc'

      character*4 Tname,Tempname  ! target name
      real Tval(maxlsegs)      ! target values
      character*(*) version

      character*300 dline
      integer order        ! index of column to search
      integer i,ns,nc
      character*6 Tlseg
      character*3 clu
      character*100 fnam2
      
      logical foundseg(maxlsegs)

**************END DECLARATION ******************************************

************ open EOF pltgen summary file
      call lencl(lscen,lenlscen)
      fnam = outdir//'pltgen/land/'//lscen(:lenlscen)//
     .       '/aveann_summary_'//clu//'.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

************ open copy file for records
      fnam2 = outdir//'pltgen/land/'//lscen(:lenlscen)//
     .        '/aveann_summary_'//clu//'_'//version//'.csv'
      open (dfile+1,file=fnam2,status='unknown',iostat=err)
      if (err.ne.0) go to 990

      read(dfile,'(a300)',err=996)dline            ! read header line
      call d2x(dline,i)
      call rytdos(dline,dfile+1)
      call shift(dline)
      read(dline,*,err=989) Tempname

      order = 1        ! find order
      do while (Tempname.ne.Tname)
        order = order + 1
        call shift(dline)
        read(dline,*,err=989) Tempname
        if (Tempname.eq.'    ') go to 992
      end do

      do ns = 1, nlsegs
        foundseg(ns) = .false.
      end do

*************** loop over all segments in file, look for active segs
      read(dfile,'(a300)',err=996)dline            ! read header line
      call d2x(dline,i)
      call rytdos(dline,dfile+1)
      do 
        read(dline,*) Tseg
        do ns = 1,nlsegs
          if (lsegs(ns).eq.Tseg) then
            foundseg(ns) = .true.
            if (dline(len(dline)-3:len(dline)).ne.'    ') go to 995
            do nc = 1,order
              call shift(dline)
            end do
            read(dline,*,err=993) Tval(ns)
            exit
          end if
        end do
        read(dfile,'(a300)',err=996,end=111)dline    ! read next line
        call d2x(dline,i)
        call rytdos(dline,dfile+1)
      end do
        
111   close (dfile)
      close (dfile+1)

      do ns = 1, nlsegs
        if (.not.foundseg(ns)) go to 994
      end do

      return

**************** ERROR SPACE *******************************************
989   report(1) = 'problem reading summary file, near line'
      report(2) = fnam
      report(3) = dline
      go to 999

990   report(1) = 'could not open file'
      report(2) = fnam2
      write(report(3),*) 'error = ',err
      go to 999

991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'could not find EOF value '//TName//' in file'
      report(2) = fnam
      report(3) = dline
      go to 999

993   report(1) = 'error reading target value in file'
      report(2) = fnam
      report(3) = dline
      go to 999

994   report(1) = 'did not find segment '//lsegs(ns)//' in file'
      report(2) = fnam
      report(3) = line
      go to 999

995   report(1) = 'data line too long, modify '
      report(2) = fnam
      report(3) = ' or adjust dline in ./pp/src/.../IQUAL/getsimEOF.f'
      go to 999

996   report(1) = 'error reading file'
      report(2) = fnam
      report(3) = dline
      go to 999

999   call stopreport(report)
      end 

************************************************************************
** subroutine get simulated EOF from summarized pltgens               **
************************************************************************
      subroutine getsimEOFnv(
     I                     TName,clu,lscen,lsegs,nlsegs,
     O                     Tval)
      implicit none
      include 'iqual.inc'

      character*4 Tname,Tempname  ! target name
      real Tval(maxlsegs)      ! target values

      character*300 dline
      integer order        ! index of column to search
      integer i,ns,nc
      character*6 Tlseg
      character*3 clu
      character*100 fnam2
      
      logical foundseg(maxlsegs)

**************END DECLARATION ******************************************

************ open EOF pltgen summary file
      call lencl(lscen,lenlscen)
      fnam = outdir//'pltgen/land/'//lscen(:lenlscen)//
     .       '/aveann_summary_'//clu//'.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a300)',err=996)dline            ! read header line
      call d2x(dline,i)
      call shift(dline)
      read(dline,*,err=989) Tempname

      order = 1        ! find order
      do while (Tempname.ne.Tname)
        order = order + 1
        call shift(dline)
        read(dline,*,err=989) Tempname
        if (Tempname.eq.'    ') go to 992
      end do

      do ns = 1, nlsegs
        foundseg(ns) = .false.
      end do

*************** loop over all segments in file, look for active segs
      read(dfile,'(a300)',err=996)dline            ! read header line
      call d2x(dline,i)
      do 
        read(dline,*) Tseg
        do ns = 1,nlsegs
          if (lsegs(ns).eq.Tseg) then
            foundseg(ns) = .true.
            if (dline(len(dline)-3:len(dline)).ne.'    ') go to 995
            do nc = 1,order
              call shift(dline)
            end do
            read(dline,*,err=993) Tval(ns)
            exit
          end if
        end do
        read(dfile,'(a300)',err=996,end=111)dline    ! read next line
        call d2x(dline,i)
      end do
        
111   close (dfile)

      do ns = 1, nlsegs
        if (.not.foundseg(ns)) go to 994
      end do

      return

**************** ERROR SPACE *******************************************
989   report(1) = 'problem reading summary file, near line'
      report(2) = fnam
      report(3) = dline
      go to 999

990   report(1) = 'could not open file'
      report(2) = fnam2
      write(report(3),*) 'error = ',err
      go to 999

991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'could not find EOF value '//TName//' in file'
      report(2) = fnam
      report(3) = dline
      go to 999

993   report(1) = 'error reading target value in file'
      report(2) = fnam
      report(3) = dline
      go to 999

994   report(1) = 'did not find segment '//lsegs(ns)//' in file'
      report(2) = fnam
      report(3) = line
      go to 999

995   report(1) = 'data line too long, modify '
      report(2) = fnam
      report(3) = ' or adjust dline in ./pp/src/.../IQUAL/getsimEOF.f'
      go to 999

996   report(1) = 'error reading file'
      report(2) = fnam
      report(3) = dline
      go to 999

999   call stopreport(report)
      end 

