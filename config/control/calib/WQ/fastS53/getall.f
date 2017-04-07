************************************************************************
** subroutine get lake flags for all rivers                           **
************************************************************************
      subroutine getlakeflags(
     I                        rsegs,nrsegs,paramscen,lenparamscen,
     O                        lakeflags)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'WQcal.inc'

      logical comment
      external comment
      integer ic,uniqueID,i,lwc,Nexits,nr

      do nr = 1,nrsegs
        lakeflags(nr) = -1
      end do

      fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .       '/gen_info_rseg.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a1)') c  ! get rid of header
      read(dfile,*)Tseg
      do while (Tseg.ne.'end')
        do nr = 1,nrsegs
          if (Tseg.eq.rsegs(nr)) then
            backspace(dfile) 
            read(dfile,*,end=992,err=993) Tseg,Nexits,lakeflags(nr)
          end if
        end do
        read(dfile,*,end=992,err=993) Tseg
      end do

      close (dfile)

      do nr = 1,nrsegs
        if (lakeflags(nr).lt.0) go to 994
      end do

      return

********************** ERROR SPACE *************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

992   report(1) = 'end of file found unexpectedly:'
      report(2) = fnam
      write(report(3),*) 'near line ',Tseg
      go to 999

993   report(1) = 'Problem reading file:'
      report(2) = fnam
      write(report(3),*) 'near line ',Tseg
      go to 999

994   report(1) = 'did not find lake flag for segment '//rsegs(nr)
      report(2) = '  in  file:'
      report(3) = fnam
      go to 999

999   call stopreport(report)
      end


************************************************************************
**    subroutine to get river calibration stats                       **
************************************************************************
      subroutine getoldstats(
     I                       rscen,basin,lenbasin,cy1,cy2,itnum,
     I                       concname,nconcs,
     O                       sim,obs,ksKstat,oldparval)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'WQcal.inc'

      if (itnum.eq.1) return

      call lencl(rscen,lenrscen)
      fnam = tree//'output/river/cfd/'//rscen(:lenrscen)//'/'//
     .             basin(:lenbasin)//'_'//cy1//'_'//cy2//'.bin'
      open(dfile,file=fnam,form='unformatted',status='old',iostat=err)
      if (err.ne.0) go to 991
      read(dfile) sim,obs,ksKstat,oldparval,testend
      if (checkend.ne.testend) go to 992
      close(dfile)

      return

991   report(1) = 'error opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'problem with binary cfd file:'
      report(2) = fnam
      report(3) = 'file or variable size do not match'
      go to 999

999   call stopreport(report)
      end

************************************************************************
** subroutine to read the current calibration statistics              **
************************************************************************
      subroutine readnewstats(
     I                        rscen,basin,lenbasin,csegs,ncsegs,
     I                        cy1,cy2,itnum,
     I                        concname,nconcs,parval,
     M                        sim,obs,ksKstat)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'WQcal.inc'
      character*1 dummy
      integer Tnb,it

      call lencl(rscen,lenrscen)
      do ns = 1,ncsegs
        do nc = 1,nconcs    ! loop over concentrations

          if (concname(nc).eq.'FLOW') then
            ksKstat(ns,nc,itnum) = misval
            do nb = 1,nbinmax
              sim(ns,nc,nb,itnum) = misval
              obs(ns,nc,nb,itnum) = misval
            end do
            cycle
          end if

          fnam = tree//'output/river/cfd/'//rscen(:lenrscen)//'/'//
     .             csegs(ns)//'_'//cy1//'_'//cy2//'.'//concname(nc)
          open(dfile,file=fnam,status='old',iostat=err)
          if (err.ne.0) go to 996  ! file should exist
  
          read(dfile,*,err=991,end=991) ksKstat(ns,nc,itnum)
          read(dfile,*,err=992,end=992) nbins
          read(dfile,*,err=993,end=993) dummy
          do nb = 1,nbins
            read(dfile,*,err=994,end=994) 
     .            Tnb,sim(ns,nc,nb,itnum),obs(ns,nc,nb,itnum)
            if (Tnb.ne.nb) go to 995
          end do
          close(dfile)
        end do

      end do

************ store binary output
      fnam = tree//'output/river/cfd/'//rscen(:lenrscen)//'/'//
     .         basin(:lenbasin)//'_'//cy1//'_'//cy2//'.bin'
      open(dfile,file=fnam,form='unformatted',status='unknown',
     .     iostat=err)
      write(dfile) sim,obs,ksKstat,parval,checkend
      close(dfile)

************ store ASCII output
      do ns = 1,ncsegs
        do nc = 1,nconcs
          if (missing(obs(ns,nc,3,itnum),misval)) cycle
          fnam = tree//'output/river/cfd/'//rscen(:lenrscen)//'/'//
     .           csegs(ns)//'_'//cy1//'_'//cy2//'_iter_stats_'//
     .           concname(nc)//'.csv'
          open(dfile,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(dfile,*)'iteration,Kstat,bias1,bias2,bias3,bias4,bias5'
          do it = 1,itnum
            write(dfile,1234) it,ksKstat(ns,nc,it),
     .           (sim(ns,nc,nb,it)-obs(ns,nc,nb,it),nb=1,nbinmax)
          end do
          close(dfile)
        end do
      end do

      return

1234  format(i3,6(',',f7.3))

*********************** ERROR SPACE ************************************
991   report(1) = 'problem reading komolgorov-smirnov stat in file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'problem reading number of bins '
      report(2) = 'in file'
      report(3) = fnam
      go to 999

993   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = dummy
      go to 999

994   report(1) = 'problem reading file: on line containing simulated:'
      report(2) = fnam
      write(report(3),*) sim(ns,nc,nb,itnum)
      go to 999

995   report(1) = 'problem with file'
      report(2) = fnam
      report(3) = 'bins not in expected place or in wrong order'
      go to 999

996   report(1) = 'can not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

999   call stopreport(report)
      end

************************************************************************
**    subroutine to get river cfd quartiles                           **
************************************************************************
      subroutine getoldsim(
     I                     rscen,basin,lenbasin,cy1,cy2,itnum,
     I                     concname,nconcs,
     O                     noObsSim)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'WQcal.inc'

      if (itnum.eq.1) return

      call lencl(rscen,lenrscen)
      fnam = tree//'output/river/cfd/'//rscen(:lenrscen)//'/all_'//
     .             basin(:lenbasin)//'_'//cy1//'_'//cy2//'.bin'
      open(dfile,file=fnam,form='unformatted',status='old',iostat=err)
      if (err.ne.0) go to 991
      read(dfile) noObsSim,testend
      if (checkend.ne.testend) go to 992
      close(dfile)

      return

991   report(1) = 'error opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'problem with binary cfd file:'
      report(2) = fnam
      report(3) = 'file or variable size do not match'
      go to 999

999   call stopreport(report)
      end

************************************************************************
** subroutine to read the current simulation statistics               **
************************************************************************
      subroutine readnewsim(
     I                      rscen,basin,lenbasin,rsegs,nrsegs,
     I                      cy1,cy2,itnum,
     I                      concname,nconcs,
     M                      noObsSim)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'WQcal.inc'
      character*1 dummy
      integer Tnb,it

      call lencl(rscen,lenrscen)
      do ns = 1,nrsegs
        do nc = 1,nconcs    ! loop over concentrations

          do nb = 1,nbinmax   ! initialize
            noObsSim(ns,nc,nb,itnum) = misval
          end do

          fnam = tree//'output/river/cfd/'//rscen(:lenrscen)//'/'//
     .             rsegs(ns)//'_'//cy1//'_'//cy2//'.'//concname(nc)
          open(dfile,file=fnam,status='old',iostat=err)
          if (err.ne.0) cycle  ! if can't open, move on
  
          read(dfile,*,err=991,end=991) dummy
          read(dfile,*,err=992,end=992) nbins
          read(dfile,*,err=993,end=993) dummy
          do nb = 1,nbins
            read(dfile,*,err=994,end=994) 
     .            Tnb,noObssim(ns,nc,nb,itnum)
            if (Tnb.ne.nb) go to 995
          end do
        end do

      end do

************ store binary output
      fnam = tree//'output/river/cfd/'//rscen(:lenrscen)//'/all_'//
     .         basin(:lenbasin)//'_'//cy1//'_'//cy2//'.bin'
      open(dfile,file=fnam,form='unformatted',status='unknown',
     .     iostat=err)
      write(dfile) noObsSim,checkend
      close(dfile)

      return

1234  format(i3,6(',',f7.3))

*********************** ERROR SPACE ************************************
991   report(1) = 'problem reading komolgorov-smirnov stat in file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'problem reading number of bins '
      report(2) = 'in file'
      report(3) = fnam
      go to 999

993   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = dummy
      go to 999

994   report(1) = 'problem reading file: on line containing simulated:'
      report(2) = fnam
      write(report(3),*) sim(ns,nc,nb,itnum)
      go to 999

995   report(1) = 'problem with file'
      report(2) = fnam
      report(3) = 'bins not in expected place or in wrong order'
      go to 999

999   call stopreport(report)
      end

************************************************************************
**  function to determine if a read value is the missing value        **
************************************************************************
      function missing(x,misval)
      implicit none
      real x,misval
      logical missing
      missing = .false.
      if (abs(x-misval).lt.0.01) missing = .true.
      end
      
