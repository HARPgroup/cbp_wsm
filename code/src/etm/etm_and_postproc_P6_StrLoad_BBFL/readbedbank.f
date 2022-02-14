      subroutine readbedbank(rscen,lenrscen,rseg,lenrseg,l2r,numsegs,
     O                       lrbedbank,sYRbb,eYRbb)

      implicit none
      include 'land.inc'

      character*25 bbscen
      integer lenbbscen
      character*4 ccols(ncstr)
      integer     icols(ncstr),i
      real    rtemp(ncstr)
      integer ns,nl2r
      character*13 trseg
      character*6  tlseg

***   1. READ THE TABLE ENTRY FOR BED/BANK
      call readcontrol_Rbedbank(rscen,lenrscen,sYRbb,eYRbb,bbscen)
      call lencl(bbscen,lenbbscen)
      print*,sYRbb,eYRbb,' ',bbscen

      fnam = ScenDatDir//'river/bedbank/bedbankload_'//
     .        bbscen(:lenbbscen)//'.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,*) trseg,tlseg,(ccols(i),i=1,ncstr)! header
      do i = 1,ncstr
         do icstr = 1,ncstr
            if ( ccols(i) .eq. ncstrname(icstr) ) then
               icols(i) = icstr
            end if
         end do
      end do

***   2. READ THE LOADS FOR ALL LRSEGS
      do ns = 1,numsegs
         do i = 1,ncstr
            lrbedbank(ns,i) = 0.0
         end do
      end do
      call lencl(l2r(1),lenlseg)

      nl2r = 0
      do while ( nl2r .lt. numsegs )
         read(dfile,*) trseg,tlseg,(rtemp(i),i=1,ncstr)
         do ns = 1,numsegs
            if (trseg//tlseg .eq. rseg(:lenrseg)//l2r(ns)(:lenlseg))then
               nl2r = nl2r + 1
               do i = 1,ncstr
                  lrbedbank(ns,icols(i)) = rtemp(i)
               end do
            end if
         end do
      end do

      do ns = 1,numsegs
         print*,'BB: ',rseg(:lenrseg),' ',l2r(ns)(:lenlseg),
     .          (lrbedbank(ns,i),i=1,ncstr)
      end do

      return


991   report(1) = 'Problem: unable to open file'
      report(2) = fnam
      report(3) = 'from subroutine readbedbank'
      go to 999

999   call stopreport(report)

      end
