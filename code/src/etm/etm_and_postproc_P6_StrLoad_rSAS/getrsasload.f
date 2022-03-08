      subroutine getrsasload(
     I     rscen,lenrscen,rseg,lenrseg,lscen,lenlscen,lseg,lenlseg,
     I     clu,sdate,edate,Lvname,
     I     nvals,
     M     hval)


      implicit none

c      include '../../lib/inc/standard.inc'
c      include '../../lib/inc/locations.inc'
      include 'land.inc'


      integer i,j

      character*3 clu
      character*4 Lvname
      integer sdate(ndate),edate(ndate)

      character*10 C_tdate
      real         R_tval

      character*100 dline
      integer       lendline

      ! H24021PM3_3040_3340_nir.load
      fnam = tree//'code/src/rsas/rsas-0.5.3/lagtime_CBP/'//
     .            'outputs/daily_baseflow_load/'//
     .            lseg(:lenlseg)//rseg(:lenrseg)//'_'//
     .            clu//'.load'
      fnam = outdir//'land/rSAS/'//rscen(:lenrscen)//
     .            '/daily_baseflow_load/'//
     .            lseg(:lenlseg)//rseg(:lenrseg)//'_'//
     .            clu//'.load'
      print*,fnam

      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) return
      print*,'rSAS ',lseg(:lenlseg),rseg(:lenrseg),clu


      do i = 1,nvals/24
         !read(dfile,'(a100)') dline
         !call lencl(dline,lendline)
         !print*, lendline
         !read(dline,*) C_tdate,R_tval
         read(dfile,*) C_tdate,R_tval
         !print*,C_tdate,R_tval
         do j = 1,24
            hval((i-1)*24+j) = R_tval/24
         end do
      end do




      return





      end
