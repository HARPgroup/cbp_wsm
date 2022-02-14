************************************************************************
**  This subroutine gets all river loads upstream of a segment.       **
**    It finds the upstream segments, then loops over them calling    **
**    the subroutine getsegload                                       **
************************************************************************
      subroutine getupstreammon(rseg,lenrseg,rscen,lenrscen,
     I                          ndel,delname,sdate,edate,
     M                          upmon)
      implicit none
      include 'tfs.inc'
      integer i,nd,ny,nm

      call getupstream(rseg,rscen,lenrscen,
     O                 upstream,nup)

      do i = 1,nup
        call lencl(upstream(i),lenup(i))
      end do

      do i = 1,nup
        call getrsegmon(upstream(i),lenup(i),rscen,lenrscen,
     .                  ndel,delname,sdate,edate,
     O                  tempmon)

        do nd = 1,ndel    
          do ny = sdate(1),edate(1)
            do nm = 1,12
              upmon(nd,ny,nm) = upmon(nd,ny,nm) + tempmon(nd,ny,nm)
            end do
          end do
        end do

      end do

      return

      end

************************************************************************
      subroutine getupstreamann(rseg,lenrseg,rscen,lenrscen,
     I                          ndel,delname,sdate,edate,
     M                          upann)
      implicit none
      include 'tfs.inc'
      integer i,nd,ny

      call getupstream(rseg,rscen,lenrscen,
     O                 upstream,nup)

      do i = 1,nup
        call lencl(upstream(i),lenup(i))
      end do

      do i = 1,nup
        call getrsegann(upstream(i),lenup(i),rscen,lenrscen,
     .                  ndel,delname,sdate,edate,
     O                  tempann)

        do nd = 1,ndel    
          do ny = sdate(1),edate(1)
            upann(nd,ny) = upann(nd,ny) + tempann(nd,ny)
          end do
        end do

      end do

      return

      end

************************************************************************
      subroutine getupstreamave(rseg,lenrseg,rscen,lenrscen,
     I                          ndel,delname,year1,year2,
     M                          upave)
      implicit none
      include 'tfs.inc'
      integer i,nd

      call getupstream(rseg,rscen,lenrscen,
     O                 upstream,nup)

      do i = 1,nup
        call lencl(upstream(i),lenup(i))
      end do

      do i = 1,nup
        call getrsegave(upstream(i),lenup(i),rscen,lenrscen,
     .                  ndel,delname,year1,year2,
     O                  tempave)

        do nd = 1,ndel    
          upave(nd) = upave(nd) + tempave(nd)
        end do

      end do

      return

      end


