************************************************************************
** Program to find the delivery factors for an entire basin.  Works   **
**   on individual segments, however.                                 **
**                                                                    **
**   Find the pourpoint of any segment, then get the list of segments **
**     upstream of that pourpoint.  Calculate the aggregate DF        **
**                                                                    **
************************************************************************
      subroutine CalcBasinDFann(
     I                          rscen,lenrscen,
     I                          rseg,rsegs,uniqid,dsid,
     I                          uniqindex,nrsegs,
     I                          sdate,edate,ndel,delname,
     O                          basinDFann)
      implicit none
      include 'dfs.inc'

      integer ndl,ns,ny

      character*13 downseg

      logical pourpoint             ! true if this is the pourpoint
      external pourpoint

      integer Tuniqid
      integer allup(maxrsegs) ! indices of upstream segments
      integer nallup,nup  ! number of upstream segments

      real BasinInAnn(ndelmax,sdate(1):edate(1))

*********** find the pour point
      Tseg = rseg
      do
        if (pourpoint(Tseg)) exit
        call downstream(
     I                  Tseg,rsegs,uniqindex,nrsegs,
     O                  downseg)
        Tseg = downseg
      end do

*********** if pourpoint is a black hole, set to zero and quit
      if (Tseg(10:13).eq.'0004') then
        do ny = sdate(1),edate(1)
          do ndl = 1,ndel
            basinDFann(ndl,ny) = 0.
          end do
        end do
        return
      end if

******** find list of upstream segments
      read (Tseg(5:8),'(i4)') Tuniqid
      ns = uniqindex(Tuniqid)
      call FindUpstreamIndices(
     I                         ns,dsid,uniqid,nrsegs,
     O                         allup,nallup)

********** loop over all upsteam and sum the EOS loads
      do ny = sdate(1),edate(1)
        do ndl = 1,ndel
          BasinInAnn(ndl,ny) = 0.0
        end do
      end do
      do nup = 1,nallup
        call annreadTFIO(
     I                   rsegs(allup(nup)),rscen,lenrscen,
     I                   sdate,edate,ndel,delname,
     O                   tfann,inann,outann)
        do ny = sdate(1),edate(1)
          do ndl = 1,ndel
            BasinInAnn(ndl,ny) = BasinInAnn(ndl,ny) + inann(ndl,ny)
          end do
        end do
      end do


********** get the output of the pourpoint
      call annreadTFIO(
     I                 rsegs(allup(1)),rscen,lenrscen,
     I                 sdate,edate,ndel,delname,
     O                 tfann,inann,outann)

********* calculate the DFs
      do ny = sdate(1),edate(1)
        do ndl = 1,ndel
          if (BasinInAnn(ndl,ny).gt.0.0001) then
            basinDFann(ndl,ny) = outann(ndl,ny) / BasinInAnn(ndl,ny)
          else
            basinDFann(ndl,ny) = 1.0
          end if
        end do
      end do

      return

************************ ERROR SPACE *************************

      end
