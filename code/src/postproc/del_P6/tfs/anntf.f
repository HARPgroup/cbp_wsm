************************************************************************
**  This program finds transport factors for each river segment.  The **
***    transport factor is a property of a river segment, not of a    **
***    land segment. It is equal to the total output divided by the   **
***    total input.  By multiplying transport factors downstream, a   **
**     total delivery factor can be found.                            **
**                                                                    **
**  The strategy for this program is to loop over all lsegs in a seg  **
**    adding the associated eos and data inputs, then add the upstream**
**    river segment loads.  All calculations are on load variables in **
**    pp/catalog/iovars/rchres_out_to_load.  This calculation is done **
**    for collective variables, (TN, TP, etc) and then applied to     **
**    constituents.  The file pp/catalog/iovars/delivery_ratio_calc   **
**    contains the relationships between collective and constituents  **
************************************************************************
      subroutine anntf(rseg,lenrseg,IamRiver,numsegs,l2r,rscen,lenrscen,
     I                 doatdep,dops,dosep,dorib,dorpa,sdate,edate,
     I                 ndel,delname,nloads)
      implicit none

      include 'tfs.inc'

      integer i,ndl,ny,nm

      integer numsegs        ! number of land segs for this river

      logical doatdep,dops,dosep,dorib,dorpa,IamRiver

*************************** END DECLARATIONS ***************************

      do ndl = 1,ndel           ! initialize
        do ny = sdate(1),edate(1)
          outann(ndl,ny) = 0.
          inann(ndl,ny) = 0.
          upann(ndl,ny) = 0.0
        end do
      end do

***************** get out loads
      if (IamRiver) then
        call getrsegann(rseg,lenrseg,rscen,lenrscen,
     I                  ndel,delname,sdate,edate,
     O                  outann)
      end if
      
*************** get upstream loads
      if (IamRiver) then
        call getupstreamann(rseg,lenrseg,rscen,lenrscen,
     I                      ndel,delname,sdate,edate,
     M                      upann)
      end if

***************** loop over lsegs in rseg, get EOS, PS, SEP, ATDEP
      do i = 1,numsegs
        call lsegann(rseg,lenrseg,l2r(i),rscen,lenrscen,
     I               ndel,delname,sdate,edate,nloads,
     M               inann)

        call datann(
     I              rseg,lenrseg,l2r(i),rscen,lenrscen,
     I              ndel,delname,sdate,edate,nloads,
     I              dops,doatdep,dosep,dorib,dorpa,
     M              inann)
      end do

*********** if not a river, set output = input
      if (.not.IamRiver) then
        do ny = sdate(1),edate(1)
          do ndl = 1,ndel
            outann(ndl,ny) = inann(ndl,ny)
          end do
        end do
      end if

******** calculate factors
      do ndl = 1,ndel
        do ny = sdate(1),edate(1)
          totalin = inann(ndl,ny) + upann(ndl,ny)
          if (totalin.gt.0.01) then
            tfann(ndl,ny) = outann(ndl,ny) / totalin
          else
            tfann(ndl,ny) = 1.
          end if
        end do
      end do

      call writeanntfs(rseg,lenrseg,rscen,lenrscen,
     I                 ndel,delname,sdate,edate,
     I                 inann,outann,tfann)


      return
      end
