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
      subroutine avetf(rseg,lenrseg,IamRiver,numsegs,l2r,rscen,lenrscen,
     I                 doatdep,dops,dosep,dorib,dorpa,year1,year2,
     I                 ndel,delname,nloads)
      implicit none

      include 'tfs.inc'

      integer i,ndl,ny,nm

      integer numsegs        ! number of land segs for this river

      logical doatdep,dops,dosep,dorib,dorpa,IamRiver

*************************** END DECLARATIONS ***************************

      do ndl = 1,ndel           ! initialize
        outave(ndl) = 0.
        inave(ndl) = 0.
        upave(ndl) = 0.0
      end do

***************** get out loads
      if (IamRiver) then
        call getrsegave(rseg,lenrseg,rscen,lenrscen,
     I                  ndel,delname,year1,year2,
     O                  outave)
      end if

*************** get upstream loads
      if (IamRiver) then
        call getupstreamave(rseg,lenrseg,rscen,lenrscen,
     I                      ndel,delname,year1,year2,
     M                      upave)
      end if

***************** loop over lsegs in rseg, get EOS, PS, SEP, ATDEP
      do i = 1,numsegs

        call lsegave(rseg,lenrseg,l2r(i),rscen,lenrscen,
     I               ndel,delname,year1,year2,nloads,
     M               inave)

        call datave(
     I              rseg,lenrseg,l2r(i),rscen,lenrscen,
     I              ndel,delname,year1,year2,nloads,
     I              dops,doatdep,dosep,dorib,dorpa,
     M              inave)

      end do

*********** if not a river, set output = input
      if (.not.IamRiver) then
        do ndl = 1,ndel
          outave(ndl) = inave(ndl)
        end do
      end if

******** calculate factors
      do ndl = 1,ndel
        totalin = inave(ndl) + upave(ndl)
        if (totalin.gt.0.01) then
          tfave(ndl) = outave(ndl) / totalin
        else
          tfave(ndl) = 1.
        end if
      end do

      call writeavetfs(rseg,lenrseg,rscen,lenrscen,
     I                 ndel,delname,year1,year2,
     I                 inave,outave,tfave)

      return
      end
