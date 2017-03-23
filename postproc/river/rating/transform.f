************************************************************************
**  This program is to transform data to log scale                    **
************************************************************************
     
      subroutine transform(npoints,flow,conc)

      implicit none

      include '../../../lib/inc/standard.inc'
             
      real:: flow(ndaymax*24)          ! flow
      real:: conc(ndaymax*24)          ! concentration

      integer:: npoints, npoints2      ! number of data points
      integer:: np                     ! index

******************* END DECLARATIONS ***********************************
       npoints2 = npoints

       do np = 1,npoints2                 ! before transform, get rid of non-positives
        if (np.gt.npoints) exit
        if (flow(np).lt.0.0001.or.conc(np).lt.0.0001) then
          do while (flow(npoints).lt.0.0001.or.conc(npoints).lt.0.0001)
           npoints = npoints - 1
          end do
          flow(np) = flow(npoints)  ! this destroyes the order
          conc(np) = conc(npoints)  ! but not the pairing
          npoints = npoints - 1
        end if
       end do

       if (npoints.eq.0) return

       do np = 1,npoints    ! log transform
        flow(np) = log(flow(np))
        conc(np) = log(conc(np))
       end do
      
      end  
      
