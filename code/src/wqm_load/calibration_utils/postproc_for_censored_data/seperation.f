************************************************************************
**  The purpose of this program is to seperate the hydrograph into    **
**  ascending, descending  and base flow part                         **
**                                                                    **
************************************************************************

      subroutine seperation(qflow,bflow,ndays,
     O                     nasdays,ndsdays,asmatch,dsmatch)

      implicit none
      include '../../lib/inc/standard.inc'

      integer:: ndays                 ! number of days
      integer:: nasdays, ndsdays

      character(100):: pfname

      integer:: nd                    ! index

      integer:: asmatch(ndaymax),dsmatch(ndaymax)   ! matching days

      real:: qflow(ndaymax), bflow(ndaymax)         !  quick and base flow
      real:: qminus1,q,qplus1                       ! temp variables

******************* END DECLARATIONS ***********************************

********** find ascending time period
      nasdays = 0
      do nd = 2,ndays
        qminus1 = qflow(nd-1)
        q       = qflow(nd)
        if (q .gt. 0.01) then            ! begin ascending 
          if (q .gt. qminus1) then       ! ascending
            if (nd.eq.2 .and. qflow(1).gt.0.01) then
              nasdays = nasdays + 1      ! deal with the first day 
              asmatch(nasdays)= nd-1
              bflow(nd-1) = -9.0         
              nasdays = nasdays + 1
              asmatch(nasdays)= nd
              bflow(nd) = -9.0         
             else
              nasdays = nasdays + 1 
              asmatch(nasdays)= nd
              bflow(nd) = -9.0         ! no base flow during storm time
             end if 
          end if
        end if
      end do

********** find descending time period
      ndsdays = 0
      do nd = 2,ndays
        qminus1 = qflow(nd-1)
        q       = qflow(nd)
        if (q .gt. 0.01) then      ! not at rock bottom
         if (q .lt. qminus1) then        ! begin falling
           if (nd.eq.2 .and. qflow(1).gt.0.01) then
             ndsdays = ndsdays + 1      ! deal with the first day
             dsmatch(ndsdays)= nd-1
             bflow(nd-1) = -9.0
             ndsdays = ndsdays + 1
             dsmatch(ndsdays)= nd
             bflow(nd) = -9.0
           else 
             ndsdays = ndsdays + 1
             dsmatch(ndsdays)= nd
             bflow(nd) = -9.0        ! no base flow during storm time
           end if
         end if
       end if
      end do

      end


