************************************************************************
**  subroutine to determine if this is a river                        **
************************************************************************
      function rivercheck(rseg)

      implicit none
      include '../inc/standard.inc'
      logical rivercheck

      call lencl(rseg,last)
      rivercheck = .true.
      if (rseg(last-3:last).eq.'0000' 
     .    .or. rseg(last-3:last).eq.'0004') rivercheck = .false.

      close(11)

      return

      end
************************************************************************
**  subroutine to stop the program if this segment is not a river     **
************************************************************************
      subroutine riverstop(rseg)

      implicit none
      include '../inc/standard.inc'

      logical rivercheck,IamRiver
      external rivercheck

      IamRiver = rivercheck(rseg)
      if (.not.IamRiver) stop

      return

      end

