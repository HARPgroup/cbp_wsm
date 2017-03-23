************************************************************************
** this program just checks a segment name to see if it is a river    ** 
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'

      logical rivercheck,IamRiver
      external rivercheck

      read*,rseg  

      IamRiver = rivercheck(rseg)

      if (IamRiver) print*, rseg

      end
