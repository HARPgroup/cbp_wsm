************************************************************************
**  variables needed to determine upstream segments                   **
**    necessary include file for calling subroutine getupstream       **
************************************************************************

      integer nupmax                   ! number of possible upstream segments
      parameter (nupmax = 5)
      character*13 upstream(nupmax)    ! upstream segments 
      integer nup                      ! number of upstream segments
      integer lenup(nupmax)            ! length of character variables


