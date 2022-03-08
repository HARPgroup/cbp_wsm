************************************************************************
**  Subroutine to find the downstream segment if one exists.          **
**   returns the name of the downstream segment and a logical         **
**   true or false for whether the passed stream is a pour point      **
************************************************************************
      function pourpoint(rseg)

      implicit none
      character*13 rseg
      logical pourpoint             ! true if this is the pourpoint
      character*4 dsid  ! unique ID for the downstream river

      dsid = rseg(10:13)

      pourpoint = .false.
      if (dsid.eq.'0001'.or.dsid.eq.'0000'.or.
     .    dsid.eq.'0004') pourpoint = .true.

      end
