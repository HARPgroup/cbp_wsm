************************************************************************
**  determines whether the LAND or RIVER column comes first in cross- **
**    referenced data files, returns the logical variable landfirst   **
**    calls stopreport if there is an error                           **
************************************************************************
      subroutine columnorder(fnam,
     M                       line,
     O                       landfirst)

      include '../inc/standard.inc'

      logical landfirst,scompcase
      integer ic
      character*4 land
      character*5 river

      land = 'land'
      river = 'river'

      call d2x(line,last)

      call findcomma(line,ic)                       ! check the header line for land and river columns
      if (scompcase(line(:ic-1),land)) then
        call shift(line)
        call findcomma(line,ic)
        if (scompcase(line(:ic-1),river)) then
          landfirst = .true.
        else
          go to 992
        end if
      else if (scompcase(line(:ic-1),river)) then
        call shift(line)
        call findcomma(line,ic)
        if (scompcase(line(:ic-1),land)) then
          landfirst = .false.
        else
          go to 992
        end if
      else
        go to 992
      end if

      return

************** ERROR SPACE *********************************************
992   report(1) = 'First line in file:  should be header, e.g.'
      report(2) = fnam
      report(3) = 'LAND,RIVER,VAR1,VAR2,VAR3 . . . '
      go to 999

999   call stopreport(report)

      end

