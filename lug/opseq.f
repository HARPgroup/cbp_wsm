************************************************************************
** write the operation sequence either PERLND or IMPLND               ****  adds pltgen lines if applicable                                   **
************************************************************************
      subroutine opseq(perlnd,implnd,pltgen)
      implicit none

      include 'lug.inc'

      logical perlnd,implnd
      integer i

      line = 'OPN SEQUENCE'
      call ryt(line,uci)

      line = '    INGRP              INDELT 01:00'
      call ryt(line,uci)

      if (perlnd) line = '      PERLND       1'
      if (implnd) line = '      IMPLND       1'
      call ryt(line,uci)

      if (pltgen) then
        line = '      PLTGEN      XX'
        do i = 1,numplts
          write(line(19:20),'(i2)') i
          call ryt(line,uci)
        end do
      end if

      line = '    END INGRP'
      call ryt(line,uci)

      line = 'END OPN SEQUENCE'
      call ryt(line,uci)

      line = '   '
      call ryt(line,uci)

      end

