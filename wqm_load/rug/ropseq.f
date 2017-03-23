************************************************************************
** write the operation sequence                                       **
************************************************************************
      subroutine ropseq(pltgen,timestep)
      implicit none

      include 'rug.inc'

      integer i,temptime

      temptime = timestep

      line = 'OPN SEQUENCE'
      call ryt(line,uci)

      line = '    INGRP              INDELT 00:00'
      i = 0
      do while (temptime.ge.60)
        i = i + 1
        temptime = temptime - 60
      end do
      if (i.lt.10) then
        write (line(32:32),'(i1)')i
      else
        write (line(31:32),'(i2)')i
      end if
      if (temptime.lt.10) then
        write (line(35:35),'(i1)')temptime
      else
        write (line(34:35),'(i2)')temptime
      end if
      call ryt(line,uci)

      line = '      RCHRES       1'
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

