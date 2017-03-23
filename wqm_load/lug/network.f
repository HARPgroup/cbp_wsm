************************************************************************
** write the network section for PLTGENS, if applicable               **
************************************************************************
      subroutine network(perlnd,implnd)
      implicit none

      include 'lug.inc'

      logical perlnd,implnd
      integer i,j,k

      line = 'NETWORK'
      call ryt(line,uci)

      line = '<-Volume-> <-Grp> <-Member-><--Mult-->Tran <-Target vols>'
     .     //' <-Grp> <-Member-> ***'
      call ryt(line,uci)

      line = '<Name>   #        <Name> # #<-factor->strg <Name>   #   #'
     .     //'        <Name> # # ***'
      call ryt(line,uci)

      line = 'XXXLND   1 GROUP  MEMBER 1 1          same PLTGEN   n    '
     .     //' INPUT  mean   1'

      do i = 1,numplts
        if (perlnd) line(:6) = 'PERLND'
        if (implnd) line(:6) = 'IMPLND'

        line(12:17) = pltgrp(i)
        line(19:24) = pltmem(i)

        do j = 1,2      ! number portion of group member
          k = 24 + j*2
          if (pltnum(i,j).gt.0) then
            write(line(k:k),'(i1)') pltnum(i,j)
          else
            line(k:k) = ' '
          end if
        end do

        line(39:42) = plttran1(i)

        write(line(52:53),'(i2)') i

        line(66:70) = pltmnpt(i)

        call ryt(line,uci)
      end do

      line = 'END NETWORK'
      call ryt(line,uci)

      line = '   '
      call ryt(line,uci)

      end

