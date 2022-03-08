************************************************************************
** write the network section for PLTGENS, if applicable               **
************************************************************************
      subroutine rnetwork
      implicit none

      include 'rug.inc'

      integer i,j,k

      integer n          ! index
      logical comment,scompare
      real factor

      character*100 templine

***********END DECLARATION ***********************************************

      line = 'NETWORK'
      call ryt(line,uci)

      line = '<-Volume-> <-Grp> <-Member-><--Mult-->Tran <-Target vols>'
     .     //' <-Grp> <-Member-> ***'
      call ryt(line,uci)

      line = '<Name>   #        <Name> # #<-factor->strg <Name>   #   #'
     .     //'        <Name> # # ***'
      call ryt(line,uci)

      line = 'RCHRES   1 GROUP  MEMBER 1 1          same PLTGEN   n    '
     .     //' INPUT  mean   1'

      do i = 1,numplts

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

      return

************** ERROR SPACE *********************************************
991   report(1) = 'Could not open file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:10),'(i2)') err
      go to 999

999   call stopreport(report)

      end

