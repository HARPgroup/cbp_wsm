************************************************************************
** write the network section for PLTGENS, if applicable               **
************************************************************************
      subroutine rpgen(rseg,timestep)
      implicit none

      include 'rug.inc'

      integer i,j,k

      character*4 MEAN
      data MEAN /'MEAN'/

      logical scompcase
      external scompcase

********************* END SPECIFICATIONS *******************************

      call lencl(rseg,lenrseg)

      line = 'PLTGEN'
      call ryt(line,uci)

********** PLOTINFO  --------------------------------------
      line = '  PLOTINFO'
      call ryt(line,uci)

      line = '    # -  # FILE  NPT  NMN LABL  PYR PIVL ***'
      call ryt(line,uci)

      do i = 1,numplts
        line = ' '
        write(line(4:5),'(i2)') i
        write(line(14:15),'(i2)') i+30

        if (scompcase(pltmnpt(i),MEAN)) then
          line(25:25) = '1'
        else
          line(20:20) = '1'
        end if

        line(34:35) = '12'  ! end of year flag

        if (timestep.ne.60) then  ! normalize pltpivl to 60 minutes
          if (pltpivl(i).gt.0) then
            pltpivl(i) = pltpivl(i) * 60 / timestep
          end if
        end if
        write(line(39:40),'(i2)') pltpivl(i)

        call ryt(line,uci)
      end do

      line = '  END PLOTINFO'
      call ryt(line,uci)

      line = '  '
      call ryt(line,uci)

********** GEN-LABELS -------------------------------------
      line = '  GEN-LABELS'
      call ryt(line,uci)

      line = '    # -  #<----------------Title----------------->   ***'
      call ryt(line,uci)

      do i = 1,numplts
        line = '   XX         '//rseg(:lenrseg)//' '//pltlabel(i)
        write(line(4:5),'(i2)') i
        call ryt(line,uci)
      end do 

      line = '  END GEN-LABELS'
      call ryt(line,uci)

      line = '  '
      call ryt(line,uci)

********** SCALING ----------------------------------------
      line = '  SCALING'
      call ryt(line,uci)

      line = '    #thru#      YMIN      YMAX     IVLIN     THRESH ***'
      call ryt(line,uci)

      line = '    1   99        0.   100000.       20.'
      call ryt(line,uci)

      line = '  END SCALING'
      call ryt(line,uci)

      line = '  '
      call ryt(line,uci)

********** CURV-DATA --------------------------------------
      line = '  CURV-DATA'
      call ryt(line,uci)

      line = '              <-Curve label--> Line Intg  Col Tran ***'
      call ryt(line,uci)

      line = '    # -  #                     type  eqv code code ***'
      call ryt(line,uci)

      line = '    1         put pltlabl here         1    1 tran'

      do i = 1,numplts
        write(line(4:5),'(i2)') i
        line(15:30) = pltlabel(i)
        line(47:50) = plttran2(i)
        call ryt(line,uci)
      end do

      line = '  END CURV-DATA'
      call ryt(line,uci)

      line = 'END PLTGEN'
      call ryt(line,uci)

      line = '   '
      call ryt(line,uci)

      end

