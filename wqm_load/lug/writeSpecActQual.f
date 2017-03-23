************************************************************************
** writes special actions for one quality consituent and parameter    **
************************************************************************
      subroutine writeSpecActQual(
     I                            Varname,nq,TimeMod,
     I                            startY,endY,par)
      implicit none
      include 'lug.inc'
      include 'acts.inc'

      character*(*) varname
      real par,yearpar
      integer year

************* END DECLARATIONHS ****************************************
      line = '  PERLND  1         Y1Y1  1  1  2      3 '//
     .       ' <var>>  i       = <<<value>> '

      call trims(varname,last)
      line(43:48) = varname

      write(line(50:51),'(i2)') nq

      do year = startY,endY
        write(line(21:24),'(i4)') year
        yearpar = par * TimeMod(year,nq)
        call ritef10(line(61:70),yearpar)
        call ryt(line,uci)
      end do

      return
      end

       

      
