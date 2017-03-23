************************************************************************
** subroutines for generating UVQUANS for special actions             **
************************************************************************
** V = variable cover in sediment                                     **
** P = plowing                                                        **
** A = atmospheric deposition                                         **
** F = fertilizer                                                     **
** M = manure                                                         **
** L = fixation for legumes                                           **
** T = variable total annual uptake target                            **
** U = variable montly fraction of uptake target                      **
************************************************************************


************************************************************************
** subroutine to write the precip uvquan line at the beginning of the **
**  special actions section.  These are shared by all special actions **
************************************************************************
      subroutine precquan(line,uci,perlnd)
      implicit none
      character*(*) line
      integer uci
      logical perlnd
      line = '***User-Defined Variable Quantity Line'
      call ryt(line,uci)
      line = '*** kwd  varnam optyp  opn  vari  s1 s2 s3 tp multiply '//
     .       ' lc ls ac as agfn ***'
      call ryt(line,uci)
      line = '  <****> <----> <----> <-> <----><-><-><-><-><-------->'//
     .       ' <><-> <><-> <--> ***'
      call ryt(line,uci)
      if (perlnd) then
        line = '  UVQUAN prec   PERLND   1 PREC             3          '
     .         //'       DY  1 SUM'
      else
        line = '  UVQUAN prec   IMPLND   1 PREC             3          '
     .         //'       DY  1 SUM'
      end if
      call ryt(line,uci)
      line = '       '
      call ryt(line,uci)
      return
      end


      subroutine Fuvquan
      implicit none
      return
      end
      subroutine Muvquan
      implicit none
      return
      end
      subroutine Vuvquan
      implicit none
      return
      end
      subroutine Puvquan
      implicit none
      return
      end
      subroutine Luvquan
      implicit none
      return
      end
      subroutine Tuvquan
      implicit none
      return
      end
      subroutine Uuvquan
      implicit none
      return
      end
      subroutine Ruvquan
      implicit none
      return
      end

