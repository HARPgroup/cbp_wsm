************************************************************************
** subroutines for generating UVNAMES for special actions             **
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

      subroutine uvnamehead(line,uci)
      implicit none
      character*(*) line
      integer uci
      line = '*** UVNAMES'
      call ryt(line,uci)
      line = '*** kwd   varnam ct  vari  s1 s2 s3  frac oper     vari '
     .       //' s1 s2 s3  frac oper'
      call ryt(line,uci)
      line = '  <****>  <----><-> <----><-><-><-> <---> <-->    <---->'
     .       //'<-><-><-> <---> <-->'
      call ryt(line,uci)
      return
      end

************************************************************************
** Fertilizer is to upper and surface layer                           **
************************************************************************
      subroutine Fuvname(line,uci)
      implicit none
      character*(*) line
      integer uci
      line = '  UVNAME  FNO3    2 SNO3              0.5 QUAN    '//
     .                           'UNO3              0.5 QUAN'
      call ryt(line,uci)
      line = '  UVNAME  FNH3    2 SAMAD             0.5 QUAN    '//
     .                           'UAMAD             0.5 QUAN'
      call ryt(line,uci)
      line = '  UVNAME  FORN    2 SORGN             0.5 QUAN    '//
     .                           'UORGN             0.5 QUAN'
      call ryt(line,uci)
      line = '  UVNAME  FPO4    2 SP4AD             0.5 QUAN    '//
     .                           'UP4AD             0.5 QUAN'
      call ryt(line,uci)
      line = '  UVNAME  FORP    2 SORGP             0.5 QUAN    '//
     .                           'UORGP             0.5 QUAN'
      call ryt(line,uci)
      return
      end

************************************************************************
** Manure is split surface and upper                                  **
************************************************************************
      subroutine Muvname(line,uci)
      implicit none
      character*(*) line
      integer uci
      line = '  UVNAME  MNO3    2 SNO3              0.3 QUAN    '//
     .                           'UNO3              0.7 QUAN'
      call ryt(line,uci)
      line = '  UVNAME  MNH3    2 SAMAD             0.3 QUAN    '//
     .                           'UAMAD             0.7 QUAN'
      call ryt(line,uci)
      line = '  UVNAME  MORN    2 SORGN             0.3 QUAN    '//
     .                           'UORGN             0.7 QUAN'
      call ryt(line,uci)
      line = '  UVNAME  MPO4    2 SP4AD             0.3 QUAN    '//
     .                           'UP4AD             0.7 QUAN'
      call ryt(line,uci)
      line = '  UVNAME  MORP    2 SORGP             0.3 QUAN    '//
     .                           'UORGP             0.7 QUAN'
      call ryt(line,uci)
      return
      end

************************************************************************
** Manure is split surface and upper                                  **
************************************************************************
      subroutine Luvname(line,uci)
      implicit none
      character*(*) line
      integer uci
      line = '  UVNAME  LEGUME  2 UAMAD             0.5 QUAN    '//
     .                           'LAMAD             0.5 QUAN'
      call ryt(line,uci)
      return
      end

************************************************************************
** Dummy subroutines                                                  **
************************************************************************
      subroutine Vuvname
      implicit none
      return
      end
      subroutine Puvname
      implicit none
      return
      end
      subroutine Tuvname
      implicit none
      return
      end
      subroutine Uuvname
      implicit none
      return
      end
      subroutine Ruvname
      implicit none
      return
      end
      subroutine Iuvname
      implicit none
      return
      end

