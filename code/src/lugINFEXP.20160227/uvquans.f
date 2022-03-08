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
      subroutine Iuvquan(perlnd,lscen,lenlscen)
      implicit none
      include 'lug.inc'
c      character*(*) line
c      integer uci
      logical perlnd

      integer days
      real    precip, infexp

      character*50 dline

      call readcontrol_Lioscen(
     I                          lscen,lenlscen,
     O                          ioscen)
      call lencl(ioscen,lenioscen)

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/infexp'
c      print*,fnam
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      read(dfile,'(a50)',err=992)dline !header
c      print*,dline

c      read(dfile,'(a300)',err=992)dline
      read(dfile,*,err=992)days,precip,infexp

      line = '***Iuvquan User-Defined Variable Quantity Line'
      call ryt(line,uci)
      line = '*** kwd  varnam optyp  opn  vari  s1 s2 s3 tp multiply '//
     .       ' lc ls ac as agfn ***'
      call ryt(line,uci)
      line = '  <****> <----> <----> <-> <----><-><-><-><-><-------->'//
     .       ' <><-> <><-> <--> ***'
      call ryt(line,uci)
      if (perlnd) then
        line = '  UVQUAN precx1 PERLND   1 PREC             3          '
     .         //'       DY??? SUM'
      else
        line = '  UVQUAN precx1 IMPLND   1 PREC             3          '
     .         //'       DY??? SUM'
      end if
      write(line(65:67),'(I3)')days
      call ryt(line,uci)
      line = '       '
      call ryt(line,uci)
      return

991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = ' insufficient data in infexp file '
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end
