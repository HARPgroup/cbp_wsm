************************************************************************
** main calling routine for the special action writing section        **
************************************************************************
      subroutine rspecact(rseg,lenrseg,rscen,lenrscen,resflag)
      implicit none
      include 'rug.inc'

********** END DECLARATION *******************************************
      line = 'SPEC-ACTIONS'
      call ryt(line,uci)

      if (resflag.eq.'V') then 
        call varftab(rseg,lenrseg,rscen,lenrscen)
      else if (resflag.eq.'W') then 
        call cowanesque(rseg,lenrseg,rscen,lenrscen)
      else if (resflag.eq.'R') then
        call curwensville(rseg,lenrseg,rscen,lenrscen)
      else if (resflag.eq.'B') then
        call sayersblanchard(rseg,lenrseg,rscen,lenrscen)
      else if (resflag.eq.'J') then
        call januarystorm(rseg,lenrseg,rscen,lenrscen)
      end if  ! if S or C, (standard or concwingo) no special actions

      line = 'END SPEC-ACTIONS'
      call ryt(line,uci)
      line = '       '
      call ryt(line,uci)

      return

      end


