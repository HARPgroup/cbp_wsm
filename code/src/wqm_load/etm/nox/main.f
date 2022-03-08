      implicit none

      real tn
      real nox
      character*25 rscen,rseg

      rscen = "P620170331WQa"

      read*,rseg,tn

      call getnox(rscen,rseg,tn,nox)

      stop

      end
