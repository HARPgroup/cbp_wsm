************************************************************************
** special variables for the perlnd modules of the land uci generator **
************************************************************************

      include 'rug.inc'
      integer nModMax,nTabMax,nHeadMax,nVarMax
      parameter (nModMax = 12)      ! max modules
      parameter (nTabMax = 100)     ! max tables in a module
      parameter (nHeadMax = 5)      ! max header lines for a table
      parameter (nVarMax = 20)      ! max variables in one table

      character*6 modules(nModMax)         ! modules in use
      character*15 table(nModMax,nTabMax)   ! name of the table
      character*100 header(nModMax,nTabMax,nHeadMax)  ! header lines
      character*10  Var(nModMax,nTabMax,nVarMax)     ! variables names
      integer iPar(nModMax,nTabMax,nVarMax)     ! variable values
      real fPar(nModMax,nTabMax,nVarMax)     ! variable values

      integer tabformat(nModMax,nTabMax,3)  ! format for tables
                    ! last index 1-nvars, 2=0 for i, 2=1 for f, 3=cols/var

      integer nmod,nTabs(nModMax),nHead(nModMax,nTabMax)

      integer Unvar,Ustart(nVarMax),Uend(nVarMax)  ! variables used for reading
                                             ! unique format tables 


