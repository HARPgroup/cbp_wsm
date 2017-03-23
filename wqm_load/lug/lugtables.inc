************************************************************************
** special variables for the perlnd modules of the land uci generator **
************************************************************************

      include 'lug.inc'
      integer nTabMax,nHeadMax,nVarMax
      parameter (nTabMax = 100)     ! max tables in a module
      parameter (nHeadMax = 3)      ! max header lines for a table
      parameter (nVarMax = 13)      ! max variables in one table

      character*15 table(nModMax,nTabMax)   ! name of the table
      character*100 header(nModMax,nTabMax,nHeadMax)  ! header lines
      character*10  Var(nModMax,nTabMax,nVarMax)     ! variables names
      integer iPar(nModMax,nTabMax,nVarMax)     ! variable values
      real fPar(nModMax,nTabMax,nVarMax)     ! variable values

      integer tabformat(nModMax,nTabMax,3)  ! format for tables
                    ! last index 1-nvars, 2=0 for i, 2=1 for f, 3=cols/var

      logical comment100

      integer nTabs(nModMax),nHead(nModMax,nTabMax)

      integer Unvar,Ustart(nVarMax),Uend(nVarMax)  ! variables used for reading
                                                   ! unique format tables

