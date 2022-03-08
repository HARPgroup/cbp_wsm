************************************************************************
** variables to support pltgen operations in a uci                    **
**  each pltgen is specified in the control file and has one variable **
************************************************************************
      logical pltgen  ! are there pltgens

      integer maxplts,numplts  ! number of pltgens in this uci
      parameter (maxplts=60)  ! these can be increased and recompiled
                            ! do not increase beyond 69
                            ! see ./pp/src/lug/files.f

      character*8 pltname(maxplts)  ! name of this plt (user specified)

      character*6 pltgrp(maxplts)  ! HSPF variable group
      character*6 pltmem(maxplts)  ! HSPF variable group member
      integer pltnum(maxplts,2)    ! HSPF variable group member numbers

      character*4 plttran1(maxplts) !  transition string (e.g. SAME)
      character*5 pltmnpt(maxplts)  !  MEAN or POINT for network

      integer pltpivl(maxplts)  !  print interval (24 = daily)

      character*26 pltlabel(maxplts)  ! plot label for GEN-LABELS

      character*4 plttran2(maxplts)  ! trasition string for CURV-DATA

      common /pltvars/ numplts,pltname,pltgrp,pltmem,pltnum,plttran1,
     .                 pltmnpt,pltpivl,pltlabel,plttran2

