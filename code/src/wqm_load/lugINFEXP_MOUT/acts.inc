************************************************************************
** variables that are used in special actions portion only            **
************************************************************************
      integer maxTimeBreaks   ! break points for time-varying special actions
      parameter (maxTimeBreaks = 35)

      integer flaglength
      parameter (flaglength=10)
      character*10 specflags        ! special actions flags

************* species of special action variables
      integer maxspecies,nspecies,sp
      parameter (maxspecies=5)
      character*4 species(maxspecies)

*********** number of special action species that relate to each pqual
      integer maxQuals,nquals,nq
      parameter (maxQuals=5)
      integer maxSpecies2Qual  ! special action species to P/Iqual const
      parameter (maxSpecies2Qual=5)
      integer nspecies2qual(maxQuals)
      integer species2qual(maxQuals,maxSpecies2Qual)
    
      real ScenarioMod(maxQuals) ! ratio of scen:cal
          ! adjust pqual params in tables

      real TimeMod(EarliestYear:LatestYear,maxQuals)
          ! adjust pqual params in special actions for trend in inputs

