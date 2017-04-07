#!/bin/csh

  if (${#argv} != 3) then
    if (${#argv} != 4) then
      echo ' '
      echo 'usage:  batch_run_all.csh scenario calscen basin '
      echo ' or     batch_run_all.csh scenario calscen basin tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set calscen  = $argv[2]
  set basin    = $argv[3]

  source ../../fragments/set_landuse

  foreach lu ($perlnds)
 
    sbatch run_per_sed_optimization_1lu.csh $scenario $calscen $basin $lu

  end

