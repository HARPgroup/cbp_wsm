#!/bin/csh

  if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_parallel_lands.csh scenario basin'
      echo ' '
      exit
  endif

  set scenario = $argv[1]
  set basin = $argv[2]

  source ../fragments/set_landuse

  foreach lu ($perlnds $implnds)

    sbatch run_1land.csh $scenario $basin $lu

  end


