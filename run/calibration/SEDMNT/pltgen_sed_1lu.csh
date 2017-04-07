#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 4) then
      echo ' '
      echo 'usage:  pltgen_sed_1lu.csh scenario basin landuse'
      echo ' or     pltgen_sed_1lu.csh scenario basin landuse tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  set lu = $argv[3]
  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif

#   SET VARIABLES  --  TO CHANGE WHAT RUNS, CHANGE THE SCENARIO, PERLND, IMPLND, AND BASIN (SEGMENT LISTS)

  source $tree/config/seglists/${basin}.land

  foreach seg ($segments)

    echo summarizing pltgens for $lu for segment $seg land scenario $scenario
    echo $seg,$lu,$scenario | $tree/code/bin/pltgen.exe

      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

# summary land sediment results
    grep dets $tree/output/pltgen/land/${scenario}/${lu}*sums > $tree/output/pltgen/summary/${scenario}/${lu}_DETS_tons.csv
    grep wssd $tree/output/pltgen/land/${scenario}/${lu}*sums > $tree/output/pltgen/summary/${scenario}/${lu}_WSSD_tons.csv

  end

  if (${#argv} == 3) then
    cd ../
    rm temp$$
  endif

