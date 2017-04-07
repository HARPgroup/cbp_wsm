#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_lug.csh scenario basin'
      echo ' or     run_lug.csh scenario basin tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  if (${#argv} == 3) then
    set tree = $argv[3]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif

  source $tree/run/fragments/set_landuse

  source $tree/config/seglists/${basin}.land

  foreach seg ($segments)

   foreach lu ($perlnds)

      if (-e problem) then
        rm problem
      endif

      echo changing pltgens for $lu for segment $seg land scenario $scenario
      echo $seg,$lu,$scenario | $tree/code/bin/pltgen2cal.exe

      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

    end

  end

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif

