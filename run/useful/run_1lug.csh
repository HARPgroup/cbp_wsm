#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo ' '
      echo 'usage:  run_1lug.csh scenario basin landuse'
      echo ' or     run_1lug.csh scenario basin landuse tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  set landuse = $argv[3]
  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

  source $tree/run/fragments/set_landuse

  source $tree/config/seglists/${basin}.land

  if (-e problem) then
    rm problem
  endif

  foreach seg ($segments)

    foreach lu ($perlnds $implnds)

      if ($lu == $landuse) then

      echo making UCI for $lu  segment $seg   land scenario $scenario

      if (-e problem) then
        rm problem
      endif

      echo $seg, $lu, $scenario | $tree/code/bin/lug.exe

      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

      endif
    end

  end
     
  if (${#argv} == 3) then
    cd ../
    rm -r temp$$
  endif
 
