#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo ' '
      echo 'usage:  summarize_ON_storage.csh scenario basin landuse'
      echo ' or     summarize_ON_storage.csh scenario basin landuse tree'
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


  source $tree/config/seglists/${basin}.land

  foreach seg ($segments)

    echo $seg $lu $scenario | $tree/code/bin/read_ON_stor.exe

    if (-e problem) then
      cat problem
      exit
    endif
     
  end

  if (${#argv} == 3) then
    cd ../
    rm -r temp$$
  endif

