#!/bin/csh

  if (${#argv} != 1) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  remove_land_directories.csh scenario '
      echo ' or     remove_land_directories.csh scenario tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  if (${#argv} == 2) then
    set tree = $argv[2]
  else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

  source $tree/run/fragments/set_landuse

  set landuse = ($perlnds $implnds)

  
########## UCIs
  foreach lu ($landuse)
    rm -r $tree/tmp/uci/land/$lu/$scenario
  end

########## wdms
  foreach lu ($landuse)
    rm -r $tree/tmp/wdm/land/$lu/$scenario
  end

########## output space
  rm -r $tree/output/pltgen/land/$scenario
  rm -r $tree/output/pltgen/summary/$scenario
  rm -r $tree/output/input/$scenario
  foreach lu ($landuse)
    rm -r $tree/output/hspf/land/out/$lu/$scenario
    rm -r $tree/output/hspf/land/ech/$lu/$scenario
  end

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif
