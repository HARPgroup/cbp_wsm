#!/bin/csh

  if (${#argv} != 2) then
    if (${#argv} != 3) then
      echo ' '
      echo 'usage:  remove_land_directories.csh scenario '
      echo ' or     remove_land_directories.csh scenario tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  if (${#argv} == 3) then
    set tree = $argv[3]
  else
    source ../fragments/set_tree
    set tempdir = $argv[2]
    mkdir -p ../../tmp/${user}-scratch/$tempdir/
    cd ../../tmp/${user}-scratch/$tempdir/
    #mkdir -p ../../tmp/scratch/temp$$/
    #cd ../../tmp/scratch/temp$$/
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
    rm -r $tempdir
  endif
