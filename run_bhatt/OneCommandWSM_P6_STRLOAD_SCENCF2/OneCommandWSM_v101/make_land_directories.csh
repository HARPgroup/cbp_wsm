#!/bin/csh

  if (${#argv} != 2) then
    if (${#argv} != 3) then
      echo ' '
      echo 'usage:  make_land_directories.csh scenario '
      echo ' or     make_land_directories.csh scenario tree'
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
  endif

  source $tree/run_bhatt/fragments/set_landuse

  set landuse = ($perlnds $implnds)

########## UCIs
  foreach lu ($landuse)
    mkdir -p $tree/tmp/uci/land/$lu/$scenario
  end

########## wdms
  foreach lu ($landuse)
    mkdir -p $tree/tmp/wdm/land/$lu/$scenario
  end

########## output space
  mkdir -p $tree/output/pltgen/land/$scenario
  mkdir -p $tree/output/pltgen/summary/$scenario
  mkdir -p $tree/output/input/$scenario
  foreach lu ($landuse)
    mkdir -p $tree/output/hspf/land/out/$lu/$scenario
    mkdir -p $tree/output/hspf/land/ech/$lu/$scenario
  end

  if (${#argv} == 2) then
    cd ../
    rm -r $tempdir
  endif

