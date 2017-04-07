#!/bin/csh
# csh run_postproc_river_IO.csh SCENARIO SEGMENT [D,M,Y] [INPUT,OUTPUT,BOTH] tempDir

  if (${#argv} != 5) then
    if (${#argv} != 4) then
      echo ' '
      echo 'usage: run_postproc_river_IO_oneseg.csh scenario river D,M,orY BOTH tempfolder'
      echo ' or    Figure it out!'
      echo ' '
      exit
    endif
  endif

  set scenario     = $argv[1]
  set seg          = $argv[2]
  set TimeInterval = $argv[3]
  set IOFlags      = $argv[4]
  set tempdir      = $argv[5]

  source ../fragments/set_tree
  mkdir -p ../../tmp/$user-scratch/$tempdir/
  cd ../../tmp/$user-scratch/$tempdir/

  if (-e problem) then
    rm problem
  endif

  if ( `echo $IOFlags | tr '[a-z]' '[A-Z]'` == 'BOTH' ) then
       echo $scenario, $seg, $TimeInterval, 'INPUT'  | $tree/code/bin/RiverLoads.exe
       echo $scenario, $seg, $TimeInterval, 'OUTPUT' | $tree/code/bin/RiverLoads.exe
  else
       echo $scenario, $seg, $TimeInterval, $IOFlags | $tree/code/bin/RiverLoads.exe
  endif

  if (-e problem) then
       echo ' '
       cat problem
       exit
  endif

  if (${#argv} == 5) then
    cd ../
    rm -r $tempdir
  endif
