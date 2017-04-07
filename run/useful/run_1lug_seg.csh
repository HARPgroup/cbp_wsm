#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 4) then
      echo ' '
      echo 'usage:  run_1lug_seg.csh scenario segment landuse'
      echo ' or     run_1lug_seg.csh scenario segment landuse tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set seg = $argv[2]
  set lu = $argv[3]
  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../fragments/set_tree
    set tempdir = temp$$`../fragments/random.ksh`
    mkdir -p ../../tmp/scratch/$tempdir/
    cd ../../tmp/scratch/$tempdir/
  endif

#  source $tree/run/fragments/set_landuse

  if (-e problem) then
    rm problem
  endif

#    foreach lu ($perlnds $implnds)

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

#    end

  if (${#argv} == 3) then
    cd ../
    rm -r $tempdir
  endif
 
