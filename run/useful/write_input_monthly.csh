#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  write_input_monthly.csh scenario segment'
      echo ' or     write_input_monthly.csh scenario segment tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set seg = $argv[2]
  if (${#argv} == 3) then
    set tree = $argv[3]
  else
    source ../fragments/set_tree
    set tempdir = temp$$`../fragments/random.ksh`
    mkdir -p ../../tmp/scratch/$tempdir/
    cd ../../tmp/scratch/$tempdir/
  endif

  source $tree/run/fragments/set_landuse

  if (-e problem) then
    rm problem
  endif

    foreach lu ($perlnds $implnds)

      echo making UCI for $lu  segment $seg   land scenario $scenario

      if (-e problem) then
        rm problem
      endif

      echo $seg, $lu, $scenario | $tree/code/bin/write_input_monthly.exe

      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

    end

  if (${#argv} == 2) then
    cd ../
    rm -r $tempdir
  endif
 
