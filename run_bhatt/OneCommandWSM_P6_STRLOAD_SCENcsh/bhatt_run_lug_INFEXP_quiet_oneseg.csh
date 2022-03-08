#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo ' '
      echo 'usage:  run_lug_seg.csh scenario segment'
      echo ' or     run_lug_seg.csh scenario segment tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set seg = $argv[2]
  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../fragments/set_tree
    set tempdir = $argv[3]
#    set tempdir = temp$$`../fragments/random.ksh`
    mkdir -p ../../tmp/${user}-scratch/$tempdir/
#    wait
    cd ../../tmp/${user}-scratch/$tempdir/
  endif

  source $tree/run_bhatt/fragments/set_landuse

  if (-e problem) then
    rm problem
  endif

    foreach lu ($perlnds $implnds)

      echo making UCI fo $lu  segment $seg   land scenario $scenario

      if (-e problem) then
        rm problem
      endif

      echo $seg, $lu, $scenario | $tree/code/bin/lugINFEXPquiet.exe

      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

    end

  if (${#argv} == 3) then
    cd ../
    rm -r $tempdir
#    wait
  endif

exit 0 
