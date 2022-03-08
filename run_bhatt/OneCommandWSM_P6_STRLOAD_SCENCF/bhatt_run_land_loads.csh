#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo ' '
      echo 'usage:  run_land_seg.csh scenario segment'
      echo ' or     run_land_seg.csh scenario segment tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set rseg     = $argv[2]
  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../fragments/set_tree
#    set tempdir = temp$$`../fragments/random.ksh`
    set tempdir = $argv[3]
    mkdir -p ../../tmp/${user}-scratch/$tempdir/
#    wait
    cd ../../tmp/${user}-scratch/$tempdir/
    pwd
  endif

  if ( -e problem ) rm problem

  source $tree/run_bhatt/fragments/set_landuse
  
    foreach lu ($annlnds)
      echo running $lu for segment $rseg land scenario $scenario
      echo $scenario $rseg $lu | $tree/code/bin/ANNUAL_Loads.exe
    end

#    foreach lu ($monlnds)
#      echo running $lu for segment $rseg land scenario $scenario
#      echo $scenario $rseg $lu | $tree/code/bin/MONTHLY_Loads.exe
#    end

  if ( -e problem ) then
     cat problem
     exit
  endif

  if (${#argv} == 3) then
    cd ../
    rm -r $tempdir
  endif
#  wait

exit 0
