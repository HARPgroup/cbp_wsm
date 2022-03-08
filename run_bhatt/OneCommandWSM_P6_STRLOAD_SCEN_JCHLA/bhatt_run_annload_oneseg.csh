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
  set rseg = $argv[2]
  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../fragments/set_tree
    set tempdir = $argv[3] 
    mkdir -p ../../tmp/${user}-scratch/$tempdir/
    cd ../../tmp/${user}-scratch/$tempdir/
    pwd
  endif

  source $tree/run_bhatt/fragments/set_landuse
  
  foreach lu ($annlnds)

    echo running $lu for segment $rseg land scenario $scenario
    echo $scenario $rseg $lu | ../../../code/bin/annloads.exe
        
  end

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
