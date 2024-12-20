#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 6) then
    if (${#argv} != 5) then
      echo ' '
      echo 'usage:  summarize_output_monthly.csh scenario basin Delivery_method year1 year2'
      echo ' or     summarize_output_monthly.csh scenario basin Delivery_method year1 year2 tree'
      echo ' '
      echo 'Delivery_Method must be res for reservoir,'
      echo '                        seg for segment, or '
      echo '                        bas for basin'
      exit
    endif
  endif

  set EOF = 1
  set EOS = 1
  set DEL = 1

  set scenario = $argv[1]
  set basin = $argv[2]
  set DFmethod = $argv[3]
  set year1 = $argv[4]
  set year2 = $argv[5]

  if (${#argv} == 6) then
    set tree = $argv[6]
  else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

      if (-e problem) then
        rm problem
      endif

  echo $scenario $basin $year1 $year2 $EOF $EOS $DEL $DFmethod | $tree/code/bin/sumout_monthly.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

  if (${#argv} == 5) then
    cd ../
    rm -r temp$$/
  endif

