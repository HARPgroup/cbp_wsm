#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 4) then
    if (${#argv} != 5) then
      echo ' '
      echo 'usage:  summarize_output_aveann.csh scenario basin year1 year2'
      echo ' or     summarize_output_aveann.csh scenario basin year1 year2 tree'
      echo ' '
      exit
    endif
  endif

  set EOF = 1
  set EOS = 1
  set DEL = 0

  set scenario = $argv[1]
  set basin = $argv[2]
  set year1 = $argv[3]
  set year2 = $argv[4]

  if (${#argv} == 5) then
    set tree = $argv[5]
  else
    source ../fragments/set_tree
    set tempdir = temp$$`../fragments/random.ksh`
    mkdir -p ../../tmp/scratch/$tempdir/
    cd ../../tmp/scratch/$tempdir/
  endif

      if (-e problem) then
        rm problem
      endif

  echo "Running command: echo $scenario $basin $year1 $year2 $EOF $EOS $DEL | $tree/code/bin/sumout_aveann.exe "
  echo $scenario $basin $year1 $year2 $EOF $EOS $DEL | $tree/code/bin/sumout_aveann.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

  if (DEL == 1) then
    echo "Running command: echo $scenario $basin $year1 $year2 | $tree/code/bin/sum_delivery_factor.exe "
    echo $scenario $basin $year1 $year2 | $tree/code/bin/sum_delivery_factor.exe
    if (-e problem) then
      echo ' '
      cat problem
      exit
    endif
  endif

  if (${#argv} == 4) then
    cd ../
    rm -r $tempdir
  endif


