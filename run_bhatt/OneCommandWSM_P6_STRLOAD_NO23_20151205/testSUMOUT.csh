#!/bin/csh
#   GET SCENARIO, BASIN, and TREE
set OMP_NUM_THREADS=8
#echo $OMP_NUM_THREADS
  if (${#argv} != 5) then
    if (${#argv} != 6) then
      echo ' '
      echo 'usage:  summarize_output_aveann.csh scenario basin year1 year2'
      echo ' or     summarize_output_aveann.csh scenario basin year1 year2 tree'
      echo ' '
      exit
    endif
  endif

  set EOF = 1
  set EOS = 1
  set DEL = 1

  set scenario = $argv[1]
  set basin = $argv[2]
  set year1 = $argv[3]
  set year2 = $argv[4]

  if (${#argv} == 6) then
    set tree = $argv[6]
  else
    source ../fragments/set_tree
    #set tempdir = temp$$`../fragments/random.ksh`
    set tempdir = $argv[5]
    #mkdir -p ../../tmp/scratch/$tempdir/
    mkdir -p ../../tmp/${user}-scratch/$tempdir/
    #cd ../../tmp/scratch/$tempdir/
    cd ../../tmp/${user}-scratch/$tempdir/
  endif

      if (-e problem) then
        rm problem
      endif

  echo $scenario $basin $year1 $year2 $EOF $EOS $DEL | $tree/code/src/postproc/postutils/sumout_P6/sumout_aveann.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

  echo $scenario $basin $year1 $year2 | $tree/code/bin/sum_delivery_factor.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

  echo $scenario | $tree/code/bin/copy_ps_loads.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

  if (${#argv} == 5) then
    cd ../
    rm -r $tempdir
  endif


