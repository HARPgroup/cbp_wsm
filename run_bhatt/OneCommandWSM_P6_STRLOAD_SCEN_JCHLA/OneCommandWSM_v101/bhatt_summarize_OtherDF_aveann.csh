#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 6) then
    if (${#argv} != 7) then
      echo ' '
      echo 'usage:  summarize_otherDF_aveann.csh scenario DFscen basin year1 year2'
      echo ' or     summarize_otherDF_aveann.csh scenario DFscen basin year1 year2 tree'
      echo ' '
      exit
    endif
  endif

  set EOF = 1
  set EOS = 1
  set DEL = 1
#$scenario $CDF_Sce $xbasin $year1 $year2 $dir-`printf "%04d" $k`
  set scenario = $argv[1]
  set DFscen   = $argv[2]
  set basin    = $argv[3]
  set year1    = $argv[4]
  set year2    = $argv[5]
  set tempdir  = $argv[6]

  if (${#argv} == 7) then
    set tree = $argv[7]
  else
    source ../fragments/set_tree
    #set tempdir = temp$$`../fragments/random.ksh`
    #mkdir -p ../../tmp/scratch/$tempdir/
    mkdir -p ../../tmp/${user}-scratch/$tempdir/
    #cd ../../tmp/scratch/$tempdir/
    cd ../../tmp/${user}-scratch/$tempdir/
  endif

      if (-e problem) then
        rm problem
      endif

  #mkdir -p $tree/sumout/aveann/${scenario}_DF_${DFscen}

  echo $scenario $DFscen $basin $year1 $year2 $EOF $EOS $DEL | $tree/code/bin/sumout_otherDF_aveann.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

  echo $scenario $DFscen $basin $year1 $year2 | $tree/code/bin/sum_otherDF_delivery_factor.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

  echo $scenario $DFscen | $tree/code/bin/copy_ps_loads_otherDF.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

  if (${#argv} == 6) then
    cd ../
    rm -r $tempdir
  endif


