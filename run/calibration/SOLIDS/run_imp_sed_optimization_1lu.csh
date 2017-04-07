#!/bin/csh

  if (${#argv} != 5) then
    if (${#argv} != 4) then
      echo ' '
      echo 'usage:  run_imp_sed_optmization_1lu.csh scenario calscen basin landuse'
      echo ' or     run_imp_sed_optmization_1lu.csh scenario calscen basin landuse tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set calscen  = $argv[2]
  set basin    = $argv[3]
  set lu       = $argv[4]

  if (${#argv} == 5) then
    set tree = $argv[5]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif

####### make a copy of the basin file to run
  cp -v $tree/config/seglists/${basin}.land $tree/config/seglists/${basin}_SOLIDS_${lu}.land

  set nums = (1 2 3 4 5 6 7 8 9 10)
  foreach num ($nums)

    $tree/run/calibration/SOLIDS/imp_sed_iter_1lu.csh $scenario ${basin}_SOLIDS_$lu $lu $tree

     if (-e problem) then
       echo 'problem in run number: ',$num
       cat problem
       rm problem
       exit
     endif

     echo $calscen $scenario ${basin}_SOLIDS_$lu $lu | $tree/code/bin/calib_SOLIDS.exe

     if (-e problem) then
       echo 'problem in parameter change program, run number: ',$num
       cat problem
       rm problem
       exit
     endif

  end

  $tree/run/calibration/SOLIDS/imp_sed_iter_1lu.csh $scenario ${basin}_SOLIDS_$lu $lu $tree

  if (-e problem) then
    echo 'problem in run number: ',$num
    cat problem
    rm problem
    exit
  endif

