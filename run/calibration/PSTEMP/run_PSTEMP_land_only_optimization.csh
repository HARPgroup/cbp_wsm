#!/bin/csh

## this script attempts to calibrate river temperature using only land parameters

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]   # typically 'noZeroAll'
  source ../../fragments/set_tree
  set nums = (1 2 3 4 5 6 7 8 9 10)

  mkdir -p ../../../tmp/scratch/temp$$
  cd ../../../tmp/scratch/temp$$

####### only need to run rug once  
  $tree/run/run_rug.csh $scenario $basin $tree


  foreach num ($nums)
    
     $tree/run/calibration/PSTEMP/run_PSTEMP_iter.csh $scenario $basin $tree

     if (-e problem) then
       echo 'problem in run number: ',$num
       cat problem
       rm problem
       exit
     endif

     $tree/run/calibration/PSTEMP/compile_PSTEMP_opt.csh $calscen $tree
     echo $calscen, $scenario | $tree/code/bin/calib_iter_PSTEMP_params_${calscen}.exe

     if (-e problem) then
       echo 'problem in parameter change program, run number: ',$num
       cat problem
       rm problem
       exit
     endif

  end

  $tree/run/calibration/PSTEMP/run_PSTEMP_iter.csh $scenario $basin $tree

  if (-e problem) then
    echo 'problem in run number: ',$num
    cat problem
    rm problem
    exit
  endif

  cd ../
  rm -r temp$$
