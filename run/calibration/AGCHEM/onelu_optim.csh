#!/bin/csh

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]
  source ../../fragments/set_tree

  set nums = (1 2 3 4 5 6 7 8 9 10)  

#  mkdir -p ../../../tmp/scratch/temp$$
#  cd ../../../tmp/scratch/temp$$

  foreach num ($nums)
    
    $tree/run/calibration/AGCHEM/onelu_iter.csh $scenario $basin $tree

     if (-e problem) then
       echo 'problem in run number: ',$num
       cat problem
       rm problem
       exit
     endif

     echo $calscen, $scenario $basin | $tree/code/bin/calib_nitr_new03.exe 

     if (-e problem) then
       echo 'problem in parameter change program, run number: ',$num
       cat problem
       rm problem
       exit
     endif

  end

  $tree/run/calibration/AGCHEM/onelu_iter.csh $scenario $basin $tree

  if (-e problem) then
    echo 'problem in run number: ',$num
    cat problem
    rm problem
    exit
  endif

