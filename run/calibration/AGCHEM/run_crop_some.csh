#!/bin/csh

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  if (${#argv} != 4) then
    if (${#argv} != 5) then
      echo ' '
      echo 'usage:  run_crop_optim.csh scenario calib_scen basin landuse'
      echo ' or     run_crop_optim.csh scenario calib_scen basin landuse tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]
  set lu = $argv[4]

  if (${#argv} == 5) then
    set tree = $argv[5]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$
    cd ../../../tmp/scratch/temp$$
  endif

  set nums = (1 2 3 4 5 6 7 8 9 10)  

  foreach num ($nums)
    
     $tree/run/calibration/AGCHEM/run_AGCHEM_iter.csh $scenario $basin $lu $tree

     if (-e problem) then
       echo 'problem in run number: ',$num
       cat problem
       rm problem
       exit
     endif

     echo $calscen $scenario $basin $lu | $tree/code/bin/calib_nitr_crop.exe
     echo $calscen $scenario $basin $lu | $tree/code/bin/calib_phos_crop.exe     

     if (-e problem) then
       echo 'problem in parameter change program, run number: ',$num
       cat problem
       rm problem
       exit
     endif

  end

  $tree/run/calibration/AGCHEM/run_AGCHEM_iter.csh $scenario $basin $lu $tree

  if (-e problem) then
    echo 'problem in run number: ',$num
    cat problem
    rm problem
    exit
  endif

 
