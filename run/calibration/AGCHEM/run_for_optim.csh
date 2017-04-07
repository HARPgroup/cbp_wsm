#!/bin/csh

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]

  source ../../fragments/set_tree

  set nums = (1 2 3 4 5 6 7 8 9 10)  
  set clu = (for)

  mkdir -p ../../../tmp/scratch/temp$$
  cd ../../../tmp/scratch/temp$$

  foreach lu ($clu)

    foreach num ($nums)
    
      $tree/run/calibration/AGCHEM/run_AGCHEM_iter.csh $scenario $basin $lu $tree

      if (-e problem) then
        echo 'problem in run number: ',$num
        cat problem
        rm problem
        exit
      endif

      echo $calscen $scenario $basin $lu | $tree/code/bin/calib_nitr_for.exe

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

  end 
