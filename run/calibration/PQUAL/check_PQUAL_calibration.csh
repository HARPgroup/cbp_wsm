#!/bin/csh
#   GET SCENARIO AND BASIN (SEGMENT LISTS)

  if (${#argv} != 4) then
    if (${#argv} != 5) then
      echo ' '
      echo 'usage:  check_PQUAL_optimization.csh scenario calib_scen basin landuse'
      echo ' or     check_PQUAL_optimization.csh scenario calib_scen basin landuse tree'
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
  endif

  if ($lu == 'all') then
    source ../../fragments/set_landuse
  else
    set perlnds = $lu
  endif

  if (-e check_all_perlnds) then
    rm check_all_perlnds
  endif

  foreach lu ($perlnds)
    echo $scenario $basin $lu 1985 2005 | $tree/code/bin/pltgen_annave_summary.exe

    $tree/run/calibration/PQUAL/compile_PQUAL_opt.csh $calscen $tree
    echo $calscen, $scenario ${basin} $lu | $tree/code/bin/calib_iter_PQUAL_check_${calscen}.exe >> check_all_perlnds

    if (-e problem) then
      echo 'message from calibration check program'
      cat problem
      rm problem
      exit
    endif
  end 

