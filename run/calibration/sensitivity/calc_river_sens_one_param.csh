#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  set scenario = $argv[1]
  set basin = $argv[2]
  set year1 = $argv[3]
  set year2 = $argv[4]
  set tree = $argv[5]

########### PROGRAM TO UPDATE PARAMETER FILE WITH NEXT CHANGE
    echo $scenario | $tree/code/bin/update_river_sens_param_file.exe
     if (-e problem) then
       echo 'problem in update_river_sens_param_file.exe'
       cat problem
       exit
     endif

########## RUN ITERATION
    $tree/run/run_river_sens_iter.csh $scenario $basin $year1 $year2 $tree
     if (-e problem) then
       echo 'problem in run_river_sens_iter'
       cat problem
       exit
     endif

########## IF FINISHED, QUIT
     if (-e finished.sens) then
       exit
     endif

########### PROGRAM TO STORE SENSITIVITIES
    echo $scenario $year1 $year2 | $tree/code/bin/write_river_sens_matrix.exe
     if (-e problem) then
       echo 'problem in write_river_sens_matrix.exe'
       cat problem
       exit
     endif

