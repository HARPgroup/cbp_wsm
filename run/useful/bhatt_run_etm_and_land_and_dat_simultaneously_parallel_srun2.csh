#!/bin/csh
#   GET SCENARIO, BASIN, and TREE
###########  The etm and land postprocessors do primarily the same thing, so 
########## half the time can be saved by combining them for scenarios and 
##########  river calibration processing
########### the date range in the etm is the date range specified in the river control file
###############  the input years are only used for average annual output

    if (${#argv} != 4) then
      echo ' '
      echo 'usage:  run_etm_and_land_and_dat_simultaneously_parallel.csh scenario basin year1 year2'
      echo ' '
      exit
    endif

  set scenario = $argv[1]
  set basin = $argv[2]
  set year1 = $argv[3]
  set year2 = $argv[4]

  source ../../config/seglists/${basin}.riv

#  srun --wait=999999 -x bluefish2,bluefish3,bluefish4 -N1 -n1 sleep 999998 &
@ i = 1
@ Num = ${#segments}
  foreach seg ($segments)
    #sleep 2
    if ($i < ${#segments} ) then
      srun --task-epilog=`sleep 10` --slurmd-debug=4 --wait=9999 --preserve-env --mpi=none --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$SLURM_JOB_NAME bhatt_run_etm_and_land_and_dat_simultaneously_oneseg.csh $scenario $seg $year1 $year2 $user-$SLURM_JOBID-`printf "%04d" $i` &
    else
      srun --task-epilog=`sleep 10` --slurmd-debug=4 --wait=9999 --preserve-env --mpi=none --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$SLURM_JOB_NAME bhatt_run_etm_and_land_and_dat_simultaneously_oneseg.csh $scenario $seg $year1 $year2 $user-$SLURM_JOBID-`printf "%04d" $i`
    endif
#--pty
    #############################
    @ j = $i % 24
    echo Hello-j= $j
    if($j == 0) then
        echo waiting...
#        wait
    endif
    #############################
    @ i += 1

  end

sleep 1
squeue
squeue -s
wait
sleep 10


