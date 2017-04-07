#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_lug_parallel.csh scenario basin'
      echo ' '
      exit
    endif

  set scenario = $argv[1]
  set basin = $argv[2]

  source ../../config/seglists/${basin}.land
@ i = 1
  foreach seg ($segments)

    srun --nodes=1 --ntasks=1 --exclusive --job-name=$SLURM_JOB_NAME ../useful/bhatt_run_lug_seg.csh $scenario $seg $user-$SLURM_JOBID-`printf "%04d" $i` &

    #############################
    @ j = $i % 32
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
 
