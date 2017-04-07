#!/bin/csh

set scenario = $argv[1]
set basin    = $argv[2]

source ../../config/seglists/${basin}.riv

foreach seg ($segments)
      srun --nodes=1 --ntasks=1 --exclusive --job-name=$SLURM_JOB_NAME ../standard/bhatt_run_scenario_river.csh $scenario $seg &
end

sleep 1
squeue
squeue -s
wait
