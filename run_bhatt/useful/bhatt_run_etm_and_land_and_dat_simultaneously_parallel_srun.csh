#!/bin/csh
# Script distributes the job based on specifiecd Number of CPU
# Created By Gopal Bhatt (gopal.bhatt@psu.edu)

    if (${#argv} < 4) then
      echo ' '
      echo 'usage:  run_etm_and_land_and_dat_simultaneously_parallel.csh scenario basin year1 year2'
      echo ' '
      exit
    endif

  set scenario = $argv[1]
  set basin = $argv[2]
  set year1 = $argv[3]
  set year2 = $argv[4]

  set numnodes = $argv[5]
  set numcores = $argv[6]

  source ../../config/seglists/${basin}.riv

  @ nsegs     = ${#segments}
  @ numforks  = $numnodes * $numcores
  @ nsegsnode = $nsegs / $numforks


@ i = 1
@ start = 0
@ stop  = 0
@ extrasegs = $nsegs - ($numforks * $nsegsnode)

while ( $i <= $numforks )
	@ start = $stop + 1
        @ stop  = $start + $nsegsnode - 1
        if($extrasegs > 0) then
                @ stop = $stop + 1
                @ extrasegs = $extrasegs - 1
        endif
        echo $start $stop
        set xsegments = ( $segments[$start-$stop] )
        echo $xsegments

	srun -l --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$SLURM_JOB_NAME bhatt_run_etm_and_land_and_dat_simultaneously_multiseg.csh $scenario $year1 $year2 $user-$SLURM_JOBID-`printf "%04d" $i` $xsegments &
	sleep 2
    #############################
#    @ j = $i % 24
#    echo Hello-j= $j
#    if($j == 0) then
#        echo waiting...
###        wait
#    endif
    #############################

    @ i += 1

end

sleep 1
squeue
squeue -s
wait
sleep 10

exit 0
