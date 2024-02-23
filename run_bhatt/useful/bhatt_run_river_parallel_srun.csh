#!/bin/csh
# Script distributes the job based on specifiecd Number of CPU
# Created By Gopal Bhatt (gopal.bhatt@psu.edu)

    if (${#argv} < 2) then
      echo ' '
      echo 'usage: bhatt_run_river_parallel_srun.csh  scenario basin'
      echo ' '
      exit
    endif

  set scenario = $argv[1]
  set basin = $argv[2]

  set numnodes = $argv[3]
  set numcores = $argv[4]

source ../../config/seglists/${basin}_order.riv

foreach odr ($order)
	source ../../config/seglists/${odr}.riv

	@ nsegs     = ${#segments}
	@ numforks  = $numnodes * $numcores
	@ nsegsnode = $nsegs / $numforks


	@ i = 1
	@ start = 0
	@ stop  = 0
	@ extrasegs = $nsegs - ($numforks * $nsegsnode)
	
	echo RIV_MSG_STARTED $odr AT `date`
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

		srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$SLURM_JOB_NAME bhatt_run_river_multiseg.csh $scenario $user-$SLURM_JOBID-`printf "%04d" $i` $xsegments &

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
	wait
	echo RIV_MSG_COMPLETED $odr AT `date`
end

sleep 1
squeue
squeue -s
wait
sleep 10

exit 0
