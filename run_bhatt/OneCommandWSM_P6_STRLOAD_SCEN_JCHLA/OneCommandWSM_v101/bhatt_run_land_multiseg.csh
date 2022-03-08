#!/bin/csh
# Script executes the serial jobs from the job-partition
# Created By Gopal Bhatt (gopal.bhatt@psu.edu)

#$scenario $year1 $year2 $user-$SLURM_JOBID-`printf "%04d" $i` $xsegments

set scenario = $argv[1]
set dir      = $argv[2]

@ start = 3
@ stop  = ${#argv}

set segments = ( $argv[$start-$stop] )

@ k = 1
foreach seg ($segments)

	csh bhatt_run_land_oneseg.csh $scenario $seg $dir-`printf "%04d" $k`
	@ k++

end

exit 0
