#!/bin/csh
# Script executes the serial jobs from the job-partition
# Created By Gopal Bhatt (gopal.bhatt@psu.edu)

#$scenario $year1 $year2 $user-$SLURM_JOBID-`printf "%04d" $i` $xsegments

set scenario = $argv[1]
set year1    = $argv[2]
set year2    = $argv[3]
set dir      = $argv[4]

@ start = 5
@ stop  = ${#argv}

set segments = ( $argv[$start-$stop] )

@ k = 1
foreach seg ($segments)

	csh bhatt_run_etm_and_land_and_dat_simultaneously_oneseg.csh $scenario $seg $year1 $year2 $dir-`printf "%04d" $k`
	@ k++

end

exit 0
