#!/bin/csh
# Script executes the serial jobs from the job-partition
# Created By Gopal Bhatt (gopal.bhatt@psu.edu)

#$scenario $year1 $year2 $user-$SLURM_JOBID-`printf "%04d" $i` $xsegments

set scenario = $argv[1]
set year1    = $argv[2]
set year2    = $argv[3]
#set PARA     = $argv[4]
set dir      = $argv[4]

@ start = 5
@ stop  = ${#argv}

set xbasins = ( $argv[$start-$stop] )
echo 'bhatt_sum_all_basins_multiseg' $xbasins ${#xbasins}

@ k = 1
foreach xbasin ($xbasins)

        echo $xbasin
        #date
        #csh cache_folder.csh $scenario $xbasin $PARA
        #csh copy_summarize_input.csh $scenario $xbasin $dir $PARA
        #date
        csh bhatt_summarize_output_aveann.csh $scenario $xbasin $year1 $year2 $dir-`printf "%04d" $k`

	@ k++

end

exit 0
