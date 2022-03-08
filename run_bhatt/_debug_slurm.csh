#!/bin/csh

set SLURM = $argv[1]

@ I = 1

while ( $I != 0 )
   echo "";
   date;
   set NSTEPS = `squeue -s | wc -l`
   @ NSTEPS = $NSTEPS - 1
   echo "Number of job steps = $NSTEPS";
   sinfo;
   echo "Number of errors = `grep 'Requested node configuration is not available' /modeling/gb604/tmp/gbhatt-slurm/$SLURM | wc -l`"
   tail -n2 /opt/slurm/etc/slurm.conf
   sleep 5;
end
