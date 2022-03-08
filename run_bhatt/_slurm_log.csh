#!/bin/csh

# PREFIX EVERY PRINTF/ECHO STATEMENT WITH $S_TIME

set LOCK = /tmp/_slurm_log.lock
if ( -e $LOCK ) then
   exit
endif
echo "`date`" >> $LOCK



set LOGDIR  = "/var/www/html/wsm/p532/logs/"
set LOGFILE = "slurm_info.log"



set DAYS   = ( Sunday Monday Tuesday Wednesday Thursday Friday Saturday Sunday )

set today  = `date +%A`
set S_TIME = `date +%A-%Y-%m-%d-%H-%M`

set I = 1
while ( $I <= 8 )
   if ( $DAYS[$I] == $today ) then
      break
   endif
   @ I = $I + 1
end
@ I = $I + 1
set nextday = "$DAYS[$I]"

find $LOGDIR -name $LOGFILE | xargs sed -i "/$nextday/d"



printf "\n\n\n\n" >> $LOGDIR/$LOGFILE
set NSTEPS = `squeue -s | wc -l`
@ NSTEPS = $NSTEPS - 1
echo "$S_TIME ... Number of job steps = $NSTEPS" >> $LOGDIR/$LOGFILE
foreach line ( "`sinfo`" )
   echo "$S_TIME ... $line" >> $LOGDIR/$LOGFILE
end
foreach line ( "`tail -n2 /opt/slurm/etc/slurm.conf`" )
   echo "$S_TIME ... $line" >> $LOGDIR/$LOGFILE
end

foreach line ( "`squeue --state=running`" )
   echo "$S_TIME ... $line" >> $LOGDIR/$LOGFILE
end



rm $LOCK
