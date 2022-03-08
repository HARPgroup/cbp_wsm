#!/bin/csh

set line = `aws_cluster_state.sh | grep "Current Maximum Cluster size"`
echo $line
set MAXPROC = `echo $line | awk '{print substr($0,31,1)}'`

set NPROC   = $argv[1]
#set MAXPROC = $argv[2]

set LWAIT = 1
#SHEDDING = 0

if ( $NPROC == $MAXPROC ) then
   set SHEDDING = 1
else
   set SHEDDING = 0
endif

set tmpfile = `uuidgen`

while ( $LWAIT == 1 )
   sinfo > .config/$tmpfile
   cat .config/$tmpfile
   set line = ( "`tail -n1 .config/$tmpfile`" )
   echo "$line"
   rm .config/$tmpfile
   set IPROC = `echo "$line" | awk '{print substr($0,33,1)}'`
   if ( $IPROC > $NPROC ) then
      @ SHEDDING = 1
   endif
   if ( $SHEDDING == 1 && $IPROC == $NPROC ) then
      echo "... reached IPROC = NPROC = $NPROC"
      exit 0
   endif
   echo "SHEDDING = $SHEDDING IPROC = $IPROC"
   sleep 10;
end
