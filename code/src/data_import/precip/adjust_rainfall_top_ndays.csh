#!/bin/csh

set NDAYS = $argv[1]

set SRC_PRAD = PRC20170329
set OUT_PRAD = ${SRC_PRAD}_D${NDAYS}

source ../../../../config/seglists/allP6.land
set LSEGS = ( $segments )
#set LSEGS = ( N10001 )

set TMPDIR = `uuidgen`
mkdir  ../../../../tmp/${user}-scratch/$TMPDIR
cd     ../../../../tmp/${user}-scratch/$TMPDIR

mkdir -p ../../../input/scenario/climate/prad/${OUT_PRAD}

foreach LSEG ( $LSEGS )
   echo ""
   set line = ("`grep $LSEG /bluefish/archive/modeling/g600/projects/Phase\ 6/Rainfall/${NDAYS}days.csv`" )
   set data = `echo $line:q | sed 's/,/ /g'`
   set FACTOR     = $data[2]
   set DTHRESHOLD = $data[3]

   cp -vp ../../../input/scenario/climate/prad/${SRC_PRAD}/prad_${LSEG}.wdm ../../../input/scenario/climate/prad/${OUT_PRAD}/prad_${LSEG}.wdm
   echo $LSEG $OUT_PRAD $FACTOR $DTHRESHOLD | ../../../code/bin/adjust_rainfall_top_ndays.exe

   if (-e problem) then
     cat problem
     exit
   endif
end

cd ..
rm -r $TMPDIR
