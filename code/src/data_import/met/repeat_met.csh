#!/bin/csh

set RYEAR    = $argv[1]

set SRC_MET = N20150521J96
set OUT_MET = ${SRC_MET}_Y${RYEAR}

source ../../../../config/seglists/allP6.land
set LSEGS = ( $segments )
#set LSEGS = ( N10001 )

set TMPDIR = `uuidgen`
mkdir  ../../../../tmp/${user}-scratch/$TMPDIR
cd     ../../../../tmp/${user}-scratch/$TMPDIR

mkdir -p ../../../input/scenario/climate/met/${OUT_MET}

foreach LSEG ( $LSEGS )

   cp -vp ../../../input/scenario/climate/met/${SRC_MET}/met_${LSEG}.wdm ../../../input/scenario/climate/met/${OUT_MET}/met_${LSEG}.wdm
   echo $LSEG $OUT_MET $RYEAR | ../../../code/bin/repeat_met.exe

   if (-e problem) then
     cat problem
     exit
   endif
end

cd ..
rm -r $TMPDIR
