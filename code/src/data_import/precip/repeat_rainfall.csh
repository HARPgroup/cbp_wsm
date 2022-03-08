#!/bin/csh

set RYEAR    = $argv[1]

set SRC_PRAD = PRC20170731
set OUT_PRAD = ${SRC_PRAD}_Y${RYEAR}

source ../../../../config/seglists/allP6.land
set LSEGS = ( $segments )
#set LSEGS = ( N10001 )

set TMPDIR = `uuidgen`
mkdir  ../../../../tmp/${user}-scratch/$TMPDIR
cd     ../../../../tmp/${user}-scratch/$TMPDIR

mkdir -p ../../../input/scenario/climate/prad/${OUT_PRAD}

foreach LSEG ( $LSEGS )

   cp -vp ../../../input/scenario/climate/prad/${SRC_PRAD}/prad_${LSEG}.wdm ../../../input/scenario/climate/prad/${OUT_PRAD}/prad_${LSEG}.wdm
   echo $LSEG $OUT_PRAD $RYEAR | ../../../code/bin/repeat_rainfall.exe

   if (-e problem) then
     cat problem
     exit
   endif
end

cd ..
rm -r $TMPDIR
