#!/bin/csh

set SCENARIO    = $argv[1]
set SEGMENT     = $argv[2]

source ../../../run/fragments/set_landuse
set LUS      = ( $perlnds $implnds )

set DIR      = `uuidgen`
mkdir -p ../../../tmp/$USER-scratch/$DIR
cd ../../../tmp/$USER-scratch/$DIR

if ( -e problem ) rm problem
foreach LU ( $LUS )
   echo $SEGMENT $LU $SCENARIO | ../../../code/bin/lugx.exe 
   #echo A10001 for PQ20140613_01c | ../../../code/bin/lugx.exe
end
if ( -e problem ) then
   cat problem
   exit
endif



cd ..
rm -r $DIR
