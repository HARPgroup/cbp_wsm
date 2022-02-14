#!/bin/csh

source ../../../run/fragments/set_landuse

set SEGMENT  = $argv[1]
set SCENARIO = PQ20140613_01c
set LUS      = ( $perlnds $implnds )

set DIR      = `uuidgen`

mkdir -p ../../../tmp/$USER-scratch/$DIR
cd ../../../tmp/$USER-scratch/$DIR

#goto EOFC

if ( -e problem ) rm problem

foreach LU ( $LUS )
   echo $SEGMENT $LU $SCENARIO | ../../../code/bin/lugx.exe 
   #echo A10001 for PQ20140613_01c | ../../../code/bin/lugx.exe
end

if ( -e problem ) then
   cat problem
   exit
endif





#EOFC:

#cd ../../../code/src/eofc
foreach LU ( $LUS )
   cp ../../../tmp/wdm/land/$LU/NLDc8505HydBeWQ/${LU}${SEGMENT}.wdm .
end

if ( -e problem ) rm problem

#echo A10001 PQ20140613_01 1984 2005 1991 2000 | ./del.exe
#echo A10001 PQ20140613_01 1984 2005 1984 2005 | ./del.exe
echo $SEGMENT $SCENARIO 1984 2005 1985 2005 | ../../../code/bin/eofc.exe
#echo A10001 PQ20140613_01 1984 2005 1991 2000 | ./del.exe

if ( -e problem ) then
   cat problem
   exit
endif

foreach LU ( $LUS )
   mv ${LU}${SEGMENT}.wdm ../../../tmp/wdm/land/$LU/$SCENARIO/
end

cd ..
rm -r $DIR
