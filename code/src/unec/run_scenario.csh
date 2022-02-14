#!/bin/csh

set SCENARIO    = $argv[1]
set CALIBRATION = $argv[2]
set SEGMENT     = $argv[3]

source ../../../run/fragments/set_landuse
set LUS      = ( $perlnds $implnds )

set DIR      = `uuidgen`
mkdir -p ../../../tmp/$USER-scratch/$DIR
cd ../../../tmp/$USER-scratch/$DIR

#??
goto EOFC

if ( -e problem ) rm problem
foreach LU ( $LUS )
   echo $SEGMENT $LU $SCENARIO | ../../../code/bin/lugx.exe 
   #echo A10001 for PQ20140613_01c | ../../../code/bin/lugx.exe
end
if ( -e problem ) then
   cat problem
   exit
endif


#??
EOFC:

foreach LU ( $LUS )
   cp -v ../../../tmp/wdm/land/$LU/$CALIBRATION/${LU}${SEGMENT}.wdm .
end

if ( -e problem ) rm problem

#echo A10001 PQ20140613_01 1984 2005 1991 2000 | ./del.exe
#echo A10001 PQ20140613_01 1984 2005 1984 2005 | ./del.exe
echo $SEGMENT $SCENARIO 1984 2005 1991 2000 | ../../../code/bin/eofc.exe
#echo A10001 PQ20140613_01 1984 2005 1991 2000 | ./del.exe

if ( -e problem ) then
   cat problem
   exit
endif

#??
exit
foreach LU ( $LUS )
   mv ${LU}${SEGMENT}.wdm ../../../tmp/wdm/land/$LU/$SCENARIO/
end

cd ..
rm -r $DIR
