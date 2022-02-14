#!/bin/csh

set SEGMENT  = $argv[1]
set SCENARIO = $argv[2]
set YEAR1    = $argv[3]
set YEAR2    = $argv[4]

mkdir -p ../../../tmp/$USER-scratch/tempLUG
cd ../../../tmp/$USER-scratch/tempLUG

source ../../../run/fragments/set_landuse

foreach lu ( $perlnds )
   echo "lu = $lu"
#   echo $SEGMENT $lu NLDc8505HydBeQual | ../../../code/bin/lugx.exe
#   echo $SEGMENT $lu $SCENARIO | ../../../code/bin/lugx.exe 
end

cd ../../../code/src/eof/

cp -p ../../../tmp/wdm/land/for/NLDc8505HydBeQual/forA10001.wdm .
cp -p ../../../tmp/wdm/land/hom/NLDc8505HydBeQual/homA10001.wdm .

cp -p ../../../tmp/gbhatt-scratch/tempLAND/homA10001.wdm .

echo $SEGMENT $SCENARIO $YEAR1 $YEAR2 | ./del.exe
