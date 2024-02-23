#!/bin/csh

set BASIN = $argv[1]

source ../../../seglists/${BASIN}.riv

foreach SEG ( $segments )
   grep $SEG land_water_area.csv
end
