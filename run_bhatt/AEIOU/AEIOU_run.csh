#!/bin/csh

set SID = 44
set EID = 56

set IID = $SID

while ( $IID <= $EID )
   sbatch AEIOU_process.csh AEIOU_N20180213 `printf '%02d' $IID`
   @ IID = $IID + 1
   sleep 210
end
