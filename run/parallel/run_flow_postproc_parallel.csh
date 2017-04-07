#!/bin/csh 

 set scenario = $argv[1]
 set calscen = $argv[2]
 set basin = $argv[3]

# set nums = (01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 )
 set nums = (01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24)

 cd ../calibration/PWATER/
 foreach num ($nums)

   sbatch run_postproc_flow_calib.csh $scenario $calscen ${basin}${num}

 end
