#!/bin/csh

source ../../../config/seglists/allBay.land

foreach seg ($segments)
   #sbatch -pdebug run.csh $seg
   sbatch run.csh $seg
end
