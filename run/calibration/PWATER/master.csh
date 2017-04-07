#!/bin/csh

set basin = $argv[1]

source ../../fragments/set_tree

source $tree/config/seglists/${basin}.riv

foreach seg ( $segments )
     srun --nodes=1 --ntasks=1 --exclusive del.csh p532cal_102413 p532hyd $seg &
end

wait
