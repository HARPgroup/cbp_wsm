#!/bin/csh
# run the hspf_config routine which finds all paths and sets variables
# such as CBP_ROOT, CBP_BIN, and CBP_SRC
. hspf_config

set SCENARIO = $argv[1]

source $CBP_ROOT/config/control/script/${SCENARIO}.con 

cd $SCRIPT_DIR

source $CBP_ROOT/config/seglists/${BASINS}.land

foreach SEG ( $segments )
   csh bhatt_run_lug_INFEXP_quiet_oneseg.csh ${SCENARIO} $SEG `uuidgen` &
end
wait

foreach SEG ( $segments )
   csh bhatt_run_land_oneseg.csh ${SCENARIO} $SEG `uuidgen` &
end
wait
