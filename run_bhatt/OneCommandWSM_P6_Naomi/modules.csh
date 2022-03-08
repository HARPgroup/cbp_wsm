#!/bin/csh

set SCENARIO = $argv[1]

source ../config/control/script/$SCENARIO.con

set modules = ''

if ( $DISTRIBUTE_SB_DATA == 1 ) then
     set modules = "$modules SB_DATA,"
endif
if ( $SEPTIC_WDMS == 1 ) then
     set modules = "$modules SEPTIC.WDMs,"
endif
if ( $POINT_SOURCE_WDMS == 1 ) then
     set modules = "$modules PS.WDMs,"
endif
if ( $RUN_MAKE_LAND_DIR == 1 ) then
     set modules = "$modules LAND.DIR,"
endif
if ( $RUN_MAKE_RIVER_DIR == 1 ) then
     set modules = "$modules RIVER.DIR,"
endif
if ( $RUN_LUG == 1 ) then
     set modules = "$modules LUG,"
endif
if ( $RUN_LAND == 1 ) then
     set modules = "$modules LAND,"
endif
if ( $RUN_ETM1 == 1 ) then
     set modules = "$modules ETM,"
endif
if ( $RUN_RIVER == 1 ) then
     set modules = "$modules RIVER ($RUN_RIVER_MODE),"
endif
if ( $RUN_POSTPROC == 1) then
     set modules = "$modules POST.PROC,"
endif
if ( $RUN_WQM_INPUT == 1) then
     set modules = "$modules WQM.INPUT,"
endif
if ( $RUN_CH3D_INPUT == 1) then
     set modules = "$modules CH3D,"
endif
if ( $RUN_REMOVE_DIRS == 1) then
     set modules = "$modules CLEAN,"
endif

echo $modules

