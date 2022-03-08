#!/bin/csh

set SCENARIO = $argv[1]

source ../config/control/script/$SCENARIO.con

set modules = ''

if ( $DISTRIBUTE_SB_DATA == 1 ) then
     set modules = "$modules SB_DATA,"
endif
if ( $POINT_SOURCE_WDMS == 1 ) then
     set modules = "$modules PS.WDMs,"
endif
if ( $RIB_WDMS == 1 ) then
     set modules = "$modules RIB.WDMs,"
endif
if ( $RPA_WDMS == 1 ) then
     set modules = "$modules RPA.WDMs,"
endif
if ( $SEPTIC_WDMS == 1 ) then
     set modules = "$modules SEPTIC.WDMs,"
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
if ( $RUN_LUGX == 1 ) then
     set modules = "$modules LUGX,"
endif
if ( $RUN_UNEC == 1 ) then
     set modules = "$modules UNEC,"
endif
if ( $RUN_BODX == 1 ) then
     set modules = "$modules BODX,"
endif
if ( $RUN_ANNLOAD == 1 ) then
     set modules = "$modules ANNLOAD,"
endif
if ( $RUN_AVGLOAD == 1 ) then
     set modules = "$modules AVGLOAD,"
endif
if ( $RUN_ETM1 == 1 ) then
     if ( ! ($?SKIP_ETMPP) ) then
        set modules = "$modules ETM (${AVG_YEAR1}-${AVG_YEAR2}),"
        set SKIP_ETMPP = 0
     else if ( $SKIP_ETMPP == 0 ) then
        set modules = "$modules ETM (${AVG_YEAR1}-${AVG_YEAR2}),"
     else
        set modules = "$modules ETM ($ETMSCEN->$SCENARIO) (${AVG_YEAR1}-${AVG_YEAR2}),"
     endif
endif
if ( $RUN_RIVER == 1 ) then
     set modules = "$modules RIVER ($RUN_RIVER_MODE),"
endif
if ( $RUN_POSTPROC == 1) then
     set modules = "$modules POST.PROC,"
endif
if ( $RUN_EOT == 1) then
     set modules = "$modules EOT,"
endif
if ( $RUN_WQM_INPUT == 1) then
     set modules = "$modules WQM.INPUT (${WQM_REQUEST}:${WQM_YEAR1}-${WQM_YEAR2}),"
endif
if ( $RUN_CH3D_INPUT == 1) then
     set modules = "$modules CH3D,"
endif
if ( $RUN_REMOVE_DIRS == 1) then
     set modules = "$modules CLEAN,"
endif

echo $modules

