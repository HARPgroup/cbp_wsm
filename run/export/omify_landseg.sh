#!/bin/bash

i=$1
syear=$2
eyear=$3
scenario=$4

Rscript run/export/wdm_export_flow.R $scenario $i $syear $eyear $CBP_EXPORT_DIR $CBP_ROOT
# preload database tables into OM
create_landseg_table.sh $i $scenario p6 1
