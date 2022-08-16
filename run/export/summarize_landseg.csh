#!/bin/csh

set scenario_name=$1
set landseg=$2
set landuse=$3
set CBP_ROOT=$4
set CBP_EXPORT_DIR=$5

set image_file_path=$CBP_EXPORT_DIR/land/$scenario/images

Rscript $CBP_ROOT/run/export/hsp_pwater.R $landseg $scenario_name $landuse $output_file_path $image_file_path
Rscript $CBP_ROOT/run/export/hsp_pwater_stats.R $landseg $scenario_name $landuse $output_file_path $image_file_path
