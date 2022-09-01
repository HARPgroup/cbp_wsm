#!/bin/csh
# todo: this should not only handle 2 types, we should have a directory 
# with a model file name convention, like "cbp_postrun_export_[model].sh"
# and if that file exists, we run it, otherwise, skip it
dir_path=$1
scenario=$2
seg=$3

if ($HSP_VERSION == "hsp2") then
  set h5file = $seg'.h5'
  #csvfile =
  # Run post-process extract routine
  echo "Exporting HYDR data for $seg"
  set ds="/RESULTS/RCHRES_R001/HYDR/table"
  set mod="hydr"
  set csvfile = ${seg}_hydr'.csv'
  echo "Notice: Rscript $CBP_ROOT/run/export/export_hsp_h5.R $h5file $csvfile $ds"
  Rscript $CBP_ROOT/run/export/export_hsp_h5.R $h5file $csvfile $ds
  Run conversion script to add Qout and other derived/alias columns
  echo "Notice (unit conversions): Rscript $CBP_ROOT/run/export/hsp_hydr_conversion.R $csvfile"
  Rscript $CBP_ROOT/run/export/hsp_hydr_conversion.R $csvfile 
  # Prep outflow data for export to river wdm
  set wdmcsv = ${seg}_rovol'.csv'
  echo "Notice: Rscript $CBP_ROOT/run/export/csv_export_wdm_format.R $csvfile $wdmcsv ROVOL"
  Rscript $CBP_ROOT/run/export/csv_export_wdm_format.R $csvfile $wdmcsv ROVOL
  # Push ROVOL into wdm
  # copy here cause it is hardto send these paths to wdm_insert_one, need escape?
  cp /usr/local/lib/hspf/message.wdm ./
  echo ${seg}.wdm $wdmcsv 111 1 w message.wdm | wdm_insert_one
  # move all files to the model data archive
  mv $csvfile $CBP_EXPORT_DIR/river/$scenario/$mod/ -f
  chgrp $MODEL_FILES_GROUP $CBP_EXPORT_DIR/river/$scenario/$mod/$csvfile
  chmod 664 $csvfile
  mv $wdmcsv $CBP_EXPORT_DIR/river/$scenario/$mod/ -f
  chgrp $MODEL_FILES_GROUP $CBP_EXPORT_DIR/river/$scenario/$mod/$wdmcsv
  chmod 664 $wdmcsv
  # Remove message and h5 file to save space
  echo "Cleaning up $h5file"
  rm $h5file
  rm message.wdm
  # todo: add the call to run the river hsp2 summary script here
  echo "Notice(analyze): Rscript $CBP_ROOT/run/export/hsp_hydr_analysis.R $seg $scenario $CBP_EXPORT_DIR/river/$scenario/$mod/ $CBP_EXPORT_DIR/river/$scenario/$mod/images/ $MODEL_VERSION_CODE"
  Rscript $CBP_ROOT/run/export/hsp_hydr_analysis.R $seg $scenario $CBP_EXPORT_DIR/river/$scenario/$mod/ $CBP_EXPORT_DIR/river/$scenario/$mod/images/ $MODEL_VERSION_CODE
else
  echo $inp | $hspf
  # Check the output to see if its OK
  tail -1 $seg'.ech' > EOJtest$$
  diff $tree/run/fragments/EOJ EOJtest$$ > diffeoj
  rm EOJtest$$
  if (!(-z diffeoj)) then
    if (-e problem) then
      rm problem
    endif
    echo 'river segment: ' $seg ' did not run'  >problem
    echo '  input file ' $inp >>problem
    set fnam = $tree/tmp/scratch/temp$$/$seg'.ech '
    echo '  check the file ' $fnam >>problem
    cat problem
    exit
  endif
  mv $seg'.out' $tree/output/hspf/river/out/$scenario/ -f
  mv $seg'.ech' $tree/output/hspf/river/ech/$scenario/ -f
  # export the flow data
  if ( $?START_YEAR ) then
    echo "echo ${seg}.wdm,$START_YEAR,$END_YEAR,111 | wdm2text"
    echo "${seg}.wdm,$START_YEAR,$END_YEAR,111" | wdm2text
    mv $seg'_0111.csv' $CBP_EXPORT_DIR/river/$scenario/stream/ -f
  endif
  mv ps_sep_div_ams_$scenario'_'$seg'.wdm' $tree/tmp/wdm/river/$scenario/eos/ -f
  # todo: add the call to run the river hspf summary script here
endif
rm message.wdm 
# move the WDM to stream folder so next watershed can access it's upstream inflows
# we curently do thisfor both hspf and hsp2, but later may move this into the 
# hspf specific code block
mv $seg'.wdm' $tree/tmp/wdm/river/$scenario/stream/ -f
