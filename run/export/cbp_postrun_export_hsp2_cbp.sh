#!/bin/bash
# todo: this should not only handle 2 types, we should have a directory 
# with a model file name convention, like "cbp_postrun_export_[model].sh"
# and if that file exists, we run it, otherwise, skip it
scenario=$1
uci_path=$2
uci=`basename $uci_path`
seg=`basename $uci .uci`
h5file=$uci_base'.h5'
# Process this if it is a stream or has a stream active
has_hydr=`Rscript $CBP_BIN/export/detect_data_source.R $h5file "/RESULTS/RCHRES_R001/HYDR"`
if [ "$has_hydr" -eq "1" ]; then
  #csvfile =
  # Run post-process extract routine
  echo "Exporting HYDR data for $seg"
  ds="/RESULTS/RCHRES_R001/HYDR/table"
  mod="hydr"
  csvfile = ${seg}_hydr'.csv'
  echo "Notice: Rscript $CBP_ROOT/run/export/export_hsp_h5.R $h5file $csvfile $ds"
  Rscript $CBP_ROOT/run/export/export_hsp_h5.R $h5file $csvfile $ds
  Run conversion script to add Qout and other derived/alias columns
  echo "Notice (unit conversions): Rscript $CBP_ROOT/run/export/hsp_hydr_conversion.R $csvfile"
  Rscript $CBP_ROOT/run/export/hsp_hydr_conversion.R $csvfile 
  # Prep outflow data for export to river wdm
  wdmcsv = ${seg}_rovol'.csv'
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
  rm message.wdm
  # todo: add the call to run the river hsp2 summary script here
  echo "Notice(analyze): Rscript $CBP_ROOT/run/export/hsp_hydr_analysis.R $seg $scenario $CBP_EXPORT_DIR/river/$scenario/$mod/ $CBP_EXPORT_DIR/river/$scenario/$mod/images/ $MODEL_VERSION_CODE"
  Rscript $CBP_ROOT/run/export/hsp_hydr_analysis.R $seg $scenario $CBP_EXPORT_DIR/river/$scenario/$mod/ $CBP_EXPORT_DIR/river/$scenario/$mod/images/ $MODEL_VERSION_CODE
  # move the WDM to stream folder so next watershed can access it's upstream inflows
  # we curently do thisfor both hspf and hsp2, but later may move this into the 
  # hspf specific code block
  mv $seg'.wdm' $CBP_ROOT/tmp/wdm/river/$scenario/stream/ -f

fi

# if is land, handle it
lu="${seg:0:3}"
lseg="${seg:3:6}"
has_pwater=`Rscript $CBP_BIN/export/detect_data_source.R $h5file "/RESULTS/PERLND_P001/PWATER"`
if [ "$has_pwater" -eq "1" ]; then
  run_landsum=1
  ds="/RESULTS/PERLND_P001/PWATER/table"
  csvfile=$CBP_EXPORT_DIR/land/$scenario/pwater/$lu${lseg}_pwater'.csv'
fi

has_iwater=`Rscript $CBP_BIN/export/detect_data_source.R $h5file "/RESULTS/PERLND_I001/IWATER"`
if [ "$has_pwater" -eq "1" ]; then
  run_landsum=1
  ds="/RESULTS/IMPLND_I001/IWATER/table"
  csvfile = $CBP_EXPORT_DIR/land/$scenario/iwater/$lu${seg}_iwater'.csv'
fi

if [ "$run_landsum" -eq "1" ]; then
  echo "Rscript $CBP_ROOT/run/export/export_hsp_h5.R $h5file $csvfile $ds"
#  Rscript $CBP_ROOT/run/export/export_hsp_h5.R $h5file $csvfile $ds
  # Now we run summary scripts.  At some point we should maybe moce to postproc area
  # but there are advantages to having this feedback early in model execution
  echo "$CBP_ROOT/run/export/summarize_landseg.csh $scenario $seg $lu $CBP_ROOT $CBP_EXPORT_DIR"
  $CBP_ROOT/run/export/summarize_landseg.csh $scenario $seg $lu $CBP_ROOT $CBP_EXPORT_DIR
fi

echo "Cleaning up $h5file"
rm $h5file
