#!/bin/bash
if [ $# -lt 5 ]; then
  echo "Usage: add_sub_watershed.bat hydrocode downstream_seg model_version scenario drainage_area [param_seg=downstream/other riverseg ID]  [land_wdm_src=downstream/blank]"
  echo " - param_seg: optional river segment ID to use for in-stream properties SCRORG, ADCALC and HYDR, useful for "
  echo "   resegmenting tidal water segments (0000) or situation where param_seg had more suitable parameters"
  echo " - land_wdm_src: sep, rpa and rib data can be blank or come from param_seg watershed. blank is useful when param_seg <> downstream since land segments may differ."
  exit
fi
. hspf_config
# arguments:
# - hydrocode
# - receiving stream segment
# - model version (cbp-6.0)
# - cbp scenario (subsheds)
# - drainage area of subshed
# load name, rseg and area info

hydrocode=$1
downstream=$2
model_version=$3
scenario=$4
darea=$5
if [ $# -gt 5 ]; then
  param_seg=$6
else
  param_seg=$downstream
fi
if [ $# -gt 6 ]; then
  land_wdm_src=$7
else
  land_wdm_src=$downstream
fi
# get info
GEO=`cbp get_config $scenario river GEO`
TRANSPORT=`cbp get_config $scenario river TRANSPORT`
PARAMS=`cbp get_config $scenario river PARAMETERS`
LANDUSE=`cbp get_config $scenario river 'LAND USE'`
DIV=`cbp get_config $scenario river DIV`
SEPTIC=`cbp get_config $scenario river SEPTIC`
RIB=`cbp get_config $scenario river 'RIB LOADS'`
RPA=`cbp get_config $scenario river 'RPA LOADS'`
# name subshed or retrieve the name if it already exists
echo "Calling: Rscript $CBP_ROOT/run/resegment/subsheds_naming.R $hydrocode $CBP_ROOT/config/catalog/geo/${GEO}/rivernames.csv $model_version $downstream"
read -r subshed downstream <<< "$(Rscript $CBP_ROOT/run/resegment/subsheds_naming.R $hydrocode $CBP_ROOT/config/catalog/geo/${GEO}/rivernames.csv $model_version $downstream)"
if [ "$subshed" == "" ]; then
  echo "Could not create a new subshed code from $hydrocode as trib to $downstream"
  exit
fi
echo 'new subshed:' $subshed
echo 'flows into:' $downstream

# set and proportion watershed area
echo "Running: Rscript $CBP_ROOT/run/resegment/area_propor.R $CBP_ROOT/config/catalog/geo/${GEO}/land_water_area.csv $subshed $downstream $darea"
Rscript $CBP_ROOT/run/resegment/area_propor.R $CBP_ROOT/config/catalog/geo/${GEO}/land_water_area.csv $subshed $downstream $darea
echo 'land_water_area.csv proportioned'

# now get a list of subwatersheds for later use
read -r -a ss_pieces <<< $(echo $subshed | tr "_" " ")
seg_id="${ss_pieces[1]}"
cbp basingen.csh $scenario $seg_id
landsegs=`cbp get_landsegs $subshed`

# set land use area; iterate through multiple files and proportion them all
cnt=0
for i in $LANDUSE; do
  ((cnt++))
  if [[ $cnt -eq 1 ]]; then
    yr=$i
  fi
  if [[ $cnt -eq 4 ]]; then
    lu_file="input/scenario/river/land_use/land_use_${i}.csv"
    echo "LU file for $yr = " $lu_file
    Rscript $CBP_ROOT/run/resegment/area_propor.R $lu_file $subshed $downstream $darea

    # reset our counter
    cnt=0
  fi
done
echo 'land use files proportioned'

# transport
Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/transport/${TRANSPORT}_l2w.csv $subshed $downstream
echo '${TRANSPORT}_l2w.csv duplicated'
Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/transport/${TRANSPORT}_s2r.csv $subshed $downstream
echo '${TRANSPORT}_s2r.csv duplicated'

# Gen Info PARAMETERS
echo "Caling: Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/river/${PARAMS}/gen_info_rseg.csv $subshed $downstream"
Rscript $CBP_ROOT/run/resegment/copy_parent.R $CBP_ROOT/input/param/river/${PARAMS}/gen_info_rseg.csv $subshed $downstream
echo 'gen_info_rseg.csv duplicated'

# duplicate information from downstream to new subshed:
# transport, adcalc, 
echo "Calling: $CBP_ROOT/run/resegment/copy_ad_hy_sc $subshed $param_seg $model_version $scenario $land_wdm_src"
$CBP_ROOT/run/resegment/copy_ad_hy_sc $subshed $param_seg $model_version $scenario $land_wdm_src

