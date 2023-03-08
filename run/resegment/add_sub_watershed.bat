#!/bin/bash
if [ $# -lt 5 ]; then
  echo "Usage: add_sub_watershed.bat hydrocode downstream_seg model_version scenario drainage_area"
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
read -r subshed downstream <<< "$(Rscript $CBP_ROOT/run/resegment/subsheds_naming.R $hydrocode $CBP_ROOT/config/catalog/geo/${GEO}/rivernames.csv $model_version)"
echo 'new subshed:' $subshed

# set and proportion watershed area
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

# duplicate information from downstream to new subshed:
# transport, adcalc, 
$CBP_ROOT/resegment/copy_ad_hy_sc $subshed $downstream $model_version $scenario

