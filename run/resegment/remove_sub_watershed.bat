#!/bin/bash
if [ $# -lt 4 ]; then
  echo "Usage: remove_sub_watershed.bat subshed downstream_seg model_version scenario"
  exit
fi
. hspf_config
# arguments:
# - subshed
# - receiving stream segment
# - model version (cbp-6.0)
# - cbp scenario (subsheds)
# - drainage area of subshed
# load name, rseg and area info

subshed=$1
downstream=$2
model_version=$3
scenario=$4

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

# set and proportion watershed area
echo "Zeroing land use in $subshed"
echo "Running: $CBP_ROOT/run/resegment/sub_divide_watershed $subshed $downstream $model_version $scenario 0"
$CBP_ROOT/run/resegment/sub_divide_watershed $subshed $downstream $model_version $scenario 0

# remove segment from master list
cp $CBP_ROOT/config/catalog/geo/$GEO/rivernames.csv /tmp/rivernames.csv
cat /tmp/rivernames.csv | grep -v $subshed > $CBP_ROOT/config/catalog/geo/$GEO/rivernames.csv
echo "Removed $subshed from config/catalog/geo/$GEO/rivernames.csv"


# transport
# PHASE 5 seems to have only this:
cp $CBP_ROOT/input/param/transport/${TRANSPORT}.csv /tmp/${TRANSPORT}.csv
cat /tmp/${TRANSPORT}.csv | grep -v $subshed >  $CBP_ROOT/input/param/transport/${TRANSPORT}.csv
echo "Removed $subshed from input/param/transport/${TRANSPORT}.csv"


# Gen Info PARAMETERS
cp $CBP_ROOT/input/param/river/${PARAMS}/gen_info_rseg.csv /tmp/gen_info_rseg.csv
cat /tmp/gen_info_rseg.csv | grep -v $subshed > $CBP_ROOT/input/param/river/${PARAMS}/gen_info_rseg.csv
echo "Removed $subshed from input/param/river/gen_info_rseg.csv"

# duplicate information from downstream to new subshed:
# transport, adcalc, 
