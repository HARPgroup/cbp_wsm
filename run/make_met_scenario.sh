#!/bin/bash


start_date=$1
end_date=$2
met_name=$3
prad_name=$4
nldas_dir=$5
model_dir=$6

# code to run the WDM creation goes here 

# move the met WDMs
met_dir="$model_dir/input/scenario/climate/met/$met_name"
mkdir $met_dir
# create F landsegs from A for p532 
asegs=`ls $nldas_dir/met_A*`
for al in $asegs; do
  fl=${al//[A]/F}
  cp $al $fl
done 
rsync $nldas_dir/met*.wdm $met_dir/

# move the prad WDMs
prad_dir="$model_dir/input/scenario/climate/prad/$prad_name"
mkdir $prad_dir
# create F landsegs from A for p532 
asegs=`ls $nldas_dir/prad_A*`
for al in $asegs; do
  fl=${al//[A]/F}
  cp $al $fl
done 
rsync $nldas_dir/prad*.wdm "$prad_dir/"
