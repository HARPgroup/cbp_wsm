#!/bin/bash

ip=$1

if [ -z "$ip" ]; then
  ip="/usr/local/bin"
fi

cp $PWD/bin_install/hsp_uci_version $ip/hsp_uci_version
cp $PWD/bin_install/find_config $ip/find_config
cp $PWD/bin_install/hspf_config $ip/hspf_config
cp $PWD/bin_install/cbp $ip/cbp
cp $PWD/bin_install/cbp_scenario_vars_bash $ip/
cp $PWD/bin_install/slurmq $ip/
cp $PWD/bin_install/wdmexp $ip/
# get cbp config block
cp $PWD/bin_install/get_config $ip/get_config
cp $PWD/bin_install/hspf.config.default $PWD/hspf.config
# mm scripts
cp $PWD/run/mm/mm_job_id $ip/
cp $PWD/run/mm/mm_job_name $ip/
cp $PWD/run/mm/mm_river_deps $ip/
cp $PWD/run/mm/mm_run_basin $ip/
cp $PWD/run/mm/mm_run_land $ip/
