#!/bin/bash

ip=$1

if [ -z "$ip" ]; then
  ip="/usr/local/bin"
fi

cp $PWD/bin_install/find_config $ip/find_config
cp $PWD/bin_install/hspf_config $ip/hspf_config
cp $PWD/bin_install/cbp $ip/cbp
# get cbp config block
cp $PWD/bin_install/get_config $ip/get_config
cp $PWD/bin_install/hspf.config.default $PWD/hspf.config
# mm scripts
cp $PWD/run/mm/mm_job_id $ip/
cp $PWD/run/mm/mm_job_name $ip/
cp $PWD/run/mm/mm_river_deps $ip/
cp $PWD/run/mm/mm_run_basin $ip/
cp $PWD/run/mm/mm_run_land $ip/
