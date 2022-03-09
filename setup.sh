#!/bin/bash

ip=$1

if [ -z "$ip" ]; then
  ip="/usr/local/bin"
fi

echo "If you want a central copy of the hspf.config file to be available in /etc"
echo "do the following:"
echo "cp hspf.config.etc /etc/hspf.config"
echo "then edit the file to point towards the prefered HSPF executable and library path"

# central config finder and the "cbp" command 
#  which allows exec'ing anything from any place in the tree
cp $PWD/bin/find_config $ip/find_config
cp $PWD/bin/hspf_config $ip/hspf_config
cp $PWD/bin/hspf.config.default $PWD/hspf.config
cp $PWD/bin/cbp $ip/cbp

# WDM export routines
cp $PWD/bin/wdmexp $ip/wdmexp
cp $PWD/code/bin/quick_wdm_2_txt_hour_2_hour /usr/local/bin/wdm2text 

# HSPF Executables and message.wdm
cp $PWD/code/src/hspf/lib3.2/lib_data/message.wdm /usr/local/lib/hspf/message.wdm
cp $PWD/code/src/hspf/hspf11.1/bin/hspf_ICPRB /usr/local/bin/hspf_ICPRB 

# NLDAS and wdm population routines
cp $PWD/run/prad_met/wdm_insert_ALL /usr/local/bin/wdm_insert_ALL 
cp $PWD/run/nldas/grid2land.sh  /usr/local/bin/grid2land.sh
cp $PWD/run/nldas/p5_g2a.bash /usr/local/bin/p5_g2a.bash
# wdm_generation_p5.bash  
# wdm_generation_p6.bash
