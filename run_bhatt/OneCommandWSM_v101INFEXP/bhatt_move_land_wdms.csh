#!/bin/csh

set SRC_SCENARIO = $argv[1]
set DST_SCENARIO = $argv[2]

source ../../run/fragments/set_landuse

printf "Land WDMs of $DST_SCENARIO will be remove. Continue? [Y/N] "
set response = $<

if ( $response == a'y' || $response == 'Y' ) then
	printf "Moving ... "
	foreach lu ( $perlnds $implnds )
        	printf " $lu"
                if ( -e ../../tmp/wdm/land/$lu/${DST_SCENARIO} ) then
	        	rm -r  ../../tmp/wdm/land/$lu/${DST_SCENARIO}
		endif
        	mv ../../tmp/wdm/land/${lu}/${SRC_SCENARIO} ../../tmp/wdm/land/$lu/${DST_SCENARIO}
	end
endif
