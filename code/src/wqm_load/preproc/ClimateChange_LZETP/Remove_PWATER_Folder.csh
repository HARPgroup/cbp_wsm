#!/bin/csh

set OUT_PWATER = $argv[1]

source ../../../../run/fragments/set_landuse

printf "Remove Land Paramters: $OUT_PWATER  ? [Y/N] "

set response = $<

if ( $response == "Y" || $response == "y" ) then
	foreach LU ( $perlnds $implnds )
		rm -r ../../../../input/param/$LU/$OUT_PWATER
	end
endif
