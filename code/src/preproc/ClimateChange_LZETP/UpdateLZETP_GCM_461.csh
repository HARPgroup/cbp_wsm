#!/bin/csh

set REFCO2  = "363.22"
set PRJCO2  = "461"

set INP_PWATER = "p532cal"
set OUT_PWATER = CO2_$PRJCO2


set perlnds = ( for )
source ../../../../run/fragments/set_landuse

set TEMPFILE = "p532062211_xyz_1984-2005.TEMP"
set WINDFILE = "NLDAS2_20150124_1980010100-2014123123.WIND"


foreach LU ( $perlnds )

	echo "Processing Land-use... $LU"
	cp -pr ../../../../input/param/$LU/$INP_PWATER ../../../../input/param/$LU/$OUT_PWATER
	echo $INP_PWATER $OUT_PWATER $LU $TEMPFILE $WINDFILE $REFCO2 $PRJCO2 | ./UpdateLZETP_GCM

end

foreach LU ( $implnds )

        echo "Processing Land-use... $LU"
        cp -pr ../../../../input/param/$LU/$INP_PWATER ../../../../input/param/$LU/$OUT_PWATER

end

cp -pr ../../../../input/param/common/$INP_PWATER ../../../../input/param/common/$OUT_PWATER
