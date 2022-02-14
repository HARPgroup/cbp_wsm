#!/bin/csh

set REFCO2  = "363.22"
#set PRJCO2  = "427"     #YEAR 2025
#set PRJCO2  = "487"     #YEAR 2050
set PRJCO2  = "432"     # YEAR 2025 RCP85
#set PRJCO2  = "567"

set INP_PWATER = "P620171001WQf"
set OUT_PWATER = CO2_$PRJCO2


#set perlnds = ( for )
source ../../../../run/fragments/set_landuse

set TEMPFILE = "CMIP5_BCSD_ENSEMBLE_N20150521J96_1991-2000.TEMP"
set WINDFILE = "CMIP5_BCSD_ENSEMBLE_N20150521J96_1991-2000.WIND"


foreach LU ( $perlnds )

	echo "Processing Land-use... $LU"
        if (-e ../../../../input/param/$LU/$OUT_PWATER) rm -r ../../../../input/param/$LU/$OUT_PWATER
        mkdir -p ../../../../input/param/$LU/$OUT_PWATER
	cp -p ../../../../input/param/$LU/$INP_PWATER/* ../../../../input/param/$LU/$OUT_PWATER/
	echo $INP_PWATER $OUT_PWATER $LU $TEMPFILE $WINDFILE $REFCO2 $PRJCO2 | ./UpdateLZETP_GCM
	#sed -i -e 's///g' ../../../../input/param/$LU/$OUT_PWATER/PWATER.csv
	#`unix2dos ../../../../input/param/$LU/$OUT_PWATER/PWATER.csv`
        #find ../../../../input/param/$LU/$OUT_PWATER/ -name PWATER.csv | xargs perl -pi -e "s/,//g"
        #find ../../../../input/param/$LU/$OUT_PWATER/ -name PWATER.csv | xargs perl -pi -e "s///g"

end

foreach LU ( $implnds )

        echo "Processing Land-use... $LU"
        if (-e ../../../../input/param/$LU/$OUT_PWATER) rm -r ../../../../input/param/$LU/$OUT_PWATER
        cp -pr ../../../../input/param/$LU/$INP_PWATER ../../../../input/param/$LU/$OUT_PWATER

end

if (-e ../../../../input/param/common/$OUT_PWATER) rm -r ../../../../input/param/common/$OUT_PWATER
cp -pr ../../../../input/param/common/$INP_PWATER ../../../../input/param/common/$OUT_PWATER

echo ""
echo "Apply '%s///g | wn' to the ../../../../input/param/common/$OUT_PWATER/PWATER.csv files"
echo ""
