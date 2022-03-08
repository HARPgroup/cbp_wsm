#!/bin/csh

set SBscen = $argv[1]

cd ../../input/scenario/from_SB/Phase_6_SCENARIO/

ls -l $SBscen/

if ( -e ../../river/bmppassthru/bmp_passthru_$SBscen.csv ) rm ../../river/bmppassthru/bmp_passthru_$SBscen.csv
cp -pv $SBscen/bmp_passthru_$SBscen.csv    ../../river/bmppassthru/bmp_passthru_$SBscen.csv

if ( -e ../../river/bmppounds/bmp_pounds_$SBscen.csv ) rm ../../river/bmppounds/bmp_pounds_$SBscen.csv
cp -pv $SBscen/bmp_pounds_$SBscen.csv   ../../river/bmppounds/bmp_pounds_$SBscen.csv

if ( -e $SBscen/afocfoload_$SBscen.csv ) then
    if ( -e ../../river/loads/afocfoload_$SBscen.csv ) rm ../../river/loads/afocfoload_$SBscen.csv
    cp -pv $SBscen/afocfoload_$SBscen.csv  ../../river/loads/afocfoload_$SBscen.csv
else
    if ( -e ../../river/loads/afocfoload_$SBscen.csv ) rm ../../river/loads/afocfoload_$SBscen.csv
    cp -pv $SBscen/afocafoload_$SBscen.csv ../../river/loads/afocfoload_$SBscen.csv
endif

if ( -e ../../land/cover/cover_$SBscen.csv ) rm ../../land/cover/cover_$SBscen.csv
cp -pv $SBscen/cover_$SBscen.csv  ../../land/cover/cover_$SBscen.csv

if ( -e ../../land/dets/dets_$SBscen.csv ) rm ../../land/dets/dets_$SBscen.csv
cp -pv $SBscen/dets_$SBscen.csv        ../../land/dets/dets_$SBscen.csv

if ( -e ../../land/fertilizer/fertilizer_$SBscen.csv ) rm ../../land/fertilizer/fertilizer_$SBscen.csv
cp -pv $SBscen/fertilizer_$SBscen.csv  ../../land/fertilizer/fertilizer_$SBscen.csv

if ( -e ../../river/land_use/land_use_$SBscen.csv ) rm ../../river/land_use/land_use_$SBscen.csv
cp -pv $SBscen/land_use_$SBscen.csv    ../../river/land_use/land_use_$SBscen.csv

if ( -e ../../land/legume/legume_$SBscen.csv ) rm ../../land/legume/legume_$SBscen.csv
cp -pv $SBscen/legume_$SBscen.csv      ../../land/legume/legume_$SBscen.csv

if ( -e ../../land/manure/manure_$SBscen.csv ) rm ../../land/manure/manure_$SBscen.csv
cp -pv $SBscen/manure_$SBscen.csv      ../../land/manure/manure_$SBscen.csv

if ( -e ../../land/biosolids/biosolids_$SBscen.csv ) rm ../../land/biosolids/biosolids_$SBscen.csv
cp -pv $SBscen/biosolids_$SBscen.csv      ../../land/biosolids/biosolids_$SBscen.csv

if ( -e ../../land/annual_uptake/max_uptake_$SBscen.csv ) rm ../../land/annual_uptake/max_uptake_$SBscen.csv
cp -pv $SBscen/max_uptake_$SBscen.csv  ../../land/annual_uptake/max_uptake_$SBscen.csv

if ( -e ../../land/monthly_fraction_uptake/uptakecurve_$SBscen.csv ) rm ../../land/monthly_fraction_uptake/uptakecurve_$SBscen.csv
cp -pv $SBscen/uptakecurve_$SBscen.csv ../../land/monthly_fraction_uptake/uptakecurve_$SBscen.csv

if ( -e ../../../unformatted/septic/septic_$SBscen.csv ) rm ../../../unformatted/septic/septic_$SBscen.csv
cp -pv $SBscen/septic_$SBscen.csv      ../../../unformatted/septic/septic_$SBscen.csv

if ( -e ../../../unformatted/rpaload/rpaload_$SBscen.csv ) rm ../../../unformatted/rpaload/rpaload_$SBscen.csv
cp -pv $SBscen/rpaload_$SBscen.csv      ../../../unformatted/rpaload/rpaload_$SBscen.csv

exit 0 
