#!/bin/csh

# csh bhatt_distribute_sb_data.csh P6DRAFT_LATEJULY . 2010WIP2_20170727

set FOLDER      = $argv[1]
set DATAVERSION = $argv[2]
set SBscen       = $argv[3]
set WSMscen      = $argv[4]

echo "$FOLDER $DATAVERSION $SBscen $WSMscen"
echo $SBscen

cd ../../input/scenario/from_SB/${FOLDER}/

if ( $SBscen != $WSMscen ) then
   mkdir -p $DATAVERSION/$WSMscen
   ls -altr $DATAVERSION/$SBscen/
   cp -vp $DATAVERSION/$SBscen/* $DATAVERSION/$WSMscen
   cd $DATAVERSION/$WSMscen
   `rename "$SBscen" "$WSMscen" *.csv`
   cd ../../
endif

ls -l $DATAVERSION/$WSMscen/

mkdir -p ../../../calib/target/$WSMscen

if ( ${#argv} == 5 ) then
   if ( $argv[5] == 'OVERWRITE' ) then
      cp -vp $DATAVERSION/$WSMscen/????.csv    ../../../calib/target/$WSMscen/
   else
      cp -vip $DATAVERSION/$WSMscen/????.csv    ../../../calib/target/$WSMscen/
   endif
else
   cp -vip $DATAVERSION/$WSMscen/????.csv    ../../../calib/target/$WSMscen/
endif

if ( -e ../../river/scenfac/scenfac_SEDM_$WSMscen.csv ) rm ../../river/scenfac/scenfac_SEDM_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/scenfac_SEDM_$WSMscen.csv    ../../river/scenfac/scenfac_SEDM_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../river/bmps/bmp_passthru_$WSMscen.csv ) rm ../../river/bmps/bmp_passthru_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/bmp_passthru_$WSMscen.csv    ../../river/bmps/bmp_passthru_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../river/bmps/bmp_loadredux_EOS_$WSMscen.csv ) rm ../../river/bmps/bmp_loadredux_EOS_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/bmp_loadredux_EOS_$WSMscen.csv   ../../river/bmps/bmp_loadredux_EOS_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../river/bmps/bmp_loadredux_EOT_$WSMscen.csv ) rm ../../river/bmps/bmp_loadredux_EOT_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/bmp_loadredux_EOT_$WSMscen.csv   ../../river/bmps/bmp_loadredux_EOT_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/loads/fnp_$WSMscen.csv ) rm ../../land/loads/fnp_$WSMscen.csv
    cp -pv $DATAVERSION/$WSMscen/fnp_$WSMscen.csv  ../../land/loads/fnp_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/loads/fsp_$WSMscen.csv ) rm ../../land/loads/fsp_$WSMscen.csv
    cp -pv $DATAVERSION/$WSMscen/fsp_$WSMscen.csv  ../../land/loads/fsp_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/loads/sho_$WSMscen.csv ) rm ../../land/loads/sho_$WSMscen.csv
    cp -pv $DATAVERSION/$WSMscen/sho_$WSMscen.csv  ../../land/loads/sho_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/loads/stb_$WSMscen.csv ) rm ../../land/loads/stb_$WSMscen.csv
    cp -pv $DATAVERSION/$WSMscen/stb_$WSMscen.csv  ../../land/loads/stb_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/cover/cover_$WSMscen.csv ) rm ../../land/cover/cover_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/cover_$WSMscen.csv  ../../land/cover/cover_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/dets/dets_$WSMscen.csv ) rm ../../land/dets/dets_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/dets_$WSMscen.csv        ../../land/dets/dets_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/fertilizer/fertilizer_$WSMscen.csv ) rm ../../land/fertilizer/fertilizer_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/fertilizer_$WSMscen.csv  ../../land/fertilizer/fertilizer_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../river/land_use/land_use_$WSMscen.csv ) rm ../../river/land_use/land_use_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/land_use_$WSMscen.csv    ../../river/land_use/land_use_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/legume/legume_$WSMscen.csv ) rm ../../land/legume/legume_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/legume_$WSMscen.csv      ../../land/legume/legume_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/manure/manure_$WSMscen.csv ) rm ../../land/manure/manure_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/manure_$WSMscen.csv      ../../land/manure/manure_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/wep/wep_$WSMscen.csv ) rm ../../land/wep/wep_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/wep_$WSMscen.csv      ../../land/wep/wep_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/annual_uptake/max_uptake_$WSMscen.csv ) rm ../../land/annual_uptake/max_uptake_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/max_uptake_$WSMscen.csv  ../../land/annual_uptake/max_uptake_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/monthly_fraction_uptake/uptakecurve_$WSMscen.csv ) rm ../../land/monthly_fraction_uptake/uptakecurve_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/uptakecurve_$WSMscen.csv ../../land/monthly_fraction_uptake/uptakecurve_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../../unformatted/septic/sep_$WSMscen.csv ) rm ../../../unformatted/septic/sep_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/sep_$WSMscen.csv      ../../../unformatted/septic/sep_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../../unformatted/rpaload/rpa_$WSMscen.csv ) rm ../../../unformatted/rpaload/rpa_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/rpa_$WSMscen.csv      ../../../unformatted/rpaload/rpa_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../../unformatted/rib/rib_$WSMscen.csv ) rm ../../../unformatted/rib/rib_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/rib_$WSMscen.csv      ../../../unformatted/rib/rib_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

mkdir -p ../../../unformatted/point_source/$WSMscen

if ( -e ../../../unformatted/point_source/$WSMscen/mun_$WSMscen.csv ) rm ../../../unformatted/point_source/$WSMscen/mun_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/mun_$WSMscen.csv      ../../../unformatted/point_source/$WSMscen/mun_$WSMscen.csv
find ../../../unformatted/point_source/$WSMscen/ -name mun_$WSMscen.csv | xargs perl -pi -e "s/,001B,/,001,/g"
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../../unformatted/point_source/$WSMscen/ind_$WSMscen.csv ) rm ../../../unformatted/point_source/$WSMscen/ind_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/ind_$WSMscen.csv      ../../../unformatted/point_source/$WSMscen/ind_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../../unformatted/point_source/$WSMscen/cso_passthru_$WSMscen.csv ) rm ../../../unformatted/point_source/$WSMscen/cso_passthru_$WSMscen.csv
cp -pv $DATAVERSION/$WSMscen/cso_passthru_$WSMscen.csv      ../../../unformatted/point_source/$WSMscen/cso_passthru_$WSMscen.csv
set failed = $status; if( $failed == 1 ) exit 1

exit 0 
