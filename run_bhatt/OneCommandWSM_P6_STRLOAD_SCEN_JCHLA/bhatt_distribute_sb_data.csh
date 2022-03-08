#!/bin/csh

# csh bhatt_distribute_sb_data.csh P6DRAFT_LATEJULY . 2010WIP2_20170727

set FOLDER      = $argv[1]
set DATAVERSION = $argv[2]
set SBscen      = $argv[3]

cd ../../input/scenario/from_SB/${FOLDER}/

ls -l $DATAVERSION/$SBscen/

mkdir -p ../../../calib/target/$SBscen

if ( ${#argv} == 4 ) then
   if ( $argv[4] == 'OVERWRITE' ) then
      cp -vp $DATAVERSION/$SBscen/????.csv    ../../../calib/target/$SBscen/
   else
      cp -vip $DATAVERSION/$SBscen/????.csv    ../../../calib/target/$SBscen/
   endif
else
   cp -vip $DATAVERSION/$SBscen/????.csv    ../../../calib/target/$SBscen/
endif

if ( -e ../../river/scenfac/scenfac_SEDM_$SBscen.csv ) rm ../../river/scenfac/scenfac_SEDM_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/scenfac_SEDM_$SBscen.csv    ../../river/scenfac/scenfac_SEDM_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../river/bmps/bmp_passthru_$SBscen.csv ) rm ../../river/bmps/bmp_passthru_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/bmp_passthru_$SBscen.csv    ../../river/bmps/bmp_passthru_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../river/bmps/bmp_loadredux_EOS_$SBscen.csv ) rm ../../river/bmps/bmp_loadredux_EOS_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/bmp_loadredux_EOS_$SBscen.csv   ../../river/bmps/bmp_loadredux_EOS_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../river/bmps/bmp_loadredux_EOT_$SBscen.csv ) rm ../../river/bmps/bmp_loadredux_EOT_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/bmp_loadredux_EOT_$SBscen.csv   ../../river/bmps/bmp_loadredux_EOT_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/loads/fnp_$SBscen.csv ) rm ../../land/loads/fnp_$SBscen.csv
    cp -pv $DATAVERSION/$SBscen/fnp_$SBscen.csv  ../../land/loads/fnp_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/loads/fsp_$SBscen.csv ) rm ../../land/loads/fsp_$SBscen.csv
    cp -pv $DATAVERSION/$SBscen/fsp_$SBscen.csv  ../../land/loads/fsp_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/loads/sho_$SBscen.csv ) rm ../../land/loads/sho_$SBscen.csv
    cp -pv $DATAVERSION/$SBscen/sho_$SBscen.csv  ../../land/loads/sho_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/loads/stb_$SBscen.csv ) rm ../../land/loads/stb_$SBscen.csv
    cp -pv $DATAVERSION/$SBscen/stb_$SBscen.csv  ../../land/loads/stb_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/cover/cover_$SBscen.csv ) rm ../../land/cover/cover_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/cover_$SBscen.csv  ../../land/cover/cover_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/dets/dets_$SBscen.csv ) rm ../../land/dets/dets_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/dets_$SBscen.csv        ../../land/dets/dets_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/fertilizer/fertilizer_$SBscen.csv ) rm ../../land/fertilizer/fertilizer_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/fertilizer_$SBscen.csv  ../../land/fertilizer/fertilizer_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../river/land_use/land_use_$SBscen.csv ) rm ../../river/land_use/land_use_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/land_use_$SBscen.csv    ../../river/land_use/land_use_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/legume/legume_$SBscen.csv ) rm ../../land/legume/legume_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/legume_$SBscen.csv      ../../land/legume/legume_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/manure/manure_$SBscen.csv ) rm ../../land/manure/manure_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/manure_$SBscen.csv      ../../land/manure/manure_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/wep/wep_$SBscen.csv ) rm ../../land/wep/wep_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/wep_$SBscen.csv      ../../land/wep/wep_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/annual_uptake/max_uptake_$SBscen.csv ) rm ../../land/annual_uptake/max_uptake_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/max_uptake_$SBscen.csv  ../../land/annual_uptake/max_uptake_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../land/monthly_fraction_uptake/uptakecurve_$SBscen.csv ) rm ../../land/monthly_fraction_uptake/uptakecurve_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/uptakecurve_$SBscen.csv ../../land/monthly_fraction_uptake/uptakecurve_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../../unformatted/septic/sep_$SBscen.csv ) rm ../../../unformatted/septic/sep_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/sep_$SBscen.csv      ../../../unformatted/septic/sep_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../../unformatted/rpaload/rpa_$SBscen.csv ) rm ../../../unformatted/rpaload/rpa_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/rpa_$SBscen.csv      ../../../unformatted/rpaload/rpa_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../../unformatted/rib/rib_$SBscen.csv ) rm ../../../unformatted/rib/rib_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/rib_$SBscen.csv      ../../../unformatted/rib/rib_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

mkdir -p ../../../unformatted/point_source/$SBscen

if ( -e ../../../unformatted/point_source/$SBscen/mun_$SBscen.csv ) rm ../../../unformatted/point_source/$SBscen/mun_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/mun_$SBscen.csv      ../../../unformatted/point_source/$SBscen/mun_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../../unformatted/point_source/$SBscen/ind_$SBscen.csv ) rm ../../../unformatted/point_source/$SBscen/ind_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/ind_$SBscen.csv      ../../../unformatted/point_source/$SBscen/ind_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

if ( -e ../../../unformatted/point_source/$SBscen/cso_passthru_$SBscen.csv ) rm ../../../unformatted/point_source/$SBscen/cso_passthru_$SBscen.csv
cp -pv $DATAVERSION/$SBscen/cso_passthru_$SBscen.csv      ../../../unformatted/point_source/$SBscen/cso_passthru_$SBscen.csv
set failed = $status; if( $failed == 1 ) exit 1

exit 0 
