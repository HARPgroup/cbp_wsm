#!/bin/csh

  set SBscen = $argv[1]

  cd ../../input/scenario/from_SB/

  cp -v $SBscen/bmpacres_$SBscen.csv    ../river/bmpacres/bmpacres_$SBscen.csv
  cp -v $SBscen/bmppounds_$SBscen.csv   ../river/bmppounds/bmppounds_$SBscen.csv
  cp -v $SBscen/afocafoload_$SBscen.csv ../river/loads/afocfoload_$SBscen.csv
  cp -v $SBscen/crop_cover_$SBscen.csv  ../land/crop_cover/crop_cover_$SBscen.csv
  cp -v $SBscen/dets_$SBscen.csv        ../land/dets/dets_$SBscen.csv
  cp -v $SBscen/fertilizer_$SBscen.csv  ../land/fertilizer/fertilizer_$SBscen.csv
  cp -v $SBscen/land_use_$SBscen.csv    ../river/land_use/land_use_$SBscen.csv
  cp -v $SBscen/legume_$SBscen.csv      ../land/legume/legume_$SBscen.csv
  cp -v $SBscen/manure_$SBscen.csv      ../land/manure/manure_$SBscen.csv
  cp -v $SBscen/max_uptake_$SBscen.csv  ../land/annual_uptake/max_uptake_$SBscen.csv
  cp -v $SBscen/uptakecurve_$SBscen.csv ../land/monthly_fraction_uptake/uptakecurve_$SBscen.csv
  cp -v $SBscen/septic_$SBscen.csv      ../../unformatted/septic/septic_$SBscen.csv
  
