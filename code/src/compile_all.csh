#!/bin/csh

####### section added by York Prado 10/2009
if( ! (-e /usr/bin/f77 || -e /bin/f77 || -e /usr/local/bin/f77) ) then 
  echo "f77 command is not found"
  echo "Please make sure you have the fortran 77 compiler installed"
  exit 1
endif 

set basedir = $PWD
set FCP = f77
set CPC = gcc
echo "##########  COMPILE HSPF LIBRARY  #########"
rm $basedir/hspf/lib3.2/lib/*
rm $basedir/hspf/lib3.2/bin/*
rm $basedir/lib/*.a
set libname = (aide util wdm adwdm awstat ghspf_den hspf ICPRBhspf phspf \
ann ghspf graph hspnodss newaqt stats waide wdimex wdmrx)
cd $basedir/hspf/lib3.2/src/util
set i = 1
 while ($i <= 18)
   set lname = $libname[$i]
   echo "Compiling Library $lname (# $i)" 
   cd $basedir/hspf/lib3.2/src/$lname
   make clean
   make FC=$FCP
   make install
   @ i = $i + 1
 end

echo "###############   COMPILE HSPF  ######"
cd $basedir/hspf/hspf11.1/src/
make -f ICPRBmakefile clean
make -f ICPRBmakefile FC=$FCP

echo "#########   COMPILE WSM LIBRARY   #####"
cd $basedir/lib/dsn/
$FCP -fbounds-check -c dsn_utils.f
cd $basedir/lib/tty
$CPC -c -o ../ttyux.o ttyux.c
foreach libname (util get)
  cd $basedir/lib/$libname
  ./compile $FCP
end

echo "###########  COMPILE WSM MODEL   ############"
cd $basedir/lug
./compile $FCP
cd $basedir/rug
./compile $FCP
cd $basedir/etm/etm_and_postproc/
foreach libname (check_river etm_and_postproc stream_wdm combine_ps_sep_div_ams_from_landsegs \
                 make_binary_transfer_coeffs)
   cd $basedir/etm/$libname
   ./compile $FCP
end
echo "#############  DATA_IMPORT  #########"
cd $basedir/data_import/add_atdep_to_precip
foreach libname (add_atdep_to_precip diversions met point_source precip quick_wdm septic)
   cd $basedir/data_import/$libname
   ./compile $FCP
end
cd $basedir/postproc/data
   ./compile $FCP
cd $basedir/postproc/scenario_compare
   ./compile $FCP
foreach libname (delload dfs otherDF_delload tfs)
   cd $basedir/postproc/del/$libname
   ./compile $FCP
end
cd $basedir/postproc/pltgen/land
   ./compile $FCP
cd $basedir/postproc/river
   ./compile $FCP
echo "############  POSTUTILS  #########"
cd $basedir/postproc/postutils/create_p4_wdms
foreach libname (create_p4_wdms pltgen2cal sumdfs sumin sumin_QA sumout sumplt sumstats sumWTMP)
   cd $basedir/postproc/postutils/$libname
   ./compile $FCP
end
cd $basedir/postproc/postutils/sumin/getAtdep
./compile $FCP
cd $basedir/postproc/postutils/sumin_QA/getAtdep
./compile $FCP
echo "############  RIVER  #########"
foreach libname (annual eval_stat part regress stats texture aveann monthly RiverLoads avemon daily obs_only_stats  rating)
   cd $basedir/postproc/river/$libname
   ./compile $FCP
end
cd $basedir/postproc/river
   ./compile $FCP
cd $basedir/spec_landuse
   ./compile $FCP
echo "############  wqm_loads  #########"
foreach libname (atdep_to_wqm57k check_loads combine_2_wqm_scenarios river_factor_scenario \
        TMDL_daily_loads wqm_factor_scenario p5_and_ps_to_wqm57k wqmlib)
   cd $basedir/wqm_load/$libname
   ./compile $FCP
end

echo "################   CALIB_INTER    ###########"
cd calibration_utils/change_param/calib_iter/IQUAL/
foreach libname (IQUAL NITR_forest NITR_pasture PQUAL PWATER \
        NITR_alf NITR_hyw PHOS_crop PSTEMP SEDMNT WQ_sensitivity \
        NITR_crop NITR_noncrop PHOS_noncrop PSTEMP_river SOLIDS )
   cd $basedir/calibration_utils/change_param/calib_iter/$libname
   ./compile $FCP
end
cd $basedir/calibration_utils/change_param/land/
   ./compile $FCP
cd $basedir/calibration_utils/change_param/river
   ./compile $FCP
echo "############  COMPILE CALIBRATION  ############"

# special directive for sumout.a
cd $basedir/calibration_utils/EOF_to_EOS_factors/sumout
   ./compile $FCP
foreach libname (a_priori_irc basingen copy_phase4_segs divide_rseg_by_stream_order \
        EOF_to_EOS_factors get1parameter get_pltgen_percentiles \
        get_total_watershed_size_for_part make_calib_seglist make_calib_site_list \
        make_first_order_calib_list make_incremental_calib_list \
        make_WQ_incremental_calib_list \
        postproc_for_censored_data read_dot_out_file reverse_seglist \
        riverinfo split_seglists )
   cd $basedir/calibration_utils/$libname
   echo $libname
   ./compile $FCP
end
cd ../../
echo "#############   COMPILATION COMPLETED   #########"



