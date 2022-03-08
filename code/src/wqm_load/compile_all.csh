#!/bin/csh

set SRCDIR = `pwd`

if( ! (-e /usr/bin/f77 || -e /bin/f77 || -e /usr/local/bin/f77) ) then 
  echo "f77 command is not found"
  echo "Please make sure you have the fortran 77 compiler installed"
  exit 1
endif 

echo "********************* Compiling HSPF lib3.2"
cd hspf/lib3.2/src/util
make clean
make
make install
cd ../
cd wdm/
make clean
make
make install
cd ../
cd adwdm/
make clean
make
make install

cd $SRCDIR/hspf/lib3.2/src/hspnodss
make clean
make

echo "********************* Compiling ICPRBhspf"
cd $SRCDIR/hspf/lib3.2/src/ICPRBhspf
make clean
make

echo "********************* Compiling ghspf"
cd $SRCDIR/hspf/lib3.2/src/ghspf
make clean
make

echo "Compiling hspf/hspf11.1/src - ICPRBmakefile"
cd $SRCDIR/hspf/hspf11.1/src
make clean
make -f ICPRBmakefile
echo "... done."

echo "Compiling hspf/hspf11.1/src - quietmake"
make clean
make -f quietmake
echo "... done."

cd ../../../ 
echo "... Compiling HSPF lib3.2 - done."

echo "... Compiling dsn library"
cd lib/dsn/
f77 -fbounds-check -c dsn_utils.f
echo "... Compiling dsn library - done."

echo "... Compiling util library"
cd ../util
if (-e ../util_lib.a) then
  rm ../util_lib.a
endif
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling util - done."

echo "... Compiling get library"
cd ../get
if (-e ../get_lib.a) then
  rm ../get_lib.a
endif
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling get library - done."

echo "... Compiling tty library"
cd ../tty/
gcc -c -o ../ttyux.o ttyux.c
echo "... Compiling tty library - done."

echo "... Compiling lug"
cd ../../lug
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling lug - done."

echo "... Compiling lug INFEXP"
cd ../lugINFEXP
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling lug INFEXP - done."

echo "... Compiling lug INFEXP quiet"
cd ../lugINFEXPquiet
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling lug INFEXP quiet - done."

cd $SRCDIR/lugx
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling lugx - done."

cd $SRCDIR/unec
echo Y | ./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling unec - done."

cd $SRCDIR/annload
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling annload - done."

cd $SRCDIR/avgload
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling avgload - done."

cd $SRCDIR/scrorgx
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling scrorgx - done."

cd $SRCDIR/reservoir_type_II
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling reservoir_type_II - done."

echo "... Compiling rug"
cd ../rug
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling rug - done."

echo "... Compiling eotbmp-nonpour"
cd ../eotbmp
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling eotbmp-nonpour - done."

echo "... Compiling bmp-pass-through"
cd ../etm/bmps_pass_through/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling bmp-pass-through - done."

echo "... Compiling etm-postproc"
cd ../etm_and_postproc_P6_StrLoad_BBFL_LOAD_NOX_YR/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling etm-postproc - done."

echo "... Compiling stream-wdm"
cd ../stream_wdm/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling stream-wdm - done."

echo "... Compiling check-river"
cd ../check_river/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling check-river - done."

echo "... Compiling combine-direct-loads"
cd ../combine_ps_sep_rib_rpa_div_ams_from_landsegs/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling combine-direct-loads - done."

echo "... Compiling basingen"
cd ../../calibration_utils/basingen/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling basingen - done."

echo "... Compiling riverinfo"
cd ../riverinfo/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling riverinfo - done."

echo "... Compiling split-seglists"
cd ../split_seglists/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling split-seglists - done."

echo "... Compiling EOF-to-EOS-factors"
cd ../EOF_to_EOS_factors/sumout/
if (-e sumout.a) then
  rm sumout.a
endif
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
cd ../
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling EOF-to-EOS-factors - done."

echo "... Compiling reverse-seglist"
cd ../reverse_seglist/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling reverse-seglist - done."

echo "... Compiling read-dot-out"
cd ../read_dot_out_file/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling read-dot-out - done."

echo "... Compiling read-dot-out-scour"
cd ../read_dot_out_file_scour/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling read-dot-out-scour - done."

echo "... Compiling make-calib-site-list"
cd ../make_calib_site_list/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling make-calib-site-list - done."

echo "... Compiling make-calib-seglist"
cd ../make_calib_seglist/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling make-calib-seglist - done."

echo "... Compiling make-incremental-calib-list"
cd ../make_incremental_calib_list/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling make-incremental-calib-list - done."

echo "... Compiling make-WQ-incremental-calib-list"
cd ../make_WQ_incremental_calib_list/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling make-WQ-incremental-calib-list - done."

echo "... Compiling change-param-land"
cd ../change_param/land/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling change-param-land - done."

echo "... Compiling change-param-river"
cd ../river/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling change-param-river - done."

cd ../calib_iter/IQUAL
#cd ../IQUAL
#./compile
#cd ../NITR_crop
#./compile
#cd ../NITR_forest
#./compile
#cd ../NITR_noncrop
#./compile
#cd ../NITR_pasture
#./compile
#cd ../NITR_alf
#./compile
#cd ../NITR_hyw
#./compile
#cd ../PHOS_crop
#./compile
#cd ../PHOS_noncrop
#./compile
#cd ../PQUAL
#./compile
#echo "... Compiling calib-iter pstemp"
#cd ../PSTEMP
#./compile
#echo "... Compiling calib-iter pstemp - done."

echo "... Compiling calib-iter pstemp river"
cd ../PSTEMP_river
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling calib-iter pstemp river - done."

echo "... Compiling calib-iter pwater"
cd ../PWATER
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling calib-iter pwater - done."

echo "... Compiling calib-iter solids"
cd ../SOLIDS
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling calib-iter solids - done."

echo "... Compiling calib-iter sedmnt"
cd ../SEDMNT
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling calib-iter sedmnt - done."

echo "... Compiling calib-iter WQ"
cd ../WQ_sensitivity
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
cd ../../../get1parameter/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
cd ../get_total_watershed_size_for_part/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
cd ../get_pltgen_percentiles/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling calib-iter WQ - done."

echo "... Compiling postproc-land"
cd ../../postproc/land/aveann/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif

echo "... Compiling postproc-river"
cd ../../river/
if (-e postriverlib.a) then
  rm postriverlib.a
endif
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif

cd part
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling river part - done."

cd ../aveann
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling river aveann - done."

cd ../monthly
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling river monthly - done."

cd ../annual
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling river annual - done."

cd ../daily
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling river daily - done."

cd ../stats
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling river stats - done."

cd ../rating
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling river rating - done."

cd ../../data_P6
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling data - done."

cd ../postutils/sumstats
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling sumstats - done."

cd progress_sumstats
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling progress-sumstats - done."

cd ../../pltgen2cal/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling pltgen2cal - done."

cd ../sumplt/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling sumplt - done."

cd ../sumout_P6/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling sumout - done."

cd ../sumin/
cd getAtdep
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling get-atdep - done."

cd ../
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling sumin - done."

cd ../sumWTMP/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling sumwtmp - done."

cd sum_PSTEMP_progress/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling sum-pstemp-progress - done."

cd ../../../pltgen/land
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling pltgen-land - done."

cd pltgen_PIQUAL/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling pltgen-piqual - done."

cd ../../river
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling postriverlib - done."

cd ../../del_P6/tfs
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling tfs - done."

cd ../dfs
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling dfs - done."

cd ../delload
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling del-load - done."

cd ../../scenario_compare/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling scenario-compare - done."

cd ../../wqm_load/wqmlib/
./compile.exe
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
cd ../wqmlib_P6/
./compile.exe
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling wqmlib - done."

cd ../atdep_to_wqm57k/
./compile_mem
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling wqm-atdep - done."

#cd ../check_loads
#./compile
#echo "... Compiling check-loads - done."

#cd ../factor_scenario_p5_and_ps_to_wqm57k
#./compile

#cd ../p5_and_ps_to_wqm57k
#./compile
cd ../p5_and_ps_to_request_y1y2_P6_RPA_RIB_EOT_PSMETHOD
#./compile.exe
./compile_all.exe
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling wqm-nps-ps-sho - done."

cd ../wqm_factor_scenario
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling wqm-factor-scenario - done."

cd ../river_factor_scenario
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling river-factor-scenario - done."

cd ../combine_2_wqm_scenarios
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling combine-2-wqm - done."

cd ../../data_import/diversions/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling diversion - done."

cd ../precip/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling precip - done."

cd ../point_source/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling point-source - done."

cd ../septic/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling septic - done."

cd ../rib/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling rib - done."

cd ../rpaloads/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling rpa - done."

cd ../add_atdep_to_precip/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
./compile_main_GrimmAtDep
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
./compile_detrend
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling atdep - done."

cd ../met/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling met - done."

cd ../quick_wdm/
./compile
if ( $status == 1 ) then
   echo "PROBLEM in `pwd`"; exit;
endif
echo "... Compiling quick-wdm - done."

cd ../../
  
