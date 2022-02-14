#!/bin/csh

source ../../../set_wdmlibdir
set wdmlibdir = ../../$wdmlibdir
set lib1 = $wdmlibdir/wdmlib.a
set lib2 = $wdmlibdir/adwdmlib.a
set lib3 = $wdmlibdir/utillib.a
set usrdir = ../../lib/
set usrlib = ($usrdir/get_lib.a $usrdir/util_lib.a)
set dsnlib = ../../lib/dsn/dsn_utils.o

f77 -fbounds-check -c adjust_rainfall_top_ndays.f

f77 -o ../../../bin/adjust_rainfall_top_ndays.exe adjust_rainfall_top_ndays.o $dsnlib $usrlib $lib1 $lib2 $lib3

rm adjust_rainfall_top_ndays.o
