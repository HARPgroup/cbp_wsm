#!/bin/csh

set SCENARIO = $argv[1]

rm -rv ../input/calib/target/$SCENARIO
rm -iv ../input/scenario/land/loads/stb_${SCENARIO}.csv
rm -iv ../input/scenario/river/bmps/bmp_passthru_${SCENARIO}.csv
rm -iv ../input/scenario/river/bmps/bmp_loadredux_EOS_${SCENARIO}.csv
