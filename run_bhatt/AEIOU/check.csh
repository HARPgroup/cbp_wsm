#!/bin/csh

set NPDES = $argv[1]

grep -il $NPDES ../../input/unformatted/point_source/PS_8514_20170902RF/CAL_P6_WWTP_MUNICIPAL_final_2017-9-2_sravi.txt
grep -il $NPDES ../../input/unformatted/point_source/PS_8514_20170902RF/CAL_P6_WWTP_INDUSTRIAL_final_2017-9-2_sravi.txt
grep -il $NPDES ../../input/unformatted/point_source/PS_8514_20170902RF/CAL_P6_CSO_Beta5_2017-04-23_sravi.csv
