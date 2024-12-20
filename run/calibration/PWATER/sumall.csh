#!/bin/csh
#   GET SCENARIO and TREE

  if (${#argv} != 2) then
    if (${#argv} != 1) then
      echo ' ' >problem
      echo 'usage:  sumall.csh scenario ' 
      echo ' or     sumall.csh scenario tree' 
      echo ' '
      cat problem
      exit
    endif
  endif

  set scenario = $argv[1]
  if (${#argv} == 2) then
    set tree = $argv[2]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif

  source $tree/run/fragments/set_landuse

 set year1 = 1985
 set year2 = 2005

 grep 'bias   n      ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.FLOW > $tree/output/river/summary/$scenario/flow_bias.csv
 grep 'efficiency    ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.FLOW > $tree/output/river/summary/$scenario/flow_effy.csv
 grep 'efficiency log' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.FLOW > $tree/output/river/summary/$scenario/flow_Leff.csv
 grep 'ave reces stat' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.FLOW > $tree/output/river/summary/$scenario/flow_avri.csv
 grep 'efficiency mon' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.FLOW > $tree/output/river/summary/$scenario/flow_Meff.csv
 grep 'winter bias   ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.FLOW > $tree/output/river/summary/$scenario/flow_Wntr.csv
 grep 'summer bias   ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.FLOW > $tree/output/river/summary/$scenario/flow_Sumr.csv
 grep 'Wint ave reces' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.FLOW > $tree/output/river/summary/$scenario/flow_Wari.csv
 grep 'Sumr ave reces' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.FLOW > $tree/output/river/summary/$scenario/flow_Sari.csv
 grep 'low10 bias    ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.FLOW > $tree/output/river/summary/$scenario/flow_lo10.csv
 grep 'low05 bias    ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.FLOW > $tree/output/river/summary/$scenario/flow_lo05.csv

 grep 'bias   n      ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.QFLW > $tree/output/river/summary/$scenario/qflw_bias.csv
 grep 'efficiency    ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.QFLW > $tree/output/river/summary/$scenario/qflw_effy.csv
 grep 'efficiency log' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.QFLW > $tree/output/river/summary/$scenario/qflw_Leff.csv
 grep 'ave reces stat' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.QFLW > $tree/output/river/summary/$scenario/qflw_avri.csv
 grep 'efficiency mon' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.QFLW > $tree/output/river/summary/$scenario/qflw_Meff.csv
 grep 'winter bias   ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.QFLW > $tree/output/river/summary/$scenario/qflw_Wntr.csv
 grep 'summer bias   ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.QFLW > $tree/output/river/summary/$scenario/qflw_Sumr.csv
 grep 'Wint ave reces' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.QFLW > $tree/output/river/summary/$scenario/qflw_Wari.csv
 grep 'Sumr ave reces' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.QFLW > $tree/output/river/summary/$scenario/qflw_Sari.csv
 grep 'low10 bias    ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.QFLW > $tree/output/river/summary/$scenario/qflw_lo10.csv
 grep 'low05 bias    ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.QFLW > $tree/output/river/summary/$scenario/qflw_lo05.csv

 grep 'bias   n      ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.BFLW > $tree/output/river/summary/$scenario/bflw_bias.csv
 grep 'efficiency    ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.BFLW > $tree/output/river/summary/$scenario/bflw_effy.csv
 grep 'efficiency log' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.BFLW > $tree/output/river/summary/$scenario/bflw_Leff.csv
 grep 'ave reces stat' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.BFLW > $tree/output/river/summary/$scenario/bflw_avri.csv
# grep 'med reces stat' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.BFLW > $tree/output/river/summary/$scenario/bflw_avri.csv
 grep 'efficiency mon' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.BFLW > $tree/output/river/summary/$scenario/bflw_Meff.csv
 grep 'winter bias   ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.BFLW > $tree/output/river/summary/$scenario/bflw_Wntr.csv
 grep 'summer bias   ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.BFLW > $tree/output/river/summary/$scenario/bflw_Sumr.csv
 grep 'Wint ave reces' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.BFLW > $tree/output/river/summary/$scenario/bflw_Wari.csv
 grep 'Sumr ave reces' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.BFLW > $tree/output/river/summary/$scenario/bflw_Sari.csv
 grep 'low10 bias    ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.BFLW > $tree/output/river/summary/$scenario/bflw_lo10.csv
 grep 'low05 bias    ' $tree/output/river/stats/${scenario}/*_${year1}_${year2}.BFLW > $tree/output/river/summary/$scenario/bflw_lo05.csv

 grep 'peak bias     ' $tree/output/river/peaks/${scenario}/*_${year1}_${year2}.peaks > $tree/output/river/summary/$scenario/peaks_pbias.csv
 grep 'vol peak bias ' $tree/output/river/peaks/${scenario}/*_${year1}_${year2}.peaks > $tree/output/river/summary/$scenario/peaks_vbias.csv

### if (-e  $tree/output/river/summary/${scenario}_SURO_inches.csv) then
###   rm  $tree/output/river/summary/${scenario}_SURO_inches.csv
### endif

### foreach lu ($perlnds)
###   grep 'suro'           $tree/output/pltgen/land/${scenario}/${lu}*RO_inches                >> $tree/output/river/summary/${scenario}_SURO_inches.csv
### end

### grep 'ifwo'           $tree/output/pltgen/land/${scenario}/*RO_inches                > $tree/output/river/summary/${scenario}_IFWO_inches.csv
### grep 'agwo'           $tree/output/pltgen/land/${scenario}/*RO_inches                > $tree/output/river/summary/${scenario}_AGWO_inches.csv

 echo ${scenario} | $tree/code/bin/summarystats.exe
 
 if (-e problem) then
   cat problem
   exit
 endif

  if (${#argv} == 1) then
    cd ../
    rm -r temp$$
  endif

