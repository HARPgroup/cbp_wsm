#!/bin/csh

  if (${#argv} != 5) then
      echo ' '
      echo ' usage:  run_quantile_seasonal.csh scenario_data qyear1 qyear2 year1 year2'
      echo ' e.g. run_quantile_seasonal.csh N1505211511 1991 2000 1984 2014'
      echo ' '
      exit
  endif

  set dataset = $argv[1]
  set qyear1 = $argv[2]
  set qyear2 = $argv[3]
  set year1 = $argv[4]
  set year2 = $argv[5]

  set folder = /input/scenario/climate/prad/

#  source ./../../../../../config/seglists/allBay.land
#  source ./../../../../../config/seglists/A10001.land

  mkdir ./../../../../input/scenario/climate/prad/$dataset/txt/qstats_seasonal_${year1}_$year2

  foreach seg ($segments)

    Rscript calculate_quantile.r $dataset $seg $qyear1 $qyear2 $year1 $year2 

  end

  foreach seg ($segments)

    Rscript precip_quantile_stat.r $dataset $seg $qyear1 $qyear2 $year1 $year2 $folder

  end

#  Rscript files.r $dataset $year1 $year2

# foreach seg ($segments)

#    Rscript regression.r $dataset $seg $year1 $year2

# end











