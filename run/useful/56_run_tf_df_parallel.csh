#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

    if (${#argv} != 3) then
      echo ' '
      echo 'usage:  run_tf_df_parallel.csh scenario aveyear1 aveyear2'
      echo ' '
      exit
    endif

  set scenario = $argv[1]
  set year1 = $argv[2]
  set year2 = $argv[3]

  set basins = (S P Y E R J W X VAcapes)

  foreach basin ($basins)

    sbatch -pdebug run_postproc_tf_df_del_aveann.csh $scenario $basin $year1 $year2

  end



      
