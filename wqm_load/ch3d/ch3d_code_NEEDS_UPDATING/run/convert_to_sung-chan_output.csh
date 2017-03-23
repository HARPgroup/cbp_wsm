#!/bin/csh

  if (${#argv} != 1 ) then
    echo 'usage: convert_to_sung-chan_output.csh RIVER_SCENARIO'
    exit
  endif

  set rscen = $argv[1]

  if (!(-e ../out/$rscen/)) then
    echo 'must have output previously generated'
    echo 'run run_p5_fall_line_with_temp.csh'
    echo '  and run_p5_and_ps_to_ch3d_with_temp.csh ' 
  endif

  echo 'running'

  cd ../out/$rscen/

  ../../bin/gen_13_33_14_34.exe

  if (-e problem) then
    type problem
    rm problem
    exit
  endif


