#!/bin/csh

  set code = '../../bin/split_single_seg_WQ_data.exe'
  set rseg = PL3_5250_0001
  set year1 = 1985
  set year2 = 2005

  echo $rseg $year1 $year2 | $code

