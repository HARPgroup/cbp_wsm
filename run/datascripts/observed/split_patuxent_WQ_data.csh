#!/bin/csh

  set code = '../../bin/split_Patuxent_WQ_data.exe'

  if (-e problem) then
    rm problem
  endif

  $code

  if (-e problem) then
    cat problem
    rm problem
  endif

