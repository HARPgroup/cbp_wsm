#!/bin/csh

  set machname = $argv[1]
  set humanname = $argv[2]

  mv $machname.riv $humanname.riv
  mv $machname.land $humanname.land
  if (-e $machname.calib) then
    mv $machname.calib $humanname.calib
  endif

