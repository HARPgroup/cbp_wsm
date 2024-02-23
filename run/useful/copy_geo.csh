#!/bin/csh

if (${#argv} != 2) then
  echo 'usage: copy_river_parameters.csh from_param_scenario to_param_scenario basin'
  exit
endif

#### process input
  set FromScen = $argv[1]
  set ToScen = $argv[2]

set dir=$CBP_ROOT/config/catalog/geo/$ToScen
if ( !(-d ${dir}) ) then
  mkdir $dir
endif

cp $CBP_ROOT/config/catalog/geo/$FromScen/* $CBP_ROOT/config/catalog/geo/$ToScen/ -Rf
