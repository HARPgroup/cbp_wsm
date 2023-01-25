#!/bin/csh

  if (${#argv} != 2) then
    echo 'usage: copy_iovars.csh from_iovars_scenario to_iovars_scenario'
    echo 'Hint: Find iovars setting with "cbp get_config [scenario name] river IOVARS"'
    exit
  endif

#### process input
  set FromScen = $argv[1]
  set ToScen = $argv[2]

#### define lists
set dir=$CBP_ROOT/config/catalog/iovars/$ToScen
if ( !(-d ${dir}) ) then
  mkdir $dir
endif

cp $CBP_ROOT/config/catalog/iovars/$FromScen/* $dir/

