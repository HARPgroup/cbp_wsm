#!/bin/csh

  if (${#argv} != 2) then
    echo 'usage: copy_river_parameters.csh from_param_scenario to_param_scenario basin'
    exit
  endif

#### process input
  set FromScen = $argv[1]
  set ToScen = $argv[2]

#### define lists
  set files = (ADCALC.csv HYDR.csv OXRX.csv RQUAL.csv HTRCH.csv NUTRX.csv PLANK.csv SEDTRN.csv tau_percentiles.csv gen_info_rseg.csv)
#  set files = (OXRX.csv RQUAL.csv NUTRX.csv PLANK.csv SEDTRN.csv gen_info_rseg.csv)

  cd $CBP_ROOT/input/param/river/$ToScen/

###### loop over files, remove old segs, grep from new file, add end
  foreach file ($files)

    echo $file

    cp ../$FromScen/$file ./$file
    unix2dos -q $file

  end

##### copy ftables
  echo 'copying ftables'
  cp ../$FromScen/ftables/*.ftable ftables/
  cp ../$FromScen/variable_ftables/*.varftable variable_ftables/

