#!/bin/csh

  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo 'usage: copy_river_parameters.csh from_param_scenario to_param_scenario basin'
      echo ' or    copy_river_parameters.csh from_param_scenario to_param_scenario basin tree'
      exit
    endif
  endif

#### process input
  set FromScen = $argv[1]
  set ToScen = $argv[2]
  set basin = $argv[3]
  if (${#argv} == 3) then
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  else
    set tree = $argv[4]
  endif

#### define lists
  source $tree/config/seglists/${basin}.riv

#  set files = (ADCALC.csv HYDR.csv OXRX.csv RQUAL.csv HTRCH.csv NUTRX.csv PLANK.csv SEDTRN.csv tau_percentiles.csv)
  set files = (OXRX.csv RQUAL.csv NUTRX.csv PLANK.csv SEDTRN.csv )

  cd $tree/input/param/river/$ToScen/

##### create ed file
  if (-e rmbasin.edp) then
    rm rmbasin.edp
  endif
  foreach seg ($segments)
    echo g/$seg/d >> rmbasin.edp
  end
  echo 'g/^end/d' >> rmbasin.edp
  echo w >>rmbasin.edp
  echo q >>rmbasin.edp

  echo updating $ToScen from $FromScen with segments from $basin

###### loop over files, remove old segs, grep from new file, add end
  foreach file ($files)

    echo $file

####### remove segs and the 'end' line
    ed -s $file < rmbasin.edp

    foreach seg ($segments)
      grep $seg ../$FromScen/$file >> $file
    end

    if ($file != tau_percentiles.csv) then
      echo 'end' >> $file
    endif

    unix2dos -q $file

  end

  rm rmbasin.edp

##### copy ftables
  echo 'copying ftables'
  foreach seg ($segments)
    if (-e ../$FromScen/ftables/$seg.ftable) then
      cp ../$FromScen/ftables/$seg.ftable ftables/
    endif
    if (-e ../$FromScen/variable_ftables/$seg.varftable) then
      cp ../$FromScen/variable_ftables/$seg.varftable variable_ftables/
    endif
    if ($seg == SL9_2720_0001) then
      cp -p ../$FromScen/reservoir_rules/* reservoir_rules/
    endif
  end

