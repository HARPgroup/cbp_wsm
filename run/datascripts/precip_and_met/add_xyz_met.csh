#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script adds the xyz temperature and PET to existing met wdms.  It may replace wdms'
    echo '  already in use. Check the script to make sure that the variables are correctly'
    echo '  set before continuing'
    echo ' '
    echo ' To make this script run, type: create_precip_wdms.csh GO'
    echo ' '
    exit
  endif
  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' running this script adds the xyz temperature and PET to existing met wdms.  It may replace wdms'
      echo '  already in use. Check the script to make sure that the variables are correctly'
      echo '  set before continuing'
      echo ' '
      echo ' To make this script run, type: create_precip_wdms.csh GO'
      echo ' '
      exit
    endif
  endif

  source ../../fragments/set_tree

######## SET THE FOLLOWING VARIABLES CORRECTLY AND DOUBLE CHECK
  set code = $tree/code/bin/add_xyz_PET_and_temp
  set oldmetscen = f8405
  set newmetscen = f8405xyz
  set basin = 'all'
  set datasource = xyz_lauren_hay_model
  set version = xyz_2006_11_29
######### NO MORE SPECIFICATION BELOW THIS LINE

  mkdir -p ../../../tmp/scratch/temp$$/
  cd ../../../tmp/scratch/temp$$/

  source $tree/config/seglists/${basin}.land

  if (-e problem) then
    rm problem
  endif

  mkdir -p $tree/input/scenario/climate/met/$newmetscen/
  mkdir -p $tree/input/scenario/climate/met/$newmetscen/txt/

  set summaryfile = ${tree}'/input/scenario/climate/met/'${newmetscen}'/ann_PET_'${newmetscen}'.txt'

  if (-e $summaryfile) then
    rm $summaryfile
  endif

  foreach seg ($segments)
    cp $tree/input/scenario/climate/met/$oldmetscen/met_${seg}.wdm .
    echo $seg $datasource $version | $code >> $summaryfile
    mv met_${seg}.wdm $tree/input/scenario/climate/met/$newmetscen/
    if (-e problem) then
      cat problem
      exit
    endif
  end

######### self-documentation
  set notefile =  $tree/input/scenario/climate/met/$newmetscen/AutoMetNotes
  if (-e $notefile) then
    rm $notefile
  endif
  echo 'This dataset,' ${newmetscen} > $notefile
  echo ' was created by' $user >> $notefile
  echo ' ' >> $notefile
  echo ' on' >> $notefile
  date >> $notefile
  echo ' ' >> $notefile
  echo ' by starting with the wdms in '$oldmetscen >> $notefile
  echo ' and adding the temperature and potential evapotranspiration data in' >> $notefile
  echo $tree/input/unformatted/precip_some_met/$datasource/$version/ >> $notefile
  echo 'Using the code' >> $notefile
  echo $code >> $notefile

  cd ../
  rm -r temp$$
