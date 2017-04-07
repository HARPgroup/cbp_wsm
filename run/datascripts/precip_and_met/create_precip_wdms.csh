#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script creates new precipitation wdms.  It may replace wdms'
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
      echo ' running this script creates new precipitation wdms.  It may replace wdms'
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
  set code = $tree/code/bin/create_precip_wdms
  set pradscen = f611
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

  mkdir -p $tree/input/scenario/climate/prad/$pradscen/
  mkdir -p $tree/input/scenario/climate/prad/$pradscen/txt/

  set summaryfile = ${tree}'/input/scenario/climate/prad/'${pradscen}'/ann_precip_'${pradscen}'.txt'

  if (-e $summaryfile) then
    rm $summaryfile
  endif

  foreach seg ($segments)
    cp $tree/config/blank_wdm/blank_prad.wdm prad_${seg}.wdm 
    echo $seg
    echo $seg $datasource $version | $code >> $summaryfile
    mv prad_${seg}.wdm $tree/input/scenario/climate/prad/$pradscen/
    if (-e problem) then
      cat problem
      exit
    endif
  end

######### self-documentation
  set notefile =  $tree/input/scenario/climate/prad/$pradscen/AutoPrecipNotes
  if (-e $notefile) then
    rm $notefile
  endif
  echo 'This dataset,' ${pradscen} > $notefile
  echo ' was created by' $user >> $notefile
  echo ' ' >> $notefile
  echo ' on' >> $notefile
  date >> $notefile
  echo ' ' >> $notefile
  echo ' using the data in' >> $notefile
  echo $tree/input/unformatted/precip_some_met/$datasource/$version/ >> $notefile
  echo 'Using the code' >> $notefile
  echo $code >> $notefile

  cd ../
  rm -r temp$$
