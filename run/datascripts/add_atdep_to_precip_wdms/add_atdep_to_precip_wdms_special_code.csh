#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script adds atmospheric deposition to the precipitation wdms.'
    echo '  it will not modify the precipitation and will replace any previously loaded'
    echo '  atmospheric deposition'
    echo ' '
    echo ' To make this script run, type: add_atdep_to_precip_wdms_special_case.csh GO'
    echo ' '
    exit
  endif
  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' running this script adds atmospheric deposition to the precipitation wdms.'
      echo '  it will not modify the precipitation and will replace any previously loaded'
      echo '  atmospheric deposition'
      echo ' '
      echo ' To make this script run, type: add_atdep_to_precip_wdms_special_case.csh GO'
      echo ' '
      exit
    endif
  endif

  source ../../fragments/abs_tree

######## SET THE FOLLOWING VARIABLES CORRECTLY AND DOUBLE CHECK
  set code = $tree/code/bin/add_atdep_pristine
  set oldpradscen = ns611a807
  set newpradscen = ns611a902_pristine
  set basin = 'all'
  set year1 = 1984
  set year2 = 2005
############ END OF USER VARIABLES

  mkdir -p ../../../tmp/scratch/temp$$/
  cd ../../../tmp/scratch/temp$$/

  source $tree/config/seglists/${basin}.land

  if (-e problem) then
    rm problem
  endif

  mkdir -p $tree/input/scenario/climate/prad/$newpradscen/
  mkdir -p $tree/input/scenario/climate/prad/$newpradscen/txt/

  set summaryfile = $tree/input/scenario/climate/prad/$newpradscen/allatdep_${newpradscen}.csv

  if (-e $summaryfile) then
    rm $summaryfile
  endif

  foreach seg ($segments)
    cp $tree/input/scenario/climate/prad/$oldpradscen/prad_${seg}.wdm .
    echo $seg
    echo $seg $year1 $year2 | $code >> $summaryfile
    mv prad_${seg}.wdm $tree/input/scenario/climate/prad/$newpradscen/
    if (-e problem) then
      cat problem
      exit
    endif
  end

######### self-documentation
  set notefile =  $tree/input/scenario/climate/prad/$newpradscen/AutoAtdepNotes
  if (-e $notefile) then
    rm $notefile
  endif
  echo 'This dataset,' ${newpradscen} > $notefile
  echo ' was created by' $user >> $notefile
  echo ' ' >> $notefile
  echo ' on' >> $notefile
  date >> $notefile
  echo ' ' >> $notefile
  echo ' using the old scenario ' $oldpradscen >> $notefile
  echo ' ' >> $notefile
  echo 'and using the code' >> $notefile
  echo $code >> $notefile

  cd ../
  rm -r temp$$

