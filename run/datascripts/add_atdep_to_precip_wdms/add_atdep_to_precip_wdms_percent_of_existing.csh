#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script adds atmospheric deposition to the precipitation wdms.'
    echo '  it will not modify the precipitation and will replace any previously loaded'
    echo '  atmospheric deposition'
    echo ' '
    echo ' To make this script run, type: add_atdep_to_precip_wdms_year_scenario.csh GO'
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
      echo ' To make this script run, type: add_atdep_to_precip_wdms_year_scenario.csh GO'
      echo ' '
      exit
    endif
  endif

  source ../../fragments/abs_tree

######## SET THE FOLLOWING VARIABLES CORRECTLY AND DOUBLE CHECK
  set code = $tree/code/bin/add_atdep_percent_of_existing
  set fromscen = p532_20cair
  set newscen = p532_5AA
  set basin = 'allBay'
  set year1 = 1984
  set year2 = 2005
  set wetno3 = 0.95
  set dryno3 = 0.95
  set wetnh3 = 1.0
  set drynh3 = 1.0
  set wetpo4 = 1.0
  set wetorn = 1.0
  set wetorp = 1.0
########## END USER-DEFINED VARIABLES

  mkdir -p ../../../tmp/scratch/temp$$/
  cd ../../../tmp/scratch/temp$$/

  source $tree/config/seglists/${basin}.land

  mkdir -p $tree/input/scenario/climate/prad/$newscen/

  if (-e problem) then
    rm problem
  endif

  set summaryfile = $tree/input/scenario/climate/prad/$newscen/allatdep_${newscen}.csv

  if (-e $summaryfile) then
    rm $summaryfile
  endif

  foreach seg ($segments)
    cp $tree/input/scenario/climate/prad/$fromscen/prad_${seg}.wdm .
    echo $seg
    echo $seg $year1 $year2 $wetno3 $dryno3 $wetnh3 $drynh3 $wetpo4 $wetorn $wetorp | $code >> $summaryfile
    mv prad_${seg}.wdm $tree/input/scenario/climate/prad/$newscen/
    if (-e problem) then
      cat problem
      exit
    endif
  end

######### self-documentation
  set notefile =  $tree/input/scenario/climate/prad/$newscen/AutoAtdepNotes
  if (-e $notefile) then
    rm $notefile
  endif
  echo 'This dataset,' ${newscen} > $notefile
  echo ' was created by' $user >> $notefile
  echo ' ' >> $notefile
  echo ' on' >> $notefile
  date >> $notefile
  echo ' ' >> $notefile
  echo 'Using the code' >> $notefile
  echo $code >> $notefile
  echo ' ' >> $notefile
  echo 'The data set in ../'$fromscen >> $notefile
  echo ' was converted by the following factors' >> $notefile
  echo 'wetno3  dryno3  wetnh3  drynh3  wetpo4  wetorn  wetorp' >> $notefile
  echo $wetno3 $dryno3 $wetnh3 $drynh3 $wetpo4 $wetorn $wetorp >> $notefile

  cd ../
  rm -r temp$$


