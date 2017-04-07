#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script creates scenario point source wdms '
    echo '  It may replace wdms already in use. '
    echo '  Check the script to make sure that the variables are correctly'
    echo '  set before continuing'
    echo ' '
    echo ' To make this script run, type: create_scenario_ps_wdms.csh GO'
    echo ' '
    exit
  endif
  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' running this script creates scenario point source wdms '
      echo '  It may replace wdms already in use. '
      echo '  Check the script to make sure that the variables are correctly'
      echo '  set before continuing'
      echo ' '
      echo ' To make this script run, type: create_scenario_ps_wdms.csh GO'
      echo ' '
      exit
    endif
  endif

  source ../../fragments/set_tree
  mkdir -p ../../../tmp/scratch/temp$$/
  cd ../../../tmp/scratch/temp$$/

############ USER DEFINED VARIABLES, SET THESE BELOW APPROPIRATELY
  set code = $tree/code/bin/create_calib_ps_wdms.exe
  set psscen = p532cal_052611
  set dataversion = p532_calib
  set WWTPfnam =   PS_CAL_WWTP_MUNICIPAL_2011-4-21.txt
  set INDfnam =   PS_CAL_WWTP_INDUSTRIAL_2011-4-21.txt
  set CSOfnam = PS_CAL_CSO_2011-5-26.txt
############ END OF USER VARIABLES

######## make directories
  mkdir -p $tree/input/scenario/river/ps/$psscen/
  mkdir -p $tree/input/scenario/river/ps/$psscen/bay_models/

####### run the point source code
  if (-e problem) then
    rm problem
  endif

  echo $psscen $dataversion $WWTPfnam $INDfnam $CSOfnam | $code

  if (-e problem) then
    cat problem
    exit
  endif

######### self-documentation
  set notefile =  $tree/input/scenario/river/ps/$psscen/AutoNotes
  if (-e $notefile) then
    rm $notefile
  endif
  echo 'This dataset,' ${psscen} > $notefile
  echo ' was created by' $user >> $notefile
  echo ' ' >> $notefile
  echo ' on' >> $notefile
  date >> $notefile
  echo ' ' >> $notefile
  echo 'Using the code' >> $notefile
  echo $code >> $notefile
  echo ' ' >> $notefile
  echo 'and the data in' >> $notefile
  echo '../../../unformatted/point_source/'${dataversion}/${WWTPfnam} >> $notefile
  echo ' ' >> $notefile
  echo '../../../unformatted/point_source/'${dataversion}/${INDfnam} >> $notefile  
  echo ' and ' >> $notefile
  echo '../../../unformatted/point_source/'${dataversion}/${CSOfnam} >> $notefile  

########## get rid of temp directory
  cd ../
  rm -r temp$$

