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
  set code = $tree/code/bin/create_scenario_ps_wdms_from_csv.exe
  set psscen =  PS_MS_MD_b_130916
  set dataversion = p532_scenarios
  set WWTPfnam =  PS_MS_MD_baseline_MUNICIPAL_2013-9-20.txt
  set INDfnam =   PS_MS_MD_baseline_INDUSTRIAL_2013-9-20.txt
  set CSOfnam =   PS_MD_MS_2011_CSO_2013-9-16.txt
  set CSOaveyear1 = 1991
  set CSOaveyear2 = 2000
############ END OF USER VARIABLES

######## make directories
  mkdir -p $tree/input/scenario/river/ps/$psscen/
  mkdir -p $tree/input/scenario/river/ps/$psscen/bay_models/

####### run the point source code
  if (-e problem) then
    rm problem
  endif

  echo $psscen $dataversion $WWTPfnam $INDfnam $CSOfnam $CSOaveyear1 $CSOaveyear2 | $code

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
  echo ' ' >> $notefile
  echo '../../../unformatted/point_source/'${dataversion}/${CSOfnam} >> $notefile  
#  echo ' and ' >> $notefile
#  echo '../../../unformatted/point_source/'${dataversion}/${CSOfacfnam} >> $notefile  

########## get rid of temp directory
  cd ../
  rm -r temp$$
