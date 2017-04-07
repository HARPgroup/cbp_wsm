#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_etm.csh scenario basin'
      echo ' or     run_etm.csh scenario basin tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  if (${#argv} == 3) then
    set tree = $argv[3]
  else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

  set loud = '1'           # zero for loud, non-zero for quiet

# EOF loads do not have bmps, delivery factors, or land use acres applied
#   the land program calculates loads internally
#   EOFdaily, EOFannual, etc  are flags for output
    set EOFdaily =   0
    set EOFmonthly = 0
    set EOFannual =  0
    set EOFaveann =  1

# EOS loads have bmps, delivery factors, and land use acres
    set EOSdaily =   0
    set EOSmonthly = 0
    set EOSannual =  0
    set EOSaveann =  1

#  need to supply these variables, but they are not used if none of the above flags are activated
    set AVEYEAR1 = 1985
    set AVEYEAR2 = 2005  

#  SET loudness 0= reduced output for some programs, 1=regular output
  set loud = 1

# set etm=1 for river output
  set ETM = 1

# set for all land uses since running etm
  set lu = all

  source $tree/config/seglists/${basin}.riv

####### RIVER SEGMENTS or WQ RECEIVING AREAS ONLY  ########

  foreach seg ($segments)
   
########## make binary transfer files ******
    if (-e problem) then
      rm problem
    endif

    echo $scenario, $seg, `${tree}/run/useful/random.ksh` | $tree/code/bin/make_binary_transfer_coeffs.exe

    if (-e problem) then
      echo ' '
      cat problem
      exit
    endif

    cp -v $tree/config/blank_wdm/river.wdm $seg'.wdm'

    echo $scenario $seg $loud $ETM $EOFdaily $EOFmonthly $EOFannual $EOFaveann $EOSdaily $EOSmonthly $EOSannual $EOSaveann $AVEYEAR1 $AVEYEAR2 $lu | $tree/code/bin/etm_and_postproc.exe

    mv $seg'.wdm' $tree/tmp/wdm/river/$scenario/eos/

    if (-e problem) then
      echo ' '
      cat problem
      exit
    endif

  end

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif

