#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_etm.csh scenario seg'
      echo ' or     run_etm.csh scenario seg tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set seg = $argv[2]
  if (${#argv} == 3) then
    set tree = $argv[3]
  else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

# EOF loads do not have bmps, delivery factors, or land use acres applied
#   the land program calculates loads internally
#   EOFdaily, EOFannual, etc  are flags for output
    set EOFdaily =   0
    set EOFmonthly = 0
    set EOFannual =  0
    set EOFaveann =  0

# EOS loads have bmps, delivery factors, and land use acres
    set EOSdaily =   0
    set EOSmonthly = 0
    set EOSannual =  0
    set EOSaveann =  0

# Tabulates data output: point sources, septic, atdep
    set DATdaily =   0
    set DATmonthly = 0
    set DATannual =  0
    set DATaveann =  0

# not used
    set AVEYEAR1 = 1985
    set AVEYEAR2 = 2000

#  SET loudness 0= reduced output for some programs, 1=regular output
  set loud = 0

# set etm=1 for river output
  set ETM = 1

# set for all land uses since running etm
  set lu = all

####### RIVER SEGMENTS or WQ RECEIVING AREAS ONLY  ########

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

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif

