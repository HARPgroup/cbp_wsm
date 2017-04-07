#!/bin/csh
#   GET SCENARIO, BASIN, and TREE
###########  The etm and land postprocessors do primarily the same thing, so 
########## half the time can be saved by combining them for scenarios and 
##########  river calibration processing
########### the date range in the etm is the date range specified in the river control file
###############  the input years are only used for average annual output

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_etm_and_land_and_dat_3_intervals_oneseg.csh scenario segment'
      echo ' or     run_etm_and_land_and_dat_3_intervals_oneseg.csh scenario segment tree'
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

  set int1start = 1985
  set int1end   = 1994
  set int2start = 1991
  set int2end   = 2000
  set int3start = 1994
  set int3end   = 2000

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

# Tabulates data output: point sources, septic, atdep
    set DATdaily =   0
    set DATmonthly = 0
    set DATannual =  0
    set DATaveann =  1

#  SET loudness 0= reduced output for some programs, 1=regular output
  set loud = 0

# set etm=1 for river output
  set ETM = 1

# set for all land uses since running etm
  set lu = all

####### RIVER SEGMENTS or WQ RECEIVING AREAS ONLY, EOS OUTPUTS DEFINED BY THE BODY THEY FLOW INTO  ########
  set segments = ($basin)

  if (-e core) then
    rm core
  endif
  if (-e problem) then
    rm problem
  endif



  foreach seg ($segments)

######## MAKE BINARY TRANSFER COEFFICIENT FILE
    echo $scenario, $seg  `${tree}/run/useful/random.ksh` | $tree/code/bin/make_binary_transfer_coeffs.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
      if (-e core) then
        echo ' '
        echo ' CORE DUMP '
        rm core
        exit
      endif

####### FIND EOF, EOS, AND EOSWDM
    cp -v $tree/config/blank_wdm/river.wdm $seg'.wdm'
    echo $scenario $seg $loud $ETM $EOFdaily $EOFmonthly $EOFannual $EOFaveann $EOSdaily $EOSmonthly $EOSannual $EOSaveann $int1start $int1end $int2start $int2end $int3start $int3end $lu | $tree/code/bin/etm_and_postproc_3_intervals.exe
    mv $seg'.wdm' $tree/tmp/wdm/river/$scenario/eos/
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
      if (-e core) then
        echo ' '
        echo ' CORE DUMP '
        rm core
        exit
      endif

####### FIND DATA INPUTS (POINT SOURCE, AT DEP, SEPTIC)
      echo $scenario $seg $DATdaily $DATmonthly $DATannual $DATaveann $int1start $int1end | $tree/code/bin/DAT.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
      if (-e core) then
        echo ' '
        echo ' CORE DUMP '
        rm core
        exit
      endif
    endif
      echo $scenario $seg $DATdaily $DATmonthly $DATannual $DATaveann $int2start $int2end | $tree/code/bin/DAT.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
      if (-e core) then
        echo ' '
        echo ' CORE DUMP '
        rm core
        exit
      endif
    endif
      echo $scenario $seg $DATdaily $DATmonthly $DATannual $DATaveann $int3start $int3end | $tree/code/bin/DAT.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
      if (-e core) then
        echo ' '
        echo ' CORE DUMP '
        rm core
        exit
      endif
    endif

  end


  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif


      
