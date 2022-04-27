#!/bin/csh
#   GET SCENARIO, BASIN, and TREE
###########  The etm and land postprocessors do primarily the same thing, so 
########## half the time can be saved by combining them for scenarios and 
##########  river calibration processing
########### the date range in the etm is the date range specified in the river control file
###############  the input years are only used for average annual output

  if (${#argv} != 6) then
    if (${#argv} != 5) then
      echo ' '
      echo 'usage:  run_etm_and_land_and_dat_simultaneously_oneseg.csh scenario rseg aveyear1 aveyear2'
      echo ' or     run_etm_and_land_and_dat_simultaneously_oneseg.csh scenario rseg aveyear1 aveyear2 tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set seg      = $argv[2]
  set AVEYEAR1 = $argv[3]
  set AVEYEAR2 = $argv[4]

  set random = `ksh random.ksh`

  if (${#argv} == 6) then
    set tree = $argv[6]
  else
    source ../fragments/set_tree
    set tempdir = $argv[5]
    mkdir -p ../../tmp/${user}-scratch/$tempdir/
    cd ../../tmp/${user}-scratch/$tempdir/
  endif

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
#  source $tree/config/seglists/${basin}.riv

  if (-e problem) then
    rm problem
  endif



#  foreach seg ($segments)

######## MAKE BINARY TRANSFER COEFFICIENT FILE
#??
#      echo $scenario, $seg  `${tree}/run/useful/random.ksh` | $tree/code/bin/make_binary_transfer_coeffs.exe
#      echo $scenario, $seg  `${tree}/run_bhatt/useful/random.ksh` | $tree/code/bin/bhatt_make_binary_transfer_coeffs.exe
#      echo $scenario, $seg  `${tree}run_bhatt/useful/random.ksh` | $tree/code/bin/make_binary_transfer_coeffs.exe
#       echo $scenario, $seg  $random | $tree/code/bin/make_binary_transfer_coeffs.exe
#      echo $scenario, $seg  `${tree}/run_bhatt/useful/random.ksh` | $tree/code/bin/bmp_pass_through.exe
#??      echo $scenario, $seg  0 | $tree/code/bin/bmp_pass_through.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
   
####### FIND EOF, EOS, AND EOSWDM
     cp -v $tree/config/blank_wdm/river.wdm $seg'.wdm'

     echo $scenario $seg $loud $ETM $EOFdaily $EOFmonthly $EOFannual $EOFaveann $EOSdaily $EOSmonthly $EOSannual $EOSaveann $AVEYEAR1 $AVEYEAR2 $lu | $tree/code/bin/etm_and_postproc_P6_StrLoad_NO23.exe
     mv $seg'.wdm' $tree/tmp/wdm/river/$scenario/eos/
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    
####### FIND DATA INPUTS (POINT SOURCE, AT DEP, SEPTIC)
      echo $scenario $seg $DATdaily $DATmonthly $DATannual $DATaveann $AVEYEAR1 $AVEYEAR2 | $tree/code/bin/DAT_P6.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

#  end

  if (${#argv} == 5) then
    cd ../
    rm -r $tempdir
  endif
