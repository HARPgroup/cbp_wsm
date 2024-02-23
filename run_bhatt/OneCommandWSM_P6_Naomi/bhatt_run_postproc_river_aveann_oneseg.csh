#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 6) then
    if (${#argv} != 5) then
      echo ' '
      echo 'usage:  run_postproc_river_aveann.csh scenario segment year1 year2 '
      echo ' or     run_postproc_river_aveann.csh scenario segment year1 year2 tree '
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set seg = $argv[2]
  set AVEYEAR1 = $argv[3]
  set AVEYEAR2 = $argv[4]
  if (${#argv} == 6) then
    set tree = $argv[6]
  else
    source ../fragments/set_tree
    #set tempdir = temp$$`../fragments/random.ksh`
    set tempdir = $argv[5]
    #mkdir -p ../../tmp/scratch/$tempdir/
    mkdir -p ../../tmp/${user}-scratch/$tempdir/
    #cd ../../tmp/scratch/$tempdir/
    cd ../../tmp/${user}-scratch/$tempdir/
  endif


# LOGICAL VARIABLES TO GENERATE DIFFERENT TYPES OF OUTPUT.  0 = NO OUTPUT, 1 = OUTPUT
  set TSSRating =  0
  set Rallmonth =  0
  set Rannual =    0
  set Raveann =    1
  set Rdaily =     0
  set RdailyPart = 0

# EOF loads do not have bmps, delivery factors, or land use acres applied
# EOS loads have bmps, delivery factors, and land use acres
#   the land program calculates loads internally
#   EOFdaily, EOFannual, etc  are flags for output
  set land =         0
    set EOFdaily =   0
    set EOFmonthly = 0
    set EOFannual =  1
    set EOFaveann =  1
    set EOSdaily =   0
    set EOSmonthly = 0
    set EOSannual =  1
    set EOSaveann =  1

# Tabulates data output: point sources, septic, atdep
  set DAT =          0
    set DATdaily =   0
    set DATmonthly = 0
    set DATannual =  1
    set DATaveann =  0

#  calculates transport factors
  set TFS =          0
    set TFSmonthly = 0
    set TFSannual =  0
    set TFSaveann =  0

#  calculates delivery factors
  set DFS =          0
    set DFSmonthly = 0
    set DFSannual =  0
    set DFSaveann =  0

# calculates delivered load
  set DEL =          0
    set DELmonthly = 0
    set DELannual =  0
    set DELaveann =  0

#  SET the number of peaks to investigate, zero if no investigation
  set npeaks = 50

#  SET the window for number of hours to search simulated data looking for match
#  Searches forward and backward in time by 2*window+1 total hours
#  set window to 0 to do the LOD analysis without a time window
#  if window is negative, it will not be calculated
  set window = 24

#  SET loudness 0= reduced output for some programs, 1=regular output
  set loud = 1

# set etm=1 for river output
  set ETM = 0

# set for all land uses since running etm
  set lu = all

####### RIVER SEGMENTS or WQ RECEIVING AREAS ONLY, EOS OUTPUTS DEFINED BY THE BODY THEY FLOW INTO  ########
  if (-e problem) then
    rm problem
  endif


####### DO ANY EDGE-OF-FIELD CALCULATIONS 

    if ($land) then
      echo $scenario $seg $loud $ETM $EOFdaily $EOFmonthly $EOFannual $EOFaveann $EOSdaily $EOSmonthly $EOSannual $EOSaveann $AVEYEAR1 $AVEYEAR2 $lu | $tree/code/bin/etm_and_postproc.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif


####### FIND DATA INPUTS (POINT SOURCE, AT DEP, SEPTIC)
    if ($DAT) then
      echo $scenario $seg $DATdaily $DATmonthly $DATannual $DATaveann $AVEYEAR1 $AVEYEAR2 | $tree/code/bin/DAT.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

  
    if ($TSSRating) then
      echo $scenario $seg, $AVEYEAR1, $AVEYEAR2 | $tree/code/bin/TSSRating.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif


    if ($Rallmonth) then
      echo $scenario $seg | $tree/code/bin/Rallmonth.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

    if ($Rannual) then
      echo $scenario, $seg | $tree/code/bin/Rannual.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

    if ($Rdaily) then
      echo $scenario, $seg | $tree/code/bin/Rdaily.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

    if ($RdailyPart) then
      echo $scenario, $seg | $tree/code/bin/part_daily_simulated.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

    if ($Raveann) then
      echo $scenario, $seg, $AVEYEAR1, $AVEYEAR2 | $tree/code/bin/Raveann.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif
  

####### DO DELIVERED CALCULATIONS LAST
  

    if ($TFS) then
      echo $scenario, $seg, $TFSmonthly, $TFSannual, $TFSaveann, $AVEYEAR1, $AVEYEAR2 | $tree/code/bin/TFS.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif


    if ($DFS) then
      echo $scenario, $seg, $DFSmonthly, $DFSannual, $DFSaveann, $AVEYEAR1, $AVEYEAR2 | $tree/code/bin/DFS.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

    if ($DEL) then
      echo $scenario, $seg, $DELmonthly, $DELannual, $DELaveann, $AVEYEAR1, $AVEYEAR2 | $tree/code/bin/DEL.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif


  if (${#argv} == 5) then
    cd ../
    rm -r $tempdir
  endif


      
