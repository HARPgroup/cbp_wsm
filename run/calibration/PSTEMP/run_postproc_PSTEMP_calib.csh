#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo ' '
      echo 'usage:  run_postproc.csh scenario calscen basin'
      echo ' or     run_postproc.csh scenario calscen basin tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]

  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif
  
# SET SCENARIO FOR OBSERVED DATA
  source $tree/config/control/calib/PSTEMP/$calscen/set_obscen

#  SET AVERAGING YEARS
  set AVEYEAR1 = 1985
  set AVEYEAR2 = 2005

# LOGICAL VARIABLES TO GENERATE DIFFERENT TYPES OF OUTPUT.  0 = NO OUTPUT, 1 = OUTPUT
  set TSSRating =  0
  set Rstats =     1
  set Rallmonth =  0
  set Rannual =    0
  set Raveann =    0
  set Rdaily =     0
  set RdailyPart = 0

# EOF loads do not have bmps, delivery factors, or land use acres applied
# EOS loads have bmps, delivery factors, and land use acres
#   the land program calculates loads internally
#   EOFdaily, EOFannual, etc  are flags for output
  set land =         0
    set EOFdaily =   0
    set EOFmonthly = 0
    set EOFannual =  0
    set EOFaveann =  0
    set EOSdaily =   0
    set EOSmonthly = 0
    set EOSannual =  0
    set EOSaveann =  0

# Tabulates data output: point sources, septic, atdep
  set DAT =          0
    set DATdaily =   0
    set DATmonthly = 0
    set DATannual =  0
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
  set loud = 0

####### RIVER SEGMENTS or WQ RECEIVING AREAS ONLY, EOS OUTPUTS DEFINED BY THE BODY THEY FLOW INTO  ########
  source $tree/config/seglists/${basin}.riv

  if (-e problem) then
    rm problem
  endif


####### DO ANY EDGE-OF-FIELD CALCULATIONS 

  foreach seg ($segments)

    if ($land) then
      echo $scenario $seg $loud $EOFdaily $EOFmonthly $EOFannual $EOFaveann $EOSdaily $EOSmonthly $EOSannual $EOSaveann $AVEYEAR1 $AVEYEAR2 | $tree/code/bin/land.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

  end


####### FIND DATA INPUTS (POINT SOURCE, AT DEP, SEPTIC)
  foreach seg ($segments)

    if ($DAT) then
      echo $scenario $seg $DATdaily $DATmonthly $DATannual $DATaveann $AVEYEAR1 $AVEYEAR2 | $tree/code/bin/DAT.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

  end


####### DO RIVER CALCULATIONS NEXT
  
  foreach seg ($segments)

    if ($TSSRating) then
      echo $scenario $seg $AVEYEAR1 $AVEYEAR2 | $tree/code/bin/TSSRating.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif


    if ($Rstats) then
      echo $scenario $obscen $seg $AVEYEAR1 $AVEYEAR2 $npeaks $window | $tree/code/bin/Rstats.exe
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
  

  end

####### DO DELIVERED CALCULATIONS LAST
  
  foreach seg ($segments)

    if ($TFS) then
      echo $scenario, $seg, $TFSmonthly, $TFSannual, $TFSaveann, $AVEYEAR1, $AVEYEAR2 | $tree/code/bin/TFS.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

  end

  foreach seg ($segments)

    if ($DFS) then
      echo $scenario, $seg, $DFSmonthly, $DFSannual, $DFSaveann, $AVEYEAR1, $AVEYEAR2 | $tree/code/bin/DFS.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

  end

  foreach seg ($segments)

    if ($DEL) then
      echo $scenario, $seg, $DELmonthly, $DELannual, $DELaveann, $AVEYEAR1, $AVEYEAR2 | $tree/code/bin/DEL.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

  end

  if (${#argv} == 3) then
    cd ../
    rm -r temp$$
  endif


      
