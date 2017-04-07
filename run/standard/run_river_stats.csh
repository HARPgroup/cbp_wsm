#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 4) then
    if (${#argv} != 5) then
      echo ' '
      echo 'usage:  run_river_stats.csh scenario calscen module basin'
      echo ' or     run_river_stats.csh scenario calscen module basin tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set calscen = $argv[2]
  set module = $argv[3]
  set basin = $argv[4]

  if (${#argv} == 5) then
    set tree = $argv[5]
  else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

# SET SCENARIO FOR OBSERVED DATA
  source $tree/run/control/calib/$module/$calscen/set_obscen

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


####### DO RIVER CALCULATIONS NEXT
  
  foreach seg ($segments)

    if ($TSSRating) then
      echo $scenario $seg, $AVEYEAR1, $AVEYEAR2 | $tree/code/bin/TSSRating.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif


    if ($Rstats) then
      echo $scenario,$obscen,$seg,$AVEYEAR1,$AVEYEAR2,$npeaks,$window | $tree/code/bin/Rstats.exe
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

  if (${#argv} == 4) then
    cd ../
    rm -r temp$$/
  endif

      
