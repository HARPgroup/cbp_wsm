#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 5) then
    if (${#argv} != 4) then
      echo ' '
      echo 'usage:  run_postproc.csh scenario basin year1 year2'
      echo ' or     run_postproc.csh scenario basin year1 year2 tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  set year1 = $argv[3]
  set year2 = $argv[4]
  if (${#argv} == 5) then
    set tree = $argv[5]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif



# LOGICAL VARIABLES TO GENERATE DIFFERENT TYPES OF OUTPUT.  0 = NO OUTPUT, 1 = OUTPUT
  set wqm = 0
  set TSSRating =  0
  set Rstats =     0
  set Rallmonth =  0
  set Rannual =    0
  set Raveann =    0
  set Rdaily =     1
  set RdailyPart = 0

# real EOF loads are calculated in the ETM, these programs just aggregate them to more meaningful variables
  set EOFdaily =   0
  set EOFannual =  0

# real EOS loads are calculated in the ETM, these programs just aggregate them to more meaningful variables
  set EOS =  0
  set EOSdaily = 0
  set EOSmonth = 0

# other outputs
  set DATannual =  0
  set DATmonthly = 0
  set DATaveann =  0

  set TFSannual =  0
  set TFSaveann =  0

  set DFSannual =  0
  set DFSaveann =  0

  set DELannual =  0
  set DELmonthly = 0
  set DELaveann =  0

#  SET the number of peaks to investigate, zero if no investigation
  set npeaks = 50

#  SET the window for number of hours to search simulated data looking for match
#  Searches forward and backward in time by 2*window+1 total hours
  set window = 24

#  SET loudness 0= no output for some programs, 1=regular output
  set loud = 0

####### RIVER SEGMENTS or WQ RECEIVING AREAS ONLY, EOS OUTPUTS DEFINED BY THE BODY THEY FLOW INTO  ########
  source $tree/config/seglists/${basin}.riv

  if (-e problem) then
    rm problem
  endif


####### DO WQM FIRST
  
  foreach seg ($segments)

    if ($wqm) then
      echo $scenario, $seg | $tree/code/bin/wqm_input.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

  end

####### DO ANY EDGE-OF-FIELD CALCULATIONS 

  foreach seg ($segments)

    if ($EOFdaily) then
      echo $scenario, $seg | $tree/code/bin/EOF_daily.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

    if ($EOFannual) then
      echo $scenario, $seg, $year1, $year2 | $tree/code/bin/EOF_annual.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

   end

####### DO ANY EDGE-OF-STREAM CALCULATIONS BEFORE RIVER
  
  foreach seg ($segments)

    if ($EOS) then
      echo $scenario, $seg, $loud, $year1, $year2 $EOSdaily $EOSmonth | $tree/code/bin/EOS.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

  end

####### FIND DATA INPUTS (POINT SOURCE, AT DEP, SEPTIC)
  foreach seg ($segments)

    if ($DATannual) then
      echo $scenario, $seg | $tree/code/bin/DATannual.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

    if ($DATmonthly) then
      echo $scenario, $seg | $tree/code/bin/DATmonthly.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

    if ($DATaveann) then
      echo $scenario, $seg, $year1, $year2 | $tree/code/bin/DATaveann.exe
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
      echo $scenario $seg, $year1, $year2 | $tree/code/bin/TSSRating.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif


    if ($Rstats) then
      echo $scenario $seg, $year1, $year2, $npeaks $window | $tree/code/bin/Rstats.exe
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
      echo $scenario, $seg, $year1, $year2 | $tree/code/bin/Raveann.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif
  

  end

####### DO DELIVERED CALCULATIONS LAST
  
  foreach seg ($segments)

    if ($TFSannual) then
      echo $scenario, $seg | $tree/code/bin/TFSannual.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

    if ($TFSaveann) then
      echo $scenario, $seg, $year1, $year2 | $tree/code/bin/TFSaveann.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

  end

  foreach seg ($segments)

    if ($DFSannual) then
      echo $scenario, $seg | $tree/code/bin/DFSannual.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

    if ($DFSaveann) then
      echo $scenario, $seg, $year1, $year2 | $tree/code/bin/DFSaveann.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

  end

  foreach seg ($segments)

    if ($DELannual) then
      echo $scenario, $seg | $tree/code/bin/DELannual.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

    if ($DELaveann) then
      echo $scenario, $seg, $year1, $year2 | $tree/code/bin/DELaveann.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

    if ($DELmonthly) then
      echo $scenario, $seg | $tree/code/bin/DELmonthly.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

  end

  if (${#argv} == 4) then
    cd ../
    rm -r temp$$
  endif


      
