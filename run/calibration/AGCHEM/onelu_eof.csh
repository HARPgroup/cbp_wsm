#!/bin/csh

#   GET SCENARIO AND BASIN
  set scenario = $argv[1]
  set basin = $argv[2]
  set lu = 'pur'

#   SET VARIABLES  --  TO CHANGE WHAT RUNS, CHANGE THE PERLND, IMPLND, AND BASIN (SEGMENT LISTS)
  source ../../fragments/set_tree
  source seglists/${basin}.riv

  mkdir -p ../../../tmp/scratch/temp$$/
  cd ../../../tmp/scratch/temp$$/

#  SET AVERAGING YEARS
  set AVEYEAR1 = 1984
  set AVEYEAR2 = 2000

# real EOF loads are calculated in the ETM, these programs just aggregate them to more meaningful variables
  set EOFannual =  1
  set EOFmonthly = 0
  set EOFaveann =  1
  set EOFavesum =  1

####### DO ANY EDGE-OF-FIELD CALCULATIONS

  foreach seg ($segments)

    if ($EOFannual) then
      echo $scenario, $seg, $lu | $tree/code/bin/EOFann1.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

    if ($EOFaveann) then
      echo $scenario, $seg, $AVEYEAR1, $AVEYEAR2, $lu | $tree/code/bin/EOFaveann1.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

    if ($EOFavesum) then
      echo $scenario, $seg, $AVEYEAR1, $AVEYEAR2, $lu | $tree/code/bin/EOFavesum1.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
    endif

  end

  cd ../
  rm -r temp$$


# summary land sediment results
 grep ','           $tree/output/eof/summary/${scenario}/*${lu}.TSED                > $tree/output/eof/summary/${scenario}/${scenario}_eof_TSED_${lu}.csv

 if (-e problem) then
   cat problem
   rm problem
 endif







