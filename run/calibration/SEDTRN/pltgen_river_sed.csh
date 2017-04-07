#!/bin/csh

#   GET SCENARIO AND BASIN
  set scenario = $argv[1]
  set basin = $argv[2]

#   SET VARIABLES  --  TO CHANGE WHAT RUNS, CHANGE THE SCENARIO, PERLND, IMPLND, AND BASIN (SEGMENT LISTS)
  source ../../fragments/set_tree
  
  source ../../seglists/${basin}.riv

  foreach seg ($segments)

############ check if a river
    echo $seg | $tree/code/bin/check_river.exe >river$$

    if (!(-z river$$)) then

      if (-e problem) then
        rm problem
      endif

      echo pltgens for segment $seg river scenario $scenario
      echo $seg,$scenario | $tree/code/bin/Rpltgen.exe

      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

    endif

    rm river$$

  end

############# summarze river results
  grep 'beddep' $tree/output/pltgen/river/${scenario}/*sums > $tree/output/pltgen/summary/${scenario}/BEDDEP_feet.csv
  grep 'depscr' $tree/output/pltgen/river/${scenario}/*sums > $tree/output/pltgen/summary/${scenario}/DEPSCR_tons.csv





