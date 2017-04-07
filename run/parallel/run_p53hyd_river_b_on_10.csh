#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script runs the p53hyd scenario on multiple basins'
    echo ' '
    echo ' To make this script run, use the argument "go"'
    echo ' '
    exit
  endif
  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' running this script runs the p53hyd scenario on multiple basins'
      echo ' '
      echo ' To make this script run, use the argument "go"'
      echo ' '
      exit
    endif
  endif

 set scenario = p53hyd

 set basins = (SlowNoZero PlowNoZero WNoZero ENoZero J_BFL_NoZero YNoZero RNoZero XNoZero GNoZero DNoZero)

 foreach basin ($basins)
   sbatch ../standard/run_river.csh $scenario $basin
 end

