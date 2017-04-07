
#   GET SCENARIO AND BASIN
  set scenario = $argv[1]
  set basin = $argv[2]
  set lu = 'for'


# summary land sediment results
 grep ','           $tree/output/eof/summary/${scenario}/*${lu}.TSED                > $tree/output/eof/summary/${scenario}/${scenario}_eof_TSED_${lu}.csv

 if (-e problem) then
   cat problem
   rm problem
 endif

