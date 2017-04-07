#!/bin/csh

  if (${#argv} != 1 ) then
    echo 'usage: run_marsh_loads_to_wqm50k.csh MARSH_SCENARIO'
    echo '   will look for a data file in '
    echo '   /wqm/p5wqm/wqm/data/marsh/MARSH_SCENARIO/'
    exit
  endif

  set scen = $argv[1]

  echo 'compiling'
  cd ../src/marsh_loads_to_wqm50k/
  compile
  cd ../../run/

  mkdir -p ../out/$scen/

  echo 'running'

  echo $scen | ../bin/marsh_loads_to_wqm50k.exe

  if (-e problem) then
    cat problem
    rm problem
    exit
  endif

