#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_lug.csh scenario basin'
      echo ' or     run_lug.csh scenario basin tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  if (${#argv} == 3) then
    set tree = $argv[3]
  else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

  # load new scenario configuration script
  source $CBP_ROOT/config/control/script/${scenario}.con
  source $CBP_ROOT/run_bhatt/.config/${user}_CONFIG

  source $tree/run/fragments/set_landuse

  source $tree/config/seglists/${basin}.land

  if (-e problem) then
    rm problem
  endif

  foreach seg ($segments)

    foreach lu ($perlnds $implnds)

      echo making UCI for $lu  segment $seg   land scenario $scenario

      if (-e problem) then
        rm problem
      endif

      echo $seg, $lu, $scenario | $tree/code/bin/lug.exe
      # modifies uci for hsp2 or hspf depending on environment variable
      set uci_path="$tree/tmp/uci/land/$lu/$scenario/$lu${seg}.uci"
      hsp_uci_version $uci_path $HSP_VERSION

      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

    end

  end
     
  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif
 
