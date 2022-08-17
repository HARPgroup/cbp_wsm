#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_land.csh scenario basin'
      echo ' or     run_land.csh scenario basin tree'
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
  source $tree/config/control/script/${scenario}.con
  source $tree/run/fragments/set_landuse
  source $tree/run/fragments/set_icprb_hspf
  
  source $tree/config/seglists/${basin}.land

  foreach seg ($segments)

    foreach lu ($perlnds $implnds)

      echo running $lu for segment $seg land scenario $scenario

      cp -v $tree/config/blank_wdm/land.wdm $lu$seg'.wdm'

      set inp = $tree/tmp/uci/land/$lu/$scenario/$lu$seg'.uci'
      echo $seg, $lu
      if ($HSP_VERSION == "hsp2") then
        hsp2 import_uci $inp $lu$seg'.h5'
        hsp2 run $lu$seg'.h5'
        set h5file = $CBP_EXPORT_DIR/land/h5/$lu/$scenario/$lu$seg'.h5'
        mv $lu$seg'.h5' $h5file
        # Run post-process extract routine
        if(" $perlnds " =~ *" $lu "*) then
         echo "Exporting PERLND $lu"
         set ds="/RESULTS/PERLND_P001/PWATER/table"
         set mod="pwater"
        else
         echo "Exporting IMPLND $lu"
         set ds="/RESULTS/IMPLND_I001/IWATER/table"
         set mod="iwater"
        endif
        set csvfile = $CBP_EXPORT_DIR/land/$scenario/$mod/$lu${seg}_pwater'.csv'
        echo "Rscript $CBP_ROOT/run/export/export_hsp_h5.R $h5file $csvfile $ds"
        Rscript $CBP_ROOT/run/export/export_hsp_h5.R $h5file $csvfile $ds
        # Remove h5 file to save space
        rm $h5file
        # Now we run summary scripts.  At some point we should maybe moce to postproc area
        # but there are advantages to having this feedback early in model execution 
        $CBP_ROOT/run/export/summarize_landseg.csh $scenario $seg $lu $CBP_ROOT $CBP_EXPORT_DIR
      else 
        echo $inp | $hspf
        tail -1 $lu$seg'.ech' > EOJtest$$
        diff $tree/run/fragments/EOJ EOJtest$$ > diffeoj
        rm EOJtest$$
        if (!(-z diffeoj)) then
          if (-e problem) then
            rm problem
          endif
          echo 'land segment: ' $seg ' did not run for land use: ' $lu >problem
          echo '  input file ' $inp >>problem
          echo   check the file ../../tmp/scratch/temp$$/$lu$seg.ech >>problem
          cat problem
          exit
        endif
        mv $lu$seg'.out' $tree/output/hspf/land/out/$lu/$scenario/
        mv $lu$seg'.ech' $tree/output/hspf/land/ech/$lu/$scenario/
        mv $lu$seg'.wdm' $tree/tmp/wdm/land/$lu/$scenario/

      endif


    end

  end

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif

