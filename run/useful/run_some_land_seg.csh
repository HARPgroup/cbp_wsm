#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 1) then
    echo ' '
    echo ' To make this script run, give the argument "go"'
    echo ' '
    exit
  endif
  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' To make this script run, give the argument "go"'
      echo ' '
      exit
    endif
  endif

  set scenario = 2010NANutManN062711
  set seg = F51171

  set perlnds = ( for hvf hwm lwm hom hyw hyo alf pas trp urs nhi nlo nho nhy nal npa rpd npd cpd rcn ccn rex nex cex )
  set implnds = ( afo cfo rid nid cid )

  set perlnds = ( alf pas trp urs nhi nlo nho nhy nal npa rpd npd cpd rcn ccn rex nex cex )
  set implnds = ( afo cfo rid nid cid )

    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  source $tree/run/fragments/set_icprb_hspf
  
    foreach lu ($perlnds $implnds)

      echo running $lu for segment $seg land scenario $scenario

      cp -v $tree/config/blank_wdm/land.wdm $lu$seg'.wdm'

      set inp = $tree/tmp/uci/land/$lu/$scenario/$lu$seg'.uci'

      echo $inp | $hspf

      tail -1 $lu$seg'.ech' > EOJtest$$
      diff $tree/run/test/EOJ EOJtest$$ > diffeoj
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

    end

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif

