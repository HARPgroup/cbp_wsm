#!/bin/csh

  set WQfiles = (SEDTRN OXRX NUTRX PLANK)

  set basins = (SC XU PM RU YP YM JL JA EM DG)
  set smbasins = (smallbay smallsova)

  set pardir = ../../input/param/river/

  foreach file ($WQfiles)

    set fnam = $file.csv

    head -2 $pardir/wq$basins[1]/$fnam > $fnam

    foreach basin ($basins)
      source ../seglists/${basin}.riv
      foreach seg ($segments)
        grep $seg $pardir/wq$basin/$fnam >> $fnam
      end
    end
      
    foreach basin ($smbasins)
      source ../seglists/${basin}.riv
      foreach seg ($segments)
        grep $seg $pardir/$basin/$fnam >> $fnam
      end
    end
      
    
  end

