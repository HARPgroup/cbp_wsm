#!/bin/csh

  if (${#argv} != 3) then
    if (${#argv} != 4) then
      echo ' '
      echo 'usage:  batch_run_all.csh scenario calscen basin '
      echo ' or     batch_run_all.csh scenario calscen basin tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set calscen  = $argv[2]
  set basin    = $argv[3]

#  set clu = (hwm hom lwm alf hyw)
  set clu = (lwm alf hyw)
##########################################################

  foreach lu ($clu)

    sbatch run_crop_some.csh $scenario $calscen $basin $lu

  end

