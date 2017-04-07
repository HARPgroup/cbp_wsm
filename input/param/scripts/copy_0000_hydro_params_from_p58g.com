#!/bin/csh

  set p58scen = try24

  set p510scen = hydro

  set lusfor = ( for    )
  set lushvf = ( hvf    )
  set luspur = ( pul puh   )
  set lusbar = ( bar    )
  set lusext = ( ext    )
  set luspas = ( pas npa   )
  set lusbpa = ( trp urs afo  )
  set lushay = ( alf hyw hyo nhy nal)
  set lushom = ( hom nho   )
  set lushwm = ( hwm nhi   )
  set luslwm = ( lwm nlo   )

  foreach lu ($lusfor)
    cp /model/p58g/pp/param/for/$p58scen/PWATER_0000.csv $lu/$p510scen/PWATER.csv
  end
  foreach lu ($lushvf)
    cp /model/p58g/pp/param/hvf/$p58scen/PWATER_0000.csv $lu/$p510scen/PWATER.csv
  end
  foreach lu ($luspur)
    cp /model/p58g/pp/param/pur/$p58scen/PWATER_0000.csv $lu/$p510scen/PWATER.csv
  end
  foreach lu ($lusbar)
    cp /model/p58g/pp/param/bar/$p58scen/PWATER_0000.csv $lu/$p510scen/PWATER.csv
  end
  foreach lu ($lusext)
    cp /model/p58g/pp/param/ext/$p58scen/PWATER_0000.csv $lu/$p510scen/PWATER.csv
  end
  foreach lu ($luspas)
    cp /model/p58g/pp/param/pas/$p58scen/PWATER_0000.csv $lu/$p510scen/PWATER.csv
  end
  foreach lu ($lusbpa)
    cp /model/p58g/pp/param/bpa/$p58scen/PWATER_0000.csv $lu/$p510scen/PWATER.csv
  end
  foreach lu ($lushay)
    cp /model/p58g/pp/param/hay/$p58scen/PWATER_0000.csv $lu/$p510scen/PWATER.csv
  end
  foreach lu ($lushom)
    cp /model/p58g/pp/param/hom/$p58scen/PWATER_0000.csv $lu/$p510scen/PWATER.csv
  end
  foreach lu ($lushwm)
    cp /model/p58g/pp/param/hwm/$p58scen/PWATER_0000.csv $lu/$p510scen/PWATER.csv
  end
  foreach lu ($luslwm)
    cp /model/p58g/pp/param/lwm/$p58scen/PWATER_0000.csv $lu/$p510scen/PWATER.csv
  end

