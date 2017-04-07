#!/bin/csh

  set fnam = $argv[1]
  source $fnam

  foreach seg ($segments)
    echo $seg
  end
