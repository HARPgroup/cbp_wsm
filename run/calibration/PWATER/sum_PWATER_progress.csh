#!/bin/csh

  set fil =  $argv[1]

  if (-e problem) then
    rm problem
  endif

  echo $fil | ../../../code/bin/progress_sumstats.exe

  if (-e problem) then
    cat problem
    rm problem
  endif

