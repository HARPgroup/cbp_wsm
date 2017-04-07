#!/bin/csh

 if (-e problem) then
   rm problem
 endif

 ../../../code/bin/Rchange_params.exe

 if (-e problem) then
   cat problem
   rm problem
 endif
