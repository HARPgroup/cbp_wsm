#!/bin/csh

  set scenario = temp804   # good to use a temp scenario

###### MAKE SURE THAT THE DATES ARE SET TO 1985 01 10 AND
########                                   1985 12 31 
########### IN THE ORIGINAL CONTROL FILE

########### if allFAT needs to be generated, use the land files
############# from the recently generated basingen files

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/1985 01 01/s//1986 01 01' > ed.edp
  echo 'g/1985 12 31/s//1986 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/1986 01 01/s//1987 01 01' > ed.edp
  echo 'g/1986 12 31/s//1987 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/1987 01 01/s//1988 01 01' > ed.edp
  echo 'g/1987 12 31/s//1988 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/1988 01 01/s//1989 01 01' > ed.edp
  echo 'g/1988 12 31/s//1989 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/1989 01 01/s//1990 01 01' > ed.edp
  echo 'g/1989 12 31/s//1990 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/1990 01 01/s//1991 01 01' > ed.edp
  echo 'g/1990 12 31/s//1991 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/1991 01 01/s//1992 01 01' > ed.edp
  echo 'g/1991 12 31/s//1992 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/1992 01 01/s//1993 01 01' > ed.edp
  echo 'g/1992 12 31/s//1993 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/1993 01 01/s//1994 01 01' > ed.edp
  echo 'g/1993 12 31/s//1994 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/1994 01 01/s//1995 01 01' > ed.edp
  echo 'g/1994 12 31/s//1995 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/1995 01 01/s//1996 01 01' > ed.edp
  echo 'g/1995 12 31/s//1996 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/1996 01 01/s//1997 01 01' > ed.edp
  echo 'g/1996 12 31/s//1997 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/1997 01 01/s//1998 01 01' > ed.edp
  echo 'g/1997 12 31/s//1998 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/1998 01 01/s//1999 01 01' > ed.edp
  echo 'g/1998 12 31/s//1999 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/1999 01 01/s//2000 01 01' > ed.edp
  echo 'g/1999 12 31/s//2000 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/2000 01 01/s//2001 01 01' > ed.edp
  echo 'g/2000 12 31/s//2001 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/2001 01 01/s//2002 01 01' > ed.edp
  echo 'g/2001 12 31/s//2002 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/2002 01 01/s//2003 01 01' > ed.edp
  echo 'g/2002 12 31/s//2003 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/2003 01 01/s//2004 01 01' > ed.edp
  echo 'g/2003 12 31/s//2004 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


  echo 'g/2004 01 01/s//2005 01 01' > ed.edp
  echo 'g/2004 12 31/s//2005 12 31' >> ed.edp
  echo 'w' >> ed.edp
  echo 'q' >> ed.edp

  ed ../../control/land/$scenario.con < ed.edp

  run_write_apps_only.csh $scenario allFAT 


