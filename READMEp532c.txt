REQUIREMENTS
   csh, c and fortran compiler(on a linux or unix machine).
1) Download p532c.tar from ftp.chesapeakebay.net 
   This will occupy ~ 880MB of disk space
2) Untar the tar file           
	tar -xvjf p532c.tar
3) Build the Phase 5.3.2 package by execute the compile_all.csh script in code/src
4) Run a test
        cd ../../run/standard/
        csh run_all.csh p532cal_062211 stmary
5) Verify the test
   Compare the values in the .csv files in the sample/ folder with those in in your sumout/ folder.
6) The Documentation/ folder contains manuals for the usage of the phase p532
watershed model. Please drop an email if you encounter problems (rtian@chesapeakebay.net).
