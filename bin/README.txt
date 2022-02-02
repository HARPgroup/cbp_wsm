CBP execution script
To set up, either:
  1. Copy all scripts from this directory to /usr/local/bin (or some other place for executables
     that is in your $PATH variable.
     cp -p find_config /usr/local/bin
     cp -p hspf_config /usr/local/bin
     cp -p cbp /usr/local/bin
  2. Run "./setup.sh" from the directory containing this README.txt, which will install soft-links 
     to these scripts in /usr/local/bin.
