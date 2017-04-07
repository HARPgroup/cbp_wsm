#!/bin/sh

# run routine to parse all the .out files
./get_uptake_totals.sh 65 1 'all' > uptakes.txt
# run the routine to pass the results to the database server
php report_uptakes.php 65 uptakes.txt
