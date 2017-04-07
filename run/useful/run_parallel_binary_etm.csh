#!/bin/csh
    source ../../config/seglists/p52cal.riv

    foreach seg ($segments)

     sbatch run_binary_etm_only_oneseg.csh p52 $seg

    end

