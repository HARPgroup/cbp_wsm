#!/bin/csh

set SYEAR = $argv[1]

cp -vip ../config/control/land/_BLANK_PROG_RA.con   ../config/control/land/${SYEAR}PROGRA_20200427.con
cp -vip ../config/control/river/_BLANK_PROG_RA.con  ../config/control/river/${SYEAR}PROGRA_20200427.con
cp -vip ../config/control/script/_BLANK_PROG_RA.con ../config/control/script/${SYEAR}PROGRA_20200427.con


find ../config/control/land/   -name ${SYEAR}PROGRA_20200427.con | xargs perl -pi -e "s/_BLANK_PROG_RA/${SYEAR}PROGRA_20200427/g"
find ../config/control/river/  -name ${SYEAR}PROGRA_20200427.con | xargs perl -pi -e "s/_BLANK_PROG_RA/${SYEAR}PROGRA_20200427/g"

find ../config/control/script/ -name ${SYEAR}PROGRA_20200427.con | xargs perl -pi -e "s/_BLANK_PROG_RA/${SYEAR}PROGRA_20200427/g"
find ../config/control/script/ -name ${SYEAR}PROGRA_20200427.con | xargs perl -pi -e "s/_SB_SCENARIO_NAME/${SYEAR}ProgressRA/g"

find ../config/control/land/   -name ${SYEAR}PROGRA_20200427.con | xargs perl -pi -e "s/_RA_PRECIP_ATMOS/P602_${SYEAR}/g"
find ../config/control/river/  -name ${SYEAR}PROGRA_20200427.con | xargs perl -pi -e "s/_RA_PRECIP_ATMOS/P602_${SYEAR}/g"


echo Y | csh temp.csh ${SYEAR}PROGRA_20200427

