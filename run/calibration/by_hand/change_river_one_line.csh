#!/bin/csh

 if (-e problem) then
   rm problem
 endif

 set paramscen = 2010TMDL_conow
 set basin = just_conow

# echo $basin,$paramscen,HTRCH,"HEAT-PARM",KATRAD,r,e,11 |../../../code/bin/Rchange_params_one_line.exe

# echo $basin,$paramscen,OXRX,"OX-GENPARM",$paramscenPSAT,r,m,1.0 |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,OXRX,"OX-BENPARM",BENOD,r,m,1. |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,OXRX,"OX-CFOREA",CFOREA,r,m,1. |../../../code/bin/Rchange_params_one_line.exe

# echo $basin,$paramscen,sedtrn,"silt-clay-pm#1",sw,r,e,0.02 |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,sedtrn,"silt-clay-pm#2",cw,r,e,0.0010 |../../../code/bin/Rchange_params_one_line.exe
 echo $basin,$paramscen,sedtrn,"silt-clay-pm#1",staucs,r,p,92.25 |../../../code/bin/Rchange_params_one_line.exe
 echo $basin,$paramscen,sedtrn,"silt-clay-pm#1",staucd,r,p,99|../../../code/bin/Rchange_params_one_line.exe
 echo $basin,$paramscen,sedtrn,"silt-clay-pm#2",ctaucs,r,p,93.25 |../../../code/bin/Rchange_params_one_line.exe
 echo $basin,$paramscen,sedtrn,"silt-clay-pm#2",ctaucd,r,p,99 |../../../code/bin/Rchange_params_one_line.exe
 echo $basin,$paramscen,sedtrn,"silt-clay-pm#1",sm,r,m, 11 |../../../code/bin/Rchange_params_one_line.exe
 echo $basin,$paramscen,sedtrn,"silt-clay-pm#2",cm,r,m, 11 |../../../code/bin/Rchange_params_one_line.exe

# echo $basin,$paramscen,PLANK,"PHYTO-PARM",REFSET,r,e,0.06 |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,NUTRX,"NUT-NITDENIT",KNO320,r,m,1. |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,NUTRX,"NUT-NITDENIT",KTAM20,r,m,1. |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,NUTRX,"NUT-BENPARM",BRTAM1,r,e,8. |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,NUTRX,"NUT-BENPARM",BRTAM2,r,e,8. |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,NUTRX,"NUT-BEDCONC",NH4-sand,r,e,20. |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,NUTRX,"NUT-BEDCONC",NH4-silt,r,e,200. |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,NUTRX,"NUT-BEDCONC",NH4-clay,r,e,500. |../../../code/bin/Rchange_params_one_line.exe
 echo $basin,$paramscen,NUTRX,"NUT-BEDCONC",PO4-sand,r,m, 15 |../../../code/bin/Rchange_params_one_line.exe
 echo $basin,$paramscen,NUTRX,"NUT-BEDCONC",PO4-silt,r,m, 15 |../../../code/bin/Rchange_params_one_line.exe
 echo $basin,$paramscen,NUTRX,"NUT-BEDCONC",PO4-clay,r,m, 15 |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,NUTRX,"NUT-BENPARM",BRPO41,r,m,0.5 |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,NUTRX,"NUT-BENPARM",BRPO42,r,m,0.5 |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,NUTRX,"NUT-ADSPARM",NH4-sand,r,e,15 |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,NUTRX,"NUT-ADSPARM",NH4-silt,r,e,150 |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,NUTRX,"NUT-ADSPARM",NH4-clay,r,e,1500 |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,NUTRX,"NUT-ADSPARM",PO4-sand,r,m,1 |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,NUTRX,"NUT-ADSPARM",PO4-silt,r,m,1 |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,NUTRX,"NUT-ADSPARM",PO4-clay,r,m,1 |../../../code/bin/Rchange_params_one_line.exe
# echo $basin,$paramscen,PLANK,"PLNK-PARM1",MALGR,r,m,0.5  |../../../code/bin/Rchange_params_one_line.exe

 if (-e problem) then
   cat problem
   rm problem
 endif
