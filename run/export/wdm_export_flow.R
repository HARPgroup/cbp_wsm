# R script to export a wdm, combine into files
basepath='/var/www/R';
source("/var/www/R/config.R");

library('lubridate')
library('data.table')
source('https://raw.githubusercontent.com/HARPgroup/cbp6/master/R/utils.R')

# Read Args
argst <- commandArgs(trailingOnly=T)
scenario <- as.character(argst[1])
lseg <- as.character(argst[2])
startyear <- as.integer(argst[3])
endyear <- as.integer(argst[4])
outbase <- as.character(argst[5])
wdmpath <- as.character(argst[6])

outpath = paste(outbase, 'out/land', scenario, 'eos', sep='/')
outname <- paste0(lseg, '_0111-0211-0411.csv' )

wdm_export_land_flow(lseg, wdmpath, scenario, startyear, endyear)
# use fast fread() mode, more than 2x speed enhancement
merged_df1 <- wdm_merge_land_flow(lseg, wdmpath, scenario , outpath, TRUE, TRUE)
# use slow mode
# merged_df2 <- wdm_merge_land_flow(lseg, wdmpath, scenario , outpath)

saved.file <- paste0(outpath, "/", outname)
# also much faster than other version
fwrite(merged_df1, saved.file, row.names = FALSE)
message(paste("Exported file", saved.file))
system(paste("chgrp almodelers ", saved.file))
