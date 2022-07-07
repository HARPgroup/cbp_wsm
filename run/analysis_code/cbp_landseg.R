# install_github("HARPGroup/hydro-tools", force=TRUE)
library("hydrotools")
library('zoo')
basepath='/var/www/R';
source("/var/www/R/config.R")

# Set up our data source
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw)

# TBD: get inputs from the comand line
#  For now we just load some samples
lseg_name="A51800"
lseg_ftype="cbp532_landseg"

landseg<- RomFeature$new(
  ds,
  list(
    hydrocode=lseg_name, 
    ftype=lseg_ftype,
    bundle='landunit'
  ), 
  TRUE
)

model <- RomProperty$new(
  ds,
  list(
    varkey="om_model_element", 
    propname=landseg$name,
    featureid=landseg$hydroid, 
    entity_type="dh_feature", 
    propcode="cbp-5.3.2" 
  ), 
  TRUE
)

lu <- RomProperty$new(
  ds,
  list(
    varkey="om_hspf_landuse", 
    propname="for",
    featureid=model$pid, 
    entity_type="dh_properties", 
    propcode="for" 
  ), 
  TRUE
)
lu$save(TRUE)

# Create/Load a model scenario property
# tstime = the run time 
# note: do not set tstime when retrieving since if we have a previous
#       timesereies event already set, we want to gt it and may not know the tstime
# 
model_scenario <- RomProperty$new(
  ds, 
  list(
    varkey="om_scenario", 
    featureid=lu$pid, 
    entity_type="dh_properties", 
    propname="p532sova_2021", 
    propcode="p532sova_2021" 
  ), 
  TRUE
)
model_scenario$save(TRUE)

# FUTURE - do not need to do this yet.
# Create/Load a timeseries record of the model run
# tstime = the run time 
# note: do not set tstime when retrieving since if we have a previous
#       timesereies event already set, we want to gt it and may not know the tstime
# 
ts <- RomTS$new(
  ds, 
  list(
    varkey="om_scenario", 
    featureid=model$pid, 
    entity_type="dh_properties", 
    tscode="p532sova_2021" 
  ), 
  TRUE
)

ts$tstime = as.integer(as.POSIXct("2022/07/07 12:00:00", origin="1970-01-01"))
ts$save(TRUE)

