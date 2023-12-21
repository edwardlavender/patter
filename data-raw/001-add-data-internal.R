#########################
#########################
#### add-data-internal.R

#### Aims
# 1) Prepare internal datasets

#### Prerequisites
# 1) NA


#########################
#########################
#### Set up

#### Wipe workspace
rm(list = ls())
try(pacman::p_unload("all"), silent = TRUE)
dv::clear()


#########################
#########################
#### Define internal datasets

#### pf class object name
pf_class <- "pf_particles"

#### Add datasets
usethis::use_data(pf_class, internal = TRUE)


#### End of code.
#########################
#########################
