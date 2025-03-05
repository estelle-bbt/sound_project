#' Targets plan
#' 

## Attach required packages ----

library(targets)
library(tarchetypes)
library(ggplot2)

tar_source()

## Load Project R Functions ----

source(here::here("R", "manage_data.R"))

## Analyses pipeline ----

list(
  
  ## Read data and create meta ----
  
  tar_target(h5_Rdata,
             read_h5_Rdata("data/imported_h5_files/all_ttt_h5.RData"))
  
  # tar_target(meta_file,create_meta(h5_Rdata))
  
  
)



