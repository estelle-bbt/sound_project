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
  
  ## Manage data ----
  
  tar_target(h5_Rdata,
             load_h5_data("data/imported_h5_files/all_ttt_h5.RData")),
  
  tar_target(meta_file,create_meta(h5_Rdata)),
  
  ## Quarto ----
  
  tarchetypes::tar_quarto(index, "index.qmd") 
  
)



