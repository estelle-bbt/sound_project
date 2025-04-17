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
  
  tar_target(h5_Rdata,load_h5_data("data/imported_h5_files/all_ttt_h5.RData")),
  
  tar_target(phenot_data,load_phenotypes("data/clean_data_phenotype.csv")),
  
  # tar_target(meta_file,create_meta(h5_Rdata)),
  
  tar_target(modified_Rdata,modify_meta(h5_Rdata)),
  
  tar_target(formatted_table,format_peaks(modified_Rdata,phenot_data)),

  tar_target(summarized_data,summarize_data(formatted_table)),

  tar_target(plot_summarized_data,get_plot_summarized_data(summarized_data)),

  tar_target(summarized_data_per_mass,summarize_data_per_mass(formatted_table)),

  tar_target(plot_summarized_data_per_mass,get_plot_summarized_data_per_mass(summarized_data_per_mass)),
  # 
  # tar_target(data_comparison_before_after,data_compare_before_after(formatted_table)),
  # 
  # tar_target(model_comparison_before_after,model_compare_before_after(data_comparison_before_after)),
  # 
  # tar_target(plot_comparison_before_after,plot_compare_before_after(data_comparison_before_after)),
  # 
  tar_target(model_comparison_time,model_compare_time(formatted_table)),

  tar_target(model_comparison_time_pval,model_compare_time_pval(formatted_table)),
  
  ## Quarto ----
  
  tarchetypes::tar_quarto(index, "index.qmd")
  
)



