#' Read h5 files
#'
#' @description 
#' This function reads the .h5 files previously imported in R. 
#' It was imported using \code{\link[provoc]{import.h5}}.
#' Read the doc here : \href{https://github.com/JHuguenin/provoc}{GitHub}.
#' Data were then saved in the form of .Rdata. Redefine the working directory.
#'
#' @param file a character of length 1. The path to the .Rdata file.
#'
#' @return A `list` containing all data and more informations. 
#' See \href{https://github.com/JHuguenin/provoc}{GitHub}. 
#' 
#' @export

load_h5_data <- function(file_path) {
  e <- new.env()
  load(file_path, envir = e)
  e$sp$wd <- here::here()
  return(e$sp)
}

#' Create empty metadata file
#'
#' @description 
#' This function create the meta_empty.csv file needed to the 
#' execution of \pkg{provoc}. Redefine the working directory.
#'
#' @param file a character of length 1. The path to the .Rdata file.
#'
#' @return A `list` containing all data and more informations. 
#' See \href{https://github.com/JHuguenin/provoc}{GitHub}. 
#' 
#' @export

create_meta <- function(file){
  
  # create meta folder to keep meta files
  if (!dir.exists("meta")) {
    dir.create("meta", recursive = TRUE)
  }
  file$wd <- here::here("meta/")
  provoc::empty.meta(file)
  
  # remove Figures folder created by provoc
  # if (!dir.exists("Figures")) {
  #   unlink("Figures", recursive = TRUE)
  # not for the moment, needed for Figures
  # have to rename this folder latter
  # }
}
