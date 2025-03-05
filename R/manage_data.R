#' Read h5 files
#'
#' @description 
#' This function reads the .h5 files previously imported in R. 
#' It was imported using \code{\link[provoc]{import.h5}}.
#' Read the doc here : \href{https://github.com/JHuguenin/provoc}{GitHub}.
#' Data were then saved in the form of .Rdata.
#'
#' @param file a character of length 1. The path to the .Rdata file.
#'
#' @return A `list` containing all data and more informations. 
#' See \href{https://github.com/JHuguenin/provoc}{GitHub}. 
#' 
#' @export

read_h5_Rdata <- function(file){
  load(file)
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

create_meta <- function(sp){
  sp$wd <- here::here()
  provoc::empty.meta(sp)
}