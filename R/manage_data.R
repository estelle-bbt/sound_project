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
#' Not sure if really necessary to create one:
#' we can deal directly with metadata from the Rdata.
#'
#' @param file a character of length 1. The path to the .Rdata file.
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

#' Modify metadata directly from Rdata
#'
#' @description 
#' This function add a column treatment and normalize time: 45 points
#' for FqB instead of 30.
#'
#' @param file a character of length 1. The Rdata.
#'
#' 
#' @export

modify_meta <- function(file){
  
  file$mt$meta <- as.data.frame(file$mt$meta)
  file$mt$meta$names <- rownames(file$mt$meta)
  file$mt$meta <- file$mt$meta |> # add temporal bloc and treatment
    dplyr::mutate(
      split_names = stringr::str_split(names, "_"),  
      temporal_bloc = sapply(split_names, function(x) x[1]),  
      treatment = sapply(split_names, function(x) x[3]),  
      temporal_bloc = dplyr::case_when(
        temporal_bloc %in% "fl" ~ "bloc_1",      # rename correctly temporal bloc
        TRUE ~ "bloc_2"
      ),  
      treatment = dplyr::case_when(
        treatment %in% c("Bl1", "Bl2") ~ "Blc",      # rename correctly blank treatment
        TRUE ~ treatment  
      )
    ) |>
    dplyr::rename(dot_column = "...") |>
    dplyr::select(!c(dot_column,split_names)) |>
    dplyr::mutate(nbr_MS = 30, # normalize time
                  start = as.numeric(start),
                  end = start + 29)
  
  return(file)
}

#' Load phenotypic data
#'
#' @description 
#' Blabla
#'
#' @param file_path a character of length 1. The path to the .csv file.
#'
#' @return A `tibble` formatted for further analyses. 
#' 
#' @export


load_phenotypes <- function(file_path){
  
  # targets::tar_load("h5_Rdata")
  
  dt_phenot <- read.csv(file_path, sep = ";", dec = ",", head = T)
  return(dt_phenot)
}


#' Format peaks data
#'
#' @description 
#' Blabla
#'
#' @param file a character of length 1. The Rdata.
#'
#' @return A `tibble` formatted for further analyses. 
#' 
#' @export

format_peaks <- function(file, phenot_dt){
  
  # targets::tar_load("h5_Rdata")
  
  file_peaks <- as.data.frame(file$peaks) |> 
    tibble::rownames_to_column(var = "names") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      sum_peaks = sum(dplyr::c_across(2:last_col()), na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      split_names = stringr::str_split(names, "_"),  
      temporal_bloc = sapply(split_names, function(x) x[1]),  
      treatment = sapply(split_names, function(x) x[3]),  
      time = as.numeric(sapply(split_names, function(x) x[4])),
      temporal_bloc = dplyr::case_when(
        temporal_bloc %in% "fl" ~ "bloc_1",      # rename correctly temporal bloc
        TRUE ~ "bloc_2"
        ),  
      treatment = dplyr::case_when(
        treatment %in% c("Bl1", "Bl2") ~ "Blc",      # rename correctly blank treatment
        TRUE ~ treatment  
      )
    ) |>
    # dplyr::filter(temporal_bloc == "bloc_2") |>
    dplyr::mutate(
      clean_names = stringr::str_remove(names, "_[^_]+$"),
      merge_names = stringr::str_extract(names, "^[^_]+_[^_]+")
    ) |>
    dplyr::left_join(phenot_dt,by = dplyr::join_by("merge_names" == "names")) |>
    dplyr::select(names, clean_names,treatment, time, sum_peaks, everything(),-split_names, -merge_names)
    
  return(file_peaks)
}

#' Summarise peaks
#'
#' @description 
#' Blabla
#'
#' @param file blabla
#'
#' @return blabla
#' 
#'
#' @export

summarize_data <- function(table, time_points = c(1:30)){
  
  # targets::tar_load("formatted_table")
  
  summarized_table <- table |> 
    dplyr::filter(time %in% time_points) |>
    dplyr::group_by(treatment,time) |> 
    dplyr::summarise(
      mean_peaks = mean(sum_peaks, na.rm = TRUE),
      sd_peaks = sd(sum_peaks, na.rm = TRUE),
      n = dplyr::n(),
      se = sd_peaks / sqrt(n),  # Erreur standard
      ci_95 = qt(0.975, df = n - 1) * se  # Intervalle de confiance 95%
    ) |> 
    dplyr::mutate(
      lower_ci = mean_peaks - ci_95,
      upper_ci = mean_peaks + ci_95
    ) 
  
  return(summarized_table)
}

#' Get plot summarised peaks
#'
#' @description 
#' Blabla
#'
#' @param file blabla
#'
#' @return blabla
#' 
#'
#' @export

get_plot_summarized_data <- function(summarized_table){
  
  # targets::tar_load("formatted_table")
  
  plot <- summarized_table |> 
    ggplot(aes(x = time,y = mean_peaks, color = treatment)) +
    geom_line() + 
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = treatment), alpha = 0.2, color = NA) + 
    theme_minimal()
  
  return(plot)
}

#' Summarise data per mass
#'
#' @description 
#' Blabla
#'
#' @param file blabla
#'
#' @return blabla
#' 
#'
#' @export

summarize_data_per_mass <- function(table,time_points = c(1:30)){
  
  # select cols
  cols_to_summarize <- names(table)[5:ncol(table)]
  
  results_list <- list()
  
  for (col in cols_to_summarize) {
    
    temp_result <- table |> 
      dplyr::filter(time %in% time_points) |> 
      dplyr::group_by(treatment, time) |> 
      dplyr::summarise(
        mean = mean(.data[[col]], na.rm = TRUE),
        sd = sd(.data[[col]], na.rm = TRUE),
        n = sum(!is.na(.data[[col]])),
        .groups = "drop"
      ) |> 
      dplyr::mutate(
        se = sd / sqrt(n), 
        ci_95 = qt(0.975, df = n - 1) * se, 
        lower_ci = mean - ci_95,
        upper_ci = mean + ci_95
      ) |> 
      dplyr::mutate(variable = col)  
    
    results_list[[col]] <- temp_result  
  }
  
  summarized_table <- dplyr::bind_rows(results_list)
  
  return(summarized_table)
}

#' Get plot summarised peaks per mass
#'
#' @description 
#' Blabla
#'
#' @param file blabla
#'
#' @return blabla
#' 
#'
#' @export
#' 


get_plot_summarized_data_per_mass <- function(summarized_table) {
  
  plots_list <- lapply(unique(summarized_table$variable), function(var) {
    plot <- summarized_table |>
      dplyr::filter(variable == var) |>  
      ggplot(aes(x = time, y = mean, color = treatment)) +
      geom_line() + 
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = treatment), alpha = 0.2, color = NA) + 
      theme_minimal() +
      labs(x = "Time", y = "Mean", title = paste0("Mass ",var))
    return(plot)
  })
  
  names(plots_list) <- unique(summarized_table$variable)
  
  return(plots_list)  # Retourner la liste de graphiques
}

#' Linear model for global effect
#'
#' @description 
#' Blabla
#'
#' @param file blabla
#'
#' @return blabla
#' 
#'
#' @export
#' 


compare_before_after <- function(formatted_table) {
  
  clean_table <- formatted_table |> 
    dplyr::mutate(
      clean_names = stringr::str_remove(names, "_[^_]+$")  # Retirer tout après le dernier underscore
    ) |>
    dplyr::group_by(clean_names,treatment,nb_flowers) |>
    dplyr::summarise(
      mean_sum_peaks_before = mean(sum_peaks[time >= 5 & time <= 10], na.rm = TRUE),
      mean_sum_peaks_after = mean(sum_peaks[time >= 12 & time <= 17], na.rm = TRUE)
    ) |>
    tidyr::pivot_longer(
      cols = c(mean_sum_peaks_before, mean_sum_peaks_after), 
      names_to = "period", 
      values_to = "mean_sum_peaks"
    ) |>
    dplyr::mutate(
      period = dplyr::case_when(
        period == "mean_sum_peaks_before" ~ "before",
        period == "mean_sum_peaks_after" ~ "after",
        TRUE ~ period
      )
    )
  
  # return(clean_table) 
  
  model <- lm(data = clean_table, mean_sum_peaks ~ period*treatment)
  result_anova <- car::Anova(model)
  
  return(result_anova)
}

#' Prepare before after table
#'
#' @description 
#' Blabla
#'
#' @param file blabla
#'
#' @return blabla
#' 
#'
#' @export
#' 


data_compare_before_after <- function(formatted_table) {
  
  clean_table <- formatted_table |> 
    dplyr::mutate(
      clean_names = stringr::str_remove(names, "_[^_]+$")  # Retirer tout après le dernier underscore
    ) |>
    dplyr::group_by(clean_names,temporal_bloc,treatment,nb_flowers) |>
    dplyr::summarise(
      mean_sum_peaks_before = mean(sum_peaks[time >= 5 & time <= 10], na.rm = TRUE),
      mean_sum_peaks_after = mean(sum_peaks[time >= 12 & time <= 17], na.rm = TRUE)
    ) |>
    tidyr::pivot_longer(
      cols = c(mean_sum_peaks_before, mean_sum_peaks_after), 
      names_to = "period", 
      values_to = "mean_sum_peaks"
    ) |>
    dplyr::mutate(
      period = dplyr::case_when(
        period == "mean_sum_peaks_before" ~ "1_before",
        period == "mean_sum_peaks_after" ~ "2_after",
        TRUE ~ period
      )
    )
  
  model <- lm(data = clean_table, mean_sum_peaks ~ period*treatment)
  result_anova <- car::Anova(model)
  
  plot <- ggplot(clean_table, aes(x=period,y=mean_sum_peaks,color=treatment)) +
    geom_boxplot()
  
  return(clean_table)
}


#' Model before after 
#'
#' @description 
#' Blabla
#'
#' @param file blabla
#'
#' @return blabla
#' 
#'
#' @export
#' 


model_compare_before_after <- function(clean_table) {
  
  model <- lme4::lmer(data = clean_table, mean_sum_peaks ~ period * treatment + nb_flowers + (1|clean_names) + (1|temporal_bloc))
  result_anova <- car::Anova(model)

  return(result_anova)
}

#' Plot before after 
#'
#' @description 
#' Blabla
#'
#' @param file blabla
#'
#' @return blabla
#' 
#'
#' @export
#' 


plot_compare_before_after <- function(clean_table) {
  
  plot <- ggplot(clean_table, aes(x=treatment,y=mean_sum_peaks,color=period)) +
    geom_boxplot()
  
  return(plot)
}


#' Model with continuous time
#'
#' @description 
#' Blabla
#'
#' @param file blabla
#'
#' @return blabla
#' 
#' 
#' @export
#' 


model_compare_time <- function(formatted_table, response = "sum_peaks") {
  
  formatted_table_reduced <- formatted_table |>
    dplyr::filter(time %in% c(5:17))
  
  # mixed linear model
  model <- lmerTest::lmer(get(response) ~ time * treatment + nb_flowers + (1 | clean_names) + (1|temporal_bloc), data = formatted_table_reduced)
  
  # summary
  result_summary <- summary(model)
  
  # anova
  result_anova <- car::Anova(model)
  
  # post-hoc tests
  post_hoc_comparisons <- emmeans::emmeans(model, pairwise ~ treatment | time)$contrasts
  
  # linear mixed model with discrete time 
  model_discrete <- lmerTest::lmer(get(response) ~ factor(time) * treatment + nb_flowers + (1 | clean_names) + (1|temporal_bloc), data = formatted_table_reduced)

  # post-hoc tests with discrete time
  post_hoc_comparisons_discrete <- emmeans::emmeans(model_discrete, pairwise ~ treatment | factor(time))$contrasts  
  
  return(list(result_anova = result_anova, 
              result_summary = result_summary, 
              post_hoc_comparisons = post_hoc_comparisons,
              post_hoc_comparisons_discrete = post_hoc_comparisons_discrete)
         )
}

#' Model with continuous time just to get the p-values
#'
#' @description 
#' Blabla
#'
#' @param file blabla
#'
#' @return blabla
#' 
#' 
#' @export
#' 


model_compare_time_pval <- function(formatted_table) {
  
  # targets::tar_load("formatted_table")
  
  formatted_table_reduced <- formatted_table |>
    dplyr::filter(time %in% c(5:17))
  
  anova_table <- dplyr::tibble(covs = character(),
                               pvalues = numeric())
  
  for(col in c(6:(length(formatted_table)-5))){
    col_name <- colnames(formatted_table[,col])
    
    model <- lmerTest::lmer(get(col_name) ~ time * treatment + nb_flowers + (1 | clean_names) + (1|temporal_bloc), data = formatted_table_reduced)
    
    anova_df <- car::Anova(model) |>
      as.data.frame()
    
    anova_df$var <- rownames(anova_df)
    
    pval <- anova_df |>
      dplyr::filter(var == "time:treatment") |>
      dplyr::pull("Pr(>Chisq)")
    
    anova_table <- anova_table |>
      dplyr::bind_rows(tibble::tibble(covs = col_name, pvalues = pval))
    
  }
  
  anova_table <- anova_table |> 
    dplyr::filter(pvalues < 0.05)
  
  return(anova_table)
}
