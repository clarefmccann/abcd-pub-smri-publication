# --- 1. setup and configuration ---
pacman::p_load(
  dplyr, 
  tidyr, 
  ggplot2, 
  purrr, 
  stringr, 
  readr, 
  glob2rx,
  install = TRUE
)

# set root directories
proj_root <- ".../projects/abcd-projs/smri-pub-abcd/"
data_root <- file.path(proj_root, "results/")
plot_output_dir <- file.path(data_root, "plots/")


# --- 2. preparing data ---

create_plotting_key <- function(path, model_type) {
  
  # define model names based on whether it's 'timing' or 'tempo'
  main_model_name <- paste0("age_", model_type, "_main_model")
  int_model_name <- paste0("age_", model_type, "_int_model")
  
  # read and process the raw p-value data
  raw_pvals <- read_csv(path, show_col_types = FALSE) %>%
    mutate(
      effect = case_when(
        rowname == "s(age_scaled)" ~ "age",
        rowname == paste0(model_type, "_scaled") ~ "main",
        rowname == paste0("s(age_scaled):", model_type, "_scaled") ~ "int",
        TRUE ~ rowname
      )
    )
  
  pvals_wide <- raw_pvals %>%
    select(
      region, metric, sex, hemi, region.name, 
      overall_best_model = contains("overall_best_"), 
      effect, sig
    ) %>%
    pivot_wider(
      names_from = effect,
      values_from = sig,
      names_glue = "{effect}_sig"
    )
  
  plotting_key <- pvals_wide %>%
    mutate(
      plot_effect = case_when(
        # rule 1: if 'age' model is best, plot age if significant
        overall_best_model == "age_model" & age_sig == 1 ~ "age",
        
        # rule 2: if 'main' model is best, plot main effect if significant
        overall_best_model == main_model_name & main_sig == 1 ~ "main",
        
        # rule 3: if 'int' model is best, try to plot interaction first
        overall_best_model == int_model_name & int_sig == 1 ~ "int",
        
        # rule 4 (fallback): if int model is best but interaction is not significant,
        # plot the main effect instead (if it's significant)
        overall_best_model == int_model_name & (is.na(int_sig) | int_sig == 0) & main_sig == 1 ~ "main",
        
        # default case: do not plot
        TRUE ~ NA_character_
      ),
      model_group = model_type
    ) %>%
    filter(!is.na(plot_effect)) %>%
    rename(region_nice = region.name) %>%
    mutate(
      region_nice = str_to_title(region_nice),
      hemi = str_to_title(hemi)
    )
  
  return(plotting_key)
}

timing_key <- create_plotting_key(file.path(data_root, "cleaned/combat_timing_pvals.csv"), "timing")
tempo_key <- create_plotting_key(file.path(data_root, "cleaned/combat_tempo_pvals.csv"), "tempo")

final_plot_list <- bind_rows(timing_key, tempo_key)


# --- 3. plot generation ---

generate_plot <- function(...) {
  
  current_plot_info <- list(...)
  
  # --- setup variables from the current row ---
  region_name <- current_plot_info$region
  metric <- current_plot_info$metric
  sex <- current_plot_info$sex
  region_nice <- current_plot_info$region_nice
  hemi <- current_plot_info$hemi
  model_group <- current_plot_info$model_group
  plot_effect <- current_plot_info$plot_effect
  
  # map short metric names to full names and file prefixes
  full_metric <- case_when(
    metric == "vol"   ~ "volume",
    metric == "thick" ~ "thickness",
    metric == "area"  ~ "area"
  )
  region_prefix <- case_when(
    metric == "vol"   ~ "smri_vol_scs_",
    metric == "thick" ~ "smri_thick_cdk_",
    metric == "area"  ~ "smri_area_cdk_"
  )
  region_to_filter <- paste0(region_prefix, region_name)
  
  # --- load observational data to unscale age ---
  obs_file <- file.path(proj_root, "data", paste0("data_", sex, "_", full_metric, "_long.csv"))
  obs_plot_data <- read_csv(obs_file, show_col_types = FALSE) %>% 
    filter(region == region_to_filter)
  
  age_mean <- mean(obs_plot_data$age, na.rm = TRUE)
  age_sd <- sd(obs_plot_data$age, na.rm = TRUE)
  
  # --- plot logic based on the determined effect ---
  
  # age effect plot
  if (plot_effect == "age") {
    folder <- "age_effect/"
    pattern <- glob2rx(paste0("age_effect_", full_metric, "_", region_to_filter, "_", sex, "_combat.csv"))
    pred_file <- list.files(path = file.path(proj_root, folder), pattern = pattern, full.names = TRUE)
    
    if (length(pred_file) == 0) return() 
    
    pred_data <- read_csv(pred_file, show_col_types = FALSE) %>%
      rename(fit = value) %>%
      mutate(for_age_plot = (age_scaled * age_sd) + age_mean)
    
    plot_title <- paste(hemi, region_nice, "-", if_else(sex == "f", "Female", "Male"))
    
    p <- ggplot(pred_data, aes(x = for_age_plot, y = fit)) +
      geom_line(color = "#b7245c", linewidth = 1.2) +
      labs(title = plot_title, x = "Age (yrs)", y = str_to_title(full_metric))
    
    # main or interaction effect plot
  } else {
    folder <- if_else(plot_effect == "int", "int_effect/", "main_effect/")
    file_prefix <- if_else(plot_effect == "int", "int_effect_full_", "main_effect_")
    
    x_nice <- model_group
    x_var <- paste0(x_nice, "_scaled")
    
    pattern <- glob2rx(paste0(file_prefix, x_nice, "_", full_metric, "_", region_to_filter, "_", sex, "_combat.csv"))
    pred_file <- list.files(path = file.path(proj_root, folder), pattern = pattern, full.names = TRUE)
    
    if (length(pred_file) == 0) return() 
    
    # setup plot aesthetics based on model group
    if (model_group == "timing") {
      colors <- c("#800000", "#3DA35D", "#166088"); labels <- c("early", "average", "later"); linetypes <- c("dashed", "solid", "twodash"); x_lab <- "Timing"
    } else {
      colors <- c("#FC6DAB", "#3DA35D", "#241E4E"); labels <- c("slower", "average", "faster"); linetypes <- c("longdash", "solid", "dotdash"); x_lab <- "Tempo"
    }
    
    pred_data <- read_csv(pred_file, show_col_types = FALSE) %>%
      rename(fit = value) %>%
      mutate(
        cat = cut(
          .data[[x_var]],
          breaks = quantile(.data[[x_var]], probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
          labels = labels, 
          include.lowest = TRUE
        ),
        for_age_plot = (age_scaled * age_sd) + age_mean
      )
    
    plot_title <- paste(hemi, region_nice, "-", if_else(sex == "f", "Female", "Male"))
    
    p <- ggplot(pred_data, aes(x = for_age_plot, y = fit, color = cat, linetype = cat, group = cat)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = colors, labels = labels) +
      scale_linetype_manual(values = linetypes, labels = labels) +
      labs(title = plot_title, x = "Age (yrs)", y = str_to_title(full_metric), color = x_lab, linetype = x_lab)
  }
  
  # --- apply common theme and save the plot ---
  final_plot <- p +
    theme_minimal() +
    theme(
      text = element_text(family = "Times", color = "black", size = 22),
      axis.text = element_text(color = "black", size = 20),
      axis.title = element_text(color = "black", size = 22),
      plot.title = element_text(color = "black", family = "Times", size = 24, face = "bold")
    )
  
  output_sub_dir <- file.path(plot_output_dir, folder, model_group)
  if (!dir.exists(output_sub_dir)) {
    dir.create(output_sub_dir, recursive = TRUE)
  }
  
  # define a clean filename
  file_name <- paste0("ggplot_", model_group, "_", plot_effect, "_effect_", metric, "_", region_name, "_", sex, "_combat.png")
  
  ggsave(
    filename = file.path(output_sub_dir, file_name),
    plot = final_plot, 
    width = 7, height = 7, units = "in", dpi = 300, bg = "transparent"
  )
  
  cat("saved plot:", file_name, "\n")
}


# --- 4. execute the plotting process ---
pwalk(final_plot_list, .f = ~generate_plot(...))
