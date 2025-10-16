### cleaning result csvs 

library(pacman)

pacman::p_load(dplyr, tidyverse, ggplot2, tidyr, lubridate, psych, readr, purrr, data.table, install = TRUE)

proj_root = ".../projects/abcd-projs/smri-pub-abcd/"
data_root = ".../projects/abcd-projs/smri-pub-abcd/results/"

files <- list.files(data_root, pattern = "\\_combat.csv$", full.names = TRUE)

options(scipen = 999)

process_df <- function(df, df_name) {
  df <- df %>%
    mutate(filename = df_name) %>% 
    mutate(
      metric = str_extract(filename, "vol|area|thick"), 
      region = str_replace(filename, ".*_smri_\\w+_\\w+_([^_]+)(rh|lh)?_(m|f)$", "\\1"),
      sex = str_extract(filename, "[mf]$"),
      hemisphere = str_extract(filename, "lh|rh") 
    )
  return(df)
}

for (file in files) {
  df_name <- str_remove(basename(file), "\\_combat.csv$")
  
  df <- read_csv(file) 
  
  df <- process_df(df, df_name)
  
  assign(df_name, df, envir = .GlobalEnv)
}

dfs <- ls(pattern = "^all_results_")

for (df_name in dfs) {
  
  df <- get(df_name)
  
  new_df_name_fit <- paste0("model_fit_", df_name)
  new_df_name_pvals <- paste0("pvals_", df_name)
  

  if ("Sign." %in% colnames(df) && !"Sig." %in% colnames(df)) {
    df <- df %>% rename(Sig. = "Sign.")
  }
  
  new_df_fit <- df %>% 
    filter(model %in% c("null_vs_timing_main", "timing_main_vs_timing_int", "null_vs_timing_int", "null_vs_tempo_main", "tempo_main_vs_tempo_int", "null_vs_tempo_int")) %>%
    group_by(model, metric, region, sex, hemisphere) %>%
    summarise(
      model_simple = first(Model),
      model_complex = last(Model),
      p.value = last(`p-value`), 
      L.Ratio = last(`L.Ratio`),
      .groups = "drop" 
    ) %>%
    rename("comparison" = "model")
  
  new_df_fit <- new_df_fit %>%
    mutate(
      best_model = if_else(p.value < 0.05, model_complex, model_simple),
      Sig. = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".",
        TRUE            ~ ""
      )
    )
  
  assign(new_df_name_fit, new_df_fit)
  

    # warnings <- read.csv(paste0(proj_root, "error/model_comparison_warnings.csv")) %>%
    #   mutate(comparison = ifelse(comparison == "age_model vs age_tempo_main_model", "null_vs_tempo_main",
    #                              ifelse(comparison == "age_tempo_main_model vs age_tempo_int_model", "tempo_main_vs_tempo_int",
    #                                     ifelse(comparison == "age_model vs age_tempo_int_model", "null_vs_tempo_int",
    #                                            ifelse(comparison == "age_model vs age_timing_main_model", "null_vs_timing_main",
    #                                                   ifelse(comparison == "age_timing_main_model vs age_timing_int_model", "timing_main_vs_timing_int",
    #                                                          ifelse(comparison == "age_model vs age_timing_int_model", "null_vs_timing_int",
    #                                                                 comparison))))))) %>% 
    #   mutate(warnings = 1)
    # 
    # new_df_fit <- left_join(new_df_fit, warnings, by = c("region", "sex", "hemisphere", "metric", "comparison")) %>%
    #   mutate(
    #     warnings = ifelse(is.na(warnings), 0, warnings)
    #   )
    
    
  # new_df_aic <- df %>%
  #   dplyr::select(model, AIC) %>%
  #   filter(!is.na(AIC)) %>%
  #   mutate(model = ifelse(model == "timing_main", "age_timing_main_model",
  #                         ifelse(model == "tempo_main", "age_tempo_main_model",
  #                                ifelse(model == "timing_int", "age_timing_int_model",
  #                                       ifelse(model == "tempo_int", "age_tempo_int_model",
  #                                              ifelse(model == "age_model", "age_model",
  #                                              model)))))) %>%
  #   rename("Model" = "model")
  
  new_df_pvals <- df %>% 
    filter(rowname %in% c("s(age_scaled)", "timing_scaled", "tempo_scaled", "s(age_scaled):timing_scaled", "s(age_scaled):tempo_scaled")) %>% 
    mutate(`Pr(>|t|)` = ifelse(is.na(`Pr(>|t|)`), `p-value`, `Pr(>|t|)`),
           Estimate = ifelse(is.na(Estimate), edf, Estimate),
           `Std. Error` = ifelse(is.na(`Std. Error`), Ref.df, `Std. Error`),
           `t value` = ifelse(is.na(`t value`), `F`, `t value`)) %>% 
    select(rowname, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`, region, hemisphere, model, sex, metric) %>% 
    rename("p-value" = "Pr(>|t|)",
           "est-or-edf" = "Estimate",
           "std.err-or-ref.df" = "Std. Error",
           "t.val-or-F" = "t value")
  
  assign(new_df_name_pvals, new_df_pvals)
    
}


model_fit_df_names <- ls(pattern = "^model_fit", envir = .GlobalEnv)
model_fit_df_names <- setdiff(model_fit_df_names, c("model_fit_combined_df", "model_fit_df_names"))

model_fit_combined_df <- model_fit_df_names %>%
  set_names() %>% 
  map_dfr(~ get(., envir = .GlobalEnv), .id = "source")

write.csv(model_fit_combined_df, file = paste0(proj_root, "results/cleaned/combat_combined_model_fit_results.csv"), row.names = FALSE)

best_fit_df <- model_fit_combined_df %>%
  group_by(metric, region, sex, hemisphere) %>% 
  distinct(comparison, .keep_all = TRUE) %>% 
  select(region, sex, metric, hemisphere, best_model, comparison) %>% 
  ungroup()

best_timing_models <- model_fit_combined_df %>%
  filter(grepl("timing", comparison)) %>%
  pivot_wider(
    id_cols = c(metric, region, sex, hemisphere),
    names_from = comparison,
    values_from = p.value
  ) %>%
  mutate(
    best_timing_model = case_when(
      timing_main_vs_timing_int < 0.05 ~ "age_timing_int_model",
      timing_main_vs_timing_int >= 0.05 & null_vs_timing_main < 0.05 ~ "age_timing_main_model",
      TRUE ~ "age_model"
    )
  ) %>%
  select(metric, region, sex, hemisphere, best_timing_model)

# --- select best tempo model ---

best_tempo_models <- model_fit_combined_df %>%
  filter(grepl("tempo", comparison)) %>%
  pivot_wider(
    id_cols = c(metric, region, sex, hemisphere),
    names_from = comparison,
    values_from = p.value
  ) %>%
  mutate(
    best_tempo_model = case_when(
      tempo_main_vs_tempo_int < 0.05 ~ "age_tempo_int_model",
      tempo_main_vs_tempo_int >= 0.05 & null_vs_tempo_main < 0.05 ~ "age_tempo_main_model",
      TRUE ~ "age_model"
    )
  ) %>%
  select(metric, region, sex, hemisphere, best_tempo_model)

# --- join the results into a final table ---

final_best_models <- full_join(
  best_timing_models,
  best_tempo_models,
  by = c("metric", "region", "sex", "hemisphere")
)


write.csv(best_fit_df, file = paste0(proj_root, "results/cleaned/combat_best_fits.csv"), row.names = FALSE)


pvals_df_names <- ls(pattern = "^pvals", envir = .GlobalEnv)

pvals_combined_df <- pvals_df_names %>%
  set_names() %>% 
  map_dfr(~ get(., envir = .GlobalEnv), .id = "source") 

write.csv(pvals_combined_df, file = paste0(proj_root, "results/cleaned/combat_combined_pvals.csv"), row.names = FALSE)





