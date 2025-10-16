# ---
# compare sensitivity analyses 
#
# this script compares results between the focal models and models including bmi and income-to-needs ration as control variables 
# separately for timing and tempo models.
# ---

# 1. setup: load packages and define file structure
# -----------------------------------------------------------------------------
pacman::p_load(dplyr, tidyr, purrr, readr, stringr, install = TRUE)

proj_root <- ".../projects/abcd-projs/smri-pub-abcd/"

# define file paths in a structured data frame
pipeline_df <- tibble::tribble(
  ~pipeline_name,     ~model_type, ~file_path,
  "focal",         "timing",    paste0(proj_root, "results/cleaned/combat_timing_pvals.csv"),
  "focal",         "tempo",     paste0(proj_root, "results/cleaned/combat_tempo_pvals.csv"),
  "sens",           "timing",    paste0(proj_root, "sens/bmi-inc2needs/results/cleaned/combat_timing_pvals.csv"),
  "sens",           "tempo",     paste0(proj_root, "sens/bmi-inc2needs/results/cleaned/combat_tempo_pvals.csv")
)

col_map <- list(
  term = "rowname",
  region = "region.name",
  metric = "metric",
  sex = "sex",
  eoi_significance = "sig",
  eoi_value = "est.or.edf"
)

# 2. define the main comparison function
# -----------------------------------------------------------------------------
run_comparison <- function(model_type_to_run, files_df) {
  
  cat("\n==================================================================\n")
  cat("  RUNNING COMPARISON FOR:", toupper(model_type_to_run), "MODELS\n")
  cat("==================================================================\n")
  
  # --- a. load data for the specified model type ---
  files_to_load <- files_df %>%
    filter(model_type == model_type_to_run)
  
  # get pipeline names for later use
  pipeline_names <- files_to_load$pipeline_name
  
  file_list <- setNames(files_to_load$file_path, files_to_load$pipeline_name)
  all_results <- imap_dfr(file_list, ~ read_csv(.x, show_col_types = FALSE) %>% mutate(pipeline = .y))
  
  cat("data loaded successfully for", model_type_to_run, "pipelines.\n")
  
  # --- b. prepare and augment data ---
  results_prepared <- all_results %>%
    mutate(
      best_model = if (model_type_to_run == "timing") overall_best_timing_model else overall_best_tempo_model
    ) %>%
    select(-any_of(c("region", "...1", "overall_best_timing_model", "overall_best_tempo_model"))) %>%
    rename(
      term = !!col_map$term,
      region = !!col_map$region,
      metric = !!col_map$metric,
      sex = !!col_map$sex,
      is_significant_val = !!col_map$eoi_significance,
      value = !!col_map$eoi_value
    ) %>%
    mutate(
      model_type = model_type_to_run,
      is_significant = as.logical(is_significant_val),
      standardized_term = case_when(
        str_detect(term, "timing_c|timing_s") & !str_detect(term, ":") ~ "timing_main_effect",
        str_detect(term, "tempo_c|tempo_s") & !str_detect(term, ":")  ~ "tempo_main_effect",
        str_detect(term, "s\\(age_c|s\\(age_s") & !str_detect(term, ":") ~ "age_smooth_effect",
        str_detect(term, ":") & str_detect(term, "timing")           ~ "age_by_timing_interaction",
        str_detect(term, ":") & str_detect(term, "tempo")            ~ "age_by_tempo_interaction",
        TRUE                                                         ~ term 
      )
    )
  
  # --- c. find and summarize discrepancies ---
  model_level_id_vars <- c("region", "metric", "sex", "hemi")
  term_level_id_vars <- c("standardized_term", "region", "metric", "sex", "hemi")
  
  # model disagreements 
  model_disagreements <- results_prepared %>%
    select(all_of(model_level_id_vars), pipeline, best_model) %>%
    distinct() %>%
    group_by(across(all_of(model_level_id_vars))) %>%
    summarise(
      n_distinct_models = n_distinct(best_model),
      models_found = paste(pipeline, best_model, sep = ": ", collapse = " | "),
      .groups = "drop"
    ) %>%
    filter(n_distinct_models > 1) %>%
    select(-n_distinct_models)
  
  # filter to only include terms from the best model before pivoting
  results_best_model_only <- results_prepared %>%
    filter(model == best_model)
  
  # pivot data for robust term-level comparisons
  pivoted_terms <- results_best_model_only %>%
    select(all_of(term_level_id_vars), pipeline, is_significant, value) %>%
    pivot_wider(
      id_cols = all_of(term_level_id_vars),
      names_from = pipeline,
      values_from = c(is_significant, value)
    )
  
  # --- MODIFICATION START: generalized discrepancy logic ---
  
  # find significance disagreements
  significance_disagreements <- pivoted_terms %>%
    rowwise() %>%
    mutate(
      # count the number of unique significance values (true, false, na)
      n_distinct_sig = n_distinct(c_across(starts_with("is_significant_")))
    ) %>%
    ungroup() %>%
    filter(n_distinct_sig > 1) %>%
    # create a summary string showing all pipeline statuses
    mutate(
      discrepancy = pmap_chr(
        select(., starts_with("is_significant_")),
        function(...) {
          vals <- c(...)
          pipeline_names_from_cols <- stringr::str_remove(names(vals), "is_significant_")
          paste(
            paste0(
              pipeline_names_from_cols, ": ",
              if_else(is.na(vals), "MISSING", as.character(vals))
            ),
            collapse = " | "
          )
        }
      )
    ) %>%
    select(all_of(term_level_id_vars), discrepancy)
  
  # find value disagreements
  value_disagreements <- pivoted_terms %>%
    rowwise() %>%
    mutate(
      all_values = list(c_across(starts_with("value_"))),
      n_present = sum(!is.na(all_values[[1]])),
      # calculate the range (max - min) of values to quantify the disagreement
      value_range = if (n_present > 1) {
        max(all_values[[1]], na.rm = TRUE) - min(all_values[[1]], na.rm = TRUE)
      } else {
        0
      }
    ) %>%
    ungroup() %>%
    # filter for disagreements: a meaningful range or a mismatch in missing values
    filter(abs(value_range) > 1e-6 | (n_present > 0 & n_present < length(pipeline_names))) %>%
    arrange(desc(value_range)) %>%
    select(
      all_of(term_level_id_vars),
      value_range,
      starts_with("value_")
    )
  
  # --- MODIFICATION END ---
  
  # --- d. view and save results ---
  cat("\n---", toupper(model_type_to_run), "Comparison Summary ---\n")
  cat("Found", nrow(model_disagreements), "disagreements in best model fit.\n")
  cat("Found", nrow(significance_disagreements), "disagreements in significance.\n")
  cat("Found", nrow(value_disagreements), "disagreements in estimate/EDF values.\n")
  
  
  print_heading <- function(title) {
    cat("\n--------------------------------------------------\n")
    cat(title, "\n")
    cat("--------------------------------------------------\n")
  }
  
  print_heading(paste("Top Model Disagreements for", model_type_to_run))
  print(head(model_disagreements, 10))
  
  print_heading(paste("Top Significance Disagreements for", model_type_to_run))
  print(head(significance_disagreements, 10))
  
  print_heading(paste("Top Value Disagreements for", model_type_to_run))
  print(head(value_disagreements, 10))
  
  # save the full disagreement reports to csv files
  write.csv(model_disagreements, paste0(proj_root, "results/cleaned/", model_type_to_run, "_pipeline_model_disagreements_bmi-inc2needs.csv"))
  write.csv(significance_disagreements, paste0(proj_root, "results/cleaned/", model_type_to_run, "_pipeline_sig_disagreements_bmi-inc2needs.csv"))
  write.csv(value_disagreements, paste0(proj_root, "results/cleaned/", model_type_to_run, "_pipeline_est_disagreements_bmi-inc2needs.csv"))
}

# 3. execute the comparisons
# -----------------------------------------------------------------------------
run_comparison("timing", pipeline_df)
run_comparison("tempo", pipeline_df)

cat("\n script finished.\n")