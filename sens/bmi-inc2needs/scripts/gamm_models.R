
required_packages <- c("dplyr", "mgcv", "grid", "tidygam", "itsadug", "tibble")
new_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(new_packages)) install.packages(new_packages)

lapply(required_packages, library, character.only = TRUE)

#### UNCOMMENT IF ON HOFFMAN

root = ".../projects/smri-pub-abcd/"

args <- commandArgs(trailingOnly = TRUE)
brain <- args[1]
sex <- args[2]
region_name <- args[3]

print("--------------------NEXT REGION------------------------")
print(paste("brain metric:", brain))
print(paste("sex:", sex))
print(paste("region:", region_name))

# files 
age_csv_file <- paste0(root, "data/data_", sex, "_", brain, "_combat_long_age_model.csv")
timing_main_csv_file <- paste0(root, "data/data_", sex, "_", brain, "_combat_long_age_timing_model.csv")
tempo_main_csv_file <- paste0(root, "data/data_", sex, "_", brain, "_combat_long_age_tempo_model.csv")
timing_int_csv_file <- paste0(root, "data/data_", sex, "_", brain, "_combat_long_age_timing_interaction_model.csv")
tempo_int_csv_file <- paste0(root, "data/data_", sex, "_", brain, "_combat_long_age_tempo_interaction_model.csv")


print(paste("loading in", age_csv_file, "with", brain, "as its metric"))
age_data <- read.csv(age_csv_file) %>%
  filter(pipeline == "combat") %>%
  rename("timing_scaled" = "timing_parent_scaled",
         "tempo_scaled" = "tempo_parent_scaled") %>% 
  mutate(id = as.factor(id),
         site_id_l = as.factor(site_id_l)) %>%
  filter(!site_id_l == "site22",
         !is.na(bmi),
         !is.na(inc_to_need)) 

timing_main_data <- read.csv(timing_main_csv_file) %>%
  filter(pipeline == "combat") %>%
  rename("timing_scaled" = "timing_parent_scaled",
         "tempo_scaled" = "tempo_parent_scaled") %>% 
  mutate(id = as.factor(id),
         site_id_l = as.factor(site_id_l)) %>%
  filter(!site_id_l == "site22",
         !is.na(bmi),
         !is.na(inc_to_need))

tempo_main_data <- read.csv(tempo_main_csv_file) %>%
  filter(pipeline == "combat") %>%
  rename("timing_scaled" = "timing_parent_scaled",
         "tempo_scaled" = "tempo_parent_scaled") %>% 
  mutate(id = as.factor(id),
         site_id_l = as.factor(site_id_l)) %>%
  filter(!site_id_l == "site22",
         !is.na(bmi),
         !is.na(inc_to_need)) 

timing_int_data <- read.csv(timing_int_csv_file) %>%
  filter(pipeline == "combat") %>%
  rename("timing_scaled" = "timing_parent_scaled",
         "tempo_scaled" = "tempo_parent_scaled") %>% 
  mutate(id = as.factor(id),
         site_id_l = as.factor(site_id_l)) %>%
  filter(!site_id_l == "site22",
         !is.na(bmi),
         !is.na(inc_to_need)) 

tempo_int_data <- read.csv(tempo_int_csv_file) %>%
  filter(pipeline == "combat") %>%
  rename("timing_scaled" = "timing_parent_scaled",
         "tempo_scaled" = "tempo_parent_scaled") %>% 
  mutate(id = as.factor(id),
         site_id_l = as.factor(site_id_l)) %>%
  filter(!site_id_l == "site22",
         !is.na(bmi),
         !is.na(inc_to_need))


# change contrast to treatment coding (difference curves)
contrasts(age_data$site_id_l) <- 'contr.treatment'
contrasts(timing_main_data$site_id_l) <- 'contr.treatment'
contrasts(tempo_main_data$site_id_l) <- 'contr.treatment'
contrasts(timing_int_data$site_id_l) <- 'contr.treatment'
contrasts(tempo_int_data$site_id_l) <- 'contr.treatment'

# Inspect contrasts:
#contrasts(data$site_id_l)

print(head(age_data))

# empty list to store best models for each region
all_results <- data.frame()

# for each region within the brain metric
age_data_region <- age_data %>% filter(region == region_name)
timing_main_data_region <- timing_main_data %>% filter(region == region_name) 
tempo_main_data_region <- tempo_main_data %>% filter(region == region_name) 
timing_int_data_region <- timing_int_data %>% filter(region == region_name) 
tempo_int_data_region <- tempo_int_data %>% filter(region == region_name) 


print(paste("processing region:", region_name))

formula_age <- as.formula(paste("value ~ s(age_scaled, bs = 'cs', k = 7) + site_id_l + bmi + inc_to_need"))
formula_timing_main <- as.formula(paste("value ~ timing_scaled + s(age_scaled, bs = 'cs', k = 7) + site_id_l + bmi + inc_to_need"))
formula_tempo_main <- as.formula(paste("value ~  tempo_scaled + s(age_scaled, bs = 'cs', k = 7) + site_id_l + bmi + inc_to_need"))
formula_timing_int <- as.formula(paste("value ~ timing_scaled + s(age_scaled, bs = 'cs', k = 7) + s(age_scaled, by = timing_scaled, k = 7, bs = 'cs') + site_id_l + bmi + inc_to_need"))
formula_tempo_int <- as.formula(paste("value ~ tempo_scaled + s(age_scaled, bs = 'cs', k = 7) + s(age_scaled, by = tempo_scaled, k = 7, bs = 'cs') + site_id_l + bmi + inc_to_need"))

# --- model fitting ---

# age --> brain
print(paste("fitting just age --> brain model for", region_name))
age_model <- gamm(formula_age,
                  data = age_data_region,
                  random = list(id = ~1))

# timing model
print(paste("fitting age timing model for", region_name))
age_timing_main_model <- gamm(formula_timing_main,
                              data = timing_main_data_region,
                              random = list(id = ~1))

# tempo model
print(paste("fitting tempo model for", region_name))
age_tempo_main_model <- gamm(formula_tempo_main,
                             data = tempo_main_data_region,
                             random = list(id = ~1))

# age * timing model
print(paste("fitting age * timing model for", region_name))
age_timing_int_model <- gamm(formula_timing_int,
                             data = timing_int_data_region,
                             random = list(id = ~1))

# age * tempo model
print(paste("fitting age * tempo model for", region_name))
age_tempo_int_model <- gamm(formula_tempo_int,
                            data = tempo_int_data_region,
                            random = list(id = ~1))


print(paste("saving model ouput for", region_name, "for", sex))

# --- summaries ---
interpret_gam_model <- function(model, model_name) {
  cat("\n---", model_name, "---\n")
  
  plot(model$gam, pages = 1, shade = TRUE, main = paste("smooth terms -", model_name))
  gam.check(model$gam, plot = FALSE)
  
  if (model_name == "age * timing interaction model") {
    view_vars <- c("age_scaled", "timing_scaled")
  } else if (model_name == "age * tempo interaction model") {
    view_vars <- c("age_scaled", "tempo_scaled")
  }
  
  if (exists("view_vars")) {
    vis.gam(model$gam,
            view = view_vars,
            theta = 30,
            type = "response")
  }
}

#pdf_filename <- paste0(root, "model_dx/model_check_plots_",region_name,"_",sex,"_combat.pdf")
#pdf(pdf_filename, width = 10, height = 8)

interpret_gam_model(age_model, "age model")
interpret_gam_model(age_timing_main_model, "age + timing model")
interpret_gam_model(age_tempo_main_model, "age + tempo model")
interpret_gam_model(age_timing_int_model, "age * timing interaction model")
interpret_gam_model(age_tempo_int_model, "age * tempo interaction model")
#dev.off()

#save interaction model
print(paste("saving summary info for tempo int model for", region_name, "--", sex))
ptable_tempo_int <- as.data.frame(summary(age_tempo_int_model$gam)$p.table) %>% tibble::rownames_to_column()
stable_tempo_int <- as.data.frame(summary(age_tempo_int_model$gam)$s.table) %>% tibble::rownames_to_column()

print(paste("saving summary info for timing int model for", region_name, "--", sex))
ptable_timing_int <- as.data.frame(summary(age_timing_int_model$gam)$p.table) %>% tibble::rownames_to_column()
stable_timing_int <- as.data.frame(summary(age_timing_int_model$gam)$s.table) %>% tibble::rownames_to_column()

#save main model
print(paste("saving summary info for tempo main model for", region_name, "--", sex))
ptable_tempo_main <- as.data.frame(summary(age_tempo_main_model$gam)$p.table) %>% tibble::rownames_to_column()
stable_tempo_main <- as.data.frame(summary(age_tempo_main_model$gam)$s.table) %>% tibble::rownames_to_column()

print(paste("saving model timing main model for", region_name, "--", sex))
ptable_timing_main <- as.data.frame(summary(age_timing_main_model$gam)$p.table) %>% tibble::rownames_to_column()
stable_timing_main <- as.data.frame(summary(age_timing_main_model$gam)$s.table) %>% tibble::rownames_to_column()

print(paste("saving null model for", region_name, "--", sex))
#save null model
ptable_age_model <- as.data.frame(summary(age_model$gam)$p.table) %>% tibble::rownames_to_column()
stable_age_model <- as.data.frame(summary(age_model$gam)$s.table) %>% tibble::rownames_to_column()

print(paste("saving model comparisons for", region_name, "--", sex))

#compare null and smooth models

print(paste("mod comp timing - null vs smooths for",  brain, "for", region_name, "--", sex))

null_timing_test <- anova(age_model$lme,age_timing_main_model$lme)
print(null_timing_test)

print(paste("mod comp timing - main vs int for",  brain, "for", region_name, "--", sex))

timing_test <- anova(age_timing_main_model$lme, age_timing_int_model$lme)
print(timing_test)

print(paste("mod comp timing - null vs int for",  brain, "for", region_name, "--", sex))

null_int_timing_test <- anova(age_model$lme, age_timing_int_model$lme)
print(null_int_timing_test)

print(paste("mod comp tempo - null vs smooths for",  brain, "for", region_name, "--", sex))

#compare null and smooth models
null_tempo_test <- anova(age_model$lme,age_tempo_main_model$lme)
print(null_tempo_test)

print(paste("mod comp tempo - main vs int for",  brain, "for", region_name, "--", sex))

tempotest <- anova(age_tempo_main_model$lme,age_tempo_int_model$lme)
tempotable <- tempotest$table

print(tempotest)

print(paste("mod comp tempo - null vs int for",  brain, "for", region_name, "--", sex))

null_int_tempo_test <- anova(age_model$lme,age_tempo_int_model$lme)
print(null_int_tempo_test)

print(paste("organizing results for",  brain, "for", region_name, "--", sex))

ptable_tempo_int <- ptable_tempo_int %>% mutate(model = "tempo_int", table_type = "p.table")
stable_tempo_int <- stable_tempo_int %>% mutate(model = "tempo_int", table_type = "s.table")

ptable_timing_int <- ptable_timing_int %>% mutate(model = "timing_int", table_type = "p.table")
stable_timing_int <- stable_timing_int %>% mutate(model = "timing_int", table_type = "s.table")

ptable_tempo_main <- ptable_tempo_main %>% mutate(model = "tempo_main", table_type = "p.table")
stable_tempo_main <- stable_tempo_main %>% mutate(model = "tempo_main", table_type = "s.table")

ptable_timing_main <- ptable_timing_main %>% mutate(model = "timing_main", table_type = "p.table")
stable_timing_main <- stable_timing_main %>% mutate(model = "timing_main", table_type = "s.table")

ptable_age_model <- ptable_age_model %>% mutate(model = "age_model", table_type = "p.table")
stable_age_model <- stable_age_model %>% mutate(model = "age_model", table_type = "s.table")

# combine null model comparisons
null_timing_test <- null_timing_test %>% mutate(model = "null_vs_timing_main")
timing_test <- timing_test %>% mutate(model = "timing_main_vs_timing_int")
null_int_timing_test <- null_int_timing_test %>% mutate(model = "null_vs_timing_int")


null_tempo_test <- null_tempo_test %>% mutate(model = "null_vs_tempo_main")
tempotest <- tempotest %>% mutate(model = "tempo_main_vs_tempo_int")
null_int_tempo_test <- null_int_tempo_test %>% mutate(model = "null_vs_tempo_int")

get_gam_fit_stats_final <- function(m, model_name) {
  s <- summary(m$gam)
  lme_obj <- m$lme
  gam_obj <- m$gam
  
  r2 <- suppressWarnings(tryCatch(
    MuMIn::r.squaredGLMM(lme_obj),
    error = function(e) NA
  ))
  
  tibble::tibble(
    model = model_name,
    table_type = "fit.stats",
    r2_adj_simple = if (!is.null(s$r.sq)) unname(s$r.sq) else NA_real_,
    r2m_marginal = if (is.matrix(r2)) r2[1, "R2m"] else NA_real_,
    r2c_conditional = if (is.matrix(r2)) r2[1, "R2c"] else NA_real_,

    dev_expl = if (length(s$dev.expl) > 0) unname(s$dev.expl) else NA_real_,
    
    n        = unname(s$n),
    edf_total = if (!is.null(s$edf)) sum(s$edf, na.rm = TRUE) else NA_real_,
    scale    = unname(s$scale),
    logLik   = suppressWarnings(tryCatch(as.numeric(stats::logLik(lme_obj)), error = function(e) NA_real_)),
    AIC      = suppressWarnings(tryCatch(stats::AIC(lme_obj), error = function(e) NA_real_)),
    BIC      = suppressWarnings(tryCatch(stats::BIC(lme_obj), error = function(e) NA_real_)),
    family   = gam_obj$family$family,
    method   = if (!is.null(gam_obj$method)) gam_obj$method else NA_character_
  )
}

fit_stats <- dplyr::bind_rows(
  get_gam_fit_stats_final(age_model,              "age_model"),
  get_gam_fit_stats_final(age_timing_main_model,  "timing_main"),
  get_gam_fit_stats_final(age_tempo_main_model,   "tempo_main"),
  get_gam_fit_stats_final(age_timing_int_model,   "timing_int"),
  get_gam_fit_stats_final(age_tempo_int_model,    "tempo_int")
)


all_results <- bind_rows(
  ptable_tempo_int, stable_tempo_int,
  ptable_timing_int, stable_timing_int,
  ptable_tempo_main, stable_tempo_main,
  ptable_timing_main, stable_timing_main,
  ptable_age_model, stable_age_model,
  null_timing_test, timing_test, null_int_timing_test,
  null_tempo_test, tempotest, null_int_tempo_test,
  fit_stats
)

print(paste("outputing csv of results for", region_name, "--", sex))

all_results_csv <- paste0(root, "sens/bmi-inc2needs/results/all_results_", region_name, "_", sex, "_combat.csv")
print(paste("saving timing all results for", region_name, "to", all_results_csv))
write.csv(all_results, file = all_results_csv, row.names = FALSE)

print("done! :)")

print("--------------------------------------------")


