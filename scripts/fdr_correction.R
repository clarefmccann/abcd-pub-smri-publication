library(dplyr)
library(psych)

proj_root <- ".../projects/abcd-projs/smri-pub-abcd/"

options(scipen = 999)

p_f <- read.csv(paste0(proj_root, "results/cleaned/combat_combined_pvals.csv")) %>% 
  filter(sex == "f",
         !(region %in% c("suprateialv", "wholeb", "allventricles", "intracranialv", 
                         "latventricles", "subcorticalgv", "meanlh", "meanrh", 
                         "mean", "total", "totalrh", "totallh")))

p_m <- read.csv(paste0(proj_root, "results/cleaned/combat_combined_pvals.csv")) %>% 
  filter(sex == "m",
         !(region %in% c("suprateialv", "wholeb", "allventricles", "intracranialv", 
                                   "latventricles", "subcorticalgv", "meanlh", "meanrh", 
                                   "mean", "total", "totalrh", "totallh")))

p_f_thk <- p_f %>% 
  filter(metric == "thick")
p_f_vol <- p_f %>% 
   filter(metric == "vol")
p_f_area <- p_f %>% 
   filter(metric == "area")

p_m_thk <- p_m %>% 
  filter(metric == "thick")
p_m_vol <- p_m %>% 
  filter(metric == "vol")
p_m_area <- p_m %>% 
  filter(metric == "area")

dfs <- list(p_f_vol, p_m_vol, p_f_thk, p_m_thk, p_f_area, p_m_area)

for (i in seq_along(dfs)) {
  df <- dfs[[i]]
  
  df <- df %>% 
    group_by(model, rowname) %>%
    mutate(fdr.corrected.pval = p.adjust(p.value, method = "fdr")) %>% 
    ungroup() %>% 
    mutate(sig = ifelse(fdr.corrected.pval < 0.05, 1, 0),
           model = ifelse(model == "tempo_int", "age_tempo_int_model",
                          ifelse(model == "timing_int", "age_timing_int_model",
                                 ifelse(model == "timing_main", "age_timing_main_model",
                                        ifelse(model == "tempo_main", "age_tempo_main_model",
                                               ifelse(model == "age_model", "age_model",
                                                      model))))))
  
  dfs[[i]] <- df 
  
}

p_f_thk <- dfs[[1]]
p_m_thk <- dfs[[2]]
p_f_vol <- dfs[[3]]
p_m_vol <- dfs[[4]]
p_f_area <- dfs[[5]]
p_m_area <- dfs[[6]]

merged <- bind_rows(p_f_thk, p_m_thk, p_f_vol, p_m_vol, p_f_area, p_m_area)

best_fit_timing <- read.csv(paste0(proj_root, "results/cleaned/combat_best_fits.csv")) %>%
  filter(comparison %in% c("null_vs_timing_main", "timing_main_vs_timing_int", "null_vs_timing_int")) %>%
  tidyr::pivot_wider(
    names_from = comparison,
    values_from = best_model
  ) %>%
  mutate(across(c(null_vs_timing_main, timing_main_vs_timing_int, null_vs_timing_int), as.character)) %>%
  mutate(
    winner_null_vs_main = if_else(null_vs_timing_main == 1, "age_model", "age_timing_main_model"),
    winner_main_vs_int  = if_else(timing_main_vs_timing_int == 1, "age_timing_main_model", "age_timing_int_model"),
    winner_null_vs_int  = if_else(null_vs_timing_int == 1, "age_model", "age_timing_int_model")
  ) %>% 
  group_by(sex, metric, region, hemisphere) %>%
  summarise(
    overall_best_timing_model = case_when(
      winner_null_vs_main == "age_model" ~ winner_null_vs_int,
      winner_null_vs_main == "age_timing_main_model" & winner_main_vs_int == "age_timing_int_model" ~ "age_timing_int_model",
      winner_null_vs_main == "age_timing_main_model" & winner_main_vs_int == "age_timing_main_model" ~ "age_timing_main_model",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  )


best_fit_tempo <- read.csv(paste0(proj_root, "results/cleaned/combat_best_fits.csv")) %>%
  filter(comparison %in% c("null_vs_tempo_main", "tempo_main_vs_tempo_int", "null_vs_tempo_int")) %>%
  tidyr::pivot_wider(
    names_from = comparison,
    values_from = best_model
  ) %>%
  mutate(
    winner_null_vs_main = if_else(null_vs_tempo_main == 1, "age_model", "age_tempo_main_model"),
    winner_main_vs_int  = if_else(tempo_main_vs_tempo_int == 1, "age_tempo_main_model", "age_tempo_int_model"),
    winner_null_vs_int  = if_else(null_vs_tempo_int == 1, "age_model", "age_tempo_int_model")
  ) %>% 
  group_by(sex, metric, region, hemisphere) %>%
  summarise(
    overall_best_tempo_model = case_when(
      winner_null_vs_main == "age_model" ~ winner_null_vs_int,
      winner_null_vs_main == "age_tempo_main_model" & winner_main_vs_int == "age_tempo_int_model" ~ "age_tempo_int_model",
      winner_null_vs_main == "age_tempo_main_model" & winner_main_vs_int == "age_tempo_main_model" ~ "age_tempo_main_model",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  )
  

write.csv(merged, file = paste0(proj_root, "results/cleaned/combat_fdr_corrected_p.csv"))

timing_pvals <- merged %>% 
  select(rowname, model, sex, region, hemisphere, metric, est.or.edf, fdr.corrected.pval, sig) %>% 
  filter(rowname %in% c("s(age_scaled)","timing_scaled", "s(age_scaled):timing_scaled")) %>% 
  full_join(., best_fit_timing, by = c("sex", "metric", "region", "hemisphere")) %>% 
  group_by(sex, metric, region, hemisphere) %>% 
  filter(`overall_best_timing_model` == `model`)
#%>% 
  #filter(!(rowname == "timing_scaled" & model == "age_timing_int_model"))

model_count_timing <- timing_pvals %>%
  group_by(sex, metric) %>%
  distinct(region, .keep_all = TRUE) %>%
  count(overall_best_timing_model)

print(model_count_timing)

sig_count_timing <- timing_pvals %>%
  group_by(sex, metric, model) %>%
  distinct(region, .keep_all = TRUE) %>%
  count(sig) %>% 
  filter(sig == 1)

print(sig_count_timing)

tempo_pvals <- merged %>% 
  select(rowname, model, sex, region, hemisphere, metric, est.or.edf, fdr.corrected.pval, sig) %>% 
  filter(rowname %in% c("s(age_scaled)","tempo_scaled", "s(age_scaled):tempo_scaled")) %>% 
  full_join(., best_fit_tempo, by = c("sex", "metric", "region", "hemisphere")) %>% 
  group_by(sex, metric, region, hemisphere) %>%
  filter(`overall_best_tempo_model` == `model`) #%>%
  #filter(!(rowname == "tempo_scaled" & model == "age_tempo_int_model")) 

model_count_tempo <- tempo_pvals %>%
  group_by(sex, metric) %>%
  distinct(region, .keep_all = TRUE) %>%
  count(overall_best_tempo_model)

print(model_count_tempo)

sig_count_tempo <- tempo_pvals %>%
  group_by(sex, metric, model) %>%
  distinct(region, .keep_all = TRUE) %>%
  count(sig) %>% 
  filter(sig == 1)

print(sig_count_tempo)

labels <- read.csv(paste0(proj_root, "scripts/ggseg_label.csv")) 

timing_pvals <- left_join(timing_pvals, labels, by = "region")
tempo_pvals <- left_join(tempo_pvals, labels, by = "region")

write.csv(timing_pvals, file = paste0(proj_root, "results/cleaned/combat_timing_pvals.csv"))
write.csv(tempo_pvals, file = paste0(proj_root, "results/cleaned/combat_tempo_pvals.csv"))


          