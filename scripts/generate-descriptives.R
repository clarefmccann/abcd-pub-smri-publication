data_f_area_long <- read.csv(paste0(proj_root, "data/data_f_area_combat_long_age_model_sens.csv")) %>% 
  mutate(sex = 1)
data_m_area_long <- read.csv(paste0(proj_root, "data/data_m_area_combat_long_age_model_sens.csv")) %>% 
  mutate(sex = 0)

included_ids <- bind_rows(
  data_f_area_long %>% distinct(id) %>% mutate(sex = "female"),
  data_m_area_long %>% distinct(id) %>% mutate(sex = "male")
)

# load all raw data files
demo_raw <- read.csv(paste0(data_root, "abcd-general/abcd_p_demo.csv"))
age_raw <- read.csv(paste0(data_root, "abcd-general/abcd_y_lt.csv"))
bmi_raw <- read.csv(".../projects/abcd-projs/puberty/bmi-abcd.csv")
inc2needs_f <- read.csv(".../projects/abcd-projs/income-to-needs/income-to-needs-pub-smri_f.csv")
inc2needs_m <- read.csv(".../projects/abcd-projs/income-to-needs/income-to-needs-pub-smri_m.csv")


# --- 2. process static (once per person) variables ---

process_race_ethnicity <- function(raw_data) {
  
  # select and prepare all relevant columns from the raw file
  race_data <- raw_data %>%
    select(
      "id"= "src_subject_id",
      "white" = "demo_prnt_race_a_v2___10",
      "black" = "demo_prnt_race_a_v2___11",
      "aina" = "demo_prnt_race_a_v2___12",
      "an" = "demo_prnt_race_a_v2___13",
      "nh" = "demo_prnt_race_a_v2___14",
      "guam" = "demo_prnt_race_a_v2___15",
      "samoan" = "demo_prnt_race_a_v2___16",
      "pin" = "demo_prnt_race_a_v2___17",
      "indian" = "demo_prnt_race_a_v2___18",
      "chinese" = "demo_prnt_race_a_v2___19",
      "filipino" = "demo_prnt_race_a_v2___20",
      "japanese" = "demo_prnt_race_a_v2___21",
      "korean" = "demo_prnt_race_a_v2___22",
      "vietnamese" = "demo_prnt_race_a_v2___23",
      "asian_other" = "demo_prnt_race_a_v2___24",
      "other" = "demo_prnt_race_a_v2___25",
      "refuse_race" = "demo_prnt_race_a_v2___77",
      "dont_know_race" = "demo_prnt_race_a_v2___99",
      "ethn" = "demo_ethn_v2"
    ) %>% 
    mutate(ethn = ifelse(ethn == 1, 1,
                         ifelse(ethn == 2, 0,
                                ethn)))
  
  race_data <- race_data %>%
    mutate(across(-id, as.numeric))
  
  str(race_data)

  # create the final race/ethnicity variable
  processed_data <- race_data %>%
    mutate(
      race_count = rowSums(select(., ethn, white, black, aina, pin, asian_other, indian, chinese, filipino, chinese, japanese, vietnamese, korean, samoan, guam, nh, other), na.rm = TRUE)
    ) %>%
    mutate(
      race_ethnicity = case_when(
        race_count > 1 ~ "Multiracial",
        ethn == 1 ~ "Latine",
        white == 1 ~ "Non-Latine White",
        black == 1 ~ "Non-Latine Black or African American",
        asian_other == 1 ~ "Non-Latine Asian (not further specified)",
        aina == 1 ~ "Non-Latine Native American or Alaskan Native",
        pin == 1 ~ "Non-Latine Pacific Islander",
        korean == 1 ~ "Non-Latine Korean",
        vietnamese == 1 ~ "Non-Latine Vietnamese",
        japanese == 1 ~ "Non-Latine Japanese",
        filipino == 1 ~ "Non-Latine Filipino",
        chinese == 1 ~ "Non-Latine Chinese",
        guam == 1 ~ "Non-Latine Guamanian",
        samoan == 1 ~ "Non-Latine Samoan",
        indian == 1 ~ "Non-Latine Indian",
        other == 1 ~ "Non-Latine Other",
        TRUE ~ "Unknown or Not Reported"
      )
    ) %>%
    select(id, race_ethnicity)
  
  return(processed_data)
}

demographics <- demo_raw %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  process_race_ethnicity()

# --- 3. process longitudinal (multi-wave) variables ---

age_long <- age_raw %>%
  transmute(id = src_subject_id, wave = eventname, age = interview_age / 12)

income_needs_long <- bind_rows(inc2needs_f, inc2needs_m)

# --- 4. prepare data for summaries ---

# for counts (n), we use a subject-level data frame (1 row per person)
subject_level_data <- included_ids %>%
  left_join(demographics, by = "id")

# for mean/sd, we use an observation-level data frame (multiple rows per person)
observation_level_data <- included_ids %>%
  left_join(age_long, by = "id") %>%
  left_join(bmi_raw, by = c("id", "wave")) %>%
  left_join(income_needs_long, by = "id")


final_sample_data <- included_ids %>%
  left_join(demographics, by = "id") %>%
  left_join(age_long, by = "id") %>%
  left_join(bmi_raw, by = c("id", "wave")) %>%
  left_join(income_needs_long, by = "id")


# --- 4. generate the summary table by sex ---

# summarize categorical variables
race_summary <- final_sample_data %>%
  distinct(id, .keep_all = TRUE) %>% # count each person only once
  group_by(sex) %>%
  count(race_ethnicity) %>%
  mutate(
    stat = "n (%)",
    value = paste0(n, " (", round(n / sum(n) * 100, 1), "%)")
  ) %>%
  ungroup() %>%
  select(variable = race_ethnicity, sex, stat, value)

# summarize continuous variables
continuous_summary <- final_sample_data %>%
  pivot_longer(
    cols = c(age, bmi, inc_to_need),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(sex, wave, variable) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(mean, sd),
    names_to = "stat",
    values_to = "value"
  ) %>%
  select(variable, sex, wave, stat, value) %>%
  mutate(value = as.character(round(value, 2)))

# combine into one final table
demographic_table <- bind_rows(race_summary, continuous_summary)

print(demographic_table %>% arrange(variable, sex, wave), n = 100)

write.csv(demographic_table, file = paste0(proj_root, "desc/updated_demographics_by_sex_sens.csv"), row.names = FALSE)

