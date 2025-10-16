pacman::p_load(dplyr, stringr, glue, tibble, purrr, fs, rmarkdown)

proj_root <- ".../projects/abcd-projs/smri-pub-abcd/"
data_root <- ".../projects/abcd-projs/smri-pub-abcd/results/"
plots_dir <- file.path(data_root, "plots/for_pub")

pngs <- dir_ls(plots_dir, recurse = TRUE, regexp = "\\.png$") %>% as.character()

# filenames look like e.g.
# ggplot_age_effect_<metric>_<region>_<sex>_combat.png
# ggplot_<effect>_<x>_<metric>_<region>_<model>_<sex>_combat.png
parse_file <- function(path) {
  f <- fs::path_file(path)

  # main/int pattern
  m <- stringr::str_match(
    f,
    "^ggplot_(tempo|timing)_(main|int)_effect_([A-Za-z]+)_(.+?)_([mf])_combat\\.png$"
  )
  if (!is.na(m[1,1])) {
    return(tibble(
      model_family = m[, 2],
      effect       = m[, 3],
      metric       = m[, 4], 
      region_token = m[, 5],
      sex          = m[, 6],
      path         = path
    ))
  }

  # age pattern
  a <- stringr::str_match(
    f,
    "^ggplot_(tempo|timing)_age_effect_([A-Za-z]+)_(.+?)_([mf])_combat\\.png$"
  )
  if (!is.na(a[1,1])) {
    return(tibble::tibble(
      effect       = "age",
      metric       = a[,3],    
      region_token = a[,4],    
      model_family = a[,2],    
      sex          = a[,5],    
      path         = path
    ))
  }

  NULL
}

meta <- map_dfr(pngs, parse_file) %>%
  mutate(
    sex_lab = ifelse(sex == "m", "male", "female"),
    group   = paste(sex_lab, metric, sep = " - "),
    effect  = factor(effect, levels = c("age","main","int")),
    model_family = factor(model_family, levels = c("timing","tempo"))
  ) %>%
  arrange(model_family, group, effect, region_token)

stopifnot(nrow(meta) > 0)

# Level 1: timing | tempo
# Level 2: male - thickness | female - thickness | ...
# Level 3: age | main | int

rmd_path <- file.path(data_root, "plots", "all_effects_tabs.Rmd")

yaml <- c(
  "---",
  'title: "model effects overview"',
  "output:",
  "  html_document:",
  "    self_contained: true",
  "    toc: true",
  "    toc_depth: 2",
  "    theme: readable",
  "    df_print: paged",
  "---",
  ""
)

# 3-wide responsive grid
css <- c(
  "```{css, echo=FALSE}",
  ".img-grid {display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 16px;}",
  ".img-grid img {width: 100%; height: auto; border: 1px solid #ddd;}",
  "```",
  ""
)

content <- meta %>%
  group_by(model_family, group) %>%
  summarise(
    group_md = {

      eff_blocks <- cur_data() %>%
        group_by(effect) %>%
        summarise(
          md = glue::glue(
            "#### {unique(effect)}\n\n", 
            '<div class="img-grid">\n\n',
            paste0("![](", path, ")", collapse = "\n\n"),
            '\n\n</div>'
          ),
          .groups = "drop"
        ) %>%
        pull(md) %>%
        paste(collapse = "\n\n")
      
      glue::glue(
        "### {unique(group)} {{.tabset .tabset-fade}}\n\n",
        eff_blocks
      )
    },
    .groups = "drop"
  ) %>%
  group_by(model_family) %>%
  summarise(
    family_md = paste(group_md, collapse = "\n\n"),
    .groups = "drop"
  ) %>%
  mutate(
    final_md = glue::glue("## {model_family} {{.tabset .tabset-fade}}\n\n{family_md}")
  ) %>%
  pull(final_md) %>%
  paste(collapse = "\n\n")


writeLines(c(yaml, css, content), rmd_path)

rmarkdown::render(rmd_path, output_file = "all_effects_tabs.html", output_dir = dirname(rmd_path))
cat("\nhtml written to:", file.path(dirname(rmd_path), "all_effects_tabs.html"), "\n")