run_it <- function(root, brain, sex, roi, log_file = "render_progress.log") {
  suppressPackageStartupMessages({
    pacman::p_load(dplyr, tidyr, ggplot2, longCombat, nlme, lme4, mgcv)
  })
  
  # ---------- diagnostics ----------
  tidy_fixed <- function(mod, label) {
    tt <- as.data.frame(summary(mod)$tTable)
    tt$term <- rownames(tt); rownames(tt) <- NULL
    tt$lower <- tt$Value + qnorm(0.025) * tt$`Std.Error`
    tt$upper <- tt$Value + qnorm(0.975) * tt$`Std.Error`
    tt$model <- label
    tt[, c("model","term","Value","Std.Error","lower","upper","t-value","p-value")]
  }
  
  per_scanner_effects <- function(mod) {
    b <- nlme::fixef(mod); V <- stats::vcov(mod)
    sc_lvls <- levels(mod$data$scanner); ref <- sc_lvls[1]
    get_mean_and_se <- function(level) {
      if (level == ref) {
        est <- b["(Intercept)"]; se  <- sqrt(V["(Intercept)","(Intercept)"])
      } else {
        coef_name <- paste0("scanner", level)
        if (!coef_name %in% names(b)) return(c(est=NA_real_, se=NA_real_))
        est <- b["(Intercept)"] + b[coef_name]
        se  <- sqrt(V["(Intercept)","(Intercept)"] + V[coef_name,coef_name] + 2*V["(Intercept)",coef_name])
      }
      return(c(est = unname(est), se = unname(se)))
    }
    out <- as.data.frame(do.call(rbind, lapply(sc_lvls, get_mean_and_se)))
    out$scanner <- sc_lvls; out$lower <- out$est + qnorm(0.025)*out$se; out$upper <- out$est + qnorm(0.975)*out$se
    out
  }
  
  file_paths <- list(
    volume_m    = paste0(root, "data/data_m_volume_long_sens.csv"),
    volume_f    = paste0(root, "data/data_f_volume_long_sens.csv"),
    thickness_m = paste0(root, "data/data_m_thickness_long_sens.csv"),
    thickness_f = paste0(root, "data/data_f_thickness_long_sens.csv"),
    area_m      = paste0(root, "data/data_m_area_long_sens.csv"),
    area_f      = paste0(root, "data/data_f_area_long_sens.csv")
  )
  
  csv_file <- file_paths[[paste(brain, sex, sep = "_")]]
  if (is.null(csv_file) || !file.exists(csv_file)) {
    stop("missing CSV for brain:", brain, " sex:", sex)
  }
  
  data_long <- read.csv(csv_file) %>%
    mutate(
      id        = factor(id),
      site_id_l = factor(site_id_l),
      scanner   = factor(scanner)
    ) %>%
    select(id, wave, age, site_id_l, scanner, region, all_of(brain), timing_parent_sens, tempo_parent_sens, bmi, inc_to_need) %>%
    filter(!scanner %in% c("scanner7", "scanner8")) %>%
    { if (sex == "m") filter(., scanner != "scanner4") else . }
  
  # ---------- specifying the brain features ----------
  data_wide <- data_long %>%
    select(-bmi, -inc_to_need) %>% 
    tidyr::pivot_wider(names_from = region, values_from = all_of(brain)) %>%
    mutate(
      age_scaled = scale(as.numeric(age))[,1],
      timing_parent_scaled = scale(as.numeric(timing_parent_sens))[,1],
      tempo_parent_scaled = scale(as.numeric(tempo_parent_sens))[,1]
    ) %>% 
    filter(
      !is.na(timing_parent_scaled),
      !is.na(tempo_parent_scaled)
    )
  
  if (brain == "volume") {
    volume_exclude <- c("smri_vol_scs_wmhintlh", "smri_vol_scs_wmhintrh")
    data_wide <- dplyr::select(data_wide, -dplyr::any_of(volume_exclude))
  }
  
  data_wide_ns <- data_wide %>%
    select(-site_id_l)
  
  feature_cols <- setdiff(names(data_wide_ns), c("id", "wave", "age", "timing_parent_sens", "tempo_parent_sens", "age_scaled", "scanner", "timing_parent_scaled", "tempo_parent_scaled"))
  if (!length(feature_cols)) stop("no features found after pivot")
  
  na_counts <- vapply(data_wide_ns[feature_cols], function(x) sum(is.na(x)), integer(1))
  drop_feats <- names(na_counts[na_counts > 0])
  if (length(drop_feats)) {
    message("longcombat is dropping ", length(drop_feats), " feature(s) with NA.")
    feature_cols <- setdiff(feature_cols, drop_feats)
    data_wide_ns <- dplyr::select(data_wide_ns, -dplyr::all_of(drop_feats))
  }
  stopifnot(length(feature_cols) > 1)
  
  data_wide_ns[feature_cols] <- lapply(data_wide_ns[feature_cols], function(x) as.numeric(as.character(x)))
  
  data_wide_ns <- data_wide_ns %>%
    mutate(wave_num = as.integer(factor(
      wave, levels = c("baseline_year_1_arm_1", "2_year_follow_up_y_arm_1", "4_year_follow_up_y_arm_1")
    ))) 
  
  # ---------- age smooth basis ----------

  k_val <- 7 
  
  smooth_age <- mgcv::smoothCon(mgcv::s(age_scaled, k = k_val, bs = "cs"), data = data_wide_ns, absorb.cons = TRUE)[[1]]
  basis_age <- smooth_age$X; colnames(basis_age) <- paste0("s_age_", seq_len(ncol(basis_age)))
  
  smooth_timing_int <- mgcv::smoothCon(mgcv::s(age_scaled, by = timing_parent_scaled, k = k_val, bs = "cs"), data = data_wide_ns, absorb.cons = TRUE)[[1]]
  basis_timing_int <- smooth_timing_int$X; colnames(basis_timing_int) <- paste0("s_age_by_timing_", seq_len(ncol(basis_timing_int)))
  
  smooth_tempo_int <- mgcv::smoothCon(mgcv::s(age_scaled, by = tempo_parent_scaled, k = k_val, bs = "cs"), data = data_wide_ns, absorb.cons = TRUE)[[1]]
  basis_tempo_int <- smooth_tempo_int$X; colnames(basis_tempo_int) <- paste0("s_age_by_tempo_", seq_len(ncol(basis_tempo_int)))
  
  data_wide_basis <- cbind(data_wide_ns, basis_age, basis_timing_int, basis_tempo_int)
  data_wide_basis$scanner <- droplevels(data_wide_basis$scanner)
  
  age_formula_part <- paste(colnames(basis_age), collapse = " + ")
  timing_int_formula_part <- paste(colnames(basis_timing_int), collapse = " + ")
  tempo_int_formula_part <- paste(colnames(basis_tempo_int), collapse = " + ")
  
  model_specs <- list(
    age                      = age_formula_part,
    age_timing               = paste(age_formula_part, "timing_parent_scaled", sep = " + "),
    age_tempo                = paste(age_formula_part, "tempo_parent_scaled", sep = " + "),
    age_timing_interaction   = paste(age_formula_part, timing_int_formula_part, sep = " + "),
    age_tempo_interaction    = paste(age_formula_part, tempo_int_formula_part, sep = " + ")
  )
  
  all_results <- list()
  
  for (model_name in names(model_specs)) {
    
    all_results[[model_name]] <- tryCatch({
      
      current_formula <- model_specs[[model_name]]
      
      combat_obj <- longCombat::longCombat(
        idvar = "id", timevar = "wave_num", batchvar = "scanner",
        features = feature_cols, formula = current_formula, ranef = "(1|id)",
        data = data_wide_basis, verbose = TRUE
      )
      
      harm_raw <- if ("data_combat" %in% names(combat_obj)) combat_obj$data_combat else combat_obj$dat.combat
      stopifnot(nrow(harm_raw) > 0)
      base_feats <- sub("\\.combat$", "", feature_cols)
      
      hr_names <- colnames(harm_raw)
      already_combat <- any(grepl("\\.combat$", hr_names))
      
      if (already_combat) {
        hr_base <- sub("\\.combat$", "", hr_names)
        keep_idx <- match(base_feats, hr_base) 
        if (anyNA(keep_idx)) { stop("Features are missing from ComBat output") }
        harm <- as.data.frame(harm_raw[, keep_idx, drop = FALSE])
      } else {
        if (!all(base_feats %in% hr_names)) { stop("Feature names do not match ComBat output") }
        harm <- as.data.frame(harm_raw[, base_feats, drop = FALSE])
      }
      
      colnames(harm) <- paste0(base_feats, ".combat")
      harm[] <- lapply(harm, function(x) suppressWarnings(as.numeric(as.character(x))))
      
      data_wide_keep <- data_wide %>%
        dplyr::semi_join(data_wide_ns %>% select(id, wave), by = c("id","wave"))
      
      data_final <- dplyr::bind_cols(
        data_wide_keep[, c("id", "wave", "age_scaled", "site_id_l", "scanner", "timing_parent_scaled", "tempo_parent_scaled")],
        data_wide_keep[base_feats], harm
      )
      
      all_roi_cols <- c(base_feats, paste0(base_feats, ".combat"))
      data_final[all_roi_cols] <- lapply(data_final[all_roi_cols], function(x) as.numeric(as.character(x)))
      
      if (!roi %in% feature_cols) {
        warning("requested ROI '", roi, "' not in final feature list; using first available.")
        roi <- feature_cols[1]
      }
      roi_before <- roi; roi_after  <- paste0(roi, ".combat")
      
      data_model <- data_final %>%
        transmute(
          id, wave, scanner, age_scaled, site_id_l, timing_parent_scaled, tempo_parent_scaled,
          brain_metric = .data[[roi_before]], brain_metric.combat = .data[[roi_after]]
        )
      
      # ----- diagnostics -----
      p_before <- ggplot(data_model, aes(scanner, brain_metric, fill = scanner)) + geom_boxplot() + labs(title = "Before ComBat") + theme_minimal()
      p_after <- ggplot(data_model, aes(scanner, brain_metric.combat, fill = scanner)) + geom_boxplot() + labs(title = paste("After ComBat (", model_name, ")")) + theme_minimal()
      dens <- data_final %>%
        select(scanner, all_of(roi_before), all_of(roi_after)) %>%
        tidyr::pivot_longer(c(all_of(roi_before), all_of(roi_after)), names_to = "pipeline", values_to = "brain") %>%
        ggplot(aes(x = brain, color = scanner)) + geom_density() + facet_wrap(~ pipeline)
      
      mod_dat  <- data_model %>% select(id, scanner, age_scaled, brain_metric) %>% filter(complete.cases(.))
      mod_datc <- data_model %>% select(id, scanner, age_scaled, brain_metric.combat) %>% filter(complete.cases(.))
      
      if (nrow(mod_dat) < 20 || nrow(mod_datc) < 20) { 
        lrt_pre  <- data.frame()
        lrt_post <- data.frame()
        age_cmp  <- data.frame()
        sc_cmp   <- data.frame()
        sc_plot  <- ggplot() + theme_void() + ggtitle("Skipped - Too few cases")
      } else {
        m0  <- nlme::lme(brain_metric ~ age_scaled, data = mod_dat,  random = ~1|id, method = "ML", na.action = na.omit)
        m1  <- nlme::lme(brain_metric ~ age_scaled + scanner, data = mod_dat,  random = ~1|id, method = "ML", na.action = na.omit)
        m0c <- nlme::lme(brain_metric.combat ~ age_scaled, data = mod_datc, random = ~1|id, method = "ML", na.action = na.omit)
        m1c <- nlme::lme(brain_metric.combat ~ age_scaled + scanner, data = mod_datc, random = ~1|id, method = "ML", na.action = na.omit)
        
        lrt_pre  <- as.data.frame(anova(m0, m1))
        lrt_post <- as.data.frame(anova(m0c, m1c))
        
        # ----- fixed effects -----
        fx_pre  <- tidy_fixed(m1, "pre")
        fx_post <- tidy_fixed(m1c, "post")
        fx_both <- dplyr::bind_rows(fx_pre, fx_post)
        
        age_cmp <- fx_both %>%
          dplyr::filter(term == "age_scaled") %>%
          tidyr::pivot_wider(names_from = model, values_from = c(Value, Std.Error, lower, upper)) %>%
          dplyr::mutate(diff = Value_post - Value_pre, pctchg = 100 * diff / Value_pre)
        
        sc_pre  <- per_scanner_effects(m1)  %>% dplyr::mutate(model="pre")
        sc_post <- per_scanner_effects(m1c) %>% dplyr::mutate(model="post")
        
        sc_cmp <- dplyr::left_join(
          sc_pre %>% dplyr::select(scanner, est_pre = est, se_pre = se),
          sc_post %>% dplyr::select(scanner, est_post = est, se_post = se),
          by = "scanner"
        ) %>%
          dplyr::mutate(diff = est_post - est_pre, pctchg = 100 * diff / est_pre) %>%
          dplyr::arrange(dplyr::desc(abs(diff)))
        
        sc_plot <- dplyr::bind_rows(sc_pre, sc_post) %>%
          ggplot(aes(x = est, y = scanner, color = model)) +
          geom_point(position = position_dodge(width = 0.4)) +
          geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, position = position_dodge(width = 0.4)) +
          geom_vline(xintercept = 0, linetype = 2) +
          labs(title = "Scanner fixed effects (age_scaled = 0)", x = "estimated effect", y = "scanner") +
          theme_minimal()
      }
      
      # ----- outputs -----
      data_long_out1 <- data_final %>%
        pivot_longer(
          cols = all_of(all_roi_cols), names_to = c("region", "pipeline"),
          names_pattern = "^(.*?)(\\.combat)?$", values_to = "value"
        ) %>%
        mutate(pipeline = ifelse(pipeline == ".combat", "combat", "raw"))
      
      data_long_covars <- data_long %>% select(id, wave, bmi, inc_to_need) %>% distinct()
      data_long_out <- left_join(data_long_out1, data_long_covars, by = c("id", "wave"))
      
      write.csv(data_long_out, file.path(root, "data", sprintf("data_%s_%s_combat_long_%s_model_sens.csv", sex, brain, model_name)), row.names = FALSE)
      
      list(
        formula_str = current_formula, data_model = data_model, p_before = p_before, p_after = p_after,
        lrt_pre = lrt_pre, lrt_post = lrt_post, density = dens, age_cmp = age_cmp, sc_cmp = sc_cmp, sc_plot = sc_plot
      )
      
    }, error = function(e) {
      
      cat(sprintf("  -> ERROR processing model: %s. Skipping.\n", model_name), file = log_file, append = TRUE)
      
      failed_plot <- ggplot() + theme_void() + 
        ggtitle(paste0("model failed to run for: '", model_name, "'")) +
        geom_text(aes(x=0, y=0, label=stringr::str_wrap(e$message, 60)), size=3, color="red")
      
      list(
        formula_str = model_specs[[model_name]], data_model = data.frame(), p_before = failed_plot,
        p_after = failed_plot, lrt_pre = data.frame(status = "failed"), lrt_post = data.frame(status = "failed"),
        density = failed_plot, age_cmp = data.frame(status = "failed"), sc_cmp = data.frame(status = "failed"),
        sc_plot = failed_plot
      )
    })
  }
  
  return(all_results)
}