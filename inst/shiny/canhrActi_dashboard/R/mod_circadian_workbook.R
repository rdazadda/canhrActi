# Reproducible circadian workbook export: a multi-sheet .xlsx of the raw and
# intermediate data plus every metric. Entry point: circadian_write_workbook().

# Guarded scalar getter: returns default when x is NULL/empty.
.cw_get <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) return(default)
  x[[1]]
}

# i-th element of a possibly-NULL/short vector (for c(lo, hi) CI fields).
.cw_idx <- function(x, i, default = NA_real_) {
  if (is.null(x) || length(x) < i) return(default)
  x[[i]]
}

# "HH:MM" -> decimal hours (vectorised; NA on failure).
.cw_hhmm <- function(s) {
  vapply(as.character(s), function(v) {
    if (is.na(v) || !nzchar(v)) return(NA_real_)
    p <- suppressWarnings(as.numeric(strsplit(v, ":", fixed = TRUE)[[1]]))
    if (length(p) < 2 || any(is.na(p[1:2]))) return(NA_real_)
    p[1] + p[2] / 60
  }, numeric(1), USE.NAMES = FALSE)
}

# Per-file activity series matching the analysis loop's `activity` definition.
.cw_activity <- function(data, metric) {
  if (identical(metric, "vm") && all(c("axis1", "axis2", "axis3") %in% names(data))) {
    sqrt(data$axis1^2 + data$axis2^2 + data$axis3^2)
  } else {
    data$axis1
  }
}

# Per-epoch logical: TRUE where the epoch's day passed the wear-validity gate.
.cw_valid_day <- function(shared, fid, timestamps) {
  d <- shared$results$wear_time[[fid]]$daily
  if (is.null(d) || !all(c("valid", "date") %in% names(d))) {
    return(rep(TRUE, length(timestamps)))
  }
  as.Date(timestamps) %in% as.Date(d$date[d$valid])
}

# Internal column name -> clean display header (applied at write time).
.CW_LABELS <- c(
  subject_id = "Subject", file_name = "Filename", metric = "Activity Metric",
  epoch_length_s = "Epoch Length (s)", n_days_analyzed = "Days Analyzed",
  n_valid_circadian_days = "Valid Circadian Days", coverage_percent = "Coverage (%)",
  n_valid_epochs = "Valid Epochs", n_total_epochs = "Total Epochs",
  L5 = "L5 Activity (counts)", L5_start = "L5 Onset (clock)", L5_start_hour = "L5 Onset (hours)",
  M10 = "M10 Activity (counts)", M10_start = "M10 Onset (clock)", M10_start_hour = "M10 Onset (hours)",
  L1 = "L1 Activity (counts)", L1_start = "L1 Onset (clock)",
  M1 = "M1 Activity (counts)", M1_start = "M1 Onset (clock)",
  RA = "Relative Amplitude", IS = "Interdaily Stability", IV = "Intradaily Variability",
  phi = "Phi (Lag 1 Autocorrelation)",
  IS_60min = "Interdaily Stability (60 min bins)", IS_30min = "Interdaily Stability (30 min bins)",
  IS_15min = "Interdaily Stability (15 min bins)",
  tau = "Endogenous Period (hours)", period_power = "Periodogram Peak Power",
  chisq_period = "Chi-square Period (hours)", chisq_significant = "Chi-square Rhythm Significant",
  Qp = "Chi-square Qp", critical = "Chi-square Significance Threshold",
  period_p_value = "Periodogram P Value",
  SRI = "Sleep Regularity Index", SRI_n_valid_pairs = "Sleep Regularity Index Epoch Pairs",
  onset_timing_variability = "Onset Timing Variability (hours)",
  L5_variability_hours = "L5 Onset Variability (hours)",
  M10_variability_hours = "M10 Onset Variability (hours)",
  CPD = "Composite Phase Deviation (hours)",
  CPD_precision = "Composite Phase Deviation Precision (hours)",
  CPD_accuracy = "Composite Phase Deviation Accuracy (hours)",
  L5_onset_mean = "L5 Onset Mean (hours)",
  L5_onset_ci_lower = "L5 Onset 95% CI Lower (hours)",
  L5_onset_ci_upper = "L5 Onset 95% CI Upper (hours)",
  social_jet_lag_hours = "Social Jet Lag (hours)", social_jet_lag_min = "Social Jet Lag (min)",
  MSW = "Mid-Sleep Workdays (hours)", MSF = "Mid-Sleep Free Days (hours)",
  n_work_nights = "Workday Nights (n)", n_free_nights = "Free-Day Nights (n)",
  cosinor_mesor = "Cosinor MESOR (counts)", cosinor_amplitude = "Cosinor Amplitude (counts)",
  cosinor_acrophase = "Cosinor Acrophase (hours)", cosinor_acrophase_time = "Cosinor Acrophase (clock)",
  cosinor_se_mesor = "Cosinor MESOR Standard Error", cosinor_se_amplitude = "Cosinor Amplitude Standard Error",
  cosinor_se_acrophase = "Cosinor Acrophase Standard Error",
  cosinor_ci_mesor_lo = "Cosinor MESOR 95% CI Lower", cosinor_ci_mesor_hi = "Cosinor MESOR 95% CI Upper",
  cosinor_ci_amplitude_lo = "Cosinor Amplitude 95% CI Lower", cosinor_ci_amplitude_hi = "Cosinor Amplitude 95% CI Upper",
  cosinor_ci_acrophase_lo = "Cosinor Acrophase 95% CI Lower", cosinor_ci_acrophase_hi = "Cosinor Acrophase 95% CI Upper",
  cosinor_r_squared = "Cosinor R Squared", cosinor_percent_rhythm = "Cosinor Percent Rhythm (%)",
  cosinor_f_statistic = "Cosinor F Statistic", cosinor_p_value = "Cosinor P Value",
  cosinor_rhythm_significant = "Cosinor Rhythm Significant",
  ext_mesor = "Extended Cosinor MESOR (counts)", ext_amplitude = "Extended Cosinor Amplitude (counts)",
  ext_acrophase = "Extended Cosinor Acrophase (hours)", ext_r_squared = "Extended Cosinor R Squared",
  ext_r_squared_single = "Single Component R Squared",
  ext_r_squared_improvement = "Multi Harmonic R Squared Improvement",
  h12_amplitude = "12 Hour Harmonic Amplitude (counts)", h12_power = "12 Hour Harmonic Relative Power (%)",
  ext_is_bimodal = "Bimodal Rhythm", ext_pattern_type = "Rhythm Pattern Type",
  cosinorExt_minimum = "Antilogistic Minimum (counts)", cosinorExt_amplitude = "Antilogistic Amplitude (counts)",
  cosinorExt_alpha = "Antilogistic Alpha", cosinorExt_beta = "Antilogistic Beta",
  cosinorExt_acrotime = "Antilogistic Acrophase (hours)", cosinorExt_peak = "Antilogistic Peak (counts)",
  cosinorExt_UpMesor = "Antilogistic Up Mesor (hours)", cosinorExt_DownMesor = "Antilogistic Down Mesor (hours)",
  cosinorExt_MESOR = "Antilogistic MESOR (counts)", cosinorExt_F_pseudo = "Antilogistic Pseudo F",
  cosinorExt_rss_cosinor = "Antilogistic Residual Sum of Squares (cosinor)",
  cosinorExt_rss_extended = "Antilogistic Residual Sum of Squares (extended)",
  cosinorExt_converged = "Antilogistic Converged",
  circadian_quotient = "Circadian Quotient", cosinor_relative_amplitude = "Cosinor Relative Amplitude",
  dfa_alpha = "DFA Alpha (overall)", dfa_alpha1 = "DFA Alpha 1 (short term)", dfa_alpha2 = "DFA Alpha 2 (long term)",
  mse_area = "Multiscale Entropy Area", mse_slope = "Multiscale Entropy Slope",
  rhythm_detected = "Rhythm Detected", ellipse_distance_stat = "Confidence Ellipse Distance",
  ellipse_critical_value = "Confidence Ellipse Critical Value",
  # Profile / raw / periodogram / fractal sheet columns
  date = "Date", hour = "Hour of Day", mean_counts = "Mean Counts",
  sd_counts = "Standard Deviation (counts)", se_counts = "Standard Error (counts)",
  n = "Epochs (n)", cosinor_fitted = "Cosinor Fitted (counts)",
  minute_of_day = "Minute of Day", clock_time = "Clock Time", n_days = "Days (n)",
  period_h = "Period (hours)", power = "Periodogram Power",
  window_n = "Window Size (epochs)", fluctuation_Fn = "Fluctuation F(n)",
  log10_n = "Log10 Window Size", log10_Fn = "Log10 Fluctuation", segment = "Segment",
  scale_tau = "Scale (tau)", sample_entropy = "Sample Entropy",
  timestamp = "Timestamp", counts = "Counts", axis1 = "Axis 1", axis2 = "Axis 2",
  axis3 = "Axis 3", VM = "Vector Magnitude", wear_flag = "Wear (1 = worn)",
  valid_day = "Valid Day (1 = valid)", sleep_state = "Sleep State",
  column = "Column", definition = "Definition", unit = "Unit", note = "Note"
)

# Rename a data frame's columns to clean display headers for export.
.cw_relabel <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(df)
  names(df) <- vapply(names(df), function(n) {
    if (n %in% names(.CW_LABELS)) .CW_LABELS[[n]]
    else tools::toTitleCase(gsub("_", " ", n))
  }, character(1))
  df
}

#' Short data dictionary (column | definition | unit) for the workbook.
#' @keywords internal
circadian_data_dictionary <- function() {
  d <- function(column, definition, unit) {
    data.frame(column = column, definition = definition, unit = unit,
               stringsAsFactors = FALSE)
  }
  rbind(
    d("L5 / M10", "Mean activity of the least-active 5 hours / most-active 10 hours", "counts"),
    d("L5 Onset / M10 Onset", "Clock time the L5 / M10 window starts", "HH:MM or hours"),
    d("L1 / M1", "Least-active 1 hour / most-active 1 hour mean", "counts"),
    d("Relative Amplitude", "(M10 - L5) / (M10 + L5)", "0 to 1"),
    d("Interdaily Stability", "Day-to-day regularity of the rhythm", "0 to 1"),
    d("Intradaily Variability", "Fragmentation of the rest-activity rhythm", "about 0 to 2"),
    d("Phi", "Lag 1 autocorrelation of the hourly profile", "-1 to 1"),
    d("Interdaily Stability (60 / 30 / 15 min)", "Interdaily stability at finer bin widths", "0 to 1"),
    d("Endogenous Period", "Period of the Lomb-Scargle spectral peak", "hours"),
    d("Periodogram Peak Power / P Value", "Spectral peak power and Baluev false-alarm probability", "unitless / 0 to 1"),
    d("Chi-square Period", "Period of the Sokolove-Bushell chi-square periodogram peak (family-wise corrected significance)", "hours"),
    d("Sleep Regularity Index", "Day-to-day sleep-wake concordance (Phillips 2017)", "-100 to 100"),
    d("Composite Phase Deviation", "Phase instability with precision and accuracy (Fischer & Roenneberg 2016)", "hours"),
    d("Onset Timing Variability", "Circular standard deviation of daily L5 / M10 onsets", "hours"),
    d("L5 Onset Mean + 95% CI", "Mean L5 onset with bootstrap confidence interval", "hours"),
    d("Social Jet Lag", "Weekday vs weekend mid-sleep difference (MSF - MSW; Wittmann/Roenneberg). MSW = mid-sleep on workdays, MSF = mid-sleep on free days", "hours"),
    d("Cosinor MESOR / Amplitude / Acrophase", "Single 24 hour cosinor fit", "counts / counts / hours"),
    d("Cosinor R Squared / Percent Rhythm", "Cosinor goodness of fit", "0 to 1 / %"),
    d("Extended Cosinor", "Multi-harmonic (24 hour + 12 hour) cosinor fit", "mixed"),
    d("Antilogistic Cosinor", "Marler antilogistic extended cosinor (minimum, amplitude, alpha, beta, acrophase, peak, up / down mesor, MESOR, pseudo-F)", "mixed"),
    d("Circadian Quotient", "Cosinor amplitude divided by MESOR", "unitless"),
    d("DFA Alpha / Alpha 1 / Alpha 2", "Detrended fluctuation scaling exponents", "unitless"),
    d("Multiscale Entropy Area / Slope", "Multiscale sample entropy complexity", "unitless"),
    d("Rhythm Detected", "Cosinor joint confidence-ellipse rhythm test", "TRUE / FALSE"),
    d("Valid Day", "Day met the Wear Time tab's minimum-wear-per-day threshold (GGIR-style includedaycrit). Recording-level metrics, averaged profiles, and the DFA / Multiscale Entropy metrics all use valid (worn) days only; DFA/MSE analyse the longest continuous worn run", "1 = valid / 0"),
    d("Counts", "ActiGraph activity counts (not GGIR mg / ENMO; shape metrics are comparable, absolute levels are not)", "counts"),
    stringsAsFactors = FALSE
  )
}

# One Summary_Metrics row for a single recording's stored result list `r`.
.cw_summary_row <- function(r, metric) {
  fr   <- r$full_result
  ca   <- r$cosinor_analysis
  anti <- r$cosinor_antilog
  quo  <- r$circadian_quotient_res
  ell  <- r$cosinor_ellipse
  ism  <- r$is_multiscale
  dfa  <- r$dfa
  mse  <- r$mse
  is_at <- function(b) {
    if (!is.null(ism) && b %in% ism$bin_minutes) ism$IS[ism$bin_minutes == b][1] else NA_real_
  }
  data.frame(
    subject_id = .cw_get(r$subject_id, NA_character_),
    file_name  = .cw_get(r$name, NA_character_),
    metric     = metric,
    epoch_length_s = .cw_get(fr$epoch_length),
    n_days_analyzed = .cw_get(fr$n_days_analyzed),
    n_valid_circadian_days = .cw_get(fr$n_valid_circadian_days),
    coverage_percent = .cw_get(fr$coverage_percent),
    n_valid_epochs = .cw_get(fr$n_valid_epochs),
    n_total_epochs = .cw_get(fr$n_total_epochs),
    L5 = .cw_get(fr$L5), L5_start = .cw_get(fr$L5_start, NA_character_),
    L5_start_hour = .cw_get(fr$L5_start_hour),
    M10 = .cw_get(fr$M10), M10_start = .cw_get(fr$M10_start, NA_character_),
    M10_start_hour = .cw_get(fr$M10_start_hour),
    L1 = .cw_get(fr$L1), L1_start = .cw_get(fr$L1_start, NA_character_),
    M1 = .cw_get(fr$M1), M1_start = .cw_get(fr$M1_start, NA_character_),
    RA = .cw_get(fr$RA), IS = .cw_get(fr$IS), IV = .cw_get(fr$IV), phi = .cw_get(fr$phi),
    IS_60min = is_at(60), IS_30min = is_at(30), IS_15min = is_at(15),
    tau = .cw_get(fr$tau), period_power = .cw_get(fr$period_power),
    period_p_value = .cw_get(fr$period_p_value),
    chisq_period = .cw_get(r$chisq$period),
    chisq_significant = .cw_get(r$chisq$significant, NA),
    SRI = .cw_get(fr$SRI), SRI_n_valid_pairs = .cw_get(fr$SRI_n_valid_pairs),
    onset_timing_variability = .cw_get(fr$onset_timing_variability),
    L5_variability_hours = .cw_get(fr$L5_variability_hours),
    M10_variability_hours = .cw_get(fr$M10_variability_hours),
    CPD = .cw_get(fr$CPD), CPD_precision = .cw_get(fr$CPD_precision),
    CPD_accuracy = .cw_get(fr$CPD_accuracy),
    L5_onset_mean = .cw_get(fr$L5_onset_mean),
    L5_onset_ci_lower = .cw_get(fr$L5_onset_ci_lower),
    L5_onset_ci_upper = .cw_get(fr$L5_onset_ci_upper),
    # Social jet lag (weekday vs weekend mid-sleep)
    social_jet_lag_hours = .cw_get(r$social_jet_lag$social_jet_lag_hours),
    social_jet_lag_min = .cw_get(r$social_jet_lag$social_jet_lag_min),
    MSW = .cw_get(r$social_jet_lag$MSW), MSF = .cw_get(r$social_jet_lag$MSF),
    n_work_nights = .cw_get(r$social_jet_lag$n_work_nights),
    n_free_nights = .cw_get(r$social_jet_lag$n_free_nights),
    # Single-component cosinor
    cosinor_mesor = .cw_get(ca$mesor), cosinor_amplitude = .cw_get(ca$amplitude),
    cosinor_acrophase = .cw_get(ca$acrophase),
    cosinor_acrophase_time = .cw_get(ca$acrophase_time, NA_character_),
    cosinor_se_mesor = .cw_get(ca$se_mesor), cosinor_se_amplitude = .cw_get(ca$se_amplitude),
    cosinor_se_acrophase = .cw_get(ca$se_acrophase),
    cosinor_ci_mesor_lo = .cw_idx(ca$ci_mesor, 1), cosinor_ci_mesor_hi = .cw_idx(ca$ci_mesor, 2),
    cosinor_ci_amplitude_lo = .cw_idx(ca$ci_amplitude, 1), cosinor_ci_amplitude_hi = .cw_idx(ca$ci_amplitude, 2),
    cosinor_ci_acrophase_lo = .cw_idx(ca$ci_acrophase, 1), cosinor_ci_acrophase_hi = .cw_idx(ca$ci_acrophase, 2),
    cosinor_r_squared = .cw_get(ca$r_squared), cosinor_percent_rhythm = .cw_get(ca$percent_rhythm),
    cosinor_f_statistic = .cw_get(ca$f_statistic), cosinor_p_value = .cw_get(ca$p_value),
    cosinor_rhythm_significant = .cw_get(ca$rhythm_significant, NA),
    # Multi-harmonic (extended) cosinor
    ext_mesor = .cw_get(r$mesor), ext_amplitude = .cw_get(r$amplitude),
    ext_acrophase = .cw_get(r$acrophase),
    ext_r_squared = .cw_get(r$r_squared), ext_r_squared_single = .cw_get(r$r_squared_single),
    ext_r_squared_improvement = .cw_get(r$r_squared_improvement),
    h12_amplitude = .cw_get(r$h12_amplitude), h12_power = .cw_get(r$h12_power),
    ext_is_bimodal = .cw_get(r$is_bimodal, NA), ext_pattern_type = .cw_get(r$pattern_type, NA_character_),
    # Anti-logistic (Marler) cosinor
    cosinorExt_minimum = .cw_get(anti$minimum), cosinorExt_amplitude = .cw_get(anti$amplitude),
    cosinorExt_alpha = .cw_get(anti$alpha), cosinorExt_beta = .cw_get(anti$beta),
    cosinorExt_acrotime = .cw_get(anti$acrotime), cosinorExt_peak = .cw_get(anti$peak),
    cosinorExt_UpMesor = .cw_get(anti$UpMesor), cosinorExt_DownMesor = .cw_get(anti$DownMesor),
    cosinorExt_MESOR = .cw_get(anti$MESOR), cosinorExt_F_pseudo = .cw_get(anti$F_pseudo),
    cosinorExt_rss_cosinor = .cw_get(anti$rss_cosinor),
    cosinorExt_rss_extended = .cw_get(anti$rss_extended),
    cosinorExt_converged = .cw_get(anti$converged, NA),
    # Circadian quotient
    circadian_quotient = .cw_get(quo$circadian_quotient),
    cosinor_relative_amplitude = .cw_get(quo$relative_amplitude),
    # Fractal / complexity
    dfa_alpha = .cw_get(dfa$alpha), dfa_alpha1 = .cw_get(dfa$alpha1), dfa_alpha2 = .cw_get(dfa$alpha2),
    mse_area = .cw_get(mse$area), mse_slope = .cw_get(mse$slope),
    # Confidence ellipse
    rhythm_detected = .cw_get(ell$rhythm_detected, NA),
    ellipse_distance_stat = .cw_get(ell$distance_stat),
    ellipse_critical_value = .cw_get(ell$critical_value),
    stringsAsFactors = FALSE
  )
}

# Stack a per-file data.frame field with subject_id/file_name keys prepended.
.cw_bind <- function(results, accessor) {
  parts <- lapply(results, function(r) {
    df <- accessor(r)
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
    cbind(subject_id = .cw_get(r$subject_id, NA_character_),
          file_name = .cw_get(r$name, NA_character_), df, stringsAsFactors = FALSE)
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) return(NULL)
  do.call(rbind, parts)
}

# Averaged minute-of-day (0-1439) profile from the raw wear-filtered counts.
.cw_minute_profile <- function(results, shared, metric) {
  parts <- lapply(names(results), function(fid) {
    r <- results[[fid]]
    f <- shared$files[[fid]]
    if (is.null(f) || is.null(f$data) || is.null(f$data$timestamp)) return(NULL)
    data <- f$data
    counts <- .cw_activity(data, metric)
    wt <- shared$results$wear_time[[fid]]$wear
    wear <- if (!is.null(wt) && length(wt) == length(counts)) as.logical(wt) else rep(TRUE, length(counts))
    counts[!(wear & .cw_valid_day(shared, fid, data$timestamp))] <- NA
    lt <- as.POSIXlt(data$timestamp)
    mod <- lt$hour * 60L + lt$min
    agg <- data.frame(minute_of_day = 0:1439)
    m <- tapply(counts, factor(mod, levels = 0:1439), mean, na.rm = TRUE)
    s <- tapply(counts, factor(mod, levels = 0:1439), function(v) stats::sd(v, na.rm = TRUE))
    n <- tapply(counts, factor(mod, levels = 0:1439), function(v) sum(!is.na(v)))
    agg$clock_time   <- sprintf("%02d:%02d", agg$minute_of_day %/% 60, agg$minute_of_day %% 60)
    agg$mean_counts  <- round(as.numeric(m), 2)
    agg$sd_counts    <- round(as.numeric(s), 2)
    agg$n_days       <- as.integer(n)
    cbind(subject_id = .cw_get(r$subject_id, NA_character_),
          file_name = .cw_get(r$name, NA_character_), agg, stringsAsFactors = FALSE)
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) return(NULL)
  do.call(rbind, parts)
}

# Per-epoch raw substrate: timestamp, axes, VM, wear flag, sleep state.
.cw_raw_epochs <- function(results, shared, metric) {
  parts <- lapply(names(results), function(fid) {
    r <- results[[fid]]
    f <- shared$files[[fid]]
    if (is.null(f) || is.null(f$data) || is.null(f$data$timestamp)) return(NULL)
    data <- f$data
    n <- nrow(data)
    vm <- if (all(c("axis1", "axis2", "axis3") %in% names(data))) {
      sqrt(data$axis1^2 + data$axis2^2 + data$axis3^2)
    } else rep(NA_real_, n)
    wt <- shared$results$wear_time[[fid]]$wear
    ss <- shared$results$sleep[[fid]]$sleep_state
    df <- data.frame(
      subject_id = .cw_get(r$subject_id, NA_character_),
      file_name  = .cw_get(r$name, NA_character_),
      timestamp  = format(data$timestamp, "%Y-%m-%d %H:%M:%S"),
      date       = format(as.Date(data$timestamp)),
      counts     = round(.cw_activity(data, metric), 2),
      axis1 = if ("axis1" %in% names(data)) data$axis1 else NA_real_,
      axis2 = if ("axis2" %in% names(data)) data$axis2 else NA_real_,
      axis3 = if ("axis3" %in% names(data)) data$axis3 else NA_real_,
      VM = round(vm, 2),
      wear_flag = if (!is.null(wt) && length(wt) == n) as.integer(as.logical(wt)) else NA_integer_,
      valid_day = as.integer(.cw_valid_day(shared, fid, data$timestamp)),
      sleep_state = if (!is.null(ss) && length(ss) == n) as.character(ss) else NA_character_,
      stringsAsFactors = FALSE
    )
    df
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) return(NULL)
  do.call(rbind, parts)
}

# DFA log-log fit points across recordings.
.cw_dfa <- function(results) {
  .cw_bind(results, function(r) {
    dfa <- r$dfa
    if (is.null(dfa) || is.null(dfa$scales) || length(dfa$scales) == 0) return(NULL)
    bp <- if (!is.null(dfa$breakpoint)) dfa$breakpoint else 90
    data.frame(
      window_n = dfa$scales,
      fluctuation_Fn = dfa$fluctuations,
      log10_n = round(log10(dfa$scales), 4),
      log10_Fn = round(log10(dfa$fluctuations), 4),
      segment = ifelse(dfa$scales < bp, "short", "long"),
      stringsAsFactors = FALSE
    )
  })
}

# MSE sample-entropy by scale across recordings.
.cw_mse <- function(results) {
  .cw_bind(results, function(r) {
    mse <- r$mse
    if (is.null(mse) || is.null(mse$scales) || length(mse$scales) == 0) return(NULL)
    data.frame(
      scale_tau = mse$scales,
      sample_entropy = round(mse$mse, 4),
      stringsAsFactors = FALSE
    )
  })
}

# Hourly profile + the fitted cosinor curve, across recordings.
.cw_hourly <- function(results) {
  parts <- lapply(results, function(r) {
    hp <- r$full_result$hourly_profile
    if (is.null(hp) || !is.data.frame(hp) || nrow(hp) == 0) return(NULL)
    ca <- r$cosinor_analysis
    fitted <- rep(NA_real_, nrow(hp))
    if (!is.null(ca) && !is.null(ca$mesor) && !is.null(ca$amplitude) && !is.null(ca$acrophase)) {
      fitted <- ca$mesor + ca$amplitude *
        cos(2 * pi * ((hp$hour + 0.5) - ca$acrophase) / 24)
    }
    cbind(subject_id = .cw_get(r$subject_id, NA_character_),
          file_name = .cw_get(r$name, NA_character_), hp,
          cosinor_fitted = round(fitted, 2), stringsAsFactors = FALSE)
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) return(NULL)
  do.call(rbind, parts)
}

# Daily metrics: per-day values + parsed onset hours + the per-day validity flag.
.cw_daily <- function(results, shared) {
  parts <- lapply(names(results), function(fid) {
    r <- results[[fid]]
    dm <- r$full_result$daily_metrics
    if (is.null(dm) || !is.data.frame(dm) || nrow(dm) == 0) return(NULL)
    dm$L5_start_hour <- .cw_hhmm(dm$L5_start)
    dm$M10_start_hour <- .cw_hhmm(dm$M10_start)
    wd <- shared$results$wear_time[[fid]]$daily
    if (!is.null(wd) && all(c("date", "valid") %in% names(wd))) {
      vmap <- stats::setNames(as.integer(wd$valid), as.character(as.Date(wd$date)))
      dm$valid_day <- unname(vmap[as.character(as.Date(dm$date))])
    } else {
      dm$valid_day <- NA_integer_
    }
    cbind(subject_id = .cw_get(r$subject_id, NA_character_),
          file_name = .cw_get(r$name, NA_character_), dm, stringsAsFactors = FALSE)
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) return(NULL)
  do.call(rbind, parts)
}

# Chi-square (Sokolove-Bushell) periodogram spectrum across recordings.
.cw_chisq <- function(results) {
  .cw_bind(results, function(r) {
    cs <- r$chisq
    if (is.null(cs) || is.null(cs$scanned) || length(cs$scanned) == 0) return(NULL)
    data.frame(period_h = cs$scanned, Qp = round(cs$Qp, 2),
               critical = round(cs$critical, 2), stringsAsFactors = FALSE)
  })
}

#' Write the reproducible circadian workbook to `file`.
#'
#' @param file Output .xlsx path.
#' @param results The per-file circadian results list (all_results).
#' @param shared The dashboard shared reactiveValues (files + wear + sleep).
#' @param metric "vm" or "axis1".
#' @keywords internal
circadian_write_workbook <- function(file, results, shared, metric = "vm") {
  wb <- openxlsx::createWorkbook()
  add <- function(name, df) {
    openxlsx::addWorksheet(wb, name)
    if (!is.null(df) && nrow(df) > 0) {
      openxlsx::writeData(wb, name, .cw_relabel(df))
      openxlsx::freezePane(wb, name, firstRow = TRUE)
    } else {
      openxlsx::writeData(wb, name, data.frame(Note = "No data available for this sheet"))
    }
  }

  summary_df <- do.call(rbind, lapply(results, .cw_summary_row, metric = metric))

  add("Summary Metrics",     summary_df)
  add("Daily Metrics",       .cw_daily(results, shared))
  add("Hourly Profile",      .cw_hourly(results))
  add("Minute Profile",      .cw_minute_profile(results, shared, metric))
  add("Periodogram",         .cw_bind(results, function(r) r$periodogram))
  add("Chi-square Periodogram", .cw_chisq(results))
  add("Fractal DFA",         .cw_dfa(results))
  add("Multiscale Entropy",  .cw_mse(results))
  add("Raw Epochs",          .cw_raw_epochs(results, shared, metric))
  add("Data Dictionary",     circadian_data_dictionary())

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}
