# Reproducible multi-sheet XLSX export for the Sedentary tab. Supersedes the
# Summary CSV and Bout-Level CSV: the Summary Metrics sheet carries every summary
# field (+ demographics), and the Bouts sheet ships the full per-bout signal
# substrate (per-axis / VM / steps / lux), so every metric can be recomputed.
# Built from the per-file results list + the dashboard `shared` (for raw data).

# ---- small helpers -----------------------------------------------------------

.sw_get <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) default else x[[1]]
}

.SW_LABELS <- c(
  subject_id = "Subject", file_name = "Filename", cut_points = "Cut Points",
  sleep_excluded = "Sleep Excluded", epoch_length_s = "Epoch Length (s)",
  weight_lbs = "Weight (lbs)", age = "Age", gender = "Gender",
  total_sedentary_hours = "Sedentary Time (hours)", sedentary_percent = "Sedentary (% wear)",
  total_bouts = "Total Bouts", mean_bout_min = "Mean Bout (min)",
  median_bout_min = "Median Bout (min)", max_bout_min = "Max Bout (min)",
  breaks_per_sed_hour = "Breaks per Sedentary Hour",
  ASTP = "ASTP (active->sedentary)", SATP = "SATP (sedentary->active)",
  W25 = "W25 (min)", W50 = "W50 Usual Bout (min)", W75 = "W75 (min)", W90 = "W90 (min)",
  alpha = "Power-law Alpha", alpha_ci_lower = "Alpha 95% CI Lower",
  alpha_ci_upper = "Alpha 95% CI Upper", alpha_gof_pvalue = "Alpha GoF P Value (Clauset)",
  gini = "Gini Index", best_model = "Best-fit Distribution",
  prolonged_bouts_count = "Prolonged Bouts (>=30 min, n)",
  pct_time_20min_bouts = "% Time in >=20 min Bouts",
  pct_time_30min_bouts = "% Time in >=30 min Bouts",
  pct_time_60min_bouts = "% Time in >=60 min Bouts",
  weibull_shape = "Weibull Shape k", weibull_hazard = "Weibull Hazard Direction",
  sedentary_regularity_index = "Sedentary Regularity Index", ABI = "Activity Balance Index",
  # Bouts sheet
  bout_id = "Bout", start_time = "Start", end_time = "End",
  duration_min = "Duration (min)", inter_bout_interval_min = "Break Before (min)",
  n_epochs = "Epochs (n)", calendar_days = "Calendar Days",
  axis1_counts = "Axis 1 Counts", axis2_counts = "Axis 2 Counts", axis3_counts = "Axis 3 Counts",
  axis1_avg = "Axis 1 Avg", axis2_avg = "Axis 2 Avg", axis3_avg = "Axis 3 Avg",
  axis1_max = "Axis 1 Max", axis2_max = "Axis 2 Max", axis3_max = "Axis 3 Max",
  axis1_cpm = "Axis 1 CPM", axis2_cpm = "Axis 2 CPM", axis3_cpm = "Axis 3 CPM",
  vm_counts = "VM Counts", vm_avg = "VM Avg", vm_max = "VM Max", vm_cpm = "VM CPM",
  steps_counts = "Steps", steps_avg = "Steps Avg", steps_max = "Steps Max",
  steps_per_min = "Steps/min", lux_avg = "Lux Avg", lux_max = "Lux Max",
  start_index = "Start Epoch", end_index = "End Epoch",
  # daily / hourly / distribution / breaks
  date = "Date", sedentary_min = "Sedentary (min)", n_bouts = "Bouts (n)",
  mean_bout = "Mean Bout (min)", breaks = "Breaks (n)",
  hour = "Hour of Day", category = "Bout Length", count = "Count", percent = "Percent (%)",
  n_breaks = "Breaks (n)", median_break_min = "Median Break (min)",
  mean_break_min = "Mean Break (min)",
  micro = "Micro <2 min", short = "Short 2-5 min", medium = "Medium 5-15 min",
  long = "Long 15-30 min", extended = "Extended >=30 min",
  column = "Column", definition = "Definition", unit = "Unit",
  # Raw-only columns: the label names the metric and its unit
  metric_sum = "{metric} Sum ({unit})", metric_avg = "{metric} Mean ({unit})",
  metric_max = "{metric} Max ({unit})", metric_per_min = "{metric} per Minute ({unit})"
)

.sw_relabel <- function(df, metric = NULL) {
  if (is.null(df) || ncol(df) == 0) return(df)
  # metric is the raw metric key ("raw:ENMO") when there is one; only the
  # raw-only columns carry tokens
  mn <- if (is.null(metric) || !circ_is_raw_metric(metric)) "Metric" else circ_metric_name(metric)
  un <- if (is.null(metric) || !circ_is_raw_metric(metric)) "" else "mg"
  names(df) <- vapply(names(df), function(n) {
    lab <- if (n %in% names(.SW_LABELS)) .SW_LABELS[[n]]
           else tools::toTitleCase(gsub("_", " ", n))
    lab <- gsub("{metric}", mn, lab, fixed = TRUE)
    trimws(gsub(" ()", "", gsub("{unit}", un, lab, fixed = TRUE), fixed = TRUE))
  }, character(1))
  df
}

.sw_hhmm <- function(x) format(as.POSIXct(x), "%Y-%m-%d %H:%M")

# Inter-bout intervals (active gap before each bout, minutes; first = NA).
.sw_ibi <- function(bouts) {
  n <- nrow(bouts)
  if (n < 2) return(rep(NA_real_, n))
  ibi <- rep(NA_real_, n)
  # end_time is the start of a bout's last epoch, so one epoch is taken off the gap
  ep <- bouts$duration_min / (bouts$end_index - bouts$start_index + 1)
  ibi[2:n] <- as.numeric(difftime(bouts$start_time[2:n], bouts$end_time[1:(n - 1)],
                                  units = "mins")) - ep[1:(n - 1)]
  round(ibi, 1)
}

# ---- per-sheet builders ------------------------------------------------------

.sw_summary_row <- function(r, shared, metric) {
  fr <- r$fragmentation
  sf <- if (!is.null(r$file_id)) shared$files[[r$file_id]] else NULL
  subj <- if (!is.null(sf)) sf$subject_info else NULL
  dur <- if (!is.null(fr$bouts)) fr$bouts$duration_min else numeric(0)
  abi <- tryCatch(canhrActi::activity.balance.index(dur)$ABI, error = function(e) NA_real_)
  bm <- if (!is.null(fr$distribution_fit)) fr$distribution_fit$best_model else NA_character_
  prolonged_n <- if (!is.null(fr$prolonged_summary))
    fr$prolonged_summary$n_bouts[fr$prolonged_summary$threshold == 30] else NA_integer_
  data.frame(
    subject_id = .sw_get(r$subject_id, NA_character_),
    file_name = .sw_get(r$name, NA_character_),
    cut_points = metric,
    sleep_excluded = isTRUE(r$sleep_excluded),
    # The run's own epoch first; shared$files has no entry for a raw id
    epoch_length_s = .sw_get(r$parameters$epoch_length %||%
                             (if (!is.null(sf)) sf$epoch_length else NA_real_)),
    weight_lbs = .sw_get(if (!is.null(subj)) subj$weight_lbs else NA_real_),
    age = .sw_get(if (!is.null(subj)) subj$age else NA_real_),
    gender = .sw_get(if (!is.null(subj)) subj$sex else NA_character_, NA_character_),
    total_sedentary_hours = round(.sw_get(fr$total_sedentary_min) / 60, 2),
    sedentary_percent = .sw_get(fr$sedentary_percent),
    total_bouts = .sw_get(fr$total_bouts, NA_integer_),
    mean_bout_min = .sw_get(fr$mean_bout_duration),
    median_bout_min = .sw_get(fr$median_bout_duration),
    max_bout_min = .sw_get(fr$max_bout_duration),
    breaks_per_sed_hour = .sw_get(fr$breaks_per_sed_hour),
    ASTP = .sw_get(fr$ASTP), SATP = .sw_get(fr$SATP),
    W25 = .sw_get(fr$W25), W50 = .sw_get(fr$W50), W75 = .sw_get(fr$W75), W90 = .sw_get(fr$W90),
    alpha = .sw_get(fr$alpha), alpha_ci_lower = .sw_get(fr$alpha_ci_lower),
    alpha_ci_upper = .sw_get(fr$alpha_ci_upper),
    alpha_gof_pvalue = .sw_get(fr$alpha_gof_pvalue),
    gini = .sw_get(fr$gini), best_model = .sw_get(bm, NA_character_),
    prolonged_bouts_count = .sw_get(prolonged_n, NA_integer_),
    pct_time_20min_bouts = .sw_get(fr$pct_time_20min_bouts),
    pct_time_30min_bouts = .sw_get(fr$pct_time_30min_bouts),
    pct_time_60min_bouts = .sw_get(fr$pct_time_60min_bouts),
    weibull_shape = .sw_get(fr$weibull_shape),
    weibull_hazard = .sw_get(fr$weibull_hazard, NA_character_),
    sedentary_regularity_index = .sw_get(fr$sedentary_regularity_index),
    ABI = round(.sw_get(abi), 3),
    stringsAsFactors = FALSE
  )
}

# How this workbook was produced, read off the stored run and never off a
# live control.
.sw_provenance <- function(results, shared, cut_label, metric = NULL) {
  p <- if (length(results)) results[[1]]$parameters else NULL
  na <- function(x) if (is.null(x) || length(x) != 1 || is.na(x)) "not recorded" else as.character(x)
  is_raw <- identical(p$family, "raw")
  rows <- list(
    c("Exported",              format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    c("Analysis run at",       na(p$run_at)),
    c("Package version",       na(p$package_version)),
    c("Recordings in export",  as.character(length(results))),
    c("Recording type",        if (is_raw) "Raw acceleration (.gt3x)" else "Activity counts (.agd)"),
    c("Metric",                if (is.null(metric)) "Axis 1 (ActiGraph counts)"
                               else circ_metric_provenance(metric)),
    c("Unit",                  if (is_raw) "mg" else "counts"),
    c("Cut point",             na(cut_label)),
    c("Sedentary threshold",   na(p$threshold)),
    c("Epoch length (s)",      na(p$epoch_length)),
    c("Prolonged bout (min)",  na(p$prolonged_threshold)),
    c("Gap bridging (min)",    na(p$min_break_length)),
    c("Sleep window excluded", if (isTRUE(p$sleep_excluded)) "yes" else "no"),
    c("Wear mask applied",     if (isTRUE(p$wear_time_available)) "yes" else "no"),
    # Name a source only when a mask was applied
    c("Wear source",           if (!isTRUE(p$wear_time_available)) "none applied"
                               else if (is_raw) "GGIR part 2" else "Wear time tab"),
    # Where the waking-hours window came from, as the adapter reported it.
    c("Sleep source",          if (isTRUE(p$include_sleep)) "sleep counted as sedentary"
                               else switch(p$sleep_source %||% "none",
                                           "none" = "none applied",
                                           "Cole-Kripke fallback" = "Cole-Kripke, scored in the Sedentary tab",
                                           p$sleep_source))
  )
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df) <- c("Setting", "Value")
  df
}

# Full per-bout signal substrate (supersedes the Bout-Level CSV).
.sw_bouts <- function(results, shared) {
  parts <- lapply(results, function(r) {
    b <- r$fragmentation$bouts
    if (is.null(b) || nrow(b) == 0 || is.null(r$file_id)) return(NULL)
    fid <- r$file_id
    sf <- shared$files[[fid]]
    # A .gt3x has no $data frame; its series comes from the adapter as one
    # metric column
    is_raw <- is.null(sf) || is.null(sf$data)
    if (is_raw) {
      # The cut point names the metric it was validated on, so the series
      # rebuilt here is the one the bouts were detected in
      m <- sed_metric_for(shared, r$parameters$cut_points, "raw")
      s <- .cw_epoch_series(shared, fid, r, m)
      if (is.null(s) || !identical(s$kind, "raw")) return(NULL)
      data <- data.frame(metric = s$value)
      epl <- s$epoch_length %||% 60
      # An index is already a row of the raw series
      kk <- 1L
    } else {
      data <- sf$data
      # A bout index counts epochs of the scored series, coarsened to the
      # coarsest epoch in the study; sf$data is at the file's native epoch.
      # The index is mapped into native rows and the per-minute columns
      # divide by the native epoch.
      e0 <- suppressWarnings(as.numeric(sf$epoch_length %||% 60))
      if (!is.finite(e0) || e0 <= 0) e0 <- 60
      epl <- e0
      run_ep <- suppressWarnings(as.numeric(r$parameters$epoch_length %||% e0))
      kk <- if (is.finite(run_ep) && run_ep > 0) max(1L, as.integer(round(run_ep / e0))) else 1L
    }
    ibi <- .sw_ibi(b)
    has <- function(col) col %in% names(data)
    rows <- lapply(seq_len(nrow(b)), function(i) {
      si <- b$start_index[i]; ei <- b$end_index[i]
      bd <- data[seq.int((si - 1L) * kk + 1L, min(nrow(data), ei * kk)), , drop = FALSE]
      vm <- if (all(c("axis1", "axis2", "axis3") %in% names(bd)))
        sqrt(bd$axis1^2 + bd$axis2^2 + bd$axis3^2) else NULL
      # Reducers that return NA (not -Inf / NaN) when a bout slice is all-NA for a column.
      red <- function(x, f) { v <- suppressWarnings(f(x, na.rm = TRUE)); if (is.finite(v)) v else NA_real_ }
      ax <- function(col, f) if (has(col)) round(red(bd[[col]], f), 1) else NA_real_
      cpm <- function(x) round(red(x, mean) * 60 / epl, 1)
      # An .agd gives three axes, VM, steps and lux; a .gt3x the one scored metric
      base <- data.frame(
        subject_id = .sw_get(r$subject_id, NA_character_),
        file_name = .sw_get(r$name, NA_character_),
        bout_id = b$bout_id[i],
        start_time = .sw_hhmm(b$start_time[i]), end_time = .sw_hhmm(b$end_time[i]),
        duration_min = round(b$duration_min[i], 2),
        inter_bout_interval_min = ibi[i],
        n_epochs = nrow(bd),
        calendar_days = as.numeric(as.Date(format(b$end_time[i], "%Y-%m-%d")) -
                                     as.Date(format(b$start_time[i], "%Y-%m-%d"))) + 1,
        stringsAsFactors = FALSE)
      sig <- if (is_raw) data.frame(
        metric_sum = ax("metric", sum), metric_avg = ax("metric", mean),
        metric_max = ax("metric", max), metric_per_min = cpm(bd$metric),
        stringsAsFactors = FALSE
      ) else data.frame(
        axis1_counts = ax("axis1", sum), axis2_counts = ax("axis2", sum), axis3_counts = ax("axis3", sum),
        axis1_avg = ax("axis1", mean), axis2_avg = ax("axis2", mean), axis3_avg = ax("axis3", mean),
        axis1_max = ax("axis1", max), axis2_max = ax("axis2", max), axis3_max = ax("axis3", max),
        axis1_cpm = if (has("axis1")) cpm(bd$axis1) else NA_real_,
        axis2_cpm = if (has("axis2")) cpm(bd$axis2) else NA_real_,
        axis3_cpm = if (has("axis3")) cpm(bd$axis3) else NA_real_,
        vm_counts = if (!is.null(vm)) round(red(vm, sum), 1) else NA_real_,
        vm_avg = if (!is.null(vm)) round(red(vm, mean), 1) else NA_real_,
        vm_max = if (!is.null(vm)) round(red(vm, max), 1) else NA_real_,
        vm_cpm = if (!is.null(vm)) cpm(vm) else NA_real_,
        steps_counts = ax("steps", sum), steps_avg = ax("steps", mean), steps_max = ax("steps", max),
        steps_per_min = if (has("steps")) cpm(bd$steps) else NA_real_,
        lux_avg = ax("lux", mean), lux_max = ax("lux", max),
        stringsAsFactors = FALSE
      )
      cbind(base, sig,
            data.frame(start_index = si, end_index = ei, stringsAsFactors = FALSE))
    })
    do.call(rbind, rows)
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) return(NULL)
  do.call(rbind, parts)
}

.sw_daily <- function(results) {
  parts <- lapply(results, function(r) {
    d <- r$fragmentation$daily_fragmentation
    if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) return(NULL)
    cbind(subject_id = .sw_get(r$subject_id, NA_character_),
          file_name = .sw_get(r$name, NA_character_), d, stringsAsFactors = FALSE)
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) return(NULL)
  do.call(rbind, parts)
}

.sw_distribution <- function(results) {
  parts <- lapply(results, function(r) {
    d <- r$fragmentation$bout_distribution
    if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) return(NULL)
    cbind(subject_id = .sw_get(r$subject_id, NA_character_),
          file_name = .sw_get(r$name, NA_character_),
          d[, c("category", "count", "percent")], stringsAsFactors = FALSE)
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) return(NULL)
  do.call(rbind, parts)
}

# Per-hour bout count + sedentary minutes, attributed to each bout's start hour.
.sw_hourly <- function(results) {
  parts <- lapply(results, function(r) {
    b <- r$fragmentation$bouts
    if (is.null(b) || nrow(b) == 0) return(NULL)
    hr <- as.integer(format(as.POSIXct(b$start_time), "%H"))
    agg <- tapply(b$duration_min, factor(hr, levels = 0:23), sum)
    cnt <- tapply(b$duration_min, factor(hr, levels = 0:23), length)
    data.frame(
      subject_id = .sw_get(r$subject_id, NA_character_),
      file_name = .sw_get(r$name, NA_character_),
      hour = 0:23,
      n_bouts = as.integer(ifelse(is.na(cnt), 0, cnt)),
      sedentary_min = round(as.numeric(ifelse(is.na(agg), 0, agg)), 1),
      stringsAsFactors = FALSE
    )
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) return(NULL)
  do.call(rbind, parts)
}

# Break (inter-bout interval) taxonomy per subject.
.sw_breaks <- function(results) {
  parts <- lapply(results, function(r) {
    b <- r$fragmentation$bouts
    if (is.null(b) || nrow(b) < 2) return(NULL)
    ibi <- .sw_ibi(b)
    ibi <- ibi[is.finite(ibi) & ibi > 0]
    if (length(ibi) == 0) return(NULL)
    data.frame(
      subject_id = .sw_get(r$subject_id, NA_character_),
      file_name = .sw_get(r$name, NA_character_),
      n_breaks = length(ibi),
      median_break_min = round(stats::median(ibi), 1),
      mean_break_min = round(mean(ibi), 1),
      micro = sum(ibi < 2), short = sum(ibi >= 2 & ibi < 5),
      medium = sum(ibi >= 5 & ibi < 15), long = sum(ibi >= 15 & ibi < 30),
      extended = sum(ibi >= 30),
      stringsAsFactors = FALSE
    )
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) return(NULL)
  do.call(rbind, parts)
}

#' Short data dictionary for the sedentary workbook.
#' @keywords internal
sedentary_data_dictionary <- function(metric = NULL) {
  is_raw <- !is.null(metric) && circ_is_raw_metric(metric)
  d <- function(column, definition, unit) {
    data.frame(column = column, definition = definition, unit = unit, stringsAsFactors = FALSE)
  }
  rbind(
    d("Sedentary Time", "Time below the sedentary cut point during valid wear (and waking, if sleep excluded)", "hours / % wear"),
    d("Bout / Break", "An uninterrupted sedentary period; a break is the active gap between two bouts (Healy 2008). Short active gaps below the Gap Bridging threshold are merged", "min / count"),
    d("ASTP / SATP", "Active->sedentary and sedentary->active transition probabilities = 1 / mean bout length in epochs (Wanigatunga 2019)", "0 to 1"),
    d("W25 / W50 / W75 / W90", "Time-weighted usual bout duration: bout length below which 25/50/75/90% of total sedentary time accumulates (Chastin & Granat 2010)", "minutes"),
    d("Power-law Alpha + 95% CI", "Exponent of the bout-duration distribution via Clauset MLE with KS-optimised xmin; lower = fewer, longer bouts", "unitless"),
    d("Alpha GoF P Value", "Clauset (2009) semiparametric bootstrap goodness-of-fit; p < 0.05 rejects the power law", "0 to 1"),
    d("Gini Index", "Inequality of bout-duration distribution (finite-sample corrected); higher = time concentrated in few long bouts", "0 to 1"),
    d("Prolonged Bouts / % Time", "Count and share of sedentary time in prolonged bouts (SBRN; >=30 min is the field convention; >=20/>=60 also reported)", "n / %"),
    d("Weibull Shape k", "Shape of the bout-duration survival model: k<1 decreasing, ~1 memoryless, >1 increasing hazard", "unitless"),
    d("Sedentary Regularity Index", "Day-to-day concordance of the sedentary/active state 24 h apart", "-100 to 100"),
    d("Activity Balance Index", "Time in short (<10 min) vs prolonged (>=30 min) sedentary bouts", "0 to 1"),
    if (is_raw)
      d(paste0("Bouts sheet (", circ_metric_name(metric), ")"),
        paste0("Per-bout signal over the bout's epochs: sum, mean, max and per-minute ",
               "of ", circ_metric_name(metric), ". A .gt3x is summarised to one metric ",
               "per epoch, so there are no separate axes, steps or lux columns as there ",
               "are for an .agd. Break Before = inter-bout interval (NA for the first bout)"),
        "mg / min")
    else
      d("Bouts sheet (Axis/VM/Steps/Lux)", "Per-bout raw signal over the bout's epochs: counts (sum), average, max and counts-per-minute for each axis and vector magnitude; steps and lux when present. Break Before = inter-bout interval (NA for the first bout)", "counts / cpm / min / lux"),
    stringsAsFactors = FALSE
  )
}

#' The Summary Metrics sheet, as a data frame.
#'
#' One definition of the per-recording metric set, used by both the workbook
#' sheet and the table on the page.
#' @keywords internal
sedentary_metrics_df <- function(results, shared, metric = "not recorded") {
  if (is.null(results) || length(results) == 0) return(NULL)
  df <- do.call(rbind, lapply(results, .sw_summary_row, shared = shared, metric = metric))
  .sw_relabel(df)
}

#' Write the reproducible sedentary workbook to `file`.
#' @keywords internal
sedentary_write_workbook <- function(file, results, shared, metric = "not recorded") {
  # The run's metric, read off the stored results and derived from the cut
  # point the same way the Bouts sheet derives its series. NULL on a counts run.
  run_metric <- tryCatch({
    p <- results[[1]]$parameters
    if (identical(p$family, "raw")) sed_metric_for(shared, p$cut_points, "raw") else NULL
  }, error = function(e) NULL)
  wb <- openxlsx::createWorkbook()
  add <- function(name, df) {
    openxlsx::addWorksheet(wb, name)
    if (!is.null(df) && nrow(df) > 0) {
      openxlsx::writeData(wb, name, if (identical(name, "Summary Metrics")) df else .sw_relabel(df, run_metric))
      openxlsx::freezePane(wb, name, firstRow = TRUE)
    } else {
      openxlsx::writeData(wb, name, data.frame(Note = "No data available for this sheet"))
    }
  }

  summary_df <- sedentary_metrics_df(results, shared, metric)

  add("Provenance",     .sw_provenance(results, shared, metric, run_metric))
  add("Summary Metrics", summary_df)
  add("Bouts", .sw_bouts(results, shared))
  add("Daily Fragmentation", .sw_daily(results))
  add("Hourly Fragmentation", .sw_hourly(results))
  add("Bout Distribution", .sw_distribution(results))
  add("Break Patterns", .sw_breaks(results))
  add("Data Dictionary", sedentary_data_dictionary(run_metric))

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}
