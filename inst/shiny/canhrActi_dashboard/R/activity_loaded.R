# Activity: the loaded page. The Summary and Daily frames are built here for
# both the table and the CSV export, from the module results and the shared
# store.

# Matthews' lifestyle band (760 to 1951 counts per minute) is light, as the
# package's light_activity() counts it; the tables have no column for it
act_light_lifestyle <- function(x) {
  if (is.factor(x)) {
    levels(x)[levels(x) == "lifestyle"] <- "light"
  } else {
    x[x %in% "lifestyle"] <- "light"
  }
  x
}

# Every time column is in minutes at any epoch length
act_min <- function(n_epochs, epoch_sec) round(n_epochs * epoch_sec / 60, 2)

# All 66 columns of the Summary export, one row per recording.
act_summary_df <- function(res, shared, bout_min = 10) {
  all_rows <- list()
  for (r in res) {
    f <- shared$files[[r$file_id]]
    data <- f$data
    weight <- f$subject_info$weight_lbs %||% 0
    age <- f$subject_info$age %||% 0
    gender <- f$subject_info$sex %||% ""
    epoch_sec <- f$epoch_length

    algo <- r$parameters$cut_points %||% "freedson"

    # Wear mask, all worn when no wear time analysis was run; a day the wear
    # time page marked invalid is non-wear throughout
    wear_result <- shared$results$wear_time[[r$file_id]]
    wear_mask <- if (!is.null(wear_result) && !is.null(wear_result$wear)) {
      wear_result$wear
    } else {
      rep(TRUE, nrow(data))
    }

    if (!is.null(wear_result) && !is.null(wear_result$daily) && "timestamp" %in% names(data)) {
      daily_valid <- wear_result$daily
      data_dates <- as.Date(data$timestamp)
      for (d in seq_len(nrow(daily_valid))) {
        if (!daily_valid$valid[d]) {
          day_date <- as.Date(daily_valid$date[d])
          wear_mask[data_dates == day_date] <- FALSE
        }
      }
    }

    # Non-wear epochs are NA
    intensity <- r$intensity_valid
    if (length(intensity) == length(wear_mask)) {
      intensity[!wear_mask] <- NA
    }

    n_epochs <- length(intensity)
    n_wear_epochs <- sum(!is.na(intensity))

    axis1 <- data$axis1
    axis2 <- if ("axis2" %in% names(data)) data$axis2 else rep(0, nrow(data))
    axis3 <- if ("axis3" %in% names(data)) data$axis3 else rep(0, nrow(data))
    steps <- if ("steps" %in% names(data)) data$steps else rep(0, nrow(data))
    lux <- if ("lux" %in% names(data)) data$lux else rep(0, nrow(data))
    vm <- sqrt(axis1^2 + axis2^2 + axis3^2)

    # Hours with any wear
    n_days <- r$n_days
    n_hours <- if ("timestamp" %in% names(data) && sum(wear_mask) > 0) {
      length(unique(paste(as.Date(data$timestamp[wear_mask]), format(data$timestamp[wear_mask], "%H"))))
    } else if (n_wear_epochs > 0) {
      n_wear_epochs * epoch_sec / 3600
    } else 0

    # Epoch counts, converted to minutes below so the "Minutes at each
    # intensity" columns hold at every epoch length
    per_min <- epoch_sec / 60
    n_sedentary <- sum(intensity == "sedentary", na.rm = TRUE)
    n_light <- sum(intensity == "light", na.rm = TRUE)
    n_moderate <- sum(intensity == "moderate", na.rm = TRUE)
    n_vigorous <- sum(intensity == "vigorous", na.rm = TRUE)
    n_very_vigorous <- sum(intensity == "very_vigorous", na.rm = TRUE)
    n_total_mvpa <- n_moderate + n_vigorous + n_very_vigorous

    total_mvpa <- n_total_mvpa * per_min

    # Percentages are epochs over worn epochs, 0 when nothing was worn
    pct_sed <- if (n_wear_epochs > 0) 100 * n_sedentary / n_wear_epochs else 0
    pct_light <- if (n_wear_epochs > 0) 100 * n_light / n_wear_epochs else 0
    pct_mod <- if (n_wear_epochs > 0) 100 * n_moderate / n_wear_epochs else 0
    pct_vig <- if (n_wear_epochs > 0) 100 * n_vigorous / n_wear_epochs else 0
    pct_vvig <- if (n_wear_epochs > 0) 100 * n_very_vigorous / n_wear_epochs else 0
    pct_mvpa <- if (n_wear_epochs > 0) 100 * n_total_mvpa / n_wear_epochs else 0

    avg_mvpa_per_day <- if (n_days > 0) total_mvpa / n_days else 0

    # Energy over the worn epochs only. NA when METs were not computed: the
    # table shows a dash and the CSV an empty cell, where 0 and 1 would read as
    # measurements
    kcals <- NA_real_
    mets_avg <- NA_real_
    if (!is.null(r$mets) && length(r$mets) > 0) {
      worn_mets <- if (length(r$mets) == length(wear_mask)) r$mets[wear_mask] else r$mets
      mets_avg <- mean(worn_mets, na.rm = TRUE)
      weight_kg <- weight * 0.453592
      kcals <- mets_avg * weight_kg * n_wear_epochs * epoch_sec / 3600
    } else if (!is.null(r$kcal_epochs) && length(r$kcal_epochs) == length(wear_mask)) {
      kcals <- sum(r$kcal_epochs[wear_mask], na.rm = TRUE)
    } else if (!is.na(r$total_ee)) {
      kcals <- r$total_ee
    }
    avg_kcals_per_day <- if (n_days > 0) kcals / n_days else 0
    avg_kcals_per_hour <- if (n_hours > 0) kcals / n_hours else 0

    # MVPA bouts
    is_mvpa <- intensity %in% c("moderate", "vigorous", "very_vigorous")
    mvpa_bouts <- rle(is_mvpa)
    bout_starts <- cumsum(c(1, head(mvpa_bouts$lengths, -1)))
    bout_ends <- cumsum(mvpa_bouts$lengths)

    bout_info <- data.frame(
      start = bout_starts[mvpa_bouts$values],
      end = bout_ends[mvpa_bouts$values],
      length = mvpa_bouts$lengths[mvpa_bouts$values]
    )
    bout_min_epochs <- as.numeric(bout_min %||% 10) * (60 / epoch_sec)
    bout_info <- bout_info[bout_info$length >= bout_min_epochs, ]

    n_mvpa_bouts <- nrow(bout_info)
    total_mvpa_bout_time <- if (n_mvpa_bouts > 0) sum(bout_info$length) else 0
    avg_mvpa_bout_time <- if (n_mvpa_bouts > 0) mean(bout_info$length) else 0
    max_mvpa_bout_time <- if (n_mvpa_bouts > 0) max(bout_info$length) else 0
    min_mvpa_bout_time <- if (n_mvpa_bouts > 0) min(bout_info$length) else 0

    total_mvpa_bout_counts <- 0
    if (n_mvpa_bouts > 0) {
      for (b in seq_len(nrow(bout_info))) {
        total_mvpa_bout_counts <- total_mvpa_bout_counts + sum(axis1[bout_info$start[b]:bout_info$end[b]], na.rm = TRUE)
      }
    }

    # Sedentary bouts
    is_sed <- !is.na(intensity) & intensity == "sedentary"
    sed_bouts_rle <- rle(is_sed)
    sed_bout_starts <- cumsum(c(1, head(sed_bouts_rle$lengths, -1)))
    sed_bout_ends <- cumsum(sed_bouts_rle$lengths)
    sed_valid <- which(sed_bouts_rle$values == TRUE)
    sed_bout_info <- if (length(sed_valid) > 0) {
      data.frame(
        start = sed_bout_starts[sed_valid],
        end = sed_bout_ends[sed_valid],
        length = sed_bouts_rle$lengths[sed_valid]
      )
    } else {
      data.frame(start = integer(0), end = integer(0), length = integer(0))
    }

    n_sed_bouts <- nrow(sed_bout_info)
    total_sed_bout_time <- if (n_sed_bouts > 0) sum(sed_bout_info$length) else 0
    avg_sed_bout_length <- if (n_sed_bouts > 0) mean(sed_bout_info$length) else 0
    max_sed_bout_length <- if (n_sed_bouts > 0) max(sed_bout_info$length) else 0
    min_sed_bout_length <- if (n_sed_bouts > 0) min(sed_bout_info$length) else 0
    daily_avg_sed_bouts <- if (n_days > 0) n_sed_bouts / n_days else 0

    # Sedentary breaks: worn, non-sedentary runs
    is_break <- !is.na(intensity) & intensity != "sedentary"
    break_bouts_rle <- rle(is_break)
    break_bout_starts <- cumsum(c(1, head(break_bouts_rle$lengths, -1)))
    break_bout_ends <- cumsum(break_bouts_rle$lengths)
    break_valid <- which(break_bouts_rle$values == TRUE)
    break_bout_info <- if (length(break_valid) > 0) {
      data.frame(
        start = break_bout_starts[break_valid],
        end = break_bout_ends[break_valid],
        length = break_bouts_rle$lengths[break_valid]
      )
    } else {
      data.frame(start = integer(0), end = integer(0), length = integer(0))
    }

    n_breaks <- nrow(break_bout_info)
    total_break_time <- if (n_breaks > 0) sum(break_bout_info$length) else 0
    avg_break_length <- if (n_breaks > 0) mean(break_bout_info$length) else 0
    max_break_length <- if (n_breaks > 0) max(break_bout_info$length) else 0
    min_break_length <- if (n_breaks > 0) min(break_bout_info$length) else 0
    daily_avg_breaks <- if (n_days > 0) n_breaks / n_days else 0

    # Count statistics over worn epochs only
    if (n_wear_epochs > 0) {
      w_axis1 <- axis1[wear_mask]
      w_axis2 <- axis2[wear_mask]
      w_axis3 <- axis3[wear_mask]
      w_steps <- steps[wear_mask]
      w_lux <- lux[wear_mask]
      w_vm <- vm[wear_mask]

      axis1_counts <- sum(w_axis1, na.rm = TRUE)
      axis2_counts <- sum(w_axis2, na.rm = TRUE)
      axis3_counts <- sum(w_axis3, na.rm = TRUE)

      axis1_avg <- mean(w_axis1, na.rm = TRUE)
      axis2_avg <- mean(w_axis2, na.rm = TRUE)
      axis3_avg <- mean(w_axis3, na.rm = TRUE)

      axis1_max <- max(w_axis1, na.rm = TRUE)
      axis2_max <- max(w_axis2, na.rm = TRUE)
      axis3_max <- max(w_axis3, na.rm = TRUE)

      axis1_cpm <- axis1_avg * (60 / epoch_sec)
      axis2_cpm <- axis2_avg * (60 / epoch_sec)
      axis3_cpm <- axis3_avg * (60 / epoch_sec)

      vm_counts <- sum(w_vm, na.rm = TRUE)
      vm_avg <- mean(w_vm, na.rm = TRUE)
      vm_max <- max(w_vm, na.rm = TRUE)
      vm_cpm <- vm_avg * (60 / epoch_sec)

      steps_counts <- sum(w_steps, na.rm = TRUE)
      steps_avg <- mean(w_steps, na.rm = TRUE)
      steps_max <- max(w_steps, na.rm = TRUE)
      steps_per_min <- steps_avg * (60 / epoch_sec)

      lux_avg <- mean(w_lux, na.rm = TRUE)
      lux_max <- max(w_lux, na.rm = TRUE)

      time_min <- n_wear_epochs * epoch_sec / 60
    } else {
      axis1_counts <- axis2_counts <- axis3_counts <- 0
      axis1_avg <- axis2_avg <- axis3_avg <- 0
      axis1_max <- axis2_max <- axis3_max <- 0
      axis1_cpm <- axis2_cpm <- axis3_cpm <- 0
      vm_counts <- vm_avg <- vm_max <- vm_cpm <- 0
      steps_counts <- steps_avg <- steps_max <- steps_per_min <- 0
      lux_avg <- lux_max <- 0
      time_min <- 0
    }

    row_data <- data.frame(
      Subject = r$subject_id,
      Filename = r$name,
      Epoch = epoch_sec,
      `Weight (lbs)` = weight,
      Age = age,
      Gender = gender,
      kcals = round(kcals, 3),
      `Average kcals per day` = round(avg_kcals_per_day, 3),
      `Average kcals per hour` = round(avg_kcals_per_hour, 3),
      METs = round(mets_avg, 3),
      # MVPA Bout statistics, times in minutes
      `MVPA Bouts` = n_mvpa_bouts,
      `Total Time in MVPA Bouts` = act_min(total_mvpa_bout_time, epoch_sec),
      `Avg Time per MVPA Bout` = round(avg_mvpa_bout_time * epoch_sec / 60, 1),
      `Max Time per MVPA Bout` = act_min(max_mvpa_bout_time, epoch_sec),
      `Min Time per MVPA Bout` = act_min(min_mvpa_bout_time, epoch_sec),
      `Total Counts in MVPA Bouts` = total_mvpa_bout_counts,
      # Sedentary Bout statistics
      `Total Sedentary Bouts` = n_sed_bouts,
      `Total Time in Sedentary Bouts` = act_min(total_sed_bout_time, epoch_sec),
      `Average Length of Sedentary Bouts` = round(avg_sed_bout_length * epoch_sec / 60, 1),
      `Maximum Length of Sedentary Bouts` = act_min(max_sed_bout_length, epoch_sec),
      `Minimum Length of Sedentary Bouts` = act_min(min_sed_bout_length, epoch_sec),
      `Daily Average of Sedentary Bouts` = round(daily_avg_sed_bouts, 1),
      # Sedentary Break statistics
      `Total Sedentary Breaks` = n_breaks,
      `Total Time in Sedentary Breaks` = act_min(total_break_time, epoch_sec),
      `Average length of Sedentary Breaks` = round(avg_break_length * epoch_sec / 60, 1),
      `Max Length of Sedentary Breaks` = act_min(max_break_length, epoch_sec),
      `Minimum Length of Sedentary Breaks` = act_min(min_break_length, epoch_sec),
      `Daily Average of Sedentary Breaks` = round(daily_avg_breaks, 1),
      # Intensity minutes
      Sedentary = act_min(n_sedentary, epoch_sec),
      Light = act_min(n_light, epoch_sec),
      Moderate = act_min(n_moderate, epoch_sec),
      Vigorous = act_min(n_vigorous, epoch_sec),
      `Very Vigorous` = act_min(n_very_vigorous, epoch_sec),
      # Percentages
      `% in Sedentary` = sprintf("%.2f%%", pct_sed),
      `% in Light` = sprintf("%.2f%%", pct_light),
      `% in Moderate` = sprintf("%.2f%%", pct_mod),
      `% in Vigorous` = sprintf("%.2f%%", pct_vig),
      `% in Very Vigorous` = sprintf("%.2f%%", pct_vvig),
      `Total MVPA` = act_min(n_total_mvpa, epoch_sec),
      `% in MVPA` = sprintf("%.2f%%", pct_mvpa),
      `Average MVPA Per day` = round(avg_mvpa_per_day, 1),
      # Axis counts
      `Axis 1 Counts` = axis1_counts,
      `Axis 2 Counts` = axis2_counts,
      `Axis 3 Counts` = axis3_counts,
      `Axis 1 Average Counts` = round(axis1_avg, 1),
      `Axis 2 Average Counts` = round(axis2_avg, 1),
      `Axis 3 Average Counts` = round(axis3_avg, 1),
      `Axis 1 Max Counts` = axis1_max,
      `Axis 2 Max Counts` = axis2_max,
      `Axis 3 Max Counts` = axis3_max,
      `Axis 1 CPM` = round(axis1_cpm, 1),
      `Axis 2 CPM` = round(axis2_cpm, 1),
      `Axis 3 CPM` = round(axis3_cpm, 1),
      # Vector Magnitude
      `Vector Magnitude Counts` = round(vm_counts, 1),
      `Vector Magnitude Average Counts` = round(vm_avg, 1),
      `Vector Magnitude Max Counts` = round(vm_max, 1),
      `Vector Magnitude CPM` = round(vm_cpm, 1),
      # Steps
      `Steps Counts` = steps_counts,
      `Steps Average Counts` = round(steps_avg, 1),
      `Steps Max Counts` = steps_max,
      `Steps Per Minute` = round(steps_per_min, 1),
      # Lux
      `Lux Average Counts` = round(lux_avg, 1),
      `Lux Max Counts` = lux_max,
      # Metadata (using wear time epochs)
      `Number of Epochs` = n_wear_epochs,
      Time = act_min(n_wear_epochs, epoch_sec),
      `Calendar Days` = n_days,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    all_rows[[length(all_rows) + 1]] <- row_data
  }

  df <- do.call(rbind, all_rows)
  df
}

# Everything the per-day arithmetic needs from one recording: the epoch table
# with its date, the wear mask with invalid days blanked, the intensity of
# every epoch and the three bout tables. act_daily_df() slices it by day and
# act_window_daily_df() by day and window. NULL when there are no timestamps.
act_ctx <- function(r, shared, bout_min = 10) {
  f <- shared$files[[r$file_id]]
  data <- f$data
  epoch_sec <- f$epoch_length

  weight <- f$subject_info$weight_lbs %||% 0
  age <- f$subject_info$age %||% 0
  gender <- f$subject_info$sex %||% ""

  algo <- r$parameters$cut_points %||% "freedson"

  wear_result <- shared$results$wear_time[[r$file_id]]
  wear_mask <- if (!is.null(wear_result) && !is.null(wear_result$wear)) {
    wear_result$wear
  } else {
    rep(TRUE, nrow(data))
  }

  daily_valid <- NULL
  if (!is.null(wear_result) && !is.null(wear_result$daily)) {
    daily_valid <- wear_result$daily
  }

  if (!("timestamp" %in% names(data))) return(NULL)
  data$date <- as.Date(data$timestamp)

  # An invalid day is non-wear throughout
  if (!is.null(daily_valid)) {
    for (d in seq_len(nrow(daily_valid))) {
      if (!daily_valid$valid[d]) {
        day_date <- as.Date(daily_valid$date[d])
        wear_mask[data$date == day_date] <- FALSE
      }
    }
  }

  axis1 <- data$axis1

  # The per-minute series the run classified, axis 1 or vector magnitude.
  # Non-wear is NA before classification, so it is never sedentary
  all_cpm <- r$activity_data
  if (length(all_cpm) != nrow(data)) all_cpm <- canhrActi::to_cpm(axis1, epoch_sec)
  all_cpm[!wear_mask] <- NA
  all_intensity <- tryCatch({
    act_light_lifestyle(canhrActi::apply_cutpoints(all_cpm, algo))
  }, error = function(e) rep(NA_character_, nrow(data)))
  all_intensity[!wear_mask] <- NA

  # MVPA bouts over the whole recording
  is_mvpa <- all_intensity %in% c("moderate", "vigorous", "very_vigorous")
  mvpa_bouts <- rle(is_mvpa)
  bout_starts <- cumsum(c(1, head(mvpa_bouts$lengths, -1)))
  bout_ends <- cumsum(mvpa_bouts$lengths)

  bout_info <- data.frame(
    start = bout_starts[mvpa_bouts$values],
    end = bout_ends[mvpa_bouts$values],
    length = mvpa_bouts$lengths[mvpa_bouts$values]
  )
  bout_min_epochs <- as.numeric(bout_min %||% 10) * (60 / epoch_sec)
  bout_info <- bout_info[bout_info$length >= bout_min_epochs, ]

  # Sedentary bouts; the !is.na() guard matters, NA == "sedentary" is NA and
  # breaks rle()
  is_sed <- !is.na(all_intensity) & all_intensity == "sedentary"
  sed_bouts_rle <- rle(is_sed)
  sed_bout_starts <- cumsum(c(1, head(sed_bouts_rle$lengths, -1)))
  sed_bout_ends <- cumsum(sed_bouts_rle$lengths)
  sed_valid <- which(sed_bouts_rle$values == TRUE)
  sed_bout_info <- if (length(sed_valid) > 0) {
    data.frame(
      start = sed_bout_starts[sed_valid],
      end = sed_bout_ends[sed_valid],
      length = sed_bouts_rle$lengths[sed_valid]
    )
  } else {
    data.frame(start = integer(0), end = integer(0), length = integer(0))
  }

  # Sedentary breaks: worn, non-sedentary runs
  is_break <- !is.na(all_intensity) & all_intensity != "sedentary"
  break_bouts_rle <- rle(is_break)
  break_bout_starts <- cumsum(c(1, head(break_bouts_rle$lengths, -1)))
  break_bout_ends <- cumsum(break_bouts_rle$lengths)
  break_valid <- which(break_bouts_rle$values == TRUE)
  break_bout_info <- if (length(break_valid) > 0) {
    data.frame(
      start = break_bout_starts[break_valid],
      end = break_bout_ends[break_valid],
      length = break_bouts_rle$lengths[break_valid]
    )
  } else {
    data.frame(start = integer(0), end = integer(0), length = integer(0))
  }

  list(data = data, epoch_sec = epoch_sec, weight = weight, age = age, gender = gender,
       wear_mask = wear_mask, all_intensity = all_intensity, axis1 = axis1,
       bout_info = bout_info, sed_bout_info = sed_bout_info, break_bout_info = break_bout_info)
}

# All 63 columns of the Daily export, one row per recording and day, plus a
# Day Type column when a schedule is in force. NULL when no recording
# yielded a day.
act_daily_df <- function(res, shared, bout_min = 10, schedule = NULL) {
  all_rows <- list()
  for (r in res) {
    ctx <- act_ctx(r, shared, bout_min)
    if (is.null(ctx)) next
    dates <- unique(ctx$data$date)
    for (date_i in dates) {
      day_indices <- which(ctx$data$date == date_i)
      row_data <- act_span_row(ctx, r, day_indices, date_i)
      if (is.null(row_data)) next
      all_rows[[length(all_rows) + 1]] <- row_data
    }
  }
  if (length(all_rows) == 0) return(NULL)
  out <- do.call(rbind, all_rows)
  if (sched_active(schedule)) out <- act_add_day_type(out, schedule)
  out
}

# One Daily row for one contiguous run of epochs, a whole day or one window
# of it. NULL when the run is empty.
act_span_row <- function(ctx, r, day_indices, date_i) {
  data <- ctx$data
  epoch_sec <- ctx$epoch_sec
  weight <- ctx$weight; age <- ctx$age; gender <- ctx$gender
  wear_mask <- ctx$wear_mask
  all_intensity <- ctx$all_intensity
  axis1 <- ctx$axis1
  bout_info <- ctx$bout_info
  sed_bout_info <- ctx$sed_bout_info
  break_bout_info <- ctx$break_bout_info

        day_data <- data[day_indices, ]
        n_epochs <- nrow(day_data)
        if (n_epochs == 0) return(NULL)

        day_wear <- wear_mask[day_indices]
        n_wear_epochs <- sum(day_wear, na.rm = TRUE)

        d_axis1 <- day_data$axis1
        d_axis2 <- if ("axis2" %in% names(day_data)) day_data$axis2 else rep(0, n_epochs)
        d_axis3 <- if ("axis3" %in% names(day_data)) day_data$axis3 else rep(0, n_epochs)
        d_steps <- if ("steps" %in% names(day_data)) day_data$steps else rep(0, n_epochs)
        d_lux <- if ("lux" %in% names(day_data)) day_data$lux else rep(0, n_epochs)
        d_vm <- sqrt(d_axis1^2 + d_axis2^2 + d_axis3^2)

        day_intensity <- all_intensity[day_indices]

        # Intensity counts over worn epochs
        sedentary <- sum(day_intensity == "sedentary", na.rm = TRUE)
        light <- sum(day_intensity == "light", na.rm = TRUE)
        moderate <- sum(day_intensity == "moderate", na.rm = TRUE)
        vigorous <- sum(day_intensity == "vigorous", na.rm = TRUE)
        very_vigorous <- sum(day_intensity == "very_vigorous", na.rm = TRUE)
        total_mvpa <- moderate + vigorous + very_vigorous

        # Percentages of worn epochs, 0 when nothing was worn
        pct_sed <- if (n_wear_epochs > 0) 100 * sedentary / n_wear_epochs else 0
        pct_light <- if (n_wear_epochs > 0) 100 * light / n_wear_epochs else 0
        pct_mod <- if (n_wear_epochs > 0) 100 * moderate / n_wear_epochs else 0
        pct_vig <- if (n_wear_epochs > 0) 100 * vigorous / n_wear_epochs else 0
        pct_vvig <- if (n_wear_epochs > 0) 100 * very_vigorous / n_wear_epochs else 0
        pct_mvpa <- if (n_wear_epochs > 0) 100 * total_mvpa / n_wear_epochs else 0

        # Hours with any wear
        wear_hours <- if (n_wear_epochs > 0) {
          unique(as.numeric(format(day_data$timestamp[day_wear], "%H")))
        } else numeric(0)
        n_hours <- length(wear_hours)
        avg_mvpa_per_hour <- if (n_hours > 0) total_mvpa * epoch_sec / 60 / n_hours else 0

        # MVPA bouts touching this run
        day_start <- min(day_indices)
        day_end <- max(day_indices)

        bouts_occurring <- if (nrow(bout_info) > 0) {
          bout_info[bout_info$start <= day_end & bout_info$end >= day_start, ]
        } else data.frame()
        n_bouts_occurring <- nrow(bouts_occurring)

        bouts_starting <- if (nrow(bout_info) > 0) {
          bout_info[bout_info$start >= day_start & bout_info$start <= day_end, ]
        } else data.frame()
        n_bouts_starting <- nrow(bouts_starting)

        bouts_ending <- if (nrow(bout_info) > 0) {
          bout_info[bout_info$end >= day_start & bout_info$end <= day_end, ]
        } else data.frame()
        n_bouts_ending <- nrow(bouts_ending)

        total_bout_time <- 0
        total_bout_counts <- 0
        if (nrow(bouts_occurring) > 0) {
          for (b in seq_len(nrow(bouts_occurring))) {
            b_start <- max(bouts_occurring$start[b], day_start)
            b_end <- min(bouts_occurring$end[b], day_end)
            total_bout_time <- total_bout_time + (b_end - b_start + 1)
            total_bout_counts <- total_bout_counts + sum(axis1[b_start:b_end], na.rm = TRUE)
          }
        }

        # Sedentary bouts
        sed_bouts_occurring <- if (nrow(sed_bout_info) > 0) {
          sed_bout_info[sed_bout_info$start <= day_end & sed_bout_info$end >= day_start, ]
        } else data.frame()
        n_sed_bouts_occurring <- nrow(sed_bouts_occurring)

        sed_bouts_starting <- if (nrow(sed_bout_info) > 0) {
          sed_bout_info[sed_bout_info$start >= day_start & sed_bout_info$start <= day_end, ]
        } else data.frame()
        n_sed_bouts_starting <- nrow(sed_bouts_starting)

        sed_bouts_ending <- if (nrow(sed_bout_info) > 0) {
          sed_bout_info[sed_bout_info$end >= day_start & sed_bout_info$end <= day_end, ]
        } else data.frame()
        n_sed_bouts_ending <- nrow(sed_bouts_ending)

        total_sed_bout_time <- 0
        if (nrow(sed_bouts_occurring) > 0) {
          for (b in seq_len(nrow(sed_bouts_occurring))) {
            b_start <- max(sed_bouts_occurring$start[b], day_start)
            b_end <- min(sed_bouts_occurring$end[b], day_end)
            total_sed_bout_time <- total_sed_bout_time + (b_end - b_start + 1)
          }
        }

        # Sedentary breaks
        break_bouts_occurring <- if (nrow(break_bout_info) > 0) {
          break_bout_info[break_bout_info$start <= day_end & break_bout_info$end >= day_start, ]
        } else data.frame()
        n_break_bouts_occurring <- nrow(break_bouts_occurring)

        break_bouts_starting <- if (nrow(break_bout_info) > 0) {
          break_bout_info[break_bout_info$start >= day_start & break_bout_info$start <= day_end, ]
        } else data.frame()
        n_break_bouts_starting <- nrow(break_bouts_starting)

        break_bouts_ending <- if (nrow(break_bout_info) > 0) {
          break_bout_info[break_bout_info$end >= day_start & break_bout_info$end <= day_end, ]
        } else data.frame()
        n_break_bouts_ending <- nrow(break_bouts_ending)

        total_break_time <- 0
        if (nrow(break_bouts_occurring) > 0) {
          for (b in seq_len(nrow(break_bouts_occurring))) {
            b_start <- max(break_bouts_occurring$start[b], day_start)
            b_end <- min(break_bouts_occurring$end[b], day_end)
            total_break_time <- total_break_time + (b_end - b_start + 1)
          }
        }

        # Count statistics over worn epochs only
        if (n_wear_epochs > 0) {
          w_axis1 <- d_axis1[day_wear]
          w_axis2 <- d_axis2[day_wear]
          w_axis3 <- d_axis3[day_wear]
          w_steps <- d_steps[day_wear]
          w_lux <- d_lux[day_wear]
          w_vm <- d_vm[day_wear]

          axis1_counts <- sum(w_axis1, na.rm = TRUE)
          axis2_counts <- sum(w_axis2, na.rm = TRUE)
          axis3_counts <- sum(w_axis3, na.rm = TRUE)

          axis1_avg <- mean(w_axis1, na.rm = TRUE)
          axis2_avg <- mean(w_axis2, na.rm = TRUE)
          axis3_avg <- mean(w_axis3, na.rm = TRUE)

          axis1_max <- max(w_axis1, na.rm = TRUE)
          axis2_max <- max(w_axis2, na.rm = TRUE)
          axis3_max <- max(w_axis3, na.rm = TRUE)

          axis1_cpm <- axis1_avg * (60 / epoch_sec)
          axis2_cpm <- axis2_avg * (60 / epoch_sec)
          axis3_cpm <- axis3_avg * (60 / epoch_sec)

          vm_counts <- sum(w_vm, na.rm = TRUE)
          vm_avg <- mean(w_vm, na.rm = TRUE)
          vm_max <- max(w_vm, na.rm = TRUE)
          vm_cpm <- vm_avg * (60 / epoch_sec)

          steps_counts <- sum(w_steps, na.rm = TRUE)
          steps_avg <- mean(w_steps, na.rm = TRUE)
          steps_max <- max(w_steps, na.rm = TRUE)
          steps_per_min <- steps_avg * (60 / epoch_sec)

          lux_avg <- mean(w_lux, na.rm = TRUE)
          lux_max <- max(w_lux, na.rm = TRUE)
        } else {
          axis1_counts <- axis2_counts <- axis3_counts <- 0
          axis1_avg <- axis2_avg <- axis3_avg <- 0
          axis1_max <- axis2_max <- axis3_max <- 0
          axis1_cpm <- axis2_cpm <- axis3_cpm <- 0
          vm_counts <- vm_avg <- vm_max <- vm_cpm <- 0
          steps_counts <- steps_avg <- steps_max <- steps_per_min <- 0
          lux_avg <- lux_max <- 0
        }

        # Energy over the worn epochs, as in the summary; NA when neither METs
        # nor energy were computed
        kcals <- NA_real_
        mets_avg <- NA_real_
        if (!is.null(r$mets) && length(r$mets) >= max(day_indices)) {
          day_mets <- r$mets[day_indices][day_wear]
          mets_avg <- mean(day_mets, na.rm = TRUE)
          weight_kg <- weight * 0.453592
          kcals <- mets_avg * weight_kg * n_wear_epochs * epoch_sec / 3600
        } else if (!is.null(r$kcal_epochs) && length(r$kcal_epochs) >= max(day_indices)) {
          kcals <- sum(r$kcal_epochs[day_indices][day_wear], na.rm = TRUE)
        }
        avg_hourly_kcals <- if (n_hours > 0) kcals / n_hours else 0

        dow <- fmt_date(as.Date(date_i), "%A")
        dow_num <- as.numeric(format(as.Date(date_i), "%u"))

        time_min <- n_wear_epochs * epoch_sec / 60

        row_data <- data.frame(
          Subject = r$subject_id,
          Filename = r$name,
          Epoch = epoch_sec,
          `Weight (lbs)` = weight,
          Age = age,
          Gender = gender,
          Date = format(as.Date(date_i), "%m/%d/%Y"),
          `Day of Week` = dow,
          `Day of Week Num` = dow_num,
          kcals = round(kcals, 3),
          `Average Hourly kcals` = round(avg_hourly_kcals, 3),
          METs = round(mets_avg, 3),
          # MVPA Bout columns
          `Number of MVPA Bouts occurring in this day` = n_bouts_occurring,
          `Number of MVPA Bouts starting in this day` = n_bouts_starting,
          `Number of MVPA Bouts ending in this day` = n_bouts_ending,
          `Total time of MVPA Bouts occurring in this day` = act_min(total_bout_time, epoch_sec),
          `Total activity counts of MVPA Bouts occurring in this day` = total_bout_counts,
          # Sedentary Bout columns
          `Number of Sedentary Bouts occurring in this day` = n_sed_bouts_occurring,
          `Number of Sedentary Bouts starting in this day` = n_sed_bouts_starting,
          `Number of Sedentary Bouts ending in this day` = n_sed_bouts_ending,
          `Total time of Sedentary Bouts occurring in this day` = act_min(total_sed_bout_time, epoch_sec),
          # Sedentary Break columns
          `Number of Sedentary Breaks occurring in this day` = n_break_bouts_occurring,
          `Number of Sedentary Breaks starting in this day` = n_break_bouts_starting,
          `Number of Sedentary Breaks ending in this day` = n_break_bouts_ending,
          `Total time of Sedentary Breaks occurring in this day` = act_min(total_break_time, epoch_sec),
          # Intensity minutes
          Sedentary = act_min(sedentary, epoch_sec),
          Light = act_min(light, epoch_sec),
          Moderate = act_min(moderate, epoch_sec),
          Vigorous = act_min(vigorous, epoch_sec),
          `Very Vigorous` = act_min(very_vigorous, epoch_sec),
          # Percentages
          `% in Sedentary` = sprintf("%.2f%%", pct_sed),
          `% in Light` = sprintf("%.2f%%", pct_light),
          `% in Moderate` = sprintf("%.2f%%", pct_mod),
          `% in Vigorous` = sprintf("%.2f%%", pct_vig),
          `% in Very Vigorous` = sprintf("%.2f%%", pct_vvig),
          `Total MVPA` = act_min(total_mvpa, epoch_sec),
          `% in MVPA` = sprintf("%.2f%%", pct_mvpa),
          `Average MVPA Per Hour` = round(avg_mvpa_per_hour, 1),
          # Axis counts
          `Axis 1 Counts` = axis1_counts,
          `Axis 2 Counts` = axis2_counts,
          `Axis 3 Counts` = axis3_counts,
          `Axis 1 Average Counts` = round(axis1_avg, 1),
          `Axis 2 Average Counts` = round(axis2_avg, 1),
          `Axis 3 Average Counts` = round(axis3_avg, 1),
          `Axis 1 Max Counts` = axis1_max,
          `Axis 2 Max Counts` = axis2_max,
          `Axis 3 Max Counts` = axis3_max,
          `Axis 1 CPM` = round(axis1_cpm, 1),
          `Axis 2 CPM` = round(axis2_cpm, 1),
          `Axis 3 CPM` = round(axis3_cpm, 1),
          # Vector Magnitude
          `Vector Magnitude Counts` = round(vm_counts, 1),
          `Vector Magnitude Average Counts` = round(vm_avg, 1),
          `Vector Magnitude Max Counts` = round(vm_max, 1),
          `Vector Magnitude CPM` = round(vm_cpm, 1),
          # Steps
          `Steps Counts` = steps_counts,
          `Steps Average Counts` = round(steps_avg, 1),
          `Steps Max Counts` = steps_max,
          `Steps Per Minute` = round(steps_per_min, 1),
          # Lux
          `Lux Average Counts` = round(lux_avg, 1),
          `Lux Max Counts` = lux_max,
          # Metadata (using wear time epochs)
          `Number of Epochs` = n_wear_epochs,
          Time = act_min(n_wear_epochs, epoch_sec),
          `Calendar Days` = if (n_wear_epochs > 0) 1 else 0,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
  row_data
}

# SCHEDULE VIEWS

# Windows inside the day and kinds of day, from activity_schedule.R. Window
# rows go through act_span_row() like day rows, so the two cannot disagree.

# The schedule's columns go right after the day-of-week pair
.ACT_AFTER <- "Day of Week Num"

act_insert_after <- function(df, after, new) {
  k <- match(after, names(df))
  if (is.na(k)) return(cbind(df, new))
  cbind(df[, seq_len(k), drop = FALSE], new, df[, -seq_len(k), drop = FALSE])
}

# The Daily rows with the kind of day each one was, from the schedule.
act_add_day_type <- function(ddf, schedule) {
  if (is.null(ddf) || nrow(ddf) == 0 || "Day Type" %in% names(ddf)) return(ddf)
  d <- as.Date(ddf$Date, format = "%m/%d/%Y")
  lab <- sched_type_labels(schedule)[sched_day_type(schedule, d)]
  act_insert_after(ddf, .ACT_AFTER,
    data.frame(`Day Type` = unname(lab), check.names = FALSE, stringsAsFactors = FALSE))
}

# One row per recording, day and window, over the epochs that start inside
# the window. A window is scored only if at least min_wear of it was worn
# (GGIR's rule for a day segment); an invalid day has no worn epochs.
act_window_daily_df <- function(res, shared, schedule, bout_min = 10) {
  if (!sched_active(schedule) || length(schedule$windows) == 0) return(NULL)
  labels <- sched_type_labels(schedule)
  min_wear <- as.numeric(schedule$min_wear %||% 0.5)
  all_rows <- list()
  for (r in res) {
    ctx <- act_ctx(r, shared, bout_min)
    if (is.null(ctx)) next
    lt <- as.POSIXlt(ctx$data$timestamp)
    clock <- lt$hour * 60 + lt$min + lt$sec / 60     # minutes from midnight, local clock
    dates <- unique(ctx$data$date)
    keys <- sched_day_type(schedule, dates)
    for (i in seq_along(dates)) {
      date_i <- dates[i]
      on_day <- ctx$data$date == date_i
      # the diary's windows on a date it covers, the rules' otherwise
      for (w in sched_day_windows(schedule, r$file_id, date_i)) {
        idx <- which(on_day & clock >= w$start & clock < w$end)
        if (length(idx) == 0) next
        if (mean(ctx$wear_mask[idx]) < min_wear) next
        row_data <- act_span_row(ctx, r, idx, date_i)
        if (is.null(row_data)) next
        all_rows[[length(all_rows) + 1]] <- act_insert_after(row_data, .ACT_AFTER,
          data.frame(`Day Type` = unname(labels[[keys[i]]]), Window = w$label,
                     `Window Start` = sched_fmt_time(w$start), `Window End` = sched_fmt_time(w$end),
                     check.names = FALSE, stringsAsFactors = FALSE))
      }
    }
  }
  if (length(all_rows) == 0) return(NULL)
  do.call(rbind, all_rows)
}

# Means over rows: identity columns are carried, numbers averaged, percentages
# averaged as numbers and written back, per-day columns dropped.
.ACT_IDENTITY <- c("Subject", "Window", "Day Type", "Window Start", "Window End",
                   "Filename", "Epoch", "Weight (lbs)", "Age", "Gender")
.ACT_PER_DAY <- c("Date", "Day of Week", "Day of Week Num", "Calendar Days")

act_mean_df <- function(df, by, drop = character(0)) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  keep <- setdiff(names(df), c(.ACT_PER_DAY, drop))
  key <- do.call(paste, c(lapply(by, function(b) as.character(df[[b]])), sep = "\r"))
  out <- list()
  for (k in unique(key)) {
    g <- df[key == k, keep, drop = FALSE]
    row <- list()
    for (col in keep) {
      v <- g[[col]]
      if (col %in% .ACT_IDENTITY) { row[[col]] <- v[1]; next }
      ch <- as.character(v)
      if (all(grepl("%$", ch))) {
        m <- mean(suppressWarnings(as.numeric(sub("%$", "", ch))), na.rm = TRUE)
        row[[col]] <- if (is.finite(m)) sprintf("%.2f%%", m) else ""
      } else {
        n <- suppressWarnings(as.numeric(ch))
        if (all(is.na(n) == is.na(ch))) {
          m <- mean(n, na.rm = TRUE)
          row[[col]] <- if (is.finite(m)) round(m, 2) else NA_real_
        } else row[[col]] <- v[1]
      }
    }
    row <- c(row[intersect(.ACT_IDENTITY, keep)], list(Days = nrow(g)),
             row[setdiff(keep, .ACT_IDENTITY)])
    out[[length(out) + 1]] <- data.frame(row, check.names = FALSE, stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

# The By window view: per-day rows in day then clock order, day and window
# columns first.
act_window_view <- function(wdf) {
  if (is.null(wdf) || nrow(wdf) == 0) return(NULL)
  first <- intersect(c("Subject", "Date", "Day of Week", "Day Type", "Window", "Window Start", "Window End"), names(wdf))
  out <- wdf[, c(first, setdiff(names(wdf), first)), drop = FALSE]
  d <- as.Date(out$Date, format = "%m/%d/%Y")
  st <- suppressWarnings(as.numeric(sub(":.*$", "", out[["Window Start"]])) * 60 + as.numeric(sub("^.*:", "", out[["Window Start"]])))
  out <- out[order(out$Subject, d, st), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# One row per recording and kind of day, the mean over its days.
act_daytype_df <- function(ddf) {
  if (is.null(ddf) || !"Day Type" %in% names(ddf)) return(NULL)
  act_mean_df(ddf, by = c("Subject", "Filename", "Day Type"))
}

# A plain table for the Daily, window and day type views, in the Summary
# table's furniture. The first column keeps the Summary's Subject width: the
# second pinned column's offset is written for it in the stylesheet.
ac_plain_grid <- function(df, pin = 2L) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  cols <- names(df)
  num <- vapply(cols, function(h) ac_is_num(df[[h]][1]), logical(1))
  widths <- vapply(cols, ac_col_width, integer(1))
  widths[1] <- 74L
  pinned <- function(j) if (j <= pin) paste0("ac-pin ac-p", j - 1L) else ""
  # ac-ch--solo: the only heading row, so it sticks at the top of the scroll
  # box, not 24px down under a group row as the Summary's does
  head <- tags$tr(lapply(seq_along(cols), function(j)
    tags$th(class = paste("ac-ch ac-ch--solo", if (num[j]) "ac-r" else "", pinned(j)), cols[j])))
  body <- lapply(seq_len(nrow(df)), function(i)
    tags$tr(class = "ac-row", lapply(seq_along(cols), function(j) {
      v <- df[[j]][i]
      txt <- trimws(as.character(v))
      zero <- identical(txt, "0") || identical(txt, "0.00%")
      tags$td(class = paste(if (num[j]) "ac-r" else "", if (zero) "is-zero" else "", pinned(j)),
              HTML(ac_num(v)))
    })))
  tags$div(class = "ac-scroll",
    tags$table(class = "ac-gt", style = paste0("width: ", sum(widths), "px;"),
      tags$colgroup(lapply(widths, function(w) tags$col(style = paste0("width: ", w, "px;")))),
      tags$thead(head),
      tags$tbody(body)))
}

# DRAWING

# The Summary export's column groups, in its order. A column not listed here
# still reaches the page, under "Other".
ac_groups <- function() {
  list(
    list("File", c("Filename", "Epoch", "Weight (lbs)", "Age", "Gender")),
    list("Energy", c("kcals", "Average kcals per day", "Average kcals per hour", "METs")),
    list("MVPA bouts", c("MVPA Bouts", "Total Time in MVPA Bouts", "Avg Time per MVPA Bout",
                         "Max Time per MVPA Bout", "Min Time per MVPA Bout", "Total Counts in MVPA Bouts")),
    list("Sedentary bouts", c("Total Sedentary Bouts", "Total Time in Sedentary Bouts",
                              "Average Length of Sedentary Bouts", "Maximum Length of Sedentary Bouts",
                              "Minimum Length of Sedentary Bouts", "Daily Average of Sedentary Bouts")),
    list("Sedentary breaks", c("Total Sedentary Breaks", "Total Time in Sedentary Breaks",
                               "Average length of Sedentary Breaks", "Max Length of Sedentary Breaks",
                               "Minimum Length of Sedentary Breaks", "Daily Average of Sedentary Breaks")),
    list("Minutes at each intensity", c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous")),
    list("Share of worn time", c("% in Sedentary", "% in Light", "% in Moderate",
                                 "% in Vigorous", "% in Very Vigorous")),
    list("MVPA", c("Total MVPA", "% in MVPA", "Average MVPA Per day")),
    list("Axis counts", c("Axis 1 Counts", "Axis 2 Counts", "Axis 3 Counts",
                          "Axis 1 Average Counts", "Axis 2 Average Counts", "Axis 3 Average Counts",
                          "Axis 1 Max Counts", "Axis 2 Max Counts", "Axis 3 Max Counts",
                          "Axis 1 CPM", "Axis 2 CPM", "Axis 3 CPM")),
    list("Vector magnitude", c("Vector Magnitude Counts", "Vector Magnitude Average Counts",
                               "Vector Magnitude Max Counts", "Vector Magnitude CPM")),
    list("Steps", c("Steps Counts", "Steps Average Counts", "Steps Max Counts", "Steps Per Minute")),
    list("Lux", c("Lux Average Counts", "Lux Max Counts")),
    list("Coverage", c("Number of Epochs", "Time", "Calendar Days"))
  )
}

# Column widths by header length
ac_col_width <- function(h) {
  n <- nchar(h)
  if (identical(h, "Filename")) return(190L)
  if (n > 26) return(190L)
  if (n > 18) return(160L)
  if (grepl("Counts|Length|Time per|Average|Number of|Daily|Total", h)) return(132L)
  if (n > 10) return(116L)
  92L
}

ac_is_num <- function(v) grepl("^-?[0-9,]+(\\.[0-9]+)?%?$", trimws(as.character(v)))

# Thousand separators above five figures, otherwise as the export writes it
ac_num <- function(v) {
  s <- trimws(as.character(v))
  if (length(s) == 0 || is.na(s) || !nzchar(s)) return("–")
  n <- suppressWarnings(as.numeric(s))
  if (is.na(n) || abs(n) < 10000) return(s)
  dec <- if (grepl("\\.", s)) nchar(sub("^.*\\.", "", s)) else 0L
  formatC(n, format = "f", digits = dec, big.mark = ",")
}

# One cell per day at the day's Total MVPA. A day the wear rule dropped is
# hatched, not coloured zero.
ac_day_cell <- function(mvpa, epochs, date) {
  if (is.na(epochs) || epochs <= 0) {
    return(tags$span(class = "ac-cell is-out", title = paste(date, "· no valid wear")))
  }
  m <- if (is.na(mvpa)) 0 else mvpa
  step <- if (m <= 0) 0 else if (m < 150) 1 else if (m < 300) 2 else if (m < 450) 3 else 4
  tags$span(class = paste0("ac-cell s", step),
            title = paste0(date, " · ", fmt_int(m), " min MVPA"))
}

ac_fig <- function(value, unit, label) {
  tags$div(class = "ac-fig",
    tags$div(tags$span(class = "ac-fig-n", value),
             if (!is.null(unit)) tags$span(class = "ac-fig-u", unit)),
    tags$div(class = "ac-fig-l", label))
}

ac_rule_div <- function() tags$div(class = "ac-vrule", `aria-hidden` = "true")

# The Summary's groups as list(name, columns): listed columns in their group,
# the rest under Other
ac_summary_groups <- function(sdf) {
  groups <- ac_groups()
  listed <- unlist(lapply(groups, function(g) g[[2]]), use.names = FALSE)
  present_cols <- setdiff(names(sdf), "Subject")
  groups <- lapply(groups, function(g) list(g[[1]], intersect(g[[2]], present_cols)))
  groups <- Filter(function(g) length(g[[2]]) > 0, groups)
  leftover <- setdiff(present_cols, listed)
  if (length(leftover)) groups <- c(groups, list(list("Other", leftover)))
  groups
}

# Go to, in a table head: one item per group cell, counts and raw alike; the
# page script scrolls the grid to the group's first column
ac_goto <- function(labels, counts) {
  if (length(labels) < 2) return(NULL)
  tags$span(class = "ac-goto-wrap",
    tags$span(class = "ac-pick ac-goto", tabindex = "0", role = "button",
              `aria-haspopup` = "menu", `aria-expanded` = "false",
              "Go to", tags$span(class = "ac-car", `aria-hidden` = "true", HTML("&#9660;"))),
    tags$div(class = "ac-gomenu", role = "menu", style = "display: none;",
      lapply(seq_along(labels), function(i)
        tags$div(class = "ac-gi", role = "menuitem", tabindex = "-1", `data-go` = i - 1L,
                 tags$span(class = "ac-tick", `aria-hidden` = "true"),
                 labels[[i]],
                 tags$span(class = "ac-sc", plural(counts[[i]], "column"))))))
}

# The whole Summary: every export column in its order plus a strip of day
# cells. Subject and the strip are pinned.
ac_summary_grid <- function(sdf, ddf, fids, sel_fid = NULL) {
  if (is.null(sdf) || nrow(sdf) == 0) return(NULL)

  key <- function(subject, filename) paste0(subject, "\r", filename)
  days_for <- list()
  longest <- 0L
  if (!is.null(ddf) && nrow(ddf) > 0) {
    k <- key(ddf$Subject, ddf$Filename)
    for (kk in unique(k)) {
      d <- ddf[k == kk, , drop = FALSE]
      days_for[[kk]] <- d
      longest <- max(longest, nrow(d))
    }
  }

  groups <- ac_summary_groups(sdf)
  flat <- unlist(lapply(groups, function(g) g[[2]]), use.names = FALSE)

  sub_w <- 74L
  day_w <- if (longest > 0) as.integer(longest * 18L - 2L + 17L) else 0L
  widths <- c(sub_w, if (day_w > 0) day_w, vapply(flat, ac_col_width, integer(1)))
  total <- sum(widths)

  colgroup <- tags$colgroup(lapply(widths, function(w) tags$col(style = paste0("width: ", w, "px;"))))

  ticks <- if (longest > 0) {
    marks <- unique(c(1L, seq(4L, longest, by = 4L), longest))
    lapply(marks, function(d)
      tags$span(class = "ac-tk", style = paste0("left: ", (d - 1L) * 18L, "px;"), d))
  } else NULL

  head_group <- tags$tr(
    tags$th(class = "ac-gh ac-pin ac-p0", rowspan = 2, "Subject"),
    if (day_w > 0) tags$th(class = "ac-gh ac-pin ac-p1", rowspan = 2,
      tags$span(class = "ac-dayhead", tags$span(class = "ac-lbl", "Each day"), ticks)),
    lapply(groups, function(g)
      tags$th(class = "ac-gg", colspan = length(g[[2]]), g[[1]])))

  head_col <- tags$tr(lapply(flat, function(h)
    tags$th(class = paste("ac-ch", if (ac_is_num(sdf[[h]][1])) "ac-r" else ""), h)))

  body <- lapply(seq_len(nrow(sdf)), function(i) {
    fid <- if (length(fids) >= i) fids[i] else NA_character_
    kk <- key(sdf$Subject[i], sdf$Filename[i])
    d <- days_for[[kk]]
    strip <- if (day_w > 0) {
      cells <- if (is.null(d)) NULL else lapply(seq_len(nrow(d)), function(j)
        ac_day_cell(suppressWarnings(as.numeric(d[["Total MVPA"]][j])),
                    suppressWarnings(as.numeric(d[["Number of Epochs"]][j])),
                    as.character(d$Date[j])))
      tags$td(class = "ac-pin ac-p1", tags$span(class = "ac-cellrow", cells))
    } else NULL

    tags$tr(
      class = paste("ac-row", if (!is.na(fid) && identical(fid, sel_fid)) "is-selected" else ""),
      `data-fid` = if (is.na(fid)) NULL else fid,
      tabindex = "0",
      tags$td(class = "ac-sub ac-pin ac-p0", sdf$Subject[i]),
      strip,
      lapply(flat, function(h) {
        v <- sdf[[h]][i]
        txt <- trimws(as.character(v))
        zero <- identical(txt, "0") || identical(txt, "0.00%")
        tags$td(class = paste(if (ac_is_num(v)) "ac-r" else "", if (zero) "is-zero" else ""),
                HTML(ac_num(v)))
      }))
  })

  tags$div(class = "ac-scroll",
    tags$table(class = "ac-gt", style = paste0("width: ", total, "px;"),
      colgroup,
      tags$thead(head_group, head_col),
      tags$tbody(body)))
}

# The table panel's empty states for the two schedule views.
ac_sched_empty <- function(rows = FALSE) {
  tags$div(class = "ac-sched-empty",
    tags$div(class = "ac-sched-empty-t", if (rows) "No rows" else "No schedule"),
    tags$div(class = "ac-sched-empty-m",
      if (rows) "No window met the wear rule on any valid day."
      else "Open Schedule in the rule bar, define windows or kinds of day, then run the analysis."))
}

# The schedule panel: windows inside the day, kinds of day, date ranges, in
# the settings panel's furniture; the module reads the boxes back. prefix is
# the branch's outer furniture (ac on counts, wt on raw); the fields inside
# are the counts page's on both branches, since both live on .ac-page.
ac_sched_panel <- function(ns, s, prefix = "ac", idp = "", foot = "Applied by Run analysis.", diary = NULL) {
  p <- function(x) paste0(prefix, "-", x)
  # every input id carries the branch prefix ("c_" or "r_") so the two panels
  # never share a box; a remove button says the branch too ("c:2")
  ns0 <- ns
  ns <- function(x) ns0(paste0(idp, x))
  branch <- sub("_$", "", idp)
  labels <- sched_type_labels(s)
  choices <- sched_pill_choices(s)
  keys <- unname(choices)
  plus <- ac_plus_icon()
  cross <- HTML('<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" aria-hidden="true"><path d="M6 6l12 12M18 6L6 18"/></svg>')
  key <- function(t) tags$span(class = "ac-sched-k", t)
  # a button with .action-button so Shiny binds it, not an <a>, which the
  # page's link rules would recolour
  add <- function(id, label) tags$button(id = ns(id), type = "button", class = "action-button ac-sched-add", plus, label)
  head <- function(k, action = NULL) tags$div(class = "ac-sched-head", key(k), action)
  remove <- function(attr, i, title) {
    b <- tags$button(type = "button", class = "ac-sched-rm", title = title, `aria-label` = title, cross)
    b$attribs[[attr]] <- paste0(branch, ":", i)
    b
  }
  grid_head <- function(cls, cols) tags$div(class = paste("ac-sched-row ac-sched-th", cls),
                                            lapply(cols, key), tags$span())
  field <- function(k, control, note = NULL) {
    tags$div(class = "ac-field ac-sched-field",
      tags$div(class = "ac-field-k", k), control,
      if (!is.null(note)) tags$div(class = "ac-field-n", note))
  }
  win_row <- function(i, w) {
    tags$div(class = "ac-sched-row ac-sched-win",
      textInput(ns(paste0("sw_label_", i)), NULL, value = w$label %||% "", placeholder = "label", width = "100%"),
      textInput(ns(paste0("sw_start_", i)), NULL, value = sched_fmt_time(w$start), placeholder = "HH:MM", width = "100%"),
      textInput(ns(paste0("sw_end_", i)), NULL, value = sched_fmt_time(w$end), placeholder = "HH:MM", width = "100%"),
      tags$div(class = "ac-sched-applies",
        checkboxGroupInput(ns(paste0("sw_applies_", i)), NULL, choices = choices,
                           selected = intersect(as.character(w$applies), keys), inline = TRUE)),
      remove("data-swrm", i, "Remove this window"))
  }
  range_row <- function(i, r) {
    tags$div(class = "ac-sched-row ac-sched-rng",
      textInput(ns(paste0("sr_label_", i)), NULL, value = r$label %||% "", placeholder = "label", width = "100%"),
      textInput(ns(paste0("sr_from_", i)), NULL, value = sched_fmt_date(r$from), placeholder = "YYYY-MM-DD", width = "100%"),
      textInput(ns(paste0("sr_to_", i)), NULL, value = sched_fmt_date(r$to), placeholder = "YYYY-MM-DD", width = "100%"),
      remove("data-srrm", i, "Remove this range"))
  }

  tags$div(
    class = paste(p("panel"), p("settings-grid"), "ac-sched"),
    tags$div(class = "ac-sched-sec",
      head("Windows inside the day", add("sw_add", "Add window")),
      if (length(s$windows) == 0) tags$div(class = "ac-sched-none", "None. Every day is reported whole.")
      else tagList(grid_head("ac-sched-win", c("Label", "Start", "End", "Applies to")),
                   lapply(seq_along(s$windows), function(i) win_row(i, s$windows[[i]]))),
      uiOutput(ns("sched_msg_w"))),
    tags$div(class = "ac-sched-sec",
      head("Kinds of day"),
      tags$div(class = "ac-sched-fields",
        field("Monday to Friday",
              textInput(ns("st_weekday"), NULL, value = labels[["weekday"]], width = "100%"),
              "unless a date range says otherwise"),
        field("Saturday and Sunday",
              textInput(ns("st_weekend"), NULL, value = labels[["weekend"]], width = "100%")),
        field("A window counts at",
              tags$div(class = "ac-num",
                numericInput(ns("s_minwear"), NULL, value = round(100 * as.numeric(s$min_wear %||% 0.5)),
                             min = 0, max = 100, step = 5, width = "100%"),
                tags$span(class = "ac-unit", `aria-hidden` = "true", "% worn")),
              "GGIR's rule for a day segment")),
      uiOutput(ns("sched_msg_t"))),
    tags$div(class = "ac-sched-sec",
      head("Date ranges", add("sr_add", "Add date range")),
      if (length(s$ranges) == 0) tags$div(class = "ac-sched-none", "None. Weekdays and weekend days only.")
      else tagList(grid_head("ac-sched-rng", c("Label", "From", "To")),
                   lapply(seq_along(s$ranges), function(i) range_row(i, s$ranges[[i]]))),
      uiOutput(ns("sched_msg_r"))),
    # the diary section is built by the module
    if (!is.null(diary)) tags$div(class = "ac-sched-sec", head("Diary", diary$action), diary$body),
    tags$div(class = p("settings-foot"),
      tags$span(class = "ac-sched-foot-n", foot),
      tags$span(class = p("spacer")),
      actionButton(ns("sched_clear"), "Clear schedule", class = paste(p("btn"), p("btn--text"), "is-destructive"))))
}

ac_plus_icon <- function() {
  HTML('<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.4" stroke-linecap="round" aria-hidden="true"><path d="M12 5v14M5 12h14"/></svg>')
}

# One line per fault, in the warn colour
ac_sched_msgs <- function(msgs) {
  if (length(msgs) == 0) return(NULL)
  mark <- HTML('<svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M12 3l10 18H2z"/><path d="M12 10v4M12 17.5v.5"/></svg>')
  tags$div(class = "ac-sched-msg", lapply(msgs, function(m) tags$div(mark, m)))
}

# The export menu: one download of the whole set, then the individual files.
# With a schedule in force Windows and DayTypes are listed too; the script
# sends whatever links the menu holds.
ac_export_menu <- function(ns, ready = TRUE, more = FALSE) {
  item <- function(id, label) {
    tags$div(class = "ac-ei",
             downloadButton(ns(id), label, class = "ac-ei-link", role = "menuitem"))
  }
  if (!ready) {
    return(tags$div(class = "ac-exportmenu", id = ns("export_menu"), role = "menu", style = "display: none;",
      tags$div(class = "ac-ei ac-ei-note",
        tags$span(class = "ac-ei-link", "Run the analysis first"))))
  }
  tags$div(class = "ac-exportmenu", id = ns("export_menu"), role = "menu", style = "display: none;",
    tags$div(class = "ac-ei ac-ei-all", role = "menuitem", tabindex = "-1",
      tags$span(class = "ac-ei-link ac-ei-strong",
                icon("download", class = "ac-ei-icon"),
                tags$span(class = "ac-ei-label", if (more) "Download all six" else "Download all four"))),
    tags$div(class = "ac-ei-rule", `aria-hidden` = "true"),
    item("export_summary", "canhrActi_Summary"),
    item("export_daily", "canhrActi_Daily"),
    item("export_hourly", "canhrActi_Hourly"),
    item("export_sedentary", "canhrActi_SedentaryAnalysis"),
    if (more) item("export_windows", "canhrActi_Windows"),
    if (more) item("export_daytypes", "canhrActi_DayTypes"))
}
