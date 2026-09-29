# Sleep: the loaded page. The Details and Summary frames are built here for
# both the table and the CSV export, with the three helpers they use.

epoch_minutes_factor <- function(epoch_length) {
  if (is.null(epoch_length) || length(epoch_length) == 0 ||
      is.na(epoch_length[1]) || !is.numeric(epoch_length[1]) || epoch_length[1] <= 0) {
    return(1)
  }
  epoch_length[1] / 60
}

format_average_time <- function(times) {
  if (length(times) == 0) return("--")
  if (!inherits(times, "POSIXt")) times <- canhrActi:::.clock_time(times)

  hours <- as.numeric(format(times, "%H"))
  minutes <- as.numeric(format(times, "%M"))
  minutes_since_midnight <- hours * 60 + minutes

  angles_rad <- (minutes_since_midnight / 1440) * 2 * pi
  sin_mean <- mean(sin(angles_rad), na.rm = TRUE)
  cos_mean <- mean(cos(angles_rad), na.rm = TRUE)
  mean_angle <- atan2(sin_mean, cos_mean)
  if (mean_angle < 0) mean_angle <- mean_angle + 2 * pi

  avg_minutes <- (mean_angle / (2 * pi)) * 1440
  avg_hour <- floor(avg_minutes / 60) %% 24
  avg_min <- round(avg_minutes %% 60)

  if (avg_hour == 0) {
    sprintf("12:%02d AM", avg_min)
  } else if (avg_hour < 12) {
    sprintf("%d:%02d AM", avg_hour, avg_min)
  } else if (avg_hour == 12) {
    sprintf("12:%02d PM", avg_min)
  } else {
    sprintf("%d:%02d PM", avg_hour - 12, avg_min)
  }
}

format_actilife_datetime <- function(dt) {
  if (is.null(dt) || length(dt) == 0 || is.na(dt)) return("")
  formatted <- fmt_date(canhrActi:::.clock_time(dt), "%m/%d/%Y %I:%M:%S %p")
  formatted <- gsub("^0", "", formatted)
  formatted <- gsub("/0", "/", formatted)
  sub(" 0", " ", formatted, fixed = TRUE)
}

# Every column of the Details export, one row per sleep period.
# NULL when no recording yielded a period.
sleep_details_df <- function(res, shared) {
  all_rows <- list()

  for (r in res) {
    if (is.null(r$periods) || nrow(r$periods) == 0) next
    # Skip files that fail wear-time validity criteria
    wt <- shared$results$wear_time[[r$file_id]]
    if (!is.null(wt) && isFALSE(wt$meets_criteria)) next

    f <- shared$files[[r$file_id]]
    weight <- f$subject_info$weight_lbs %||% 0
    age <- f$subject_info$age %||% 0
    gender <- f$subject_info$sex %||% "Undefined"
    if (gender == "M") gender <- "Male"
    else if (gender == "F") gender <- "Female"
    else if (gender == "") gender <- "Undefined"

    algorithm_display <- if (r$algorithm == "cole.kripke") "Cole-Kripke"
                         else if (r$algorithm == "sadeh") "Sadeh"
                         else r$algorithm

    # sleep_time / wake_time are epoch counts; convert to minutes
    emf <- epoch_minutes_factor(r$epoch_length)

    for (i in 1:nrow(r$periods)) {
      period <- r$periods[i, ]
      in_bed_posix <- canhrActi:::.clock_time(period$in_bed_time)
      onset_posix <- canhrActi:::.clock_time(period$onset)
      latency <- as.numeric(difftime(onset_posix, in_bed_posix, units = "mins"))
      sleep_frag_index <- period$movement_index + period$fragmentation_index

      row_data <- data.frame(
        `Subject Name` = r$subject_id,
        `File Name` = r$name,
        `Serial Number` = r$serial_number %||% "",
        `Epoch Length` = r$epoch_length,
        Weight = weight,
        Age = age,
        Gender = gender,
        `Sleep/Wake Algorithm` = algorithm_display,
        `Sleep Period Detection Algorithm` = r$detection_method %||% "Tudor-Locke",
        `In Bed Time` = format_actilife_datetime(period$in_bed_time),
        `Out Bed Time` = format_actilife_datetime(period$out_bed_time),
        Efficiency = round(period$sleep_efficiency, 3),
        Onset = format_actilife_datetime(period$onset),
        Latency = round(latency, 0),
        `Total Sleep Time` = round(period$sleep_time * emf, 0),
        WASO = round(period$wake_time * emf, 0),
        `Number of Awakenings` = period$number_of_awakenings,
        `Length of Awakenings in Minutes` = round(period$average_awakening, 2),
        `Activity Counts` = round(period$total_counts, 0),
        `Movement Index` = round(period$movement_index, 3),
        `Fragmentation Index` = round(period$fragmentation_index, 3),
        `Sleep Fragmentation Index` = round(sleep_frag_index, 3),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      all_rows[[length(all_rows) + 1]] <- row_data
    }
  }

  df <- do.call(rbind, all_rows)
  if (length(all_rows) == 0) return(NULL)
  do.call(rbind, all_rows)
}

# Every column of the Summary export, one row per recording.
sleep_summary_df <- function(res, shared) {
  all_rows <- list()

  for (r in res) {
    if (is.null(r$periods) || nrow(r$periods) == 0) next
    # Skip files that fail wear-time validity criteria
    wt <- shared$results$wear_time[[r$file_id]]
    if (!is.null(wt) && isFALSE(wt$meets_criteria)) next

    f <- shared$files[[r$file_id]]
    weight <- f$subject_info$weight_lbs %||% 0
    age <- f$subject_info$age %||% 0
    gender <- f$subject_info$sex %||% "Undefined"
    if (gender == "M") gender <- "Male"
    else if (gender == "F") gender <- "Female"
    else if (gender == "") gender <- "Undefined"

    algorithm_display <- if (r$algorithm == "cole.kripke") "Cole-Kripke"
                         else if (r$algorithm == "sadeh") "Sadeh"
                         else r$algorithm
    periods <- r$periods
    # sleep_time / wake_time are epoch counts; convert to minutes
    emf <- epoch_minutes_factor(r$epoch_length)

    onset_times <- canhrActi:::.clock_time(periods$onset)
    in_bed_times <- canhrActi:::.clock_time(periods$in_bed_time)
    out_bed_times <- canhrActi:::.clock_time(periods$out_bed_time)
    latencies <- as.numeric(difftime(onset_times, in_bed_times, units = "mins"))

    row_data <- data.frame(
      `Subject Name` = r$subject_id,
      `File Name` = r$name,
      `Serial Number` = r$serial_number %||% "",
      `Epoch Length` = r$epoch_length,
      Weight = weight,
      Age = age,
      Gender = gender,
      `Sleep/Wake Algorithm` = algorithm_display,
      `Sleep Period Detection Algorithm` = r$detection_method %||% "Tudor-Locke",
      `Number of Sleep Periods` = r$n_periods,
      `Average In Bed Time` = format_average_time(in_bed_times),
      `Average Out Bed Time` = format_average_time(out_bed_times),
      `Average Efficiency` = round(mean(periods$sleep_efficiency, na.rm = TRUE), 3),
      `Average Onset` = format_average_time(onset_times),
      `Average Latency` = round(mean(latencies, na.rm = TRUE), 0),
      `Average Total Sleep Time` = round(mean(periods$sleep_time, na.rm = TRUE) * emf, 0),
      `Average WASO` = round(mean(periods$wake_time, na.rm = TRUE) * emf, 2),
      `Average Number of Awakenings` = round(mean(periods$number_of_awakenings, na.rm = TRUE), 2),
      `Average Length of Awakenings in Minutes` = round(mean(periods$average_awakening, na.rm = TRUE), 2),
      `Average Activity Counts` = round(mean(periods$total_counts, na.rm = TRUE), 2),
      `Average Movement Index` = round(mean(periods$movement_index, na.rm = TRUE), 3),
      `Average Fragmentation Index` = round(mean(periods$fragmentation_index, na.rm = TRUE), 3),
      `Average Sleep Fragmentation Index` = round(mean(periods$movement_index + periods$fragmentation_index, na.rm = TRUE), 3),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    all_rows[[length(all_rows) + 1]] <- row_data
  }

  df <- do.call(rbind, all_rows)
  if (length(all_rows) == 0) return(NULL)
  do.call(rbind, all_rows)
}

# DRAWING

# The Details export's column groups, in its order. A column not listed here
# still reaches the page, under "Other".
sl_groups <- function() {
  list(
    list("Recording", c("File Name", "Serial Number", "Epoch Length", "Weight", "Age", "Gender")),
    list("Scoring", c("Sleep/Wake Algorithm", "Sleep Period Detection Algorithm")),
    list("The period", c("In Bed Time", "Out Bed Time", "Onset", "Latency")),
    list("How much sleep", c("Total Sleep Time", "Efficiency", "WASO")),
    list("Broken by", c("Number of Awakenings", "Length of Awakenings in Minutes")),
    list("Movement", c("Activity Counts", "Movement Index", "Fragmentation Index",
                       "Sleep Fragmentation Index"))
  )
}

sl_col_width <- function(h) {
  n <- nchar(h)
  # a full "10/7/2025 11:28:00 PM"
  if (h %in% c("In Bed Time", "Out Bed Time", "Onset")) return(165L)
  if (identical(h, "File Name")) return(190L)
  if (n > 30) return(190L)
  if (n > 20) return(165L)
  if (grepl("Time|Index|Counts|Number|Length|Algorithm|Serial|File", h)) return(140L)
  if (n > 10) return(116L)
  92L
}

sl_is_num <- function(v) grepl("^-?[0-9,]+(\\.[0-9]+)?$", trimws(as.character(v)))

sl_num <- function(v) {
  s <- trimws(as.character(v))
  if (length(s) == 0 || is.na(s) || !nzchar(s)) return("–")
  n <- suppressWarnings(as.numeric(s))
  if (is.na(n) || abs(n) < 10000) return(s)
  dec <- if (grepl("\\.", s)) nchar(sub("^.*\\.", "", s)) else 0L
  formatC(n, format = "f", digits = dec, big.mark = ",")
}

# Minutes past midnight from the export's own "9/19/2024 02:05:00 AM".
sl_clock <- function(x) {
  m <- regmatches(x, regexec("(\\d+):(\\d+):(\\d+)\\s*([AaPp])", x))[[1]]
  if (length(m) < 5) return(NA_real_)
  h <- as.numeric(m[2]) %% 12
  if (toupper(m[5]) == "P") h <- h + 12
  h * 60 + as.numeric(m[3])
}

sl_hhmm <- function(m) {
  if (is.na(m)) return("")
  sprintf("%02d:%02d", floor(m / 60), round(m %% 60))
}

# One bar per sleep period on a noon to noon clock, so a night running 23:00
# to 07:00 stays one bar. A period that runs past the right edge is drawn in
# two pieces. Position is bedtime, length time in bed, shade the efficiency.
sl_night_bar <- function(o) {
  a <- sl_clock(o[["In Bed Time"]])
  b <- sl_clock(o[["Out Bed Time"]])
  if (is.na(a) || is.na(b)) return(tags$span(class = "sl-nb"))
  span <- b - a
  if (span <= 0) span <- span + 1440
  span <- min(span, 1440)
  eff <- suppressWarnings(as.numeric(o[["Efficiency"]]))
  step <- if (is.na(eff)) 1 else if (eff < 85) 1 else if (eff < 90) 2 else if (eff < 95) 3 else 4
  start <- ((a + 720) %% 1440) / 1440
  w <- span / 1440

  piece <- function(l, ww) tags$span(class = paste0("sl-nb-b s", step),
    style = sprintf("left: %.2f%%; width: %.2f%%;", l * 100, ww * 100))
  parts <- if (start + w <= 1) list(piece(start, w))
           else list(piece(start, 1 - start), piece(0, start + w - 1))

  tags$span(class = "sl-nb",
    title = paste0(o[["Subject Name"]], " · in bed ", sl_hhmm(a), ", out ", sl_hhmm(b),
                   " · ", o[["Total Sleep Time"]], " min asleep · ",
                   o[["Efficiency"]], "% efficient"),
    parts,
    tags$i(style = "left: 25%;"), tags$i(style = "left: 50%;"), tags$i(style = "left: 75%;"))
}

sl_fig <- function(value, unit, label) {
  tags$div(class = "sl-fig",
    tags$div(tags$span(class = "sl-fig-n", value),
             if (!is.null(unit)) tags$span(class = "sl-fig-u", unit)),
    tags$div(class = "sl-fig-l", label))
}

sl_rule_div <- function() tags$div(class = "sl-vrule", `aria-hidden` = "true")

# The whole Details export on the page: one row per sleep period, every
# column it writes, plus the bar.
sl_periods_grid <- function(ddf, sel_subject = NULL) {
  if (is.null(ddf) || nrow(ddf) == 0) return(NULL)
  if (!is.null(sel_subject)) ddf <- ddf[ddf[["Subject Name"]] == sel_subject, , drop = FALSE]
  if (nrow(ddf) == 0) return(NULL)

  groups <- sl_groups()
  listed <- unlist(lapply(groups, function(g) g[[2]]), use.names = FALSE)
  present_cols <- setdiff(names(ddf), "Subject Name")
  groups <- lapply(groups, function(g) list(g[[1]], intersect(g[[2]], present_cols)))
  groups <- Filter(function(g) length(g[[2]]) > 0, groups)
  leftover <- setdiff(present_cols, listed)
  if (length(leftover)) groups <- c(groups, list(list("Other", leftover)))
  flat <- unlist(lapply(groups, function(g) g[[2]]), use.names = FALSE)

  sub_w <- 74L
  bar_w <- 232L
  widths <- c(sub_w, bar_w, vapply(flat, sl_col_width, integer(1)))
  total <- sum(widths)

  head_group <- tags$tr(
    tags$th(class = "sl-gh sl-pin sl-p0", rowspan = 2, "Subject"),
    tags$th(class = "sl-gh sl-pin sl-p1", rowspan = 2,
      tags$span(class = "sl-barhead",
        tags$span(class = "sl-lbl", "Each night"),
        tags$span(class = "sl-tk first", style = "left: 0;", "12:00"),
        tags$span(class = "sl-tk", style = "left: 25%;", "18:00"),
        tags$span(class = "sl-tk", style = "left: 50%;", "00:00"),
        tags$span(class = "sl-tk", style = "left: 75%;", "06:00"),
        tags$span(class = "sl-tk last", style = "left: 100%;", "12:00"))),
    lapply(groups, function(g) tags$th(class = "sl-gg", colspan = length(g[[2]]), g[[1]])))

  head_col <- tags$tr(lapply(flat, function(h)
    tags$th(class = paste("sl-ch", if (sl_is_num(ddf[[h]][1])) "sl-r" else ""), h)))

  body <- lapply(seq_len(nrow(ddf)), function(i) {
    o <- as.list(ddf[i, , drop = FALSE])
    subj <- as.character(o[["Subject Name"]])
    tags$tr(
      class = paste("sl-row", if (identical(subj, sel_subject)) "is-selected" else ""),
      `data-fid` = subj,
      tabindex = "0",
      tags$td(class = "sl-sub sl-pin sl-p0", subj),
      tags$td(class = "sl-pin sl-p1", sl_night_bar(o)),
      lapply(flat, function(h) {
        v <- as.character(o[[h]])
        txt <- trimws(v)
        zero <- identical(txt, "0") || identical(txt, "0.00") || !nzchar(txt)
        tags$td(class = paste(if (sl_is_num(v)) "sl-r" else "", if (zero) "is-zero" else ""),
                HTML(sl_num(v)))
      }))
  })

  tags$div(class = "sl-scroll",
    tags$table(class = "sl-gt", style = paste0("width: ", total, "px;"),
      tags$colgroup(lapply(widths, function(w) tags$col(style = paste0("width: ", w, "px;")))),
      tags$thead(head_group, head_col),
      tags$tbody(body)))
}

# Export, as one thing; both files go out inside the click itself
sl_export_menu <- function(ns, ready = TRUE) {
  if (!ready) {
    return(tags$div(class = "sl-exportmenu", id = ns("export_menu"), style = "display: none;",
      tags$div(class = "sl-ei sl-ei-note",
        tags$span(class = "sl-ei-link", "Run the analysis first"))))
  }
  tags$div(class = "sl-exportmenu", id = ns("export_menu"), style = "display: none;",
    tags$div(class = "sl-ei sl-ei-all",
      tags$span(class = "sl-ei-link sl-ei-strong",
                icon("download"), tags$span(class = "sl-ei-label", "Download both"))),
    tags$div(class = "sl-ei-rule", `aria-hidden` = "true"),
    tags$div(class = "sl-ei", downloadButton(ns("export_summary"), "BatchSleepExportSummary", class = "sl-ei-link")),
    tags$div(class = "sl-ei", downloadButton(ns("export_details"), "BatchSleepExportDetails", class = "sl-ei-link")))
}
