# Activity schedule: windows inside the day and kinds of day, as pure
# functions. A schedule is a set of rules; applied to one recording it gives
# a per-date answer, which kind of day and which windows, and a diary fills
# the same place. Times are minutes from midnight, 0 to 1440; dates are Date.

SCHED_BUILTIN <- c(weekday = "Weekday", weekend = "Weekend")

sched_new <- function() {
  list(
    version = 1L,
    # the two built in kinds of day, by key; the values are the names shown
    types = SCHED_BUILTIN,
    # labelled date ranges, in order; the first that holds a date wins
    ranges = list(),      # list(key, label, from, to)
    # labelled clock ranges; applies is type keys, empty means every kind of day
    windows = list(),     # list(label, start, end, applies)
    # worn fraction a window needs; GGIR's segmentWEARcrit.part5 default
    min_wear = 0.5,
    # a diary, keyed by the recording's key (raw: the part 5 id; counts: the
    # file id), each a data.frame(date, label, start, end); on a date with
    # entries they replace the rules' windows
    overrides = list(),
    # what the diary import found, for the panel to state
    diary = NULL
  )
}

sched_active <- function(s) {
  !is.null(s) && (length(s$windows) > 0 || length(s$ranges) > 0 || length(s$overrides) > 0)
}

# The 24-hour clock: "7", "07:00", "7:30", "07:30:00" and "24:00" parse, and
# so do "9am", "5:30 pm", "12am" (midnight) and "12pm" (noon). Else NA.
sched_parse_time <- function(x) {
  x <- tolower(trimws(as.character(x)))
  out <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i]) || !nzchar(x[i])) next
    t <- x[i]
    ampm <- if (grepl("[ap]\\.?m\\.?$", t)) substr(gsub("[^ap]", "", t), 1, 1) else NA_character_
    if (!is.na(ampm)) t <- trimws(sub("[ap]\\.?m\\.?$", "", t))
    p <- suppressWarnings(as.numeric(strsplit(t, ":", fixed = TRUE)[[1]]))
    if (length(p) < 1 || length(p) > 3 || any(is.na(p))) next
    h <- p[1]; m <- if (length(p) >= 2) p[2] else 0; s <- if (length(p) == 3) p[3] else 0
    if (!is.na(ampm)) {
      if (h < 1 || h > 12) next
      if (ampm == "a" && h == 12) h <- 0
      if (ampm == "p" && h < 12) h <- h + 12
    }
    if (h < 0 || m < 0 || m >= 60 || s < 0 || s >= 60) next
    v <- h * 60 + m + s / 60
    if (v > 1440) next
    out[i] <- v
  }
  out
}

sched_fmt_time <- function(m) {
  m <- as.numeric(m)
  ifelse(is.na(m), "", sprintf("%02d:%02d", floor(m / 60), round(m %% 60)))
}

# A date typed into a box: 2026-03-09 first, then 03/09/2026; anything else NA.
sched_parse_date <- function(x) {
  x <- trimws(as.character(x))
  d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  alt <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
  d[is.na(d)] <- alt[is.na(d)]
  d
}

sched_fmt_date <- function(d) {
  d <- as.Date(d)
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}

sched_type_keys <- function(s) {
  c(names(SCHED_BUILTIN), vapply(s$ranges, function(r) r$key, character(1)))
}

sched_type_labels <- function(s) {
  lab <- c(s$types[["weekday"]] %||% SCHED_BUILTIN[["weekday"]],
           s$types[["weekend"]] %||% SCHED_BUILTIN[["weekend"]],
           vapply(s$ranges, function(r) as.character(r$label), character(1)))
  stats::setNames(lab, sched_type_keys(s))
}

# The pills a window's "applies to" row offers, by key, shown by label;
# "(unnamed)" for a range with no label yet, or Shiny would show the key.
sched_pill_choices <- function(s) {
  lab <- sched_type_labels(s)
  shown <- ifelse(nzchar(trimws(lab)), lab, "(unnamed)")
  stats::setNames(names(lab), shown)
}

# The kind of day per date: a range wins, then weekend, then weekday
sched_day_type <- function(s, dates) {
  d <- as.Date(dates)
  key <- ifelse(format(d, "%u") %in% c("6", "7"), "weekend", "weekday")
  for (r in rev(s$ranges)) {   # reversed so the first range wins where two overlap
    if (is.na(r$from) || is.na(r$to)) next
    hit <- !is.na(d) & d >= as.Date(r$from) & d <= as.Date(r$to)
    key[hit] <- r$key
  }
  key
}

# A window the pipeline may use: a label, a start, an end. A row still being
# typed fails this and is never handed to part 5, which would stop on it.
sched_complete <- function(w) {
  nzchar(trimws(w$label %||% "")) && length(w$start) == 1 && length(w$end) == 1 &&
    is.finite(w$start) && is.finite(w$end) && w$end > w$start
}

sched_windows_for <- function(s, type_key) {
  w <- Filter(function(x) sched_complete(x) && (length(x$applies) == 0 || type_key %in% x$applies),
              s$windows)
  if (length(w) == 0) return(w)
  w[order(vapply(w, function(x) as.numeric(x$start), numeric(1)))]
}

sched_override_for <- function(s, key, date) {
  if (is.null(key) || is.na(key) || length(s$overrides) == 0) return(NULL)
  ov <- s$overrides[[as.character(key)]]
  if (!is.data.frame(ov) || nrow(ov) == 0) return(NULL)
  rows <- ov[!is.na(ov$date) & ov$date == as.Date(date), , drop = FALSE]
  if (nrow(rows) == 0) return(NULL)
  w <- lapply(seq_len(nrow(rows)), function(i)
    list(label = as.character(rows$label[i]), start = as.numeric(rows$start[i]),
         end = as.numeric(rows$end[i]), applies = character(0)))
  w <- Filter(sched_complete, w)
  if (length(w) == 0) return(NULL)
  w[order(vapply(w, function(x) x$start, numeric(1)))]
}

# The windows in force for one recording on one date: the diary's if it has
# entries for that date, otherwise the rules'. Both pages read this.
sched_day_windows <- function(s, key, date) {
  ov <- sched_override_for(s, key, date)
  if (!is.null(ov)) return(ov)
  sched_windows_for(s, sched_day_type(s, date))
}

# Every problem at once, in words; empty means the schedule is sound
sched_validate <- function(s, epoch_sec = NULL) {
  msg <- character(0)
  say <- function(...) msg <<- c(msg, paste0(...))

  labels <- sched_type_labels(s)
  if (!nzchar(trimws(labels[["weekday"]])) || !nzchar(trimws(labels[["weekend"]])))
    say("The two built in kinds of day both need a name.")
  if (anyDuplicated(tolower(trimws(labels))))
    say("Two kinds of day have the same name.")

  for (i in seq_along(s$ranges)) {
    r <- s$ranges[[i]]
    if (!nzchar(trimws(r$label %||% ""))) say("Date range ", i, " has no label.")
    if (is.na(r$from) || is.na(r$to)) say("Date range ", i, " needs both dates.")
    else if (as.Date(r$from) > as.Date(r$to)) say("Date range ", i, " ends before it starts.")
  }
  if (length(s$ranges) > 1) {
    for (i in 2:length(s$ranges)) for (j in 1:(i - 1)) {
      a <- s$ranges[[i]]; b <- s$ranges[[j]]
      if (any(is.na(c(a$from, a$to, b$from, b$to)))) next
      if (as.Date(a$from) <= as.Date(b$to) && as.Date(b$from) <= as.Date(a$to))
        say("Date ranges ", j, " and ", i, " overlap.")
    }
  }

  for (i in seq_along(s$windows)) {
    w <- s$windows[[i]]
    if (!nzchar(trimws(w$label %||% ""))) say("Window ", i, " has no label.")
    if (is.na(w$start) || is.na(w$end)) say("Window ", i, " needs a start and an end, as HH:MM.")
    else {
      if (w$end <= w$start) say("Window ", i, " ends before it starts. A window that crosses midnight is two windows.")
      if (!is.null(epoch_sec) && is.finite(epoch_sec) && epoch_sec > 0) {
        if ((w$start * 60) %% epoch_sec != 0 || (w$end * 60) %% epoch_sec != 0)
          say("Window ", i, " does not sit on the ", epoch_sec, " second epoch boundary.")
      }
    }
  }
  wl <- vapply(s$windows, function(w) tolower(trimws(w$label %||% "")), character(1))
  if (anyDuplicated(wl[nzchar(wl)])) say("Two windows have the same label.")

  # overlap is judged per kind of day
  for (k in sched_type_keys(s)) {
    ws <- sched_windows_for(s, k)
    if (length(ws) < 2) next
    for (i in 2:length(ws)) {
      a <- ws[[i - 1]]; b <- ws[[i]]
      if (any(is.na(c(a$start, a$end, b$start, b$end)))) next
      if (b$start < a$end)
        say("Windows ", shQuote(a$label), " and ", shQuote(b$label), " overlap on ",
            labels[[k]], " days.")
    }
  }
  unique(msg)
}

sched_text <- function(s) {
  if (!sched_active(s)) return("none")
  nw <- length(s$windows); nt <- length(sched_type_keys(s)); nd <- length(s$overrides)
  paste0(nw, if (nw == 1) " window · " else " windows · ", nt, " day types",
         if (nd > 0) paste0(" · diary for ", nd) else "")
}

# A fingerprint of everything that changes a number, so a run can say what
# schedule it was computed with; "" when not in force.
sched_sig <- function(s) {
  if (!sched_active(s)) return("")
  canon <- list(
    types = unname(sched_type_labels(s)),
    ranges = lapply(s$ranges, function(r) list(as.character(r$label), as.character(r$from), as.character(r$to))),
    windows = lapply(s$windows, function(w) list(as.character(w$label), as.numeric(w$start), as.numeric(w$end), sort(as.character(w$applies)))),
    min_wear = as.numeric(s$min_wear),
    overrides = local({
      ov <- s$overrides
      if (length(ov) > 0) ov <- ov[order(names(ov))]
      lapply(ov, function(o) list(as.character(o$date), as.character(o$label), as.numeric(o$start), as.numeric(o$end)))
    }))
  digest::digest(canon, algo = "xxhash64")
}

# GGIR's activity diary after g.conv.actlog: one row per recording date with
# the day's boundaries as times, hours and names. Part 5 names a segment by
# the boundaries around it ("work-evening"); gaps are "daystart", "other" and
# "dayend", GGIR's own names. A date with no windows gets no row and part 5
# treats the day whole, as GGIR does for a date missing from a diary.
sched_qwindow <- function(s, id, dates) {
  d <- as.Date(dates)
  rows <- list()
  for (i in seq_along(d)) {
    ws <- sched_day_windows(s, id, d[i])
    if (length(ws) == 0) next
    b <- sched_boundaries(ws)
    if (is.null(b)) next
    rows[[length(rows) + 1]] <- list(
      ID = as.character(id), date = d[i],
      times = sched_fmt_hms(b$minutes), values = b$minutes / 60, names = b$names)
  }
  if (length(rows) == 0) return(NULL)
  out <- data.frame(ID = vapply(rows, function(r) r$ID, ""),
                    date = as.Date(vapply(rows, function(r) as.character(r$date), "")),
                    stringsAsFactors = FALSE)
  out$qwindow_times <- lapply(rows, function(r) r$times)
  out$qwindow_values <- lapply(rows, function(r) r$values)
  out$qwindow_names <- lapply(rows, function(r) r$names)
  out
}

# The boundaries of one kind of day: every window start and end in order, each
# named by the window starting there or "other", daystart and dayend at the ends
sched_boundaries <- function(ws) {
  ws <- Filter(sched_complete, ws)
  if (length(ws) == 0) return(NULL)
  starts <- vapply(ws, function(w) as.numeric(w$start), numeric(1))
  ends <- vapply(ws, function(w) as.numeric(w$end), numeric(1))
  labels <- vapply(ws, function(w) sched_ggir_name(w$label), character(1))
  minutes <- sort(unique(c(starts, ends)))
  names_ <- vapply(minutes, function(m) {
    i <- which(starts == m)
    if (length(i) > 0) labels[i[1]] else if (m >= 1440) "dayend" else "other"
  }, character(1))
  if (minutes[1] > 0) { minutes <- c(0, minutes); names_ <- c("daystart", names_) }
  if (minutes[length(minutes)] < 1440) { minutes <- c(minutes, 1440); names_ <- c(names_, "dayend") }
  list(minutes = minutes, names = names_)
}

# GGIR strips "_" and "-" from a diary heading and joins names with "-"
sched_ggir_name <- function(label) {
  x <- gsub("[-_]+", " ", trimws(as.character(label)))
  x <- gsub("[[:space:]]+", " ", x)
  if (!nzchar(x)) "window" else x
}

sched_fmt_hms <- function(m) {
  s <- round(as.numeric(m) * 60)
  sprintf("%02d:%02d:%02d", s %/% 3600, (s %% 3600) %/% 60, s %% 60)
}

# The numeric form, one set of boundaries for every day, when there is no ID
sched_qwindow_numeric <- function(s) {
  ws <- Filter(sched_complete, s$windows)
  m <- sort(unique(unlist(lapply(ws, function(w) c(w$start, w$end)))))
  m <- m[is.finite(m)]
  sort(unique(c(0, m / 60, 24)))
}

sched_has_windows <- function(s) {
  if (!sched_active(s)) return(FALSE)
  any(vapply(s$windows, sched_complete, logical(1))) ||
    any(vapply(s$overrides, function(ov) is.data.frame(ov) && nrow(ov) > 0, logical(1)))
}

# DIARIES

# A diary is what each participant reported, day by day; on a date with
# entries its windows replace the rules'. It is a csv in GGIR's activity
# diary layout: ID, then a date, then one column per activity holding its
# start time under the activity's name, another date starting the next day.
# An activity runs until the next start, the last one to the end of the day.

.SCHED_GAP_NAMES <- c("other", "daystart", "dayend")
.SCHED_DATE_FORMATS <- c("%d-%m-%Y", "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d")

# Every cell that reads as a date under the format
.sched_diary_dates <- function(cells, fmt) {
  d <- suppressWarnings(as.Date(cells, format = fmt))
  # %Y-%m-%d reads "12-03-2026" as year 12
  d[!is.na(d) & format(d, "%Y") < "1900"] <- NA
  d
}

# The GGIR diary, read by GGIR's own rules: entries keyed by the diary's ID,
# the date format used, and what was skipped.
sched_read_ggir_diary <- function(path, dateformat = NULL) {
  x <- tryCatch(utils::read.csv(path, check.names = FALSE, colClasses = "character",
                                strip.white = TRUE, na.strings = character(0), fileEncoding = "UTF-8-BOM"),
                error = function(e) NULL)
  if (is.null(x) || ncol(x) < 3 || nrow(x) == 0) {
    return(list(entries = list(), dateformat = NA_character_, problems = "The file has no rows with an ID, a date and a time."))
  }
  heads <- sub("\\.[0-9]+$", "", names(x))                # read.csv suffixes repeated headings
  skip <- grepl("impute|imputa|uncertain", heads, ignore.case = TRUE)   # GGIR's rule
  cells <- unlist(x[, -1, drop = FALSE], use.names = FALSE)
  cells <- cells[!is.na(cells) & nzchar(cells)]
  fmts <- if (is.null(dateformat)) .SCHED_DATE_FORMATS else dateformat
  hits <- vapply(fmts, function(f) sum(!is.na(.sched_diary_dates(cells, f))), numeric(1))
  if (all(hits == 0)) {
    return(list(entries = list(), dateformat = NA_character_,
                problems = "No cell reads as a date. GGIR's default is day-month-year, such as 02-03-2026."))
  }
  fmt <- fmts[which.max(hits)]
  entries <- list(); problems <- character(0)
  for (r in seq_len(nrow(x))) {
    id <- trimws(as.character(x[r, 1]))
    if (!nzchar(id)) next
    day <- NULL; bounds <- list()
    flush <- function() {
      if (is.null(day) || length(bounds) == 0) return()
      b <- do.call(rbind, lapply(bounds, function(z) data.frame(label = z$label, t = z$t, stringsAsFactors = FALSE)))
      b <- b[order(b$t), , drop = FALSE]
      ends <- c(b$t[-1], 1440)
      keep <- !(tolower(b$label) %in% .SCHED_GAP_NAMES) & ends > b$t
      if (any(keep)) {
        entries[[id]] <<- rbind(entries[[id]],
          data.frame(date = rep(day, sum(keep)), label = b$label[keep], start = b$t[keep], end = ends[keep],
                     stringsAsFactors = FALSE))
      }
    }
    for (j in 2:ncol(x)) {
      v <- trimws(as.character(x[r, j]))
      if (!nzchar(v) || is.na(v)) next
      d <- .sched_diary_dates(v, fmt)
      if (!is.na(d)) { flush(); day <- d; bounds <- list(); next }
      if (skip[j] || is.null(day)) next
      t <- sched_parse_time(v)
      if (is.na(t)) { problems <- c(problems, paste0(id, ", ", format(day), ": '", v, "' under ", heads[j], " is not a time")); next }
      bounds[[length(bounds) + 1]] <- list(label = sched_ggir_name(heads[j]), t = t)
    }
    flush()
  }
  list(entries = entries, dateformat = fmt, problems = unique(problems))
}

# Match a diary's IDs to the loaded recordings by pipeline id or loaded name,
# either without its extension; the result is keyed by the pipeline id.
sched_norm_id <- function(x) tolower(trimws(sub("\\.(gt3x|cwa|bin|csv|agd)$", "", as.character(x), ignore.case = TRUE)))

sched_match_diary <- function(entries, recs) {
  # recs: list of list(pid, names)
  overrides <- list(); matched <- character(0)
  for (rc in recs) {
    keys <- unique(sched_norm_id(c(rc$pid, rc$names)))
    hit <- names(entries)[sched_norm_id(names(entries)) %in% keys]
    if (length(hit) == 0) next
    overrides[[rc$pid]] <- entries[[hit[1]]]
    matched <- c(matched, hit[1])
  }
  list(overrides = overrides, matched = unique(matched), unmatched = setdiff(names(entries), matched))
}
