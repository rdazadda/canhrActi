# One circadian engine, either file type. This file is the adapter: it turns
# a counts file or a raw recording into the same bundle (activity, timestamps,
# epoch, wear and sleep masks) and the run loop works on bundles, as GGIR's
# acc.metric does. Three things differ: counts are summed when coarsened and
# ENMO/MAD averaged; raw activity is scaled from g to mg here, once; and a
# run is brought to one epoch, only ever by aggregating, the coarsest present.

CIRC_TARGET_EPOCH <- 60          # seconds, the base a raw read is brought to
CIRC_RAW_METRICS <- c("ENMO", "ENMOa", "MAD")

# The epoch a run uses: the coarsest present, so a single-epoch counts study
# keeps scoring at its own epoch. A raw read at 5 s is brought to 60 s.
circ_target_epoch <- function(shared, family = "counts") {
  if (identical(family, "raw")) {
    nat <- vapply(shared$raw %||% list(), function(r)
      suppressWarnings(as.numeric(tryCatch(r$meta$windowsizes[1], error = function(e) NA))),
      numeric(1))
    nat <- nat[is.finite(nat) & nat > 0]
    return(max(c(CIRC_TARGET_EPOCH, nat)))
  }
  nat <- vapply(shared$files %||% list(), function(f)
    suppressWarnings(as.numeric(f$epoch_length %||% NA)), numeric(1))
  nat <- nat[is.finite(nat) & nat > 0]
  if (length(nat) == 0) return(CIRC_TARGET_EPOCH)
  max(nat)
}

# The switch picks the family and the metric picks the column inside it. Both
# sides run the same methods; the switch selects recordings and a column,
# never an analysis.

circ_n_counts <- function(shared) length(shared$files %||% list())
circ_n_raw <- function(shared) length(shared$raw %||% list())

circ_families <- function(shared) {
  c(if (circ_n_counts(shared) > 0) "counts",
    if (circ_n_raw(shared) > 0) "raw")
}

# Counts first when both are loaded
circ_default_family <- function(shared) {
  f <- circ_families(shared)
  if (length(f) == 0) "counts" else f[1]
}

circ_metric_choices <- function(shared, family = "counts") {
  if (identical(family, "raw")) {
    raws <- shared$raw %||% list()
    if (length(raws) == 0) return(character(0))
    have <- vapply(CIRC_RAW_METRICS, function(m)
      sum(vapply(raws, function(r) circ_raw_has(r, m), logical(1))), integer(1))
    keep <- CIRC_RAW_METRICS[have > 0]
    if (length(keep) == 0) return(character(0))
    return(stats::setNames(paste0("raw:", keep), keep))
  }
  if (circ_n_counts(shared) == 0) return(character(0))
  stats::setNames(c("axis1", "vm"), c("axis1, vertical", "vector magnitude"))
}

circ_family_of_metric <- function(metric) if (circ_is_raw_metric(metric)) "raw" else "counts"

circ_is_raw_metric <- function(metric) isTRUE(startsWith(metric %||% "", "raw:"))
circ_metric_name <- function(metric) sub("^raw:", "", metric %||% "")

circ_metric_label <- function(metric) {
  if (circ_is_raw_metric(metric)) {
    paste0(circ_metric_name(metric), ", raw acceleration, mg")
  } else if (identical(metric, "vm")) {
    "vector magnitude, counts"
  } else {
    "axis1, counts"
  }
}

# The unit a level output carries; M10, L5, MESOR and amplitude are not unit free
circ_unit <- function(metric) if (circ_is_raw_metric(metric)) "mg" else "counts"

# The metric named as the instrument, for the provenance sheet
circ_metric_provenance <- function(metric) {
  if (circ_is_raw_metric(metric)) {
    paste0(circ_metric_name(metric), ", raw acceleration (mg)")
  } else if (identical(metric, "vm")) {
    "Vector magnitude (ActiGraph counts)"
  } else {
    "Axis 1 (ActiGraph counts)"
  }
}

circ_raw_ms <- function(r) {
  ms <- tryCatch(r$imputed$metashort, error = function(e) NULL)
  if (is.data.frame(ms) && nrow(ms) > 0) return(ms)
  ms <- tryCatch(r$meta$metashort, error = function(e) NULL)
  if (is.data.frame(ms) && nrow(ms) > 0) return(ms)
  NULL
}

circ_raw_has <- function(r, m) {
  ms <- circ_raw_ms(r)
  !is.null(ms) && m %in% names(ms)
}

# Block aggregation only ever coarsens; a trailing partial block is dropped
# rather than scaled.
circ_agg <- function(x, k, how = c("mean", "sum")) {
  how <- match.arg(how)
  k <- as.integer(k)
  if (is.na(k) || k <= 1L) return(x)
  n <- length(x)
  keep <- (n %/% k) * k
  if (keep < k) return(x[0])
  m <- matrix(x[seq_len(keep)], nrow = k)
  if (identical(how, "sum")) colSums(m) else colMeans(m)
}

# A block is worn only if every epoch in it was worn
circ_agg_all <- function(x, k) {
  k <- as.integer(k)
  if (is.na(k) || k <= 1L) return(as.logical(x))
  n <- length(x); keep <- (n %/% k) * k
  if (keep < k) return(logical(0))
  m <- matrix(as.logical(x)[seq_len(keep)], nrow = k)
  colSums(m, na.rm = TRUE) == k
}

# Sleep takes the majority of the block and comes back as character "S"/"W",
# whichever convention it went in on. The output must stay character:
# sleep.tudor.locke() tests sleep.state == "S", and the counts side hands
# over character while GGIR's T5A5 is numeric 1/0. NA stays NA.
circ_agg_vote <- function(x, k) {
  s <- if (is.character(x) || is.factor(x)) {
    xc <- tolower(as.character(x))
    ifelse(is.na(xc), NA_real_, ifelse(xc %in% c("s", "sleep", "1"), 1, 0))
  } else {
    as.numeric(x)
  }
  as_sw <- function(v) ifelse(is.na(v) | is.nan(v), NA_character_,
                              ifelse(v >= 0.5, "S", "W"))
  k <- as.integer(k)
  if (is.na(k) || k <= 1L) return(as_sw(s))
  n <- length(s); keep <- (n %/% k) * k
  if (keep < k) return(character(0))
  m <- matrix(s[seq_len(keep)], nrow = k)
  as_sw(suppressWarnings(colMeans(m, na.rm = TRUE)))
}

circ_agg_first <- function(x, k) {
  k <- as.integer(k)
  if (is.na(k) || k <= 1L) return(x)
  n <- length(x); keep <- (n %/% k) * k
  if (keep < k) return(x[0])
  x[seq(1L, keep, by = k)]
}

# BUNDLES

circ_skip <- function(id, name, kind, reason) {
  list(id = id, name = name, kind = kind, ok = FALSE, reason = reason)
}

circ_bundle_counts <- function(fid, f, shared, metric, epoch) {
  nm <- f$name %||% fid
  d <- f$data
  if (!is.data.frame(d) || nrow(d) == 0) {
    return(circ_skip(fid, nm, "counts", "The file carries no epoch data."))
  }
  if (circ_is_raw_metric(metric)) {
    return(circ_skip(fid, nm, "counts",
      "An AGD file holds counts. The raw acceleration is not in the file."))
  }
  if (identical(metric, "vm") && !all(c("axis1", "axis2", "axis3") %in% names(d))) {
    return(circ_skip(fid, nm, "counts",
      "Only one axis was recorded, so there is no vector magnitude."))
  }
  e0 <- suppressWarnings(as.numeric(f$epoch_length %||% 60))
  if (!is.finite(e0) || e0 <= 0) e0 <- 60
  if (e0 > epoch + 1e-9) {
    return(circ_skip(fid, nm, "counts", sprintf(
      "Recorded at %g s, which is coarser than the %g s the rest of this run uses.",
      e0, epoch)))
  }
  k <- round(epoch / e0)
  act <- if (identical(metric, "vm")) {
    sqrt(d$axis1^2 + d$axis2^2 + d$axis3^2)
  } else d$axis1

  wear <- NULL
  wt <- tryCatch(shared$results$wear_time[[fid]], error = function(e) NULL)
  if (!is.null(wt) && !is.null(wt$wear) && length(wt$wear) == nrow(d)) {
    wear <- as.logical(wt$wear)
    dly <- wt$daily
    if (!is.null(dly) && "valid" %in% names(dly) && "timestamp" %in% names(d)) {
      wear <- wear & (as.Date(d$timestamp) %in% as.Date(dly$date[dly$valid]))
    }
  }

  sleep <- NULL; ssrc <- "none"
  st <- tryCatch(shared$results$sleep[[fid]]$sleep_state, error = function(e) NULL)
  if (!is.null(st) && length(st) == nrow(d)) { sleep <- st; ssrc <- "Sleep tab" }
  if (is.null(sleep)) {
    sleep <- tryCatch(canhrActi::sleep.cole.kripke(d$axis1, apply_rescoring = TRUE,
                                                   epoch_length = e0),
                      error = function(e) NULL)
    ssrc <- if (is.null(sleep)) "none" else "Cole-Kripke fallback"
  }

  list(id = fid, name = nm, kind = "counts", ok = TRUE, reason = NA_character_,
       # the metric is carried so a figure can name what it plots
       metric = metric,
       subject_id = tryCatch(f$subject_info$id, error = function(e) NULL),
       activity = circ_agg(act, k, "sum"),
       timestamps = circ_agg_first(d$timestamp, k),
       epoch_length = k * e0, native_epoch = e0,
       wear_time = if (is.null(wear)) NULL else circ_agg_all(wear, k),
       sleep_state = if (is.null(sleep)) NULL else circ_agg_vote(sleep, k),
       sleep_source = ssrc,
       wear_params = tryCatch(shared$results$wear_time[[fid]]$parameters,
                              error = function(e) NULL))
}

circ_bundle_raw <- function(rid, r, metric, epoch) {
  nm <- tryCatch(ovr_name(r, rid), error = function(e) rid)
  if (!circ_is_raw_metric(metric)) {
    return(circ_skip(rid, nm, "raw", paste0(
      "A gt3x file holds raw acceleration, not counts. Choose a raw metric, ",
      "or convert the file to .agd first.")))
  }
  mcol <- circ_metric_name(metric)
  ms <- circ_raw_ms(r)
  if (is.null(ms)) return(circ_skip(rid, nm, "raw", "The read produced no epoch series."))
  if (!mcol %in% names(ms)) {
    return(circ_skip(rid, nm, "raw", sprintf(
      "%s was not computed at import. Re-import with that metric switched on.", mcol)))
  }
  e0 <- suppressWarnings(as.numeric(tryCatch(r$meta$windowsizes[1], error = function(e) NA)))
  if (!is.finite(e0) || e0 <= 0) e0 <- 5
  if (e0 > epoch + 1e-9) {
    return(circ_skip(rid, nm, "raw", sprintf(
      "Read at %g s, coarser than the %g s this run uses.", e0, epoch)))
  }
  k <- round(epoch / e0)

  # g to milli-g, once and here, as GGIR does
  act <- as.numeric(ms[[mcol]]) * 1000

  tt <- ms$timestamp
  if (is.character(tt) || is.factor(tt)) {
    tt <- tryCatch(canhrActi:::.raw.iso8601.to.posix(as.character(tt),
                     tz = tryCatch(r$tz$effective_tz, error = function(e) "") %||% ""),
                   error = function(e) NULL)
    if (is.null(tt)) tt <- as.POSIXct(as.character(ms$timestamp), format = "%Y-%m-%dT%H:%M:%S%z")
  }

  # Part 3's per-epoch table is row-aligned to the metashort: invalid is the
  # non-wear flag and the other column is GGIR's own sleep/wake scoring
  wear <- NULL; sleep <- NULL; ssrc <- "none"
  ep <- tryCatch(r$sleep$epochs, error = function(e) NULL)
  if (is.data.frame(ep) && nrow(ep) == nrow(ms)) {
    if ("invalid" %in% names(ep)) wear <- as.numeric(ep$invalid) == 0
    scol <- setdiff(names(ep), c("time", "invalid", "night"))
    if (length(scol) >= 1) {
      sleep <- as.numeric(ep[[scol[1]]])
      ssrc <- paste0("GGIR part 3, ", scol[1])
      # GGIR's rule for the regularity index (CalcSleepRegularityIndex.R):
      # invalid epochs become NA, never sleep
      if (!is.null(wear)) sleep[which(!wear)] <- NA
    }
  }
  # Without part 3, fall back to the part-2 wear decision. rout is at the long
  # epoch (windowsizes[2], 900 s by default), so it is expanded first
  if (is.null(wear)) {
    rt <- tryCatch(r$imputed$rout %||% r$wear$rout, error = function(e) NULL)
    ws2 <- suppressWarnings(as.numeric(tryCatch(r$meta$windowsizes[2], error = function(e) NA)))
    if (is.data.frame(rt) && "r5" %in% names(rt) && is.finite(ws2) && ws2 >= e0) {
      w <- rep(as.numeric(rt$r5) == 0, each = round(ws2 / e0))
      if (length(w) >= nrow(ms)) wear <- w[seq_len(nrow(ms))]
      else wear <- c(w, rep(utils::tail(w, 1), nrow(ms) - length(w)))
    }
  }

  list(id = rid, name = nm, kind = "raw", ok = TRUE, reason = NA_character_,
       # the metric is carried so a figure can name what it plots
       metric = metric,
       subject_id = tryCatch(r$sleep$id, error = function(e) NULL),
       activity = circ_agg(act, k, "mean"),
       timestamps = circ_agg_first(tt, k),
       epoch_length = k * e0, native_epoch = e0,
       wear_time = if (is.null(wear)) NULL else circ_agg_all(wear, k),
       sleep_state = if (is.null(sleep)) NULL else circ_agg_vote(sleep, k),
       sleep_source = ssrc,
       # GGIR's day rule from GGIR's own decision: a day is valid when it holds
       # includedaycrit valid hours; the engine applies it to the per-day rows
       valid_days = tryCatch({
         d <- r$wear$daily
         if (is.data.frame(d) && all(c("date", "valid") %in% names(d)))
           as.Date(d$date[d$valid %in% TRUE]) else NULL
       }, error = function(e) NULL),
       # The same rule for the provenance sheet, in minutes. GGIR part 2 has
       # no minimum-number-of-days rule, so that one stays blank.
       wear_params = list(
         algorithm = "GGIR part 2",
         min_wear_day = tryCatch({
           v <- suppressWarnings(as.numeric(r$params$includedaycrit)[1])
           if (is.finite(v)) v * 60 else NULL
         }, error = function(e) NULL)))
}

# Every recording on the selected family only; a file on the other side of
# the switch is not part of this run and never appears in the not-scored list.
circ_sources <- function(shared, metric, epoch = NULL, family = NULL) {
  if (is.null(family)) family <- circ_family_of_metric(metric)
  if (is.null(epoch)) epoch <- circ_target_epoch(shared, family)
  out <- list()
  if (identical(family, "raw")) {
    rs <- shared$raw %||% list()
    for (rid in names(rs)) out[[length(out) + 1L]] <- circ_bundle_raw(rid, rs[[rid]], metric, epoch)
  } else {
    fs <- shared$files %||% list()
    for (fid in names(fs)) out[[length(out) + 1L]] <- circ_bundle_counts(fid, fs[[fid]], shared, metric, epoch)
  }
  out
}

circ_scored <- function(src) Filter(function(b) isTRUE(b$ok), src)
circ_unscored <- function(src) Filter(function(b) !isTRUE(b$ok), src)

# The unit goes on the axis, since a level is not unit free. Raw metrics read
# "ENMO (mg)" as the package's raw figures do; the counts label is unchanged.
circ_axis_label <- function(metric) {
  if (circ_is_raw_metric(metric)) {
    paste0(circ_metric_name(metric), " (mg)")
  } else {
    "Activity (counts/min)"
  }
}

# The switch, the Overview's Counts / Raw markup (.ovr-seg is global in
# styles.css). Shown only when both sides are loaded.
circ_switch_ui <- function(ns, shared, family) {
  fams <- circ_families(shared)
  if (length(fams) < 2) return(NULL)
  seg <- function(key, label, n) {
    tags$button(id = ns(paste0("cr_fam_", key)), type = "button",
                class = paste("action-button ovr-seg-b",
                              if (identical(family, key)) "is-on" else ""),
                label, tags$b(fmt_int(n)))
  }
  tags$div(class = "ovr-seg", role = "tablist",
           seg("counts", "Counts", circ_n_counts(shared)),
           seg("raw", "Raw", circ_n_raw(shared)))
}

# The epoch series behind one recording, for the charts that need it (the
# actogram, the periodograms, the Marler fit, DFA): the same bundle the run
# used, whichever family. Metric and epoch come from the run's stored
# parameters, not the live boxes.
circ_series_for <- function(shared, fid, params = NULL) {
  if (is.null(fid) || !nzchar(fid)) return(NULL)
  metric <- params$metric %||% "axis1"
  epoch <- suppressWarnings(as.numeric(params$epoch_length %||% CIRC_TARGET_EPOCH))
  if (!is.finite(epoch) || epoch <= 0) epoch <- CIRC_TARGET_EPOCH

  # Dispatch on where the recording is, not on the metric string: a params
  # left over from the other side would send a raw id into shared$files
  in_raw <- fid %in% names(shared$raw %||% list())
  in_counts <- fid %in% names(shared$files %||% list())
  if (!in_raw && !in_counts) return(NULL)

  if (in_raw) {
    if (!circ_is_raw_metric(metric)) {
      av <- circ_metric_choices(shared, "raw")
      metric <- if (length(av) > 0) unname(av[1]) else "raw:ENMO"
    }
    b <- circ_bundle_raw(fid, shared$raw[[fid]], metric, epoch)
  } else {
    if (circ_is_raw_metric(metric)) metric <- "axis1"
    b <- circ_bundle_counts(fid, shared$files[[fid]], shared, metric, epoch)
  }
  if (!isTRUE(b$ok)) return(NULL)
  b
}
