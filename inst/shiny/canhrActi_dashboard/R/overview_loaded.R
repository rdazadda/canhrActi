# Overview with data loaded: the pieces that draw it. Nothing here is
# reactive; each function takes a file record (or a coverage summary read from
# one) and returns markup. Coverage is drawn the same way everywhere: worn is
# a solid fill, non-wear a hatch.

# .agd stores time as .NET ticks: 100 ns since year 1.
ovl_ticks <- function(x) {
  as.POSIXct(as.numeric(x) / 1e7 - 62135596800, origin = "1970-01-01", tz = "UTC")
}

# What the recording covers and how much was worn: from the wear time analysis
# if it has run for this file, else the wear bouts ActiLife wrote into the .agd.
ovl_coverage <- function(f, wear = NULL) {
  ts <- if (is.data.frame(f$data) && "timestamp" %in% names(f$data)) f$data$timestamp else NULL
  if (is.null(ts) || length(ts) == 0) return(NULL)

  epoch <- suppressWarnings(as.numeric(f$epoch_length))
  if (length(epoch) != 1 || is.na(epoch) || epoch <= 0) epoch <- 60
  start <- min(ts)
  end <- max(ts) + epoch
  recorded <- as.numeric(difftime(end, start, units = "hours"))
  out <- list(start = start, end = end, recorded = recorded,
              segs = NULL, worn = NA_real_, gaps = 0L, source = "none")

  mask <- wear$wear
  if (!is.null(mask) && length(mask) == length(ts)) {
    mask <- !is.na(mask) & as.logical(mask)
    r <- rle(mask)
    stops <- cumsum(r$lengths)
    starts <- stops - r$lengths + 1
    segs <- data.frame(from = ts[starts], to = ts[stops] + epoch, worn = r$values)
    out$source <- "analysis"
  } else {
    b <- f$actilife_wear_time
    if (is.null(b) || !is.data.frame(b) || nrow(b) == 0 ||
        !all(c("startTicks", "stopTicks", "isWearTime") %in% names(b))) {
      return(out)
    }
    segs <- data.frame(from = ovl_ticks(b$startTicks), to = ovl_ticks(b$stopTicks),
                       worn = as.numeric(b$isWearTime) == 1)
    out$source <- "agd"
  }

  # A bout table can run past the recording's ends; clip it
  segs$from <- pmax(segs$from, start)
  segs$to <- pmin(segs$to, end)
  segs <- segs[segs$to > segs$from, , drop = FALSE]
  segs <- segs[order(segs$from), , drop = FALSE]
  if (nrow(segs) == 0) return(out)

  hrs <- as.numeric(difftime(segs$to, segs$from, units = "hours"))
  out$segs <- segs
  out$worn <- sum(hrs[segs$worn])
  out$gaps <- sum(!segs$worn)
  out
}

# The coverage axis of both tables: the two ends with their clock time and a
# label at each local midnight, placed at the time it names, plus the day lines
# for the lanes. The page script hides labels that would collide.
ovl_axis <- function(w0, w1) {
  total <- as.numeric(difftime(w1, w0, units = "secs"))
  if (length(total) != 1 || !is.finite(total) || total <= 0) return(NULL)
  tz <- attr(w0, "tzone")
  tz <- if (length(tz) == 0 || is.na(tz[1])) "" else tz[1]
  d0 <- as.Date(format(w0, "%Y-%m-%d", tz = tz)) + 1
  d1 <- as.Date(format(w1, "%Y-%m-%d", tz = tz))
  days <- if (d1 >= d0) seq(d0, d1, by = "day") else d0[0]
  # past six weeks a line a day is a blur, so weekly, then monthly
  step <- if (length(days) <= 45) 1 else if (length(days) <= 320) 7 else 30
  if (length(days) > 0) days <- days[seq(1, length(days), by = step)]
  at <- as.POSIXct(format(days, "%Y-%m-%d"), tz = tz)
  pct <- as.numeric(difftime(at, w0, units = "secs")) / total * 100
  keep <- is.finite(pct) & pct > 0 & pct < 100
  days <- days[keep]
  pos <- sprintf("left:%.3f%%;", pct[keep])
  list(
    ticks = tagList(
      tags$span(class = "is-from", fmt_date(w0, "%d %b %H:%M")),
      lapply(seq_along(pos), function(i) tags$span(style = pos[i], fmt_date(days[i], "%d %b"))),
      tags$span(class = "is-to", fmt_date(w1, "%d %b %H:%M"))),
    grid = tags$span(class = "ovl-grid", `aria-hidden` = "true",
                     lapply(pos, function(p) tags$i(style = p))))
}

# A block's hover text: what it is, from when to when, and how long
ovl_span_title <- function(kind, from, to) {
  hrs <- as.numeric(difftime(to, from, units = "hours"))
  sprintf("%s · %s to %s · %s h", kind, fmt_date(from, "%d %b %H:%M"),
          fmt_date(to, "%d %b %H:%M"), fmt_dec(hrs, 1))
}

# One coverage bar, on a window shared by every row
ovl_cov_cell <- function(cov, w0, w1) {
  if (is.null(cov)) return(tags$span(class = "ovl-cov-none", "–"))
  total <- as.numeric(difftime(w1, w0, units = "secs"))
  if (!is.finite(total) || total <= 0) return(tags$span(class = "ovl-cov-none", "–"))

  pct <- function(secs) max(0, min(100, secs / total * 100))
  left <- pct(as.numeric(difftime(cov$start, w0, units = "secs")))
  span_secs <- as.numeric(difftime(cov$end, cov$start, units = "secs"))
  width <- pct(span_secs)

  kids <- list()
  if (!is.null(cov$segs) && span_secs > 0) {
    share <- function(secs) max(0, min(100, secs / span_secs * 100))
    cursor <- cov$start
    for (i in seq_len(nrow(cov$segs))) {
      from <- cov$segs$from[i]
      to <- cov$segs$to[i]
      if (from > cursor) {
        # unclassified: left as the track colour rather than called either way
        kids <- c(kids, list(tags$span(style = sprintf("width: %.4f%%;",
                                                       share(as.numeric(difftime(from, cursor, units = "secs")))))))
      }
      worn <- isTRUE(cov$segs$worn[i])
      kids <- c(kids, list(tags$span(
        class = if (worn) "on" else "off",
        title = ovl_span_title(if (worn) "Worn" else "Non-wear", max(cursor, from), to),
        style = sprintf("width: %.4f%%;", share(as.numeric(difftime(to, max(cursor, from), units = "secs")))))))
      cursor <- max(cursor, to)
    }
  }

  tags$div(class = "ovl-cov",
           style = sprintf("margin-left: %.4f%%; width: %.4f%%;", left, width),
           title = paste0(fmt_date(cov$start, "%d %b %H:%M"), " to ", fmt_date(cov$end, "%d %b %H:%M")),
           kids)
}

# Worn hours as a bar gauge, the share of that recording's own span
ovl_gauge <- function(cov) {
  if (is.null(cov) || is.na(cov$worn)) return(tags$span(class = "ovl-cov-none", "Not scored"))
  share <- if (cov$recorded > 0) max(0, min(100, cov$worn / cov$recorded * 100)) else 0
  tags$span(class = "ovl-gauge",
            title = sprintf("%s of %s hours recorded (%.0f%%)",
                            fmt_dec(cov$worn, 1), fmt_dec(cov$recorded, 1), share),
            tags$span(class = "ovl-bar", tags$i(style = sprintf("width: %.1f%%;", share))),
            tags$span(class = "ovl-num", paste0(fmt_dec(cov$worn, 1), " h")))
}

# Mean counts per hour across the recorded span. The line breaks across
# non-wear rather than drawing through it: a zero nobody wore is absence.
ovl_trace <- function(f, cov, uid, width = 1000, height = 104) {
  if (is.null(cov) || !is.data.frame(f$data) || !("timestamp" %in% names(f$data))) return(NULL)
  col <- if ("axis1" %in% names(f$data)) "axis1" else if ("vector_magnitude" %in% names(f$data)) "vector_magnitude" else NULL
  if (is.null(col)) return(NULL)
  x <- suppressWarnings(as.numeric(f$data[[col]]))
  ts <- f$data$timestamp
  if (all(is.na(x))) return(NULL)

  hr <- floor(as.numeric(difftime(ts, cov$start, units = "hours")))
  n <- max(hr, na.rm = TRUE) + 1
  if (!is.finite(n) || n < 2) return(NULL)
  means <- tapply(x, factor(hr, levels = seq_len(n) - 1), mean, na.rm = TRUE)
  vals <- as.numeric(means)
  peak <- suppressWarnings(max(vals, na.rm = TRUE))
  if (!is.finite(peak) || peak <= 0) return(NULL)

  # an hour counts as worn when its midpoint falls inside a worn bout
  worn <- rep(TRUE, n)
  if (!is.null(cov$segs)) {
    mid <- cov$start + (seq_len(n) - 0.5) * 3600
    worn <- vapply(mid, function(m) {
      hit <- which(cov$segs$from <= m & cov$segs$to > m)
      if (length(hit) == 0) TRUE else isTRUE(cov$segs$worn[hit[1]])
    }, logical(1))
  }

  px <- function(i) (i - 0.5) / n * width
  py <- function(v) 3 + (1 - v / peak) * (height - 6)

  # the non-wear stretches first, as hatch behind the line
  parts <- character(0)
  runs <- rle(worn)
  stops <- cumsum(runs$lengths)
  starts <- stops - runs$lengths + 1
  for (k in which(!runs$values)) {
    x0 <- (starts[k] - 1) / n * width
    x1 <- stops[k] / n * width
    parts <- c(parts, sprintf(
      '<rect x="%.2f" y="0" width="%.2f" height="%d" fill="url(#%s)" stroke="var(--ovl-border)" stroke-width="1"/>',
      x0, max(0, x1 - x0), height, paste0("ovlgap-", uid)))
  }
  # then the line, one polyline per worn run, so it breaks across the gaps
  for (k in which(runs$values)) {
    idx <- seq(starts[k], stops[k])
    idx <- idx[!is.na(vals[idx])]
    if (length(idx) < 2) next
    pts <- paste(sprintf("%.2f,%.2f", px(idx), py(vals[idx])), collapse = " ")
    parts <- c(parts, sprintf(
      '<polyline points="%s" fill="none" stroke="var(--ovl-worn)" stroke-width="1.4" stroke-linejoin="round" stroke-linecap="round"/>',
      pts))
  }

  list(
    peak = peak,
    svg = HTML(sprintf(
      paste0('<svg class="ovl-trace" viewBox="0 0 %d %d" preserveAspectRatio="none" role="img" ',
             'aria-label="Mean counts per hour across the recorded span">',
             '<defs><pattern id="%s" width="6" height="6" patternTransform="rotate(135)" patternUnits="userSpaceOnUse">',
             '<rect width="6" height="6" fill="var(--ovl-gap-fill)"/>',
             '<line x1="0" y1="0" x2="0" y2="6" stroke="var(--ovl-gap-line)" stroke-width="1.4"/>',
             '</pattern></defs>%s</svg>'),
      width, height, paste0("ovlgap-", uid), paste(parts, collapse = "")))
  )
}

# Wear site, as one phrase: "Wrist, non-dominant".
ovl_worn_on <- function(f) {
  s <- f$subject_info
  limb <- present(s$limb)
  side <- present(s$dominance) %||% present(s$side)
  if (is.null(limb) && is.null(side)) return(NULL)
  if (is.null(side)) return(as.character(limb))
  if (is.null(limb)) return(as.character(side))
  paste0(limb, ", ", tolower(side))
}

ovl_positive <- function(x) {
  n <- suppressWarnings(as.numeric(present(x)))
  if (length(n) != 1 || is.na(n) || n <= 0) return(NULL)
  n
}

# The six values that decide which analyses are valid (epoch, filter, wear
# site, mass, age) ride on the closed disclosure row; the rest is provenance.
ovl_fields <- function(f) {
  dev <- f$device_info
  s <- f$subject_info
  data <- f$data
  has_times <- is.data.frame(data) && "timestamp" %in% names(data) && nrow(data) > 0
  first <- if (has_times) min(data$timestamp) else agd_time(dev$start_datetime)
  last <- if (has_times) max(data$timestamp) else agd_time(dev$stop_datetime)
  software <- paste(c(present(dev$software), present(dev$software_version)), collapse = " ")
  dob <- agd_time(s$date_of_birth)
  height <- ovl_positive(s$height)
  mass <- ovl_positive(s$mass)
  rate <- ovl_positive(dev$sample_rate)
  battery <- ovl_positive(dev$battery)

  fmt_time <- function(t) if (is.null(t)) NULL else format(t, "%Y-%m-%d %H:%M")

  list(
    constrain = list(
      list("Epoch length", paste0(fmt_int(f$epoch_length), " s"), "epochs"),
      list("Filter", present(dev$filter), "filter"),
      list("Worn on", ovl_worn_on(f), ""),
      list("Raw sample rate", if (is.null(rate)) NULL else paste0(fmt_int(rate), " Hz"), "raw"),
      list("Body mass", if (is.null(mass)) NULL else paste0(fmt_dec(mass, 1), " kg"), ""),
      list("Age", present(s$age), "years")
    ),
    device = list(
      list("Type", present(dev$device_type)),
      list("Serial", present(dev$serial_number)),
      list("Firmware", present(dev$firmware)),
      list("Software", if (nzchar(software)) software else NULL)
    ),
    recording = list(
      list("Epochs", fmt_int(f$n_epochs)),
      list("First epoch", fmt_time(first)),
      list("Last epoch", fmt_time(last)),
      list("Battery", if (is.null(battery)) NULL else paste0(fmt_dec(battery, 2), " V"))
    ),
    subject = list(
      list("Subject ID", present(s$id)),
      list("Sex", format_sex(s$sex)),
      list("Born", if (is.null(dob)) NULL else format(dob, "%Y-%m-%d")),
      list("Height", if (is.null(height)) NULL else paste0(fmt_dec(height, 1), " cm")),
      list("Race", present(s$race)),
      list("Side", present(s$side))
    )
  )
}

# One table: two name/value pairs abreast, grouped by spanning label rows
ovl_meta_table <- function(fields) {
  cell <- function(entry) {
    value <- entry[[2]]
    tagList(tags$td(entry[[1]]),
            if (is.null(value)) tags$td(class = "miss", "Not in file") else tags$td(as.character(value)))
  }
  group <- function(label) tags$td(class = "grp", colspan = "2", label)

  left <- c(list(list(grp = "Constrains the analysis")), lapply(fields$constrain, function(e) list(pair = e)),
            list(list(grp = "Device")), lapply(fields$device, function(e) list(pair = e)))
  right <- c(list(list(grp = "Recording")), lapply(fields$recording, function(e) list(pair = e)),
             list(list(grp = "Subject")), lapply(fields$subject, function(e) list(pair = e)))
  n <- max(length(left), length(right))
  side <- function(x, i) {
    if (i > length(x)) return(tagList(tags$td(), tags$td()))
    if (!is.null(x[[i]]$grp)) group(x[[i]]$grp) else cell(x[[i]]$pair)
  }

  tags$table(
    class = "ovl-gt ovl-meta",
    tags$colgroup(tags$col(style = "width: 150px;"), tags$col(),
                  tags$col(style = "width: 130px;"), tags$col()),
    tags$tbody(lapply(seq_len(n), function(i) tags$tr(side(left, i), side(right, i))))
  )
}

# The closed summary row: the constraints, in ink, with their names muted.
ovl_meta_chips <- function(fields) {
  tags$span(class = "ovl-disc-sum", lapply(fields$constrain, function(e) {
    if (is.null(e[[2]])) {
      tags$span(class = "miss", paste0("no ", tolower(e[[1]])))
    } else {
      tags$span(tags$b(as.character(e[[2]])), if (nzchar(e[[3]])) paste0(" ", e[[3]]))
    }
  }))
}

ovl_chevron <- function() {
  HTML(paste0('<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.8" ',
              'stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">',
              '<path d="M6 3.5L10.5 8L6 12.5"></path></svg>'))
}
