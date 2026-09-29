# Wear time, after a run: the pieces that draw it. Nothing here is reactive.
# Three states, one language, as the Overview coverage bar: solid is worn,
# hatch is recorded and not worn, empty is outside the recording.

WT_MINUTES_IN_DAY <- 1440

# Fill is hours worn on a four step ramp; a day that misses the rule carries a
# bar along its foot, and a day the recording does not cover in full is notched.
wt_cell <- function(worn_h, recorded_h, valid, label) {
  step <- if (is.na(worn_h) || worn_h <= 0.02) 0
    else if (worn_h < 6) 1 else if (worn_h < 12) 2 else if (worn_h < 18) 3 else 4
  classes <- paste0("wt-cell wt-s", step,
                    if (isTRUE(valid)) "" else " is-bad",
                    if (!is.na(recorded_h) && recorded_h < 23.9) " is-part" else "")
  tags$span(class = classes, title = label)
}

# Midnight to midnight on an axis every row shares
wt_strip <- function(segs) {
  pct <- function(m) sprintf("%.3f%%", m / WT_MINUTES_IN_DAY * 100)
  parts <- list()
  if (!is.null(segs) && nrow(segs) > 0) {
    for (i in seq_len(nrow(segs))) {
      parts[[length(parts) + 1]] <- tags$span(
        class = if (isTRUE(segs$worn[i])) "wt-on" else "wt-off",
        style = paste0("left: ", pct(segs$from[i]), "; width: ", pct(segs$to[i] - segs$from[i]), ";"))
    }
  }
  tags$div(
    class = "wt-strip", parts,
    tags$i(style = "left: 25%;"), tags$i(style = "left: 50%;"), tags$i(style = "left: 75%;"))
}

# VERDICTS

# One shape in both tables: the decision first, then a reason only when it
# adds something the other columns do not carry.

wt_status <- function(ok, word, reason = NULL) {
  list(ok = isTRUE(ok), word = word, reason = reason)
}

# What a day was, and whether it counts.
wt_day_verdict <- function(day, min_hours_num, min_hours_label) {
  part <- !is.na(day$recorded_h) && day$recorded_h < 23.9
  worn <- day$wear_hours

  if (!isTRUE(day$valid)) {
    if (worn < 0.5) {
      return(wt_status(FALSE, "Excluded",
                       if (part) paste0("not worn, ", fmt_dec(day$recorded_h, 1), " h recorded") else "not worn"))
    }
    if (part && worn >= day$recorded_h - 0.05) {
      return(wt_status(FALSE, "Excluded",
                       paste0("part day, ", fmt_dec(day$recorded_h, 1), " h recorded, ",
                              fmt_dec(min_hours_num - worn, 1), " h short")))
    }
    short <- min_hours_num - worn
    return(wt_status(FALSE, "Excluded",
                     paste0("under ", min_hours_label, " h by ", fmt_dec(short, 1), " h")))
  }

  if (part) {
    return(wt_status(TRUE, "Valid", paste0("part day, ", fmt_dec(day$recorded_h, 1), " h recorded")))
  }
  over <- worn - min_hours_num
  if (over < 1) {
    return(wt_status(TRUE, "Valid", paste0("clears ", min_hours_label, " h by ", fmt_dec(over, 1), " h")))
  }
  wt_status(TRUE, "Valid")
}

# What a recording was, and whether it is in the study. Why it failed comes
# first, then a pass at exactly the minimum, then what it lost along the way.
wt_recording_verdict <- function(res, min_days, min_hours_label) {
  valid <- as.numeric(res$valid_days %||% 0)
  daily <- res$daily

  if (!isTRUE(res$meets_criteria)) {
    return(wt_status(FALSE, "Excluded",
                     paste0(fmt_int(valid), " valid ", if (valid == 1) "day" else "days",
                            ", ", fmt_int(min_days), " required")))
  }
  if (valid <= min_days) {
    return(wt_status(TRUE, "Included",
                     paste0("at the minimum, ", fmt_int(min_days), " required")))
  }

  if (is.null(daily) || nrow(daily) == 0) return(wt_status(TRUE, "Included"))
  dead <- sum(daily$wear_hours < 0.5 & daily$recorded_h > 23.9, na.rm = TRUE)
  last <- daily[nrow(daily), ]
  if (dead >= 2) {
    return(wt_status(TRUE, "Included", paste0(dead, " days recorded, never worn")))
  }
  if (!isTRUE(last$valid) && !is.na(last$recorded_h) && last$recorded_h < 23.9) {
    return(wt_status(TRUE, "Included",
                     paste0("last day ", fmt_dec(last$wear_hours, 1), " h of ",
                            fmt_dec(last$recorded_h, 1), " h recorded")))
  }
  short <- sum(!daily$valid)
  if (short > 0) {
    return(wt_status(TRUE, "Included",
                     paste0(short, if (short == 1) " day under " else " days under ", min_hours_label, " h")))
  }
  wt_status(TRUE, "Included", "every day valid")
}

# The decision in a fixed column, then the caveat. A day that did not count
# carries a mark as well as a word.
wt_verdict_cell <- function(v) {
  tags$span(
    class = "wt-verdict",
    tags$span(class = paste("wt-vd", if (v$ok) "is-ok" else "is-out"),
              if (v$ok) NULL else tags$i(class = "wt-vd-mark", `aria-hidden` = "true"),
              v$word),
    if (!is.null(v$reason)) tags$span(class = "wt-vd-why", v$reason) else NULL)
}

# Days of one recording, as a list of rows the drawing code can walk.
wt_days <- function(res) {
  daily <- res$daily
  if (is.null(daily) || nrow(daily) == 0) return(list())
  segs <- res$day_segs
  lapply(seq_len(nrow(daily)), function(i) {
    d <- as.list(daily[i, ])
    d$segs <- if (is.null(segs)) NULL else segs[segs$date == as.character(daily$date[i]), , drop = FALSE]
    d
  })
}

wt_fig <- function(value, unit = NULL, label) {
  tags$div(class = "wt-fig",
    tags$div(tags$span(class = "wt-fig-n", value),
             if (!is.null(unit)) tags$span(class = "wt-fig-u", unit)),
    tags$div(class = "wt-fig-l", label))
}

wt_rule_div <- function() tags$div(class = "wt-fig-rule", `aria-hidden` = "true")

wt_chip <- function(label, n, on = FALSE, key = NULL) {
  tags$span(class = paste("wt-chip", if (on) "is-on" else ""), `data-chip` = key,
            label, tags$b(fmt_int(n)))
}

wt_chevron <- function() {
  HTML(paste0('<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.8" ',
              'stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">',
              '<path d="M6 3.5L10.5 8L6 12.5"></path></svg>'))
}

wt_caret <- function(up = TRUE) {
  d <- if (isTRUE(up)) "M5 2.5 8.5 7h-7z" else "M5 7.5 1.5 3h7z"
  HTML(paste0('<svg class="wt-caret" viewBox="0 0 10 10" fill="currentColor" aria-hidden="true"><path d="',
              d, '"></path></svg>'))
}
