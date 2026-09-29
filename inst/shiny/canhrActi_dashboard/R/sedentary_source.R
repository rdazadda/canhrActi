# The Sedentary tab, either file type. The bundle comes from circ_sources()
# in circadian_source.R; only the threshold is particular to this tab: a
# counts-per-minute cut point on counts, and on raw the light boundary of the
# 48 published cut points the Activity raw page offers.

SED_COUNT_CUTS <- stats::setNames(
  c("freedson", "canhr"),
  c("Freedson 1998 · under 100 CPM", "CANHR 2025 · 150 CPM or fewer"))

sed_raw_available <- function(shared) {
  raws <- shared$raw %||% list()
  if (length(raws) == 0) return(character(0))
  Filter(function(m) any(vapply(raws, function(r) circ_raw_has(r, m), logical(1))),
         CIRC_RAW_METRICS)
}

# raw.cutpoints() filtered to the metrics read and to the sets that state a
# light boundary, since without one a cut point cannot define sedentary
sed_cut_table <- function(shared) {
  have <- sed_raw_available(shared)
  if (length(have) == 0) return(NULL)
  cp <- tryCatch(canhrActi::raw.cutpoints(available = have),
                 error = function(e) tryCatch(canhrActi::raw.cutpoints(), error = function(e2) NULL))
  if (!is.data.frame(cp) || nrow(cp) == 0) return(NULL)
  cp <- cp[cp$metric %in% have & is.finite(suppressWarnings(as.numeric(cp$light))), , drop = FALSE]
  if (nrow(cp) == 0) return(NULL)
  cp[order(cp$metric, as.numeric(cp$light)), , drop = FALSE]
}

sed_cut_choices <- function(shared, family = "counts") {
  if (!identical(family, "raw")) return(SED_COUNT_CUTS)
  cp <- sed_cut_table(shared)
  if (is.null(cp)) return(character(0))
  lab <- sprintf("%s %s, %s · %s mg",
                 cp$study, cp$group, cp$metric, format(as.numeric(cp$light), trim = TRUE))
  stats::setNames(cp$key, lab)
}

# A key that is valid for this family. The family and the threshold arrive by
# two round trips, so a fast click could pair "raw" with "freedson".
sed_resolve_cut <- function(shared, key, family = "counts") {
  ch <- sed_cut_choices(shared, family)
  if (length(ch) == 0) return(NULL)
  if (!is.null(key) && key %in% unname(ch)) key else unname(ch)[1]
}

# The mg line a raw cut point draws and its metric; exact match only
sed_cut_spec <- function(shared, key) {
  cp <- sed_cut_table(shared)
  if (is.null(cp) || is.null(key)) return(NULL)
  row <- cp[cp$key == key, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  num <- function(v) { z <- suppressWarnings(as.numeric(v)); if (length(z) && is.finite(z)) z else NA_real_ }
  list(key = row$key[1], metric = row$metric[1], study = row$study[1], group = row$group[1],
       light = num(row$light[1]), moderate = num(row$moderate[1]), vigorous = num(row$vigorous[1]))
}

sed_cut_label <- function(shared, key, family = "counts") {
  if (!identical(family, "raw")) {
    return(if (identical(key, "canhr")) "CANHR 2025, 150 CPM or fewer"
           else "Freedson 1998, under 100 CPM")
  }
  s <- sed_cut_spec(shared, key)
  if (is.null(s)) return("no raw cut point available")
  sprintf("%s %s, %s below %s mg", s$study, s$group, s$metric,
          format(s$light, trim = TRUE))
}

# The engine wants an ordered factor whose lowest level is "sedentary", the
# same five levels the counts cut points produce
SED_LEVELS <- c("sedentary", "light", "moderate", "vigorous", "very_vigorous")

sed_intensity_raw <- function(mg, spec) {
  if (is.null(spec) || !is.finite(spec$light)) return(NULL)
  # a set with no moderate boundary leaves everything above light as light
  mod <- if (is.finite(spec$moderate)) spec$moderate else Inf
  vig <- if (is.finite(spec$vigorous)) spec$vigorous else Inf
  out <- rep(NA_character_, length(mg))
  ok <- !is.na(mg)
  out[ok & mg < spec$light] <- "sedentary"
  out[ok & mg >= spec$light & mg < mod] <- "light"
  out[ok & mg >= mod & mg < vig] <- "moderate"
  out[ok & mg >= vig] <- "vigorous"
  factor(out, levels = SED_LEVELS, ordered = TRUE)
}

sed_intensity_counts <- function(cpm, key) {
  if (identical(key, "canhr")) canhrActi::CANHR.Cutpoints(cpm) else canhrActi::freedson(cpm)
}

# The bundle with $intensity added, or a skip with $reason when it cannot be
# scored under this threshold
sed_prepare <- function(shared, b, cut_key, family) {
  if (!isTRUE(b$ok)) return(b)
  # the key actually scored with (sed_resolve_cut() may have corrected it)
  b$cut_key <- cut_key
  if (identical(family, "raw")) {
    spec <- sed_cut_spec(shared, cut_key)
    if (is.null(spec)) {
      return(circ_skip(b$id, b$name, b$kind, "No published cut point states a light boundary for the metrics this file carries."))
    }
    b$intensity <- sed_intensity_raw(b$activity, spec)
    b$cut_spec <- spec
    b$threshold_txt <- sprintf("%s mg", format(spec$light, trim = TRUE))
  } else {
    # the count cut points are defined on counts per minute
    cpm <- canhrActi::to_cpm(b$activity, b$epoch_length)
    b$intensity <- sed_intensity_counts(cpm, cut_key)
    b$cut_spec <- NULL
    b$threshold_txt <- if (identical(cut_key, "canhr")) "150 CPM" else "100 CPM"
  }
  if (is.null(b$intensity)) {
    return(circ_skip(b$id, b$name, b$kind, "The threshold could not be applied to this recording."))
  }
  b
}

# The raw metric a chosen cut point implies, as on the Activity raw page
sed_metric_for <- function(shared, cut_key, family) {
  if (!identical(family, "raw")) return("axis1")
  s <- sed_cut_spec(shared, cut_key)
  if (is.null(s)) return("raw:ENMO")
  paste0("raw:", s$metric)
}

# Every recording on the selected side, classified and ready for the engine.
sed_sources <- function(shared, cut_key, family) {
  cut_key <- sed_resolve_cut(shared, cut_key, family)
  metric <- sed_metric_for(shared, cut_key, family)
  src <- circ_sources(shared, metric, circ_target_epoch(shared, family), family)
  lapply(src, function(b) sed_prepare(shared, b, cut_key, family))
}
