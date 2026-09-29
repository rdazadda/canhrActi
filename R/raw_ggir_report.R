# GGIR draws its own part-5 report and writes its own part-5 tables: the recording is
# written into GGIR's milestone layout and GGIR's g.report.part5() and visualReport() read
# it, so the csv files and the PDF are the ones GGIR itself produces. GGIR is a Suggests;
# without it the function returns a state rather than failing.

#' Run GGIR's Own Part-5 Report and Plot Over a Recording
#'
#' Writes the recording, its nights and its time use into a GGIR milestone
#' directory and calls GGIR's \code{g.report.part5()} and
#' \code{visualReport()} on it, so the csv files and the report PDF are the
#' ones GGIR itself produces rather than a reproduction.
#'
#' @param x A canhrActi_raw recording.
#' @param nights The part-4 night summary from \code{\link{raw.sleep.nights}}.
#' @param timeuse The part-5 object from \code{\link{raw.timeuse}}.
#' @param dir Where to build the milestone tree. Defaults to a new temporary
#'   directory, which is what the dashboard wants; pass a path to keep it.
#' @param report TRUE to draw the report PDF, which is the slow half.
#' @param params_cleaning,params_output Named lists of overrides on GGIR's
#'   own defaults for the report, as \code{GGIR::load_params()} names them,
#'   for example \code{list(week_weekend_aggregate.part5 = TRUE)} or
#'   \code{list(segmentWEARcrit.part5 = 0.5)}. Names GGIR does not know are
#'   ignored.
#'
#' @return A list with \code{state}, \code{dir}, \code{pdf} (path or NA), and
#'   \code{csv}, a named character vector of the result files GGIR wrote.
#' @export
raw.ggir.report <- function(x, nights, timeuse, dir = NULL, report = TRUE,
                            params_cleaning = NULL, params_output = NULL) {
  out <- list(state = "ok", dir = NA_character_, pdf = NA_character_,
              csv = character(0), messages = character(0))
  if (!requireNamespace("GGIR", quietly = TRUE)) {
    out$state <- "no_ggir"
    out$messages <- paste0("GGIR is not installed, so its own report cannot be produced. ",
                           "install.packages(\"GGIR\") and re-open this page.")
    return(out)
  }
  if (is.null(timeuse) || !identical(timeuse$status$state, "ok")) {
    out$state <- timeuse$status$state %||% "no_timeuse"
    out$messages <- timeuse$status$messages %||% character(0)
    return(out)
  }
  # not a runif() name: g.report.part5 calls set.seed(1234), so one would repeat
  if (is.null(dir)) dir <- tempfile("ggir_")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  out$dir <- dir

  w <- tryCatch({
    write.ggir.milestone(x, dir, parts = c(1, 2, 3, 4, 5), nights = nights, timeuse = timeuse)
    TRUE
  }, error = function(e) conditionMessage(e))
  if (!isTRUE(w)) {
    out$state <- "milestone_failed"; out$messages <- as.character(w); return(out)
  }

  P <- GGIR::load_params()
  # named overrides on GGIR's defaults; unknown names are dropped, not passed on
  for (n in names(params_cleaning)) if (n %in% names(P$params_cleaning)) P$params_cleaning[[n]] <- params_cleaning[[n]]
  for (n in names(params_output)) if (n %in% names(P$params_output)) P$params_output[[n]] <- params_output[[n]]
  ok <- tryCatch({
    GGIR::g.report.part5(metadatadir = dir, f0 = 1, f1 = 1,
                         params_cleaning = P$params_cleaning,
                         params_output = P$params_output, verbose = FALSE)
    TRUE
  }, error = function(e) conditionMessage(e))
  if (!isTRUE(ok)) {
    out$state <- "report_failed"; out$messages <- as.character(ok)
  }

  if (isTRUE(report)) {
    # with part6_threshold_combi NULL visualReport lists the wrong level and draws nothing
    combi <- list.dirs(file.path(dir, "meta", "ms5.outraw"), recursive = FALSE,
                       full.names = FALSE)
    combi <- setdiff(combi, "sib.reports")
    pv <- tryCatch({
      GGIR::visualReport(metadatadir = dir, f0 = 1, f1 = 1, verbose = FALSE,
                         part6_threshold_combi = combi[1],
                         GGIRversion = as.character(utils::packageVersion("GGIR")),
                         params_sleep = P$params_sleep,
                         params_output = P$params_output,
                         params_general = P$params_general)
      TRUE
    }, error = function(e) conditionMessage(e))
    if (!isTRUE(pv)) out$messages <- c(out$messages, paste("visualReport:", pv))
    pdfs <- list.files(file.path(dir, "results", "file summary reports"),
                       pattern = "\\.pdf$", full.names = TRUE)
    if (length(pdfs) > 0) out$pdf <- pdfs[1]
  }

  res <- list.files(file.path(dir, "results"), pattern = "\\.csv$",
                    full.names = TRUE, recursive = TRUE)
  names(res) <- basename(res)
  out$csv <- res
  out
}

#' GGIR's Own Part-5 Report Over Several Recordings
#'
#' Copies the part-5 milestone of each \code{raw.ggir.report()} result into one
#' milestone folder and calls GGIR's \code{g.report.part5()} over all of them, as
#' GGIR's own run over a study does. The study-level person summary can have columns
#' a one-recording report does not.
#'
#' @param reports A list of \code{raw.ggir.report()} results.
#' @param dir,params_cleaning,params_output As for \code{raw.ggir.report()}.
#' @return A list with \code{state}, \code{dir}, \code{csv} and \code{messages}.
#' @keywords internal
#' @noRd
.raw.ggir.report.study <- function(reports, dir = NULL, params_cleaning = NULL,
                                   params_output = NULL) {
  out <- list(state = "ok", dir = NA_character_, csv = character(0),
              messages = character(0))
  if (!requireNamespace("GGIR", quietly = TRUE)) {
    out$state <- "no_ggir"
    out$messages <- "GGIR is not installed, so its own report cannot be produced."
    return(out)
  }
  ms5 <- unlist(lapply(reports, function(r) {
    d <- r$dir
    if (!is.character(d) || length(d) != 1 || is.na(d)) return(character(0))
    list.files(file.path(d, "meta", "ms5.out"), full.names = TRUE)
  }))
  if (length(ms5) == 0) {
    out$state <- "no_timeuse"
    out$messages <- "No recording has a part-5 milestone to report on."
    return(out)
  }
  # GGIR names the milestone after the file, so two recordings cannot share a name
  dup <- basename(ms5)[duplicated(basename(ms5))]
  if (length(dup) > 0) {
    out$state <- "duplicate_names"
    out$messages <- paste0("More than one recording is named ", sub("\\.RData$", "", dup[1]),
                           "; GGIR's study report keeps one row set per file name.")
    return(out)
  }
  if (is.null(dir)) dir <- tempfile("ggir_")
  dir.create(file.path(dir, "meta", "ms5.out"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "results", "QC"), recursive = TRUE, showWarnings = FALSE)
  out$dir <- dir
  if (!all(file.copy(ms5, file.path(dir, "meta", "ms5.out", basename(ms5))))) {
    out$state <- "milestone_failed"
    out$messages <- "The part-5 milestones could not be copied into one folder."
    return(out)
  }

  P <- GGIR::load_params()
  for (n in names(params_cleaning)) if (n %in% names(P$params_cleaning)) P$params_cleaning[[n]] <- params_cleaning[[n]]
  for (n in names(params_output)) if (n %in% names(P$params_output)) P$params_output[[n]] <- params_output[[n]]
  ok <- tryCatch({
    GGIR::g.report.part5(metadatadir = dir, f0 = 1, f1 = length(ms5),
                         params_cleaning = P$params_cleaning,
                         params_output = P$params_output, verbose = FALSE)
    TRUE
  }, error = function(e) conditionMessage(e))
  if (!isTRUE(ok)) {
    out$state <- "report_failed"; out$messages <- as.character(ok)
  }
  res <- list.files(file.path(dir, "results"), pattern = "\\.csv$",
                    full.names = TRUE, recursive = TRUE)
  names(res) <- basename(res)
  out$csv <- res
  out
}

# The result files split into the part-5 tables GGIR writes
#' @rdname raw.ggir.report
#' @param res A \code{raw.ggir.report()} result.
#' @export
ggir.report.tables <- function(res) {
  f <- res$csv
  if (length(f) == 0) return(list())
  out <- list(
    # _full_ also starts part5_daysummary_, so exclude it explicitly
    daysummary = f[grepl("^part5_daysummary_", names(f)) &
                   !grepl("_full_", names(f))],
    personsummary = f[grepl("^part5_personsummary_", names(f))],
    daysummary_full = f[grepl("part5_daysummary_full_", names(f))])
  lapply(out, function(v) v[order(names(v))])
}
