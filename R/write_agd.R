#' Write Activity Counts to an .agd File
#'
#' Writes a per-epoch counts data frame to a SQLite \code{.agd} file (the
#' standard \code{settings} + \code{data} schema), readable by
#' \code{\link{read.agd}}.
#'
#' @param data Data frame with \code{timestamp} (POSIXct) and \code{axis1},
#'   \code{axis2}, \code{axis3}; optional \code{steps}, \code{lux} and
#'   \code{inclineOff}/\code{Standing}/\code{Sitting}/\code{Lying}.
#' @param path Output \code{.agd} path.
#' @param epoch_length Epoch length in seconds (default 60).
#' @param start_time Recording start (POSIXct); defaults to the first timestamp.
#' @param device_serial,device_name,subject_name,sample_rate,mode Metadata for
#'   the settings table.
#'
#' @return The output \code{path}, invisibly.
#' @export
write.agd <- function(data, path, epoch_length = 60, start_time = NULL,
                      device_serial = "", device_name = "wGT3XBT",
                      subject_name = "", sample_rate = 30, mode = 61) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("write.agd() requires the 'RSQLite' package")
  }
  ts_col <- intersect(c("timestamp", "time"), names(data))[1]
  if (is.na(ts_col)) stop("data needs a 'timestamp' (or 'time') column")
  if (!all(c("axis1", "axis2", "axis3") %in% names(data))) {
    stop("data needs axis1, axis2, axis3")
  }
  ts <- as.POSIXct(data[[ts_col]])
  if (is.null(start_time)) start_time <- ts[1]

  to_ticks <- function(t) (as.numeric(t) + 62135596800) * 1e7
  col <- function(name) if (name %in% names(data)) data[[name]] else rep(0L, nrow(data))

  data_tbl <- data.frame(
    dataTimestamp   = to_ticks(ts),
    axis1 = as.integer(round(data$axis1)),
    axis2 = as.integer(round(data$axis2)),
    axis3 = as.integer(round(data$axis3)),
    steps = as.integer(round(col("steps"))),
    lux   = as.integer(round(col("lux"))),
    inclineOff      = as.integer(col("inclineOff")),
    inclineStanding = as.integer(col("inclineStanding")),
    inclineSitting  = as.integer(col("inclineSitting")),
    inclineLying    = as.integer(col("inclineLying")),
    stringsAsFactors = FALSE
  )

  settings_tbl <- data.frame(
    settingName  = c("softwarename", "deviceversion", "devicename", "deviceserial",
                     "modenumber", "epochlength", "startdatetime", "stopdatetime",
                     "original sample rate", "subjectname", "epochcount"),
    settingValue = c("canhrActi", "1.9.2", device_name, device_serial,
                     as.character(mode), as.character(epoch_length),
                     sprintf("%.0f", to_ticks(start_time)),
                     sprintf("%.0f", to_ticks(ts[length(ts)] + epoch_length)),
                     as.character(sample_rate), subject_name, as.character(nrow(data))),
    stringsAsFactors = FALSE
  )

  con <- RSQLite::dbConnect(RSQLite::SQLite(), path)
  on.exit(RSQLite::dbDisconnect(con))
  RSQLite::dbWriteTable(con, "settings", settings_tbl, overwrite = TRUE)
  RSQLite::dbWriteTable(con, "data", data_tbl, overwrite = TRUE)
  invisible(path)
}


#' Convert a Raw .gt3x File to an .agd File
#'
#' Computes counts with \code{\link{gt3x.counts}} and writes them to a small
#' \code{.agd}, carrying device/subject metadata from the \code{.gt3x} header.
#' Requires \pkg{agcounts}.
#'
#' @param gt3x_path Path to the \code{.gt3x} file.
#' @param agd_path Output \code{.agd} path; defaults next to the input.
#' @param epoch Epoch length in seconds (default 60).
#' @param lfe Use the low-frequency extension filter (default \code{FALSE}).
#' @param tz Time zone (default \code{"UTC"}).
#'
#' @return The output \code{.agd} path.
#' @export
gt3x.to.agd <- function(gt3x_path, agd_path = NULL, epoch = 60, lfe = FALSE, tz = "UTC") {
  if (is.null(agd_path)) {
    agd_path <- sub("\\.gt3x$", sprintf("_%dsec.agd", epoch), gt3x_path, ignore.case = TRUE)
  }
  ct   <- gt3x.counts(gt3x_path, epoch = epoch, lfe = lfe, tz = tz)
  info <- read.gt3x::parse_gt3x_info(gt3x_path)
  get  <- function(k, d) { v <- info[[k]]; if (is.null(v) || length(v) == 0) d else v }

  write.agd(
    data = data.frame(timestamp = ct$time, axis1 = ct$axis1, axis2 = ct$axis2, axis3 = ct$axis3),
    path = agd_path,
    epoch_length  = epoch,
    start_time    = ct$time[1],
    device_serial = as.character(get("Serial Number", "")),
    device_name   = as.character(get("Device Type", "wGT3XBT")),
    subject_name  = as.character(get("Subject Name", "")),
    sample_rate   = as.numeric(get("Sample Rate", 30))
  )
  agd_path
}
