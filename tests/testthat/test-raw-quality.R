# raw.quality.report, raw.quality.checks and their helpers against GGIR's
# data_quality_report.csv and part2_summary.csv. Reference data come from CANHRACTI_GGIR_REF
# (the MOS2 and EE milestones and results, and the two failmodes files); tests skip when a
# file is missing and live comparisons when GGIR is not installed. The csv parity is shown two
# ways: the row written with data.table::fwrite as GGIR writes it is byte-identical to the
# stored line, and every column is compared as text (numbers at the csv's 15 significant
# digits, with a small relative tolerance where as.character() and fwrite round the 15th digit
# differently). The MOS2 raw file is read once through raw.getmeta (about 45 s).

.ggir_ref <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")

skip_if_no_ggir_ref <- function() {
  if (.ggir_ref == "" || !dir.exists(.ggir_ref)) {
    testthat::skip("CANHRACTI_GGIR_REF is unset or does not point to an existing folder")
  }
}
skip_if_no_ggir <- function() {
  if (!requireNamespace("GGIR", quietly = TRUE)) {
    testthat::skip("GGIR is not installed; live parity comparisons skipped")
  }
}
skip_if_no_file <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    testthat::skip(paste0("reference file not found: ", path))
  }
}
skip_if_no_datatable <- function() {
  if (!requireNamespace("data.table", quietly = TRUE)) testthat::skip("data.table is not installed")
}

ref_file <- function(...) file.path(sub("/+$", "", .ggir_ref), ...)
MOS2 <- function() ref_file("din", "MOS2E39230594.gt3x")
MOS2_RDATA <- function() ref_file("out", "output_din", "meta", "basic", "meta_MOS2E39230594.gt3x.RData")
MOS2_MS2 <- function() ref_file("out", "output_din", "meta", "ms2.out", "MOS2E39230594.gt3x.RData")
MOS2_QC <- function() ref_file("out", "output_din", "results", "QC", "data_quality_report.csv")
MOS2_SUM <- function() ref_file("out", "output_din", "results", "part2_summary.csv")
EE_RDATA <- function() ref_file("timing_out", "output_timing", "meta", "basic", "meta_EE_left_29.5.2017-05-30.gt3x.RData")
EE_QC <- function() ref_file("timing_out", "output_timing", "results", "QC", "data_quality_report.csv")
EE_SUM <- function() ref_file("timing_out", "output_timing", "results", "part2_summary.csv")
TRUNC <- function() ref_file("failmodes", "din", "truncated.gt3x")
SHORT <- function() ref_file("failmodes", "din", "tooshort.gt3x")

MOS2_TZ <- "America/Anchorage"   # what the stored run's desiredtz "" resolved to
EE_TZ <- "Europe/Helsinki"

QC_NAMES <- c("filename", "file.corrupt", "file.too.short", "use.temperature",
              "scale.x", "scale.y", "scale.z", "offset.x", "offset.y", "offset.z",
              "temperature.offset.x", "temperature.offset.y", "temperature.offset.z",
              "cal.error.start", "cal.error.end", "n.10sec.windows", "n.hours.considered",
              "QCmessage", "mean.temp", "device.serial.number", "NFilePagesSkipped",
              "filehealth_totimp_min", "filehealth_totimp_N")
CWA_NAMES <- c("filehealth_totimp_min", "filehealth_checksumfail_min", "filehealth_niblockid_min",
               "filehealth_fbias0510_min", "filehealth_fbias1020_min", "filehealth_fbias2030_min",
               "filehealth_fbias30_min", "filehealth_totimp_N", "filehealth_checksumfail_N",
               "filehealth_niblockid_N", "filehealth_fbias0510_N", "filehealth_fbias1020_N",
               "filehealth_fbias2030_N", "filehealth_fbias30_N")
P2_NAMES <- c("samplefreq", "device", "clipping_score", "meas_dur_dys", "meas_dur_def_proto_day",
              "wear_dur_def_proto_day", "calib_err", "calib_status",
              "N valid weekend days (WE)", "N valid weekdays (WD)")
EXTRA_NAMES <- c("too_small", "uppercase_extension", "header_timezone", "machine_timezone",
                 "desiredtz_used", "read_gt3x_version", "ggirread_version", "canhrActi_version",
                 "gap_count_over_90min", "minutes_epoch_level_imputed", "seconds_trimmed_start",
                 "seconds_trimmed_end", "samples_discarded_end", "zero_triplets_removed")
CHECK_NAMES <- c("file_size", "readable", "format", "header", "sample_rate", "gaps", "long_gaps",
                 "zero_triplets", "duration_floor", "start_trim", "end_trim", "calibration_status",
                 "calibration_error", "sphere_coverage", "coefficient_sanity", "temperature",
                 "dynamic_range", "clipping", "nonwear_score", "wear_time", "valid_days", "timezone",
                 "unexpected_resets", "header_vs_tables", "versions")
MSG_POSSIBLY <- "recalibration attempted with all available data, but possibly not good enough: Check calibration error variable to varify this"
MSG_NOT_ENOUGH <- "recalibration not done because not enough data in the file or because file is corrupt"

memo <- new.env()

stored <- function(which = c("mos2", "ee")) {
  which <- match.arg(which)
  key <- paste0("stored_", which)
  if (is.null(memo[[key]])) {
    path <- if (which == "mos2") MOS2_RDATA() else EE_RDATA()
    skip_if_no_file(path)
    e <- new.env(); load(path, envir = e)
    memo[[key]] <- list(C = e$C, M = e$M, I = e$I, desiredtz_part1 = e$desiredtz_part1)
  }
  memo[[key]]
}
read_csv_chr <- function(path) {
  skip_if_no_file(path)
  utils::read.csv(path, colClasses = "character", check.names = FALSE, stringsAsFactors = FALSE)
}
mos2_info <- function() {
  if (is.null(memo$info)) {
    skip_if_no_file(MOS2())
    memo$info <- raw.inspect(MOS2(), desiredtz = MOS2_TZ)
  }
  memo$info
}
mos2_meta <- function() {
  if (is.null(memo$meta)) memo$meta <- raw.getmeta(mos2_info(), calibration = stored("mos2")$C)
  memo$meta
}
mos2_wear <- function() {
  if (is.null(memo$wear)) memo$wear <- raw.wear.decision(mos2_meta())
  memo$wear
}
mos2_cal <- function() {
  if (is.null(memo$cal)) memo$cal <- raw.calibrate(mos2_info())
  memo$cal
}
# the 23 GGIR columns of a report row, written as g.report.part2 writes them
fwrite_line <- function(row) {
  skip_if_no_datatable()
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf))
  data.table::fwrite(x = row[, QC_NAMES], file = tf, row.names = FALSE, na = "", sep = ",", dec = ".")
  readLines(tf)
}
# column by column against a csv row read as text: strings exact, numbers as.character-exact
# or within 1e-14 relative where fwrite and as.character round the 15th digit differently
expect_row_matches_csv <- function(row, csv_row, cols = QC_NAMES) {
  for (cn in cols) {
    expect_true(cn %in% names(row), info = cn)
    mine <- row[[cn]]
    txt <- as.character(mine)
    if (identical(txt, csv_row[[cn]])) {
      expect_identical(txt, csv_row[[cn]], info = cn)
    } else if (is.numeric(mine)) {
      expect_true(abs(mine - as.numeric(csv_row[[cn]])) <= 1e-14 * abs(mine),
                  info = paste0(cn, ": ", format(mine, digits = 17), " vs csv ", csv_row[[cn]]))
    } else {
      expect_identical(txt, csv_row[[cn]], info = cn)
    }
  }
}
check_line <- function(checks, name) checks[checks$check == name, ]

test_that("P11: the MOS2 report row from the stored milestone equals data_quality_report.csv", {
  skip_if_no_ggir_ref()
  s <- stored("mos2")
  csv <- read_csv_chr(MOS2_QC())
  expect_identical(names(csv), QC_NAMES)
  r <- raw.quality.report(s$I, s$C, s$M, desiredtz = MOS2_TZ)
  expect_s3_class(r, "data.frame")
  expect_identical(nrow(r), 1L)
  expect_identical(names(r)[1:23], QC_NAMES)
  expect_identical(names(r), c(QC_NAMES, P2_NAMES, EXTRA_NAMES))
  expect_false(any(grepl("checksumfail|niblockid|fbias", names(r))))
  expect_row_matches_csv(r, csv)
  # MOS2 has no 15th-digit rounding difference
  for (cn in QC_NAMES) expect_identical(as.character(r[[cn]]), csv[[cn]], info = cn)
  # the fwrite line is the stored line, byte for byte
  expect_identical(fwrite_line(r), readLines(MOS2_QC()))
  # types of GGIR's in-memory row
  expect_type(r$file.corrupt, "logical"); expect_type(r$file.too.short, "logical")
  expect_type(r$use.temperature, "logical")
  expect_type(r$scale.x, "double"); expect_type(r$offset.z, "double")
  expect_type(r$cal.error.start, "double"); expect_type(r$cal.error.end, "double")
  expect_type(r$n.10sec.windows, "integer"); expect_type(r$n.hours.considered, "double")
  expect_type(r$mean.temp, "character"); expect_type(r$NFilePagesSkipped, "double")
  expect_type(r$filehealth_totimp_min, "double"); expect_type(r$filehealth_totimp_N, "double")
  expect_identical(r$filename, "MOS2E39230594.gt3x")
  expect_false(r$file.corrupt); expect_false(r$file.too.short); expect_false(r$use.temperature)
  expect_identical(r$scale.x, s$C$scale[1]); expect_identical(r$offset.y, s$C$offset[2])
  expect_identical(r$cal.error.start, 0.01614); expect_identical(r$cal.error.end, 0.0058)
  expect_identical(r$n.10sec.windows, 1176L); expect_identical(r$n.hours.considered, 41)
  expect_identical(r$QCmessage, MSG_POSSIBLY)
  expect_identical(r$mean.temp, ""); expect_identical(r$device.serial.number, "not extracted")
  expect_identical(r$NFilePagesSkipped, 0)
  expect_identical(r$filehealth_totimp_min, 7451.65); expect_identical(r$filehealth_totimp_N, 1440)
  expect_identical(sum(s$M$QClog$timegaps_n), 1440L)
  expect_equal(sum(s$M$QClog$timegaps_min), 7451.65)
})

test_that("P11: the part2_summary fields of the MOS2 row equal part2_summary.csv (3 decimals) and the ms2 milestone (exact)", {
  skip_if_no_ggir_ref()
  s <- stored("mos2")
  r <- raw.quality.report(s$I, s$C, s$M, desiredtz = MOS2_TZ)
  csv <- read_csv_chr(MOS2_SUM())
  expect_identical(r$samplefreq, 30); expect_identical(r$device, "actigraph")
  expect_identical(r$clipping_score, 0)
  expect_identical(r$meas_dur_dys, 6.875); expect_identical(r$meas_dur_def_proto_day, 6.875)
  expect_identical(r$wear_dur_def_proto_day, 3.0625)
  expect_identical(r$calib_err, 0.0058); expect_identical(r$calib_status, MSG_POSSIBLY)
  expect_identical(r[["N valid weekend days (WE)"]], 0); expect_identical(r[["N valid weekdays (WD)"]], 3)
  # the csv (tidyup_df rounds to 3 decimals)
  expect_identical(as.character(round(r$clipping_score, 3)), csv$clipping_score)
  expect_identical(as.character(round(r$meas_dur_dys, 3)), csv$meas_dur_dys)
  expect_identical(as.character(round(r$wear_dur_def_proto_day, 3)), csv$wear_dur_def_proto_day)
  expect_identical(as.character(round(r$calib_err, 3)), csv$calib_err)
  expect_identical(r$calib_status, csv$calib_status)
  expect_identical(as.character(r[["N valid weekend days (WE)"]]), csv[["N valid weekend days (WE)"]])
  expect_identical(as.character(r[["N valid weekdays (WD)"]]), csv[["N valid weekdays (WD)"]])
  expect_identical(as.character(r$samplefreq), csv$samplefreq); expect_identical(r$device, csv$device)
  # the stored ms2.out summary holds the in-memory (unrounded, char2num) values
  skip_if_no_file(MOS2_MS2())
  e <- new.env(); load(MOS2_MS2(), envir = e)
  SUM <- e$SUM$summary
  for (cn in c("filehealth_totimp_min", "filehealth_totimp_N", "samplefreq", "device", "clipping_score",
               "meas_dur_dys", "meas_dur_def_proto_day", "wear_dur_def_proto_day", "calib_err",
               "calib_status", "N valid weekend days (WE)", "N valid weekdays (WD)")) {
    expect_identical(r[[cn]], SUM[[cn]], info = cn)
  }
})

test_that("P11: the EE report row from its milestone equals its csvs (fwrite line byte-identical)", {
  skip_if_no_ggir_ref()
  s <- stored("ee")
  csv <- read_csv_chr(EE_QC())
  r <- raw.quality.report(s$I, s$C, s$M, desiredtz = EE_TZ)
  expect_identical(names(r)[1:23], QC_NAMES)
  expect_row_matches_csv(r, csv)
  expect_identical(fwrite_line(r), readLines(EE_QC()))
  # offset.y is the column where as.character() and fwrite round the 15th digit differently
  expect_identical(r$offset.y, s$C$offset[2])
  expect_identical(r$filehealth_totimp_N, 2)
  expect_equal(r$filehealth_totimp_min, sum(s$M$QClog$timegaps_min))
  expect_identical(as.character(r$filehealth_totimp_min), "0.0336666666666667")
  expect_identical(r$n.10sec.windows, 29490L); expect_identical(r$n.hours.considered, 168)
  expect_identical(r$cal.error.start, 0.01428); expect_identical(r$cal.error.end, 0.00629)
  sum_csv <- read_csv_chr(EE_SUM())
  expect_identical(r$wear_dur_def_proto_day, 5.84375)
  expect_identical(as.character(round(r$wear_dur_def_proto_day, 3)), sum_csv$wear_dur_def_proto_day)
  expect_identical(r$meas_dur_dys, 7); expect_identical(r$clipping_score, 0)
  expect_identical(r[["N valid weekend days (WE)"]], 2); expect_identical(r[["N valid weekdays (WD)"]], 3)
  expect_identical(r$samplefreq, 100)
  # extras from a GGIR milestone: what the header and tables give, NA for the block traces
  expect_identical(r$gap_count_over_90min, 0L); expect_identical(r$minutes_epoch_level_imputed, 0)
  expect_identical(r$seconds_trimmed_end, 2129)   # 09:50:29 header last sample minus 09:15:00
  expect_true(is.na(r$seconds_trimmed_start)); expect_true(is.na(r$samples_discarded_end))
  expect_identical(r$header_timezone, "03:00:00"); expect_identical(r$desiredtz_used, EE_TZ)
  ch <- raw.quality.checks(s$I, s$C, s$M, desiredtz = EE_TZ)
  expect_identical(check_line(ch, "timezone")$status, "ok")
  expect_identical(check_line(ch, "timezone")$message, "Device configured at +03:00; timestamps labelled Europe/Helsinki (+03:00).")
  expect_identical(check_line(ch, "end_trim")$message,
                   "35.5 min after the last whole 15-min epoch were dropped (header last sample minus last epoch).")
  expect_identical(check_line(ch, "long_gaps")$status, "ok")
  expect_identical(check_line(ch, "valid_days")$message, "5 valid days (>= 16 h), 2 weekend days.")
  expect_identical(check_line(ch, "wear_time")$message, "5.84 days worn of 7 recorded.")
  expect_identical(check_line(ch, "calibration_status")$message,
                   "Calibrated with 168 h of data (GGIR wants > 168 h); error 14.3 mg -> 6.3 mg; coefficients applied.")
  expect_identical(check_line(ch, "header_vs_tables")$message, "7.025 d in header, 7 d in tables (start and end trims).")
})

test_that("P11: the MOS2 row through raw.inspect + raw.getmeta + raw.wear.decision equals the csv, with the canhrActi additions", {
  skip_if_no_ggir_ref()
  s <- stored("mos2")
  csv <- read_csv_chr(MOS2_QC())
  info <- mos2_info(); meta <- mos2_meta(); w <- mos2_wear()
  expect_identical(as.ggir.M(meta)$metalong, s$M$metalong)
  r <- raw.quality.report(info, s$C, meta, w)
  expect_identical(names(r), c(QC_NAMES, P2_NAMES, EXTRA_NAMES))
  for (cn in QC_NAMES) expect_identical(as.character(r[[cn]]), csv[[cn]], info = cn)
  expect_identical(fwrite_line(r), readLines(MOS2_QC()))
  r_stored <- raw.quality.report(s$I, s$C, s$M, desiredtz = MOS2_TZ)
  expect_identical(r[, c(QC_NAMES, P2_NAMES)], r_stored[, c(QC_NAMES, P2_NAMES)])
  r_nowear <- raw.quality.report(info, s$C, meta)
  expect_identical(r_nowear, r)
  # the canhrActi additions
  expect_false(r$too_small); expect_false(r$uppercase_extension)
  expect_identical(r$header_timezone, "-04:00:00")
  expect_identical(r$machine_timezone, Sys.timezone())
  expect_identical(r$desiredtz_used, MOS2_TZ)
  expect_identical(r$read_gt3x_version, as.character(utils::packageVersion("read.gt3x")))
  expect_identical(r$ggirread_version, if (requireNamespace("GGIRread", quietly = TRUE)) as.character(utils::packageVersion("GGIRread")) else NA_character_)
  expect_identical(r$gap_count_over_90min, 15L)
  expect_identical(r$minutes_epoch_level_imputed, 4815)          # 57780 replicas x 5 s
  expect_identical(sum(meta$chunks$epochs_short_added), 57780)
  expect_identical(r$seconds_trimmed_start, 116)                  # 3480 samples at 30 Hz
  expect_identical(meta$chunks$rows_dropped_start[1], 3480)
  expect_identical(r$seconds_trimmed_end, 1076)                   # 17:47:56 header minus 17:30:00
  expect_identical(r$samples_discarded_end, 17160)
  expect_true(is.na(r$zero_triplets_removed))                     # the stored C carries no block trace
})

test_that("P11: with raw.calibrate's own calibration the row is the same and the zero count is 0", {
  skip_if_no_ggir_ref()
  s <- stored("mos2")
  csv <- read_csv_chr(MOS2_QC())
  cal <- mos2_cal()
  expect_identical(as.ggir.C(cal)[names(s$C) != "bsc_qc"], s$C[names(s$C) != "bsc_qc"])
  r <- raw.quality.report(mos2_info(), cal, mos2_meta(), mos2_wear())
  for (cn in QC_NAMES) expect_identical(as.character(r[[cn]]), csv[[cn]], info = cn)
  expect_identical(fwrite_line(r), readLines(MOS2_QC()))
  expect_identical(r$zero_triplets_removed, 0)
  expect_identical(sum(cal$chunks$zeros_removed), 0)
  ch <- raw.quality.checks(mos2_info(), cal, mos2_meta(), mos2_wear())
  expect_identical(check_line(ch, "zero_triplets")$status, "ok")
  expect_identical(check_line(ch, "zero_triplets")$message, "0 all-zero samples (ActiLife zero imputation not present).")
})

test_that("raw.quality.checks: every check present with a status, and the MOS2 values the spec quotes", {
  skip_if_no_ggir_ref()
  s <- stored("mos2")
  ch <- raw.quality.checks(mos2_info(), s$C, mos2_meta(), mos2_wear())
  expect_s3_class(ch, "data.frame")
  expect_identical(names(ch), c("id", "check", "status", "value", "threshold", "message"))
  expect_identical(nrow(ch), 25L)
  expect_identical(ch$id, 1:25)
  expect_identical(ch$check, CHECK_NAMES)
  expect_true(all(ch$status %in% c("ok", "warn", "fail", "info")))
  expect_true(all(nzchar(ch$message)))
  expect_type(ch$value, "character"); expect_type(ch$threshold, "character")
  m <- function(name) check_line(ch, name)$message
  st <- function(name) check_line(ch, name)$status
  expect_identical(st("file_size"), "ok"); expect_identical(m("file_size"), "22.1 MB, above GGIR's 2 MB floor.")
  expect_identical(st("readable"), "ok"); expect_identical(m("readable"), "The file can be read.")
  expect_identical(st("format"), "ok"); expect_identical(m("format"), "Format detected as ActiGraph .gt3x")
  expect_identical(st("header"), "ok"); expect_identical(m("header"), "Header parsed: 25 fields.")
  expect_identical(st("sample_rate"), "ok"); expect_identical(m("sample_rate"), "30 Hz from the header")
  expect_identical(st("gaps"), "warn")
  expect_identical(m("gaps"), "1440 gaps, 7451.65 min (5.2 of 6.9 days) were imputed by last value.")
  expect_identical(check_line(ch, "gaps")$value, "1440 gaps; 7451.65 min")
  expect_identical(st("long_gaps"), "warn")
  expect_identical(m("long_gaps"), "15 gaps over 90 min; 80.2 h were filled at epoch level and scored non-wear 3.")
  expect_identical(st("zero_triplets"), "info")   # the stored C has no block trace
  expect_identical(st("duration_floor"), "ok")
  expect_identical(m("duration_floor"), "Block 1 held 2592000 samples, above the 2 h floor of 216001.")
  expect_identical(st("start_trim"), "info"); expect_identical(m("start_trim"), "116 s trimmed so the first epoch starts at 20:30:00.")
  expect_identical(st("end_trim"), "info")
  expect_identical(m("end_trim"), "17.9 min after the last whole 15-min epoch were dropped (header last sample minus last epoch).")
  expect_identical(st("calibration_status"), "warn")
  expect_identical(m("calibration_status"),
                   "Calibrated with 41 h of data (GGIR wants > 168 h); error 16.1 mg -> 5.8 mg; coefficients applied.")
  expect_identical(check_line(ch, "calibration_status")$value, MSG_POSSIBLY)
  expect_identical(st("calibration_error"), "ok")
  expect_identical(m("calibration_error"), "Post-calibration error 5.8 mg is under the 10 mg target.")
  expect_identical(st("sphere_coverage"), "ok"); expect_identical(m("sphere_coverage"), "All three axes reach beyond +/- 0.3 g (fit possible).")
  expect_identical(st("coefficient_sanity"), "ok"); expect_identical(m("coefficient_sanity"), "Scale within 0.5 %, offsets under 21 mg: plausible.")
  expect_identical(st("temperature"), "info"); expect_identical(m("temperature"), "No temperature channel (ActiGraph); temperature not used.")
  expect_identical(st("dynamic_range"), "ok"); expect_identical(m("dynamic_range"), "8 g from serial prefix MOS")
  expect_identical(st("clipping"), "ok"); expect_identical(m("clipping"), "0 of 660 blocks clipped.")
  expect_identical(st("nonwear_score"), "info"); expect_identical(m("nonwear_score"), "343 of 660 blocks scored 3 (all axes still).")
  expect_identical(check_line(ch, "nonwear_score")$value, "0:308 1:9 2:0 3:343")
  expect_identical(st("wear_time"), "warn"); expect_identical(m("wear_time"), "3.06 days worn of 6.88 recorded.")
  expect_identical(st("valid_days"), "ok"); expect_identical(m("valid_days"), "3 valid days (>= 16 h), 0 weekend days.")
  expect_identical(st("timezone"), "warn")
  expect_identical(m("timezone"),
                   "Device configured at -04:00; timestamps labelled America/Anchorage (-08:00). Set desiredtz/configtz if this is wrong.")
  expect_identical(st("unexpected_resets"), "info"); expect_identical(m("unexpected_resets"), "0 unexpected resets in the header (GGIR does not check this).")
  expect_identical(st("header_vs_tables"), "info"); expect_identical(m("header_vs_tables"), "6.889 d in header, 6.875 d in tables (start and end trims).")
  expect_identical(st("versions"), "info")
  expect_true(grepl("^read.gt3x [0-9.]+, GGIRread [0-9.]+, canhrActi .+ produced these numbers \\(GGIR 3.3.6 semantics\\)\\.$", m("versions")))
  # the same checks from the GGIR milestone lists differ only where the block traces are missing
  ch2 <- raw.quality.checks(s$I, s$C, s$M, desiredtz = MOS2_TZ)
  expect_identical(ch2$check, CHECK_NAMES)
  same <- setdiff(CHECK_NAMES, c("file_size", "readable", "duration_floor", "start_trim"))
  expect_identical(ch2[ch2$check %in% same, c("status", "message")], ch[ch$check %in% same, c("status", "message")])
  expect_identical(check_line(ch2, "start_trim")$message, "The first epoch starts at 20:30:00 (trim not recorded for GGIR milestone input).")
})

test_that("Live GGIR: g.report.part2 on a temp copy of the milestone folder (with fabricated corrupt and too-short milestones) writes the rows this module builds", {
  skip_if_no_ggir_ref(); skip_if_no_ggir(); skip_if_no_datatable()
  skip_if_no_file(MOS2_RDATA()); skip_if_no_file(MOS2_MS2()); skip_if_no_file(TRUNC()); skip_if_no_file(SHORT())
  P <- GGIR::load_params()
  P$params_general[["desiredtz"]] <- MOS2_TZ
  tmp <- file.path(tempdir(), "canhrActi_qc_live"); unlink(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  for (d in c("meta/basic", "meta/ms2.out", "results/QC")) dir.create(file.path(tmp, d), recursive = TRUE)
  file.copy(MOS2_RDATA(), file.path(tmp, "meta/basic/"))
  file.copy(MOS2_MS2(), file.path(tmp, "meta/ms2.out/"))
  # milestones as g.part1 leaves them for the two failure modes
  mk <- function(f, name) {
    I <- suppressWarnings(GGIR::g.inspectfile(f, desiredtz = MOS2_TZ, params_rawdata = P$params_rawdata,
                                              configtz = P$params_general[["configtz"]]))
    C <- .raw.quality.Cdefault()
    if (!is.null(I$sf)) {
      C2 <- suppressWarnings(GGIR::g.calibrate(f, params_rawdata = P$params_rawdata, params_general = P$params_general,
                                               params_cleaning = P$params_cleaning, inspectfileobject = I, verbose = FALSE))
      if (!is.null(C2)) {
        C <- C2
        cal.error.start <- C$cal.error.start; cal.error.end <- C$cal.error.end
        if (length(cal.error.start) == 0) cal.error.start <- NA
        if (is.na(cal.error.start) == T | length(cal.error.end) == 0) {
          C$scale <- c(1,1,1); C$offset <- c(0,0,0); C$tempoffset <- c(0,0,0)
        } else if (cal.error.start < cal.error.end) {
          C$scale <- c(1,1,1); C$offset <- c(0,0,0); C$tempoffset <- c(0,0,0)
        }
      }
    }
    M <- suppressWarnings(GGIR::g.getmeta(f, params_metrics = P$params_metrics, params_rawdata = P$params_rawdata,
                                          params_general = P$params_general, params_cleaning = P$params_cleaning,
                                          inspectfileobject = I, verbose = FALSE))
    GGIRversion <- utils::packageVersion("GGIR"); desiredtz_part1 <- MOS2_TZ; tail_expansion_log <- NULL
    save(M, I, C, GGIRversion, desiredtz_part1, tail_expansion_log,
         file = file.path(tmp, "meta/basic", paste0("meta_", name, ".RData")))
    list(M = M, I = I, C = C)
  }
  tr <- mk(TRUNC(), "truncated.gt3x")
  sh <- mk(SHORT(), "tooshort.gt3x")
  expect_null(tr$I$sf); expect_true(tr$M$filecorrupt)
  expect_identical(sh$I$sf, 100); expect_true(sh$M$filetooshort); expect_length(sh$C$cal.error.end, 0)
  GGIR::g.report.part2(metadatadir = tmp, f0 = 1, f1 = 3, params_output = P$params_output, verbose = FALSE, desiredtz = MOS2_TZ)
  live <- read_csv_chr(file.path(tmp, "results/QC/data_quality_report.csv"))
  expect_identical(names(live), QC_NAMES)
  expect_identical(nrow(live), 3L)
  s <- stored("mos2")
  r <- raw.quality.report(s$I, s$C, s$M, desiredtz = MOS2_TZ)
  live_mos2 <- live[live$filename == "MOS2E39230594.gt3x", ]
  rownames(live_mos2) <- NULL
  expect_identical(live_mos2, read_csv_chr(MOS2_QC()))
  for (cn in QC_NAMES) expect_identical(as.character(r[[cn]]), live_mos2[[cn]], info = cn)
  # GGIR pads the filehealth columns with "" for the corrupt file and " " for the too-short
  # one; canhrActi returns NA for both
  base <- QC_NAMES[1:21]
  r_tr <- raw.quality.report(tr$I, tr$C, tr$M, desiredtz = MOS2_TZ)
  live_tr <- live[live$filename == "truncated.gt3x", ]
  for (cn in base) expect_identical(as.character(r_tr[[cn]]), live_tr[[cn]], info = cn)
  expect_identical(live_tr$filehealth_totimp_min, ""); expect_true(is.na(r_tr$filehealth_totimp_min))
  r_sh <- raw.quality.report(sh$I, sh$C, sh$M, desiredtz = MOS2_TZ)
  live_sh <- live[live$filename == "tooshort.gt3x", ]
  for (cn in base) expect_identical(as.character(r_sh[[cn]]), live_sh[[cn]], info = cn)
  expect_identical(live_sh$filehealth_totimp_min, " "); expect_true(is.na(r_sh$filehealth_totimp_min))
  expect_identical(r_sh$cal.error.end, " ")
  # the canhrActi pipeline objects for the same two files give the same 21 base columns
  info_tr <- raw.inspect(TRUNC(), desiredtz = MOS2_TZ)
  r_tr2 <- raw.quality.report(info_tr, raw.calibrate(info_tr), raw.getmeta(info_tr, NULL))
  expect_identical(r_tr2[, base], r_tr[, base])
  info_sh <- raw.inspect(SHORT(), desiredtz = MOS2_TZ)
  cal_sh <- raw.calibrate(info_sh)
  r_sh2 <- raw.quality.report(info_sh, cal_sh, raw.getmeta(info_sh, cal_sh))
  expect_identical(r_sh2[, base], r_sh[, base])
})

test_that("Live GGIR: the filehealth and part-2 fields equal g.analyse's summary after g.part2's numeric conversion", {
  skip_if_no_ggir_ref(); skip_if_no_ggir()
  s <- stored("mos2")
  P <- GGIR::load_params()
  P$params_general[["desiredtz"]] <- MOS2_TZ
  IMP <- GGIR::g.impute(s$M, s$I, params_cleaning = P$params_cleaning, desiredtz = MOS2_TZ, dayborder = 0, ID = "x")
  SUM <- GGIR::g.analyse(s$I, s$C, s$M, IMP, params_247 = P$params_247, params_phyact = P$params_phyact,
                         params_general = P$params_general, params_cleaning = P$params_cleaning, ID = "x")
  fh_live <- SUM$summary[, grep("filehealth", names(SUM$summary), value = TRUE)]
  expect_identical(names(fh_live), c("filehealth_totimp_min", "filehealth_totimp_N"))
  expect_identical(fh_live$filehealth_totimp_min, "7451.65")   # a character matrix in g.analyse.perfile
  fh <- .raw.quality.filehealth(.raw.quality.file.summary(s$M$QClog, fname = s$I$filename))
  expect_identical(fh, .raw.quality.char2num(fh_live))
  # part-2 fields: g.analyse's strings converted as g.part2 converts them
  W <- raw.wear.decision(s$M, desiredtz = MOS2_TZ)
  p2 <- .raw.quality.part2.fields(s$I, s$C, W, TRUE)
  live_p2 <- .raw.quality.char2num(SUM$summary[, P2_NAMES])
  expect_identical(p2, live_p2)
  # a cwa-style QClog through the same path: fourteen columns and the " " bias placeholders
  Mc <- s$M
  Mc$QClog <- data.frame(checksum_pass = c(FALSE, FALSE, TRUE, TRUE), blockID_current = c(13, 14, 12, 20),
                         blockID_next = c(14, 15, 15, 21), start = c(0, 0, 1000, 2000), end = c(0, 0, 1003.64, 2003),
                         blockLengthSeconds = c(0, 0, 3.64, 3), frequency_blockheader = c(100, 100, 100, 100),
                         frequency_observed = c(100, 100, 33.4, 100), imputed = c(FALSE, FALSE, TRUE, FALSE))
  SUMc <- GGIR::g.analyse(s$I, s$C, Mc, IMP, params_247 = P$params_247, params_phyact = P$params_phyact,
                          params_general = P$params_general, params_cleaning = P$params_cleaning, ID = "x")
  fhc_live <- SUMc$summary[, grep("filehealth", names(SUMc$summary), value = TRUE)]
  expect_identical(names(fhc_live), CWA_NAMES)
  fhc <- .raw.quality.filehealth(.raw.quality.file.summary(Mc$QClog, fname = s$I$filename))
  expect_identical(fhc, .raw.quality.char2num(fhc_live))
  expect_identical(fhc$filehealth_totimp_N, 1); expect_identical(fhc$filehealth_checksumfail_N, 2)
  expect_identical(fhc$filehealth_checksumfail_min, 0)
  expect_identical(fhc$filehealth_niblockid_N, 4)                 # 13 - 14 != 1: every row, as GGIR tests it
  # the " " placeholders become NA in g.part2's conversion
  expect_identical(fhc_live$filehealth_fbias0510_min, " ")
  expect_identical(fhc$filehealth_fbias0510_min, NA_real_); expect_identical(fhc$filehealth_fbias30_N, NA_real_)
  expect_equal(fhc$filehealth_totimp_min, 3.64 / 60)
  expect_equal(fhc$filehealth_niblockid_min, (3.64 + 3) / 60)
  # the whole report on that M carries the fourteen columns in place of the two
  rc <- raw.quality.report(s$I, s$C, Mc, desiredtz = MOS2_TZ)
  expect_identical(names(rc)[1:35], c(QC_NAMES[1:21], CWA_NAMES))
  expect_identical(rc[, CWA_NAMES], fhc)
})

test_that("F1: a corrupt gt3x gives file.corrupt TRUE with cal.error.start 0 and n.10sec.windows 0, and the checks fail where GGIR stops", {
  skip_if_no_ggir_ref(); skip_if_no_file(TRUNC())
  info <- raw.inspect(TRUNC(), desiredtz = MOS2_TZ)
  expect_true(info$corrupt)
  cal <- raw.calibrate(info)
  expect_identical(cal$source, "not_attempted")
  meta <- raw.getmeta(info, cal)
  expect_true(meta$filecorrupt)
  r <- raw.quality.report(info, cal, meta)
  expect_identical(names(r), c(QC_NAMES, P2_NAMES, EXTRA_NAMES))
  expect_true(r$file.corrupt); expect_false(r$file.too.short); expect_true(r$use.temperature)
  expect_identical(unlist(r[, c("scale.x", "scale.y", "scale.z")], use.names = FALSE), c(1, 1, 1))
  expect_identical(unlist(r[, c("offset.x", "offset.y", "offset.z")], use.names = FALSE), c(0, 0, 0))
  expect_identical(r$cal.error.start, 0); expect_identical(r$cal.error.end, 0)
  expect_identical(r$n.10sec.windows, 0); expect_identical(r$n.hours.considered, 0)
  expect_identical(r$QCmessage, "Autocalibration not done")
  expect_identical(r$mean.temp, ""); expect_identical(r$device.serial.number, "not extracted")
  expect_identical(r$NFilePagesSkipped, 0)
  expect_true(is.na(r$filehealth_totimp_min)); expect_true(is.na(r$filehealth_totimp_N))
  expect_true(is.na(r$samplefreq)); expect_identical(r$device, "actigraph"); expect_true(is.na(r$calib_status))
  expect_true(r$too_small); expect_true(is.na(r$header_timezone)); expect_true(is.na(r$gap_count_over_90min))
  expect_identical(raw.quality.report(info, cal, NULL), r)
  expect_identical(raw.quality.report(info, NULL, NULL)[, QC_NAMES], r[, QC_NAMES])
  ch <- raw.quality.checks(info, cal, meta)
  expect_identical(nrow(ch), 25L); expect_identical(ch$check, CHECK_NAMES)
  st <- function(name) check_line(ch, name)$status
  expect_identical(st("file_size"), "warn")
  expect_identical(check_line(ch, "file_size")$message,
                   "This file is below GGIR's 2 MB floor; GGIR would skip it. canhrActi analysed it anyway.")
  expect_identical(st("header"), "fail")
  expect_identical(check_line(ch, "header")$message, "info.txt could not be extracted; the file is corrupt or truncated.")
  expect_identical(st("sample_rate"), "fail"); expect_identical(check_line(ch, "sample_rate")$message, "Sample frequency not recognised.")
  expect_identical(st("duration_floor"), "fail")
  expect_identical(st("calibration_status"), "fail")
  expect_identical(check_line(ch, "calibration_status")$message, "Autocalibration not done (the file could not be read).")
  expect_identical(st("calibration_error"), "info")
  expect_identical(st("coefficient_sanity"), "info"); expect_identical(check_line(ch, "coefficient_sanity")$message, "Identity coefficients (no calibration applied).")
  expect_identical(st("dynamic_range"), "info")
  expect_true(all(ch$status[ch$check %in% c("gaps", "long_gaps", "clipping", "nonwear_score", "wear_time", "valid_days")] == "info"))
})

test_that("F2: a too-short gt3x gives file.too.short TRUE with the ' ' placeholder for cal.error.end; skip_small_files gives a skipped row", {
  skip_if_no_ggir_ref(); skip_if_no_file(SHORT())
  info <- raw.inspect(SHORT(), desiredtz = MOS2_TZ)
  expect_identical(info$sf, 100); expect_true(info$too_small)
  cal <- raw.calibrate(info)
  expect_identical(cal$qcmessage, MSG_NOT_ENOUGH)
  meta <- raw.getmeta(info, cal)
  expect_true(meta$filetooshort)
  r <- raw.quality.report(info, cal, meta)
  expect_false(r$file.corrupt); expect_true(r$file.too.short); expect_false(r$use.temperature)
  expect_identical(r$cal.error.start, 0); expect_identical(r$cal.error.end, " ")
  expect_type(r$cal.error.end, "character")
  expect_identical(r$n.10sec.windows, 0); expect_identical(r$n.hours.considered, 0)
  expect_identical(r$QCmessage, MSG_NOT_ENOUGH)
  expect_identical(unlist(r[, c("scale.x", "scale.y", "scale.z")], use.names = FALSE), c(1, 1, 1))
  expect_true(is.na(r$filehealth_totimp_min))
  expect_identical(r$samplefreq, 100); expect_true(is.na(r$wear_dur_def_proto_day))
  expect_identical(r$header_timezone, "-04:00:00")
  expect_true(is.na(r$seconds_trimmed_start)); expect_true(is.na(r$samples_discarded_end))
  expect_true(is.na(r$zero_triplets_removed))                 # the calibration pass read nothing usable
  expect_error(raw.quality.report(info, cal, NULL), "meta is required")
  ch <- raw.quality.checks(info, cal, meta)
  expect_identical(nrow(ch), 25L)
  st <- function(name) check_line(ch, name)$status
  expect_identical(st("file_size"), "warn"); expect_identical(st("header"), "ok"); expect_identical(st("sample_rate"), "ok")
  expect_identical(check_line(ch, "sample_rate")$message, "100 Hz from the header")
  expect_identical(st("duration_floor"), "fail")
  expect_identical(check_line(ch, "duration_floor")$message, "Fewer than 2 h of recorded data; nothing was analysed.")
  expect_identical(st("calibration_status"), "fail")
  expect_identical(st("start_trim"), "info"); expect_identical(check_line(ch, "start_trim")$message, "No epoch tables.")
  expect_identical(st("dynamic_range"), "warn")
  expect_identical(check_line(ch, "dynamic_range")$message, "TAS prefix: 8 g assumed, set dynrange if the device differs.")
  expect_identical(st("timezone"), "warn")
  expect_identical(check_line(ch, "timezone")$message,
                   "Device configured at -04:00; timestamps labelled America/Anchorage (-08:00). Set desiredtz/configtz if this is wrong.")
  # skipped at inspection
  sk <- raw.inspect(SHORT(), desiredtz = MOS2_TZ, skip_small_files = TRUE)
  expect_true(sk$skipped)
  rs <- raw.quality.report(sk, raw.calibrate(sk), raw.getmeta(sk, NULL))
  expect_identical(names(rs), c(QC_NAMES, P2_NAMES, EXTRA_NAMES))   # the .gt3x extension picks the two gap columns
  expect_false(rs$file.corrupt); expect_true(rs$file.too.short)
  expect_identical(rs$QCmessage, "Autocalibration not done"); expect_identical(rs$device.serial.number, "not extracted")
  expect_true(is.na(rs$filehealth_totimp_N))
  expect_true(is.na(rs$device)); expect_true(rs$too_small)
  expect_identical(raw.quality.report(sk, NULL, NULL)[, QC_NAMES], rs[, QC_NAMES])
  cs <- raw.quality.checks(sk, raw.calibrate(sk), raw.getmeta(sk, NULL))
  expect_identical(check_line(cs, "file_size")$status, "fail")
  expect_identical(check_line(cs, "file_size")$message, "This file is below GGIR's 2 MB floor; GGIR would skip it. It was skipped.")
  expect_identical(check_line(cs, "format")$status, "fail")
})

test_that(".raw.quality.ggir.qc: GGIR's placeholders, overrides and the serial branches", {
  I <- list(header = data.frame(value = c("X1", ""), row.names = c("Serial Number", "Firmware")),
            monc = 3L, monn = "actigraph", dformc = 6L, dformn = "gt3x", sf = 30, decn = ".", filename = "a.gt3x")
  M <- list(filecorrupt = FALSE, filetooshort = FALSE, NFilePagesSkipped = 0,
            metalong = data.frame(timestamp = c("t1", "t2", "t3"), nonwearscore = 0, clippingscore = 0, EN = 1),
            QClog = NULL)
  # no still windows: start, end and npoints are NULL, so " " placeholders
  C <- list(scale = c(1, 1, 1), offset = c(0, 0, 0), tempoffset = c(), cal.error.start = c(), cal.error.end = c(),
            spheredata = c(), npoints = c(), nhoursused = 2, QCmessage = "recalibration not done because no non-movement data available",
            use.temp = FALSE, meantempcal = c())
  q <- .raw.quality.ggir.qc(I, C, M, "a.gt3x")
  expect_identical(names(q), QC_NAMES[1:21])
  expect_identical(q$cal.error.start, " "); expect_identical(q$cal.error.end, " "); expect_identical(q$n.10sec.windows, " ")
  expect_identical(unlist(q[, c("temperature.offset.x", "temperature.offset.y", "temperature.offset.z")], use.names = FALSE), c(0, 0, 0))
  expect_identical(q$mean.temp, ""); expect_identical(q$device.serial.number, "not extracted")
  # too short: start and npoints forced to 0 even when a value exists; end " " when NULL
  M2 <- M; M2$filetooshort <- TRUE
  C2 <- C; C2$cal.error.start <- 0.02; C2$npoints <- 5L
  q2 <- .raw.quality.ggir.qc(I, C2, M2, "a.gt3x")
  expect_identical(q2$cal.error.start, 0); expect_identical(q2$n.10sec.windows, 0); expect_identical(q2$cal.error.end, " ")
  expect_true(q2$file.too.short)
  # g.part2's corrupt rule: sf NULL makes the file corrupt; a skipped file is left alone
  I0 <- I; I0$sf <- NULL
  expect_true(.raw.quality.ggir.qc(I0, C, M, "a.gt3x")$file.corrupt)
  expect_false(.raw.quality.ggir.qc(I0, C, M, "a.gt3x", skipped = TRUE)$file.corrupt)
  # temperature mean over all but the last long epoch, as.character
  M3 <- M; M3$metalong$temperaturemean <- c(20, 22, 99)
  expect_identical(.raw.quality.ggir.qc(I, C, M3, "a.gt3x")$mean.temp, "21")
  # GENEActiv serial from the header, "" replaced by "not stored in header"
  Ig <- list(header = data.frame(value = c("012345", ""), row.names = c("Device_Unique_Serial_Code", "Subject_Code")),
             monc = 2L, monn = "geneactive", dformc = 1L, dformn = "bin", sf = 100, decn = ".", filename = "g.bin")
  expect_identical(.raw.quality.ggir.qc(Ig, C, M, "g.bin")$device.serial.number, "012345")
  Ig2 <- Ig; Ig2$header <- data.frame(value = c("", "S9"), row.names = c("Subject_Code", "serial_number"))
  expect_identical(.raw.quality.ggir.qc(Ig2, C, M, "g.bin")$device.serial.number, "S9")
  Ia <- list(header = data.frame(value = c("30", "SN7"), row.names = c("sample_rate", "device_serial_number")),
             monc = 0L, monn = "unknown", dformc = 5L, dformn = "csv", sf = 30, decn = ".", filename = "a.csv")
  expect_identical(.raw.quality.ggir.qc(Ia, C, M, "a.csv")$device.serial.number, "SN7")
  Ia2 <- Ia; Ia2$header <- "no header"
  expect_identical(.raw.quality.ggir.qc(Ia2, C, M, "a.csv")$device.serial.number, "not extracted")
  M4 <- M; M4$NFilePagesSkipped <- NULL
  expect_identical(.raw.quality.ggir.qc(I, C, M4, "a.gt3x")$NFilePagesSkipped, 0)
})

test_that(".raw.quality.file.summary and .raw.quality.filehealth: log shapes and the empty columns per brand", {
  # the g.imputeTimegaps log: sums of timegaps_min and timegaps_n
  ql <- data.frame(imputed = c(TRUE, TRUE), start = c(1, 2), end = c(3, 4), blockLengthSeconds = c(2, 2),
                   timegaps_n = c(915L, 525L), timegaps_min = c(1358.54166666667, 6093.10833333333))
  fs <- .raw.quality.file.summary(ql, "f")
  expect_identical(names(fs), c("fname", "Dur_imputed", "Nblocks_imputed"))
  expect_identical(fs$Nblocks_imputed, 1440L)
  fh <- .raw.quality.filehealth(fs)
  expect_identical(names(fh), c("filehealth_totimp_min", "filehealth_totimp_N"))
  expect_identical(fh$filehealth_totimp_N, 1440)
  expect_identical(fh$filehealth_totimp_min, as.numeric(as.character(sum(ql$timegaps_min))))
  expect_null(.raw.quality.filehealth(.raw.quality.file.summary(NULL, "f")))
  expect_null(.raw.quality.filehealth(.raw.quality.file.summary(data.frame(start = 1, end = 2), "f")))
  # the empty columns for the brands
  expect_identical(names(.raw.quality.filehealth.empty(.RAW_MONITOR[["ACTIGRAPH"]], .RAW_FORMAT[["GT3X"]])),
                   c("filehealth_totimp_min", "filehealth_totimp_N"))
  ec <- .raw.quality.filehealth.empty(.RAW_MONITOR[["AXIVITY"]], .RAW_FORMAT[["CWA"]])
  expect_identical(names(ec), CWA_NAMES)
  expect_true(all(vapply(ec, is.double, logical(1)))); expect_true(all(is.na(unlist(ec))))
  expect_null(.raw.quality.filehealth.empty(.RAW_MONITOR[["GENEACTIV"]], .RAW_FORMAT[["BIN"]]))
  # skipped at inspection: the extension decides
  expect_null(.raw.quality.filehealth.empty(NULL, NULL))
  expect_identical(names(.raw.quality.filehealth.empty(NULL, NULL, "GT3X")), c("filehealth_totimp_min", "filehealth_totimp_N"))
  expect_identical(names(.raw.quality.filehealth.empty(NULL, NULL, "cwa")), CWA_NAMES)
  expect_null(.raw.quality.filehealth.empty(NULL, NULL, "bin"))
  # char2num: numbers convert, a blank placeholder converts silently to NA, text stays
  cn <- .raw.quality.char2num(data.frame(a = "7451.65", b = " ", c = "x", stringsAsFactors = FALSE))
  expect_identical(cn$a, 7451.65); expect_identical(cn$b, NA_real_); expect_identical(cn$c, "x")
})

test_that(".raw.quality.long.gaps: the run signature of the epoch-level imputation", {
  mk <- function(runs) {
    # runs: list of c(length, ENMO, anglez)
    d <- do.call(rbind, lapply(runs, function(r) data.frame(ENMO = rep(r[2], r[1]), anglez = rep(r[3], r[1]))))
    d$timestamp <- sprintf("t%06d", seq_len(nrow(d)))
    d[, c("timestamp", "ENMO", "anglez")]
  }
  lim <- max(6 * 900 / 60, 90) * 60 / 5
  expect_identical(lim, 1080)
  # a run one epoch longer than the limit counts, one at the limit does not
  ms <- mk(list(c(100, 0.05, 10), c(1081, 0, 20), c(50, 0.02, 30), c(1080, 0, 40), c(10, 0.01, 50)))
  lg <- .raw.quality.long.gaps(ms, 5, 900)
  expect_identical(lg$count, 1L); expect_identical(lg$run_lengths, 1081L); expect_identical(lg$epochs_added, 1080)
  expect_identical(lg$limit_epochs, 1080)
  # a long run with a non-zero metric is real data, not a gap
  ms2 <- mk(list(c(2000, 0.0031, 20)))
  expect_identical(.raw.quality.long.gaps(ms2, 5, 900)$count, 0L)
  # EN must be 1 in the replicas
  ms3 <- mk(list(c(1200, 0, 20))); ms3$EN <- 1
  expect_identical(.raw.quality.long.gaps(ms3, 5, 900)$count, 1L)
  ms3$EN <- 0.99
  expect_identical(.raw.quality.long.gaps(ms3, 5, 900)$count, 0L)
  # two separate long gaps; a canhrActi time column is ignored
  ms4 <- mk(list(c(1261, 0, 1), c(5, 0.1, 2), c(5041, 0, 3)))
  ms4$time <- seq_len(nrow(ms4))
  lg4 <- .raw.quality.long.gaps(ms4, 5, 900)
  expect_identical(lg4$count, 2L); expect_identical(lg4$run_lengths, c(1261L, 5041L)); expect_identical(lg4$epochs_added, 6300)
  expect_identical(.raw.quality.long.gaps(NULL)$count, 0L)
  expect_identical(.raw.quality.long.gaps(ms4[0, ])$count, 0L)
})

test_that(".raw.quality.long.gaps on MOS2 reproduces the fifteen remaining_epochs values of the gap imputation", {
  skip_if_no_ggir_ref()
  s <- stored("mos2")
  lg <- .raw.quality.long.gaps(s$M$metashort, 5, 900)
  expect_identical(lg$count, 15L)
  expect_identical(sort(lg$run_lengths), sort(c(1261L, 5041L, 1981L, 3781L, 1261L, 1441L, 7021L, 9001L, 5581L, 6841L, 1621L, 1081L, 8461L, 1261L, 2161L)))
  expect_identical(lg$epochs_added, 57780)
})

test_that("small helpers: offsets, header times, formatting", {
  expect_identical(.raw.quality.offset("-04:00:00"), "-04:00")
  expect_identical(.raw.quality.offset("03:00:00"), "+03:00")
  expect_identical(.raw.quality.offset("-0800"), "-08:00")
  expect_identical(.raw.quality.offset("+0530"), "+05:30")
  expect_true(is.na(.raw.quality.offset(NA))); expect_true(is.na(.raw.quality.offset("")))
  expect_identical(.raw.quality.offset.hours("-08:00"), -8); expect_identical(.raw.quality.offset.hours("+05:30"), 5.5)
  I <- list(header = data.frame(value = "2025-10-07 20:28:00", row.names = "Start Date"))
  expect_identical(.raw.quality.header.time(list(), I, "Start Date"), as.numeric(as.POSIXct("2025-10-07 20:28:00", tz = "UTC")))
  info <- list(header_list = list(`Start Date` = as.POSIXct("2025-10-07 20:28:00", tz = "GMT")))
  expect_identical(.raw.quality.header.time(info, list(), "Start Date"), as.numeric(info$header_list[["Start Date"]]))
  expect_true(is.na(.raw.quality.header.time(list(), list(), "Start Date")))
  expect_identical(.raw.quality.fmt(6.875, 2), "6.88"); expect_identical(.raw.quality.fmt(7451.65, 2), "7451.65")
  expect_identical(.raw.quality.fmt(NULL), "NA"); expect_identical(.raw.quality.fmt(3, 3), "3")
  expect_identical(.raw.quality.brand("actigraph"), "ActiGraph"); expect_identical(.raw.quality.brand(NULL), "unknown")
})

test_that("input validation", {
  expect_error(raw.quality.report(list(a = 1)), "info must be")
  expect_error(raw.quality.report(42), "info must be")
  I <- list(header = data.frame(value = c("MOS2X", "1.9.2"), row.names = c("Serial Number", "Firmware")),
            monc = 3L, monn = "actigraph", dformc = 6L, dformn = "gt3x", sf = 30, decn = ".", filename = "a.gt3x")
  expect_error(raw.quality.report(I, calibration = 42, meta = NULL), "calibration must be")
  expect_error(raw.quality.report(I, NULL, NULL), "meta is required")
  expect_error(raw.quality.report(I, NULL, meta = list(x = 1)), "meta must be")
  M <- list(filecorrupt = TRUE, filetooshort = FALSE, NFilePagesSkipped = 0, metalong = c(), metashort = c(), QClog = NULL)
  expect_error(raw.quality.report(I, NULL, M, wear = 42), "wear must be")
  r <- raw.quality.report(I, NULL, M)
  expect_true(r$file.corrupt)
  ch <- raw.quality.checks(I, NULL, M)
  expect_identical(nrow(ch), 25L)
  expect_identical(check_line(ch, "dynamic_range")$message, "8 g from serial prefix MOS")
  # a GGIR I whose header is not GGIR's data.frame does not break the checks
  I2 <- I; I2$header <- "no header"
  ch2 <- raw.quality.checks(I2, NULL, M)
  expect_identical(nrow(ch2), 25L)
  expect_identical(check_line(ch2, "dynamic_range")$message, "8 g assumed, set dynrange if the device differs.")
})
