# Parity tests for R/raw_starttime.R against GGIR's get_starttime_weekday_truncdata
# and g.getstarttime. Reference data come from CANHRACTI_GGIR_REF; those tests and
# the live GGIR comparisons skip when it is unset or GGIR is not installed.

ref_dir <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")
has_ref <- nzchar(ref_dir) && dir.exists(ref_dir)
has_ggir <- requireNamespace("GGIR", quietly = TRUE)

skip_if_no_ref <- function() {
  if (!has_ref) skip("CANHRACTI_GGIR_REF is not set or the folder does not exist")
}
skip_if_no_ggir <- function() {
  if (!has_ggir) skip("GGIR is not installed; live parity comparison skipped")
}

TZ <- "America/Anchorage"
SF <- 30

# A synthetic first block, as the matrix g.getmeta holds at the truncation call.
make_block <- function(start, sf = SF, minutes = 40, tz = TZ) {
  t0 <- as.POSIXct(start, tz = tz)
  n <- sf * 60 * minutes
  set.seed(7)
  d <- data.frame(time = as.numeric(t0) + (0:(n - 1)) / sf,
                  x = round(stats::rnorm(n, 0, 0.02), 3),
                  y = round(stats::rnorm(n, 0, 0.02), 3),
                  z = round(1 + stats::rnorm(n, 0, 0.02), 3))
  as.matrix(d)
}

ggir_truncdata <- function(data, sf = SF, ws2 = 900, tz = TZ, monc = 3, dformat = 6,
                           datafile = "", configtz = NULL) {
  GGIR:::get_starttime_weekday_truncdata(monc = monc, dformat = dformat, data = data,
                                         header = NULL, desiredtz = tz, sf = sf,
                                         datafile = datafile, ws2 = ws2,
                                         configtz = configtz)
}

# The start time keeps the "time" name from data[1, "time"]; drop it for string comparisons.
fmt_start <- function(st) unname(format(as.POSIXct(st), "%Y-%m-%d %H:%M:%S"))

# One synthetic case: run the port, assert the quoted values, then identical() to GGIR.
check_case <- function(start, dropped, expected_start, wday, wdayname, sf = SF,
                       ws2 = 900, tz = TZ, minutes = 40) {
  d <- make_block(start, sf = sf, minutes = minutes, tz = tz)
  res <- .raw.starttime.truncate(data = d, mon = 3, dformat = 6, desiredtz = tz,
                                 configtz = NULL, ws2 = ws2, sf = sf)
  expect_equal(nrow(d) - nrow(res$data), dropped, info = start)
  expect_equal(fmt_start(res$starttime), expected_start, info = start)
  expect_equal(res$wday, wday, info = start)
  expect_equal(res$wdayname, wdayname, info = start)
  expect_s3_class(res$starttime, "POSIXlt")
  expect_equal(res$starttime$sec, 0, info = start)
  if (dropped > 1) {
    # after the shift the first retained sample sits exactly on the new start
    expect_equal(unname(res$data[1, "time"]), as.numeric(as.POSIXct(res$starttime)),
                 info = start)
  }
  if (has_ggir) {
    ref <- ggir_truncdata(d, sf = sf, ws2 = ws2, tz = tz)
    if (!identical(res, ref)) {
      # report the first difference before failing
      for (nm in names(ref)) {
        if (!identical(res[[nm]], ref[[nm]])) {
          if (nm == "data") {
            i <- which(res$data != ref$data, arr.ind = TRUE)[1, ]
            fail(sprintf("%s: data differs first at row %d col %d: %s vs %s", start,
                         i[1], i[2], res$data[i[1], i[2]], ref$data[i[1], i[2]]))
          } else {
            fail(sprintf("%s: field %s differs: %s vs %s", start, nm,
                         paste(format(unclass(res[[nm]])), collapse = ","),
                         paste(format(unclass(ref[[nm]])), collapse = ",")))
          }
          break
        }
      }
    }
    expect_true(identical(res, ref), info = paste("identical() to GGIR:", start))
  }
  invisible(res)
}

# SYNTHETIC STARTS

test_that("start on the hour: nothing dropped", {
  # secshift 60 -> 0; min 0 %% 15 == 0 -> minshift 15 -> 0; sampleshift 0
  check_case("2025-10-07 20:00:00", dropped = 0, expected_start = "2025-10-07 20:00:00",
             wday = 3, wdayname = "Tuesday")
})

test_that("start at hh:14:59: one second of samples dropped", {
  # secshift 1, min 14 + 1 = 15, 15 %% 15 == 0 -> minshift 0; sampleshift 30
  check_case("2025-10-07 20:14:59", dropped = 30, expected_start = "2025-10-07 20:15:00",
             wday = 3, wdayname = "Tuesday")
})

test_that("start at hh:59:30 crosses the hour", {
  # secshift 30, min 59 + 1 = 60, 60 %% 15 == 0 -> minshift 0; sampleshift 900;
  # starttime$min = 60 normalises to 21:00:00
  check_case("2025-10-07 20:59:30", dropped = 900, expected_start = "2025-10-07 21:00:00",
             wday = 3, wdayname = "Tuesday")
})

test_that("start at 23:59:30 crosses midnight and keeps the first sample's weekday", {
  res <- check_case("2025-10-07 23:59:30", dropped = 900,
                    expected_start = "2025-10-08 00:00:00",
                    wday = 3, wdayname = "Tuesday")
  # the aligned start is a Wednesday, the reported wday is still Tuesday
  expect_equal(as.POSIXlt(as.POSIXct(res$starttime))$wday + 1, 4)
})

test_that("MOS2-shaped start 20:28:04 drops 3480 samples at 30 Hz", {
  # secshift 56, min 28 + 1 = 29, minshift 15 - 14 = 1; sampleshift 1800 + 1680 = 3480
  check_case("2025-10-07 20:28:04", dropped = 3480, expected_start = "2025-10-07 20:30:00",
             wday = 3, wdayname = "Tuesday")
})

test_that("start on a whole minute off the grid: secshift 0, minshift only", {
  # secshift 60 -> 0 (no +1 minute); min 7 -> minshift 8; sampleshift 14400
  check_case("2025-10-07 20:07:00", dropped = 14400, expected_start = "2025-10-07 20:15:00",
             wday = 3, wdayname = "Tuesday")
})

test_that("fractional-second start is floored to whole samples", {
  # sec 59.5 -> secshift 0.5 -> 15 samples; min 14 + 1 = 15 -> minshift 0
  check_case("2025-10-07 20:14:59.5", dropped = 15, expected_start = "2025-10-07 20:15:00",
             wday = 3, wdayname = "Tuesday")
})

test_that("a sampleshift of exactly 1 drops nothing (GGIR's > 1 test)", {
  # at 1 Hz, hh:14:59 gives sampleshift 1: data untouched, starttime still moved
  d <- make_block("2025-10-07 20:14:59", sf = 1, minutes = 40)
  res <- .raw.starttime.truncate(data = d, mon = 3, dformat = 6, desiredtz = TZ,
                                 ws2 = 900, sf = 1)
  expect_equal(nrow(res$data), nrow(d))
  expect_equal(fmt_start(res$starttime), "2025-10-07 20:15:00")
  if (has_ggir) expect_true(identical(res, ggir_truncdata(d, sf = 1)))
})

test_that("ws2 of 1800 aligns to the half hour", {
  # start_meas 30: min 15 -> minshift 15; sampleshift 15*60*30 + 30 = 27030
  check_case("2025-10-07 20:14:59", dropped = 27030, expected_start = "2025-10-07 20:30:00",
             wday = 3, wdayname = "Tuesday", ws2 = 1800, minutes = 60)
})

test_that("start on a Sunday gives wday 1 and a Saturday wday 7", {
  check_case("2025-10-05 08:00:00", dropped = 0, expected_start = "2025-10-05 08:00:00",
             wday = 1, wdayname = "Sunday")
  check_case("2025-10-11 08:00:00", dropped = 0, expected_start = "2025-10-11 08:00:00",
             wday = 7, wdayname = "Saturday")
})

test_that("100 Hz start 09:15:00 drops nothing (EE pattern)", {
  check_case("2017-05-30 09:15:00", dropped = 0, expected_start = "2017-05-30 09:15:00",
             wday = 3, wdayname = "Tuesday", sf = 100, tz = "Europe/Helsinki", minutes = 20)
})

test_that("a shift across a DST change is identical to GGIR", {
  skip_if_no_ggir()
  # America/Anchorage leaves DST on 2025-11-02 at 02:00 (clocks back to 01:00)
  d <- make_block("2025-11-02 00:59:30", minutes = 40)
  res <- .raw.starttime.truncate(data = d, mon = 3, dformat = 6, desiredtz = TZ,
                                 ws2 = 900, sf = SF)
  ref <- ggir_truncdata(d)
  expect_true(identical(res, ref))
  expect_equal(nrow(d) - nrow(res$data), 900)
  expect_equal(res$wday, 1)
  expect_equal(res$wdayname, "Sunday")
})

test_that("desiredtz '' uses the machine zone and matches GGIR", {
  skip_if_no_ggir()
  d <- make_block("2025-10-07 20:28:04", tz = "")
  res <- .raw.starttime.truncate(data = d, mon = 3, dformat = 6, desiredtz = "",
                                 ws2 = 900, sf = SF)
  ref <- ggir_truncdata(d, tz = "")
  expect_true(identical(res, ref))
  expect_equal(nrow(d) - nrow(res$data), 3480)
})

# ARGUMENT PLUMBING

test_that("info and params objects fill the explicit arguments", {
  d <- make_block("2025-10-07 20:28:04")
  explicit <- .raw.starttime.truncate(data = d, mon = 3, dformat = 6, desiredtz = TZ,
                                      configtz = NULL, ws2 = 900, sf = SF)
  info <- list(monc = 3, dformc = 6, sf = SF, path = "", header = NULL)
  params <- list(desiredtz = TZ, configtz = NULL, windowsizes = c(5, 900, 3600),
                 ggir_exact = TRUE)
  via_objects <- .raw.starttime.truncate(data = d, info = info, params = params)
  expect_true(identical(explicit, via_objects))
  # explicit arguments win over the objects
  override <- .raw.starttime.truncate(data = d, info = info, params = params, ws2 = 1800)
  expect_equal(fmt_start(override$starttime), "2025-10-07 20:30:00")
  expect_equal(nrow(d) - nrow(override$data), 3480)
})

test_that("monitor and format codes may be given by GGIR name", {
  d <- make_block("2025-10-07 20:28:04")
  by_code <- .raw.starttime.truncate(data = d, mon = 3, dformat = 6, desiredtz = TZ,
                                     ws2 = 900, sf = SF)
  by_name <- .raw.starttime.truncate(data = d, mon = "ACTIGRAPH", dformat = "gt3x",
                                     desiredtz = TZ, ws2 = 900, sf = SF)
  expect_true(identical(by_code, by_name))
  expect_error(.raw.starttime.truncate(data = d, mon = "NOPE", dformat = 6,
                                       desiredtz = TZ, ws2 = 900, sf = SF),
               "Unknown mon name")
  expect_error(.raw.starttime.truncate(data = d, dformat = 6, desiredtz = TZ,
                                       ws2 = 900, sf = SF),
               "mon and dformat must be supplied")
  expect_error(.raw.starttime.truncate(data = d, mon = 3, dformat = 6, desiredtz = TZ,
                                       ws2 = 900, sf = NULL),
               "sf must be a single positive number")
})

test_that("a format without a time column that is not csv or Movisens errors like GGIR", {
  d <- make_block("2025-10-07 20:00:00")[, c("x", "y", "z")]
  msg <- "Timestamps not found for monitor type 2 and file format type 1"
  expect_error(.raw.getstarttime(data = d, mon = 2, dformat = 1, desiredtz = TZ), msg,
               fixed = TRUE)
  if (has_ggir) {
    expect_error(GGIR:::g.getstarttime(datafile = "", data = d, mon = 2, dformat = 1,
                                       desiredtz = TZ), msg, fixed = TRUE)
  }
})

# ACTIGRAPH CSV HEADER

# A synthetic ActiGraph RAW csv with the start time, start date and date-format token as parameters.
write_ag_csv <- function(path, start_time = "10:00:00", start_date = "3/1/2024",
                         dateformat = "M/d/yyyy", drop_start_time = FALSE,
                         n = 30 * 60 * 3) {
  hdr <- c(paste0("------------ Data File Created By ActiGraph GT3X+ ActiLife v6.13.4 ",
                  "Firmware v3.2.1 date format ", dateformat,
                  " at 30 Hz  Filter Normal -----------"),
           "Serial Number: NEO1F16120034",
           paste0("Start Time ", start_time),
           paste0("Start Date ", start_date),
           "Epoch Period (hh:mm:ss) 00:00:00",
           "Download Time 12:00:00",
           "Download Date 3/1/2024",
           "Current Memory Address: 0",
           "Current Battery Voltage: 4.12     Mode = 12",
           "--------------------------------------------------")
  if (drop_start_time) hdr <- hdr[-3]
  set.seed(2)
  ag <- data.frame(x = round(stats::rnorm(n, 0, 0.02), 3),
                   y = round(stats::rnorm(n, 0, 0.02), 3),
                   z = round(1 + stats::rnorm(n, 0, 0.02), 3))
  con <- file(path, "w")
  writeLines(hdr, con)
  close(con)
  suppressWarnings(utils::write.table(
    data.frame(`Accelerometer X` = ag$x, `Accelerometer Y` = ag$y,
               `Accelerometer Z` = ag$z, check.names = FALSE),
    path, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE))
  as.matrix(ag)
}

test_that("ActiGraph csv header: M/d/yyyy start parsed like GGIR (study verify/04 case)", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  ag <- write_ag_csv(f)
  st <- .raw.getstarttime(data = data.frame(x = 1), mon = 3, dformat = 2,
                          desiredtz = "Europe/London", configtz = "Europe/London",
                          datafile = f)
  expect_s3_class(st, "POSIXlt")
  expect_equal(format(st, "%Y-%m-%d %H:%M:%S %Z"), "2024-03-01 10:00:00 GMT")
  expect_equal(as.numeric(as.POSIXct(st)), 1709287200)
  if (has_ggir) {
    ref <- GGIR:::g.getstarttime(datafile = f, data = data.frame(x = 1), mon = 3,
                                 dformat = 2, desiredtz = "Europe/London",
                                 configtz = "Europe/London")
    expect_true(identical(st, ref))
  }
  # through the truncation: 10:00:00 is on the grid, nothing dropped
  res <- .raw.starttime.truncate(data = ag, mon = 3, dformat = 2,
                                 desiredtz = "Europe/London", configtz = NULL,
                                 ws2 = 900, sf = 30, datafile = f)
  expect_equal(nrow(res$data), nrow(ag))
  expect_equal(fmt_start(res$starttime), "2024-03-01 10:00:00")
  expect_equal(res$wday, 6)
  expect_equal(res$wdayname, "Friday")
  if (has_ggir) {
    ref <- ggir_truncdata(ag, sf = 30, tz = "Europe/London", monc = 3, dformat = 2,
                          datafile = f)
    expect_true(identical(res, ref))
  }
})

test_that("ActiGraph csv header: off-grid start 10:07:30 drops 13500 samples", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  ag <- write_ag_csv(f, start_time = "10:07:30", n = 30 * 60 * 20)
  # secshift 30, min 7 + 1 = 8, minshift 7; sampleshift 7*60*30 + 30*30 = 13500
  res <- .raw.starttime.truncate(data = ag, mon = 3, dformat = 2,
                                 desiredtz = "Europe/London", ws2 = 900, sf = 30,
                                 datafile = f)
  expect_equal(nrow(ag) - nrow(res$data), 13500)
  expect_equal(fmt_start(res$starttime), "2024-03-01 10:15:00")
  if (has_ggir) {
    ref <- ggir_truncdata(ag, sf = 30, tz = "Europe/London", monc = 3, dformat = 2,
                          datafile = f)
    expect_true(identical(res, ref))
  }
})

test_that("ActiGraph csv header: configtz differs from desiredtz", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  write_ag_csv(f)
  # device configured in Berlin, results wanted in London: 10:00 CET = 09:00 GMT
  st <- .raw.getstarttime(data = data.frame(x = 1), mon = 3, dformat = 2,
                          desiredtz = "Europe/London", configtz = "Europe/Berlin",
                          datafile = f)
  expect_equal(format(st, "%Y-%m-%d %H:%M:%S %Z"), "2024-03-01 09:00:00 GMT")
  if (has_ggir) {
    ref <- GGIR:::g.getstarttime(datafile = f, data = data.frame(x = 1), mon = 3,
                                 dformat = 2, desiredtz = "Europe/London",
                                 configtz = "Europe/Berlin")
    expect_true(identical(st, ref))
  }
})

test_that("ActiGraph csv header: d/M/yyyy and Verisense monitor code", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  write_ag_csv(f, start_date = "1/3/2024", dateformat = "d/M/yyyy")
  st <- .raw.getstarttime(data = data.frame(x = 1), mon = 6, dformat = 2,
                          desiredtz = "Europe/London", datafile = f)
  expect_equal(as.numeric(as.POSIXct(st)), 1709287200)
  if (has_ggir) {
    ref <- GGIR:::g.getstarttime(datafile = f, data = data.frame(x = 1), mon = 6,
                                 dformat = 2, desiredtz = "Europe/London")
    expect_true(identical(st, ref))
  }
})

test_that("ActiGraph csv header: month-name date depends on LC_TIME, reproduced under ggir_exact", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  write_ag_csv(f, start_date = "01-Mar-2024", dateformat = "dd-MMM-yyyy")
  # in the C locale the parse works either way
  st <- withr::with_locale(c(LC_TIME = "C"),
                           .raw.getstarttime(data = data.frame(x = 1), mon = 3, dformat = 2,
                                             desiredtz = "Europe/London", datafile = f, ggir_exact = TRUE))
  expect_equal(as.numeric(as.POSIXct(st)), 1709287200)
  if (has_ggir) {
    ref <- withr::with_locale(c(LC_TIME = "C"),
                              GGIR:::g.getstarttime(datafile = f, data = data.frame(x = 1), mon = 3,
                                                    dformat = 2, desiredtz = "Europe/London"))
    expect_true(identical(st, ref))
  }
  # now in a locale whose month abbreviation for March is not "Mar"
  local_german_time()
  r <- Sys.getlocale("LC_TIME")
  # this locale does not parse "Mar"
  expect_true(is.na(as.POSIXct("01-Mar-2024 10:00:00", format = "%d-%b-%Y %H:%M:%S",
                               tz = "UTC")))
  exact <- .raw.getstarttime(data = data.frame(x = 1), mon = 3, dformat = 2,
                             desiredtz = "Europe/London", datafile = f, ggir_exact = TRUE)
  expect_equal(as.numeric(as.POSIXct(exact)), 1709287200)
  # the locale is restored after the call
  expect_equal(Sys.getlocale("LC_TIME"), r)
  loose <- .raw.getstarttime(data = data.frame(x = 1), mon = 3, dformat = 2,
                             desiredtz = "Europe/London", datafile = f, ggir_exact = FALSE)
  expect_true(is.na(as.POSIXct(loose)))
  if (has_ggir) {
    # GGIR's function without the session-level LC_TIME "C" fails the same way
    ref <- GGIR:::g.getstarttime(datafile = f, data = data.frame(x = 1), mon = 3,
                                 dformat = 2, desiredtz = "Europe/London")
    expect_true(is.na(as.POSIXct(ref)))
    expect_true(identical(loose, ref))
  }
})

test_that("ActiGraph csv header without Start Time errors with GGIR's text", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  write_ag_csv(f, drop_start_time = TRUE)
  msg <- paste0("Start Time not found in the header of ", f)
  expect_error(.raw.getstarttime(data = data.frame(x = 1), mon = 3, dformat = 2,
                                 desiredtz = "Europe/London", datafile = f), msg,
               fixed = TRUE)
  if (has_ggir) {
    expect_error(GGIR:::g.getstarttime(datafile = f, data = data.frame(x = 1), mon = 3,
                                       dformat = 2, desiredtz = "Europe/London"),
                 msg, fixed = TRUE)
  }
})

# MOVISENS

test_that("Movisens start time from unisens.xml is forced into configtz like GGIR", {
  skip_if_not_installed("unisensR")
  dir <- tempfile("unisens")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>',
               '<unisens xmlns="http://www.unisens.org/unisens2.0" timestampStart="2024-03-01T10:00:00" duration="600" version="2.0"/>'),
             file.path(dir, "unisens.xml"))
  datafile <- file.path(dir, "acc.bin")
  d <- matrix(0, 10, 3, dimnames = list(NULL, c("x", "y", "z")))
  # configured in Berlin (10:00 CET) reported in London (09:00 GMT)
  st <- .raw.getstarttime(data = d, mon = 5, dformat = 1, desiredtz = "Europe/London",
                          configtz = "Europe/Berlin", datafile = datafile)
  expect_s3_class(st, "POSIXlt")
  expect_equal(format(st, "%Y-%m-%d %H:%M:%S %Z"), "2024-03-01 09:00:00 GMT")
  if (has_ggir) {
    ref <- GGIR:::g.getstarttime(datafile = datafile, data = d, mon = 5, dformat = 1,
                                 desiredtz = "Europe/London", configtz = "Europe/Berlin")
    expect_true(identical(st, ref))
  }
  # configtz NULL falls back to desiredtz; "" keeps the system-zone parse
  st2 <- .raw.getstarttime(data = d, mon = 5, dformat = 1, desiredtz = "Europe/Berlin",
                           datafile = datafile)
  expect_equal(format(st2, "%Y-%m-%d %H:%M:%S"), "2024-03-01 10:00:00")
  if (has_ggir) {
    ref2 <- GGIR:::g.getstarttime(datafile = datafile, data = d, mon = 5, dformat = 1,
                                  desiredtz = "Europe/Berlin")
    expect_true(identical(st2, ref2))
  }
})

# MOS2 REFERENCE RECORDING

test_that("MOS2 block 1 after imputation: 3480 rows dropped, start 20:30:00, wday 3 Tuesday", {
  skip_if_no_ref()
  skip_if_no_ggir()
  mos2 <- file.path(ref_dir, "din", "MOS2E39230594.gt3x")
  rdata <- file.path(ref_dir, "out", "output_din", "meta", "basic",
                     "meta_MOS2E39230594.gt3x.RData")
  if (!file.exists(mos2)) skip(paste("missing", mos2))
  if (!file.exists(rdata)) skip(paste("missing", rdata))

  P <- GGIR::load_params()
  params_general <- P$params_general
  params_rawdata <- P$params_rawdata
  params_general[["desiredtz"]] <- TZ
  I <- GGIR::g.inspectfile(mos2, desiredtz = TZ, params_rawdata = params_rawdata,
                           configtz = c())
  expect_equal(I$sf, 30)
  fq <- data.frame(filetooshort = FALSE, filecorrupt = FALSE, filedoesnotholdday = FALSE,
                   NFilePagesSkipped = 0)
  blk <- GGIR::g.readaccfile(filename = mos2, blocksize = 86400, blocknumber = 1,
                             filequality = fq, ws = 3600, PreviousEndPage = c(),
                             inspectfileobject = I, PreviousLastValue = c(0, 0, 1),
                             PreviousLastTime = NULL, params_rawdata = params_rawdata,
                             params_general = params_general, header = NULL)
  expect_equal(nrow(blk$P$data), 2592000)
  imp <- GGIR:::g.imputeTimegaps(blk$P$data, sf = 30, k = 0.25,
                                 PreviousLastValue = c(0, 0, 1), PreviousLastTime = NULL,
                                 epochsize = c(5, 900))
  data <- as.matrix(imp$x)
  expect_equal(nrow(data), 5036460)
  expect_equal(unname(format(as.POSIXct(data[1, "time"], tz = TZ, origin = "1970-01-01"),
                             "%Y-%m-%d %H:%M:%OS3")), "2025-10-07 20:28:04.000")

  res <- .raw.starttime.truncate(data = data, mon = I$monc, dformat = I$dformc,
                                 desiredtz = TZ, configtz = NULL, ws2 = 900, sf = I$sf,
                                 datafile = mos2)
  expect_equal(nrow(data) - nrow(res$data), 3480)
  expect_equal(nrow(res$data), 5032980)
  expect_equal(fmt_start(res$starttime), "2025-10-07 20:30:00")
  expect_equal(as.numeric(as.POSIXct(res$starttime)), 1759897800)
  expect_equal(res$wday, 3)
  expect_equal(res$wdayname, "Tuesday")
  expect_equal(unname(res$data[1, "time"]), 1759897800)
  expect_equal(unname(res$data[1, c("x", "y", "z")]), unname(data[3481, c("x", "y", "z")]))

  # identical() to the installed GGIR function on the same input
  ref <- ggir_truncdata(data, sf = I$sf, ws2 = 900, tz = TZ, monc = I$monc,
                        dformat = I$dformc, datafile = mos2)
  expect_true(identical(res$starttime, ref$starttime))
  expect_true(identical(res$wday, ref$wday))
  expect_true(identical(res$wdayname, ref$wdayname))
  expect_true(identical(res$data, ref$data))
  expect_true(identical(res, ref))

  # agrees with the stored milestone
  e <- new.env()
  load(rdata, envir = e)
  expect_equal(res$wday, e$M$wday)
  expect_equal(res$wdayname, e$M$wdayname)
  first_ts <- unname(strftime(as.POSIXlt(round(as.numeric(res$starttime)), tz = TZ,
                                         origin = "1970-01-01"),
                              format = "%Y-%m-%dT%H:%M:%S%z"))
  expect_equal(first_ts, "2025-10-07T20:30:00-0800")
  expect_equal(first_ts, e$M$metashort$timestamp[1])
})
