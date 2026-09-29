# Parity tests for R/raw_sleeplog.R against GGIR 3.3-9 g.loadlog and g.part4_extractid.
# The diaries come from GGIR:::create_test_sleeplog_csv, patched by hand for the edge
# cases; every assertion is identical() to the live GGIR function or an exact value.

ggir_available <- function() {
  isTRUE(requireNamespace("GGIR", quietly = TRUE)) &&
    isTRUE(exists("g.loadlog", envir = asNamespace("GGIR"), inherits = FALSE))
}

skip_without_ggir <- function() {
  testthat::skip_if_not(ggir_available(), "GGIR (with g.loadlog) is not installed")
}

# Collect warnings without stopping, so both sides' warnings compare as data.
with_warnings <- function(expr) {
  w <- character(0)
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) structure(conditionMessage(e), class = "sleeplog_error")),
    warning = function(cond) {
      w <<- c(w, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = w)
}

# Both diaries, generated into a temporary folder.
make_diaries <- function() {
  root <- tempfile("canhrActi_sleeplog_")
  dir.create(root, recursive = TRUE)
  bdir <- file.path(root, "basic")
  adir <- file.path(root, "advanced")
  dir.create(bdir)
  dir.create(adir)
  GGIR:::create_test_sleeplog_csv(Nnights = 7, storagelocation = bdir)
  GGIR:::create_test_sleeplog_csv(advanced = TRUE, begin_date = "2025/10/07",
                                  storagelocation = adir)
  list(root = root,
       basic = normalizePath(file.path(bdir, "testsleeplogfile.csv"), winslash = "/"),
       advanced = normalizePath(file.path(adir, "testsleeplogfile.csv"), winslash = "/"))
}

write_csv_lines <- function(root, name, lines) {
  path <- file.path(root, name)
  writeLines(lines, path)
  normalizePath(path, winslash = "/")
}

# A stand-in for GGIR's meta.sleep.folder: one RData file holding ID and rec_starttime.
make_ms3_folder <- function(root, name, ID, rec_starttime) {
  folder <- file.path(root, name)
  dir.create(folder, showWarnings = FALSE)
  save(ID, rec_starttime, file = file.path(folder, "one.RData"))
  folder
}

expect_same_logs <- function(ours, theirs, label) {
  expect_identical(names(ours), c("sleeplog", "nonwearlog", "naplog", "bedlog",
                                  "imputecodelog", "dateformat"),
                   info = label)
  for (member in names(theirs)) {
    expect_identical(ours[[member]], theirs[[member]],
                     info = paste(label, "member", member))
  }
}

# BASIC (WIDE) FORMAT

test_that("the basic diary reproduces g.loadlog exactly, SPT and TimeInBed", {
  skip_without_ggir()
  d <- make_diaries()

  theirs <- GGIR:::g.loadlog(loglocation = d$basic, coln1 = 2, colid = 1, sleepwindowType = "SPT")
  ours <- raw.sleeplog(path = d$basic, colid = 1, coln1 = 2, sleepwindowType = "SPT")
  expect_same_logs(ours, theirs, "basic SPT")

  # seven nights, night 2 is the day sleeper
  expect_equal(nrow(ours$sleeplog), 7L)
  expect_identical(ours$sleeplog$duration, c("8", "14", "8", "8", "8", "8", "8"))
  expect_identical(ours$sleeplog$sleeponset,
                   c("23:0:0", "3:0:0", "23:0:0", "23:0:0", "23:0:0", "23:0:0", "23:0:0"))
  expect_identical(ours$sleeplog$sleepwake,
                   c("7:0:0", "17:0:0", "7:0:0", "7:0:0", "7:0:0", "7:0:0", "7:0:0"))
  expect_identical(ours$sleeplog$night, as.character(1:7))
  expect_identical(unique(ours$sleeplog$ID), "123")
  expect_null(ours$bedlog)
  expect_identical(ours$dateformat, "%Y-%m-%d")

  # TimeInBed: the same table comes back as bedlog and sleeplog is NULL.
  theirs_tib <- GGIR:::g.loadlog(loglocation = d$basic, coln1 = 2, colid = 1,
                                 sleepwindowType = "TimeInBed")
  ours_tib <- raw.sleeplog(path = d$basic, colid = 1, coln1 = 2, sleepwindowType = "TimeInBed")
  expect_same_logs(ours_tib, theirs_tib, "basic TimeInBed")
  expect_null(ours_tib$sleeplog)
  expect_identical(names(ours_tib$bedlog), c("ID", "night", "duration", "bedstart", "bedend"))
  expect_identical(ours_tib$bedlog$duration, c("8", "14", "8", "8", "8", "8", "8"))
  expect_identical(ours_tib$bedlog$bedstart,
                   c("23:0:0", "3:0:0", "23:0:0", "23:0:0", "23:0:0", "23:0:0", "23:0:0"))
})

test_that("a mis-set coln1 shifts the pairing the way GGIR shifts it", {
  skip_without_ggir()
  d <- make_diaries()

  theirs <- with_warnings(GGIR:::g.loadlog(loglocation = d$basic, coln1 = 3, colid = 1,
                                           sleepwindowType = "SPT"))
  ours <- with_warnings(raw.sleeplog(path = d$basic, colid = 1, coln1 = 3, sleepwindowType = "SPT"))
  expect_same_logs(ours$value, theirs$value, "basic coln1 = 3")
  expect_identical(ours$warnings, theirs$warnings)
  expect_match(ours$warnings[1], "odd number of timestamp columns")
  # the pairing shifted by one column: 20, 6, 16, 16, ...
  expect_identical(ours$value$sleeplog$duration, c("20", "6", "16", "16", "16", "16"))
})

test_that("a blank night is dropped entirely, no placeholder row", {
  skip_without_ggir()
  d <- make_diaries()
  path <- write_csv_lines(
    d$root, "blank.csv",
    c('"ID","onset","wakeup","onset","wakeup","onset","wakeup","onset","wakeup"',
      paste0('"MOS2E39230594.gt3x","22:45:00","07:25:00","","",',
             '"23:30:00","07:10:00","21:50:00","07:30:00"')))

  theirs <- GGIR:::g.loadlog(loglocation = path, coln1 = 2, colid = 1, sleepwindowType = "SPT")
  ours <- raw.sleeplog(path = path, colid = 1, coln1 = 2, sleepwindowType = "SPT")
  expect_same_logs(ours, theirs, "blank night")

  # 3 rows, nights 1, 3 and 4
  expect_equal(nrow(ours$sleeplog), 3L)
  expect_identical(ours$sleeplog$night, c("1", "3", "4"))
  expect_identical(ours$sleeplog$duration,
                   c("8.66666666666667", "7.66666666666667", "9.66666666666667"))
  expect_identical(ours$sleeplog$sleeponset, c("22:45:0", "23:30:0", "21:50:0"))
  expect_identical(ours$sleeplog$sleepwake, c("7:25:0", "7:10:0", "7:30:0"))
  # leading zeros are stripped
  expect_false(any(grepl("^0", ours$sleeplog$sleepwake)))
})

test_that("a malformed time raises GGIR's error, character for character", {
  skip_without_ggir()
  d <- make_diaries()
  path <- write_csv_lines(d$root, "bad.csv",
                          c('"ID","onset","wakeup"', '"123","2x:00:00","07:00:00"'))

  theirs <- with_warnings(GGIR:::g.loadlog(loglocation = path, coln1 = 2, colid = 1,
                                           sleepwindowType = "SPT"))
  ours <- with_warnings(raw.sleeplog(path = path, colid = 1, coln1 = 2, sleepwindowType = "SPT"))
  expect_s3_class(ours$value, "sleeplog_error")
  expect_identical(unclass(ours$value), unclass(theirs$value))
  expect_identical(as.character(ours$value),
                   "NA as found in the sleeplog is not a valid timestamp")
  # "TimeInBed" puts the same table through as a bedlog, so the message names the bedlog.
  ours_tib <- with_warnings(raw.sleeplog(path = path, colid = 1, coln1 = 2,
                                         sleepwindowType = "TimeInBed"))
  expect_identical(as.character(ours_tib$value),
                   "NA as found in the bedlog is not a valid timestamp")
})

test_that("duplicated identifiers and an empty column header stop the way GGIR stops", {
  skip_without_ggir()
  d <- make_diaries()

  dup <- write_csv_lines(d$root, "dup.csv",
                         c('"ID","onset","wakeup"', '"123","23:00:00","07:00:00"',
                           '"123","22:00:00","06:00:00"'))
  theirs <- with_warnings(GGIR:::g.loadlog(loglocation = dup, coln1 = 2, colid = 1,
                                           sleepwindowType = "SPT"))
  ours <- with_warnings(raw.sleeplog(path = dup, colid = 1, coln1 = 2, sleepwindowType = "SPT"))
  expect_identical(unclass(ours$value), unclass(theirs$value))
  expect_identical(as.character(ours$value),
                   paste0("Sleeplog has duplicated entries (rows) for ID(s) 123, please fix. ",
                          "GGIR expects one sleeplog row per unique ID. "))

  hdr <- write_csv_lines(d$root, "emptyhdr.csv",
                         c(",onset,wakeup", "123,23:00:00,07:00:00"))
  theirs <- with_warnings(GGIR:::g.loadlog(loglocation = hdr, coln1 = 2, colid = 1,
                                           sleepwindowType = "SPT"))
  ours <- with_warnings(raw.sleeplog(path = hdr, colid = 1, coln1 = 2, sleepwindowType = "SPT"))
  expect_identical(unclass(ours$value), unclass(theirs$value))
  expect_identical(as.character(ours$value),
                   paste0("Sleeplog column found with empty header, please fix. This can also ",
                          "happen if there are empty columns at the end, delete those columns ",
                          "if applicable."))
})

test_that("a wake time equal to the onset carries the previous duration, as in GGIR", {
  skip_without_ggir()
  d <- make_diaries()
  path <- write_csv_lines(d$root, "equal.csv",
                          c('"ID","onset","wakeup","onset","wakeup"',
                            '"a","23:00:00","07:00:00","05:00:00","05:00:00"'))
  theirs <- GGIR:::g.loadlog(loglocation = path, coln1 = 2, colid = 1, sleepwindowType = "SPT")
  ours <- raw.sleeplog(path = path, colid = 1, coln1 = 2, sleepwindowType = "SPT")
  expect_same_logs(ours, theirs, "wake equals onset")
  expect_identical(ours$sleeplog$duration, c("8", "0.00222222222222222"))

  # on the very first pair there is nothing to carry, so both error
  first <- write_csv_lines(d$root, "equalfirst.csv",
                           c('"ID","onset","wakeup"', '"a","05:00:00","05:00:00"'))
  theirs_e <- with_warnings(GGIR:::g.loadlog(loglocation = first, coln1 = 2, colid = 1,
                                             sleepwindowType = "SPT"))
  ours_e <- with_warnings(raw.sleeplog(path = first, colid = 1, coln1 = 2, sleepwindowType = "SPT"))
  expect_identical(unclass(ours_e$value), unclass(theirs_e$value))
  expect_identical(as.character(ours_e$value), "object 'dur' not found")
})

test_that("a two-column diary warns instead of parsing", {
  skip_without_ggir()
  d <- make_diaries()
  path <- write_csv_lines(d$root, "two.csv", c('"ID","onset"', '"123","23:00:00"'))
  theirs <- with_warnings(GGIR:::g.loadlog(loglocation = path, coln1 = 2, colid = 1,
                                           sleepwindowType = "SPT"))
  ours <- with_warnings(raw.sleeplog(path = path, colid = 1, coln1 = 2, sleepwindowType = "SPT"))
  expect_identical(ours$warnings, theirs$warnings)
  expect_match(ours$warnings[1], "Does it have at least 3 columns")
  expect_same_logs(ours$value, theirs$value, "two columns")
})

# ADVANCED (DATE BASED) FORMAT

test_that("the advanced diary reproduces g.loadlog exactly, including naps and non-wear", {
  skip_without_ggir()
  d <- make_diaries()
  folder <- make_ms3_folder(d$root, "ms3", "123A", "2025-10-07T09:00:00-0800")

  theirs <- GGIR:::g.loadlog(loglocation = d$advanced, coln1 = 2, colid = 1,
                             meta.sleep.folder = folder, desiredtz = "America/Anchorage",
                             sleepwindowType = "SPT")
  ours <- raw.sleeplog(path = d$advanced, colid = 1, coln1 = 2, sleepwindowType = "SPT",
                       desiredtz = "America/Anchorage",
                       rec_starttime = "2025-10-07T09:00:00-0800", id = "123A")
  expect_same_logs(ours, theirs, "advanced SPT")

  # the wake time is taken from the next date, so night 1 is 23:00:01 to 07:00:02
  expect_equal(nrow(ours$sleeplog), 7L)
  expect_identical(ours$sleeplog$sleeponset[1], "23:0:1")
  expect_identical(ours$sleeplog$sleepwake[1], "7:0:2")
  expect_identical(ours$sleeplog$sleeponset[7], "23:0:7")
  expect_identical(ours$sleeplog$sleepwake[7], "7:0:8")
  expect_identical(unique(ours$sleeplog$duration), "8.00027777777778")
  expect_identical(unique(ours$sleeplog$ID), "123A")

  # two naps and one non-wear window on each of the eight days
  expect_equal(nrow(ours$naplog), 8L)
  expect_equal(nrow(ours$nonwearlog), 8L)
  expect_identical(colnames(ours$naplog), c("ID", "date", "nap1", "nap1", "nap2", "nap2"))
  expect_identical(colnames(ours$nonwearlog), c("ID", "date", "nonwear1", "nonwear1"))
  expect_identical(as.character(ours$naplog[1, ]),
                   c("123A", "2025-10-07", "13:00:01", "13:30:00", "15:00:01", "15:40:00"))
  expect_identical(as.character(ours$nonwearlog[1, ]),
                   c("123A", "2025-10-07", "17:00:01", "17:15:00"))
  expect_null(ours$bedlog)
  expect_null(ours$imputecodelog)
  expect_identical(ours$dateformat, "%Y-%m-%d")

  # sleepwindowType is ignored by the advanced parser: it decides from the column keywords.
  theirs_tib <- GGIR:::g.loadlog(loglocation = d$advanced, coln1 = 2, colid = 1,
                                 meta.sleep.folder = folder, desiredtz = "America/Anchorage",
                                 sleepwindowType = "TimeInBed")
  ours_tib <- raw.sleeplog(path = d$advanced, colid = 1, coln1 = 2, sleepwindowType = "TimeInBed",
                           desiredtz = "America/Anchorage",
                           rec_starttime = "2025-10-07T09:00:00-0800", id = "123A")
  expect_same_logs(ours_tib, theirs_tib, "advanced TimeInBed")
  expect_null(ours_tib$bedlog)
  expect_equal(nrow(ours_tib$sleeplog), 7L)
})

test_that("inbed and outbed keywords fill the bedlog instead of the sleeplog", {
  skip_without_ggir()
  d <- make_diaries()
  folder <- make_ms3_folder(d$root, "ms3bed", "123A", "2025-10-07T09:00:00-0800")
  path <- write_csv_lines(
    d$root, "bedkeywords.csv",
    c(paste0("ID,D1_date,D1_outbed,D1_inbed,D2_date,D2_outbed,D2_inbed,",
             "D3_date,D3_outbed,D3_inbed,D4_date,D4_outbed,D4_inbed"),
      paste0("123A,2025-10-07,07:00:00,23:00:00,2025-10-08,07:10:00,23:10:00,",
             "2025-10-09,07:20:00,23:20:00,2025-10-10,07:30:00,23:30:00")))

  theirs <- GGIR:::g.loadlog(loglocation = path, coln1 = 2, colid = 1,
                             meta.sleep.folder = folder, desiredtz = "America/Anchorage",
                             sleepwindowType = "TimeInBed")
  ours <- raw.sleeplog(path = path, colid = 1, coln1 = 2, sleepwindowType = "TimeInBed",
                       desiredtz = "America/Anchorage",
                       rec_starttime = "2025-10-07T09:00:00-0800", id = "123A")
  expect_same_logs(ours, theirs, "advanced bed keywords")
  expect_null(ours$sleeplog)
  expect_identical(names(ours$bedlog), c("ID", "night", "duration", "bedstart", "bedend"))
  expect_equal(nrow(ours$bedlog), 3L)
  expect_identical(ours$bedlog$bedstart, c("23:0:0", "23:10:0", "23:20:0"))
  expect_identical(ours$bedlog$bedend, c("7:10:0", "7:20:0", "7:30:0"))
  expect_identical(unique(ours$bedlog$duration), "8.16666666666667")
})

test_that("the date format is sniffed and reported", {
  skip_without_ggir()
  d <- make_diaries()
  folder <- make_ms3_folder(d$root, "ms3dmy", "123A", "2025-10-07T09:00:00-0800")
  lines <- readLines(d$advanced)
  lines <- gsub("(2025)-([0-9][0-9])-([0-9][0-9])", "\\3/\\2/\\1", lines)
  path <- write_csv_lines(d$root, "dmy.csv", lines)

  theirs <- GGIR:::g.loadlog(loglocation = path, coln1 = 2, colid = 1,
                             meta.sleep.folder = folder, desiredtz = "America/Anchorage",
                             sleepwindowType = "SPT")
  ours <- raw.sleeplog(path = path, colid = 1, coln1 = 2, sleepwindowType = "SPT",
                       desiredtz = "America/Anchorage",
                       rec_starttime = "2025-10-07T09:00:00-0800", id = "123A")
  expect_same_logs(ours, theirs, "advanced d/m/Y")
  expect_identical(ours$dateformat, "%d/%m/%Y")
  expect_equal(nrow(ours$sleeplog), 7L)
})

test_that("a recording that starts at or before 04:00 shifts the nights by one", {
  skip_without_ggir()
  d <- make_diaries()
  folder <- make_ms3_folder(d$root, "ms3mid", "123A", "2025-10-07T00:00:00-0800")

  theirs <- GGIR:::g.loadlog(loglocation = d$advanced, coln1 = 2, colid = 1,
                             meta.sleep.folder = folder, desiredtz = "America/Anchorage",
                             sleepwindowType = "SPT")
  ours <- raw.sleeplog(path = d$advanced, colid = 1, coln1 = 2, sleepwindowType = "SPT",
                       desiredtz = "America/Anchorage",
                       rec_starttime = "2025-10-07T00:00:00-0800", id = "123A")
  expect_same_logs(ours, theirs, "advanced startAtMidnight")
  # startAtMidnight adds a day to deltadate, so the nights are numbered 2 to 8
  expect_identical(ours$sleeplog$night, as.character(2:8))
  expect_identical(ours$sleeplog$sleeponset[1], "23:0:1")
})

test_that("an identifier that matches nothing yields no diary and GGIR's warning", {
  skip_without_ggir()
  d <- make_diaries()
  folder <- make_ms3_folder(d$root, "ms3none", "ZZZ", "2025-10-07T09:00:00-0800")

  theirs <- with_warnings(GGIR:::g.loadlog(loglocation = d$advanced, coln1 = 2, colid = 1,
                                           meta.sleep.folder = folder,
                                           desiredtz = "America/Anchorage",
                                           sleepwindowType = "SPT"))
  ours <- with_warnings(raw.sleeplog(path = d$advanced, colid = 1, coln1 = 2,
                                     sleepwindowType = "SPT",
                                     desiredtz = "America/Anchorage",
                                     rec_starttime = "2025-10-07T09:00:00-0800", id = "ZZZ"))
  expect_same_logs(ours$value, theirs$value, "advanced no matching ID")
  expect_identical(ours$warnings, theirs$warnings)
  expect_match(ours$warnings[1], "None of the IDs in the accelerometer data could be matched")
  expect_null(ours$value$sleeplog)
  expect_null(ours$value$bedlog)
  expect_equal(nrow(ours$value$naplog), 0L)
})

test_that("a gap in the diary dates and a duplicated diary date both stop", {
  skip_without_ggir()
  d <- make_diaries()
  folder <- make_ms3_folder(d$root, "ms3gap", "123A", "2025-10-07T09:00:00-0800")
  lines <- readLines(d$advanced)

  gap <- lines
  gap[2] <- sub("2025-10-09", "", gap[2])
  gap[2] <- sub("2025-10-11", "", gap[2])
  gap_path <- write_csv_lines(d$root, "gap.csv", gap)
  theirs <- with_warnings(GGIR:::g.loadlog(loglocation = gap_path, coln1 = 2, colid = 1,
                                           meta.sleep.folder = folder,
                                           desiredtz = "America/Anchorage",
                                           sleepwindowType = "SPT"))
  ours <- with_warnings(raw.sleeplog(path = gap_path, colid = 1, coln1 = 2,
                                     sleepwindowType = "SPT",
                                     desiredtz = "America/Anchorage",
                                     rec_starttime = "2025-10-07T09:00:00-0800", id = "123A"))
  expect_identical(unclass(ours$value), unclass(theirs$value))
  expect_identical(as.character(ours$value), "\nSleeplog for ID: 123A has missing date(s)")

  dup <- lines
  dup[2] <- sub("2025-10-09", "2025-10-08", dup[2])
  dup_path <- write_csv_lines(d$root, "dupdate.csv", dup)
  theirs <- with_warnings(GGIR:::g.loadlog(loglocation = dup_path, coln1 = 2, colid = 1,
                                           meta.sleep.folder = folder,
                                           desiredtz = "America/Anchorage",
                                           sleepwindowType = "SPT"))
  ours <- with_warnings(raw.sleeplog(path = dup_path, colid = 1, coln1 = 2,
                                     sleepwindowType = "SPT",
                                     desiredtz = "America/Anchorage",
                                     rec_starttime = "2025-10-07T09:00:00-0800", id = "123A"))
  expect_identical(unclass(ours$value), unclass(theirs$value))
  expect_identical(as.character(ours$value),
                   "\n123A has duplicate dates in the diary, please fix 2025-10-08")
})

test_that("a single date column leaves the diary untouched and errors in the basic parser", {
  skip_without_ggir()
  d <- make_diaries()
  folder <- make_ms3_folder(d$root, "ms3one", "123A", "2025-10-07T09:00:00-0800")
  path <- write_csv_lines(d$root, "onedate.csv",
                          c("ID,D1_date,D1_onset,D1_wakeup", "123A,2025-10-07,23:00:00,07:00:00"))

  theirs <- with_warnings(GGIR:::g.loadlog(loglocation = path, coln1 = 2, colid = 1,
                                           meta.sleep.folder = folder,
                                           desiredtz = "America/Anchorage",
                                           sleepwindowType = "SPT"))
  ours <- with_warnings(raw.sleeplog(path = path, colid = 1, coln1 = 2, sleepwindowType = "SPT",
                                     desiredtz = "America/Anchorage",
                                     rec_starttime = "2025-10-07T09:00:00-0800", id = "123A"))
  # The date parser never runs, so the date string reaches the timestamp parser as-is.
  expect_identical(unclass(ours$value), unclass(theirs$value))
  expect_identical(as.character(ours$value),
                   "NA as found in the sleeplog is not a valid timestamp")
})

test_that("an advanced diary without recording start information warns and matches nothing", {
  skip_without_ggir()
  d <- make_diaries()
  # GGIR errors here with "object 'startdates' not found"; the port warns and returns empty.
  ours <- with_warnings(raw.sleeplog(path = d$advanced, colid = 1, coln1 = 2,
                                     sleepwindowType = "SPT",
                                     desiredtz = "America/Anchorage"))
  expect_false(inherits(ours$value, "sleeplog_error"))
  expect_true(any(grepl("rec_starttime and id have not been specified", ours$warnings)))
  expect_true(any(grepl("None of the IDs", ours$warnings)))
  expect_null(ours$value$sleeplog)
})

# IDENTIFIER EXTRACTION

test_that(".raw.sleeplog.extractid reproduces g.part4_extractid on every branch", {
  skip_without_ggir()
  sl <- function(ids) data.frame(ID = ids, night = seq_along(ids), stringsAsFactors = FALSE)
  cases <- list(
    list(label = "idloc 1 strips .RDa", idloc = 1, fname = "MOS2E39230594.gt3x.RData",
         dolog = TRUE, sleeplog = sl(c("MOS2E39230594.gt3x", "other")), accid = c()),
    list(label = "idloc 2 underscore", idloc = 2, fname = "123_left.gt3x",
         dolog = TRUE, sleeplog = sl(c("123", "456")), accid = c()),
    list(label = "idloc 5 space", idloc = 5, fname = "123 left.gt3x",
         dolog = TRUE, sleeplog = sl(c("123", "456")), accid = c()),
    list(label = "idloc 6 dot", idloc = 6, fname = "123.left.gt3x",
         dolog = TRUE, sleeplog = sl(c("123", "456")), accid = c()),
    list(label = "idloc 7 hyphen", idloc = 7, fname = "123-left.gt3x",
         dolog = TRUE, sleeplog = sl(c("123", "456")), accid = c()),
    list(label = "match 1 spaces stripped", idloc = 1, fname = "x",
         dolog = TRUE, sleeplog = sl(c(" 123 ", "999")), accid = "123"),
    list(label = "match 2 case insensitive", idloc = 1, fname = "x",
         dolog = TRUE, sleeplog = sl(c("abc123", "zzz")), accid = "ABC123"),
    list(label = "match 3 letters removed", idloc = 1, fname = "x",
         dolog = TRUE, sleeplog = sl(c("sub123", "zzz")), accid = "ID123"),
    list(label = "match 4 leading zeros", idloc = 1, fname = "x",
         dolog = TRUE, sleeplog = sl(c("123", "zzz")), accid = "000123"),
    list(label = "no match at all", idloc = 1, fname = "x",
         dolog = TRUE, sleeplog = sl(c("999", "888")), accid = "123"),
    list(label = "two diary rows match", idloc = 1, fname = "x",
         dolog = TRUE, sleeplog = sl(c("123", "123 ")), accid = "123"),
    list(label = "dolog FALSE", idloc = 1, fname = "MOS2.gt3x.RData",
         dolog = FALSE, sleeplog = NULL, accid = c())
  )
  for (cs in cases) {
    theirs <- with_warnings(GGIR:::g.part4_extractid(cs$idloc, cs$fname, cs$dolog,
                                                     cs$sleeplog, cs$accid))
    ours <- with_warnings(canhrActi:::.raw.sleeplog.extractid(cs$idloc, cs$fname, cs$dolog,
                                                              cs$sleeplog, cs$accid))
    expect_identical(ours$value, theirs$value, info = cs$label)
    expect_identical(ours$warnings, theirs$warnings, info = cs$label)
  }

  # The four fallbacks on their own, so a reordering is caught.
  strip <- canhrActi:::.raw.sleeplog.extractid
  expect_identical(strip(1, "x", TRUE, sl(c(" 123 ", "999")), "123")$matching_indices_sleeplog, 1L)
  expect_identical(strip(1, "x", TRUE, sl(c("abc123", "zzz")), "ABC123")$matching_indices_sleeplog, 1L)
  expect_identical(strip(1, "x", TRUE, sl(c("zzz", "sub123")), "ID123")$matching_indices_sleeplog, 2L)
  expect_identical(strip(1, "x", TRUE, sl(c("zzz", "123")), "000123")$matching_indices_sleeplog, 2L)
  expect_identical(strip(1, "x", TRUE, sl(c("999", "888")), "123")$matching_indices_sleeplog,
                   integer(0))
  expect_identical(strip(1, "MOS2.gt3x.RData", FALSE, NULL, c())$accid, "MOS2.gt3x")
  expect_identical(strip(1, "MOS2.gt3x.RData", FALSE, NULL, c())$matching_indices_sleeplog, 1)
  expect_warning(strip(1, "x", TRUE, sl(c("123", "123 ")), "123"),
                 "matched to more than one entrance")
})

# INTERNALS

test_that("the basic parser keeps GGIR's defects", {
  # dur carries over when wake equals onset and is divided by 3600 again, so night 2
  # reports 8 / 3600 hours instead of 0, as in g.loadlog
  S <- data.frame(ID = "a", o1 = "23:00:00", w1 = "07:00:00",
                  o2 = "05:00:00", w2 = "05:00:00", stringsAsFactors = FALSE)
  out <- canhrActi:::.raw.sleeplog.basic(S, nnights = 2, mode = "sleeplog", colid = 1, coln1 = 2)
  expect_identical(out$duration, c("8", "0.00222222222222222"))
  expect_equal(as.numeric(out$duration[2]), 8 / 3600)

  # on the very first pair it errors instead
  S0 <- data.frame(ID = "a", o1 = "05:00:00", w1 = "05:00:00", stringsAsFactors = FALSE)
  expect_error(canhrActi:::.raw.sleeplog.basic(S0, nnights = 1, mode = "sleeplog",
                                               colid = 1, coln1 = 2),
               "'dur' not found")

  # HH:MM without seconds gets a zero second field
  S2 <- data.frame(ID = "a", o1 = "23:00", w1 = "07:00", stringsAsFactors = FALSE)
  out2 <- canhrActi:::.raw.sleeplog.basic(S2, nnights = 1, mode = "sleeplog", colid = 1, coln1 = 2)
  expect_identical(out2$sleeponset, "23:0:0")
  expect_identical(out2$sleepwake, "7:0:0")
  expect_identical(out2$duration, "8")

  # an identifier that is literally "0" is swept away with the unfilled rows
  S3 <- data.frame(ID = c("0", "b"), o1 = c("23:00:00", "23:00:00"),
                   w1 = c("07:00:00", "07:00:00"), stringsAsFactors = FALSE)
  out3 <- canhrActi:::.raw.sleeplog.basic(S3, nnights = 1, mode = "sleeplog", colid = 1, coln1 = 2)
  expect_identical(out3$ID, "b")
})

test_that("the bedlog mode names its columns differently and wraps past midnight", {
  S <- data.frame(ID = "a", b1 = "22:30:00", e1 = "06:45:30", stringsAsFactors = FALSE)
  out <- canhrActi:::.raw.sleeplog.basic(S, nnights = 1, mode = "bedlog", colid = 1, coln1 = 2)
  expect_identical(names(out), c("ID", "night", "duration", "bedstart", "bedend"))
  expect_identical(out$bedstart, "22:30:0")
  expect_identical(out$bedend, "6:45:30")
  expect_equal(as.numeric(out$duration), (((24 * 3600) - 81000) + 24330) / 3600)
})
