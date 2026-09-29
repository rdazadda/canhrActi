# raw.sleep.part3 and as.ggir.ms3 against GGIR's g.part3: the whole ms3 milestone, identical()
# field by field and then as a whole, against the stored milestones of the two reference
# recordings and the eight fixtures, a live GGIR::g.part3 run, and the full canhrActi
# pipeline. GGIRversion and tail_expansion_log are not compared. Reference data come from
# CANHRACTI_GGIR_REF and the fixtures beside it, and are skipped when absent; live GGIR
# comparisons skip when GGIR is not installed. The MOS2 milestones were stored with desiredtz
# "" (the system zone) on a machine set to America/Anchorage, so the file runs in that zone.

withr::local_timezone("America/Anchorage")

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

ref_file <- function(...) file.path(sub("/+$", "", .ggir_ref), ...)
p34_fixture <- function(case) {
  file.path(dirname(sub("/+$", "", .ggir_ref)), "ggir-study-p34", "fixtures", case,
            "output_din")
}

# The twelve ms3 objects that are compared.
MS3_FIELDS <- c("sib.cla.sum", "L5list", "SPTE_end", "SPTE_start", "tib.threshold",
                "rec_starttime", "ID", "longitudinal_axis", "SleepRegularityIndex",
                "part3_guider", "desiredtz_part1", "SPTE_corrected")

P3_CASES <- list(
  MOS2 = list(out = c("out", "output_din"),           fn = "MOS2E39230594.gt3x"),
  EE   = list(out = c("timing_out", "output_timing"), fn = "EE_left_29.5.2017-05-30.gt3x"))

p3_path <- function(case, what) {
  cc <- P3_CASES[[case]]
  switch(what,
         basic = do.call(ref_file, c(as.list(cc$out), list("meta", "basic",
                                                           paste0("meta_", cc$fn, ".RData")))),
         ms2   = do.call(ref_file, c(as.list(cc$out), list("meta", "ms2.out",
                                                           paste0(cc$fn, ".RData")))),
         ms3   = do.call(ref_file, c(as.list(cc$out), list("meta", "ms3.out",
                                                           paste0(cc$fn, ".RData")))),
         dir   = do.call(ref_file, as.list(cc$out)))
}

# One recording as canhrActi sees it: the part-1 milestone through read.ggir.milestone() and
# the stored part-2 IMP as $imputed, so a part-3 difference cannot come from part 2.
p3_load <- function(basic, ms2, ms3) {
  e2 <- new.env(parent = emptyenv()); load(ms2, envir = e2)
  e3 <- new.env(parent = emptyenv()); load(ms3, envir = e3)
  x <- read.ggir.milestone(basic)
  x$imputed <- e2$IMP
  list(x = x, IMP = e2$IMP, SUM = e2$SUM, ms3 = e3, tz = e3$desiredtz_part1)
}

p3_case <- local({
  cache <- new.env(parent = emptyenv())
  function(case) {
    if (exists(case, envir = cache, inherits = FALSE)) return(get(case, envir = cache))
    skip_if_no_ggir_ref()
    pb <- p3_path(case, "basic"); skip_if_no_file(pb)
    p2 <- p3_path(case, "ms2");   skip_if_no_file(p2)
    p3 <- p3_path(case, "ms3");   skip_if_no_file(p3)
    d <- p3_load(pb, p2, p3)
    ours <- raw.sleep.part3(d$x)
    out <- list(d = d, ours = ours, ms3 = as.ggir.ms3(ours))
    assign(case, out, envir = cache)
    out
  }
})

# The fixtures; two were produced with ignorenonwear = FALSE.
P3_FIXTURE_ARGS <- list(novalid_keepsibs = list(ignorenonwear = FALSE),
                        keepsibs = list(ignorenonwear = FALSE))

p3_fixture <- local({
  cache <- new.env(parent = emptyenv())
  function(case) {
    if (exists(case, envir = cache, inherits = FALSE)) return(get(case, envir = cache))
    skip_if_no_ggir_ref()
    root <- p34_fixture(case)
    if (!dir.exists(root)) testthat::skip(paste0("fixture not present: ", case))
    pb <- list.files(file.path(root, "meta", "basic"), full.names = TRUE)
    p2 <- list.files(file.path(root, "meta", "ms2.out"), full.names = TRUE)
    p3 <- list.files(file.path(root, "meta", "ms3.out"), full.names = TRUE)
    if (length(pb) == 0 || length(p2) == 0 || length(p3) == 0) {
      testthat::skip(paste0("fixture incomplete: ", case))
    }
    d <- p3_load(pb[1], p2[1], p3[1])
    ours <- do.call(raw.sleep.part3, c(list(x = d$x), P3_FIXTURE_ARGS[[case]]))
    out <- list(d = d, ours = ours, ms3 = as.ggir.ms3(ours))
    assign(case, out, envir = cache)
    out
  }
})

# The twelve objects one by one and then as a whole, naming the first difference.
expect_ms3_identical <- function(ms3, ref_env, info = "") {
  for (nm in MS3_FIELDS) {
    ours <- ms3[[nm]]
    theirs <- get(nm, envir = ref_env)
    ok <- identical(ours, theirs)
    if (!ok && is.data.frame(ours) && is.data.frame(theirs) &&
        identical(dim(ours), dim(theirs))) {
      for (cl in names(theirs)) {
        if (!identical(ours[[cl]], theirs[[cl]])) {
          i <- which(ours[[cl]] != theirs[[cl]] |
                       is.na(ours[[cl]]) != is.na(theirs[[cl]]))[1]
          testthat::fail(paste0(info, " ", nm, "$", cl, " differs first at row ", i,
                                ": ours ", ours[[cl]][i], ", GGIR ", theirs[[cl]][i]))
        }
      }
    }
    expect_identical(ours, theirs, info = paste(info, nm))
  }
  expect_identical(ms3[MS3_FIELDS], mget(MS3_FIELDS, envir = ref_env), info = info)
}

test_that("S6a raw.sleep.part3 reproduces the stored MOS2 ms3, object by object", {
  cc <- p3_case("MOS2")
  expect_ms3_identical(cc$ms3, cc$d$ms3, info = "MOS2")
})

test_that("S6a the MOS2 reference numbers are the ones the specification quotes", {
  cc <- p3_case("MOS2")
  m <- cc$ms3
  expect_identical(dim(m$sib.cla.sum), c(111L, 9L))
  expect_identical(names(m$sib.cla.sum),
                   c("night", "definition", "start.time.day", "nsib.periods",
                     "tot.sib.dur.hrs", "fraction.night.invalid", "sib.period",
                     "sib.onset.time", "sib.end.time"))
  expect_identical(as.numeric(table(m$sib.cla.sum$night)), c(34, 38, 36, 3))
  expect_identical(sort(unique(m$sib.cla.sum$night)), c(1, 2, 3, 4))
  expect_identical(unique(as.character(m$sib.cla.sum$definition)), "T5A5")
  expect_identical(m$sib.cla.sum$sib.onset.time[1], "2025-10-07T21:11:00-0800")
  expect_identical(m$sib.cla.sum$sib.end.time[1], "2025-10-07T21:18:50-0800")
  expect_identical(m$sib.cla.sum$tot.sib.dur.hrs[1], 7.92 / 60)
  expect_identical(round(m$sib.cla.sum$fraction.night.invalid[1], 7), 0.4061921)
  # the last three L5 values repeat because the imputed non-wear days are the same average day
  expect_length(m$L5list, 7L)
  expect_equal(round(m$L5list, 5),
               c(19.04306, 26.01389, 28.00139, 27.95139, 27.95139, 27.95139, 27.95139),
               tolerance = 1e-9)
  # the leading NA is the partial-first-day defect
  expect_length(m$SPTE_start, 7L)
  expect_true(is.na(m$SPTE_start[1]))
  expect_identical(which(is.na(m$SPTE_start)), c(1L, 5L, 6L, 7L))
  expect_equal(round(m$SPTE_start[2:4], 5), c(23.45972, 23.72361, 22.04028), tolerance = 1e-9)
  expect_length(m$SPTE_end, 7L)
  expect_identical(which(is.na(m$SPTE_end)), c(1L, 5L, 6L, 7L))
  expect_equal(round(m$SPTE_end[2:4], 5), c(31.85972, 31.07083, 31.07083), tolerance = 1e-9)
  # night 1 has a real threshold and guider next to NA hours; nights 5 to 7 are all non-wear
  expect_identical(m$tib.threshold, c(0.13, 0.13, 0.13, 0.13, NA, NA, NA))
  expect_identical(m$part3_guider, c("HDCZA", "HDCZA", "HDCZA", "HDCZA", NA, NA, NA))
  expect_false(is.na(m$tib.threshold[1]))
  expect_identical(dim(m$SleepRegularityIndex), c(7L, 5L))
  expect_identical(names(m$SleepRegularityIndex),
                   c("day", "SleepRegularityIndex", "weekday", "frac_valid", "date"))
  expect_identical(m$SleepRegularityIndex$weekday[1], "Tuesday")
  expect_identical(m$SleepRegularityIndex$date[1], "07/10/2025")
  expect_identical(m$SleepRegularityIndex$SleepRegularityIndex[1:2], c(17.619, 30.256))
  expect_identical(m$ID, "MOS2E39230594.gt3x")
  expect_identical(m$rec_starttime, "2025-10-07T20:30:00-0800")
  expect_null(m$longitudinal_axis)
  expect_null(m$SPTE_corrected)
  expect_identical(m$desiredtz_part1, "")
})

test_that("S6a rec_starttime comes from the IMPUTED frame, not the part-1 one", {
  cc <- p3_case("MOS2")
  # the same string on this recording, but the source is IMP
  expect_identical(cc$ms3$rec_starttime, cc$d$IMP$metashort[1, 1])
  expect_identical(cc$ms3$rec_starttime, cc$ours$rec_starttime)
  # the 04:00 rule of the sib summary reads the part-1 frame
  expect_identical(cc$d$IMP$metashort$timestamp, as.ggir.M(cc$d$x$meta)$metashort$timestamp)
})

test_that("as.ggir.ms3 emits GGIR's fourteen names and nothing else", {
  cc <- p3_case("MOS2")
  expect_length(cc$ms3, 14L)
  expect_identical(names(cc$ms3),
                   c("sib.cla.sum", "L5list", "SPTE_end", "SPTE_start", "tib.threshold",
                     "rec_starttime", "ID", "longitudinal_axis", "SleepRegularityIndex",
                     "tail_expansion_log", "GGIRversion", "part3_guider", "desiredtz_part1",
                     "SPTE_corrected"))
  expect_identical(sort(names(cc$ms3)), sort(ls(cc$d$ms3)))
  # the two exempt fields
  expect_null(cc$ms3$tail_expansion_log)
  if (requireNamespace("GGIR", quietly = TRUE)) {
    expect_identical(cc$ms3$GGIRversion, utils::packageVersion("GGIR"))
  }
  # as.ggir.ms3 is a pure renaming
  x <- cc$d$x
  x$sleep <- cc$ours
  expect_identical(as.ggir.ms3(x), cc$ms3)
  expect_identical(cc$ms3$ID, cc$ours$id)
  expect_identical(cc$ms3$sib.cla.sum, cc$ours$sib.cla.sum)
  expect_error(as.ggir.ms3(list(a = 1)), "canhrActi_raw_sleep")
})

test_that("the object carries the state GGIR expresses by writing or not writing a file", {
  cc <- p3_case("MOS2")
  s <- cc$ours
  expect_s3_class(s, "canhrActi_raw_sleep")
  expect_identical(s$status$state, "ok")
  expect_false(s$status$detection_failed)
  expect_identical(s$settings$twd, c(-12, 12))
  expect_identical(s$settings$desiredtz, "")
  expect_true(s$settings$ignorenonwear)
  expect_false(s$settings$guider_cor_do)
  expect_true(s$settings$ggir_exact)
  expect_true(all(c("impute", "detect", "sri", "summary", "total") %in%
                    names(s$status$stage_times)))
  expect_true(is.na(s$status$stage_times[["guider_correct"]])) # off by default
  expect_gt(s$status$stage_times[["total"]], 0)
})

test_that("$epochs is the series GGIR discards, with spt_crude_estimate dropped", {
  cc <- p3_case("MOS2")
  s <- cc$ours
  expect_identical(dim(s$epochs), c(118800L, 4L))
  expect_identical(names(s$epochs), c("time", "invalid", "night", "T5A5"))
  expect_identical(names(s$sib$output),
                   c("time", "invalid", "night", "T5A5", "spt_crude_estimate"))
  expect_identical(s$epochs, s$sib$output[, -which(names(s$sib$output) == "spt_crude_estimate")])
  expect_identical(as.numeric(table(s$epochs$night)),
                   c(3959, 11161, 17280, 17280, 17280, 17280, 17280, 17280))
  # the drop is what makes sib.cla.sum 111 rows and not 112
  expect_identical(nrow(raw.sib.summary(list(output = s$epochs), cc$d$x$meta,
                                        ignorenonwear = TRUE, desiredtz = "")), 111L)
  expect_error(raw.sib.summary(list(output = s$sib$output), cc$d$x$meta),
               "spt_crude_estimate")
})

test_that("T2 GGIR itself returns 112 rows when spt_crude_estimate is not dropped", {
  skip_if_no_ggir()
  cc <- p3_case("MOS2")
  s <- cc$ours
  SLE <- as.ggir.SLE(s$sib)
  M <- as.ggir.M(cc$d$x$meta)
  undropped <- GGIR:::g.sib.sum(SLE, M, ignorenonwear = TRUE, desiredtz = "")
  expect_identical(nrow(undropped), 112L)
  expect_identical(sort(unique(as.character(undropped$definition))),
                   c("spt_crude_estimate", "T5A5"))
  SLE$output <- SLE$output[, -which(names(SLE$output) == "spt_crude_estimate")]
  expect_identical(GGIR:::g.sib.sum(SLE, M, ignorenonwear = TRUE, desiredtz = ""),
                   s$sib.cla.sum)
})

test_that("$nights is complete where sib.cla.sum is not", {
  cc <- p3_case("MOS2")
  n <- cc$ours$nights
  expect_identical(nrow(n), 7L)
  expect_identical(n$night, c(1, 2, 3, 4, 5, 6, 7))
  # nights 5 to 7 have no row in GGIR's table
  expect_identical(sort(unique(cc$ours$sib.cla.sum$night)), c(1, 2, 3, 4))
  expect_identical(n$in_sib_cla_sum, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(n$n_sib_periods, c(34, 38, 36, 3, 0, 0, 0))
  expect_identical(n$n_epochs, c(11161, 17280, 17280, 17280, 17280, 17280, 17280))
  expect_identical(n$n_invalid, c(900, 0, 0, 9181, 17280, 17280, 17280))
  expect_identical(n$start.time[1], "2025-10-07T20:30:00-0800")
  expect_identical(n$end.time[7], "2025-10-14T12:00:00-0800")
  expect_identical(n$date, c("2025-10-07", "2025-10-08", "2025-10-09", "2025-10-10",
                             "2025-10-11", "2025-10-12", "2025-10-13"))
  expect_identical(n$partial_first_day, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(n$skipped, rep(FALSE, 7))
  # the recomputed fraction is the one g.sib.sum stored
  for (k in 1:4) {
    expect_identical(n$fraction_night_invalid[k],
                     cc$ours$sib.cla.sum$fraction.night.invalid[
                       which(cc$ours$sib.cla.sum$night == k)[1]])
  }
  expect_identical(n$fraction_night_invalid[5:7], c(1, 1, 1))
  expect_identical(n$SPTE_start, cc$ms3$SPTE_start)
  expect_identical(n$SPTE_end, cc$ms3$SPTE_end)
  expect_identical(n$tib.threshold, cc$ms3$tib.threshold)
  expect_identical(n$L5, cc$ms3$L5list)
  expect_identical(n$guider, cc$ms3$part3_guider)
  expect_identical(unique(n$definition), "T5A5")
  expect_equal(sum(n$sib_dur_hrs), sum(cc$ours$sib.cla.sum$tot.sib.dur.hrs),
               tolerance = 1e-12)
  expect_equal(round(sum(n$sib_dur_hrs), 4), 33.0625, tolerance = 1e-9)
})

test_that("print.canhrActi_raw_sleep says what the object holds", {
  cc <- p3_case("MOS2")
  out <- utils::capture.output(print.canhrActi_raw_sleep(cc$ours))
  expect_true(any(grepl("GGIR part 3", out)))
  expect_true(any(grepl("MOS2E39230594.gt3x", out)))
  expect_true(any(grepl("111 rows over 4 nights", out)))
  expect_true(any(grepl("7 noon-to-noon windows, 4 with sustained", out)))
  expect_true(any(grepl("system zone", out))) # desiredtz_part1 is ""
})

# GGIR's own g.part3 over a copy of a reference run's part-1 and part-2 milestones.
p3_run_ggir <- function(srcdir, ...) {
  dest <- file.path(tempfile("p3ggir"), "out")
  dir.create(file.path(dest, "meta", "basic"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dest, "meta", "ms2.out"), recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(file.path(srcdir, "meta", "basic"), full.names = TRUE),
            file.path(dest, "meta", "basic"))
  file.copy(list.files(file.path(srcdir, "meta", "ms2.out"), full.names = TRUE),
            file.path(dest, "meta", "ms2.out"))
  GGIR::g.part3(metadatadir = dest, f0 = 1, f1 = 1, verbose = FALSE, ...)
  produced <- list.files(file.path(dest, "meta", "ms3.out"), full.names = TRUE)
  if (length(produced) == 0) return(NULL)
  e <- new.env(parent = emptyenv())
  load(produced[1], envir = e)
  e
}

test_that("S6c T2 a live GGIR g.part3 on the same milestones gives the same twelve objects", {
  skip_if_no_ggir()
  cc <- p3_case("MOS2")
  fresh <- p3_run_ggir(p3_path("MOS2", "dir"))
  expect_false(is.null(fresh))
  expect_ms3_identical(cc$ms3, fresh, info = "MOS2 live g.part3")
  # the fresh run also matches the stored milestone
  for (nm in MS3_FIELDS) {
    expect_identical(get(nm, envir = fresh), get(nm, envir = cc$d$ms3), info = nm)
  }
})

test_that("S6c T2 the guider correction branch is wired the way g.part3 wires it", {
  skip_if_no_ggir()
  skip_if_no_ggir_ref()
  cc <- p3_case("MOS2")
  ours <- raw.sleep.part3(cc$d$x, guider_cor_do = TRUE)
  fresh <- p3_run_ggir(p3_path("MOS2", "dir"), guider_cor_do = TRUE)
  expect_false(is.null(fresh))
  expect_ms3_identical(as.ggir.ms3(ours), fresh, info = "MOS2 guider_cor_do")
  # the correction ran: SPTE_corrected is set and the corrector dropped the crude estimate
  expect_false(is.null(ours$SPTE_corrected))
  expect_length(ours$SPTE_corrected, length(ours$SPTE_start))
  expect_false("spt_crude_estimate" %in% names(ours$epochs))
  expect_false(is.na(ours$status$stage_times[["guider_correct"]]))
})

test_that("S6c T2 the strongest test: GGIR's own g.part4 reads the port's ms3", {
  skip_if_no_ggir()
  cc <- p3_case("MOS2")
  ms4ref <- ref_file("out", "output_din", "meta", "ms4.out", "MOS2E39230594.gt3x.RData")
  skip_if_no_file(ms4ref)
  # write the fourteen objects as g.part3 saves them and let GGIR's own part 4 read them
  dest <- file.path(tempfile("p3ms3"), "out")
  for (d in c(file.path("meta", "basic"), file.path("meta", "ms2.out"),
              file.path("meta", "ms3.out"), "results")) {
    dir.create(file.path(dest, d), recursive = TRUE, showWarnings = FALSE)
  }
  src <- p3_path("MOS2", "dir")
  file.copy(list.files(file.path(src, "meta", "basic"), full.names = TRUE),
            file.path(dest, "meta", "basic"))
  file.copy(list.files(file.path(src, "meta", "ms2.out"), full.names = TRUE),
            file.path(dest, "meta", "ms2.out"))
  env <- list2env(cc$ms3, envir = new.env(parent = emptyenv()))
  save(list = names(cc$ms3), envir = env,
       file = file.path(dest, "meta", "ms3.out", "MOS2E39230594.gt3x.RData"))
  GGIR::g.part4(metadatadir = dest, f0 = 1, f1 = 1, verbose = FALSE)
  produced <- list.files(file.path(dest, "meta", "ms4.out"), full.names = TRUE)
  expect_length(produced, 1L)
  en <- new.env(parent = emptyenv()); load(produced[1], envir = en)
  er <- new.env(parent = emptyenv()); load(ms4ref, envir = er)
  expect_identical(dim(en$nightsummary), c(4L, 39L))
  if (!identical(en$nightsummary, er$nightsummary)) {
    for (cl in names(er$nightsummary)) {
      if (!identical(en$nightsummary[[cl]], er$nightsummary[[cl]])) {
        testthat::fail(paste0("nightsummary$", cl, " differs: ours ",
                              paste(en$nightsummary[[cl]], collapse = ", "), "; GGIR ",
                              paste(er$nightsummary[[cl]], collapse = ", ")))
      }
    }
  }
  expect_identical(en$nightsummary, er$nightsummary)
})

test_that("the sib member is the detector object as.ggir.SLE expects", {
  cc <- p3_case("MOS2")
  x <- cc$d$x
  x$sleep <- cc$ours
  expect_s3_class(x$sleep$sib, "canhrActi_raw_sib")
  SLE <- as.ggir.SLE(x)
  expect_identical(names(SLE),
                   c("output", "detection.failed", "L5list", "SPTE_end", "SPTE_start",
                     "tib.threshold", "longitudinal_axis", "part3_guider"))
  expect_true(is.factor(SLE$output$time))
  expect_identical(SLE$L5list, cc$ms3$L5list)
  expect_identical(SLE$SPTE_start, cc$ms3$SPTE_start)
  expect_identical(nrow(x$sleep$sib$windows), 7L)
})

test_that("do.part3.pdf is recorded rather than drawn", {
  cc <- p3_case("MOS2")
  s <- raw.sleep.part3(cc$d$x, do.part3.pdf = TRUE)
  expect_true(s$settings$do.part3.pdf)
  expect_true(any(grepl("graphperday_id_", s$status$messages)))
  expect_identical(as.ggir.ms3(s)[MS3_FIELDS], cc$ms3[MS3_FIELDS])
})

test_that("part 1's timezone overrides the caller's, and the empty string is not NULL", {
  cc <- p3_case("MOS2")
  # the stored "" wins over the caller's zone
  other <- raw.sleep.part3(cc$d$x, desiredtz = "Europe/Helsinki")
  expect_identical(other$desiredtz_part1, "")
  expect_identical(other$settings$desiredtz, "")
  expect_identical(as.ggir.ms3(other)[MS3_FIELDS], cc$ms3[MS3_FIELDS])
  # with no recorded part-1 timezone at all, the caller's value is used and recorded
  x <- cc$d$x
  x$tz["desiredtz"] <- list(NULL)
  x$meta$settings$desiredtz <- "America/Anchorage"
  none <- raw.sleep.part3(x, desiredtz = "America/Anchorage")
  expect_identical(none$desiredtz_part1, "America/Anchorage")
  expect_identical(none$settings$desiredtz, "America/Anchorage")
})

test_that("S6b raw.sleep.part3 reproduces the stored EE ms3, object by object", {
  cc <- p3_case("EE")
  expect_ms3_identical(cc$ms3, cc$d$ms3, info = "EE")
  m <- cc$ms3
  expect_identical(m$desiredtz_part1, "Europe/Helsinki")
  expect_identical(m$rec_starttime, "2017-05-23T09:15:00+0300")
  expect_identical(dim(m$sib.cla.sum), c(132L, 9L))
  expect_identical(as.numeric(table(m$sib.cla.sum$night)), c(25, 22, 23, 18, 20, 24))
  # the 0.13 floor is not always hit
  expect_equal(round(m$tib.threshold, 5),
               c(0.13, 0.20385, 0.132, 0.207, 0.1785, 0.1725, NA), tolerance = 1e-9)
  expect_equal(round(m$SPTE_start, 5),
               c(22.27778, 23.62222, 25.02222, 24.33333, 24.28472, 22.77639, NA),
               tolerance = 1e-9)
  expect_equal(round(m$SPTE_end, 5),
               c(30.11806, 33.01667, 32.46944, 30.65833, 32.825, 30.24722, NA),
               tolerance = 1e-9)
  expect_identical(m$part3_guider, c(rep("HDCZA", 6), NA))
  # night 1 is a full day here, so its SPTE is not NA
  expect_false(cc$ours$nights$partial_first_day[1])
  expect_false(is.na(m$SPTE_start[1]))
})

test_that("S6c T2 a live GGIR g.part3 reproduces EE as well", {
  skip_if_no_ggir()
  cc <- p3_case("EE")
  fresh <- p3_run_ggir(p3_path("EE", "dir"))
  expect_false(is.null(fresh))
  expect_ms3_identical(cc$ms3, fresh, info = "EE live g.part3")
})

for (.case in c("spring", "autumn", "autumn_edge", "daysleeper", "novalid",
                "novalid_keepsibs", "keepsibs", "nonights")) {
  local({
    case <- .case
    test_that(paste0("fixture ", case, ": the twelve ms3 objects are identical()"), {
      cc <- p3_fixture(case)
      expect_ms3_identical(cc$ms3, cc$d$ms3, info = case)
    })
  })
}

test_that("fixture novalid: the zero-row sib.cla.sum keeps its nine columns and classes", {
  cc <- p3_fixture("novalid")
  s <- cc$ours
  expect_identical(dim(s$sib.cla.sum), c(0L, 9L))
  expect_identical(vapply(s$sib.cla.sum, function(v) class(v)[1], character(1)),
                   c(night = "numeric", definition = "character",
                     start.time.day = "character", nsib.periods = "numeric",
                     tot.sib.dur.hrs = "numeric", fraction.night.invalid = "numeric",
                     sib.period = "numeric", sib.onset.time = "character",
                     sib.end.time = "character"))
  # never assigned, so the rep(NA, countmidn) pre-allocation is still logical
  expect_true(is.logical(s$SPTE_start) && all(is.na(s$SPTE_start)))
  expect_true(is.logical(s$SPTE_end) && all(is.na(s$SPTE_end)))
  expect_true(is.logical(s$tib.threshold) && all(is.na(s$tib.threshold)))
  expect_true(is.logical(s$part3_guider) && all(is.na(s$part3_guider)))
  expect_identical(s$L5list, rep(0, 7))
  expect_identical(s$status$state, "ok") # GGIR did write this milestone
  # the SRI is a real 7 x 5 frame of zeros, not NA
  expect_identical(dim(s$SleepRegularityIndex), c(7L, 5L))
  expect_identical(s$SleepRegularityIndex$frac_valid, rep(0, 7))
  expect_identical(nrow(s$nights), 7L)
  expect_identical(s$nights$in_sib_cla_sum, rep(FALSE, 7))
  expect_identical(s$nights$n_sib_periods, rep(0, 7))
})

test_that("fixture nonights: SleepRegularityIndex is the atomic NA, not a data frame", {
  cc <- p3_fixture("nonights")
  s <- cc$ours
  # 21600 epochs at 5 s is 1.25 days, under the two-day gate
  expect_identical(nrow(s$epochs), 21600L)
  expect_identical(s$SleepRegularityIndex, NA)
  expect_false(is.data.frame(s$SleepRegularityIndex))
  expect_identical(s$SleepRegularityIndex, cc$d$ms3$SleepRegularityIndex)
  expect_identical(dim(s$sib.cla.sum), c(55L, 9L))
  expect_identical(as.numeric(table(s$sib.cla.sum$night)), c(34, 21))
  expect_identical(s$status$state, "ok")
})

test_that("fixture keepsibs: ignorenonwear FALSE reaches the summary", {
  cc <- p3_fixture("keepsibs")
  s <- cc$ours
  expect_false(s$settings$ignorenonwear)
  expect_identical(dim(s$sib.cla.sum), c(225L, 9L))
  expect_identical(as.numeric(table(s$sib.cla.sum$night)), c(38, 38, 36, 29, 28, 28, 28))
  expect_identical(s$nights$in_sib_cla_sum, rep(TRUE, 7))
})

test_that("fixture daysleeper: the accepted re-run pushes the hours past 36", {
  cc <- p3_fixture("daysleeper")
  s <- cc$ours
  expect_true(all(s$SPTE_end[2:4] > 36, na.rm = TRUE))
  expect_equal(round(s$L5list, 5), c(19.00139, 32.01389, 33.5, 33.5, 33.5, 33.5, 33.5),
               tolerance = 1e-9)
  # the four per-night vectors are one short of the number of nights because the recording
  # starts between midnight and 04:00
  expect_length(s$L5list, 7L)
  expect_length(s$SPTE_start, 6L)
  expect_identical(max(s$nights$night), 7)
  expect_true(is.na(s$nights$SPTE_start[7]))
  expect_identical(s$nights$L5[7], 33.5)
})

test_that("F1 a corrupt recording gives a state, not an error and not a file", {
  cc <- p3_case("MOS2")
  x <- cc$d$x
  x$meta$filecorrupt <- TRUE
  s <- expect_silent(raw.sleep.part3(x))
  expect_identical(s$status$state, "skipped")
  expect_true(s$status$detection_failed)
  expect_null(s$sib.cla.sum)
  expect_null(s$rec_starttime)
  expect_null(s$epochs)
  expect_null(s$nights)
  expect_identical(s$SleepRegularityIndex, NA)
  expect_identical(s$id, "MOS2E39230594.gt3x")
  expect_identical(s$desiredtz_part1, "")
  expect_true(any(grepl("corrupt", s$status$messages)))
  # the GGIR-shaped list still has fourteen members
  m <- as.ggir.ms3(s)
  expect_length(m, 14L)
  expect_null(m$sib.cla.sum)
  expect_null(m$rec_starttime)
})

test_that("F2 a too-short recording (part 1's own flag) gives the same state", {
  cc <- p3_case("MOS2")
  x <- cc$d$x
  x$meta$filetooshort <- TRUE
  s <- raw.sleep.part3(x)
  expect_identical(s$status$state, "skipped")
  expect_null(s$sib.cla.sum)
  expect_true(any(grepl("too short", s$status$messages)))
})

test_that("F3 a sub-0.2-day recording fails the detector's own gate", {
  cc <- p3_case("MOS2")
  x <- cc$d$x
  IMP <- cc$d$IMP
  # 3000 epochs at 5 s is 0.174 days, 6000 is 0.347
  short <- IMP; short$metashort <- IMP$metashort[1:3000, ]
  x$imputed <- short
  s <- raw.sleep.part3(x)
  expect_identical(s$status$state, "too_short_for_sleep")
  expect_true(s$status$detection_failed)
  expect_null(s$sib.cla.sum)
  expect_null(s$epochs)
  expect_identical(s$SleepRegularityIndex, NA)
  expect_true(any(grepl("0.2 days", s$status$messages)))
  longer <- IMP; longer$metashort <- IMP$metashort[1:6000, ]
  x$imputed <- longer
  s2 <- raw.sleep.part3(x)
  expect_identical(s2$status$state, "ok")
  expect_identical(nrow(s2$epochs), 6000L)
  expect_identical(sort(unique(s2$sib.cla.sum$night)), 1)
})

test_that("the Sleep Regularity Index gate is strictly more than two days", {
  cc <- p3_case("MOS2")
  x <- cc$d$x
  IMP <- cc$d$IMP
  # 2 * 24 * (3600/5) = 34560 rows exactly: the gate is >, so this is NA
  at_gate <- IMP; at_gate$metashort <- IMP$metashort[1:34560, ]
  x$imputed <- at_gate
  s <- raw.sleep.part3(x)
  expect_identical(nrow(s$epochs), 34560L)
  expect_identical(s$SleepRegularityIndex, NA)
  over <- IMP; over$metashort <- IMP$metashort[1:34561, ]
  x$imputed <- over
  s2 <- raw.sleep.part3(x)
  expect_true(is.data.frame(s2$SleepRegularityIndex))
  expect_identical(names(s2$SleepRegularityIndex),
                   c("day", "SleepRegularityIndex", "weekday", "frac_valid", "date"))
})

test_that("a recording with no imputed series has one computed, with a message", {
  cc <- p3_case("MOS2")
  x <- cc$d$x
  x$imputed <- NULL
  s <- raw.sleep.part3(x)
  expect_true(any(grepl("raw.impute", s$status$messages)))
  # the same as with the stored IMP
  expect_identical(as.ggir.ms3(s)[MS3_FIELDS], cc$ms3[MS3_FIELDS])
})

test_that("the LC_TIME locale is forced to C and restored, including on the error path", {
  cc <- p3_case("MOS2")
  before <- Sys.getlocale("LC_TIME")
  s <- raw.sleep.part3(cc$d$x)
  expect_identical(Sys.getlocale("LC_TIME"), before)
  expect_error(raw.sleep.part3(list()), "canhrActi_raw")
  expect_identical(Sys.getlocale("LC_TIME"), before)
  # GGIR forces the C locale only in its parallel workers; here the weekday is always English
  expect_true(all(s$SleepRegularityIndex$weekday %in%
                    c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
                      "Sunday")))
})

test_that("ggir_exact = FALSE reaches the detector through the orchestrator", {
  cc <- p3_case("MOS2")
  loose <- raw.sleep.part3(cc$d$x, ggir_exact = FALSE)
  expect_false(loose$settings$ggir_exact)
  # the partial-first-day defect: NA under ggir_exact, a real value without it
  expect_true(is.na(cc$ms3$SPTE_start[1]))
  expect_equal(round(loose$SPTE_start[1], 5), 22.6375, tolerance = 1e-9)
  expect_equal(round(loose$SPTE_end[1], 5), 31.17917, tolerance = 1e-9)
  expect_identical(loose$SPTE_start[2:7], cc$ms3$SPTE_start[2:7])
  expect_identical(loose$L5list, cc$ms3$L5list)
  expect_identical(loose$tib.threshold, cc$ms3$tib.threshold)
  expect_identical(loose$sib.cla.sum, cc$ms3$sib.cla.sum)
  expect_identical(loose$SleepRegularityIndex, cc$ms3$SleepRegularityIndex)
})

test_that("raw.sleep.part3 refuses anything that is not a canhrActi_raw", {
  expect_error(raw.sleep.part3(list(meta = 1)), "canhrActi_raw")
  cc <- p3_case("MOS2")
  expect_error(raw.sleep.part3(cc$d$x, progress = "no"), "progress must be NULL")
})

test_that("the progress callback is called once per stage and once per night", {
  cc <- p3_case("MOS2")
  seen <- list()
  p <- function(stage, i, n, message) {
    seen[[length(seen) + 1]] <<- list(stage = stage, i = i, n = n)
    invisible(NULL)
  }
  s <- raw.sleep.part3(cc$d$x, progress = p)
  stages <- vapply(seen, function(z) z$stage, character(1))
  expect_true(all(c("part3", "sib_detect") %in% stages))
  expect_identical(length(which(stages == "part3")), 4L) # 5 stages, correction skipped
  expect_identical(length(which(stages == "sib_detect")), 7L) # one per night
  expect_identical(s$status$state, "ok")
})

test_that(".raw.sleep.part3.hip.axis reproduces g.part3's is.na test", {
  # part 2 estimated one
  expect_identical(.raw.sleep.part3.hip.axis(1L), 1L)
  expect_identical(.raw.sleep.part3.hip.axis(3), 3)
  # part 2 did not: GGIR's SUM carries NA, canhrActi carries ""
  expect_identical(.raw.sleep.part3.hip.axis(NA), 2)
  expect_identical(.raw.sleep.part3.hip.axis(NA_real_), 2)
  expect_identical(.raw.sleep.part3.hip.axis(""), 2)
  expect_identical(.raw.sleep.part3.hip.axis(NULL), 2)
  expect_identical(.raw.sleep.part3.hip.axis(character(0)), 2)
  expect_identical(.raw.sleep.part3.hip.axis("2"), 2)
})

test_that("the hip pre-step only fires for a hip recording and an unset axis", {
  cc <- p3_case("MOS2")
  x <- cc$d$x
  # wrist: the axis stays unset
  expect_null(cc$ours$longitudinal_axis)
  expect_null(cc$ours$settings$params$longitudinal_axis)
  # hip with no part-2 estimate: the axis becomes 2; raw.params() itself warns here, but the
  # part-3 call must be silent
  p_hip <- suppressWarnings(raw.params(sensor.location = "hip", HASPT.algo = "HorAngle"))
  hip <- expect_silent(raw.sleep.part3(x, params = p_hip))
  expect_identical(hip$longitudinal_axis, 2)
  expect_identical(hip$settings$params$longitudinal_axis, 2)
  # an explicit user value wins over part 2's
  p_hip3 <- suppressWarnings(raw.params(sensor.location = "hip", HASPT.algo = "HorAngle",
                                        longitudinal_axis = 3))
  hip3 <- raw.sleep.part3(x, params = p_hip3)
  expect_identical(hip3$longitudinal_axis, 3)
})

test_that("S6a the full pipeline read.raw.accelerometer -> raw.impute -> raw.sleep.part3 (slow)", {
  skip_if_no_ggir_ref()
  gt3x <- ref_file("din", "MOS2E39230594.gt3x")
  skip_if_no_file(gt3x)
  cc <- p3_case("MOS2")
  x <- read.raw.accelerometer(gt3x, desiredtz = "America/Anchorage")
  x$imputed <- raw.impute(x)
  s <- raw.sleep.part3(x)
  m <- as.ggir.ms3(s)
  # the reference run stored desiredtz ""; this call names the same zone explicitly, so only
  # the recorded string differs
  for (nm in setdiff(MS3_FIELDS, "desiredtz_part1")) {
    expect_identical(m[[nm]], get(nm, envir = cc$d$ms3), info = nm)
  }
  expect_identical(m$desiredtz_part1, "America/Anchorage")
  expect_identical(s$status$state, "ok")
  expect_identical(s$id, "MOS2E39230594.gt3x")
  expect_identical(s$rec_starttime, "2025-10-07T20:30:00-0800")
  expect_identical(dim(s$sib.cla.sum), c(111L, 9L))
  expect_identical(nrow(s$nights), 7L)
})
