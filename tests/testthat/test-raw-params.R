# Parity tests for raw.params() against GGIR's load_params() and check_params().
# No reference data are needed; the live comparisons skip when GGIR is not installed.

skip_if_no_ggir <- function() {
  if (!requireNamespace("GGIR", quietly = TRUE)) {
    testthat::skip("GGIR is not installed; live parity comparisons skipped")
  }
}

# The eight GGIR parameter objects raw.params() draws on, as one flat named list.
ggir_defaults <- function() {
  lp <- GGIR::load_params()
  c(lp$params_rawdata, lp$params_general, lp$params_metrics, lp$params_cleaning,
    lp$params_sleep, lp$params_output, lp$params_247, lp$params_phyact)
}

# GGIR::check_params on GGIR's own defaults with one member overridden; only the named
# group is passed.
ggir_check <- function(group, name, value) {
  lp <- GGIR::load_params()
  obj <- paste0("params_", group)
  if (is.null(value)) {
    lp[[obj]][name] <- list(NULL)
  } else {
    lp[[obj]][[name]] <- value
  }
  args <- list(lp[[obj]])
  names(args) <- obj
  do.call(GGIR::check_params, args)
}

# Collect the warning messages and the value of an expression.
with_warnings <- function(expr) {
  msgs <- character(0)
  val <- withCallingHandlers(expr, warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = val, warnings = msgs)
}

error_message <- function(expr) {
  tryCatch({ expr; NA_character_ }, error = function(e) conditionMessage(e))
}

# Members with no GGIR counterpart.
canhr_only <- c("ggir_exact", "rename_uppercase", "skip_small_files", "progress",
                "ggir_version_label", "unzip_once", "decode_once", "stream_gt3x")
# backup.cal.coef is NULL because there is no output folder to retrieve from;
# save_ms5raw_without_invalid is FALSE because there is no visualreport to force it.
deliberate_default_deviation <- c("backup.cal.coef", "save_ms5raw_without_invalid")
# New in GGIR 3.3-9, so absent from the installed 3.3.6 these tests compare against.
newer_than_installed_ggir <- c("HDCZA_roll_windowsize", "LowAcc_threshold")
# Members whose load_params() default check_params() changes on GGIR's own defaults;
# Sadeh_axis is blanked unless HASIB.algo is a count-based algorithm.
coerced_from_load_params_default <- c("Sadeh_axis")

test_that("raw.params() returns the documented class, groups and canhrActi switches", {
  p <- raw.params()
  expect_s3_class(p, "canhrActi_raw_params")
  expect_true(is.list(p))
  expect_identical(names(p), names(.raw.params.defaults()))
  expect_identical(names(attr(p, "groups")), names(p))
  expect_true(all(attr(p, "groups") %in% c("rawdata", "general", "metrics", "cleaning",
                                           "sleep", "output", "phyact", "247",
                                           "canhrActi")))
  expect_identical(p$ggir_exact, TRUE)
  expect_identical(p$rename_uppercase, FALSE)
  expect_identical(p$skip_small_files, FALSE)
  expect_null(p$progress)
  expect_true("progress" %in% names(p))
  expect_identical(p$ggir_version_label, "3.3.6")
  expect_identical(p$unzip_once, TRUE)
  expect_identical(p$decode_once, FALSE)
  expect_identical(p$stream_gt3x, TRUE)
  # backup.cal.coef: NULL, the value GGIR falls back to on a first run
  expect_true("backup.cal.coef" %in% names(p))
  expect_null(p$backup.cal.coef)
  counts <- table(attr(p, "groups"))
  expect_identical(as.integer(counts[c("rawdata", "general", "metrics", "cleaning",
                                       "sleep", "output", "phyact", "247", "canhrActi")]),
                   c(36L, 9L, 40L, 20L, 43L, 15L, 10L, 8L, 8L))
  # 181 GGIR-derived members and eight canhrActi switches
  expect_length(p, 189L)
  expect_identical(sum(attr(p, "groups") != "canhrActi"), 181L)
})

test_that("the spec's headline defaults are present with the quoted values", {
  p <- raw.params()
  expect_identical(p$desiredtz, "")
  expect_null(p$configtz)
  expect_identical(p$windowsizes, c(5, 900, 3600))
  expect_identical(p$chunksize, 1)
  expect_identical(p$minimumFileSizeMB, 2)
  expect_identical(p$imputeTimegaps, TRUE)
  expect_identical(p$interpolationType, 1)
  expect_identical(p$frequency_tol, 0.1)
  expect_null(p$dynrange)
  expect_identical(p$nonwear_range_threshold, 150)
  expect_identical(p$nonwear_approach, "2023")
  expect_identical(p$do.cal, TRUE)
  expect_identical(p$spherecrit, 0.3)
  expect_identical(p$minloadcrit, 168)
  expect_identical(p$rmc.noise, 13)
  expect_identical(p$do.enmo, TRUE)
  expect_identical(p$do.anglez, TRUE)
  expect_identical(p$do.anglex, FALSE)
  expect_identical(p$do.neishabouricounts, FALSE)
  expect_identical(p$hb, 15)
  expect_identical(p$lb, 0.2)
  expect_identical(p$n, 4)
  expect_identical(p$zc.lb, 0.25)
  expect_identical(p$zc.hb, 3)
  expect_identical(p$zc.sb, 0.01)
  expect_identical(p$zc.order, 2)
  expect_identical(p$actilife_LFE, FALSE)
  expect_identical(p$nonWearEdgeCorrection, TRUE)
  expect_identical(p$includedaycrit, 16)
  expect_identical(p$sensor.location, "wrist")
  expect_null(p$recordingEndSleepHour)
  expect_identical(p$dayborder, 0)
  expect_identical(p$idloc, 1)
  expect_identical(p$rmc.col.acc, 1:3)
  expect_identical(p$rmc.format.time, "%Y-%m-%d %H:%M:%OS")
  # every do.* flag other than enmo, anglez, visual and sibreport is FALSE
  do_flags <- names(p)[startsWith(names(p), "do.") & names(p) != "do.cal"]
  expect_length(do_flags, 34L)
  expect_true(all(vapply(do_flags[!do_flags %in% c("do.enmo", "do.anglez", "do.visual",
                                                   "do.sibreport")],
                         function(nm) identical(p[[nm]], FALSE), logical(1))))
  expect_true(p$do.visual)
  expect_true(p$do.sibreport)
  expect_false(p$do.part3.pdf)
})

test_that("every GGIR-derived default is identical() to GGIR::load_params()", {
  skip_if_no_ggir()
  p <- raw.params()
  g <- ggir_defaults()
  groups <- attr(p, "groups")
  compare <- setdiff(names(p), c(canhr_only, deliberate_default_deviation,
                                 newer_than_installed_ggir,
                                 coerced_from_load_params_default))
  expect_length(compare, 176L)
  unknown <- setdiff(compare, names(g))
  expect_identical(unknown, character(0), info = toString(unknown))
  n_checked <- 0L
  for (nm in compare) {
    expect_identical(p[[nm]], g[[nm]], label = paste0("default of ", nm))
    n_checked <- n_checked + 1L
  }
  expect_identical(n_checked, 176L)
  # the two 3.3-9 members, with the values 3.3.6 hardcodes
  expect_false(any(newer_than_installed_ggir %in% names(g)))
  expect_identical(p$HDCZA_roll_windowsize, 5)
  # Sadeh_axis: load_params says "Y" but check_params blanks it; the port matches post-coercion
  ggir_coerced <- suppressWarnings(do.call(GGIR::check_params, GGIR::load_params()))
  expect_identical(g[["Sadeh_axis"]], "Y")
  expect_identical(ggir_coerced$params_sleep$Sadeh_axis, "")
  expect_identical(p$Sadeh_axis, ggir_coerced$params_sleep$Sadeh_axis)
  # and it comes back when a Sadeh-type algorithm is chosen
  expect_identical(suppressWarnings(raw.params(HASIB.algo = "Sadeh1994")$Sadeh_axis), "Y")
  expect_identical(g[["backup.cal.coef"]], "retrieve")
  expect_null(p[["backup.cal.coef"]])
})

test_that("every member's group attribute names the GGIR object that holds it", {
  skip_if_no_ggir()
  lp <- GGIR::load_params()
  groups <- attr(raw.params(), "groups")
  canhr <- names(groups)[groups == "canhrActi"]
  newer <- setdiff(intersect(names(groups), newer_than_installed_ggir), canhr)
  rest <- setdiff(names(groups), c(canhr, newer))
  in_ggir <- intersect(canhr, unlist(lapply(lp, names)))
  expect_identical(in_ggir, character(0), info = toString(in_ggir))
  not_sleep <- newer[groups[newer] != "sleep"]
  expect_identical(not_sleep, character(0), info = toString(not_sleep))
  misplaced <- rest[!vapply(rest, function(nm) {
    nm %in% names(lp[[paste0("params_", groups[[nm]])]])
  }, logical(1))]
  expect_identical(misplaced, character(0), info = toString(misplaced))
})

test_that("raw.params() on GGIR's defaults is a fixed point of the coercions", {
  # every default passed back in reproduces raw.params(), except the Sadeh_axis blanking
  d <- .raw.params.defaults()
  p2 <- do.call(raw.params, d)
  expect_identical(unclass(p2)[setdiff(names(d), coerced_from_load_params_default)],
                   d[setdiff(names(d), coerced_from_load_params_default)])
  expect_identical(d$Sadeh_axis, "Y")
  expect_identical(p2$Sadeh_axis, "")
  expect_identical(p2, raw.params())
  expect_identical(do.call(raw.params, unclass(p2)), p2)
})

test_that(".raw.params.ggir() regroups members with values identical to GGIR's defaults", {
  skip_if_no_ggir()
  lp <- GGIR::load_params()
  split <- .raw.params.ggir(raw.params())
  expect_identical(names(split), c("params_rawdata", "params_general", "params_metrics",
                                   "params_cleaning", "params_sleep", "params_output",
                                   "params_247", "params_phyact"))
  skipped <- c(deliberate_default_deviation, newer_than_installed_ggir,
               coerced_from_load_params_default)
  for (obj in names(split)) {
    for (nm in names(split[[obj]])) {
      if (nm %in% skipped) next
      expect_identical(split[[obj]][[nm]], lp[[obj]][[nm]], label = paste0(obj, "$", nm))
    }
  }
  expect_true(all(c(newer_than_installed_ggir, coerced_from_load_params_default) %in%
                    names(split$params_sleep)))
  expect_false(any(canhr_only %in% unlist(lapply(split, names))))
  expect_true("backup.cal.coef" %in% names(split$params_rawdata))
  expect_null(split$params_rawdata$backup.cal.coef)
})

test_that("an unknown name errors and names the argument", {
  expect_error(raw.params(chunksiz = 1), "Parameter chunksiz is unknown")
  expect_error(raw.params(windowsize = c(5, 900, 3600)), "windowsize is unknown")
  expect_error(raw.params(chunksize = 1, foo = 2, bar = 3), "Parameters foo and bar are unknown")
  # GGIR names that are deliberately not carried are unknown too
  expect_error(raw.params(printsummary = TRUE), "printsummary is unknown")
  expect_error(raw.params(do.brondcounts = FALSE), "do.brondcounts is unknown")
  expect_error(raw.params(strategy = 2), "strategy is unknown")
  # the three params_sleep members GGIR declares but never reads
  expect_error(raw.params(nnights = 7), "nnights is unknown")
  expect_error(raw.params(sleeplogsep = ","), "sleeplogsep is unknown")
  expect_error(raw.params(relyonsleeplog = TRUE), "relyonsleeplog is unknown")
  expect_identical(raw.params(anglethreshold = 7)$anglethreshold, 7)
})

test_that("unnamed and duplicated arguments error", {
  expect_error(raw.params(1), "must be named")
  expect_error(raw.params(chunksize = 1, 2), "must be named")
  expect_error(raw.params(chunksize = 1, chunksize = 0.5), "chunksize provided more than once")
})

test_that("passing NULL keeps the member and sets it to NULL", {
  p <- raw.params(dynrange = NULL, recordingEndSleepHour = NULL, configtz = NULL)
  expect_true(all(c("dynrange", "recordingEndSleepHour", "configtz") %in% names(p)))
  expect_null(p$dynrange)
  expect_length(p, 189L)
  p <- raw.params(dynrange = 8)
  expect_identical(p$dynrange, 8)
  p <- raw.params(configtz = "America/Anchorage", desiredtz = "America/Anchorage")
  expect_identical(p$configtz, "America/Anchorage")
  expect_identical(p$desiredtz, "America/Anchorage")
})

test_that("chunksize is floored at 0.1 exactly as check_params does (check_params.R:93)", {
  expect_identical(raw.params(chunksize = 0.05)$chunksize, 0.1)
  expect_identical(raw.params(chunksize = 0)$chunksize, 0.1)
  expect_identical(raw.params(chunksize = -1)$chunksize, 0.1)
  expect_identical(raw.params(chunksize = 0.1)$chunksize, 0.1)
  expect_identical(raw.params(chunksize = 0.5)$chunksize, 0.5)
  expect_identical(raw.params(chunksize = 1)$chunksize, 1)
  skip_if_no_ggir()
  for (v in c(0.05, 0, -1, 0.1, 0.099, 0.5, 1, 2)) {
    expect_identical(raw.params(chunksize = v)$chunksize,
                     ggir_check("rawdata", "chunksize", v)$params_rawdata$chunksize,
                     label = paste0("chunksize ", v))
  }
})

test_that("windowsizes c(5, 900, 3600) is untouched and emits no warning", {
  expect_silent(p <- raw.params(windowsizes = c(5, 900, 3600)))
  expect_identical(p$windowsizes, c(5, 900, 3600))
  expect_silent(p <- raw.params())
  expect_identical(p$windowsizes, c(5, 900, 3600))
  skip_if_no_ggir()
  r <- with_warnings(ggir_check("general", "windowsizes", c(5, 900, 3600)))
  expect_length(r$warnings, 0L)
  expect_identical(p$windowsizes, r$value$params_general$windowsizes)
})

test_that("windowsizes c(5, 901, 3600) is coerced to c(5, 960, 3840) with GGIR's warning text", {
  ours <- with_warnings(raw.params(windowsizes = c(5, 901, 3600)))
  expect_identical(ours$value$windowsizes, c(5, 960, 3840))
  expect_length(ours$warnings, 2L)
  expect_identical(ours$warnings[1],
                   paste0("The long windowsize needs to be a multitude of 1 minute periods.\n",
                          "Long windowsize has now been automatically adjusted to 960 seconds in order to meet this criteria."))
  expect_identical(ours$warnings[2],
                   paste0("The third value of parameter windowsizes needs to be a multitude of the second value.\n",
                          "The third value has been automatically adjusted to 3840 seconds in order to meet this criteria.\n"))
  skip_if_no_ggir()
  ggir <- with_warnings(ggir_check("general", "windowsizes", c(5, 901, 3600)))
  expect_identical(ours$value$windowsizes, ggir$value$params_general$windowsizes)
  expect_identical(ours$warnings, ggir$warnings)
})

test_that("every windowsizes coercion branch matches GGIR value for value and warning for warning", {
  skip_if_no_ggir()
  cases <- list(
    c(5, 901, 3600),    # ws2 up to 960, ws up to 3840
    c(7, 900, 3600),    # ws3 snapped to 5 (nearest of 1,5,10,15,20,30,60)
    c(5, 900, 3601),    # ws to 4500
    c(60, 900, 3600),   # untouched
    c(1, 60, 60),       # untouched
    c(13, 900, 3600),   # ws3 snapped to 15
    c(5, 30, 3600),     # ws2 to 60
    c(5, 900, 900),     # untouched
    c(5, 900, 1000),    # ws to 1800
    c(5L, 900L, 3600L), # integer input stays integer in both
    c(25, 900, 3600),   # ws3 tie between 20 and 30: GGIR returns both (length 2)
    c(7.5, 900, 3600)   # ws3 tie between 5 and 10: GGIR returns both (length 2)
  )
  for (ws in cases) {
    ours <- with_warnings(raw.params(windowsizes = ws))
    ggir <- with_warnings(ggir_check("general", "windowsizes", ws))
    expect_identical(ours$value$windowsizes, ggir$value$params_general$windowsizes,
                     label = paste0("windowsizes ", deparse(ws)))
    expect_identical(ours$warnings, ggir$warnings,
                     label = paste0("warnings for windowsizes ", deparse(ws)))
  }
  expect_identical(suppressWarnings(raw.params(windowsizes = c(7, 900, 3600))$windowsizes), c(5, 900, 3600))
  expect_identical(suppressWarnings(raw.params(windowsizes = c(5, 900, 3601))$windowsizes), c(5, 900, 4500))
  expect_identical(suppressWarnings(raw.params(windowsizes = c(13, 900, 3600))$windowsizes), c(15, 900, 3600))
  expect_identical(suppressWarnings(raw.params(windowsizes = c(5, 30, 3600))$windowsizes), c(5, 60, 3600))
  expect_identical(suppressWarnings(raw.params(windowsizes = c(5, 900, 1000))$windowsizes), c(5, 900, 1800))
  expect_identical(raw.params(windowsizes = c(5L, 900L, 3600L))$windowsizes, c(5L, 900L, 3600L))
})

test_that("frequency_tol outside [0, 1] errors with GGIR's message (check_params.R:194-197)", {
  expect_error(raw.params(frequency_tol = 1.5), "frequency_tol is 1.5")
  expect_error(raw.params(frequency_tol = -0.1), "frequency_tol is -0.1")
  expect_identical(raw.params(frequency_tol = 0)$frequency_tol, 0)
  expect_identical(raw.params(frequency_tol = 1)$frequency_tol, 1)
  expect_identical(raw.params(frequency_tol = 0.05)$frequency_tol, 0.05)
  skip_if_no_ggir()
  for (v in c(1.5, -0.1, 2)) {
    expect_identical(error_message(raw.params(frequency_tol = v)),
                     error_message(ggir_check("rawdata", "frequency_tol", v)),
                     label = paste0("frequency_tol ", v))
  }
  expect_false(is.na(error_message(raw.params(frequency_tol = 1.5))))
})

test_that("a logical rmc.noise is reset to NULL as in check_params.R:85-88", {
  p <- raw.params(rmc.noise = TRUE)
  expect_true("rmc.noise" %in% names(p))
  expect_null(p$rmc.noise)
  expect_null(raw.params(rmc.noise = FALSE)$rmc.noise)
  expect_identical(raw.params(rmc.noise = 0.013)$rmc.noise, 0.013)
  skip_if_no_ggir()
  expect_identical(raw.params(rmc.noise = TRUE)$rmc.noise,
                   ggir_check("rawdata", "rmc.noise", TRUE)$params_rawdata$rmc.noise)
  expect_identical(raw.params(rmc.noise = 0.013)$rmc.noise,
                   ggir_check("rawdata", "rmc.noise", 0.013)$params_rawdata$rmc.noise)
})

test_that("type checks error with GGIR's message for every typed member", {
  typed <- list(
    rawdata = list(
      numeric = c("chunksize", "spherecrit", "minloadcrit", "minimumFileSizeMB", "dynrange",
                  "rmc.col.acc", "interpolationType",
                  "rmc.firstrow.acc", "rmc.firstrow.header", "rmc.header.length",
                  "rmc.col.temp", "rmc.col.time",
                  "rmc.sf", "rmc.col.wear", "rmc.noise", "frequency_tol",
                  "rmc.scalefactor.acc", "nonwear_range_threshold"),
      boolean = c("do.cal", "rmc.unsignedbit", "rmc.check4timegaps", "rmc.doresample",
                  "imputeTimegaps"),
      character = c("backup.cal.coef", "rmc.dec", "rmc.unit.acc",
                    "rmc.unit.temp", "rmc.unit.time", "rmc.format.time",
                    "rmc.origin", "rmc.headername.sf",
                    "rmc.headername.sn", "rmc.headername.recordingid",
                    "rmc.header.structure")),
    general = list(
      numeric = c("windowsizes", "idloc", "dayborder"),
      boolean = character(0),
      character = c("desiredtz", "configtz", "sensor.location")),
    metrics = list(
      numeric = c("hb", "lb", "n", "zc.lb", "zc.hb", "zc.sb", "zc.order"),
      boolean = c("do.anglex", "do.angley", "do.anglez",
                  "do.zcx", "do.zcy", "do.zcz",
                  "do.enmo", "do.lfenmo", "do.en", "do.mad", "do.enmoa",
                  "do.roll_med_acc_x", "do.roll_med_acc_y", "do.roll_med_acc_z",
                  "do.dev_roll_med_acc_x", "do.dev_roll_med_acc_y", "do.dev_roll_med_acc_z",
                  "do.bfen", "do.hfen", "do.hfenplus", "do.lfen",
                  "do.lfx", "do.lfy", "do.lfz", "do.hfx", "do.hfy", "do.hfz",
                  "do.bfx", "do.bfy", "do.bfz"),
      character = character(0)),
    cleaning = list(
      numeric = c("includedaycrit", "data_masking_strategy", "maxdur", "hrs.del.start",
                  "hrs.del.end", "nonwearFiltermaxHours", "nonwearFilterWindow"),
      boolean = c("nonWearEdgeCorrection"),
      character = character(0))
  )
  wrong <- list(numeric = "abc", boolean = 1, character = 1)
  category <- c(rawdata = "Raw data", general = "general", metrics = "Metrics", cleaning = "cleaning")
  n <- 0L
  for (g in names(typed)) {
    for (cls in names(typed[[g]])) {
      for (nm in typed[[g]][[cls]]) {
        args <- list(wrong[[cls]]); names(args) <- nm
        msg <- error_message(do.call(raw.params, args))
        expect_identical(msg, paste0("\n", category[[g]], " parameter ", nm, " is not ", cls),
                         label = paste0("type error for ", nm))
        n <- n + 1L
      }
    }
  }
  expect_identical(n, 18L + 5L + 11L + 3L + 3L + 7L + 30L + 7L + 1L)
  skip_if_no_ggir()
  for (g in names(typed)) {
    for (cls in names(typed[[g]])) {
      for (nm in typed[[g]][[cls]]) {
        args <- list(wrong[[cls]]); names(args) <- nm
        expect_identical(error_message(do.call(raw.params, args)),
                         error_message(ggir_check(g, nm, wrong[[cls]])),
                         label = paste0("GGIR type error for ", nm))
      }
    }
  }
  # integer values pass the numeric check, in both
  expect_identical(raw.params(minloadcrit = 72L)$minloadcrit, 72L)
  expect_identical(raw.params(minloadcrit = 72L)$minloadcrit,
                   ggir_check("rawdata", "minloadcrit", 72L)$params_rawdata$minloadcrit)
})

test_that("members GGIR leaves untyped are accepted as GGIR accepts them", {
  # check_params has no class entry for these
  expect_identical(raw.params(actilife_LFE = TRUE)$actilife_LFE, TRUE)
  expect_identical(raw.params(nonwear_approach = "2013")$nonwear_approach, "2013")
  expect_identical(raw.params(rmc.bitrate = 12)$rmc.bitrate, 12)
  expect_identical(raw.params(rmc.dynamic_range = 8)$rmc.dynamic_range, 8)
  expect_identical(raw.params(recordingEndSleepHour = 21)$recordingEndSleepHour, 21)
})

test_that("recordingEndSleepHour below 19 errors with GGIR's message (check_params.R:534-544)", {
  expect_error(raw.params(recordingEndSleepHour = 18), "recordingEndSleepHour expects the latest time")
  expect_identical(raw.params(recordingEndSleepHour = 19)$recordingEndSleepHour, 19)
  expect_identical(raw.params(recordingEndSleepHour = 22)$recordingEndSleepHour, 22)
  skip_if_no_ggir()
  expect_identical(error_message(raw.params(recordingEndSleepHour = 18)),
                   error_message(ggir_check("general", "recordingEndSleepHour", 18)))
  expect_identical(error_message(raw.params(recordingEndSleepHour = 0)),
                   error_message(ggir_check("general", "recordingEndSleepHour", 0)))
})

test_that("cleaning cross-checks reproduce GGIR's warnings and errors (check_params.R:301-352)", {
  w <- with_warnings(raw.params(data_masking_strategy = 2, hrs.del.start = 1))
  expect_length(w$warnings, 1L)
  expect_match(w$warnings, "hrs.del.start in combination with data_masking_strategy = 2")
  w2 <- with_warnings(raw.params(data_masking_strategy = 4, hrs.del.end = 2))
  expect_match(w2$warnings, "hrs.del.end in combination with data_masking_strategy = 4")
  expect_silent(raw.params(data_masking_strategy = 1, hrs.del.start = 1))
  expect_silent(raw.params(data_masking_strategy = 3, hrs.del.start = 1))
  expect_error(raw.params(nonwearFiltermaxHours = 13), "nonwearFiltermaxHours is expected")
  expect_error(raw.params(nonwearFiltermaxHours = -1), "nonwearFiltermaxHours is expected")
  expect_error(raw.params(nonwearFiltermaxHours = 6, nonwearFilterWindow = 22),
               "nonwearFilterWindow does not have expected length of 2")
  w3 <- with_warnings(raw.params(nonwearFiltermaxHours = 6, nonwearFilterWindow = c(2, 20)))
  expect_match(w3$warnings, "probably not the night")
  expect_silent(raw.params(nonwearFiltermaxHours = 6, nonwearFilterWindow = c(22, 8)))
  # nonwearFilterWindow is only examined when nonwearFiltermaxHours is set, as in GGIR
  expect_silent(raw.params(nonwearFilterWindow = 22))
  skip_if_no_ggir()
  lp <- GGIR::load_params()
  ggir_clean <- function(...) {
    pc <- lp$params_cleaning
    mods <- list(...)
    for (nm in names(mods)) pc[[nm]] <- mods[[nm]]
    GGIR::check_params(params_cleaning = pc)
  }
  expect_identical(w$warnings, with_warnings(ggir_clean(data_masking_strategy = 2, hrs.del.start = 1))$warnings)
  expect_identical(w2$warnings, with_warnings(ggir_clean(data_masking_strategy = 4, hrs.del.end = 2))$warnings)
  expect_identical(error_message(raw.params(nonwearFiltermaxHours = 13)),
                   error_message(ggir_clean(nonwearFiltermaxHours = 13)))
  expect_identical(error_message(raw.params(nonwearFiltermaxHours = 6, nonwearFilterWindow = 22)),
                   error_message(ggir_clean(nonwearFiltermaxHours = 6, nonwearFilterWindow = 22)))
  expect_identical(w3$warnings,
                   with_warnings(ggir_clean(nonwearFiltermaxHours = 6, nonwearFilterWindow = c(2, 20)))$warnings)
})

test_that("a sweep of user values through both implementations gives identical coerced values", {
  skip_if_no_ggir()
  sweep <- list(
    list("rawdata", "chunksize", 0.02),
    list("rawdata", "spherecrit", 0.5),
    list("rawdata", "minloadcrit", 1),
    list("rawdata", "dynrange", 8),
    list("rawdata", "minimumFileSizeMB", 0),
    list("rawdata", "interpolationType", 2),
    list("rawdata", "imputeTimegaps", FALSE),
    list("rawdata", "frequency_tol", 0.5),
    list("rawdata", "nonwear_range_threshold", 200),
    list("rawdata", "rmc.firstrow.acc", 2),
    list("rawdata", "rmc.col.acc", 2:4),
    list("rawdata", "rmc.unit.acc", "mg"),
    list("rawdata", "do.cal", FALSE),
    list("general", "desiredtz", "Europe/Helsinki"),
    list("general", "configtz", "America/Anchorage"),
    list("general", "idloc", 2),
    list("general", "dayborder", 6),
    list("general", "windowsizes", c(1, 60, 3600)),
    list("metrics", "do.en", TRUE),
    list("metrics", "do.neishabouricounts", TRUE),
    list("metrics", "hb", 10),
    list("metrics", "lb", 0.5),
    list("metrics", "n", 2),
    list("metrics", "zc.order", 4),
    list("metrics", "actilife_LFE", TRUE),
    list("cleaning", "includedaycrit", 10),
    list("cleaning", "nonWearEdgeCorrection", FALSE),
    list("cleaning", "nonwear_approach", "2013"),
    list("cleaning", "maxdur", 7),
    list("cleaning", "nonwearFiltermaxHours", 6)
  )
  for (s in sweep) {
    args <- list(s[[3]]); names(args) <- s[[2]]
    ours <- suppressWarnings(do.call(raw.params, args))
    ggir <- suppressWarnings(ggir_check(s[[1]], s[[2]], s[[3]]))
    expect_identical(ours[[s[[2]]]], ggir[[paste0("params_", s[[1]])]][[s[[2]]]],
                     label = paste0(s[[1]], " ", s[[2]]))
  }
})

test_that("the canhrActi switches are validated", {
  expect_identical(raw.params(ggir_exact = FALSE)$ggir_exact, FALSE)
  expect_identical(raw.params(rename_uppercase = TRUE)$rename_uppercase, TRUE)
  expect_identical(raw.params(skip_small_files = TRUE)$skip_small_files, TRUE)
  expect_error(raw.params(ggir_exact = "yes"), "ggir_exact must be a single TRUE or FALSE")
  expect_error(raw.params(ggir_exact = NA), "ggir_exact must be a single TRUE or FALSE")
  expect_error(raw.params(ggir_exact = c(TRUE, FALSE)), "ggir_exact must be a single TRUE or FALSE")
  expect_error(raw.params(rename_uppercase = 1), "rename_uppercase must be a single TRUE or FALSE")
  expect_error(raw.params(skip_small_files = NULL), "skip_small_files must be a single TRUE or FALSE")
  expect_identical(raw.params(unzip_once = FALSE)$unzip_once, FALSE)
  expect_identical(raw.params(stream_gt3x = FALSE)$stream_gt3x, FALSE)
  expect_error(raw.params(unzip_once = "yes"), "unzip_once must be a single TRUE or FALSE")
  expect_error(raw.params(decode_once = NA), "decode_once must be a single TRUE or FALSE")
  expect_error(raw.params(stream_gt3x = 1), "stream_gt3x must be a single TRUE or FALSE")
  cb <- function(stage, i, n, message) NULL
  expect_identical(raw.params(progress = cb)$progress, cb)
  expect_null(raw.params(progress = NULL)$progress)
  expect_error(raw.params(progress = "cb"), "progress must be NULL or a function")
  expect_error(raw.params(progress = TRUE), "progress must be NULL or a function")
})

test_that("print method lists every member, groups them and marks changed values", {
  # the method is called directly so the test does not depend on NAMESPACE registration
  p <- raw.params()
  out <- capture.output(res <- print.canhrActi_raw_params(p))
  expect_identical(res, p)
  expect_true(any(grepl("canhrActi raw accelerometer parameters", out, fixed = TRUE)))
  expect_true(any(grepl("Raw data (GGIR params_rawdata)", out, fixed = TRUE)))
  expect_true(any(grepl("General (GGIR params_general)", out, fixed = TRUE)))
  expect_true(any(grepl("Metrics (GGIR params_metrics)", out, fixed = TRUE)))
  expect_true(any(grepl("Cleaning (GGIR params_cleaning)", out, fixed = TRUE)))
  expect_true(any(grepl("canhrActi switches", out, fixed = TRUE)))
  expect_true(any(grepl("All values at GGIR defaults.", out, fixed = TRUE)))
  unprinted <- names(p)[!vapply(names(p), function(nm) {
    any(grepl(paste0("  ", nm, " "), out, fixed = TRUE))
  }, logical(1))]
  expect_identical(unprinted, character(0), info = toString(unprinted))
  expect_true(any(grepl("windowsizes                  c(5, 900, 3600)", out, fixed = TRUE)))
  expect_true(any(grepl("progress                     NULL", out, fixed = TRUE)))
  expect_false(any(grepl("^\\* ", out)))

  p2 <- raw.params(desiredtz = "UTC", do.en = TRUE, progress = function(stage, i, n, message) NULL)
  out2 <- capture.output(print.canhrActi_raw_params(p2))
  expect_true(any(grepl("* desiredtz", out2, fixed = TRUE)))
  expect_true(any(grepl("* do.en", out2, fixed = TRUE)))
  expect_true(any(grepl("* progress                     <function>", out2, fixed = TRUE)))
  expect_true(any(grepl("* 3 values differ from the defaults: desiredtz, do.en, progress", out2, fixed = TRUE)))
  expect_identical(sum(grepl("^\\* ", out2)), 4L)
})

test_that("print(x) dispatches to the method once NAMESPACE registers it", {
  p <- raw.params()
  expect_identical(capture.output(print(p)), capture.output(print.canhrActi_raw_params(p)))
})
