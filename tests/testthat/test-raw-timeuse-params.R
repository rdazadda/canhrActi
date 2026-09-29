# The part-5 members of raw.params(): the 40 members across the phyact, 247, cleaning,
# output, general and sleep groups, the coercions check_params applies to them, the
# validations canhrActi adds, and ggir_version_label. No reference data are needed; live
# GGIR comparisons skip when GGIR is not installed.

skip_if_no_ggir_p5 <- function() {
  if (!requireNamespace("GGIR", quietly = TRUE)) {
    testthat::skip("GGIR is not installed; live parity comparisons skipped")
  }
}

with_warnings_p5 <- function(expr) {
  msgs <- character(0)
  val <- withCallingHandlers(expr, warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = val, warnings = msgs)
}

error_message_p5 <- function(expr) {
  tryCatch({ expr; NA_character_ }, error = function(e) conditionMessage(e))
}

# GGIR::check_params over all eight parameter objects, with the named members overridden
# wherever they live.
ggir_all_p5 <- function(...) {
  lp <- GGIR::load_params()
  mods <- list(...)
  for (nm in names(mods)) {
    found <- FALSE
    for (obj in names(lp)) {
      if (nm %in% names(lp[[obj]])) {
        found <- TRUE
        if (is.null(mods[[nm]])) lp[[obj]][nm] <- list(NULL) else lp[[obj]][[nm]] <- mods[[nm]]
      }
    }
    if (!found) stop(paste0("GGIR has no parameter called ", nm))
  }
  do.call(GGIR::check_params, lp)
}

# The same with visualreport off, for the three save_ms5raw* members that GGIR's
# visualreport block rewrites; canhrActi carries no visualreport.
ggir_all_p5_novisual <- function(...) {
  lp <- GGIR::load_params()
  lp$params_output$visualreport <- FALSE
  mods <- list(...)
  for (nm in names(mods)) {
    for (obj in names(lp)) {
      if (nm %in% names(lp[[obj]])) {
        if (is.null(mods[[nm]])) lp[[obj]][nm] <- list(NULL) else lp[[obj]][[nm]] <- mods[[nm]]
      }
    }
  }
  do.call(GGIR::check_params, lp)
}

# One flat list of GGIR's own defaults, all eight objects.
ggir_defaults_p5 <- function() {
  lp <- GGIR::load_params()
  c(lp$params_rawdata, lp$params_general, lp$params_metrics, lp$params_cleaning,
    lp$params_sleep, lp$params_output, lp$params_247, lp$params_phyact)
}

# The 40 part-5 members, by group.
new_phyact <- c("boutcriter.in", "boutcriter.lig", "boutcriter.mvpa",
                "threshold.lig", "threshold.mod", "threshold.vig",
                "boutdur.mvpa", "boutdur.in", "boutdur.lig", "frag.metrics")
new_247 <- c("qwindow", "qwindow_dateformat", "iglevels", "LUXthresholds",
             "LUX_cal_constant", "LUX_cal_exponent", "LUX_day_segments", "clevels")
new_cleaning <- c("includedaycrit.part5", "excludefirstlast.part5",
                  "minimum_MM_length.part5", "segmentWEARcrit.part5",
                  "segmentDAYSPTcrit.part5", "includenightcrit.part5")
new_output <- c("save_ms5rawlevels", "save_ms5raw_format", "save_ms5raw_without_invalid",
                "timewindow", "week_weekend_aggregate.part5", "do.sibreport",
                "require_complete_lastnight_part5", "method_research_vars")
new_general <- c("part5_agg2_60seconds")
new_sleep <- c("possible_nap_window", "possible_nap_dur", "possible_nap_gap",
               "possible_nap_edge_acc", "nap_model", "nap_markerbutton_method",
               "nap_markerbutton_max_distance")
new_part5_members <- c(new_phyact, new_247, new_cleaning, new_output, new_general,
                       new_sleep)
# GGIR declares TRUE but its visualreport default forces FALSE; canhrActi stores FALSE.
deliberate_p5_deviation <- "save_ms5raw_without_invalid"

test_that("raw.params() gains exactly the 40 part-5 members and the two new groups", {
  p <- raw.params()
  expect_length(new_part5_members, 40L)
  expect_false(any(duplicated(new_part5_members)))
  expect_true(all(new_part5_members %in% names(p)))
  # 145 before part 5, plus the 40, plus ggir_version_label, unzip_once, decode_once and
  # stream_gt3x
  expect_length(p, 189L)
  groups <- attr(p, "groups")
  expect_identical(sort(unique(unname(groups))),
                   sort(c("rawdata", "general", "metrics", "cleaning", "sleep", "output",
                          "phyact", "247", "canhrActi")))
  counts <- table(groups)
  expect_identical(as.integer(counts[c("rawdata", "general", "metrics", "cleaning",
                                       "sleep", "output", "phyact", "247", "canhrActi")]),
                   c(36L, 9L, 40L, 20L, 43L, 15L, 10L, 8L, 8L))
  expect_identical(sum(groups != "canhrActi"), 181L)
  expect_identical(names(groups)[groups == "phyact"], new_phyact)
  expect_identical(names(groups)[groups == "247"], new_247)
  for (nm in new_cleaning) expect_identical(unname(groups[nm]), "cleaning", label = nm)
  for (nm in new_output) expect_identical(unname(groups[nm]), "output", label = nm)
  for (nm in new_general) expect_identical(unname(groups[nm]), "general", label = nm)
  for (nm in new_sleep) expect_identical(unname(groups[nm]), "sleep", label = nm)
  # 43 of GGIR's 45 sleep members; the two dead names are dropped
  expect_length(names(groups)[groups == "sleep"], 43L)
})

test_that("the part-5 defaults are the values BUILD_SPEC_P5 3.1 quotes", {
  p <- raw.params()
  # phyact
  expect_identical(p$boutcriter.in, 0.9)
  expect_identical(p$boutcriter.lig, 0.8)
  expect_identical(p$boutcriter.mvpa, 0.8)
  expect_identical(p$threshold.lig, 40)
  expect_identical(p$threshold.mod, 100)
  expect_identical(p$threshold.vig, 400)
  expect_identical(p$boutdur.mvpa, c(1, 5, 10))
  expect_identical(p$boutdur.in, c(10, 20, 30))
  expect_identical(p$boutdur.lig, c(1, 5, 10))
  expect_null(p$frag.metrics)
  # 247
  expect_identical(p$qwindow, c(0, 24))
  expect_identical(p$qwindow_dateformat, "%d-%m-%Y")
  expect_null(p$iglevels)
  expect_identical(p$LUXthresholds, c(0, 100, 500, 1000, 3000, 5000, 10000))
  expect_null(p$LUX_cal_constant)
  expect_null(p$LUX_cal_exponent)
  expect_null(p$LUX_day_segments)
  expect_identical(p$clevels, c(30, 150))
  # cleaning
  expect_identical(p$includedaycrit.part5, 2/3)
  expect_identical(p$includenightcrit.part5, 0)
  expect_identical(p$excludefirstlast.part5, FALSE)
  expect_identical(p$minimum_MM_length.part5, 23)
  expect_identical(p$segmentWEARcrit.part5, 0.5)
  expect_identical(p$segmentDAYSPTcrit.part5, c(0.9, 0))
  # output
  expect_identical(p$timewindow, c("MM", "WW"))
  expect_identical(p$save_ms5rawlevels, TRUE)
  expect_identical(p$save_ms5raw_format, "RData")
  expect_identical(p$save_ms5raw_without_invalid, FALSE)
  expect_identical(p$do.sibreport, TRUE)
  expect_identical(p$week_weekend_aggregate.part5, FALSE)
  expect_identical(p$require_complete_lastnight_part5, FALSE)
  expect_null(p$method_research_vars)
  # general
  expect_identical(p$part5_agg2_60seconds, FALSE)
  # sleep, the seven nap members
  expect_null(p$possible_nap_window)
  expect_null(p$possible_nap_dur)
  expect_identical(p$possible_nap_gap, 0)
  expect_identical(p$possible_nap_edge_acc, Inf)
  expect_identical(p$nap_markerbutton_method, 0)
  expect_identical(p$nap_markerbutton_max_distance, 30)
  expect_null(p$nap_model)
})

test_that("every part-5 default is identical() to GGIR::load_params(), name by name", {
  skip_if_no_ggir_p5()
  p <- raw.params()
  g <- ggir_defaults_p5()
  compare <- setdiff(new_part5_members, deliberate_p5_deviation)
  expect_length(compare, 39L)
  unknown <- setdiff(compare, names(g))
  expect_identical(unknown, character(0), info = toString(unknown))
  n <- 0L
  for (nm in compare) {
    expect_identical(p[[nm]], g[[nm]], label = paste0("part-5 default of ", nm))
    n <- n + 1L
  }
  expect_identical(n, 39L)
  # the one deliberate deviation
  expect_identical(g[["save_ms5raw_without_invalid"]], TRUE)
  expect_identical(p[["save_ms5raw_without_invalid"]], FALSE)
  # GGIR's effective default is FALSE too, through visualreport
  ggir_coerced <- suppressWarnings(do.call(GGIR::check_params, GGIR::load_params()))
  expect_identical(ggir_coerced$params_output$save_ms5raw_without_invalid, FALSE)
  expect_identical(p[["save_ms5raw_without_invalid"]],
                   ggir_coerced$params_output$save_ms5raw_without_invalid)
})

test_that("V2: the GGIR parameter objects still have the member counts the port assumes", {
  skip_if_no_ggir_p5()
  lp <- GGIR::load_params()
  # counts for the installed 3.3.6; 3.3-9 adds HDCZA_roll_windowsize and LowAcc_threshold
  # to sleep and save_dashboard_parquet to output, which changes no part-5 number
  expect_length(lp$params_phyact, 14L)
  expect_length(lp$params_247, 26L)
  expect_length(lp$params_cleaning, 28L)
  expect_length(lp$params_output, 28L)
  expect_length(lp$params_general, 22L)
  expect_length(lp$params_sleep, 43L)
  # the members the port drops
  expect_identical(sort(setdiff(names(lp$params_phyact), new_phyact)),
                   sort(c("mvpathreshold", "boutcriter", "mvpadur", "part6_threshold_combi")))
  expect_identical(sort(setdiff(names(lp$params_247), new_247)),
                   sort(c("qlevels", "ilevels", "IVIS_windowsize_minutes",
                          "IVIS_epochsize_seconds", "IVIS.activity.metric",
                          "IVIS_acc_threshold", "qM5L5", "MX.ig.min.dur", "M5L5res",
                          "winhr", "L5M5window", "cosinor", "part6CR", "part6HCA",
                          "part6Window", "part6DFA", "SRI2_WASOmin", "part2CR")))
})

test_that("the part-5 type checks error with GGIR's exact message", {
  typed <- list(
    phyact = list(
      numeric = c("boutcriter.in", "boutcriter.lig", "boutcriter.mvpa",
                  "threshold.lig", "threshold.mod", "threshold.vig",
                  "boutdur.mvpa", "boutdur.in", "boutdur.lig"),
      boolean = character(0),
      character = c("frag.metrics")),
    `247` = list(
      numeric = c("LUXthresholds", "LUX_cal_constant", "LUX_cal_exponent",
                  "LUX_day_segments", "clevels"),
      boolean = character(0),
      character = c("qwindow_dateformat")),
    cleaning = list(
      numeric = c("includedaycrit.part5", "minimum_MM_length.part5",
                  "includenightcrit.part5"),
      boolean = c("excludefirstlast.part5"),
      character = character(0)),
    output = list(
      numeric = character(0),
      boolean = c("save_ms5rawlevels", "save_ms5raw_without_invalid",
                  "week_weekend_aggregate.part5", "do.sibreport",
                  "require_complete_lastnight_part5"),
      character = c("save_ms5raw_format", "timewindow", "method_research_vars")),
    general = list(
      numeric = character(0),
      boolean = c("part5_agg2_60seconds"),
      character = character(0)),
    Sleep = list(
      numeric = c("possible_nap_window", "possible_nap_dur", "possible_nap_gap",
                  "possible_nap_edge_acc", "nap_markerbutton_method",
                  "nap_markerbutton_max_distance"),
      boolean = character(0),
      character = c("nap_model"))
  )
  # wrong-class values that pass the length and range guards ahead of the class check
  wrong <- list(numeric = "abc", boolean = 1, character = 1)
  n <- 0L
  for (g in names(typed)) {
    for (cls in names(typed[[g]])) {
      for (nm in typed[[g]][[cls]]) {
        args <- list(wrong[[cls]]); names(args) <- nm
        msg <- error_message_p5(suppressWarnings(do.call(raw.params, args)))
        expect_identical(msg, paste0("\n", g, " parameter ", nm, " is not ", cls),
                         label = paste0("type error for ", nm))
        n <- n + 1L
      }
    }
  }
  # 10 phyact + 6 247 + 4 cleaning + 8 output + 1 general + 7 sleep
  expect_identical(n, 36L)
  skip_if_no_ggir_p5()
  for (g in names(typed)) {
    for (cls in names(typed[[g]])) {
      for (nm in typed[[g]][[cls]]) {
        args <- list(wrong[[cls]]); names(args) <- nm
        expect_identical(error_message_p5(suppressWarnings(do.call(raw.params, args))),
                         error_message_p5(suppressWarnings(
                           do.call(ggir_all_p5, stats::setNames(list(wrong[[cls]]), nm)))),
                         label = paste0("GGIR type error for ", nm))
      }
    }
  }
})

test_that("the part-5 members GGIR leaves untyped are accepted as GGIR accepts them", {
  # qwindow and iglevels may be numeric or character; the two segment criteria are untyped too
  expect_identical(raw.params(qwindow = "C:/diary.csv")$qwindow, "C:/diary.csv")
  expect_identical(raw.params(qwindow = c(0, 8, 24))$qwindow, c(0, 8, 24))
  expect_identical(raw.params(iglevels = c(0, 50, 100))$iglevels, c(0, 50, 100))
  expect_identical(raw.params(segmentWEARcrit.part5 = 0.25)$segmentWEARcrit.part5, 0.25)
  expect_identical(raw.params(segmentDAYSPTcrit.part5 = c(0, 1))$segmentDAYSPTcrit.part5,
                   c(0, 1))
  expect_identical(raw.params(frag.metrics = c("mean", "TP"))$frag.metrics,
                   c("mean", "TP"))
  skip_if_no_ggir_p5()
  for (v in list(list(qwindow = "C:/diary.csv"), list(qwindow = c(0, 8, 24)),
                 list(iglevels = c(0, 50, 100)), list(segmentWEARcrit.part5 = 0.25),
                 list(segmentDAYSPTcrit.part5 = c(0, 1)))) {
    nm <- names(v)
    expect_identical(do.call(raw.params, v)[[nm]],
                     unlist(unname(lapply(do.call(ggir_all_p5, v),
                                          function(o) o[[nm]])), use.names = FALSE),
                     label = paste0("untyped ", nm))
  }
})

test_that("iglevels of length 1 expands to the 162 standard edges (check_params.R:423-427)", {
  expanded <- c(seq(0, 4000, by = 25), 8000)
  expect_length(expanded, 162L)
  expect_identical(raw.params(iglevels = 1)$iglevels, expanded)
  expect_identical(raw.params(iglevels = TRUE)$iglevels, expanded)
  expect_identical(raw.params(iglevels = 999)$iglevels, expanded)
  # length 0 and length > 1 are left alone
  expect_null(raw.params(iglevels = c())$iglevels)
  expect_identical(raw.params(iglevels = c(0, 25))$iglevels, c(0, 25))
  skip_if_no_ggir_p5()
  for (v in list(1, TRUE, 999, c(0, 25), c(0, 50, 100))) {
    expect_identical(raw.params(iglevels = v)$iglevels,
                     ggir_all_p5(iglevels = v)$params_247$iglevels,
                     label = paste0("iglevels ", paste(v, collapse = ",")))
  }
})

test_that("a character qwindow has its backslashes converted (check_params.R:428-433)", {
  expect_identical(raw.params(qwindow = "C:\\data\\q.csv")$qwindow, "C:/data/q.csv")
  expect_identical(raw.params(qwindow = c(0, 24))$qwindow, c(0, 24))
  skip_if_no_ggir_p5()
  for (v in list("C:\\data\\q.csv", "C:/data/q.csv", c(0, 24), c(0, 8, 24))) {
    expect_identical(raw.params(qwindow = v)$qwindow,
                     ggir_all_p5(qwindow = v)$params_247$qwindow,
                     label = paste0("qwindow ", paste(v, collapse = ",")))
  }
})

test_that("LUX_day_segments is rounded, uniqued, sorted and bracketed by 0 and 24", {
  expect_identical(raw.params(LUX_day_segments = c(18.4, 9.6))$LUX_day_segments,
                   c(0, 10, 18, 24))
  expect_identical(raw.params(LUX_day_segments = c(0, 12, 24))$LUX_day_segments,
                   c(0, 12, 24))
  expect_identical(raw.params(LUX_day_segments = c(12, 12, 6))$LUX_day_segments,
                   c(0, 6, 12, 24))
  expect_null(raw.params(LUX_day_segments = c())$LUX_day_segments)
  skip_if_no_ggir_p5()
  for (v in list(c(18.4, 9.6), c(0, 12, 24), c(12, 12, 6), c(0, 24), 8)) {
    expect_identical(raw.params(LUX_day_segments = v)$LUX_day_segments,
                     ggir_all_p5(LUX_day_segments = v)$params_247$LUX_day_segments,
                     label = paste0("LUX_day_segments ", paste(v, collapse = ",")))
  }
})

test_that("clevels of length 1 warns with GGIR's text (check_params.R:453-455)", {
  w <- with_warnings_p5(raw.params(clevels = 30))
  expect_length(w$warnings, 1L)
  expect_identical(w$warnings,
                   "\nParameter clevels expects a number vector of at least 2 values, current length is 1")
  expect_identical(w$value$clevels, 30)
  expect_silent(raw.params(clevels = c(30, 150)))
  expect_silent(raw.params(clevels = NULL))
  skip_if_no_ggir_p5()
  expect_identical(w$warnings, with_warnings_p5(ggir_all_p5(clevels = 30))$warnings)
})

test_that("save_ms5raw_format drops unknown formats, appends RData to a lone csv, or stops", {
  expect_identical(raw.params(save_ms5raw_format = "RData")$save_ms5raw_format, "RData")
  expect_identical(raw.params(save_ms5raw_format = c("RData", "csv"))$save_ms5raw_format,
                   c("RData", "csv"))
  expect_identical(raw.params(save_ms5raw_format = "csv")$save_ms5raw_format,
                   c("csv", "RData"))
  # unknown values are dropped first
  expect_identical(raw.params(save_ms5raw_format = c("csv", "xyz"))$save_ms5raw_format,
                   c("csv", "RData"))
  expect_identical(raw.params(save_ms5raw_format = c("RData", "xyz"))$save_ms5raw_format,
                   "RData")
  expect_error(raw.params(save_ms5raw_format = "xyz"),
               "Parameter save_ms5raw_format incorrectly specified, please fix.",
               fixed = TRUE)
  skip_if_no_ggir_p5()
  for (v in list("RData", c("RData", "csv"), "csv", c("csv", "xyz"), c("RData", "xyz"),
                 c("csv", "csv"))) {
    expect_identical(raw.params(save_ms5raw_format = v)$save_ms5raw_format,
                     ggir_all_p5_novisual(save_ms5raw_format = v)$params_output$save_ms5raw_format,
                     label = paste0("save_ms5raw_format ", paste(v, collapse = ",")))
  }
  expect_identical(error_message_p5(raw.params(save_ms5raw_format = "xyz")),
                   error_message_p5(ggir_all_p5(save_ms5raw_format = "xyz")))
  # with visualreport on, GGIR appends RData a second time to c("csv", "csv")
  expect_identical(suppressWarnings(
    ggir_all_p5(save_ms5raw_format = c("csv", "csv")))$params_output$save_ms5raw_format,
    c("csv", "RData"))
  expect_identical(raw.params(save_ms5raw_format = c("csv", "csv"))$save_ms5raw_format,
                   c("csv", "csv"))
})

test_that("includedaycrit.part5 and includenightcrit.part5 stop below 0 and above 24", {
  # a fraction in [0, 1] or an hour count in (1, 24]
  for (v in c(0, 0.5, 2/3, 1, 16, 24)) {
    expect_identical(raw.params(includedaycrit.part5 = v)$includedaycrit.part5, v,
                     label = paste0("includedaycrit.part5 ", v))
    expect_identical(raw.params(includenightcrit.part5 = v)$includenightcrit.part5, v,
                     label = paste0("includenightcrit.part5 ", v))
  }
  expect_error(raw.params(includedaycrit.part5 = -0.1),
               "Negative value of includedaycrit.part5 is not allowed")
  expect_error(raw.params(includedaycrit.part5 = 24.5),
               "Incorrect value of includedaycrit.part5")
  expect_error(raw.params(includedaycrit.part5 = 26),
               "Incorrect value of includedaycrit.part5")
  expect_error(raw.params(includenightcrit.part5 = -1),
               "Negative value of includenightcrit.part5 is not allowed")
  expect_error(raw.params(includenightcrit.part5 = 25),
               "Incorrect value of includenightcrit.part5")
  # NULL fails the < 0 comparison first, with R's own message, as in GGIR
  expect_identical(error_message_p5(raw.params(includedaycrit.part5 = NULL)),
                   "argument is of length zero")
  skip_if_no_ggir_p5()
  for (v in list(-0.1, 0, 0.5, 1, 24, 24.5, 25, 26, NULL)) {
    expect_identical(error_message_p5(raw.params(includedaycrit.part5 = v)),
                     error_message_p5(ggir_all_p5(includedaycrit.part5 = v)),
                     label = paste0("includedaycrit.part5 ", paste(v, collapse = "")))
  }
  for (v in list(-1, 0, 0.5, 24, 25)) {
    expect_identical(error_message_p5(raw.params(includenightcrit.part5 = v)),
                     error_message_p5(ggir_all_p5(includenightcrit.part5 = v)),
                     label = paste0("includenightcrit.part5 ", v))
  }
  # 26 stops on the > 24 test, so the message is the fraction-of-the-day wording
  expect_match(error_message_p5(raw.params(includedaycrit.part5 = 26)),
               "this should be a fraction of the day", fixed = TRUE)
})

test_that("segmentWEARcrit.part5 and segmentDAYSPTcrit.part5 reproduce :564-595", {
  w <- with_warnings_p5(raw.params(segmentWEARcrit.part5 = NULL))
  expect_identical(w$value$segmentWEARcrit.part5, 0.5)
  expect_length(w$warnings, 1L)
  expect_match(w$warnings, "the default value has been assigned", fixed = TRUE)
  expect_identical(raw.params(segmentWEARcrit.part5 = 0)$segmentWEARcrit.part5, 0)
  expect_identical(raw.params(segmentWEARcrit.part5 = 1)$segmentWEARcrit.part5, 1)
  expect_error(raw.params(segmentWEARcrit.part5 = 1.2),
               "Incorrect value of segmentWEARcrit.part5")
  expect_error(raw.params(segmentWEARcrit.part5 = -0.1),
               "Incorrect value of segmentWEARcrit.part5")
  expect_error(raw.params(segmentDAYSPTcrit.part5 = 0.9),
               "expected to be a numeric vector of length 2")
  expect_error(raw.params(segmentDAYSPTcrit.part5 = c(0.5, 0.5, 0.5)),
               "expected to be a numeric vector of length 2")
  expect_error(raw.params(segmentDAYSPTcrit.part5 = c(0.9, 1.1)),
               "Incorrect values of segmentDAYSPTcrit.part5")
  expect_error(raw.params(segmentDAYSPTcrit.part5 = c(-0.1, 0)),
               "Incorrect values of segmentDAYSPTcrit.part5")
  expect_identical(raw.params(segmentDAYSPTcrit.part5 = c(0, 1))$segmentDAYSPTcrit.part5,
                   c(0, 1))
  skip_if_no_ggir_p5()
  expect_identical(w$warnings,
                   with_warnings_p5(ggir_all_p5(segmentWEARcrit.part5 = NULL))$warnings)
  expect_identical(w$value$segmentWEARcrit.part5,
                   suppressWarnings(
                     ggir_all_p5(segmentWEARcrit.part5 = NULL))$params_cleaning$segmentWEARcrit.part5)
  for (v in list(0, 0.5, 1, 1.2, -0.1)) {
    expect_identical(error_message_p5(raw.params(segmentWEARcrit.part5 = v)),
                     error_message_p5(ggir_all_p5(segmentWEARcrit.part5 = v)),
                     label = paste0("segmentWEARcrit.part5 ", v))
  }
  for (v in list(0.9, c(0.5, 0.5, 0.5), c(0.9, 1.1), c(-0.1, 0), c(0, 1))) {
    expect_identical(error_message_p5(raw.params(segmentDAYSPTcrit.part5 = v)),
                     error_message_p5(ggir_all_p5(segmentDAYSPTcrit.part5 = v)),
                     label = paste0("segmentDAYSPTcrit.part5 ", paste(v, collapse = ",")))
  }
})

test_that("the three nap length guards stop with GGIR's message (check_params.R:211-224)", {
  expect_error(raw.params(possible_nap_gap = NULL),
               "Parameter possible_nap_gap has length 0 while length 1 is expected",
               fixed = TRUE)
  expect_error(raw.params(possible_nap_gap = c(1, 2)),
               "Parameter possible_nap_gap has length 2 while length 1 is expected",
               fixed = TRUE)
  expect_error(raw.params(possible_nap_window = 9),
               "Parameter possible_nap_window has length 1 while length 2 is expected",
               fixed = TRUE)
  expect_error(raw.params(possible_nap_dur = c(1, 2, 3)),
               "Parameter possible_nap_dur has length 3 while length 2 is expected",
               fixed = TRUE)
  # a NULL window or duration keeps nap detection off
  expect_null(raw.params(possible_nap_window = NULL)$possible_nap_window)
  expect_null(raw.params(possible_nap_dur = NULL)$possible_nap_dur)
  expect_identical(raw.params(possible_nap_window = c(9, 18))$possible_nap_window,
                   c(9, 18))
  expect_identical(raw.params(possible_nap_gap = 5)$possible_nap_gap, 5)
  skip_if_no_ggir_p5()
  for (v in list(list(possible_nap_gap = NULL), list(possible_nap_gap = c(1, 2)),
                 list(possible_nap_window = 9), list(possible_nap_dur = c(1, 2, 3)),
                 list(possible_nap_window = c(9, 18)), list(possible_nap_gap = 5))) {
    expect_identical(error_message_p5(do.call(raw.params, v)),
                     error_message_p5(do.call(ggir_all_p5, v)),
                     label = paste0("nap guard ", names(v)))
  }
})

test_that("a nap window with a nap duration rewrites the four output members (:225-232)", {
  p <- raw.params(possible_nap_window = c(9, 18), possible_nap_dur = c(15, 120))
  expect_identical(p$do.sibreport, TRUE)
  expect_identical(p$save_ms5raw_format, "RData")
  expect_identical(p$save_ms5rawlevels, TRUE)
  expect_identical(p$save_ms5raw_without_invalid, FALSE)
  # it overrides explicit contrary values
  p2 <- raw.params(possible_nap_window = c(9, 18), possible_nap_dur = c(15, 120),
                   do.sibreport = FALSE, save_ms5rawlevels = FALSE,
                   save_ms5raw_without_invalid = TRUE, save_ms5raw_format = "csv")
  expect_identical(p2$do.sibreport, TRUE)
  expect_identical(p2$save_ms5rawlevels, TRUE)
  expect_identical(p2$save_ms5raw_without_invalid, FALSE)
  # the nap block appends RData; the lone-csv rule does not add it again
  expect_identical(p2$save_ms5raw_format, c("csv", "RData"))
  # with only one of the two set, nothing is rewritten
  p3 <- raw.params(possible_nap_window = c(9, 18), do.sibreport = FALSE)
  expect_identical(p3$do.sibreport, FALSE)
  skip_if_no_ggir_p5()
  g <- suppressWarnings(ggir_all_p5(possible_nap_window = c(9, 18),
                                    possible_nap_dur = c(15, 120)))
  for (nm in c("do.sibreport", "save_ms5raw_format", "save_ms5rawlevels",
               "save_ms5raw_without_invalid")) {
    expect_identical(p[[nm]], g$params_output[[nm]], label = paste0("nap to output ", nm))
  }
  # visualreport off, or GGIR's visualreport block would set the same four members
  lp <- GGIR::load_params()
  lp$params_output$visualreport <- FALSE
  lp$params_output$do.sibreport <- FALSE
  lp$params_output$save_ms5rawlevels <- FALSE
  lp$params_output$save_ms5raw_without_invalid <- TRUE
  lp$params_output$save_ms5raw_format <- "csv"
  lp$params_sleep$possible_nap_window <- c(9, 18)
  lp$params_sleep$possible_nap_dur <- c(15, 120)
  g2 <- suppressWarnings(do.call(GGIR::check_params, lp))
  for (nm in c("do.sibreport", "save_ms5raw_format", "save_ms5rawlevels",
               "save_ms5raw_without_invalid")) {
    expect_identical(p2[[nm]], g2$params_output[[nm]],
                     label = paste0("nap to output, contrary values, ", nm))
  }
})

test_that("a sweep of part-5 user values through both implementations agrees", {
  skip_if_no_ggir_p5()
  sweep <- list(
    list("phyact", "threshold.lig", 18), list("phyact", "threshold.mod", 60),
    list("phyact", "threshold.vig", 428.8), list("phyact", "boutcriter.in", 0.5),
    list("phyact", "boutcriter.lig", 1), list("phyact", "boutcriter.mvpa", 0.9),
    list("phyact", "boutdur.in", c(5, 10)), list("phyact", "boutdur.lig", 10),
    list("phyact", "boutdur.mvpa", c(1, 2, 5, 10)),
    list("phyact", "frag.metrics", "all"),
    list("phyact", "frag.metrics", c("mean", "TP", "Gini")),
    list("247", "qwindow", c(0, 8, 24)), list("247", "qwindow_dateformat", "%Y-%m-%d"),
    list("247", "iglevels", 1), list("247", "LUXthresholds", c(0, 100)),
    list("247", "LUX_cal_constant", 1.3), list("247", "LUX_cal_exponent", 0.002),
    list("247", "LUX_day_segments", c(0, 9, 18, 24)), list("247", "clevels", c(30, 100)),
    list("cleaning", "includedaycrit.part5", 16),
    list("cleaning", "includenightcrit.part5", 0.5),
    list("cleaning", "excludefirstlast.part5", TRUE),
    list("cleaning", "minimum_MM_length.part5", 20),
    list("cleaning", "segmentWEARcrit.part5", 0.8),
    list("cleaning", "segmentDAYSPTcrit.part5", c(0.5, 0.5)),
    list("output", "timewindow", "MM"), list("output", "timewindow", c("MM", "WW", "OO")),
    list("output", "save_ms5rawlevels", FALSE),
    list("output", "save_ms5raw_format", c("RData", "csv")),
    list("output", "do.sibreport", FALSE),
    list("output", "week_weekend_aggregate.part5", TRUE),
    list("output", "require_complete_lastnight_part5", TRUE),
    list("output", "method_research_vars", "nap"),
    list("general", "part5_agg2_60seconds", TRUE),
    list("sleep", "possible_nap_gap", 15),
    list("sleep", "possible_nap_edge_acc", 30),
    list("sleep", "nap_markerbutton_method", 1),
    list("sleep", "nap_markerbutton_max_distance", 45)
  )
  # the three members GGIR's visualreport block rewrites need the visualreport-off reference
  visual_touched <- c("save_ms5rawlevels", "save_ms5raw_format",
                      "save_ms5raw_without_invalid")
  n <- 0L
  for (s in sweep) {
    args <- stats::setNames(list(s[[3]]), s[[2]])
    ours <- suppressWarnings(do.call(raw.params, args))
    ggir <- suppressWarnings(do.call(
      if (s[[2]] %in% visual_touched) ggir_all_p5_novisual else ggir_all_p5, args))
    expect_identical(ours[[s[[2]]]], ggir[[paste0("params_", s[[1]])]][[s[[2]]]],
                     label = paste0(s[[1]], " ", s[[2]], " ",
                                    paste(s[[3]], collapse = ",")))
    n <- n + 1L
  }
  expect_identical(n, 38L)
})

test_that("the checker does NOT sort the bout durations, because raw.timeuse() does", {
  # g.part5 sorts the bout durations decreasing itself; check_params does not
  p <- raw.params(boutdur.in = c(10, 30, 20), boutdur.lig = c(5, 1, 10),
                  boutdur.mvpa = c(1, 10, 5))
  expect_identical(p$boutdur.in, c(10, 30, 20))
  expect_identical(p$boutdur.lig, c(5, 1, 10))
  expect_identical(p$boutdur.mvpa, c(1, 10, 5))
  d <- raw.params()
  expect_identical(d$boutdur.in, c(10, 20, 30))
  expect_identical(d$boutdur.mvpa, c(1, 5, 10))
  expect_identical(sort(d$boutdur.in, decreasing = TRUE), c(30, 20, 10))
  skip_if_no_ggir_p5()
  for (nm in c("boutdur.in", "boutdur.lig", "boutdur.mvpa")) {
    g <- suppressWarnings(do.call(ggir_all_p5,
                                  stats::setNames(list(c(10, 30, 20)), nm)))
    expect_identical(g$params_phyact[[nm]], c(10, 30, 20),
                     label = paste0("check_params leaves ", nm, " unsorted"))
    expect_identical(do.call(raw.params, stats::setNames(list(c(10, 30, 20)), nm))[[nm]],
                     g$params_phyact[[nm]], label = paste0("unsorted ", nm))
  }
})

test_that("raw.params() with the part-5 members is still a fixed point of the coercions", {
  p <- raw.params()
  expect_identical(do.call(raw.params, unclass(p)), p)
  p2 <- raw.params(threshold.lig = c(18, 40), threshold.mod = c(60, 100),
                   timewindow = c("MM", "WW", "OO"), frag.metrics = "all",
                   iglevels = 1, LUX_day_segments = c(18.4, 9.6))
  expect_identical(do.call(raw.params, unclass(p2)), p2)
  expect_identical(p2$iglevels, c(seq(0, 4000, by = 25), 8000))
  expect_identical(p2$LUX_day_segments, c(0, 10, 18, 24))
})

test_that("timewindow is whitelisted, which GGIR does not do", {
  expect_identical(raw.params(timewindow = "MM")$timewindow, "MM")
  expect_identical(raw.params(timewindow = c("MM", "WW", "OO"))$timewindow,
                   c("MM", "WW", "OO"))
  expect_identical(raw.params(timewindow = "OO")$timewindow, "OO")
  expect_error(raw.params(timewindow = "ZZ"),
               "Parameter timewindow may only hold \"MM\", \"WW\" and \"OO\", not \"ZZ\"",
               fixed = TRUE)
  expect_error(raw.params(timewindow = c("MM", "XX")), "not \"XX\"", fixed = TRUE)
  expect_error(raw.params(timewindow = c("XX", "YY")), "not \"XX\" or \"YY\"", fixed = TRUE)
  expect_error(raw.params(timewindow = character(0)),
               "must name at least one of", fixed = TRUE)
  expect_error(raw.params(timewindow = NULL), "must name at least one of", fixed = TRUE)
  skip_if_no_ggir_p5()
  # GGIR accepts "ZZ" silently
  expect_identical(suppressWarnings(ggir_all_p5(timewindow = "ZZ"))$params_output$timewindow,
                   "ZZ")
  expect_true(is.na(error_message_p5(suppressWarnings(ggir_all_p5(timewindow = "ZZ")))))
})

test_that("the three thresholds must be ordered, which GGIR does not check", {
  expect_identical(raw.params(threshold.lig = 18, threshold.mod = 60)$threshold.lig, 18)
  expect_identical(raw.params(threshold.lig = c(18, 40),
                              threshold.mod = c(60, 100))$threshold.mod, c(60, 100))
  # equal boundaries are allowed
  expect_identical(raw.params(threshold.lig = 100)$threshold.lig, 100)
  expect_error(raw.params(threshold.lig = 200), "threshold.lig <= threshold.mod",
               fixed = TRUE)
  expect_error(raw.params(threshold.mod = 500), "threshold.mod", fixed = TRUE)
  # every element is checked
  expect_error(raw.params(threshold.lig = c(30, 500)), "30 100 400|500 100 400")
  expect_error(raw.params(threshold.vig = c(400, 50)), "100 50")
  skip_if_no_ggir_p5()
  expect_true(is.na(error_message_p5(suppressWarnings(ggir_all_p5(threshold.lig = 200)))))
})

test_that("boutcriter.* must be a single fraction in (0, 1], which GGIR does not check", {
  for (nm in c("boutcriter.in", "boutcriter.lig", "boutcriter.mvpa")) {
    expect_identical(do.call(raw.params, stats::setNames(list(0.5), nm))[[nm]], 0.5,
                     label = paste0(nm, " 0.5"))
    expect_identical(do.call(raw.params, stats::setNames(list(1), nm))[[nm]], 1,
                     label = paste0(nm, " 1"))
    for (bad in list(0, -0.1, 2, c(0.8, 0.9), NA_real_)) {
      expect_error(do.call(raw.params, stats::setNames(list(bad), nm)),
                   paste0("Parameter ", nm, " must be a single fraction"),
                   fixed = TRUE, label = paste0(nm, " ", paste(bad, collapse = ",")))
    }
  }
  skip_if_no_ggir_p5()
  expect_true(is.na(error_message_p5(suppressWarnings(ggir_all_p5(boutcriter.mvpa = 2)))))
  expect_identical(suppressWarnings(ggir_all_p5(boutcriter.mvpa = 2))$params_phyact$boutcriter.mvpa, 2)
})

test_that("ggir_version_label defaults to the targeted GGIR version and is validated", {
  expect_identical(raw.params()$ggir_version_label, "3.3.6")
  expect_identical(raw.params(ggir_version_label = "3.3.9")$ggir_version_label, "3.3.9")
  expect_error(raw.params(ggir_version_label = 3.36),
               "ggir_version_label must be a single character string")
  expect_error(raw.params(ggir_version_label = c("3.3.6", "3.3.9")),
               "ggir_version_label must be a single character string")
  expect_error(raw.params(ggir_version_label = NA_character_),
               "ggir_version_label must be a single character string")
  expect_error(raw.params(ggir_version_label = NULL),
               "ggir_version_label must be a single character string")
  expect_identical(unname(attr(raw.params(), "groups")[["ggir_version_label"]]),
                   "canhrActi")
  expect_false("ggir_version_label" %in%
                 names(unlist(.raw.params.ggir(raw.params()), recursive = FALSE)))
  skip_if_no_ggir_p5()
  # package_version turns 3.3-9 into "3.3.9", which is what the GGIRversion column holds
  expect_identical(as.character(package_version("3.3-9")), "3.3.9")
  expect_identical(as.character(utils::packageVersion("GGIR")),
                   raw.params(ggir_version_label =
                                as.character(utils::packageVersion("GGIR")))$ggir_version_label)
})

test_that("the part-5 members GGIR has and canhrActi drops are unknown names", {
  dropped <- c("mvpathreshold", "boutcriter", "mvpadur", "part6_threshold_combi",
               "visualreport", "part6CR", "part6HCA", "qlevels", "ilevels", "winhr",
               "M5L5res", "MX.ig.min.dur", "qM5L5", "L5M5window", "cosinor",
               "part6Window", "part6DFA", "SRI2_WASOmin", "part2CR", "ndayswindow",
               "do.imp", "TimeSegments2ZeroFile", "max_calendar_days",
               "includecrit.part6", "study_dates_file", "study_dates_dateformat",
               "epochvalues2csv", "viewingwindow", "dofirstpage",
               "visualreport_without_invalid", "old_visualreport",
               "visualreport_hrsPerRow", "visualreport_focus", "visualreport_validcrit",
               "save_dashboard_parquet", "sep_config", "dec_config")
  for (nm in dropped) {
    expect_error(do.call(raw.params, stats::setNames(list(1), nm)),
                 paste0("Parameter ", nm, " is unknown to raw.params()"),
                 fixed = TRUE, label = paste0(nm, " is unknown"))
  }
  expect_length(dropped, 37L)
  # the two dead sleep names are still unknown, and the seven nap ones are not
  expect_error(raw.params(nnights = 7), "nnights is unknown")
  expect_error(raw.params(sleeplogsep = ","), "sleeplogsep is unknown")
  expect_identical(raw.params(possible_nap_gap = 3)$possible_nap_gap, 3)
  skip_if_no_ggir_p5()
  # every dropped name is a real GGIR parameter
  g <- ggir_defaults_p5()
  lp <- GGIR::load_params()
  known <- c(names(g), "save_dashboard_parquet") # 3.3-9 only, absent from installed 3.3.6
  expect_true(all(dropped %in% known))
})

test_that(".raw.params.ggir() emits params_247 and params_phyact", {
  split <- .raw.params.ggir(raw.params())
  expect_identical(names(split), c("params_rawdata", "params_general", "params_metrics",
                                   "params_cleaning", "params_sleep", "params_output",
                                   "params_247", "params_phyact"))
  expect_identical(names(split$params_phyact), new_phyact)
  expect_identical(names(split$params_247), new_247)
  expect_length(split$params_cleaning, 20L)
  expect_length(split$params_output, 15L)
  expect_length(split$params_sleep, 43L)
  expect_length(split$params_general, 9L)
  skip_if_no_ggir_p5()
  lp <- GGIR::load_params()
  for (obj in c("params_247", "params_phyact")) {
    for (nm in names(split[[obj]])) {
      expect_identical(split[[obj]][[nm]], lp[[obj]][[nm]],
                       label = paste0(obj, "$", nm))
    }
  }
  # merged over GGIR's own objects, since check_params reads cosinor, which is not carried
  full247 <- lp$params_247
  for (nm in names(split$params_247)) full247[nm] <- list(split$params_247[[nm]])
  fullphy <- lp$params_phyact
  for (nm in names(split$params_phyact)) fullphy[nm] <- list(split$params_phyact[[nm]])
  back <- suppressWarnings(GGIR::check_params(params_247 = full247,
                                              params_phyact = fullphy))
  for (nm in new_247) {
    expect_identical(back$params_247[[nm]], split$params_247[[nm]],
                     label = paste0("round trip 247$", nm))
  }
  for (nm in new_phyact) {
    expect_identical(back$params_phyact[[nm]], split$params_phyact[[nm]],
                     label = paste0("round trip phyact$", nm))
  }
})

test_that("the print method labels the two new groups and lists every new member", {
  p <- raw.params()
  out <- capture.output(res <- print.canhrActi_raw_params(p))
  expect_identical(res, p)
  expect_true(any(grepl("Physical activity (GGIR params_phyact)", out, fixed = TRUE)))
  expect_true(any(grepl("Round the clock (GGIR params_247)", out, fixed = TRUE)))
  unprinted <- new_part5_members[!vapply(new_part5_members, function(nm) {
    any(grepl(paste0("  ", nm, " "), out, fixed = TRUE))
  }, logical(1))]
  expect_identical(unprinted, character(0), info = toString(unprinted))
  label_line <- grep("ggir_version_label", out, fixed = TRUE, value = TRUE)
  expect_length(label_line, 1L)
  expect_match(label_line, "\"3.3.6\"", fixed = TRUE)
  expect_true(any(grepl("All values at GGIR defaults.", out, fixed = TRUE)))
  out2 <- capture.output(print.canhrActi_raw_params(raw.params(threshold.lig = 18,
                                                               threshold.mod = 60)))
  expect_true(any(grepl("* threshold.lig", out2, fixed = TRUE)))
  expect_true(any(grepl("* threshold.mod", out2, fixed = TRUE)))
  expect_true(any(grepl("* 2 values differ from the defaults: threshold.lig, threshold.mod",
                        out2, fixed = TRUE)))
})

test_that("the part-four merge helper is now .raw.nights.merge.part5", {
  ns <- asNamespace("canhrActi")
  expect_true(exists(".raw.nights.merge.part5", envir = ns, inherits = FALSE))
  expect_false(exists(".raw.report.part5", envir = ns, inherits = FALSE))
  expect_true(is.function(get(".raw.nights.merge.part5", envir = ns)))
  expect_null(.raw.nights.merge.part5(NULL))
  expect_null(.raw.nights.merge.part5(list()))
  p5 <- data.frame(ID = c("a", "a"), window = c("WW", "MM"), night_number = c(1, 1),
                   nonwear_perc_spt = c(1.5, 2.5), ACC_spt_mg = c(10, 20),
                   stringsAsFactors = FALSE)
  got <- .raw.nights.merge.part5(p5)
  expect_identical(colnames(got),
                   c("ID", "window", "night", "nonwear_perc_spt", "ACC_spt_mg"))
  expect_identical(nrow(got), 1L)
})
