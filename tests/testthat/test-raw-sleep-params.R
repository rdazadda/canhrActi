# The sleep members of raw.params(): the carried members of GGIR's params_sleep, the 14
# non-sleep members parts 3 and 4 read, and the coercions check_params applies to them. No
# reference data are needed; live GGIR comparisons skip when GGIR is not installed.

skip_if_no_ggir_sleep <- function() {
  if (!requireNamespace("GGIR", quietly = TRUE)) {
    testthat::skip("GGIR is not installed; live parity comparisons skipped")
  }
}

# GGIR::check_params over all of GGIR's parameter objects, with the named members overridden
# wherever they live.
ggir_full <- function(...) {
  lp <- GGIR::load_params()
  mods <- list(...)
  for (nm in names(mods)) {
    hit <- FALSE
    for (obj in names(lp)) {
      if (nm %in% names(lp[[obj]])) {
        if (is.null(mods[[nm]])) lp[[obj]][nm] <- list(NULL) else lp[[obj]][[nm]] <- mods[[nm]]
        hit <- TRUE
      }
    }
    if (!hit) stop("GGIR does not know the parameter ", nm)
  }
  do.call(GGIR::check_params, lp)
}

# GGIR's whole default parameter set as one flat list.
ggir_all_defaults <- function() {
  lp <- GGIR::load_params()
  do.call(c, unname(lp))
}

with_warnings_sleep <- function(expr) {
  msgs <- character(0)
  val <- withCallingHandlers(expr, warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = val, warnings = msgs)
}

error_message_sleep <- function(expr) {
  tryCatch({ expr; NA_character_ }, error = function(e) conditionMessage(e))
}

# The params_sleep names canhrActi does not carry.
dead_sleep_members <- c("nnights", "sleeplogsep", "relyonsleeplog")
nap_sleep_members <- c("possible_nap_window", "possible_nap_dur", "possible_nap_gap",
                       "possible_nap_edge_acc", "nap_model", "nap_markerbutton_method",
                       "nap_markerbutton_max_distance")
# New in GGIR 3.3-9, absent from the installed 3.3.6.
new_in_339 <- c("HDCZA_roll_windowsize", "LowAcc_threshold")
# The non-sleep members parts 3 and 4 read, with the GGIR object each belongs to.
nonsleep_members <- c(acc.metric = "general", zc.scale = "metrics",
                      includenightcrit = "cleaning", excludefirstlast = "cleaning",
                      excludefirst.part4 = "cleaning", excludelast.part4 = "cleaning",
                      data_cleaning_file = "cleaning",
                      do.part3.pdf = "output", criterror = "output", do.visual = "output",
                      outliers.only = "output", storefolderstructure = "output",
                      sep_reports = "output", dec_reports = "output")

test_that("the sleep group holds 43 members and the object holds 189", {
  p <- raw.params()
  groups <- attr(p, "groups")
  expect_identical(sort(unique(unname(groups))),
                   sort(c("rawdata", "general", "metrics", "cleaning", "sleep", "output",
                          "phyact", "247", "canhrActi")))
  counts <- table(groups)
  expect_identical(as.integer(counts[c("rawdata", "general", "metrics", "cleaning",
                                       "output", "sleep", "phyact", "247", "canhrActi")]),
                   c(36L, 9L, 40L, 20L, 15L, 43L, 10L, 8L, 8L))
  expect_length(p, 189L)
  sleep_names <- names(groups)[groups == "sleep"]
  expect_length(sleep_names, 43L)
  expect_identical(sleep_names, names(.raw.sleep.params.defaults()))
  # the sleep members keep GGIR's own order
  expect_identical(sleep_names[1:6],
                   c("anglethreshold", "timethreshold", "ignorenonwear", "HASPT.algo",
                     "HASIB.algo", "Sadeh_axis"))
  expect_identical(sleep_names[42:43], new_in_339)
  expect_identical(sleep_names[15:20],
                   c("possible_nap_window", "possible_nap_dur", "possible_nap_gap",
                     "possible_nap_edge_acc", "nap_model", "sleepefficiency.metric"))
  expect_identical(sleep_names[26:27],
                   c("nap_markerbutton_method", "nap_markerbutton_max_distance"))
})

test_that("the sleep defaults are the values the specification quotes", {
  p <- raw.params()
  expect_identical(p$anglethreshold, 5)
  expect_identical(p$timethreshold, 5)
  expect_identical(p$ignorenonwear, TRUE)
  expect_identical(p$HASPT.algo, "HDCZA")
  expect_identical(p$HASIB.algo, "vanHees2015")
  expect_null(p$longitudinal_axis)
  expect_identical(p$HASPT.ignore.invalid, FALSE)
  expect_null(p$loglocation)
  expect_identical(p$colid, 1)
  expect_identical(p$coln1, 2)
  expect_identical(p$relyonguider, FALSE)
  expect_identical(p$def.noc.sleep, 1)
  expect_identical(p$sleepwindowType, "SPT")
  expect_identical(p$sleepefficiency.metric, 1)
  expect_null(p$HDCZA_threshold)
  expect_identical(p$oakley_threshold, 20)
  expect_identical(p$consider_marker_button, FALSE)
  expect_identical(p$impute_marker_button, FALSE)
  expect_identical(p$sib_must_fully_overlap_with_TimeInBed, c(TRUE, TRUE))
  expect_null(p$SRI1_smoothing_wsize_hrs)
  expect_null(p$SRI1_smoothing_frac)
  expect_identical(p$spt_min_block_dur, 30)
  expect_identical(p$spt_max_gap_dur, 60)
  expect_identical(p$spt_max_gap_ratio, 1)
  expect_identical(p$HorAngle_threshold, 60)
  expect_identical(p$guider_cor_maxgap_hrs, 2)
  expect_identical(p$guider_cor_min_frac_sib, 0.5)
  expect_identical(p$guider_cor_min_hrs, 2)
  expect_identical(p$guider_cor_meme_frac_out, 0.9)
  expect_identical(p$guider_cor_meme_frac_in, 0.4)
  expect_identical(p$guider_cor_meme_min_hrs, 1)
  expect_identical(p$guider_cor_do, FALSE)
  expect_identical(p$guider_cor_meme_min_dys, 3)
  expect_identical(p$HDCZA_roll_windowsize, 5)
  expect_identical(p$LowAcc_threshold, 0.014)
  # Sadeh_axis is "Y" before the coercions and "" after them, at the defaults
  expect_identical(.raw.sleep.params.defaults()$Sadeh_axis, "Y")
  expect_identical(p$Sadeh_axis, "")
})

test_that("every sleep default is identical() to GGIR::load_params()$params_sleep", {
  skip_if_no_ggir_sleep()
  g <- GGIR::load_params()$params_sleep
  # the installed 3.3.6 has 43 members; 3.3-9 adds two
  expect_length(g, 43L)
  expect_false(any(new_in_339 %in% names(g)))
  d <- .raw.sleep.params.defaults()
  compare <- setdiff(names(d), new_in_339)
  expect_length(compare, 41L)
  unknown <- setdiff(compare, names(g))
  expect_identical(unknown, character(0), info = toString(unknown))
  n <- 0L
  for (nm in compare) {
    expect_identical(d[[nm]], g[[nm]], label = paste0("sleep default of ", nm))
    n <- n + 1L
  }
  expect_identical(n, 41L)
  # the two 3.3-9 members, from the clone's load_params
  expect_identical(d$HDCZA_roll_windowsize, 5)
  expect_identical(d$LowAcc_threshold, 0.014)
  # the two GGIR declares that canhrActi does not carry
  expect_identical(sort(setdiff(names(g), names(d))),
                   sort(c("nnights", "sleeplogsep")))
  expect_true(all(nap_sleep_members %in% names(d)))
})

test_that("the dead members are unknown names and the nap members are live", {
  for (nm in dead_sleep_members) {
    args <- list(1); names(args) <- nm
    expect_error(do.call(raw.params, args), paste0("Parameter ", nm, " is unknown"),
                 label = paste0("dead member ", nm))
  }
  # the nap members' own guards reject a bad value; test-raw-timeuse-params.R has the full set
  expect_identical(raw.params(possible_nap_gap = 5)$possible_nap_gap, 5)
  expect_identical(raw.params(possible_nap_window = c(9, 18))$possible_nap_window,
                   c(9, 18))
  expect_identical(raw.params(possible_nap_edge_acc = 30)$possible_nap_edge_acc, 30)
  expect_identical(raw.params(nap_markerbutton_method = 1)$nap_markerbutton_method, 1)
  expect_identical(raw.params(nap_markerbutton_max_distance = 45)$nap_markerbutton_max_distance,
                   45)
  expect_identical(raw.params(nap_model = "x")$nap_model, "x")
  expect_error(raw.params(possible_nap_dur = 1), "while length 2 is expected")
  # relyonguider is the live replacement for relyonsleeplog
  expect_identical(raw.params(relyonguider = TRUE)$relyonguider, TRUE)
  expect_error(raw.params(HASPT.alg = "HDCZA"), "Parameter HASPT.alg is unknown")
  expect_error(raw.params(HASPT.alg = "HDCZA", HASIBalgo = "x"),
               "Parameters HASPT.alg and HASIBalgo are unknown")
})

test_that("the fourteen non-sleep members parts 3 and 4 read are carried at GGIR's defaults", {
  p <- raw.params()
  groups <- attr(p, "groups")
  uncarried <- setdiff(names(nonsleep_members), names(p))
  expect_identical(uncarried, character(0), info = toString(uncarried))
  for (nm in names(nonsleep_members)) {
    expect_identical(unname(groups[[nm]]), unname(nonsleep_members[[nm]]),
                     label = paste0("group of ", nm))
  }
  expect_identical(p$acc.metric, "ENMO")
  expect_identical(p$zc.scale, 1)
  expect_identical(p$includenightcrit, 16)
  expect_identical(p$excludefirstlast, FALSE)
  expect_identical(p$excludefirst.part4, FALSE)
  expect_identical(p$excludelast.part4, FALSE)
  expect_null(p$data_cleaning_file)
  expect_identical(p$do.part3.pdf, FALSE)
  expect_identical(p$outliers.only, FALSE)
  expect_identical(p$criterror, 3)
  expect_identical(p$do.visual, TRUE)
  expect_identical(p$storefolderstructure, FALSE)
  expect_identical(p$sep_reports, ",")
  expect_identical(p$dec_reports, ".")
  # includenightcrit is a different parameter from includedaycrit, both 16
  expect_identical(p$includedaycrit, 16)
  expect_identical(raw.params(includenightcrit = 10)$includedaycrit, 16)
  expect_identical(raw.params(includenightcrit = 10)$includenightcrit, 10)
  expect_identical(raw.params(includedaycrit = 10)$includenightcrit, 16)
  # dayborder is untouched by the sleep coercions
  expect_identical(p$dayborder, 0)
  skip_if_no_ggir_sleep()
  g <- ggir_all_defaults()
  for (nm in names(nonsleep_members)) {
    expect_identical(p[[nm]], g[[nm]], label = paste0("GGIR default of ", nm))
  }
})

test_that("sleep type checks error with GGIR's message for every typed member", {
  typed <- list(
    numeric = c("anglethreshold", "timethreshold", "longitudinal_axis",
                "colid", "coln1", "def.noc.sleep", "sleepefficiency.metric",
                "HDCZA_threshold", "oakley_threshold", "SRI1_smoothing_wsize_hrs",
                "SRI1_smoothing_frac", "spt_min_block_dur", "spt_max_gap_dur",
                "spt_max_gap_ratio", "HorAngle_threshold", "guider_cor_maxgap_hrs",
                "guider_cor_min_frac_sib", "guider_cor_min_hrs", "guider_cor_meme_frac_out",
                "guider_cor_meme_frac_in", "guider_cor_meme_min_hrs",
                "guider_cor_meme_min_dys", "HDCZA_roll_windowsize", "LowAcc_threshold"),
    boolean = c("ignorenonwear", "HASPT.ignore.invalid", "relyonguider",
                "impute_marker_button", "consider_marker_button",
                "sib_must_fully_overlap_with_TimeInBed", "guider_cor_do"),
    character = c("HASPT.algo", "HASIB.algo", "Sadeh_axis", "sleepwindowType", "loglocation"))
  expect_identical(lengths(typed), c(numeric = 24L, boolean = 7L, character = 5L))
  wrong <- list(numeric = "abc", boolean = 1, character = 1)
  n <- 0L
  for (cls in names(typed)) {
    for (nm in typed[[cls]]) {
      args <- list(wrong[[cls]]); names(args) <- nm
      expect_identical(error_message_sleep(do.call(raw.params, args)),
                       paste0("\nSleep parameter ", nm, " is not ", cls),
                       label = paste0("type error for ", nm))
      n <- n + 1L
    }
  }
  expect_identical(n, 36L)
  skip_if_no_ggir_sleep()
  for (cls in names(typed)) {
    for (nm in typed[[cls]]) {
      if (nm %in% new_in_339) next  # not type-checked by the installed 3.3.6
      args <- list(wrong[[cls]]); names(args) <- nm
      expect_identical(error_message_sleep(do.call(raw.params, args)),
                       error_message_sleep(do.call(ggir_full, args)),
                       label = paste0("GGIR type error for ", nm))
    }
  }
})

test_that("the non-sleep members parts 3 and 4 read are type-checked as GGIR checks them", {
  cases <- list(list("includenightcrit", "abc", "cleaning parameter includenightcrit is not numeric"),
                list("criterror", "abc", "output parameter criterror is not numeric"),
                list("excludefirstlast", 1, "cleaning parameter excludefirstlast is not boolean"),
                list("excludefirst.part4", 1, "cleaning parameter excludefirst.part4 is not boolean"),
                list("excludelast.part4", 1, "cleaning parameter excludelast.part4 is not boolean"),
                list("do.part3.pdf", 1, "output parameter do.part3.pdf is not boolean"),
                list("outliers.only", 1, "output parameter outliers.only is not boolean"),
                list("do.visual", 1, "output parameter do.visual is not boolean"),
                list("storefolderstructure", 1, "output parameter storefolderstructure is not boolean"),
                list("data_cleaning_file", 1, "cleaning parameter data_cleaning_file is not character"),
                list("sep_reports", 1, "output parameter sep_reports is not character"),
                list("dec_reports", 1, "output parameter dec_reports is not character"),
                list("acc.metric", 1, "general parameter acc.metric is not character"),
                list("zc.scale", "abc", "Metrics parameter zc.scale is not numeric"))
  for (cs in cases) {
    args <- list(cs[[2]]); names(args) <- cs[[1]]
    expect_identical(error_message_sleep(do.call(raw.params, args)),
                     paste0("\n", cs[[3]]), label = paste0("type error for ", cs[[1]]))
  }
  skip_if_no_ggir_sleep()
  for (cs in cases) {
    args <- list(cs[[2]]); names(args) <- cs[[1]]
    expect_identical(error_message_sleep(do.call(raw.params, args)),
                     error_message_sleep(do.call(ggir_full, args)),
                     label = paste0("GGIR type error for ", cs[[1]]))
  }
})

haspt_cases <- list("HDCZA", "HorAngle", "NotWorn", "MotionWare", "HLRB", "LowAcc",
                    "notused", "bogus", c("HorAngle", "NotWorn"), c("NotWorn", "HorAngle"),
                    c("HDCZA", "NotWorn"), c("LowAcc", "NotWorn"), c("NotWorn", "LowAcc"))

test_that("the HASPT.algo whitelist and the NotWorn swap reproduce GGIR's table", {
  got <- lapply(haspt_cases, function(a) suppressWarnings(raw.params(HASPT.algo = a))$HASPT.algo)
  expect_identical(got[[1]], "HDCZA")
  expect_identical(got[[2]], "HorAngle")
  expect_identical(got[[3]], "NotWorn")
  expect_identical(got[[4]], "MotionWare")
  expect_identical(got[[5]], "HLRB")
  expect_identical(got[[6]], "LowAcc")
  # a user-supplied "notused" is rewritten; only a length-2 def.noc.sleep reaches it
  expect_identical(got[[7]], "HDCZA")
  # an unrecognised string becomes HDCZA without a warning
  expect_identical(got[[8]], "HDCZA")
  expect_silent(raw.params(HASPT.algo = "bogus"))
  # NotWorn is expected first, so a pair is reversed
  expect_identical(got[[9]], c("NotWorn", "HorAngle"))
  expect_identical(got[[10]], c("NotWorn", "HorAngle"))
  # the whole vector is replaced before the swap test, so the fallback is lost
  expect_identical(got[[11]], "HDCZA")
  expect_identical(got[[12]], c("NotWorn", "LowAcc"))
  expect_identical(got[[13]], c("NotWorn", "LowAcc"))
  skip_if_no_ggir_sleep()
  for (a in haspt_cases) {
    # LowAcc joined the whitelist in 3.3-9; the installed 3.3.6 rewrites it to HDCZA
    if (any(a == "LowAcc")) next
    ours <- with_warnings_sleep(raw.params(HASPT.algo = a))
    ggir <- with_warnings_sleep(ggir_full(HASPT.algo = a))
    expect_identical(ours$value$HASPT.algo, ggir$value$params_sleep$HASPT.algo,
                     label = paste0("HASPT.algo ", paste(a, collapse = ",")))
    expect_identical(ours$value$sensor.location, ggir$value$params_general$sensor.location,
                     label = paste0("sensor.location after HASPT.algo ", paste(a, collapse = ",")))
    expect_identical(ours$value$sleepwindowType, ggir$value$params_sleep$sleepwindowType,
                     label = paste0("sleepwindowType after HASPT.algo ", paste(a, collapse = ",")))
    expect_identical(ours$warnings, ggir$warnings,
                     label = paste0("warnings for HASPT.algo ", paste(a, collapse = ",")))
  }
})

test_that("a def.noc.sleep of length 2 sets HASPT.algo to notused, and only that", {
  expect_identical(suppressWarnings(raw.params(def.noc.sleep = c(21, 9)))$HASPT.algo, "notused")
  expect_identical(suppressWarnings(raw.params(def.noc.sleep = c(3, 17)))$HASPT.algo, "notused")
  expect_identical(suppressWarnings(raw.params(def.noc.sleep = c(21, 9),
                                               HASPT.algo = "NotWorn"))$HASPT.algo, "notused")
  # length 1 and length 0 leave the whitelist branch in charge
  expect_identical(raw.params(def.noc.sleep = 1)$HASPT.algo, "HDCZA")
  expect_identical(raw.params(def.noc.sleep = c())$HASPT.algo, "HDCZA")
  skip_if_no_ggir_sleep()
  for (v in list(c(21, 9), c(3, 17), 1, c())) {
    ours <- with_warnings_sleep(raw.params(def.noc.sleep = v))
    ggir <- with_warnings_sleep(ggir_full(def.noc.sleep = v))
    expect_identical(ours$value$HASPT.algo, ggir$value$params_sleep$HASPT.algo,
                     label = paste0("def.noc.sleep ", deparse(v)))
    expect_identical(ours$value$def.noc.sleep, ggir$value$params_sleep$def.noc.sleep,
                     label = paste0("def.noc.sleep value ", deparse(v)))
    expect_identical(ours$warnings, ggir$warnings)
  }
})

test_that("a count-based HASIB.algo forces the zero-crossing metric of its Sadeh_axis", {
  for (algo in c("Sadeh1994", "Galland2012", "ColeKripke1992", "Oakley1997")) {
    p <- raw.params(HASIB.algo = algo)
    expect_identical(p$Sadeh_axis, "Y", label = paste0(algo, " keeps Sadeh_axis"))
    expect_identical(p$do.zcy, TRUE, label = paste0(algo, " forces do.zcy"))
    expect_identical(p$do.zcx, FALSE)
    expect_identical(p$do.zcz, FALSE)
    px <- raw.params(HASIB.algo = algo, Sadeh_axis = "X")
    expect_identical(c(px$do.zcx, px$do.zcy, px$do.zcz), c(TRUE, FALSE, FALSE),
                     label = paste0(algo, " on X"))
    pz <- raw.params(HASIB.algo = algo, Sadeh_axis = "Z")
    expect_identical(c(pz$do.zcx, pz$do.zcy, pz$do.zcz), c(FALSE, FALSE, TRUE),
                     label = paste0(algo, " on Z"))
  }
  # a Sadeh_axis that is not X, Y or Z warns and turns nothing on
  w <- with_warnings_sleep(raw.params(HASIB.algo = "Sadeh1994", Sadeh_axis = "y"))
  expect_identical(w$warnings,
                   "Parameter Sadeh_axis does not have meaningful value, it needs to be X, Y or Z (capital)")
  expect_identical(c(w$value$do.zcx, w$value$do.zcy, w$value$do.zcz), c(FALSE, FALSE, FALSE))
  expect_identical(w$value$Sadeh_axis, "y")
  # an already-TRUE flag is left alone and nothing warns
  expect_silent(raw.params(HASIB.algo = "Sadeh1994", do.zcy = TRUE))
  skip_if_no_ggir_sleep()
  for (algo in c("Sadeh1994", "Galland2012", "ColeKripke1992", "Oakley1997", "vanHees2015",
                 "NotWorn", "data")) {
    for (ax in c("X", "Y", "Z", "y")) {
      ours <- with_warnings_sleep(raw.params(HASIB.algo = algo, Sadeh_axis = ax))
      ggir <- with_warnings_sleep(ggir_full(HASIB.algo = algo, Sadeh_axis = ax))
      lbl <- paste0(algo, " / ", ax)
      expect_identical(ours$value$Sadeh_axis, ggir$value$params_sleep$Sadeh_axis, label = lbl)
      for (fl in c("do.zcx", "do.zcy", "do.zcz")) {
        expect_identical(ours$value[[fl]], ggir$value$params_metrics[[fl]],
                         label = paste0(lbl, " ", fl))
      }
      expect_identical(ours$warnings, ggir$warnings, label = paste0("warnings ", lbl))
    }
  }
})

test_that("Sadeh_axis is blanked when HASIB.algo is not one of the four count algorithms", {
  expect_identical(raw.params()$Sadeh_axis, "")
  expect_identical(raw.params(HASIB.algo = "vanHees2015", Sadeh_axis = "Z")$Sadeh_axis, "")
  expect_identical(raw.params(HASIB.algo = "NotWorn", Sadeh_axis = "Z")$Sadeh_axis, "")
  expect_identical(raw.params(Sadeh_axis = "Z")$do.zcz, FALSE)
})

test_that("the Sadeh_axis blanking is not idempotent, exactly as in GGIR", {
  # GGIR calls extract_params on entry to both g.part3 and g.part4, so every coercion runs twice
  once <- unclass(raw.params())             # vanHees2015, so Sadeh_axis is now ""
  expect_identical(once$Sadeh_axis, "")
  once$HASIB.algo <- "Sadeh1994"
  twice <- with_warnings_sleep(.raw.params.check(once))
  expect_identical(twice$warnings,
                   "Parameter Sadeh_axis does not have meaningful value, it needs to be X, Y or Z (capital)")
  expect_identical(twice$value$Sadeh_axis, "")
  skip_if_no_ggir_sleep()
  lp <- GGIR::load_params()
  lp$params_sleep$Sadeh_axis <- ""
  lp$params_sleep$HASIB.algo <- "Sadeh1994"
  ggir <- with_warnings_sleep(do.call(GGIR::check_params, lp))
  expect_identical(twice$warnings, ggir$warnings)
  expect_identical(twice$value$Sadeh_axis, ggir$value$params_sleep$Sadeh_axis)
})

test_that("sensor.location hip forces the angles and HASPT.algo, with GGIR's warnings", {
  ours <- with_warnings_sleep(raw.params(sensor.location = "hip"))
  expect_identical(c(ours$value$do.anglex, ours$value$do.angley, ours$value$do.anglez),
                   c(TRUE, TRUE, TRUE))
  expect_identical(ours$value$HASPT.algo, "HorAngle")
  expect_identical(ours$value$def.noc.sleep, 1)
  expect_identical(ours$value$sleepwindowType, "TimeInBed")
  expect_length(ours$warnings, 3L)
  expect_identical(ours$warnings,
                   c(paste0("\nWhen working with hip data all three angle metrics are needed,",
                            "so GGIR now auto-sets parameters do.anglex, do.angley, and do.anglez to TRUE."),
                     "\nChanging HASPT.algo value to HorAngle, because sensor.location is set as hip",
                     "\nHASPT.algo is set to HorAngle, therefore auto-updating sleepwindowType to TimeInBed"))
  # the angle warning goes away when all three are already TRUE, the two sleep ones remain
  still <- with_warnings_sleep(raw.params(sensor.location = "hip", do.anglex = TRUE,
                                          do.angley = TRUE, do.anglez = TRUE))
  expect_length(still$warnings, 2L)
  expect_identical(raw.params(sensor.location = "wrist")$do.anglex, FALSE)
  skip_if_no_ggir_sleep()
  ggir <- with_warnings_sleep(ggir_full(sensor.location = "hip"))
  expect_identical(ours$warnings, ggir$warnings)
  expect_identical(ours$value$HASPT.algo, ggir$value$params_sleep$HASPT.algo)
  expect_identical(ours$value$sleepwindowType, ggir$value$params_sleep$sleepwindowType)
  expect_identical(ours$value$do.anglex, ggir$value$params_metrics$do.anglex)
  expect_identical(ours$value$do.angley, ggir$value$params_metrics$do.angley)
  expect_identical(ours$value$do.anglez, ggir$value$params_metrics$do.anglez)
})

test_that("the hip rule is suppressed under HASPT.algo notused and NotWorn", {
  # GGIR gates the angle forcing on HASPT.algo[1] being neither "notused" nor "NotWorn"
  nw <- with_warnings_sleep(raw.params(sensor.location = "hip", HASPT.algo = "NotWorn"))
  expect_identical(c(nw$value$do.anglex, nw$value$do.angley, nw$value$do.anglez),
                   c(FALSE, FALSE, TRUE))
  expect_identical(nw$value$HASPT.algo, "NotWorn")
  expect_identical(nw$value$sensor.location, "hip")
  expect_length(nw$warnings, 0L)
  nu <- with_warnings_sleep(raw.params(sensor.location = "hip", def.noc.sleep = c(21, 9)))
  expect_identical(c(nu$value$do.anglex, nu$value$do.angley, nu$value$do.anglez),
                   c(FALSE, FALSE, TRUE))
  expect_identical(nu$value$HASPT.algo, "notused")
  expect_length(nu$warnings, 0L)
  # every other guider on the hip gets the full forcing
  for (a in c("HDCZA", "LowAcc", "MotionWare", "HLRB")) {
    o <- suppressWarnings(raw.params(sensor.location = "hip", HASPT.algo = a))
    expect_identical(o$HASPT.algo, "HorAngle", label = paste0("hip + ", a))
    expect_identical(c(o$do.anglex, o$do.angley, o$do.anglez), c(TRUE, TRUE, TRUE),
                     label = paste0("hip angles + ", a))
  }
  skip_if_no_ggir_sleep()
  cases <- list(list(HASPT.algo = "NotWorn"), list(def.noc.sleep = c(21, 9)),
                list(HASPT.algo = "HDCZA"), list(HASPT.algo = "LowAcc"),
                list(HASPT.algo = "MotionWare"), list(HASPT.algo = "HLRB"),
                list(HASPT.algo = "HorAngle"))
  for (cs in cases) {
    args <- c(list(sensor.location = "hip"), cs)
    ours <- with_warnings_sleep(do.call(raw.params, args))
    ggir <- with_warnings_sleep(do.call(ggir_full, args))
    lbl <- paste0("hip + ", paste(names(cs), unlist(lapply(cs, paste, collapse = ",")), collapse = " "))
    expect_identical(ours$value$HASPT.algo, ggir$value$params_sleep$HASPT.algo, label = lbl)
    expect_identical(ours$value$def.noc.sleep, ggir$value$params_sleep$def.noc.sleep, label = lbl)
    for (fl in c("do.anglex", "do.angley", "do.anglez")) {
      expect_identical(ours$value[[fl]], ggir$value$params_metrics[[fl]],
                       label = paste0(lbl, " ", fl))
    }
    expect_identical(ours$warnings, ggir$warnings, label = paste0("warnings ", lbl))
  }
})

test_that("HorAngle forces sensor.location to hip after the hip block has read it", {
  o <- with_warnings_sleep(raw.params(HASPT.algo = "HorAngle"))
  expect_identical(o$value$sensor.location, "hip")
  # a hip configuration with no x and no y angle column
  expect_identical(c(o$value$do.anglex, o$value$do.angley, o$value$do.anglez),
                   c(FALSE, FALSE, TRUE))
  expect_identical(o$value$sleepwindowType, "TimeInBed")
  expect_identical(o$warnings,
                   "\nHASPT.algo is set to HorAngle, therefore auto-updating sleepwindowType to TimeInBed")
  # a pair whose first element is NotWorn does not trigger it
  expect_identical(suppressWarnings(raw.params(HASPT.algo = c("HorAngle", "NotWorn")))$sensor.location,
                   "wrist")
  skip_if_no_ggir_sleep()
  ggir <- with_warnings_sleep(ggir_full(HASPT.algo = "HorAngle"))
  expect_identical(o$value$sensor.location, ggir$value$params_general$sensor.location)
  expect_identical(o$value$do.anglex, ggir$value$params_metrics$do.anglex)
  expect_identical(o$value$do.angley, ggir$value$params_metrics$do.angley)
  expect_identical(o$warnings, ggir$warnings)
})

test_that("loglocation hygiene reproduces GGIR", {
  expect_null(raw.params(loglocation = "")$loglocation)
  expect_identical(raw.params(loglocation = "C:\\data\\sleeplog.csv")$loglocation,
                   "C:/data/sleeplog.csv")
  expect_identical(raw.params(loglocation = "/home/x/log.csv")$loglocation, "/home/x/log.csv")
  # a diary with a def.noc.sleep that is not length 1 warns and resets it
  w <- with_warnings_sleep(raw.params(loglocation = "log.csv", def.noc.sleep = c(21, 9)))
  expect_identical(w$value$def.noc.sleep, 1)
  expect_identical(w$value$HASPT.algo, "notused")
  expect_true(any(grepl("reset def.noc.sleep to its default value of 1", w$warnings, fixed = TRUE)))
  skip_if_no_ggir_sleep()
  for (v in list("", "C:\\data\\sleeplog.csv", "/home/x/log.csv")) {
    ours <- with_warnings_sleep(raw.params(loglocation = v))
    ggir <- with_warnings_sleep(ggir_full(loglocation = v))
    expect_identical(ours$value$loglocation, ggir$value$params_sleep$loglocation,
                     label = paste0("loglocation ", v))
    expect_identical(ours$warnings, ggir$warnings)
  }
  ours <- with_warnings_sleep(raw.params(loglocation = "log.csv", def.noc.sleep = c(21, 9)))
  ggir <- with_warnings_sleep(ggir_full(loglocation = "log.csv", def.noc.sleep = c(21, 9)))
  expect_identical(ours$value$def.noc.sleep, ggir$value$params_sleep$def.noc.sleep)
  expect_identical(ours$value$HASPT.algo, ggir$value$params_sleep$HASPT.algo)
  expect_identical(ours$warnings, ggir$warnings)
})

test_that("sleepwindowType is forced back to SPT without a diary, and kept with one", {
  w <- with_warnings_sleep(raw.params(sleepwindowType = "TimeInBed"))
  expect_identical(w$value$sleepwindowType, "SPT")
  expect_identical(w$warnings,
                   "\nAuto-updating sleepwindowType to SPT because no sleeplog used and neither HASPT.algo HorAngle or NotWorn used.")
  # a diary keeps it, and so do the two guiders GGIR exempts
  expect_silent(p <- raw.params(sleepwindowType = "TimeInBed", loglocation = "log.csv"))
  expect_identical(p$sleepwindowType, "TimeInBed")
  expect_silent(p <- raw.params(sleepwindowType = "TimeInBed", HASPT.algo = "NotWorn"))
  expect_identical(p$sleepwindowType, "TimeInBed")
  expect_identical(raw.params(sleepwindowType = "SPT")$sleepwindowType, "SPT")
  skip_if_no_ggir_sleep()
  cases <- list(list(sleepwindowType = "TimeInBed"),
                list(sleepwindowType = "TimeInBed", loglocation = "log.csv"),
                list(sleepwindowType = "TimeInBed", HASPT.algo = "NotWorn"),
                list(sleepwindowType = "TimeInBed", HASPT.algo = "HorAngle"),
                list(sleepwindowType = "SPT", HASPT.algo = "HorAngle"))
  for (cs in cases) {
    ours <- with_warnings_sleep(do.call(raw.params, cs))
    ggir <- with_warnings_sleep(do.call(ggir_full, cs))
    lbl <- paste(names(cs), unlist(cs), collapse = " ")
    expect_identical(ours$value$sleepwindowType, ggir$value$params_sleep$sleepwindowType, label = lbl)
    expect_identical(ours$warnings, ggir$warnings, label = paste0("warnings ", lbl))
  }
})

test_that(".raw.sleep.params.check and .raw.sleep.params.hip alone reproduce check_params", {
  skip_if_no_ggir_sleep()
  scenarios <- list(
    list(),
    list(HASPT.algo = "NotWorn"),
    list(HASPT.algo = "HorAngle"),
    list(HASPT.algo = c("HDCZA", "NotWorn")),
    list(HASIB.algo = "ColeKripke1992", Sadeh_axis = "Z"),
    list(HASIB.algo = "Oakley1997", Sadeh_axis = "X"),
    list(sensor.location = "hip"),
    list(sensor.location = "hip", HASPT.algo = "NotWorn"),
    list(def.noc.sleep = c(21, 9)),
    list(def.noc.sleep = c(21, 9), sensor.location = "hip"),
    list(loglocation = "C:\\d\\log.csv", sleepwindowType = "TimeInBed"),
    list(sleepwindowType = "TimeInBed")
  )
  sleep_names <- names(.raw.sleep.params.defaults())
  for (sc in scenarios) {
    sleep <- .raw.sleep.params.defaults()
    general <- list(sensor.location = "wrist")
    metrics <- list(do.anglex = FALSE, do.angley = FALSE, do.anglez = TRUE,
                    do.zcx = FALSE, do.zcy = FALSE, do.zcz = FALSE)
    for (nm in names(sc)) {
      if (nm %in% names(sleep)) sleep[[nm]] <- sc[[nm]]
      if (nm %in% names(general)) general[[nm]] <- sc[[nm]]
      if (nm %in% names(metrics)) metrics[[nm]] <- sc[[nm]]
    }
    ours <- with_warnings_sleep({
      a <- .raw.sleep.params.check(sleep = sleep, general = general, metrics = metrics)
      b <- .raw.sleep.params.hip(sleep = a$sleep, general = a$general)
      list(sleep = a$sleep, metrics = a$metrics, general = b$general)
    })
    ggir <- with_warnings_sleep(do.call(ggir_full, sc))
    lbl <- if (length(sc) == 0) "defaults" else paste(names(sc), unlist(lapply(sc, paste, collapse = ",")), collapse = " ")
    for (nm in sleep_names) {
      if (nm %in% new_in_339) next
      expect_identical(ours$value$sleep[[nm]], ggir$value$params_sleep[[nm]],
                       label = paste0(lbl, " sleep$", nm))
    }
    for (nm in names(metrics)) {
      expect_identical(ours$value$metrics[[nm]], ggir$value$params_metrics[[nm]],
                       label = paste0(lbl, " metrics$", nm))
    }
    expect_identical(ours$value$general$sensor.location, ggir$value$params_general$sensor.location,
                     label = paste0(lbl, " sensor.location"))
    expect_identical(ours$warnings, ggir$warnings, label = paste0(lbl, " warnings"))
  }
})

test_that("a sweep of sleep values through both implementations gives identical coerced values", {
  skip_if_no_ggir_sleep()
  sweep <- list(
    list("anglethreshold", c(5, 10)), list("timethreshold", c(5, 10, 30)),
    list("ignorenonwear", FALSE), list("HASPT.ignore.invalid", NA),
    list("longitudinal_axis", 2), list("colid", 3), list("coln1", 4),
    list("relyonguider", TRUE), list("def.noc.sleep", c()),
    list("sleepefficiency.metric", 2), list("HDCZA_threshold", c(15, 20)),
    list("HDCZA_threshold", 0.2), list("oakley_threshold", 40),
    list("consider_marker_button", TRUE), list("impute_marker_button", TRUE),
    list("sib_must_fully_overlap_with_TimeInBed", c(FALSE, TRUE)),
    list("SRI1_smoothing_wsize_hrs", 1), list("SRI1_smoothing_frac", 0.5),
    list("spt_min_block_dur", 45), list("spt_max_gap_dur", 90),
    list("spt_max_gap_ratio", 0.5), list("HorAngle_threshold", 45),
    list("guider_cor_do", TRUE), list("guider_cor_maxgap_hrs", 3),
    list("guider_cor_min_frac_sib", 0.6), list("guider_cor_min_hrs", 3),
    list("guider_cor_meme_frac_out", 0.8), list("guider_cor_meme_frac_in", 0.5),
    list("guider_cor_meme_min_hrs", 2), list("guider_cor_meme_min_dys", 4),
    list("acc.metric", "MAD"), list("zc.scale", 2),
    list("includenightcrit", 10), list("excludefirstlast", TRUE),
    list("excludefirst.part4", TRUE), list("excludelast.part4", TRUE),
    list("data_cleaning_file", "clean.csv"), list("criterror", 1),
    list("do.visual", FALSE), list("outliers.only", TRUE),
    list("storefolderstructure", TRUE), list("sep_reports", ";"),
    list("do.part3.pdf", TRUE)
  )
  n <- 0L
  for (s in sweep) {
    args <- list(s[[2]]); names(args) <- s[[1]]
    ours <- suppressWarnings(do.call(raw.params, args))
    ggir <- suppressWarnings(do.call(ggir_full, args))
    flat <- do.call(c, unname(ggir))
    expect_identical(ours[[s[[1]]]], flat[[s[[1]]]], label = paste0("sweep ", s[[1]]))
    n <- n + 1L
  }
  expect_identical(n, 43L)
  # the two 3.3-9 members have no installed counterpart
  expect_identical(raw.params(HDCZA_roll_windowsize = 10)$HDCZA_roll_windowsize, 10)
  expect_identical(raw.params(LowAcc_threshold = 0.02)$LowAcc_threshold, 0.02)
})

test_that("sep_reports equal to dec_reports errors as in check_params.R:400-403", {
  expect_error(raw.params(dec_reports = ","),
               "You have set sep_reports and dec_reports both to , this is ambiguous")
  expect_error(raw.params(sep_reports = "."),
               "You have set sep_reports and dec_reports both to . this is ambiguous")
  expect_identical(raw.params(sep_reports = ";", dec_reports = ",")$dec_reports, ",")
  skip_if_no_ggir_sleep()
  expect_identical(error_message_sleep(raw.params(dec_reports = ",")),
                   error_message_sleep(ggir_full(dec_reports = ",")))
  expect_identical(error_message_sleep(raw.params(sep_reports = ".")),
                   error_message_sleep(ggir_full(sep_reports = ".")))
})

test_that("a NULL sleep member fails exactly where GGIR fails", {
  # GGIR indexes HASPT.algo[1] and compares sleepwindowType unguarded; both die with R's message
  expect_error(raw.params(HASPT.algo = NULL), "argument is of length zero")
  expect_error(raw.params(sleepwindowType = NULL), "argument is of length zero")
  # HASIB.algo NULL is survivable in both, because any(NULL %in% ...) is FALSE
  expect_null(raw.params(HASIB.algo = NULL)$HASIB.algo)
  expect_identical(raw.params(HASIB.algo = NULL)$Sadeh_axis, "")
  # the members that are NULL by default stay NULL and stay present
  p <- raw.params()
  null_members <- c("longitudinal_axis", "loglocation", "HDCZA_threshold",
                    "SRI1_smoothing_wsize_hrs", "SRI1_smoothing_frac")
  absent <- setdiff(null_members, names(p))
  expect_identical(absent, character(0), info = toString(absent))
  not_null <- null_members[!vapply(null_members, function(nm) is.null(p[[nm]]), logical(1))]
  expect_identical(not_null, character(0), info = toString(not_null))
  skip_if_no_ggir_sleep()
  for (nm in c("HASPT.algo", "sleepwindowType", "HASIB.algo")) {
    args <- list(NULL); names(args) <- nm
    expect_identical(error_message_sleep(do.call(raw.params, args)),
                     error_message_sleep(do.call(ggir_full, args)),
                     label = paste0("NULL ", nm))
  }
})

test_that("raw.sleep.params() is a coerced view onto the sleep group", {
  s <- raw.sleep.params()
  expect_s3_class(s, "canhrActi_raw_sleep_params")
  expect_length(s, 43L)
  expect_identical(names(s), names(.raw.sleep.params.defaults()))
  p <- raw.params()
  for (nm in names(s)) expect_identical(s[[nm]], p[[nm]], label = paste0("view of ", nm))
  expect_identical(s$Sadeh_axis, "")   # coerced, not the raw default "Y"
  # named arguments go to raw.params()
  expect_identical(raw.sleep.params(HASPT.algo = "NotWorn")$HASPT.algo, "NotWorn")
  expect_identical(suppressWarnings(raw.sleep.params(def.noc.sleep = c(21, 9)))$HASPT.algo, "notused")
  # a cross-group argument is accepted
  expect_identical(suppressWarnings(raw.sleep.params(sensor.location = "hip"))$HASPT.algo, "HorAngle")
  # an existing object is used as is
  p2 <- suppressWarnings(raw.params(HASIB.algo = "Sadeh1994"))
  expect_identical(raw.sleep.params(p2)$Sadeh_axis, "Y")
  expect_error(raw.sleep.params(nnights = 3), "Parameter nnights is unknown")
})

test_that(".raw.params.ggir() emits params_sleep and params_output", {
  split <- .raw.params.ggir(raw.params())
  expect_identical(names(split), c("params_rawdata", "params_general", "params_metrics",
                                   "params_cleaning", "params_sleep", "params_output",
                                   "params_247", "params_phyact"))
  expect_length(split$params_sleep, 43L)
  expect_length(split$params_output, 15L)
  expect_identical(names(split$params_sleep), names(.raw.sleep.params.defaults()))
  skip_if_no_ggir_sleep()
  lp <- GGIR::load_params()
  for (nm in names(split$params_sleep)) {
    # Sadeh_axis is the one member check_params changes at the defaults
    if (nm %in% c(new_in_339, "Sadeh_axis")) next
    expect_identical(split$params_sleep[[nm]], lp$params_sleep[[nm]],
                     label = paste0("params_sleep$", nm))
  }
  for (nm in names(split$params_output)) {
    # GGIR declares TRUE and its visualreport default forces FALSE
    if (nm == "save_ms5raw_without_invalid") next
    expect_identical(split$params_output[[nm]], lp$params_output[[nm]],
                     label = paste0("params_output$", nm))
  }
  expect_identical(lp$params_output$save_ms5raw_without_invalid, TRUE)
  expect_identical(split$params_output$save_ms5raw_without_invalid, FALSE)
  expect_identical(lp$params_sleep$Sadeh_axis, "Y")
  expect_identical(split$params_sleep$Sadeh_axis, "")
})

test_that("version guard: params_sleep has 43 members on 3.3.6 and the port carries the two 3.3-9 names", {
  skip_if_no_ggir_sleep()
  g <- GGIR::load_params()$params_sleep
  expect_length(g, 43L)
  d <- .raw.sleep.params.defaults()
  # a third new member would mean the port's parameter object is behind the clone
  expect_identical(sort(setdiff(names(d), names(g))), sort(new_in_339))
  expect_identical(length(g) + length(new_in_339), 45L)
})
