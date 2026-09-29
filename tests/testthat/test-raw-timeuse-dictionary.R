# Parity tests for R/raw_timeuse_dictionary.R against GGIR part 5's
# g.report.part5_dictionary and its baseDictionary: the dictionary csvs shipped with
# the two reference recordings (CANHRACTI_GGIR_REF), a live GGIR run, and the token
# parser on its own. Reference and live tests skip when either is unavailable.

.ggir_ref_dict <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")

dict_skip_ref <- function() {
  if (.ggir_ref_dict == "" || !dir.exists(.ggir_ref_dict)) {
    testthat::skip("CANHRACTI_GGIR_REF is unset or does not point to an existing folder")
  }
}
dict_skip_ggir <- function() {
  if (!requireNamespace("GGIR", quietly = TRUE)) {
    testthat::skip("GGIR is not installed; live parity comparisons skipped")
  }
}
dict_ref <- function(...) file.path(sub("/+$", "", .ggir_ref_dict), ...)

# results folders of the two reference runs
dict_runs <- function() {
  list(MOS2 = dict_ref("out", "output_din", "results"),
       EE = dict_ref("timing_out", "output_timing", "results"))
}

dict_cnames <- function(path) {
  colnames(data.table::fread(path, verbose = FALSE, nrows = 2))
}

# write a dictionary with GGIR's own fwrite call
dict_write_lines <- function(df) {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  data.table::fwrite(df, file = f, row.names = FALSE, na = "", sep = ",", dec = ".")
  readLines(f)
}

# report the first differing row rather than only "not identical"
dict_first_diff <- function(a, b) {
  n <- min(length(a), length(b))
  k <- which(a[seq_len(n)] != b[seq_len(n)])
  if (length(k) == 0) {
    return(paste0("same first ", n, " lines, lengths ", length(a), " and ", length(b)))
  }
  paste0("line ", k[1], "\n  mine: ", a[k[1]], "\n  ref : ", b[k[1]],
         "\n  (", length(k), " differing lines)")
}

# BASE DICTIONARY

test_that(".raw.base.dictionary is GGIR's 41-entry baseDictionary, in its order", {
  bd <- .raw.base.dictionary()
  expect_type(bd, "list")
  expect_identical(length(bd), 41L)
  expect_true(all(vapply(bd, function(s) is.character(s) && length(s) == 1L, logical(1))))
  expect_identical(names(bd),
                   c("ID", "filename", "weekday", "window_number", "window", "night_number",
                     "start_end_window", "daytype", "calendar_date", "startday",
                     "Nvaliddays", "Nvaliddays_WD", "Nvaliddays_WE", "Nvaliddays_AL10F_WD",
                     "Nvaliddays_AL10F_WE", "Nvalidsegments", "Nvalidsegments_WD",
                     "Nvalidsegments_WE", "daysleeper", "cleaningcode", "guider",
                     "sleeplog_used", "acc_available", "N_atleast5minwakenight",
                     "Ndaysleeper", "Ncleaningcodezero", "Ncleaningcode1", "Ncleaningcode2",
                     "Ncleaningcode3", "Ncleaningcode4", "Ncleaningcode5", "Ncleaningcode6",
                     "Nsleeplog_used", "Nacc_available", "tail_expansion_minutes",
                     "boutcriter.in", "boutcriter.lig", "boutcriter.mvpa", "boutdur.in",
                     "boutdur.lig", "boutdur.mvpa"))
  # three texts literally, including the part numbers the person summary rewrites
  expect_identical(bd$filename, "File name")
  expect_identical(bd$Nvaliddays,
                   "Number of valid days based on the cleaning parameters for part 2, 4, and 5")
  expect_identical(bd$boutdur.in, "Duration/s of inactivity bouts (min)")
})

test_that(".raw.base.dictionary is identical() to GGIR's own sysdata object", {
  dict_skip_ggir()
  expect_identical(.raw.base.dictionary(), GGIR:::baseDictionary)
})

# SHIPPED DICTIONARIES

test_that("P7c MOS2: both shipped dictionaries reproduce under identical(readLines())", {
  dict_skip_ref()
  res <- dict_runs()$MOS2
  skip_if_not(dir.exists(res))
  dn <- dict_cnames(file.path(res, "part5_daysummary_MM_L40M100V400_T5A5.csv"))
  pn <- dict_cnames(file.path(res, "part5_personsummary_MM_L40M100V400_T5A5.csv"))
  expect_identical(length(dn), 115L)
  expect_identical(length(pn), 207L)
  d <- raw.timeuse.dictionary(dn, pn)
  expect_identical(nrow(d$daysummary), 115L)
  expect_identical(nrow(d$personsummary), 207L)
  expect_identical(names(d$daysummary), c("Variable", "Definition"))
  # the row order is the report's own column order
  expect_identical(d$daysummary$Variable, dn)
  expect_identical(d$personsummary$Variable, pn)
  ref_day <- readLines(file.path(res, "variableDictionary", "part5_dictionary_daysummary.csv"))
  ref_per <- readLines(file.path(res, "variableDictionary", "part5_dictionary_personsummary.csv"))
  mine_day <- dict_write_lines(d$daysummary)
  mine_per <- dict_write_lines(d$personsummary)
  expect_identical(length(ref_day), 116L)
  expect_identical(length(ref_per), 208L)
  expect_identical(mine_day, ref_day, info = dict_first_diff(mine_day, ref_day))
  expect_identical(mine_per, ref_per, info = dict_first_diff(mine_per, ref_per))
})

test_that("P7c EE: both shipped dictionaries reproduce, the person one 208 rows", {
  dict_skip_ref()
  res <- dict_runs()$EE
  skip_if_not(dir.exists(res))
  dn <- dict_cnames(file.path(res, "part5_daysummary_MM_L40M100V400_T5A5.csv"))
  pn <- dict_cnames(file.path(res, "part5_personsummary_MM_L40M100V400_T5A5.csv"))
  expect_identical(length(dn), 115L)
  expect_identical(length(pn), 208L)
  d <- raw.timeuse.dictionary(dn, pn)
  ref_day <- readLines(file.path(res, "variableDictionary", "part5_dictionary_daysummary.csv"))
  ref_per <- readLines(file.path(res, "variableDictionary", "part5_dictionary_personsummary.csv"))
  expect_identical(length(ref_day), 116L)
  expect_identical(length(ref_per), 209L)
  mine_day <- dict_write_lines(d$daysummary)
  mine_per <- dict_write_lines(d$personsummary)
  expect_identical(mine_day, ref_day, info = dict_first_diff(mine_day, ref_day))
  expect_identical(mine_per, ref_per, info = dict_first_diff(mine_per, ref_per))
  # the one column EE's person summary has and MOS2's does not
  expect_true("ACC_spt_wake_MOD_mg_pla" %in% d$personsummary$Variable)
})

test_that("the shipped MOS2 person dictionary is wrong for WW, and the port can say so", {
  dict_skip_ref()
  res <- dict_runs()$MOS2
  skip_if_not(dir.exists(res))
  mm <- dict_cnames(file.path(res, "part5_personsummary_MM_L40M100V400_T5A5.csv"))
  ww <- dict_cnames(file.path(res, "part5_personsummary_WW_L40M100V400_T5A5.csv"))
  expect_identical(length(mm), 207L)
  expect_identical(length(ww), 206L)
  expect_identical(setdiff(mm, ww), c("ACC_spt_wake_MOD_mg_pla", "ACC_spt_wake_MOD_mg_wei"))
  expect_identical(setdiff(ww, mm), "ACC_spt_wake_MOD_mg")
  # GGIR ships only the MM dictionary, which names a column WW lacks and misses one it
  # has; here the caller chooses the report
  shipped <- read.csv(file.path(res, "variableDictionary", "part5_dictionary_personsummary.csv"),
                      stringsAsFactors = FALSE)
  expect_true("ACC_spt_wake_MOD_mg_pla" %in% shipped$Variable)
  expect_false("ACC_spt_wake_MOD_mg" %in% shipped$Variable)
  dww <- raw.timeuse.dictionary(personsummary = ww)$personsummary
  expect_identical(nrow(dww), 206L)
  expect_true("ACC_spt_wake_MOD_mg" %in% dww$Variable)
  expect_false("ACC_spt_wake_MOD_mg_pla" %in% dww$Variable)
  # the trailing space is GGIR's own, from paste(def, NULL) when there is no aggregation
  expect_identical(dww$Definition[dww$Variable == "ACC_spt_wake_MOD_mg"],
                   "Mean acceleration in awake moderate physical activity during the sleep period time (mili-gravity units) ")
})

# LIVE FUNCTION

test_that("the port matches a live GGIR:::g.report.part5_dictionary run", {
  dict_skip_ref()
  dict_skip_ggir()
  res <- dict_runs()$MOS2
  skip_if_not(dir.exists(res))
  root <- file.path(tempdir(), "canhrActi_p5dict_live")
  unlink(root, recursive = TRUE)
  dir.create(file.path(root, "results", "QC"), recursive = TRUE, showWarnings = FALSE)
  for (f in c("part5_daysummary_MM_L40M100V400_T5A5.csv",
              "part5_daysummary_WW_L40M100V400_T5A5.csv",
              "part5_personsummary_MM_L40M100V400_T5A5.csv",
              "part5_personsummary_WW_L40M100V400_T5A5.csv")) {
    file.copy(file.path(res, f), file.path(root, "results", f))
  }
  po <- GGIR::load_params()$params_output
  GGIR:::g.report.part5_dictionary(metadatadir = normalizePath(root, winslash = "/"),
                                   params_output = po)
  vd <- file.path(root, "results", "variableDictionary")
  expect_true(dir.exists(vd))
  expect_identical(sort(basename(dir(vd))),
                   c("part5_dictionary_daysummary.csv", "part5_dictionary_personsummary.csv"))
  # GGIR's dir() picks the MM pair for both
  dn <- dict_cnames(file.path(res, "part5_daysummary_MM_L40M100V400_T5A5.csv"))
  pn <- dict_cnames(file.path(res, "part5_personsummary_MM_L40M100V400_T5A5.csv"))
  d <- raw.timeuse.dictionary(dn, pn)
  live_day <- readLines(file.path(vd, "part5_dictionary_daysummary.csv"))
  live_per <- readLines(file.path(vd, "part5_dictionary_personsummary.csv"))
  mine_day <- dict_write_lines(d$daysummary)
  mine_per <- dict_write_lines(d$personsummary)
  expect_identical(mine_day, live_day, info = dict_first_diff(mine_day, live_day))
  expect_identical(mine_per, live_per, info = dict_first_diff(mine_per, live_per))
})

# THE TWO DEFECTS

test_that("defect 1, the leaked tokens, is reproduced and ggir_exact = FALSE removes it", {
  dict_skip_ref()
  res <- dict_runs()$MOS2
  skip_if_not(dir.exists(res))
  dn <- dict_cnames(file.path(res, "part5_daysummary_MM_L40M100V400_T5A5.csv"))
  pn <- dict_cnames(file.path(res, "part5_personsummary_MM_L40M100V400_T5A5.csv"))
  exact <- raw.timeuse.dictionary(dn, pn)$personsummary
  fixed <- raw.timeuse.dictionary(dn, pn, ggir_exact = FALSE)$personsummary
  # the six parameter-echo columns that follow Nblocks_day_total_VIG_pla inherit its tokens
  leaked <- c("boutcriter.in", "boutcriter.lig", "boutcriter.mvpa",
              "boutdur.in", "boutdur.lig", "boutdur.mvpa")
  i <- match(leaked, exact$Variable)
  expect_false(anyNA(i))
  expect_true(all(grepl(" - plain average$", exact$Definition[i])))
  expect_false(any(grepl("- plain average", fixed$Definition[i])))
  expect_identical(exact$Definition[exact$Variable == "boutdur.in"],
                   "Duration/s of inactivity bouts (min) - plain average")
  expect_identical(fixed$Definition[fixed$Variable == "boutdur.in"],
                   "Duration/s of inactivity bouts (min)")
  # the column whose tokens leaked into them
  expect_identical(exact$Variable[i[1] - 1L], "Nblocks_day_total_VIG_pla")
  # 28 person rows differ: the 6 leaked and the 22 with only a trailing space
  expect_identical(sum(exact$Definition != fixed$Definition), 28L)
})

test_that("the leak crosses the report boundary, because GGIR runs both in one call", {
  # the MOS2 day summary ends on daytype, whose tokens hold no "pla", so a synthetic
  # day summary whose last column holds "pla" is needed to show the leak
  day <- c("ID", "dur_day_pla_min")
  person <- c("filename", "ID")
  both <- raw.timeuse.dictionary(day, person)
  solo <- raw.timeuse.dictionary(personsummary = person)
  expect_identical(both$personsummary$Definition, c("File name - plain average",
                                                    "File/participant identifier - plain average"))
  # alone there are no leftover tokens; GGIR itself would raise "object 'elements' not found"
  expect_identical(solo$personsummary$Definition, c("File name ", "File/participant identifier "))
  clean <- raw.timeuse.dictionary(day, person, ggir_exact = FALSE)
  expect_identical(clean$personsummary$Definition, c("File name", "File/participant identifier"))
})

test_that("defect 2, the trailing space, is reproduced and ggir_exact = FALSE removes it", {
  dict_skip_ref()
  res <- dict_runs()$MOS2
  skip_if_not(dir.exists(res))
  dn <- dict_cnames(file.path(res, "part5_daysummary_MM_L40M100V400_T5A5.csv"))
  pn <- dict_cnames(file.path(res, "part5_personsummary_MM_L40M100V400_T5A5.csv"))
  exact <- raw.timeuse.dictionary(dn, pn)
  fixed <- raw.timeuse.dictionary(dn, pn, ggir_exact = FALSE)
  expect_identical(sum(grepl(" $", exact$daysummary$Definition)), 52L)
  expect_identical(sum(grepl(" $", exact$personsummary$Definition)), 22L)
  expect_identical(sum(grepl(" $", fixed$daysummary$Definition)), 0L)
  expect_identical(sum(grepl(" $", fixed$personsummary$Definition)), 0L)
  # the shipped csv carries the space: "filename,File name "
  expect_identical(exact$daysummary$Definition[exact$daysummary$Variable == "filename"],
                   "File name ")
  # the day summary differs between the two settings by the trailing space alone
  expect_identical(sum(exact$daysummary$Definition != fixed$daysummary$Definition), 52L)
  expect_identical(trimws(exact$daysummary$Definition, which = "right"),
                   fixed$daysummary$Definition)
})

test_that("GGIRversion keeps an empty definition at both settings, and it is not NA", {
  d <- raw.timeuse.dictionary("GGIRversion", "GGIRversion")
  # the parser falls through to what = "", which is not NULL, so the blank-definition check never fires
  expect_identical(d$daysummary$Definition, "")
  expect_identical(d$personsummary$Definition, "")
  expect_false(is.na(d$daysummary$Definition))
  expect_identical(raw.timeuse.dictionary("GGIRversion", ggir_exact = FALSE)$daysummary$Definition,
                   "")
})

# TOKEN PARSER

test_that("the token parser builds what, class, window and unit in GGIR's order", {
  nms <- c("dur_day_total_IN_min", "dur_day_IN_unbt_min", "dur_day_MVPA_bts_10_min",
           "dur_day_IN_bts_20_30_min", "ACC_spt_sleep_mg", "ACC_day_mg_median",
           "ACC_day_mg_stdev", "nonwear_perc_day_spt", "nonwear_perc_spt",
           "quantile_mostactive60min_mg", "quantile_mostactive30min_mg",
           "sleep_efficiency_after_onset", "sleeponset", "sleeponset_ts", "wakeup_ts",
           "Nbouts_day_IN_bts_30", "Nblocks_spt_wake_LIG", "dur_spt_wake_IN_min",
           # the unbouted branch needs the bout columns of the same report to be present
           "dur_day_IN_bts_30_min", "dur_day_IN_bts_10_20_min")
  d <- raw.timeuse.dictionary(nms)$daysummary
  g <- function(v) d$Definition[d$Variable == v]
  expect_identical(g("dur_day_total_IN_min"),
                   "Time accumulated in inactivity during the waking time (minutes)")
  expect_identical(g("dur_day_IN_unbt_min"),
                   "Time accumulated in unbouted inactivity (0-10 min) during the waking time (minutes)")
  expect_identical(g("dur_day_MVPA_bts_10_min"),
                   paste("Time accumulated in bouts of 10 min moderate-to-vigorous physical",
                         "activity during the waking time (minutes)"))
  expect_identical(g("dur_day_IN_bts_20_30_min"),
                   "Time accumulated in bouts of 20-30 min inactivity during the waking time (minutes)")
  expect_identical(g("ACC_spt_sleep_mg"),
                   "Mean acceleration in sleep during the sleep period time (mili-gravity units)")
  expect_identical(g("ACC_day_mg_median"), "Median acceleration during the waking time (mili-gravity units)")
  expect_identical(g("ACC_day_mg_stdev"),
                   "Standard deviation of acceleration during the waking time (mili-gravity units)")
  # nonwear is a what of "Time accumulated" and a class of "non-wear time"
  expect_identical(g("nonwear_perc_day_spt"),
                   paste("Time accumulated in non-wear time during the waking time and sleep",
                         "period time (i.e., full window) (%)"))
  expect_identical(g("nonwear_perc_spt"),
                   "Time accumulated in non-wear time during the sleep period time (%)")
  # the two quantile names, whose class overwrites rather than appends and takes no "in"
  expect_identical(g("quantile_mostactive60min_mg"),
                   paste("Acceleration above which (percentile) the most active 60 minutes of",
                         "the day are accumulated (mili-gravity units)"))
  expect_identical(g("quantile_mostactive30min_mg"),
                   paste("Acceleration above which (percentile) the most active 30 minutes of",
                         "the day are accumulated (mili-gravity units)"))
  # the sleep-efficiency override, which also sets the unit and takes no "in"
  expect_identical(g("sleep_efficiency_after_onset"), "Sleep efficiency after onset (%)")
  expect_identical(g("sleeponset"), "Sleep onset time (hours from previous midnight)")
  expect_identical(g("sleeponset_ts"), "Sleep onset time (hh:mm:ss)")
  expect_identical(g("wakeup_ts"), "Wake up time (hh:mm:ss)")
  # Nbouts suppresses the bout class, Nblocks does not
  expect_identical(g("Nbouts_day_IN_bts_30"),
                   "Number of bouts in inactivity during the waking time ")
  expect_identical(g("Nblocks_spt_wake_LIG"),
                   paste("Number of blocks (defined as consecutive series of epochs with the",
                         "same behavioural class) in awake light physical activity during the",
                         "sleep period time "))
  # "wake" prefixes the class and is not itself in the class list
  expect_identical(g("dur_spt_wake_IN_min"),
                   "Time accumulated in awake inactivity during the sleep period time (minutes)")
})

test_that("the parser handles the fragmentation, intensity-gradient and daytype names", {
  nms <- c("FRAG_Nfrag_PA2IN_day", "FRAG_TP_IN2PA_day", "FRAG_mean_dur_IN_day",
           "FRAG_Gini_dur_PA_day", "FRAG_NFragPM_IN_day", "ig_gradient_day",
           "ig_intercept_day", "ig_rsquared_day", "daytype", "weekday", "window_number")
  d <- raw.timeuse.dictionary(nms)$daysummary
  g <- function(v) d$Definition[d$Variable == v]
  expect_identical(g("FRAG_Nfrag_PA2IN_day"),
                   paste("Fragmentation analysis: number of fragments in physical activity to",
                         "inactivity during the waking time "))
  expect_identical(g("FRAG_TP_IN2PA_day"),
                   paste("Fragmentation analysis: transition probability (%) in inactivity to",
                         "physical activity during the waking time "))
  expect_identical(g("FRAG_mean_dur_IN_day"),
                   paste("Fragmentation analysis: mean duration in the time accumulated in",
                         "inactivity during the waking time "))
  expect_identical(g("FRAG_NFragPM_IN_day"),
                   paste("Fragmentation analysis: number of fragments per minute in inactivity",
                         "during the waking time "))
  # the intensity gradient takes no "in" connector
  expect_identical(g("ig_gradient_day"), "Intensity gradient during the waking time ")
  expect_identical(g("ig_rsquared_day"),
                   paste("R-squared from the log-log time to intensity regression to calculate",
                         "the intensity gradient during the waking time "))
  # daytype is in baseDictionary and in the parser; the lookup wins
  expect_identical(g("daytype"), "WD = weekday; WE = weekend day ")
  expect_identical(g("weekday"), "Day of the week (full name) ")
  expect_identical(g("window_number"), "Window number in the recording ")
})

test_that("the person summary adds the aggregation suffix and rewrites the Nvalid parts", {
  nms <- c("Nvaliddays", "Nvaliddays_WD", "Nvaliddays_WE", "dur_day_total_IN_min_pla",
           "dur_day_total_IN_min_wei", "dur_day_total_IN_min_WD", "dur_day_total_IN_min_WE")
  d <- raw.timeuse.dictionary(personsummary = nms)$personsummary
  g <- function(v) d$Definition[d$Variable == v]
  # the Nvalid names are looked up whole, so _WD and _WE stay, and the part numbers are rewritten
  expect_identical(g("Nvaliddays"),
                   "Number of valid days based on the cleaning parameters for part 5 ")
  expect_identical(g("Nvaliddays_WD"),
                   "Number of valid weekdays based on the cleaning parameters for part 5 ")
  expect_identical(g("Nvaliddays_WE"),
                   "Number of valid weekend days based on the cleaning parameters for part 5 ")
  # no aggregation suffix on those
  expect_false(any(grepl("average", g("Nvaliddays"))))
  base <- "Time accumulated in inactivity during the waking time (minutes)"
  expect_identical(g("dur_day_total_IN_min_pla"), paste(base, "- plain average"))
  expect_identical(g("dur_day_total_IN_min_wei"), paste(base, "- weighted average"))
  expect_identical(g("dur_day_total_IN_min_WD"), paste(base, "- weekdays average"))
  expect_identical(g("dur_day_total_IN_min_WE"), paste(base, "- weekend days average"))
  # the day summary of the same names carries no suffix at all
  dd <- raw.timeuse.dictionary(nms)$daysummary
  expect_false(any(grepl("average", dd$Definition)))
})

test_that(".raw.dictionary.class splits a transition token and joins two classes", {
  expect_null(.raw.dictionary.class(character(0)))
  expect_identical(.raw.dictionary.class("IN"), "inactivity")
  expect_identical(.raw.dictionary.class("MVPA"), "moderate-to-vigorous physical activity")
  expect_identical(.raw.dictionary.class("PA2IN"), "physical activity to inactivity")
  expect_identical(.raw.dictionary.class("sleep2wake"), "sleep to awake")
  expect_identical(.raw.dictionary.class("IN2LIPA"), "inactivity to light physical activity")
  # the two mostactive tokens overwrite the whole class rather than one element
  expect_identical(.raw.dictionary.class("mostactive60min"),
                   "the most active 60 minutes of the day are accumulated")
  # an unknown token gives NULL, which the caller treats as "no class"
  expect_null(.raw.dictionary.class("nosuchclass"))
})

test_that("the LUX branch names the segment and the threshold range", {
  # No reference recording has a lightpeak channel; the names are GGIR's own, from
  # g.part5_analyseSegment and g.part5.lux_persegment.
  nms <- c("LUX_max_day", "LUX_mean_day", "LUX_mean_spt", "LUX_mean_day_mvpa",
           "LUX_min_0_10_day", "LUX_min_1000_inf_day", "LUX_above1000_0_8hr_day",
           "LUX_timeawake_8_16hr_day", "LUX_mean_16_24hr_day", "LUX_imputed_0_8hr_day",
           "LUX_ignored_0_8hr_day")
  d <- raw.timeuse.dictionary(nms)$daysummary
  g <- function(v) d$Definition[d$Variable == v]
  expect_identical(g("LUX_max_day"), "Maximum LUX value during the waking time (luxes)")
  expect_identical(g("LUX_mean_day"), "Mean LUX value during the waking time (luxes)")
  expect_identical(g("LUX_mean_spt"), "Mean LUX value during the sleep period time (luxes)")
  expect_identical(g("LUX_mean_day_mvpa"),
                   paste("Mean LUX value in moderate-to-vigorous physical activity time",
                         "during the waking time (luxes)"))
  expect_identical(g("LUX_min_0_10_day"),
                   "Time accumulated with LUX between 0 and 10 luxes during the waking time (minutes)")
  # "inf" parses to Inf, so the two-number branch is skipped for the open-ended range
  expect_identical(g("LUX_min_1000_inf_day"),
                   "Time accumulated with LUX above 1000 luxes during the waking time (minutes)")
  expect_identical(g("LUX_above1000_0_8hr_day"),
                   paste("Time accumulated with LUX above 1000 luxes in the segment from 0:00",
                         "to 8:00 during the waking time (minutes)"))
  expect_identical(g("LUX_timeawake_8_16hr_day"),
                   paste("Time classified as awake in the segment from 8:00 to 16:00 during",
                         "the waking time (minutes)"))
  # the segment start is padded to hh:00 only when it is one character long, so 16 is
  # written "16" and 0 is written "0:00"
  expect_identical(g("LUX_mean_16_24hr_day"),
                   "Mean LUX value in the segment from 16 to 24:00 during the waking time (luxes)")
  expect_identical(g("LUX_imputed_0_8hr_day"),
                   paste("Time in which the LUX has been imputed in the segment from 0:00 to",
                         "8:00 during the waking time (minutes)"))
  expect_identical(g("LUX_ignored_0_8hr_day"),
                   paste("Time in which the LUX has been ignored in the segment from 0:00 to",
                         "8:00 during the waking time (minutes)"))
})

test_that("the LUX names match a live GGIR:::g.report.part5_dictionary run", {
  dict_skip_ggir()
  nms <- c("LUX_max_day", "LUX_mean_day", "LUX_mean_spt", "LUX_mean_day_mvpa",
           "LUX_min_0_10_day", "LUX_min_1000_inf_day", "LUX_above1000_0_8hr_day",
           "LUX_timeawake_8_16hr_day", "LUX_mean_16_24hr_day", "LUX_imputed_0_8hr_day",
           "LUX_ignored_0_8hr_day")
  root <- file.path(tempdir(), "canhrActi_p5dict_lux")
  unlink(root, recursive = TRUE)
  dir.create(file.path(root, "results", "QC"), recursive = TRUE, showWarnings = FALSE)
  df <- as.data.frame(matrix(1, nrow = 2, ncol = length(nms)))
  names(df) <- nms
  data.table::fwrite(df, file.path(root, "results", "part5_daysummary_MM_L40M100V400_T5A5.csv"))
  GGIR:::g.report.part5_dictionary(metadatadir = normalizePath(root, winslash = "/"),
                                   params_output = GGIR::load_params()$params_output)
  live <- read.csv(file.path(root, "results", "variableDictionary",
                             "part5_dictionary_daysummary.csv"), stringsAsFactors = FALSE)
  mine <- raw.timeuse.dictionary(nms)$daysummary
  expect_identical(mine$Variable, live$Variable)
  expect_identical(mine$Definition, live$Definition)
})

# ARGUMENTS AND SHAPES

test_that("the three input shapes agree and the two members are always present", {
  nms <- c("ID", "filename", "dur_day_total_IN_min")
  chr <- raw.timeuse.dictionary(nms, nms)
  df <- raw.timeuse.dictionary(as.data.frame(matrix("", 1, 3, dimnames = list(NULL, nms))),
                               as.data.frame(matrix("", 1, 3, dimnames = list(NULL, nms))))
  mx <- raw.timeuse.dictionary(matrix(0, 1, 3, dimnames = list(NULL, nms)), nms)
  expect_identical(chr, df)
  expect_identical(chr, mx)
  expect_identical(names(chr), c("daysummary", "personsummary"))
  only_day <- raw.timeuse.dictionary(nms)
  expect_null(only_day$personsummary)
  expect_identical(only_day$daysummary, chr$daysummary)
  expect_null(raw.timeuse.dictionary()$daysummary)
  expect_null(raw.timeuse.dictionary()$personsummary)
  # a data.table also works, which is what fread hands back
  skip_if_not_installed("data.table")
  dt <- data.table::data.table(ID = "a", filename = "b", dur_day_total_IN_min = 1)
  expect_identical(raw.timeuse.dictionary(dt)$daysummary, chr$daysummary)
})

test_that("the argument checks reject what cannot be described", {
  expect_error(raw.timeuse.dictionary("ID", ggir_exact = NA), "TRUE or FALSE")
  expect_error(raw.timeuse.dictionary("ID", ggir_exact = "yes"), "TRUE or FALSE")
  expect_error(raw.timeuse.dictionary(character(0)), "no column names")
  expect_error(raw.timeuse.dictionary(c("ID", "")), "must have a name")
  expect_error(raw.timeuse.dictionary(personsummary = c("ID", NA)), "must have a name")
})
