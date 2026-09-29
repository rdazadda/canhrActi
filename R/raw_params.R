# Ported from GGIR 3.3-9 R/load_params.R, R/check_params.R and R/extract_params.R
# (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; copyright holders Medical Research
# Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Changes: the eight GGIR parameter
# objects are reduced to the members that reach parts 1, 3, 4 and 5 plus the part-2 wear
# decision and merged into one flat list; the config.csv reader and the checks that only
# concern parts 2 and 6 are dropped; unknown names are an error; the canhrActi-only
# switches, four extra validations and a print method are added.

#' Raw Accelerometer Pipeline Parameters
#'
#' Builds the parameter object used by the raw accelerometer pipeline
#' (\code{raw.inspect}, \code{raw.calibrate}, \code{raw.getmeta},
#' \code{read.raw.accelerometer}) and by the ported GGIR parts 3, 4 and 5. Members,
#' defaults, type checks and coercions are GGIR's, so a call with the defaults
#' reproduces a GGIR run.
#'
#' @param ... Named parameter values overriding the defaults. An unknown name is an
#'   error. \code{NULL} sets the member to \code{NULL}.
#'
#' @details
#' The members are the subset of GGIR's \code{params_rawdata}, \code{params_general},
#' \code{params_metrics}, \code{params_cleaning}, \code{params_sleep},
#' \code{params_output}, \code{params_phyact} and \code{params_247} that reaches parts
#' 1, 3, 4 and 5 and the part-2 wear decision. All groups sit in one flat object, so
#' every GGIR cross-check runs, as in a full GGIR run. Members and GGIR defaults:
#'
#' \strong{rawdata}: chunksize 1 (floored at 0.1), spherecrit 0.3, minloadcrit 168, do.cal
#' TRUE, backup.cal.coef NULL, dynrange NULL, minimumFileSizeMB 2, rmc.dec ".",
#' rmc.firstrow.acc NULL, rmc.firstrow.header NULL, rmc.header.length NULL, rmc.col.acc 1:3,
#' rmc.col.temp NULL, rmc.col.time NULL, rmc.unit.acc "g", rmc.unit.temp "C", rmc.unit.time
#' "POSIX", rmc.format.time "%Y-%m-%d %H:%M:%OS", rmc.bitrate NULL, rmc.dynamic_range
#' NULL, rmc.unsignedbit TRUE, rmc.origin "1970-01-01", rmc.sf NULL, rmc.headername.sf NULL,
#' rmc.headername.sn NULL, rmc.headername.recordingid NULL, rmc.header.structure NULL,
#' rmc.check4timegaps FALSE, rmc.noise 13, nonwear_range_threshold 150, rmc.col.wear NULL,
#' rmc.doresample FALSE, interpolationType 1, imputeTimegaps TRUE, frequency_tol 0.1,
#' rmc.scalefactor.acc 1. backup.cal.coef is NULL rather than GGIR's "retrieve" because
#' there is no output folder to read coefficients from; pass a stored calibration to
#' \code{read.raw.accelerometer(calibration = )} instead.
#'
#' \strong{general}: acc.metric "ENMO", windowsizes c(5, 900, 3600), desiredtz "", configtz
#' NULL, idloc 1, dayborder 0, part5_agg2_60seconds FALSE, sensor.location "wrist",
#' recordingEndSleepHour NULL. part5_agg2_60seconds averages the epoch series to 60 s
#' before part 5 classifies it; the thresholds stay relative to the original epoch.
#' dayborder is read by parts 1, 2 and 5; parts 3 and 4 always use midnight.
#'
#' \strong{metrics}: do.anglex FALSE, do.angley FALSE, do.anglez TRUE, do.zcx FALSE, do.zcy
#' FALSE, do.zcz FALSE, do.enmo TRUE, do.lfenmo FALSE, do.en FALSE, do.mad FALSE, do.enmoa
#' FALSE, do.roll_med_acc_x/y/z FALSE, do.dev_roll_med_acc_x/y/z FALSE, do.bfen FALSE,
#' do.hfen FALSE, do.hfenplus FALSE, do.lfen FALSE, do.lfx/y/z FALSE, do.hfx/y/z FALSE,
#' do.bfx/y/z FALSE, do.neishabouricounts FALSE, hb 15, lb 0.2, n 4, zc.lb 0.25, zc.hb 3,
#' zc.sb 0.01, zc.order 2, zc.scale 1, actilife_LFE FALSE.
#'
#' \strong{cleaning}: includedaycrit 16, data_masking_strategy 1, maxdur 0, hrs.del.start
#' 0, hrs.del.end 0, includedaycrit.part5 2/3, excludefirstlast.part5 FALSE,
#' data_cleaning_file NULL, minimum_MM_length.part5 23, excludefirstlast FALSE,
#' includenightcrit 16, excludefirst.part4 FALSE, excludelast.part4 FALSE,
#' nonWearEdgeCorrection TRUE, nonwear_approach "2023", segmentWEARcrit.part5 0.5,
#' segmentDAYSPTcrit.part5 c(0.9, 0), includenightcrit.part5 0, nonwearFiltermaxHours
#' NULL, nonwearFilterWindow NULL. includedaycrit.part5 and includenightcrit.part5 are a
#' fraction of the window's waking (sleep period) hours when between 0 and 1 and a number of
#' hours when in (1, 24]. minimum_MM_length.part5 is in hours. Part 5 reads the second
#' element of includedaycrit as its full-window criterion; a scalar leaves part 5 without
#' one. includenightcrit is the part-4 per-night minimum and includedaycrit the part-2
#' per-day minimum; they are separate members.
#'
#' \strong{output}: save_ms5rawlevels TRUE, save_ms5raw_format "RData",
#' save_ms5raw_without_invalid FALSE, storefolderstructure FALSE, timewindow
#' c("MM", "WW"), week_weekend_aggregate.part5 FALSE, do.part3.pdf FALSE, outliers.only
#' FALSE, criterror 3, do.visual TRUE, do.sibreport TRUE, sep_reports ",", dec_reports
#' ".", require_complete_lastnight_part5 FALSE, method_research_vars NULL. timewindow
#' selects the part-5 day definitions ("MM" midnight to midnight, "WW" waking to waking,
#' "OO" onset to onset); each produces its own rows, told apart by the \code{window}
#' column. save_ms5raw_without_invalid defaults to FALSE, the effective default of an
#' ordinary GGIR run (GGIR declares TRUE and its visualreport default forces FALSE).
#'
#' \strong{phyact}: boutcriter.in 0.9, boutcriter.lig 0.8, boutcriter.mvpa 0.8,
#' threshold.lig 40, threshold.mod 100, threshold.vig 400, boutdur.mvpa c(1, 5, 10),
#' boutdur.in c(10, 20, 30), boutdur.lig c(1, 5, 10), frag.metrics NULL. The thresholds
#' are in mg for a g-unit metric and in counts per epoch for a count metric; all three
#' may be vectors and part 5 computes every combination. The defaults are GGIR's, from
#' Hildebrand (2014, 2016) for ActiGraph, non-dominant wrist, ENMO, adults, rounded from
#' 44.8, 100.6 and 428.8 mg. For older adults Migueles (2021) gives light 18 and moderate
#' 60 for the same placement (22 and 64 dominant wrist, 7 and 14 hip), Sanders (2019)
#' 20 and 32 for a GENEActiv non-dominant wrist and Bammann (2021) moderate 100 and
#' vigorous 245 for ActiGraph; the choice changes the minutes substantially and belongs
#' in the methods section. Bout durations are in minutes and \code{raw.timeuse()} sorts
#' them decreasing before use. boutcriter.* is the fraction of a centred window that must
#' satisfy the intensity test. frag.metrics is "mean", "TP", "Gini", "power", "CoV",
#' "NFragPM" or "all"; NULL skips fragmentation.
#'
#' \strong{247}: qwindow c(0, 24), qwindow_dateformat "%d-%m-%Y", iglevels NULL,
#' LUXthresholds c(0, 100, 500, 1000, 3000, 5000, 10000), LUX_cal_constant NULL,
#' LUX_cal_exponent NULL, LUX_day_segments NULL, clevels c(30, 150). qwindow splits every
#' window at those clock hours; c(0, 24) is one segment per window. iglevels holds the
#' intensity gradient bin edges in mg; a length-1 value expands to
#' \code{c(seq(0, 4000, by = 25), 8000)} and NULL skips the gradient. LUX_day_segments
#' gates all lux output, which needs a lightpeak channel a .gt3x does not carry. clevels
#' is inert in GGIR itself.
#'
#' \strong{sleep} (43 members): anglethreshold 5, timethreshold 5, ignorenonwear TRUE,
#' HASPT.algo "HDCZA", HASIB.algo "vanHees2015", Sadeh_axis "Y" (blanked to "" unless
#' HASIB.algo is count-based), longitudinal_axis NULL, HASPT.ignore.invalid FALSE,
#' loglocation NULL, colid 1, coln1 2, relyonguider FALSE, def.noc.sleep 1,
#' sleepwindowType "SPT", possible_nap_window NULL, possible_nap_dur NULL,
#' possible_nap_gap 0, possible_nap_edge_acc Inf, nap_model NULL,
#' sleepefficiency.metric 1, HDCZA_threshold NULL, oakley_threshold 20,
#' consider_marker_button FALSE, impute_marker_button FALSE,
#' sib_must_fully_overlap_with_TimeInBed c(TRUE, TRUE), nap_markerbutton_method 0,
#' nap_markerbutton_max_distance 30, SRI1_smoothing_wsize_hrs NULL,
#' SRI1_smoothing_frac NULL, spt_min_block_dur 30, spt_max_gap_dur 60, spt_max_gap_ratio 1,
#' HorAngle_threshold 60, guider_cor_maxgap_hrs 2, guider_cor_min_frac_sib 0.5,
#' guider_cor_min_hrs 2, guider_cor_meme_frac_out 0.9, guider_cor_meme_frac_in 0.4,
#' guider_cor_meme_min_hrs 1, guider_cor_do FALSE, guider_cor_meme_min_dys 3,
#' HDCZA_roll_windowsize 5, LowAcc_threshold 0.014. The last two are new in GGIR 3.3-9;
#' at the defaults GGIR 3.3.6 gives the same numbers. The nap members are part-5 only:
#' possible_nap_window is in clock hours, possible_nap_dur and possible_nap_gap in
#' minutes (GGIR's help page says seconds for the gap), and nap_model is carried but
#' unread. nnights, sleeplogsep and relyonsleeplog are not carried because GGIR never
#' reads them; use relyonguider.
#'
#' \strong{canhrActi only}: ggir_exact TRUE, rename_uppercase FALSE (rename a .GT3X on
#' disk as GGIR does, instead of copying it to a temporary lowercase name),
#' skip_small_files FALSE (drop files at or below minimumFileSizeMB as GGIR does, instead
#' of inspecting and flagging them), unzip_once TRUE (extract a .gt3x once and read every
#' block from the extraction instead of re-extracting the archive per block;
#' read.raw.accelerometer removes the extraction when it returns, and so does raw.inspect,
#' raw.calibrate or raw.getmeta called on its own), decode_once FALSE (decode the whole
#' file once through read.gt3x), stream_gt3x TRUE (read each block straight from the
#' extracted log.bin with canhrActi's own reader, which allocates only the block and hands
#' any file or block it was not checked on to read.gt3x; implies unzip_once),
#' progress NULL (a callback \code{function(stage, i, n, message)}), ggir_version_label
#' "3.3.6" (the GGIR the port is proven against, copied verbatim into the "GGIR version"
#' column of the part-2 summary and the GGIRversion column of the part-5 tables so they
#' can be \code{identical()} to GGIR's).
#'
#' The sleep coercions are GGIR's, applied in GGIR's order and not repaired. A
#' HASPT.algo whose first element is not "HorAngle", "NotWorn", "MotionWare", "HLRB" or
#' "LowAcc" becomes "HDCZA", so c("HDCZA", "NotWorn") collapses to "HDCZA"; a
#' def.noc.sleep of length 2 sets it to "notused". A nap window with a nap duration
#' forces do.sibreport, save_ms5rawlevels and the RData format. A count-based
#' HASIB.algo turns on the zero-crossing metric of Sadeh_axis; any other blanks
#' Sadeh_axis, so a second pass over a checked object warns. sensor.location "hip"
#' forces the three angle metrics and HASPT.algo "HorAngle". HASPT.algo "HorAngle"
#' forces sleepwindowType "TimeInBed" and, after the hip block has run, sensor.location
#' "hip". No loglocation forces sleepwindowType back to "SPT", which loses part 4's
#' sleep latency and efficiency columns. An unrecognised HASIB.algo is rejected later,
#' by the sustained inactivity dispatch.
#'
#' ggir_exact reproduces ten GGIR behaviours that change numbers and are needed for
#' \code{identical()} parity; FALSE selects the corrected arithmetic. Parts 3 and 4: the
#' partial-first-day NA in SPTE_start and SPTE_end; the \code{midn_start == 0}
#' off-by-one in the SPTE vectors; the time shifts in the count-based
#' sustained-inactivity algorithms; the marker-button hour guard mis-scaled by
#' \code{3600/ws3}. Part 5: the vacuous end-of-bout gap test and the short bouts of
#' \code{g.getbout}; the unconditional \code{- 2} in N_atleast5minwakenight; dur_spt_min
#' zeroed in MM windows while dur_spt_sleep_min is not; the \code{x0.5}
#' mis-parenthesisation and the xmin collapse in the fragmentation power-law fit; the
#' NA-angle repair indexing the wrong frame. The missing NA guard in
#' \code{g.part5.definedays} is fixed at both settings, and the parameter coercions are
#' GGIR's at both settings.
#'
#' Members not carried because they do not change the numbers of parts 1, 3, 4 or 5
#' (passing one is an error): printsummary, print.filename, overwrite, maxNcores,
#' do.parallel, use_trycatch_serial, expand_tail_max_hours, dataFormat,
#' maxRecordingInterval, extEpochData_timeformat, recording_split_*, do.brondcounts,
#' rmc.desiredtz, rmc.configtz, strategy, mvpathreshold, boutcriter, mvpadur,
#' part6_threshold_combi, visualreport, part6HCA, part6CR, and the rest of params_247,
#' params_phyact, params_cleaning and params_output.
#'
#' Four validations GGIR does not make: timewindow must name only "MM", "WW" or "OO"
#' (an unknown name makes GGIR's window loop spin forever); threshold.lig at most
#' threshold.mod and threshold.mod at most threshold.vig for every combination; each
#' boutcriter.* is a single number in (0, 1]; includedaycrit.part5 above 25 is an error.
#'
#' @return A named list of class \code{"canhrActi_raw_params"} holding every member after
#'   coercion, with attribute \code{"groups"} naming the GGIR parameter object each member
#'   came from ("rawdata", "general", "metrics", "cleaning", "sleep", "output", "phyact",
#'   "247" or "canhrActi").
#'
#' @seealso \code{\link{raw.sleep.params}} for the sleep group on its own.
#'
#' @examples
#' p <- raw.params()
#' p$windowsizes
#' p$HASPT.algo
#' p$threshold.lig
#' p <- raw.params(desiredtz = "America/Anchorage", do.en = TRUE)
#' print(p)
#' # the Migueles 2021 older-adult wrist cut-points instead of GGIR's adult defaults
#' raw.params(threshold.lig = 18, threshold.mod = 60)$threshold.lig
#'
#' @export
raw.params <- function(...) {
  params <- .raw.params.defaults()
  dots <- list(...)
  if (length(dots) > 0) {
    nms <- names(dots)
    if (is.null(nms) || any(is.na(nms)) || any(nms == "")) {
      stop("All arguments to raw.params() must be named, for example raw.params(desiredtz = \"UTC\").",
           call. = FALSE)
    }
    unknown <- nms[!(nms %in% names(params))]
    if (length(unknown) == 1) {
      stop(paste0("Parameter ", unknown, " is unknown to raw.params(), ",
                  "please check for typos or remove it."), call. = FALSE)
    } else if (length(unknown) > 1) {
      stop(paste0("Parameters ", paste0(unknown, collapse = " and "),
                  " are unknown to raw.params(), ",
                  "please check for typos or remove them."), call. = FALSE)
    }
    dup <- unique(nms[duplicated(nms)])
    if (length(dup) > 0) {
      stop(paste0("Parameter", if (length(dup) > 1) "s " else " ",
                  paste0(dup, collapse = " and "),
                  " provided more than once to raw.params()."), call. = FALSE)
    }
    # a NULL input keeps the member and sets it to NULL rather than dropping it
    for (aN in nms) {
      if (is.null(dots[[aN]])) {
        params[aN] <- list(NULL)
      } else {
        params[[aN]] <- dots[[aN]]
      }
    }
  }
  params <- .raw.params.check(params)
  structure(params, groups = .raw.params.groups(), class = "canhrActi_raw_params")
}

#' Default Values of the Raw Pipeline Parameters
#'
#' Internal. The uncoerced default list, transcribed from GGIR 3.3-9 \code{load_params}
#' in GGIR's order within each parameter object. The sleep block comes from
#' \code{.raw.sleep.params.defaults()}.
#'
#' @return A named list.
#' @keywords internal
#' @noRd
.raw.params.defaults <- function() {
  c(list(
    # params_rawdata
    chunksize = 1, spherecrit = 0.3, minloadcrit = 168,
    do.cal = TRUE, backup.cal.coef = NULL, dynrange = c(),
    minimumFileSizeMB = 2, rmc.dec = ".",
    rmc.firstrow.acc = c(), rmc.firstrow.header = c(),
    rmc.header.length = c(), rmc.col.acc = 1:3,
    rmc.col.temp = c(), rmc.col.time = c(),
    rmc.unit.acc = "g",  rmc.unit.temp = "C",
    rmc.unit.time = "POSIX", rmc.format.time = "%Y-%m-%d %H:%M:%OS",
    rmc.bitrate = c(),  rmc.dynamic_range = c(),
    rmc.unsignedbit = TRUE, rmc.origin = "1970-01-01",
    rmc.sf = c(),
    rmc.headername.sf = c(), rmc.headername.sn = c(),
    rmc.headername.recordingid = c(), rmc.header.structure = c(),
    rmc.check4timegaps = FALSE,  rmc.noise = 13, nonwear_range_threshold = 150,
    rmc.col.wear = c(), rmc.doresample = FALSE,
    interpolationType = 1,
    imputeTimegaps = TRUE, frequency_tol = 0.1, rmc.scalefactor.acc = 1,
    # params_general
    acc.metric = "ENMO",
    windowsizes = c(5, 900, 3600),
    desiredtz = "", configtz = c(), idloc = 1, dayborder = 0,
    part5_agg2_60seconds = FALSE,
    sensor.location = "wrist",
    recordingEndSleepHour = NULL,
    # params_metrics
    do.anglex = FALSE, do.angley = FALSE, do.anglez = TRUE,
    do.zcx = FALSE, do.zcy = FALSE, do.zcz = FALSE,
    do.enmo = TRUE, do.lfenmo = FALSE, do.en = FALSE,
    do.mad = FALSE, do.enmoa = FALSE,
    do.roll_med_acc_x = FALSE, do.roll_med_acc_y = FALSE,
    do.roll_med_acc_z = FALSE, do.dev_roll_med_acc_x = FALSE,
    do.dev_roll_med_acc_y = FALSE, do.dev_roll_med_acc_z = FALSE,
    do.bfen = FALSE, do.hfen = FALSE, do.hfenplus = FALSE, do.lfen = FALSE,
    do.lfx = FALSE, do.lfy = FALSE, do.lfz = FALSE,
    do.hfx = FALSE, do.hfy = FALSE, do.hfz = FALSE,
    do.bfx = FALSE, do.bfy = FALSE, do.bfz = FALSE,
    do.neishabouricounts = FALSE,
    hb = 15, lb = 0.2, n = 4,
    zc.lb = 0.25, zc.hb = 3, zc.sb = 0.01, zc.order = 2, zc.scale = 1,
    actilife_LFE = FALSE,
    # params_247
    qwindow = c(0, 24),
    qwindow_dateformat = "%d-%m-%Y",
    iglevels = c(),
    LUXthresholds = c(0, 100, 500, 1000, 3000, 5000, 10000),
    LUX_cal_constant = c(), LUX_cal_exponent = c(), LUX_day_segments = c(),
    clevels = c(30, 150),
    # params_phyact
    boutcriter.in = 0.9, boutcriter.lig = 0.8,
    boutcriter.mvpa = 0.8, threshold.lig = 40,
    threshold.mod = 100, threshold.vig = 400,
    boutdur.mvpa = c(1, 5, 10), boutdur.in = c(10, 20, 30),
    boutdur.lig = c(1, 5, 10), frag.metrics = c(),
    # params_cleaning
    includedaycrit = 16,
    data_masking_strategy = 1,
    maxdur = 0,
    hrs.del.start = 0, hrs.del.end = 0,
    includedaycrit.part5 = 2/3, excludefirstlast.part5 = FALSE,
    data_cleaning_file = c(), minimum_MM_length.part5 = 23,
    excludefirstlast = FALSE,
    includenightcrit = 16,
    excludefirst.part4 = FALSE,
    excludelast.part4 = FALSE,
    nonWearEdgeCorrection = TRUE, nonwear_approach = "2023",
    segmentWEARcrit.part5 = 0.5,
    segmentDAYSPTcrit.part5 = c(0.9, 0),
    includenightcrit.part5 = 0,
    nonwearFiltermaxHours = NULL,
    nonwearFilterWindow = NULL,
    # params_output. save_ms5raw_without_invalid is FALSE: GGIR's visualreport default forces it.
    save_ms5rawlevels = TRUE,
    save_ms5raw_format = "RData", save_ms5raw_without_invalid = FALSE,
    storefolderstructure = FALSE, timewindow = c("MM", "WW"),
    week_weekend_aggregate.part5 = FALSE, do.part3.pdf = FALSE,
    outliers.only = FALSE, criterror = 3, do.visual = TRUE,
    do.sibreport = TRUE,
    sep_reports = ",", dec_reports = ".",
    require_complete_lastnight_part5 = FALSE,
    method_research_vars = NULL
  ),
  # params_sleep
  .raw.sleep.params.defaults(),
  list(
    # canhrActi only
    ggir_exact = TRUE,
    rename_uppercase = FALSE,
    skip_small_files = FALSE,
    # unzip_once reads every block from one extraction of the .gt3x, which
    # read.raw.accelerometer removes when it returns. decode_once stays off: it inherits
    # read.gt3x's whole-file buffer, which froze a machine low on memory. stream_gt3x
    # reads each block from the extracted log.bin (raw_gt3x_stream.R).
    unzip_once = TRUE,
    decode_once = FALSE,
    stream_gt3x = TRUE,
    progress = NULL,
    ggir_version_label = .RAW_GGIR_VERSION_LABEL
  ))
}

# The GGIR version the port is proven against, written wherever GGIR writes its version
.RAW_GGIR_VERSION_LABEL <- "3.3.6"

#' Default Values of the Sleep Parameters
#'
#' Internal. The uncoerced defaults of the 43 carried members of GGIR's
#' \code{params_sleep}, in GGIR's order. nnights and sleeplogsep are left out because
#' GGIR declares them and never reads them.
#'
#' @return A named list of 43 members.
#' @keywords internal
#' @noRd
.raw.sleep.params.defaults <- function() {
  list(
    anglethreshold = 5, timethreshold = 5,
    ignorenonwear = TRUE,
    HASPT.algo = "HDCZA",
    HASIB.algo = "vanHees2015", Sadeh_axis = "Y",
    longitudinal_axis = c(),
    HASPT.ignore.invalid = FALSE,
    loglocation = c(), colid = 1, coln1 = 2,
    relyonguider = FALSE,
    def.noc.sleep = 1,
    sleepwindowType = "SPT",
    possible_nap_window = NULL,
    possible_nap_dur = NULL,
    possible_nap_gap = 0,
    possible_nap_edge_acc = Inf,
    nap_model = c(),
    sleepefficiency.metric = 1,
    HDCZA_threshold = c(),
    oakley_threshold = 20,
    consider_marker_button = FALSE,
    impute_marker_button = FALSE,
    sib_must_fully_overlap_with_TimeInBed = c(TRUE, TRUE),
    nap_markerbutton_method = 0,
    nap_markerbutton_max_distance = 30,
    SRI1_smoothing_wsize_hrs = NULL,
    SRI1_smoothing_frac = NULL,
    spt_min_block_dur =  30,
    spt_max_gap_dur =  60,
    spt_max_gap_ratio = 1,
    HorAngle_threshold = 60,
    guider_cor_maxgap_hrs = 2,
    guider_cor_min_frac_sib = 0.5,
    guider_cor_min_hrs = 2,
    guider_cor_meme_frac_out = 0.9,
    guider_cor_meme_frac_in = 0.4,
    guider_cor_meme_min_hrs = 1,
    guider_cor_do = FALSE,
    guider_cor_meme_min_dys = 3,
    HDCZA_roll_windowsize = 5,
    LowAcc_threshold = 0.014
  )
}

#' Parameter Object of Origin for Every Raw Pipeline Parameter
#'
#' Internal. Maps each member of \code{raw.params()} to the GGIR parameter object it came
#' from, or to "canhrActi" for the switches that have no GGIR counterpart.
#'
#' @return A named character vector, names in the order of \code{.raw.params.defaults()}.
#' @keywords internal
#' @noRd
.raw.params.groups <- function() {
  c(
    chunksize = "rawdata", spherecrit = "rawdata", minloadcrit = "rawdata",
    do.cal = "rawdata", backup.cal.coef = "rawdata", dynrange = "rawdata",
    minimumFileSizeMB = "rawdata", rmc.dec = "rawdata",
    rmc.firstrow.acc = "rawdata", rmc.firstrow.header = "rawdata",
    rmc.header.length = "rawdata", rmc.col.acc = "rawdata",
    rmc.col.temp = "rawdata", rmc.col.time = "rawdata",
    rmc.unit.acc = "rawdata", rmc.unit.temp = "rawdata",
    rmc.unit.time = "rawdata", rmc.format.time = "rawdata",
    rmc.bitrate = "rawdata", rmc.dynamic_range = "rawdata",
    rmc.unsignedbit = "rawdata", rmc.origin = "rawdata",
    rmc.sf = "rawdata",
    rmc.headername.sf = "rawdata", rmc.headername.sn = "rawdata",
    rmc.headername.recordingid = "rawdata", rmc.header.structure = "rawdata",
    rmc.check4timegaps = "rawdata", rmc.noise = "rawdata", nonwear_range_threshold = "rawdata",
    rmc.col.wear = "rawdata", rmc.doresample = "rawdata",
    interpolationType = "rawdata",
    imputeTimegaps = "rawdata", frequency_tol = "rawdata", rmc.scalefactor.acc = "rawdata",
    acc.metric = "general", windowsizes = "general",
    desiredtz = "general", configtz = "general", idloc = "general", dayborder = "general",
    part5_agg2_60seconds = "general",
    sensor.location = "general",
    recordingEndSleepHour = "general",
    do.anglex = "metrics", do.angley = "metrics", do.anglez = "metrics",
    do.zcx = "metrics", do.zcy = "metrics", do.zcz = "metrics",
    do.enmo = "metrics", do.lfenmo = "metrics", do.en = "metrics",
    do.mad = "metrics", do.enmoa = "metrics",
    do.roll_med_acc_x = "metrics", do.roll_med_acc_y = "metrics",
    do.roll_med_acc_z = "metrics", do.dev_roll_med_acc_x = "metrics",
    do.dev_roll_med_acc_y = "metrics", do.dev_roll_med_acc_z = "metrics",
    do.bfen = "metrics", do.hfen = "metrics", do.hfenplus = "metrics", do.lfen = "metrics",
    do.lfx = "metrics", do.lfy = "metrics", do.lfz = "metrics",
    do.hfx = "metrics", do.hfy = "metrics", do.hfz = "metrics",
    do.bfx = "metrics", do.bfy = "metrics", do.bfz = "metrics",
    do.neishabouricounts = "metrics",
    hb = "metrics", lb = "metrics", n = "metrics",
    zc.lb = "metrics", zc.hb = "metrics", zc.sb = "metrics", zc.order = "metrics",
    zc.scale = "metrics",
    actilife_LFE = "metrics",
    qwindow = "247", qwindow_dateformat = "247", iglevels = "247",
    LUXthresholds = "247", LUX_cal_constant = "247", LUX_cal_exponent = "247",
    LUX_day_segments = "247", clevels = "247",
    boutcriter.in = "phyact", boutcriter.lig = "phyact", boutcriter.mvpa = "phyact",
    threshold.lig = "phyact", threshold.mod = "phyact", threshold.vig = "phyact",
    boutdur.mvpa = "phyact", boutdur.in = "phyact", boutdur.lig = "phyact",
    frag.metrics = "phyact",
    includedaycrit = "cleaning",
    data_masking_strategy = "cleaning",
    maxdur = "cleaning",
    hrs.del.start = "cleaning", hrs.del.end = "cleaning",
    includedaycrit.part5 = "cleaning", excludefirstlast.part5 = "cleaning",
    data_cleaning_file = "cleaning", minimum_MM_length.part5 = "cleaning",
    excludefirstlast = "cleaning", includenightcrit = "cleaning",
    excludefirst.part4 = "cleaning", excludelast.part4 = "cleaning",
    nonWearEdgeCorrection = "cleaning", nonwear_approach = "cleaning",
    segmentWEARcrit.part5 = "cleaning", segmentDAYSPTcrit.part5 = "cleaning",
    includenightcrit.part5 = "cleaning",
    nonwearFiltermaxHours = "cleaning",
    nonwearFilterWindow = "cleaning",
    save_ms5rawlevels = "output", save_ms5raw_format = "output",
    save_ms5raw_without_invalid = "output",
    storefolderstructure = "output", timewindow = "output",
    week_weekend_aggregate.part5 = "output", do.part3.pdf = "output",
    outliers.only = "output", criterror = "output", do.visual = "output",
    do.sibreport = "output",
    sep_reports = "output", dec_reports = "output",
    require_complete_lastnight_part5 = "output", method_research_vars = "output",
    stats::setNames(rep("sleep", length(.raw.sleep.params.defaults())),
                    names(.raw.sleep.params.defaults())),
    ggir_exact = "canhrActi",
    rename_uppercase = "canhrActi",
    skip_small_files = "canhrActi",
    unzip_once = "canhrActi",
    decode_once = "canhrActi",
    stream_gt3x = "canhrActi",
    progress = "canhrActi",
    ggir_version_label = "canhrActi"
  )
}

#' Type Checks and Coercions of the Raw Pipeline Parameters
#'
#' Internal. Applies, in GGIR's order, every check of \code{check_params} that concerns a
#' carried member, then the four validations canhrActi adds and the checks of the
#' canhrActi-only switches. Warning and error texts are GGIR's; members GGIR leaves
#' untyped stay untyped.
#'
#' @param params The flat parameter list.
#' @return The list after coercion.
#' @keywords internal
#' @noRd
.raw.params.check <- function(params) {
  check_class <- function(category, params, parnames, parclass) {
    for (parname in parnames) {
      if (length(params[[parname]]) > 0) {
        if (params[[parname]][1] %in% c("c()", "NULL") == FALSE) { # because some variables are initialised empty
          x <- params[[parname]]
          if (parclass == "numeric") {
            if (!is.numeric(x)) {
              stop(paste0("\n", category, " parameter ", parname, " is not ", parclass))
            }
          }
          if (parclass == "boolean") {
            if (!is.logical(x)) {
              stop(paste0("\n", category, " parameter ", parname, " is not ", parclass))
            }
          }
          if (parclass == "character") {
            if (!is.character(x)) {
              stop(paste0("\n", category, " parameter ", parname, " is not ", parclass))
            }
          }
        }
      }
    }
  }
  # Sleep classes (GGIR's lists without the uncarried nnights, sleeplogsep and sleeplogidnum)
  numeric_params <- c("anglethreshold", "timethreshold", "longitudinal_axis",
                      "possible_nap_window", "possible_nap_dur",
                      "colid", "coln1", "def.noc.sleep",
                      "sleepefficiency.metric", "possible_nap_edge_acc", "HDCZA_threshold",
                      "possible_nap_gap", "oakley_threshold",
                      "nap_markerbutton_method",
                      "nap_markerbutton_max_distance",
                      "SRI1_smoothing_wsize_hrs",
                      "SRI1_smoothing_frac",
                      "spt_min_block_dur",
                      "spt_max_gap_dur", "spt_max_gap_ratio", "HorAngle_threshold",
                      "guider_cor_maxgap_hrs",
                      "guider_cor_min_frac_sib", "guider_cor_min_hrs",
                      "guider_cor_meme_frac_out",
                      "guider_cor_meme_frac_in", "guider_cor_meme_min_hrs",
                      "guider_cor_meme_min_dys", "HDCZA_roll_windowsize", "LowAcc_threshold")
  boolean_params <- c("ignorenonwear", "HASPT.ignore.invalid",
                      "relyonguider",
                      "impute_marker_button", "consider_marker_button",
                      "sib_must_fully_overlap_with_TimeInBed", "guider_cor_do")
  character_params <- c("HASPT.algo", "HASIB.algo", "Sadeh_axis", "nap_model",
                        "sleepwindowType", "loglocation")
  check_class("Sleep", params = params, parnames = numeric_params, parclass = "numeric")
  check_class("Sleep", params = params, parnames = boolean_params, parclass = "boolean")
  check_class("Sleep", params = params, parnames = character_params, parclass = "character")
  # Metrics classes
  boolean_params <- c("do.anglex", "do.angley", "do.anglez",
                      "do.zcx", "do.zcy", "do.zcz",
                      "do.enmo", "do.lfenmo", "do.en", "do.mad", "do.enmoa",
                      "do.roll_med_acc_x", "do.roll_med_acc_y", "do.roll_med_acc_z",
                      "do.dev_roll_med_acc_x", "do.dev_roll_med_acc_y", "do.dev_roll_med_acc_z",
                      "do.bfen", "do.hfen", "do.hfenplus", "do.lfen",
                      "do.lfx", "do.lfy", "do.lfz", "do.hfx", "do.hfy", "do.hfz",
                      "do.bfx", "do.bfy", "do.bfz")
  check_class("Metrics", params = params, parnames = boolean_params, parclass = "boolean")
  check_class("Metrics", params = params, parnames = c("hb", "lb", "n", "zc.lb", "zc.hb",
                                                        "zc.sb", "zc.order", "zc.scale"),
              parclass = "numeric")
  # Raw data classes, rmc.noise reset, chunksize floor
  numeric_params <- c("chunksize", "spherecrit", "minloadcrit", "minimumFileSizeMB", "dynrange",
                      "rmc.col.acc", "interpolationType",
                      "rmc.firstrow.acc", "rmc.firstrow.header", "rmc.header.length",
                      "rmc.col.temp", "rmc.col.time",
                      "rmc.sf", "rmc.col.wear", "rmc.noise", "frequency_tol",
                      "rmc.scalefactor.acc", "nonwear_range_threshold")
  boolean_params <- c("do.cal", "rmc.unsignedbit", "rmc.check4timegaps", "rmc.doresample",
                      "imputeTimegaps")
  character_params <- c("backup.cal.coef", "rmc.dec", "rmc.unit.acc",
                        "rmc.unit.temp", "rmc.unit.time", "rmc.format.time",
                        "rmc.origin", "rmc.headername.sf",
                        "rmc.headername.sn", "rmc.headername.recordingid",
                        "rmc.header.structure")
  if (is.logical(params[["rmc.noise"]])) {
    # Older config files used this, so overwrite with NULL value
    params["rmc.noise"] <- list(c())
  }
  check_class("Raw data", params = params, parnames = numeric_params, parclass = "numeric")
  check_class("Raw data", params = params, parnames = boolean_params, parclass = "boolean")
  check_class("Raw data", params = params, parnames = character_params, parclass = "character")

  if (params[["chunksize"]] < 0.1) params[["chunksize"]] <- 0.1
  # 247 classes; iglevels and qwindow may be numeric or character and stay untyped
  numeric_params <- c("LUXthresholds", "LUX_cal_constant",
                      "LUX_cal_exponent", "LUX_day_segments", "clevels")
  character_params <- c("qwindow_dateformat")
  check_class("247", params = params, parnames = numeric_params, parclass = "numeric")
  check_class("247", params = params, parnames = character_params, parclass = "character")
  # phyact classes (no boolean check: GGIR's line is commented out)
  numeric_params <- c("boutcriter.in", "boutcriter.lig", "boutcriter.mvpa",
                      "threshold.lig", "threshold.mod", "threshold.vig", "boutdur.mvpa",
                      "boutdur.in", "boutdur.lig")
  character_params <- c("frag.metrics")
  check_class("phyact", params = params, parnames = numeric_params, parclass = "numeric")
  check_class("phyact", params = params, parnames = character_params, parclass = "character")
  # Cleaning classes; segmentWEARcrit.part5 and segmentDAYSPTcrit.part5 are untyped in GGIR too
  numeric_params <- c("includedaycrit", "data_masking_strategy", "maxdur", "hrs.del.start",
                      "hrs.del.end", "includedaycrit.part5", "minimum_MM_length.part5",
                      "includenightcrit", "includenightcrit.part5",
                      "nonwearFiltermaxHours", "nonwearFilterWindow")
  boolean_params <- c("excludefirstlast.part5", "excludefirstlast",
                      "excludefirst.part4", "excludelast.part4", "nonWearEdgeCorrection")
  character_params <- c("data_cleaning_file")
  check_class("cleaning", params = params, parnames = numeric_params, parclass = "numeric")
  check_class("cleaning", params = params, parnames = boolean_params, parclass = "boolean")
  check_class("cleaning", params = params, parnames = character_params, parclass = "character")
  # Output classes
  numeric_params <- c("criterror")
  boolean_params <- c("save_ms5rawlevels", "save_ms5raw_without_invalid",
                      "storefolderstructure", "week_weekend_aggregate.part5",
                      "do.part3.pdf", "outliers.only", "do.visual", "do.sibreport",
                      "require_complete_lastnight_part5")
  character_params <- c("save_ms5raw_format", "timewindow", "sep_reports", "dec_reports",
                        "method_research_vars")
  check_class("output", params = params, parnames = numeric_params, parclass = "numeric")
  check_class("output", params = params, parnames = boolean_params, parclass = "boolean")
  check_class("output", params = params, parnames = character_params, parclass = "character")
  # General classes and the windowsizes coercions
  numeric_params <- c("windowsizes", "idloc", "dayborder")
  boolean_params <- c("part5_agg2_60seconds")
  character_params <- c("acc.metric", "desiredtz", "configtz", "sensor.location")
  check_class("general", params = params, parnames = numeric_params, parclass = "numeric")
  check_class("general", params = params, parnames = boolean_params, parclass = "boolean")
  check_class("general", params = params, parnames = character_params, parclass = "character")

  ws3 <- params[["windowsizes"]][1]; ws2 <- params[["windowsizes"]][2]; ws <- params[["windowsizes"]][3]
  if (ws2/60 != round(ws2/60)) {
    ws2 <- as.numeric(60 * ceiling(ws2/60))
    warning(paste0("The long windowsize needs to be a multitude of 1 minute periods.\n",
                   "Long windowsize has now been automatically adjusted to ",
                   ws2, " seconds in order to meet this criteria."), call. = FALSE)
  }
  if (ws2/ws3 != round(ws2/ws3)) {
    def <- c(1,5,10,15,20,30,60)
    def2 <- abs(def - ws3)
    ws3 <- as.numeric(def[which(def2 == min(def2))])
    warning(paste0("The long windowsize needs to be a multitude of short windowsize.\n",
                   "The short windowsize has now been automatically adjusted to ",
                   ws3, " seconds in order to meet this criteria.\n"), call. = FALSE)
  }
  if (ws/ws2 != round(ws/ws2)) {
    ws <- ws2 * ceiling(ws/ws2)
    warning(paste0("The third value of parameter windowsizes needs to be a multitude of the second value.\n",
                   "The third value has been automatically adjusted to ",
                   ws, " seconds in order to meet this criteria.\n"), call. = FALSE)
  }
  params[["windowsizes"]] <- c(ws3, ws2, ws)
  if (params[["frequency_tol"]] < 0 | params[["frequency_tol"]] > 1) {
    stop(paste0("\nParameter frequency_tol is ", params[["frequency_tol"]],
                " , please adjust such that it is a number between 0 and 1"))
  }
  # Sleep cross-checks. The nap block writes into four output members, which go in and
  # come back out.
  sleep_names <- names(.raw.sleep.params.defaults())
  output_names <- c("do.sibreport", "save_ms5raw_format", "save_ms5rawlevels",
                    "save_ms5raw_without_invalid")
  checked <- .raw.sleep.params.check(sleep = params[sleep_names],
                                     general = params[c("sensor.location")],
                                     metrics = params[c("do.anglex", "do.angley", "do.anglez",
                                                        "do.zcx", "do.zcy", "do.zcz")],
                                     output = params[output_names])
  for (nm in names(checked$sleep)) params[nm] <- list(checked$sleep[[nm]])
  for (nm in names(checked$general)) params[nm] <- list(checked$general[[nm]])
  for (nm in names(checked$metrics)) params[nm] <- list(checked$metrics[[nm]])
  for (nm in names(checked$output)) params[nm] <- list(checked$output[[nm]])
  if (params[["data_masking_strategy"]] %in% c(2, 4) & params[["hrs.del.start"]] != 0) {
    warning(paste0("\nSetting parameter hrs.del.start in combination with data_masking_strategy = ",
                   params[["data_masking_strategy"]]," is not meaningful, because this is only used when straytegy = 1"), call. = FALSE)
  }
  if (params[["data_masking_strategy"]] %in% c(2, 4) & params[["hrs.del.end"]] != 0) {
    warning(paste0("\nSetting parameter hrs.del.end in combination with data_masking_strategy = ",
                   params[["data_masking_strategy"]]," is not meaningful, because this is only used when straytegy = 1"), call. = FALSE)
  }
  # includedaycrit.part5 is read downstream as a fraction in [0, 1] and as hours in (1, 24]
  if (params[["includedaycrit.part5"]] < 0) {
    stop("\nNegative value of includedaycrit.part5 is not allowed, please change.")
  } else if (params[["includedaycrit.part5"]]  > 24) {
    stop(paste0("\nIncorrect value of includedaycrit.part5, this should be",
                " a fraction of the day between zero and one or the number ",
                "of hours in a day."))
  }
  if (!is.null(params[["nonwearFiltermaxHours"]])) {
    if (params[["nonwearFiltermaxHours"]] < 0 ||
        params[["nonwearFiltermaxHours"]] > 12) {
      stop("Parameters nonwearFiltermaxHours is expected to have a value > 0 and < 12")
    }
    if (!is.null(params[["nonwearFilterWindow"]])) {
      if (length(params[["nonwearFilterWindow"]]) != 2) {
        stop("Parameter nonwearFilterWindow does not have expected length of 2, please fix.", call. = FALSE)
      }
      if (params[["nonwearFilterWindow"]][1] < params[["nonwearFilterWindow"]][2] &&
          params[["nonwearFilterWindow"]][2] > 18 &&
          params[["nonwearFilterWindow"]][1] < 12) {
        warning(paste0("The NonwearFilter applied to window starting at ",
                       params[["nonwearFilterWindow"]][1], " and ending at ",
                       params[["nonwearFilterWindow"]][2],
                       " this is probably not the night, please check that order of",
                       " values in nonwearFilterWindow is correct"), call. = FALSE)
      }
    }
  }
  # the same two regimes for includenightcrit.part5
  if (params[["includenightcrit.part5"]] < 0) {
    stop("\nNegative value of includenightcrit.part5 is not allowed, please change.")
  } else if (params[["includenightcrit.part5"]]  > 24) {
    stop(paste0("\nIncorrect value of includenightcrit.part5, this should be",
                " a fraction of the day between zero and one or the number ",
                "of hours in a day."))
  }
  # GGIR's phyact cross-checks read only uncarried part-2 and part-6 members and are skipped.
  if (!all(params[["save_ms5raw_format"]] %in% c("RData", "csv"))) {
    formats2keep <- which(params[["save_ms5raw_format"]] %in% c("RData", "csv"))
    if (length(formats2keep) > 0) {
      params[["save_ms5raw_format"]] <- params[["save_ms5raw_format"]][formats2keep]
    } else {
      stop("Parameter save_ms5raw_format incorrectly specified, please fix.", call. = FALSE)
    }
  }
  if (params[["sep_reports"]] == params[["dec_reports"]]) {
    stop(paste0("\nYou have set sep_reports and dec_reports both to ",
                params[["sep_reports"]], " this is ambiguous. Please fix."))
  }
  # a length-1 iglevels is a switch: any single value becomes the 162 standard edges
  if (length(params[["iglevels"]]) > 0) {
    if (length(params[["iglevels"]]) == 1) {
      params[["iglevels"]] <- c(seq(0, 4000, by = 25), 8000) # to introduce option to just say TRUE
    }
  }
  if (length(params[["qwindow"]]) > 0) {
    if (is.character(params[["qwindow"]])) {
      # Convert paths from Windows specific slashed to generic slashes
      params[["qwindow"]] <- gsub(pattern = "\\\\", replacement = "/", x = params[["qwindow"]])
    }
  }
  # rounded, uniqued, sorted, and forced to open at 0 and close at 24
  if (length(params[["LUX_day_segments"]]) > 0) {
    params[["LUX_day_segments"]] <- sort(unique(round(params[["LUX_day_segments"]])))
    if (params[["LUX_day_segments"]][1] != 0) {
      params[["LUX_day_segments"]] <- c(0, params[["LUX_day_segments"]])
    }
    if (params[["LUX_day_segments"]][length(params[["LUX_day_segments"]])] != 24) {
      params[["LUX_day_segments"]] <- c(params[["LUX_day_segments"]], 24)
    }
  }
  if (length(params[["save_ms5raw_format"]]) == 1 &&
      params[["save_ms5raw_format"]] == "csv") {
    # always add RData if only csv is specified, because otherwise visualreport cannot be generated
    params[["save_ms5raw_format"]] <- c(params[["save_ms5raw_format"]], "RData")
  }
  if (length(params[["clevels"]]) == 1) {
    warning("\nParameter clevels expects a number vector of at least 2 values, current length is 1", call. = FALSE)
  }
  hipped <- .raw.sleep.params.hip(sleep = params[c("HASPT.algo")],
                                  general = params[c("sensor.location")])
  for (nm in names(hipped$general)) params[nm] <- list(hipped$general[[nm]])
  if (!is.null(params[["recordingEndSleepHour"]])) {
    # stop if expand_tail_max_hours was defined before 7pm
    if (params[["recordingEndSleepHour"]] < 19) {
      stop(paste0("\nrecordingEndSleepHour expects the latest time at which",
                  " the participant is expected to fall asleep. recordingEndSleepHour",
                  " has been defined as ", params[["recordingEndSleepHour"]],
                  ", which does not look plausible, please specify time at or later than 19:00",
                  " . Please note that it is your responsibility as user to verify that the
                  assumption is credible."), call. = FALSE)
    }
  }
  # Two of these branches are dead (a NULL fails the earlier < 0 test, and > 24 stops
  # before > 25); they stay so the error a user sees is GGIR's.
  if (is.null(params[["includedaycrit.part5"]]) == TRUE) {
    stop(paste0("\nSetting includedaycrit.part5 to an empty value is not allowed",
                ", please change."), call. = FALSE)
  } else if (params[["includedaycrit.part5"]] < 0) {
    stop(paste0("\nNegative value of includedaycrit.part5 is not allowed",
                ", please change."), call. = FALSE)
  } else if (params[["includedaycrit.part5"]] > 25) {
    stop(paste0("\nIncorrect value of includedaycrit.part5, this should ",
                "be a fraction of the day between zero and one or the ",
                "number of hours in a day."), call. = FALSE)
  }
  if (is.null(params[["segmentWEARcrit.part5"]])) {
    # if null, then assign default value
    params[["segmentWEARcrit.part5"]] <- 0.5
    warning(paste0("\nsegmentWEARcrit.part5 is expected to be a number between 0 and 1",
                   ", the default value has been assigned (i.e., 0.5) "), call. = FALSE)
  } else if (params[["segmentWEARcrit.part5"]] < 0 |
             params[["segmentWEARcrit.part5"]] > 1) {
    stop(paste0("Incorrect value of segmentWEARcrit.part5, this should be a ",
                "fraction between zero and one, please change."),
         call. = FALSE)
  }
  if (length(params[["segmentDAYSPTcrit.part5"]]) != 2) {
    stop("\nParameter segmentDAYSPTcrit.part5 is expected to be a numeric vector of length 2", call. = FALSE)
  }
  if (any(params[["segmentDAYSPTcrit.part5"]] < 0) |
      any(params[["segmentDAYSPTcrit.part5"]] > 1)) {
    stop(paste0("Incorrect values of segmentDAYSPTcrit.part5, these should be a ",
                "fractions between zero and one, please change."),
         call. = FALSE)
  }
  # Four validations GGIR does not make; each rejects a configuration GGIR mishandles.
  bad_window <- setdiff(params[["timewindow"]], c("MM", "WW", "OO"))
  if (length(bad_window) > 0) {
    stop(paste0("Parameter timewindow may only hold \"MM\", \"WW\" and \"OO\", not ",
                paste0("\"", bad_window, "\"", collapse = " or "),
                ". GGIR accepts an unknown window name and then never terminates."),
         call. = FALSE)
  }
  if (length(params[["timewindow"]]) == 0) {
    stop("Parameter timewindow must name at least one of \"MM\", \"WW\" and \"OO\".",
         call. = FALSE)
  }
  grid <- expand.grid(lig = params[["threshold.lig"]],
                      mod = params[["threshold.mod"]],
                      vig = params[["threshold.vig"]])
  bad_triple <- which(grid$lig > grid$mod | grid$mod > grid$vig)
  if (length(bad_triple) > 0) {
    i <- bad_triple[1]
    stop(paste0("The intensity thresholds have to satisfy threshold.lig <= threshold.mod",
                " <= threshold.vig for every combination, and the combination ",
                grid$lig[i], " ", grid$mod[i], " ", grid$vig[i], " does not."),
         call. = FALSE)
  }
  for (bc in c("boutcriter.in", "boutcriter.lig", "boutcriter.mvpa")) {
    x <- params[[bc]]
    if (length(x) != 1 || is.na(x) || x <= 0 || x > 1) {
      stop(paste0("Parameter ", bc, " must be a single fraction greater than 0 and at",
                  " most 1. GGIR does not range check it and a value above 1 silently",
                  " finds no bouts at all."), call. = FALSE)
    }
  }
  # canhrActi-only switches
  for (sw in c("ggir_exact", "rename_uppercase", "skip_small_files", "unzip_once",
               "decode_once", "stream_gt3x")) {
    x <- params[[sw]]
    if (!is.logical(x) || length(x) != 1 || is.na(x)) {
      stop(paste0("Parameter ", sw, " must be a single TRUE or FALSE."), call. = FALSE)
    }
  }
  if (!is.null(params[["progress"]]) && !is.function(params[["progress"]])) {
    stop("Parameter progress must be NULL or a function(stage, i, n, message).", call. = FALSE)
  }
  if (!is.character(params[["ggir_version_label"]]) ||
      length(params[["ggir_version_label"]]) != 1 ||
      is.na(params[["ggir_version_label"]])) {
    stop("Parameter ggir_version_label must be a single character string, for example \"3.3.6\".",
         call. = FALSE)
  }
  params
}

#' Cross-Checks and Coercions of the Sleep Parameters
#'
#' Internal. Applies, in GGIR's order, the cross-check blocks of \code{check_params} that
#' read \code{params_sleep}: the HASPT.algo whitelist and NotWorn swap, the nap length
#' guards, the nap rewrite of four output members, the Sadeh family forcing the
#' zero-crossing metrics, the hip forcing of the angle metrics and HASPT.algo, and the
#' loglocation and sleepwindowType hygiene. Takes and returns the four GGIR lists it
#' reads, so it can be compared with \code{GGIR:::check_params} on the same input.
#'
#' @param sleep Named list holding the carried members of GGIR's params_sleep.
#' @param general Named list holding at least sensor.location.
#' @param metrics Named list holding at least do.anglex, do.angley, do.anglez, do.zcx,
#'   do.zcy and do.zcz.
#' @param output Named list holding at least do.sibreport, save_ms5raw_format,
#'   save_ms5rawlevels and save_ms5raw_without_invalid; an empty list when the caller
#'   does not want the output half.
#' @return A list with elements sleep, general, metrics and output, each coerced.
#' @keywords internal
#' @noRd
.raw.sleep.params.check <- function(sleep, general, metrics, output = list()) {
  # HASPT.algo whitelist; "HDCZA" survives only because the same value is written back
  if (length(sleep[["def.noc.sleep"]]) != 2) {
    if (sleep[["HASPT.algo"]][1] %in% c("HorAngle", "NotWorn", "MotionWare", "HLRB", "LowAcc") == FALSE) {
      sleep[["HASPT.algo"]] <- "HDCZA"
    }
    if (length(sleep[["HASPT.algo"]]) == 2 && sleep[["HASPT.algo"]][2] == "NotWorn") {
      sleep[["HASPT.algo"]] <- sleep[["HASPT.algo"]][2:1] # NotWorn is expected to be first
    }
  } else if (length(sleep[["def.noc.sleep"]]) == 2) {
    sleep[["HASPT.algo"]] <- "notused"
  }
  if (length(sleep[["possible_nap_gap"]]) != 1) {
    stop(paste0("Parameter possible_nap_gap has length ", length(sleep[["possible_nap_gap"]]),
                " while length 1 is expected"), call. = FALSE)
  }
  if (!is.null(sleep[["possible_nap_window"]]) &&
      length(sleep[["possible_nap_window"]]) != 2) {
    stop(paste0("Parameter possible_nap_window has length ", length(sleep[["possible_nap_window"]]),
                " while length 2 is expected"), call. = FALSE)
  }
  if (!is.null(sleep[["possible_nap_dur"]]) &&
      length(sleep[["possible_nap_dur"]]) != 2) {
    stop(paste0("Parameter possible_nap_dur has length ", length(sleep[["possible_nap_dur"]]),
                " while length 2 is expected"), call. = FALSE)
  }
  # a nap window with a nap duration rewrites four output members
  if (!is.null(sleep[["possible_nap_window"]]) &&
      !is.null(sleep[["possible_nap_dur"]])) {
    output[["do.sibreport"]] <- TRUE
    output[["save_ms5raw_format"]] <- unique(c(output[["save_ms5raw_format"]], "RData"))
    output[["save_ms5rawlevels"]] <- TRUE
    output[["save_ms5raw_without_invalid"]] <- FALSE
  }
  # a count-based HASIB.algo needs the zero-crossing metric of Sadeh_axis
  sib_90s_algo_names <- c("Sadeh1994", "Galland2012", "ColeKripke1992", "Oakley1997")
  if (any(sleep[["HASIB.algo"]] %in% sib_90s_algo_names == TRUE)) {
    if (sleep[["Sadeh_axis"]] %in% c("X", "Y", "Z") == FALSE) {
      warning("Parameter Sadeh_axis does not have meaningful value, it needs to be X, Y or Z (capital)", call. = FALSE)
    }
    if (sleep[["Sadeh_axis"]] == "X" & metrics[["do.zcx"]] == FALSE) metrics[["do.zcx"]] <- TRUE
    if (sleep[["Sadeh_axis"]] == "Y" & metrics[["do.zcy"]] == FALSE) metrics[["do.zcy"]] <- TRUE
    if (sleep[["Sadeh_axis"]] == "Z" & metrics[["do.zcz"]] == FALSE) metrics[["do.zcz"]] <- TRUE
  } else { # vanHees2015
    sleep[["Sadeh_axis"]] <- "" # not used
  }
  # hip forces the three angle metrics and HorAngle
  if (general[["sensor.location"]] == "hip" &&
      sleep[["HASPT.algo"]][1] %in% c("notused", "NotWorn") == FALSE) {
    if (metrics[["do.anglex"]] == FALSE | metrics[["do.angley"]] == FALSE | metrics[["do.anglez"]] == FALSE) {
      warning(paste0("\nWhen working with hip data all three angle metrics are needed,",
                     "so GGIR now auto-sets parameters do.anglex, do.angley, and do.anglez to TRUE."), call. = FALSE)
      metrics[["do.anglex"]] <- metrics[["do.angley"]] <- metrics[["do.anglez"]] <- TRUE
    }
    if (length(sleep[["HASPT.algo"]]) == 1 && sleep[["HASPT.algo"]][1] != "HorAngle") {
      warning("\nChanging HASPT.algo value to HorAngle, because sensor.location is set as hip", call. = FALSE)
      sleep[["HASPT.algo"]] <- "HorAngle"; sleep[["def.noc.sleep"]] <- 1
    }
  }
  # loglocation hygiene and sleepwindowType
  if (length(sleep[["loglocation"]]) == 1) {
    if (sleep[["loglocation"]] == "") {
      sleep["loglocation"] <- list(c()) # inserted because some users mistakingly use this
    } else {
      # Convert paths from Windows specific slashed to generic slashes
      sleep[["loglocation"]] <- gsub(pattern = "\\\\", replacement = "/", x = sleep[["loglocation"]])
    }
  }
  if (length(sleep[["loglocation"]]) > 0 & length(sleep[["def.noc.sleep"]]) != 1) {
    warning(paste0("\nloglocation was specified and def.noc.sleep does not have length of 1, this is not compatible. ",
                   " We assume you want to use the sleeplog and misunderstood",
                   " parameter def.noc.sleep. Therefore, we will reset def.noc.sleep to its default value of 1"), call. = FALSE)
    sleep[["def.noc.sleep"]] <- 1
  }
  if (sleep[["HASPT.algo"]][1] == "HorAngle" & sleep[["sleepwindowType"]] != "TimeInBed") {
    warning("\nHASPT.algo is set to HorAngle, therefore auto-updating sleepwindowType to TimeInBed", call. = FALSE)
    sleep[["sleepwindowType"]] <- "TimeInBed"
  }
  if (length(sleep[["loglocation"]]) == 0 &
      sleep[["HASPT.algo"]][1] != "HorAngle" &
      sleep[["HASPT.algo"]][1] != "NotWorn" &
      sleep[["sleepwindowType"]] != "SPT") {
    warning("\nAuto-updating sleepwindowType to SPT because no sleeplog used and neither HASPT.algo HorAngle or NotWorn used.", call. = FALSE)
    sleep[["sleepwindowType"]] <- "SPT"
  }
  list(sleep = sleep, general = general, metrics = metrics, output = output)
}

#' HorAngle Implies a Hip Sensor Location
#'
#' Internal. The last of GGIR's sleep cross-checks, kept separate because GGIR applies it
#' after the cleaning cross-checks. It runs after the hip block has read sensor.location,
#' so HASPT.algo "HorAngle" on a wrist gives a hip configuration with do.anglex and
#' do.angley still FALSE, as in GGIR.
#'
#' @param sleep Named list holding at least HASPT.algo.
#' @param general Named list holding at least sensor.location.
#' @return A list with elements sleep and general.
#' @keywords internal
#' @noRd
.raw.sleep.params.hip <- function(sleep, general) {
  if (sleep[["HASPT.algo"]][1] == "HorAngle") {
    general[["sensor.location"]] <- "hip"
  }
  list(sleep = sleep, general = general)
}

#' The Sleep Group of a Raw Parameter Object
#'
#' Returns the members of \code{\link{raw.params}} that came from GGIR's
#' \code{params_sleep}, after every coercion, as a plain named list in GGIR's order. The
#' sleep members live in \code{raw.params()} because two of the coercions change the
#' column set of \code{metashort} and must be settled before part 1 computes the metrics.
#'
#' @param ... Either one \code{canhrActi_raw_params} object, or named parameter values
#'   passed to \code{\link{raw.params}}. Any member may be named, because the coercions
#'   are cross-group.
#'
#' @return A named list of 43 members of class \code{"canhrActi_raw_sleep_params"}, ready
#'   to be handed to GGIR as \code{params_sleep} once nnights and sleeplogsep are filled
#'   in from \code{GGIR::load_params()}.
#'
#' @examples
#' raw.sleep.params()$HASPT.algo
#' raw.sleep.params(HASPT.algo = "NotWorn")$HASPT.algo
#' raw.sleep.params(raw.params(def.noc.sleep = c(21, 9)))$HASPT.algo
#'
#' @export
raw.sleep.params <- function(...) {
  dots <- list(...)
  if (length(dots) == 1 && is.null(names(dots)) && inherits(dots[[1]], "canhrActi_raw_params")) {
    params <- dots[[1]]
  } else {
    params <- do.call(raw.params, dots)
  }
  nms <- names(.raw.sleep.params.defaults())
  out <- vector("list", length(nms))
  names(out) <- nms
  for (nm in nms) out[nm] <- list(params[[nm]])
  structure(out, class = "canhrActi_raw_sleep_params")
}

#' Split a Raw Parameter Object into GGIR's Parameter Lists
#'
#' Internal. Regroups the members of a \code{raw.params()} object into the eight GGIR
#' parameter objects they came from, for parity tests that call GGIR functions. The
#' canhrActi-only switches are dropped and members GGIR has but canhrActi does not carry
#' are absent; merge the result over \code{GGIR::load_params()} for a complete object.
#'
#' @param params A \code{canhrActi_raw_params} object.
#' @return A list with elements params_rawdata, params_general, params_metrics,
#'   params_cleaning, params_sleep, params_output, params_247 and params_phyact.
#' @keywords internal
#' @noRd
.raw.params.ggir <- function(params) {
  groups <- .raw.params.groups()
  pick <- function(g) {
    nms <- names(groups)[groups == g]
    out <- vector("list", length(nms))
    names(out) <- nms
    for (nm in nms) out[nm] <- list(params[[nm]])
    out
  }
  list(params_rawdata = pick("rawdata"),
       params_general = pick("general"),
       params_metrics = pick("metrics"),
       params_cleaning = pick("cleaning"),
       params_sleep = pick("sleep"),
       params_output = pick("output"),
       params_247 = pick("247"),
       params_phyact = pick("phyact"))
}

#' Format One Parameter Value for Printing
#'
#' Internal. One line of deparsed text per value; functions and NULL are named rather
#' than deparsed.
#'
#' @param x Any parameter value.
#' @return A single string.
#' @keywords internal
#' @noRd
.raw.params.format.value <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.function(x)) return("<function>")
  txt <- paste(deparse(x, width.cutoff = 500L), collapse = " ")
  if (nchar(txt) > 70) txt <- paste0(substr(txt, 1, 67), "...")
  txt
}

#' Print Method for Raw Pipeline Parameters
#'
#' Prints every member grouped by the GGIR parameter object it came from, marking with an
#' asterisk the members whose value differs from the default.
#'
#' @param x An object of class \code{canhrActi_raw_params}.
#' @param ... Not used.
#' @return \code{x}, invisibly.
#' @export
print.canhrActi_raw_params <- function(x, ...) {
  # compare against the coerced defaults: Sadeh_axis is "Y" before coercion and "" after
  defaults <- unclass(raw.params())
  groups <- attr(x, "groups")
  if (is.null(groups)) groups <- .raw.params.groups()
  labels <- c(rawdata = "Raw data (GGIR params_rawdata)",
              general = "General (GGIR params_general)",
              metrics = "Metrics (GGIR params_metrics)",
              cleaning = "Cleaning (GGIR params_cleaning)",
              sleep = "Sleep (GGIR params_sleep)",
              output = "Output (GGIR params_output)",
              phyact = "Physical activity (GGIR params_phyact)",
              `247` = "Round the clock (GGIR params_247)",
              canhrActi = "canhrActi switches")
  changed <- character(0)
  cat("\ncanhrActi raw accelerometer parameters (GGIR parts 1, 3, 4 and 5 subset)\n")
  for (g in names(labels)) {
    nms <- names(groups)[groups == g]
    nms <- nms[nms %in% names(x)]
    if (length(nms) == 0) next
    cat("\n", labels[[g]], "\n", sep = "")
    for (nm in nms) {
      is_default <- nm %in% names(defaults) && identical(x[[nm]], defaults[[nm]])
      mark <- if (is_default) "  " else "* "
      if (!is_default) changed <- c(changed, nm)
      cat(sprintf("%s%-28s %s\n", mark, nm, .raw.params.format.value(x[[nm]])))
    }
  }
  cat("\n")
  if (length(changed) == 0) {
    cat("All values at GGIR defaults.\n")
  } else {
    cat("* ", length(changed), " value", if (length(changed) > 1) "s" else "",
        " differ from the defaults: ", paste(changed, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}
