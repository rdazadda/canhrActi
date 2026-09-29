# Ported from GGIR 3.3-9 R/g.report.part5_dictionary.R and
# dev-functions/createInternalData.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles et al.; copyright holders Medical Research
# Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. The two reports are passed in as
# column names or frames and the two dictionaries are returned, so each report is
# described by its own columns; the file I/O moved to write.ggir.milestone(). GGIR's
# baseDictionary is transcribed from the source that generates it. The two defects of the
# original are reproduced behind ggir_exact.

#' GGIR's baseDictionary, the 41 Fixed Variable Definitions
#'
#' The lookup \code{g.report.part5_dictionary} tries first, on the column name with
#' \code{_pla}, \code{_wei}, \code{_WD} and \code{_WE} stripped. The text is GGIR's, errors
#' included: \code{sleeplog_used} and \code{acc_available} are described as TRUE/FALSE
#' although part 5 writes 0 and 1.
#'
#' @return A named list of 41 single strings.
#' @keywords internal
#' @noRd
.raw.base.dictionary <- function() {
  list(
    # general
    ID = "File/participant identifier",
    filename = "File name",

    # day, dates, windows
    weekday = "Day of the week (full name)",
    window_number = "Window number in the recording",
    window = "Window start and end names in segment analysis",
    night_number = "Night number in the recording",
    start_end_window = "Start and end time for the window (hh:mm:ss-hh:mm:ss)",
    daytype = "WD = weekday; WE = weekend day",
    calendar_date = "Calendar date",
    startday = "Calendar date for the first day in the recording",
    Nvaliddays = "Number of valid days based on the cleaning parameters for part 2, 4, and 5",
    Nvaliddays_WD = "Number of valid weekdays based on the cleaning parameters for part 2, 4, and 5",
    Nvaliddays_WE = "Number of valid weekend days based on the cleaning parameters for part 2, 4, and 5",
    Nvaliddays_AL10F_WD = "Number of valid weekdays with at least 10 fragments (fragmentation analysis)",
    Nvaliddays_AL10F_WE = "Number of valid weekend days with at least 10 fragments (fragmentation analysis)",
    Nvalidsegments = "Number of valid segments in the recording based on segmentWEARcrit.part5 and segmentDAYSPTcrit.part5",
    Nvalidsegments_WD = "Number of valid segments in the weekdays based on segmentWEARcrit.part5 and segmentDAYSPTcrit.part5 in the recording",
    Nvalidsegments_WE = "Number of valid segments in the weekend days based on segmentWEARcrit.part5 and segmentDAYSPTcrit.part5 in the recording",

    # sleep info in part 4 and part 5
    daysleeper = "Night classified as daysleeper (i.e., wake-up time after noon)",
    cleaningcode = "Cleaning code for the sleep period time classification (0=no problem; 1=no sleeplog; 2=insufficient valid data; 3=no acc data available; 4=no nights; 5=guider-defined SPT; 6=SPT not found)",
    guider = "Guider used for the sleep period time identification",
    sleeplog_used = "Whether sleep log information was used for the identification of the sleep period time (TRUE/FALSE)",
    acc_available = "Whether accelerometer data was available for the identification of the sleep period time (TRUE/FALSE)",
    N_atleast5minwakenight = "Number of blocks awake after sleep onset with a duration of at least 5 minutes",
    Ndaysleeper = "Number of nights classified as daysleeper (i.e., wake-up time after noon)",
    Ncleaningcodezero = "Number of nights with cleaning code for the sleep period time classification = 0 (i.e., no problem)",
    Ncleaningcode1 = "Number of nights with cleaning code for the sleep period time classification = 1 (i.e., no sleeplog)",
    Ncleaningcode2 = "Number of nights with cleaning code for the sleep period time classification = 2 (i.e., insufficient valid data)",
    Ncleaningcode3 = "Number of nights with cleaning code for the sleep period time classification = 3 (i.e., no acc data available)",
    Ncleaningcode4 = "Number of nights with cleaning code for the sleep period time classification = 4 (i.e., no nights)",
    Ncleaningcode5 = "Number of nights with cleaning code for the sleep period time classification = 5 (i.e., guider-defined SPT)",
    Ncleaningcode6 = "Number of nights with cleaning code for the sleep period time classification = 6 (i.e., SPT not found)",
    Nsleeplog_used = "Number of nights in which the sleep log was used as guider for the sleep period time identification",
    Nacc_available = "Number of nights in which accelerometer data was available for the identification of the sleep period time",

    # processing information
    tail_expansion_minutes = "Time expanded at the end of the recording with expand_tail_max_hours to trigger the last sleep onset identification (min)",
    boutcriter.in = "Fraction of the bout that needs to be below the inactivity threshold",
    boutcriter.lig = "Fraction of the bout that needs to meet the light physical activity threshold",
    boutcriter.mvpa = "Fraction of the bout that needs to be above the moderate physical activity threshold",
    boutdur.in = "Duration/s of inactivity bouts (min)",
    boutdur.lig = "Duration/s of light physical activity bouts (min)",
    boutdur.mvpa = "Duration/s of moderate-to-vigorous physical activity bouts (min)")
}

#' Turn the Behavioural-Class Tokens of a Column Name Into Prose
#'
#' A token holding a "2", such as \code{PA2IN}, is split on it and the two halves are joined
#' with " to ". Two GGIR constructions are kept because changing either would change the
#' text on some input: \code{if (grepl("2", x))} tests a vector, and the two
#' \code{mostactive} tokens assign to \code{class} rather than to \code{class[i]}.
#'
#' @param x The tokens of the column name that are behavioural classes, possibly empty.
#' @return A single string, or NULL when x is empty.
#' @keywords internal
#' @noRd
.raw.dictionary.class <- function(x) {
  if (length(x) == 0) return(NULL)
  class <- NULL
  if (grepl("2", x)) x <- unlist(strsplit(x, "2"))
  for (i in 1:length(x)) {
    if (x[i] == "IN") class[i] <- "inactivity"
    if (x[i] == "LIG" | x[i] == "LIPA") class[i] <- "light physical activity"
    if (x[i] == "MOD") class[i] <- "moderate physical activity"
    if (x[i] == "VIG") class[i] <- "vigorous physical activity"
    if (x[i] == "MVPA") class[i] <- "moderate-to-vigorous physical activity"
    if (x[i] == "PA") class[i] <- "physical activity"
    if (x[i] == "sleep") class[i] <- "sleep"
    if (x[i] == "wake") class[i] <- "awake"
    if (x[i] == "nonwear") class[i] <- "non-wear time"
    if (x[i] == "mostactive60min") class <- "the most active 60 minutes of the day are accumulated"
    if (x[i] == "mostactive30min") class <- "the most active 30 minutes of the day are accumulated"
  }
  if (length(class) == 2) class <- paste(class[1], "to", class[2])
  return(class)
}

#' The Dictionary of One Part-5 Report
#'
#' A definition is \code{paste(what, class, window, unit)} and, on a person summary,
#' \code{paste(def, agg)}. \code{elements} is threaded in and out because GGIR leaks it: it
#' is assigned only when \code{baseDictionary} missed and never reset, so a column whose
#' definition came from \code{baseDictionary} still holds the previous column's tokens when
#' the aggregation suffix is decided, across the report boundary too.
#'
#' @param cnames The report's column names.
#' @param person TRUE for a person summary, which adds the aggregation suffix and rewrites
#'   the part numbers of the Nvaliddays definitions.
#' @param elements The token vector left over from the previous column, or NULL on the first
#'   call.
#' @param ggir_exact TRUE reproduces the two defects; FALSE refreshes the tokens on every
#'   column and drops the trailing space.
#' @return list(dictionary = data.frame(Variable, Definition), elements = the leftover tokens).
#' @keywords internal
#' @noRd
.raw.dictionary.table <- function(cnames, person = FALSE, elements = NULL, ggir_exact = TRUE) {
  baseDictionary <- .raw.base.dictionary()
  dictionary <- data.frame(Variable = cnames,
                           Definition = NA)
  for (coli in 1:length(cnames)) {
    # a definition is what, window, when (LUX segments only), class and unit
    what <- window <- class <- unit <- NULL
    nam <- gsub("_pla|_wei|_WD|_WE", "", cnames[coli])
    if (grepl("Nvalid", cnames[coli])) nam <- cnames[coli]
    if (nam %in% names(baseDictionary)) {
      what <- baseDictionary[[nam]]
    }
    # not in GGIR: refresh the tokens on every column
    if (ggir_exact == FALSE) elements <- unlist(strsplit(cnames[coli], "[.]|_"))
    # not in baseDictionary, so parse the name
    if (is.null(what)) {
      elements <- unlist(strsplit(cnames[coli], "[.]|_"))
      # what
      if ("dur" %in% elements | "nonwear" %in% elements) {
        what <- "Time accumulated"
      } else if ("ACC" %in% elements) {
        what <- "Mean acceleration"
        if ("median" %in% elements) {
          what <- "Median acceleration"
        } else if ("stdev" %in% elements) {
          what <- "Standard deviation of acceleration"
        }
      } else if ("Nbouts" %in% elements) {
        what <- "Number of bouts"
      } else if ("Nblocks" %in% elements) {
        what <- "Number of blocks (defined as consecutive series of epochs with the same behavioural class)"
      } else if ("quantile" %in% elements) {
        what <- "Acceleration above which (percentile)"
      } else if ("LUX" %in% elements) {
        if ("min" %in% elements | "above1000" %in% elements) {
          # time in lux ranges
          what <- "Time accumulated with LUX"
          if ("above1000" %in% elements) {
            numbers <- 1000
          } else {
            numbers <- suppressWarnings(as.numeric(elements))
            numbers <- numbers[!is.na(numbers)]
          }
          if (any(numbers == Inf) | length(numbers) == 1) {
            thresholds <- paste("above", numbers[1])
          } else if (length(numbers) == 2) {
            thresholds <- paste("between", numbers[1], "and", numbers[2])
          } else {
            thresholds <- ""
          }
          what <- paste(what, thresholds, "luxes")
          unit <- "(minutes)"
        } else if ("mean" %in% elements) {
          what <- "Mean LUX value"
          if ("mvpa" %in% elements) what <- paste(what, "in moderate-to-vigorous physical activity time")
          unit <- "(luxes)"
        } else if ("max" %in% elements) {
          what <- "Maximum LUX value"
          unit <- "(luxes)"
        } else if ("timeawake" %in% elements) {
          what <- "Time classified as awake"
          unit <- "(minutes)"
        } else if ("imputed" %in% elements) {
          what <- "Time in which the LUX has been imputed"
          unit <- "(minutes)"
        } else if ("ignored" %in% elements) {
          what <- "Time in which the LUX has been ignored"
          unit <- "(minutes)"
        }
        # lux segments
        if (any(grepl("hr", elements))) {
          t1 <- elements[grep("hr", elements)]; t1 <- gsub("hr", ":00", t1)
          t0 <- elements[grep("hr", elements) - 1]
          if (nchar(t0) == 1) t0 <- paste0(t0, ":00")
          what <- paste(what, "in the segment from", t0, "to", t1)
        }
      }
      # fragmentation metrics
      if ("FRAG" %in% elements) {
        what_bu <- tolower(what)
        what <- "Fragmentation analysis:"
        if ("TP" %in% elements) {
          what <- paste(what, "transition probability (%)")
        } else if ("Nfrag" %in% elements) {
          what <- paste(what, "number of fragments")
        } else if ("NFragPM" %in% elements) {
          what <- paste(what, "number of fragments per minute")
        } else if ("mean" %in% elements) {
          what <- paste(what, "mean duration in the")
        } else if ("Gini" %in% elements) {
          what <- paste(what, "Gini inequality index as calculated in the ineq R package")
        } else if ("CoV" %in% elements) {
          what <- paste(what, "Coefficient of variance as proposed in https://shorturl.at/nsDU9")
        } else if ("alpha" %in% elements) {
          what <- paste(what, "Alpha power law exponent metric as proposed in https://shorturl.at/gwzB8")
        } else if ("x0" %in% elements) {
          what <- paste(what, "x0.5 power law exponent metric as proposed in https://shorturl.at/gwzB8")
        } else if ("W0" %in% elements) {
          what <- paste(what, "W0.5 power law exponent metric as proposed in https://shorturl.at/gwzB8")
        } else if ("SD" %in% elements) {
          what <- paste(what, "standard deviation in the")
        }
        what <- paste(what, what_bu)
      }
      # window
      if ("day" %in% elements & "spt" %in% elements) {
        window <- "during the waking time and sleep period time (i.e., full window)"
      } else if ("day" %in% elements) {
        window <- "during the waking time"
      } else if ("spt" %in% elements) {
        window <- "during the sleep period time"
      }
      # class
      classes <- c("IN", "LIG", "LIPA", "MOD", "VIG", "MVPA", "PA",
                   "sleep", "nonwear", "mostactive60min", "mostactive30min",
                   "IN2PA", "PA2IN", "IN2LIPA", "IN2MVPA", "sleep2wake", "wake2sleep")
      x <- elements[which(elements %in% classes)]
      class <- .raw.dictionary.class(x)
      # sleep efficiency overrides the class
      if ("sleep" %in% elements & "efficiency" %in% elements) {
        class <- "Sleep efficiency after onset"
        unit <- "(%)"
      }
      # wakefulness after sleep onset
      if ("wake" %in% elements) class <- paste("awake", class)
      # bouts
      if ("bts" %in% elements & !("Nbouts" %in% elements)) {
        numbers <- suppressWarnings(as.numeric(elements))
        boutdur <- numbers[!is.na(numbers)]
        class <- paste("bouts of", paste(boutdur, collapse = "-"), "min", class)
      }
      # unbouted time
      if ("unbt" %in% elements & !("Nbouts" %in% elements)) {
        intensity <- elements[which(elements %in% c("IN", "LIG", "MOD", "VIG"))]
        if (intensity %in% c("MOD", "VIG")) intensity <- "MVPA"
        look4 <- paste("dur_day", intensity, "bts", sep = "_")
        boutVars <- grep(look4, cnames, value = TRUE)
        boutdurs <- c()
        for (i in 1:length(boutVars)) {
          x <- unlist(strsplit(boutVars[i], split = "_"))
          numbers <- suppressWarnings(as.numeric(x))
          boutdurs <- c(boutdurs, numbers[!is.na(numbers)])
        }
        minboutdur <- min(boutdurs)
        class <- paste("unbouted", class, paste0("(", 0, "-", minboutdur, " min)"))
      }
      # intensity gradient
      if ("ig" %in% elements) {
        if ("gradient" %in% elements) class <- "Intensity gradient"
        if ("rsquared" %in% elements) class <- paste("R-squared from the log-log time to intensity regression to calculate the intensity gradient")
        if ("intercept" %in% elements) class <- paste("Intercept from the log-log time to intensity regression to calculate the intensity gradient")
      }
      # connector (in)
      if (!is.null(class)) {
        if (class != "Sleep efficiency after onset"
            & substr(class, 1, 8) != "the most"
            & !("ig" %in% elements)) class <- paste("in", class)
      }
      # units
      if ("min" %in% elements) {
        unit <- "(minutes)"
      } else if ("mg" %in% elements) {
        unit <- "(mili-gravity units)"
      } else if ("perc" %in% elements) {
        unit <- "(%)"
      }
      # the rest of the column names
      if (is.null(what)) {
        if ("sleeponset" %in% elements | "wakeup" %in% elements) {
          if ("sleeponset" %in% elements) what <- "Sleep onset time"
          if ("wakeup" %in% elements) what <- "Wake up time"
          if ("ts" %in% elements) {
            unit <- "(hh:mm:ss)"
          } else {
            unit <- "(hours from previous midnight)"
          }
        } else if ((substr(elements[1], 1, 1) == "L" | substr(elements[1], 1, 1) == "M")
                   & elements[1] != "LUX") {
          # LX and MX metrics
          if (grepl("TIME", elements[1])) {
            what <- "Starting time"
            unit <- "(timestamp)"
            if ("num" %in% elements) {
              unit <- "(timestamp)"
            }
          } else if (grepl("VALUE", elements[1])) {
            what <- "Mean acceleration"
            unit <- "(mili-gravity units)"
          } else if ("peakLUX" %in% elements) {
            if ("mean" %in% elements) {
              what <- "Mean peak Lux"
            } else if ("max" %in% elements) {
              what <- "Max peak Lux"
            }
          }
          X <- as.numeric(gsub("\\D", "", elements))[1]
          if (substr(elements[1], 1, 1) == "L") {
            class <- paste("during the", X, "consecutive hours with the lowest acceleration")
          } else if (substr(elements[1], 1, 1) == "M") {
            class <- paste("during the", X, "consecutive hours with the highest acceleration")
          }
        } else if ("daytype" %in% elements) {
          what <- "WD = weekday; WE = weekend day"
        } else {
          what <- ""
        }
      }
    }
    def <- paste(what, class, window, unit)
    # personsummary only: the aggregation method
    if (person == TRUE) {
      agg <- NULL
      if (!grepl("Nvalid", cnames[coli])) {
        if ("pla" %in% elements) agg <- "- plain average"
        if ("wei" %in% elements) agg <- "- weighted average"
        if ("WD" %in% elements) agg <- "- weekdays average"
        if ("WE" %in% elements) agg <- "- weekend days average"
      } else {
        # adapt definition of valid days to part 5 reports
        def <- gsub("part 2, 4, and 5", "part 5", def)
      }
      def <- paste(def, agg)
    }
    def <- gsub("\\s+", " ", def)
    def <- gsub("^ ", "", def)
    # not in GGIR: drop the trailing space GGIR leaves when a part is NULL
    if (ggir_exact == FALSE) def <- gsub(" $", "", def)

    # neither window nor class identified: blank definition
    if (is.null(what) & is.null(class)) def <- NA

    dictionary[coli, "Definition"] <- def
  }
  list(dictionary = dictionary, elements = elements)
}

#' The GGIR Variable Dictionary for a Pair of Part-5 Reports
#'
#' Describes every column of a part-5 day summary and of a part-5 person summary in the words
#' GGIR uses, and returns the two tables, so that the GGIR-format export can ship the two
#' \code{results/variableDictionary/part5_dictionary_*.csv} files. It is not the package's
#' own data dictionary: the definitions are GGIR's, defects included.
#'
#' @details A definition is \code{paste(what, class, window, unit)}, and on the person
#'   summary \code{paste(def, agg)}. \code{what} is looked up first in the 41-entry
#'   \code{baseDictionary} on the column name with \code{_pla}, \code{_wei}, \code{_WD} and
#'   \code{_WE} stripped, and only on a miss does a token parser run over
#'   \code{strsplit(name, "[.]|_")}.
#'
#'   Two deliberate changes from GGIR. GGIR reads one day summary and one person summary off
#'   disk in \code{dir()} order, so on a \code{timewindow = c("MM", "WW")} run its dictionary
#'   describes MM only; here each report is described by its own columns. And GGIR's two
#'   defects are reproduced only while \code{ggir_exact} is TRUE: the token vector is never
#'   reset, so a column whose definition came from \code{baseDictionary} takes the previous
#'   column's aggregation suffix (the parameter-echo columns end "- plain average"), and
#'   \code{paste("a", NULL)} leaves a trailing space the tidy-up does not strip. With
#'   \code{ggir_exact = FALSE} both are removed; \code{GGIRversion} still gets an empty
#'   definition either way.
#'
#' @param daysummary The part-5 day summary: a data frame, a matrix, a character vector of
#'   column names, or NULL to describe no day summary. Only the column names are read.
#' @param personsummary The part-5 person summary, in the same three forms, or NULL.
#' @param ggir_exact TRUE, the default, reproduces GGIR's two defects, which a byte-compatible
#'   export needs. FALSE returns the same definitions without the leaked tokens and without
#'   the trailing space.
#' @return A list of two members, \code{daysummary} and \code{personsummary}, each either NULL
#'   (when that report was not given) or a data frame of \code{Variable} and \code{Definition}
#'   with one row per column of that report, in the report's own column order.
#' @examples
#' d <- c("ID", "filename", "calendar_date", "dur_day_total_IN_min", "ACC_day_mg")
#' p <- c("filename", "ID", "Nvaliddays", "dur_day_total_IN_min_pla", "boutdur.in")
#' dict <- raw.timeuse.dictionary(d, p)
#' dict$daysummary
#' # the leaked tokens: boutdur.in inherits the previous column's "- plain average"
#' dict$personsummary$Definition[5]
#' raw.timeuse.dictionary(d, p, ggir_exact = FALSE)$personsummary$Definition[5]
#' @seealso \code{\link{raw.timeuse}} for the analysis these reports summarise and
#'   \code{\link{write.ggir.milestone}}, which writes the two dictionaries into
#'   \code{results/variableDictionary} beside the reports they describe.
#' @export
raw.timeuse.dictionary <- function(daysummary = NULL, personsummary = NULL,
                                   ggir_exact = TRUE) {
  if (!is.logical(ggir_exact) || length(ggir_exact) != 1 || is.na(ggir_exact)) {
    stop("ggir_exact must be TRUE or FALSE", call. = FALSE)
  }
  cnames_of <- function(v, what) {
    if (is.null(v)) return(NULL)
    nm <- if (is.data.frame(v) || is.matrix(v)) colnames(v) else as.character(v)
    if (length(nm) == 0) {
      stop(what, " has no column names to describe", call. = FALSE)
    }
    if (anyNA(nm) || any(!nzchar(nm))) {
      stop("every column of ", what, " must have a name", call. = FALSE)
    }
    nm
  }
  dn <- cnames_of(daysummary, "daysummary")
  pn <- cnames_of(personsummary, "personsummary")
  out <- list(daysummary = NULL, personsummary = NULL)
  # the day summary first, as in GGIR; under ggir_exact its last tokens leak into the second
  elements <- NULL
  if (!is.null(dn)) {
    r <- .raw.dictionary.table(dn, person = FALSE, elements = elements,
                               ggir_exact = ggir_exact)
    out$daysummary <- r$dictionary
    elements <- r$elements
  }
  if (!is.null(pn)) {
    r <- .raw.dictionary.table(pn, person = TRUE, elements = elements,
                               ggir_exact = ggir_exact)
    out$personsummary <- r$dictionary
  }
  out
}
