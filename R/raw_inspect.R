# Ported from GGIR 3.3-9 R/g.inspectfile.R, R/inspect_binFile_brand.R, R/g.dotorcomma.R,
# R/g.extractheadervars.R, R/extractID.R, R/get_nw_clip_block_params.R,
# R/datadir2fnames.R, R/isfilelist.R and R/ismovisens.R, with the block size formulas of
# g.calibrate.R and the file discovery of g.part1.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Changes: the pieces are
# canhrActi-named functions with explicit arguments returning one info object; file
# renaming, console output and the double header read are removed; warnings are collected
# into the object instead of raised; the canhrActi-only behaviours (case-insensitive
# extensions, temporary lowercase .gt3x copy, small files inspected and flagged, the
# epoch-csv NA crash turned into an error) sit behind parameters.

# CONSTANTS

# The MONITOR and FORMAT code tables live in R/raw_constants.R as .RAW_MONITOR and
# .RAW_FORMAT (integer, because monc and dformc are compared with identical()).

.RAW_MONITOR_NAMES <- c("genea", "geneactive", "actigraph", "axivity", "movisens", "verisense",
                        "parmay_mtx")
.RAW_FORMAT_NAMES <- c("bin", "csv", "wav", "cwa", "csv", "gt3x", "BIN")

# in GGIR's listing order ("[.]gt3" matches .gt3x)
.RAW_EXTENSIONS <- c("csv", "bin", "wav", "cwa", "gt3x")

# PARAMETER PLUMBING

#' Defaults of the Inspection-Stage Parameters
#'
#' GGIR's defaults for the members of params_rawdata and params_general that the discovery and
#' inspection stage reads, plus the canhrActi-only switches. Used when no raw.params() object is
#' supplied.
#'
#' @return Named list.
#' @keywords internal
#' @noRd
.raw.inspect.defaults <- function() {
  list(
    desiredtz = "", configtz = NULL, idloc = 1, dynrange = NULL, minimumFileSizeMB = 2,
    chunksize = 1, nonwear_range_threshold = 150,
    rmc.dec = ".", rmc.firstrow.acc = NULL, rmc.firstrow.header = NULL, rmc.header.length = NULL,
    rmc.col.acc = 1:3, rmc.col.temp = NULL, rmc.col.time = NULL, rmc.unit.acc = "g",
    rmc.unit.temp = "C", rmc.unit.time = "POSIX", rmc.format.time = "%Y-%m-%d %H:%M:%OS",
    rmc.bitrate = NULL, rmc.dynamic_range = NULL, rmc.unsignedbit = TRUE, rmc.origin = "1970-01-01",
    rmc.desiredtz = NULL, rmc.configtz = NULL, rmc.sf = NULL, rmc.headername.sf = NULL,
    rmc.headername.sn = NULL, rmc.headername.recordingid = NULL, rmc.header.structure = NULL,
    rmc.check4timegaps = FALSE, rmc.scalefactor.acc = 1, rmc.noise = 13,
    ggir_exact = TRUE, rename_uppercase = FALSE, skip_small_files = FALSE
  )
}

#' Read One Member of a Params List With a Default
#'
#' @param params List (or NULL).
#' @param name Member name.
#' @param default Value when the member is absent.
#' @return The member, which may legitimately be NULL when it is present and NULL.
#' @keywords internal
#' @noRd
.raw.param <- function(params, name, default = NULL) {
  if (!is.null(params) && name %in% names(params)) params[[name]] else default
}

#' Resolve the Parameter List for This Stage
#'
#' With params NULL the overrides go through raw.params(), so they get GGIR's checks and
#' coercions. With a supplied list its members override the defaults and the named
#' overrides override both. Unknown override names are an error.
#'
#' @param params List or NULL.
#' @param overrides Named list of individual overrides.
#' @return Named list with every member of .raw.inspect.defaults().
#' @keywords internal
#' @noRd
.raw.inspect.params <- function(params = NULL, overrides = list()) {
  if (is.null(params)) {
    # overrides go through raw.params() so they get GGIR's type checks and coercions
    params <- do.call(raw.params, overrides)
    overrides <- list()
  }
  if (!is.list(params)) stop("params must be a list (see raw.params())")
  out <- .raw.inspect.defaults()
  for (nm in names(params)) out[nm] <- list(params[[nm]])
  if (length(overrides) > 0) {
    bad <- setdiff(names(overrides), names(out))
    if (length(bad) > 0) {
      stop("Unknown inspection parameter(s): ", paste(bad, collapse = ", "))
    }
    for (nm in names(overrides)) out[nm] <- list(overrides[[nm]])
  }
  out
}

#' Signal a Classed Inspection Error
#'
#' @param msg Message text (GGIR's text where GGIR stops).
#' @param path The file the error is about.
#' @keywords internal
#' @noRd
.raw.stop <- function(msg, path = NULL) {
  stop(structure(class = c("canhrActi_raw_inspect_error", "error", "condition"),
                 list(message = msg, call = NULL, path = path)))
}

# SMALL HELPERS

#' File Name as GGIR Derives It
#'
#' The last "/"-separated token. Backslashes are normalised to "/" by the callers first.
#'
#' @param datafile Path.
#' @return Character scalar.
#' @keywords internal
#' @noRd
.raw.filename <- function(datafile) {
  filename <- unlist(strsplit(as.character(datafile), "/"))
  filename[length(filename)]
}

#' Extension Token as GGIR's getbrand Sees It
#'
#' The last dot-separated token, or the one before it after peeling one ".gz" (any case).
#' Returns the spelling as found; callers lower-case it when they want case-insensitive matching.
#'
#' @param filename Character vector of file names.
#' @return Character vector of the same length ("" when there is no dot).
#' @keywords internal
#' @noRd
.raw.extension <- function(filename) {
  vapply(filename, function(fn) {
    extension <- unlist(strsplit(fn, "[.]"))
    if (length(extension) < 2) return("")
    if (tolower(extension[length(extension)]) == "gz") {
      extension <- extension[length(extension) - 1]
    } else {
      extension <- extension[length(extension)]
    }
    if (is.na(extension)) "" else extension
  }, character(1), USE.NAMES = FALSE)
}

#' Is This a Movisens Recording?
#'
#' Takes the first file of a directory (or the file itself), strips the last path component
#' and tests for unisens.xml next to it.
#'
#' @param data A directory or a file path with forward slashes.
#' @return Logical.
#' @keywords internal
#' @noRd
.raw.is.movisens <- function(data) {
  first_file <- dir(data, recursive = TRUE, full.names = TRUE)[1]
  isdir <- !is.na(first_file)
  if (isdir) data <- first_file
  data_tmp <- strsplit(data, "/")
  data_ln <- length(data_tmp[[1]]) - 1
  data <- paste(data_tmp[[1]][1:data_ln], collapse = "/")
  unisensXML <- paste(data, "unisens.xml", sep = "/")
  file.exists(unisensXML)
}

#' Brand of a .bin File
#'
#' A "Device Type" line in the first 69 text lines that mentions GENEActiv or GENEAsleep gives
#' GENEACTIV; otherwise bytes 513 to 516 equal to "MDTC" give PARMAY_MTX; otherwise
#' "not_recognised". As in GGIR the whole file is read into memory for the MDTC test.
#'
#' @param filename Path.
#' @return Integer monitor code or the string "not_recognised".
#' @keywords internal
#' @noRd
.raw.bin.brand <- function(filename) {
  mon <- "not_recognised"
  suppressWarnings({fh <- readLines(filename, 69)})
  suppressWarnings({deviceGeneactiv <- grep("Device Type", fh, ignore.case = TRUE)})
  if (length(deviceGeneactiv) > 0) {
    if (grepl("GENEActiv|GENEAsleep", fh[deviceGeneactiv])) mon <- .RAW_MONITOR[["GENEACTIV"]]
  } else {
    raw <- readBin(filename, "raw", file.info(filename)$size)
    header_raw <- raw[513:516]
    header <- rawToChar(header_raw[header_raw != 0], multiple = FALSE)
    if (header == "MDTC") mon <- .RAW_MONITOR[["PARMAY_MTX"]]
  }
  mon
}

#' Installed GGIRread Version, or NULL When It Is Not Installed
#' @keywords internal
#' @noRd
.raw.ggirread.version <- function() {
  if (requireNamespace("GGIRread", quietly = TRUE)) utils::packageVersion("GGIRread") else NULL
}

#' Require a Reader Package With an Install Hint
#' @keywords internal
#' @noRd
.raw.require <- function(pkg, what) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required to read %s: install.packages('%s')", pkg, what, pkg),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' A Lowercase-Extension Stand-In for a .GT3X File
#'
#' read.gt3x only unzips paths matching "\\.gt3x$", which is why GGIR renames .GT3X files
#' on disk. canhrActi instead places a lowercase-named stand-in under
#' tempdir()/canhrActi_raw/<key>/ and reads that: a hard link first (no bytes copied; it
#' needs the same volume and a file system that supports links) and a plain copy as the
#' fallback. The stand-in has its own directory so it cannot collide with the source on a
#' case-insensitive file system, and is reused while its size still matches the source.
#'
#' @param path Source path (forward slashes).
#' @param method "auto" (link, then copy), "link" or "copy".
#' @return The stand-in path with attribute "method" ("link", "copy" or "reused").
#' @keywords internal
#' @noRd
.raw.lowercase.gt3x <- function(path, method = c("auto", "link", "copy")) {
  method <- match.arg(method)
  fi <- file.info(path)
  bn <- basename(path)
  parts <- unlist(strsplit(bn, "[.]"))
  idx <- if (tolower(parts[length(parts)]) == "gz") length(parts) - 1 else length(parts)
  parts[idx] <- tolower(parts[idx])
  target_name <- paste(parts, collapse = ".")
  # a short, stable key from the normalised path plus size and mtime
  key_src <- utf8ToInt(tolower(normalizePath(path, winslash = "/", mustWork = FALSE)))
  key <- sprintf("%08x_%.0f_%.0f", sum(key_src * seq_along(key_src)) %% 2147483647,
                 fi$size, as.numeric(fi$mtime))
  root <- file.path(tempdir(), "canhrActi_raw", key)
  target <- file.path(root, target_name)
  if (file.exists(target)) {
    ti <- file.info(target)
    if (identical(ti$size, fi$size)) {
      attr(target, "method") <- "reused"
      return(target)
    }
    unlink(target)
  }
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  how <- NULL
  if (method %in% c("auto", "link")) {
    ok <- suppressWarnings(tryCatch(file.link(path, target), error = function(e) FALSE))
    if (isTRUE(ok) && file.exists(target)) how <- "link"
  }
  if (is.null(how) && method %in% c("auto", "copy")) {
    ok <- file.copy(path, target, overwrite = TRUE, copy.date = TRUE)
    if (isTRUE(ok) && file.exists(target)) how <- "copy"
  }
  if (is.null(how)) {
    stop(sprintf("Could not create a lowercase stand-in for %s under %s (method %s)",
                 path, root, method), call. = FALSE)
  }
  attr(target, "method") <- how
  target
}

# EXTENSION AND BRAND SWITCH

#' Brand, Format and Sample Frequency of One File
#'
#' getbrand() inside GGIR's g.inspectfile: the extension switch, the Movisens test, the .bin
#' brand sniff and the GENEActiv sample-rate rounding with the page-header override, the
#' csv sample-rate branches, the Axivity .cwa header read and the gt3x info.txt parse with
#' the corrupt-file warning. Deviations: the extension is matched case-insensitively; a
#' .gt3x whose extension is not lowercase is read through a temporary stand-in unless
#' rename_uppercase is TRUE, which reproduces GGIR's in-place rename and warning; the parsed
#' header objects are kept so the header is not read twice. GGIR's warnings are raised as
#' GGIR raises them (the caller collects them); canhrActi-only remarks go in notes.
#'
#' @param datafile Path with forward slashes.
#' @param filename File name (last path token); derived from datafile when NULL.
#' @param desiredtz Time zone forwarded to read.gt3x::parse_gt3x_info and GGIRread::readAxivity.
#' @param rename_uppercase Reproduce GGIR's on-disk rename of .GT3X files.
#' @param unzip_once Read the gt3x info from the cached extraction.
#' @param lowercase_method How the lowercase stand-in is made: "auto", "link" or "copy".
#' @return List: dformat, mon, sf, datafile (the path to read from now on), raw_header (the
#'   parsed header object of the brand or NULL), extension (as spelled), uppercase_extension,
#'   ggir_accepts (would GGIR's case-sensitive switch have accepted this spelling), renamed,
#'   stand_in_method, notes.
#' @keywords internal
#' @noRd
.raw.brand <- function(datafile, filename = NULL, desiredtz = "", rename_uppercase = FALSE,
                       unzip_once = FALSE,
                       lowercase_method = "auto") {
  if (is.null(filename)) filename <- .raw.filename(datafile)
  sf <- c(); isitageneactive <- c(); mon <- c(); dformat <- c() # generating empty variables
  raw_header <- NULL; renamed <- FALSE; stand_in_method <- NA_character_; notes <- character()
  label_path <- datafile
  extension <- .raw.extension(filename)
  ext_key <- tolower(extension)
  ggir_accepts <- extension %in% c("bin", "BIN", "cwa", "CWA", "gt3x", "GT3X", "csv", "wav")
  uppercase_extension <- !identical(ext_key, extension)
  switch(ext_key,
         "bin" = { dformat <- .RAW_FORMAT[["BIN"]] },
         "cwa" = { mon <- .RAW_MONITOR[["AXIVITY"]]
           dformat <- .RAW_FORMAT[["CWA"]]
         },
         "gt3x" = { mon <- .RAW_MONITOR[["ACTIGRAPH"]]
           dformat <- .RAW_FORMAT[["GT3X"]]
           if (extension != "gt3x") {
             if (isTRUE(rename_uppercase)) {
               # GGIR's in-place rename, reproduced on request only
               if (file.access(datafile, 2) == 0) { # test for write access to file
                 # rename file to be lower case gt3x extension
                 file.rename(from = datafile, to = gsub(pattern = paste0(".", extension), replacement = ".gt3x", x = datafile))
                 datafile <- gsub(pattern = paste0(".", extension), replacement = ".gt3x", x = datafile)
                 renamed <- TRUE
                 warning("\nWe have renamed the GT3X file to gt3x because GGIR dependency read.gt3x cannot handle uper case extension")
               } else {
                 stop("\nGGIR needs to change the file extension from GT3X to gt3x, but it does not seem to have write permission to the file.")
               }
             } else {
               datafile <- .raw.lowercase.gt3x(datafile, method = lowercase_method)
               stand_in_method <- attr(datafile, "method")
               attr(datafile, "method") <- NULL
               .raw.gt3x.forget(datafile)
               notes <- c(notes, paste0(
                 "Extension .", extension, ": GGIR renames such a file to .gt3x on disk; canhrActi read it ",
                 "through a temporary lowercase stand-in (", stand_in_method, ") at ", datafile,
                 " and left the original untouched."))
             }
           }
         },
         "csv" = { dformat <- .RAW_FORMAT[["CSV"]]
           testheader <- read.csv(datafile, nrow = 1, skip = 0, header = FALSE)

           if (grepl("ActiGraph", testheader[1], fixed = TRUE)) {
             mon <- .RAW_MONITOR[["ACTIGRAPH"]]
           } else {
             testcsv <- read.csv(datafile, nrow = 10, skip = 10)
             testcsvtopline <- read.csv(datafile, nrow = 2, skip = 1)
             if (ncol(testcsv) == 2 && ncol(testcsvtopline) < 4) {
               mon <- .RAW_MONITOR[["GENEACTIV"]]
             } else if (ncol(testcsv) >= 4 && ncol(testcsvtopline) >= 4) {
               mon <- .RAW_MONITOR[["AXIVITY"]]
             } else {
               .raw.stop(paste0("\nError processing ", filename, ": unrecognised csv file format.\n"), label_path)
             }
           }
         },
         "wav" = { .raw.stop(paste0("\nError processing ", filename, ": Axivity .wav file format is no longer supported.\n"), label_path) },
         { .raw.stop(paste0("\nError processing ", filename, ": unrecognised file format.\n"), label_path) }
  )
  if (!ggir_accepts) {
    notes <- c(notes, paste0(
      "Extension .", extension, " is not in GGIR's case-sensitive extension switch (GGIR would stop with ",
      "'unrecognised file format'); canhrActi matched it as .", ext_key, "."))
  }

  if (.raw.is.movisens(datafile)) {
    dformat <- .RAW_FORMAT[["BIN"]]
    mon <- .RAW_MONITOR[["MOVISENS"]]
    sf <- 64
    header <- "no header"
  } else if (dformat == .RAW_FORMAT[["BIN"]]) { # .bin and not movisens, could be GENEActiv or Parmay Matrix
    mon <- .raw.bin.brand(filename = datafile)
    if (mon == .RAW_MONITOR[["PARMAY_MTX"]]) {
      .raw.require("GGIRread", "Parmay Matrix .BIN files")
      if (utils::packageVersion("GGIRread") < "1.0.4") {
        stop("Please update R package GGIRread to version 1.0.4 or higher", call. = FALSE)
      }
    }
    # try read the file as if it is a geneactiv and store output in variable 'isitageneactive'
    if (mon == .RAW_MONITOR[["GENEACTIV"]]) {
      .raw.require("GGIRread", "GENEActiv .bin files")
      # GGIR evaluates this test while isitageneactive is still c(), so it is always TRUE
      # (all(logical(0))); kept as written, the else branch is unreachable.
      if (all(names(isitageneactive) %in% c("header", "data.out") == TRUE)) {
        isitageneactive <- GGIRread::readGENEActiv(filename = datafile, start = 0, end = 1)
        raw_header <- isitageneactive$header
        tmp <- unlist(strsplit(unlist(as.character(isitageneactive$header$SampleRate)), " "))[1]
        # occasionally we'll get a decimal seperated by comma; if so, replace the comma with a dot
        tmp <- sub(",", ".", tmp, fixed = TRUE)
        sf <- as.numeric(tmp)
        if (sf != round(sf)) sf <- round(sf) # Round GENEACtiv sampling rate. In g.readaccfile the raw data is resampled to this sampling rate
        # also try to read sf from first page header
        sf_r <- sf
        csvr <- c()
        suppressWarnings(expr = {
          try(expr = {csvr <- as.matrix(read.csv(datafile, nrow = 10,
                                                 skip = 200, sep = ""))
          }, silent = TRUE)
        })
        if (length(csvr) > 1) {
          for (ii in 1:nrow(csvr)) {
            tmp3 <- unlist(strsplit(as.character(csvr[ii, 1]), "quency:")) # part of 'frequency'
            if (length(tmp3) > 1) {
              # occasionally we'll get a decimal seperated by comma; if so, replace the comma with a dot
              tmp3 <- sub(",", ".", tmp3, fixed = TRUE)
              sf_r <- as.numeric(tmp3)
            }
          }
          if (length(sf_r) > 0 && !is.na(sf_r)) {
            if (sf_r != sf && abs(sf_r - sf) > 5) { # use pageheader sample frequency if it is not the same as header sample frequency
              sf <- sf_r
              warning(paste0("sample frequency used from page header: ", sf, " Hz"))
            }
          }
        }
      } else {
        .raw.stop(paste0("\nError processing ", filename, ": possibibly a corrupt GENEActive file"), label_path)
      }
    } else if (mon == .RAW_MONITOR[["PARMAY_MTX"]]) {
      header <- NULL
      sf <- GGIRread::readParmayMatrix(datafile, output = "sf")
    } else {
      .raw.stop(paste0("\nError processing ", filename, ": unrecognised .bin file"), label_path)
    }
  } else if (dformat == .RAW_FORMAT[["CSV"]]) { # no checks for corrupt file yet...maybe not needed for csv-format?
    if (mon == .RAW_MONITOR[["GENEACTIV"]]) {
      tmp <- read.csv(datafile, nrow = 50, skip = 0)
      tmp <- as.character(tmp[which(as.character(tmp[, 1]) == "Measurement Frequency"), 2])
      tmp <- as.numeric(unlist(strsplit(tmp, " "))[1])
      # occasionally we'll get a decimal seperated by comma; if so, replace the comma with a dot
      tmp <- sub(",", ".", tmp, fixed = TRUE)
      sf <- as.numeric(tmp)
    } else if (mon == .RAW_MONITOR[["ACTIGRAPH"]]) {
      tmp <- read.csv(datafile, nrow = 9, skip = 0)
      tmp <- colnames(tmp)
      tmp <- as.character(unlist(strsplit(tmp, ".Hz"))[1])
      # following suggestion by XInyue on github https://github.com/wadpac/GGIR/issues/102 replaced by:
      tmp <- as.character(unlist(strsplit(tmp, ".at.", fixed = TRUE))[2])
      # occasionally we'll get a decimal seperated by comma; if so, replace the comma with a dot
      tmp <- sub(",", ".", tmp, fixed = TRUE)
      sf <- as.numeric(tmp)
    } else if (mon == .RAW_MONITOR[["AXIVITY"]]) {
      # sample frequency is not stored
      tmp <- read.csv(datafile, nrow = 100000, skip = 0)
      tmp <- as.numeric(as.POSIXct(tmp[, 1], origin = "1970-01-01"))
      sf <- length(tmp) / (tmp[length(tmp)] - tmp[1])
      sf <- floor((sf) / 5) * 5 # round down to nearest integer of 5, we never want to assume that there is more frequency content in a signal than there truly is
    }
  } else if (dformat == .RAW_FORMAT[["CWA"]]) {
    .raw.require("GGIRread", "Axivity .cwa files")
    PP <- GGIRread::readAxivity(datafile, start = 1, end = 10, desiredtz = desiredtz)
    H <- PP$header
    sf <- H$frequency
    raw_header <- H
  } else if (dformat == .RAW_FORMAT[["GT3X"]]) {
    .raw.require("read.gt3x", "ActiGraph .gt3x files")
    # from the cached extraction when unzip_once is on; parse_gt3x_info reads only info.txt
    info <- try(expr = {read.gt3x::parse_gt3x_info(.raw.gt3x.read.path(datafile, unzip_once),
                                                   tz = desiredtz)}, silent = TRUE)
    if (inherits(info, "try-error") == TRUE || is.null(info)) {
      warning(paste0("\nFile info could not be extracted from ", label_path), call. = FALSE)
      sf <- NULL # set to NULL in order to tell other GGIR functions that file was corrupt
    } else {
      info <- info[lengths(info) != 0] # remove odd NULL in the list
      sf <- info[["Sample Rate"]]
      raw_header <- info
    }
  }
  invisible(list(dformat = dformat, mon = mon, sf = sf, datafile = datafile, raw_header = raw_header,
                 extension = extension, uppercase_extension = uppercase_extension,
                 ggir_accepts = ggir_accepts, renamed = renamed, stand_in_method = stand_in_method,
                 notes = notes))
}

# HEADER NORMALISATION

#' Header data.frame Exactly as GGIR Formats It
#'
#' The main body of GGIR's g.inspectfile: the header read per brand, the sample-frequency
#' warning, the normalisation into a one-column data.frame with row names and the
#' Verisense re-detection for ActiGraph csv. The header objects parsed by .raw.brand are
#' reused instead of being read a second time.
#'
#' @param dformat,mon Integer codes.
#' @param sf Sample frequency (NULL for a corrupt gt3x).
#' @param raw_header Parsed header object from .raw.brand.
#' @param datafile Path to read from (the lowercase stand-in for .GT3X).
#' @param filename File name; replaced by the parent folder name for Movisens.
#' @param label_path Path named in warning texts (the user's original path).
#' @param adhoc_header Header returned by the ad-hoc csv reader (AD_HOC_CSV only).
#' @return List: header, sf, mon, filename.
#' @keywords internal
#' @noRd
.raw.format.header <- function(dformat, mon, sf, raw_header = NULL, datafile, filename,
                               label_path = datafile, adhoc_header = NULL) {
  header <- NULL
  H <- NULL
  if (dformat == .RAW_FORMAT[["BIN"]]) {
    if (mon == .RAW_MONITOR[["GENEACTIV"]]) {
      H <- raw_header # GGIR reads the header again here
    } else if (mon == .RAW_MONITOR[["MOVISENS"]]) {
      H <- "file does not have header" # these files have no header
      xmlfile <- paste0(dirname(datafile), "/unisens.xml")
      if (file.exists(xmlfile)) {
        # read the xml as text to avoid a dependency
        header <- as.character(read.csv(xmlfile, nrow = 1))
        tmp1 <- unlist(strsplit(header, "measurementId="))[2]
        ID <- gsub(pattern = " ", replacement = "", unlist(strsplit(tmp1, " timestampStart"))[1])

        header <- paste0(read.csv(xmlfile, nrow = 10, skip = 2), collapse = " ")
        tmp1 <- unlist(strsplit(header, "sensorSerialNumber value="))[2]
        SN <- unlist(strsplit(tmp1, "/>"))[1]
        header <- data.frame(serialnumber = SN, ID = ID)
        if (length(header) > 1) {
          H <- t(header)
        }
        filename <- unlist(strsplit(as.character(datafile), "/"))
        filename <- filename[length(filename) - 1]
      }
    } else if (mon == .RAW_MONITOR[["PARMAY_MTX"]]) {
      H <- "file does not have header" # these files have no header
    }
  } else if (dformat == .RAW_FORMAT[["CSV"]]) {
    if (mon == .RAW_MONITOR[["ACTIGRAPH"]]) {
      H <- read.csv(datafile, nrow = 9, skip = 0)
    } else if (mon == .RAW_MONITOR[["AXIVITY"]]) {
      H <- "file does not have header" # these files have no header
    }
  } else if (dformat == .RAW_FORMAT[["CWA"]]) {
    H <- raw_header # GGIR reads the header again here
  } else if (dformat == .RAW_FORMAT[["AD_HOC_CSV"]]) { # csv data in a user-specified format
    header <- adhoc_header
  } else if (dformat == .RAW_FORMAT[["GT3X"]]) { # gt3x
    if (is.null(raw_header)) {
      # GGIR warns "File info could not be extracted" a second time here; .raw.brand already did
      sf <- NULL
      H <- NULL
      header <- NULL
    } else {
      info <- raw_header

      H <- matrix("", length(info), 2)
      H[, 1] <- names(info)
      for (ci in 1:length(info)) {
        if (inherits(info[[ci]], "POSIXct") == TRUE) {
          H[ci, 2] <- format(info[[ci]])
        } else {
          H[ci, 2] <- as.character(info[[ci]])
        }
      }
      sf <- as.numeric(H[which(H[, 1] == "Sample Rate"), 2])
    }
  }
  if (is.null(sf) || sf == 0) {
    warning(paste0("\nSample frequency not recognised in ", basename(label_path)), call. = FALSE)
  }

  if (dformat != .RAW_FORMAT[["AD_HOC_CSV"]] && is.null(sf) == FALSE) {
    H <- as.matrix(H)
    if (ncol(H) == 3 && dformat == .RAW_FORMAT[["CSV"]] && mon == .RAW_MONITOR[["ACTIGRAPH"]]) {
      if (length(which(is.na(H[, 2]) == FALSE)) == 0) {
        H <- as.matrix(H[, 1])
      }
    }
    if (ncol(H) == 1 && dformat == .RAW_FORMAT[["CSV"]]) {
      if (mon == .RAW_MONITOR[["ACTIGRAPH"]]) {
        vnames <- c("Number:", "t Time", "t Date", ":ss)", "d Time", "d Date", "Address:", "Voltage:", "Mode =")
        Hvalues <- Hnames <- rep(" ", length(H))
        firstline <- colnames(H)
        for (run in 1:length(H)) {
          for (runb in 1:length(vnames)) {
            tmp <- unlist(strsplit(H[run], vnames[runb]))
            if (length(tmp) > 1) {
              Hnames[run] <- paste(tmp[1], vnames[runb], sep = "")
              Hvalues[run] <- paste(tmp[2], sep = "")
            }
          }
        }
        H <- cbind(Hnames, Hvalues)
        H <- rbind(c("First line", firstline), H)
      } else {
        H <- cbind(c(1:length(H)), H)
      }
    }
    if (dformat == .RAW_FORMAT[["CWA"]]) {
      header <- data.frame(value = H, row.names = rownames(H), stringsAsFactors = TRUE)
    } else {
      if ((mon == .RAW_MONITOR[["GENEACTIV"]] && dformat == .RAW_FORMAT[["BIN"]]) || (mon == .RAW_MONITOR[["MOVISENS"]] && length(H) > 0)) {
        varname <- rownames(as.matrix(H))
        H <- data.frame(varname = varname, varvalue = as.character(H), stringsAsFactors = TRUE)
      } else if (dformat != .RAW_FORMAT[["AD_HOC_CSV"]]) {
        if (length(H) > 1 && class(H)[1] == "matrix") H <- data.frame(varname = H[, 1], varvalue = H[, 2], stringsAsFactors = TRUE)
      }
    }
    if (dformat != .RAW_FORMAT[["CWA"]] && length(H) > 1 && (class(H)[1] == "matrix" || class(H)[1] == "data.frame")) {
      RowsWithData <- which(is.na(H[, 1]) == FALSE)
      header <- data.frame(value = H[RowsWithData, 2], row.names = H[RowsWithData, 1], stringsAsFactors = TRUE)
    }
    if (H[1, 1] == "file does not have header") { # no header
      header <- "no header"
    }
    if (mon == .RAW_MONITOR[["ACTIGRAPH"]] && dformat != .RAW_FORMAT[["GT3X"]]) {
      verisense_check <- substr(colnames(read.csv(datafile, nrow = 1)[1]), start = 36, stop = 44)
      if (identical('Verisense', toString(verisense_check))) {
        mon <- .RAW_MONITOR[["VERISENSE"]]
      }
    }
  }
  list(header = header, sf = sf, mon = mon, filename = filename)
}

# DECIMAL SEPARATOR

#' Decimal Separator of the Data in a File
#'
#' GGIR's g.dotorcomma. Starts from getOption("OutDec"); csv: reads 10 rows from row 100,
#' stepping 10000 rows up to 1e6, stops at the first non-zero numeric cell (row 2, column 2) and calls
#' "," when that cell contains a comma or is not numeric; GENEActiv .bin: pages 1 to 3, cell
#' \code{data.out[2,2]}; Parmay: "."; .cwa: blocks 1 to 10; .gt3x: batches 1 to 10. raw.inspect
#' calls it with rmc.dec only, as GGIR does, so the ad-hoc csv override is not taken from
#' there. GGIR's exists("deci") looks through enclosing environments; here it is restricted
#' to the function frame.
#'
#' @param inputfile Path to read.
#' @param dformat,mon Integer codes.
#' @param rmc.dec Decimal separator of an ad-hoc csv (NULL means OutDec).
#' @param rmc.firstrow.acc Ad-hoc csv switch (NULL from raw.inspect).
#' @param unzip_once,stream_gt3x The optional gt3x read paths of .raw.gt3x.read.batch.
#' @return "." or ",".
#' @keywords internal
#' @noRd
.raw.decimal <- function(inputfile, dformat, mon, rmc.dec = NULL, rmc.firstrow.acc = NULL,
                         unzip_once = FALSE, stream_gt3x = FALSE) {
  decn <- getOption("OutDec") # extract system decimal separator
  if (length(decn) == 0) decn <- "." # assume . if not retrieved
  if (is.null(rmc.dec)) rmc.dec <- decn
  if (length(rmc.firstrow.acc) == 1) {
    dformat <- .RAW_FORMAT[["AD_HOC_CSV"]]
    mon <- .RAW_MONITOR[["AD_HOC"]]
    decn <- rmc.dec
  }
  if (dformat == .RAW_FORMAT[["CSV"]]) {
    skiprows <- 100
    # some ActiGraph files start with many zeros, which hides the separator
    while (skiprows < 1000000) {
      tmp <- try(expr = {as.matrix(read.csv(inputfile, skip = skiprows, nrow = 10))}, silent = TRUE)
      if (inherits(tmp, "try-error")) break # nothing left in the file to read
      deci <- tmp

      skiprows <- skiprows + 10000
      if (length(unlist(strsplit(as.character(deci[2, 2]), ","))) > 1) {
        decn <- ","
        break
      }
      numtemp <- suppressWarnings(as.numeric(deci[2, 2]))
      if (is.na(numtemp) == FALSE && numtemp != 0) break
    }
    if (!exists("deci", inherits = FALSE)) stop("Problem with reading .csv file in GGIR function dotorcomma")
    if (is.na(suppressWarnings(as.numeric(deci[2, 2]))) == TRUE & decn == ".") decn <- ","
  } else if (dformat == .RAW_FORMAT[["BIN"]]) {
    if (mon == .RAW_MONITOR[["GENEACTIV"]]) {
      try(expr = {deci <- GGIRread::readGENEActiv(filename = inputfile,
                                                  start = 1, end = 3)}, silent = TRUE)
      if (!exists("deci", inherits = FALSE)) stop("Problem with reading .bin file in GGIR function dotorcomma")
      if (is.na(suppressWarnings(as.numeric(deci$data.out[2, 2]))) == TRUE & decn == ".") decn <- ","
    } else if (mon == .RAW_MONITOR[["PARMAY_MTX"]]) {
      decn <- "."
    }
  } else if (dformat == .RAW_FORMAT[["CWA"]]) {
    try(expr = {deci <- GGIRread::readAxivity(filename = inputfile, start = 1, end = 10,
                                              interpolationType = 1)$data}, silent = TRUE)
    if (!exists("deci", inherits = FALSE)) stop("Problem with reading .cwa file in GGIR function dotorcomma")
    if (is.na(suppressWarnings(as.numeric(deci[2, 2]))) == TRUE & decn == ".") decn <- ","
  } else if (dformat == .RAW_FORMAT[["GT3X"]]) {
    if (length(grep(pattern = "[.]GT", x = inputfile)) > 0 & file.exists(inputfile) == FALSE) {
      inputfile <- gsub(pattern = "[.]GT3X", replacement = "[.]gt3x", x = inputfile)
    }
    # through the block reader, so unzip_once and stream_gt3x apply here too
    try(expr = {deci <- as.data.frame(.raw.gt3x.read.batch(inputfile, 1, 10, unzip_once = unzip_once,
                                                            stream_gt3x = stream_gt3x))}, silent = TRUE)
    if (!exists("deci", inherits = FALSE)) stop("Problem with reading .gt3x file in GGIR function dotorcomma")
    if (is.na(suppressWarnings(as.numeric(deci[2, 2]))) == TRUE & decn == ".") decn <- ","
  }
  decn
}

# HEADER VARIABLES AND PARTICIPANT ID

#' Header Variables of an Inspection Object
#'
#' GGIR's g.extractheadervars on a canhrActi_raw_info object or a GGIR I object (both carry
#' header, monn, dformn and filename). Empty header values become "not stored in header";
#' the branch is chosen on the monitor name. For ActiGraph gt3x the serial is
#' "<Serial Number>_firmware_<Firmware>"; for ActiGraph csv the serial keeps its leading
#' space and the firmware is cut from the first header line.
#'
#' @param I A canhrActi_raw_info or GGIR I object.
#' @return List: ID, iID, HN, sensor.location, SX, deviceSerialNumber (GGIR's names).
#' @keywords internal
#' @noRd
.raw.header.vars <- function(I) {
  header <- I$header
  mon <- I$monn
  hnames <- rownames(header)
  hvalues <- as.character(as.matrix(header))
  pp <- which(hvalues == "")
  if (length(pp) > 0) hvalues[pp] <- c("not stored in header")
  # set defaults
  ID <- I$filename # recording ID
  SX <- "not available" # sex
  iID <- "not extracted" # investigator ID
  HN <- "not extracted" # handedness
  sensor.location <- "not extracted" # body location
  deviceSerialNumber <- "not extracted"
  # attempt to extract from hvalues and hnames
  if (mon == "genea") {
    IDd <- hvalues[which(hnames == "Volunteer_Number")]
    ID <- as.character(unlist(IDd))
    iIDd <- hvalues[which(hnames == "Investigator_Id")]
    iID <- as.character(unlist(iIDd))
    sensor.location <- hvalues[which(hnames == "Body_Location")]
    deviceSerialNumber <- hvalues[which(hnames == "Serial_Number")] # serial number
  } else if (mon == "geneactive") {
    check_GENEAread <- which(hnames == "Subject_Code")
    if (length(check_GENEAread) > 0) {
      # This if-statement can be deprecated once GENEAread is deprecated as a dependency
      ID <- hvalues[which(hnames == "Subject_Code")]
      iID <- hvalues[which(hnames == "Investigator_ID")] # investigator ID
      HN <- hvalues[which(hnames == "Handedness_Code")] # handedness
      sensor.location <- as.character(as.matrix(hvalues[which(hnames == "Device_Location_Code")])) # body location
      SX <- hvalues[which(hnames == "Sex")] # gender
      deviceSerialNumber <- hvalues[which(hnames == "Device_Unique_Serial_Code")] # serial number
    } else {
      ID <- hvalues[which(hnames == "RecordingID")]
      HN <- hvalues[which(hnames == "Handedness")]
      sensor.location <- hvalues[which(hnames == "DeviceLocation")]
      deviceSerialNumber <- hvalues[which(hnames == "serial_number")] # serial number
    }
  } else if (mon == "actigraph" | mon == 'verisense') {
    if (I$dformn == "gt3x") {
      header <- I$header
      deviceSerialNumber <- as.character(header["Serial Number", ])
      firmwareversion <- as.character(header["Firmware", ])
    } else { # .csv format
      deviceSerialNumber <- as.character(I$header$value[which(row.names(I$header) == "Serial Number:")])
      if (length(deviceSerialNumber) == 0) deviceSerialNumber <- "not extracted" # serial number
      # the firmware version is appended to the serial number
      firmwareversion <- unlist(strsplit(unlist(strsplit(as.character(I$header$value[1]), "Firmware[.]"))[2], "[.]date"))[1]
    }
    if (length(firmwareversion) == 1) deviceSerialNumber <- paste0(deviceSerialNumber, "_firmware_", firmwareversion)
  } else if (mon == "axivity" | mon == "movisens") {
    if (mon == "actigraph") {
      deviceSerialNumber <- as.character(I$header$value[grep(pattern = "serial number", x = row.names(I$header), ignore.case = TRUE, value = FALSE)])
    }
    if (mon == "axivity") {
      seriali <- which(hnames %in% c("uniqueSerialCode", "IART2Id"))
      if (length(seriali) > 0) deviceSerialNumber <- hvalues[seriali[1]] # serial number
    }
    if (mon == "movisens") {
      deviceSerialNumber <- as.character(I$header$value[which(row.names(I$header) == "serialnumber")])
      ID <- as.character(I$header$value[which(row.names(I$header) == "ID")])
    }
  } else if (mon == "unknown") {
    if (length(which(hnames == "recordingID")) > 0) {
      ID <- hvalues[which(hnames == "recordingID")]
    }
    if (length(which(hnames == "device_serial_number")) > 0) {
      deviceSerialNumber <- hvalues[which(hnames == "device_serial_number")] # serial number
    }
  }
  invisible(list(ID = ID, iID = iID, HN = HN, sensor.location = sensor.location,
                 SX = SX, deviceSerialNumber = deviceSerialNumber))
}

#' Participant ID From the Header Variables and the File Name
#'
#' GGIR's extractID. idloc 1 keeps the header ID (the file
#' name for ActiGraph); 2 the part of the file name before the first "_"; 3 the part before a
#' single hyphen (Pelotas legacy); 4 the IDd field (never set by the header variables); 5 before
#' the first space; 6 before the first "."; 7 before the first "-". An empty result falls back to
#' the file name with GGIR's warning; otherwise every space is removed.
#'
#' @param hvars Output of .raw.header.vars.
#' @param idloc Integer 1 to 7.
#' @param fname File name.
#' @return Character scalar.
#' @keywords internal
#' @noRd
.raw.extract.id <- function(hvars, idloc, fname) {
  ID <- hvars$ID
  iID <- hvars$iID
  IDd <- hvars$IDd

  # legacy handling for the Pelotas cohort
  ID2 <- ID
  iID2 <- iID
  if (idloc == 3) { # remove hyphen in id-name for Pelotas id-numbers
    get_char_before_hyphen <- function(x) {
      x2 <- c()
      for (j in 1:length(x)) {
        temp <- unlist(strsplit(x, "-"))
        if (length(temp) == 2) {
          x2[j] <- as.character(temp[1])
        } else {
          x2[j] <- as.character(x[j])
        }
      }
      return(x2)
    }
    ID2 <- get_char_before_hyphen(ID)
    iID2 <- get_char_before_hyphen(iID)
  }
  ID_NAs <- which(ID == "NA")
  ID2_NAs <- which(ID == "NA")
  if (length(ID_NAs) > 0) ID[ID_NAs] <- iID[ID_NAs]
  if (length(ID2_NAs) > 0) ID2[ID2_NAs] <- iID2[ID2_NAs]
  if (idloc == 2) { # default is idloc=1, where ID just stays ID
    ID <- unlist(strsplit(fname, "_"))[1]
  } else if (idloc == 3) {
    ID <- ID2
  } else if (idloc == 4) {
    ID <- IDd
  } else if (idloc == 5) {
    ID <- unlist(strsplit(fname, " "))[1]
  } else if (idloc == 6) {
    ID <- unlist(strsplit(fname, "[.]"))[1]
  } else if (idloc == 7) {
    ID <- unlist(strsplit(fname, "-"))[1]
  }
  if (length(ID) == 0) { # If ID could not be extracted
    ID <- basename(fname)
    warning(paste0("\nUnable to extract ID from, ", fname, ". Using filname instead. ",
                   " You may want to check argument idloc, which is currently set to ", idloc))
  } else {
    ID <- gsub(pattern = " ", replacement = "", ID)
  }
  return(ID)
}

# CLIPPING THRESHOLD, NON-WEAR CRITERIA AND BLOCK SIZES

#' Clipping Threshold, Non-Wear Criteria and Block Size for One Pass
#'
#' GGIR's get_nw_clip_block_params for the g.getmeta pass (24 h blocks) and the block-size
#' formulas of g.calibrate (12 h blocks, Verisense folded into ActiGraph) for the
#' calibration pass. The Parmay dynamic range read from the file overrides the user's
#' dynrange, from info$dynrange_file. Dynamic range for ActiGraph comes from the serial
#' prefix only: CLE 6 g, MOS 8 g, NEO 6 g; everything else takes the user's dynrange or
#' the 8 g assumption (clipthres 7.5), Movisens 15.5, ad-hoc csv rmc.dynamic_range.
#' racriter is nonwear_range_threshold/1000 (0.20 for Verisense); sdcriter 0.013
#' (rmc.noise * 1.2 for ad-hoc csv, in g).
#'
#' @param info A canhrActi_raw_info (needs monc, dformc, sf, device_serial, dynrange_file).
#' @param params Optional params list; the explicit arguments below default from it.
#' @param pass "getmeta" or "calibrate".
#' @param chunksize,dynrange,nonwear_range_threshold,rmc.noise,rmc.dynamic_range GGIR's
#'   params_rawdata members.
#' @return List: clipthres, blocksize, sdcriter, racriter (GGIR's four, in GGIR's order), then
#'   dynrange (the effective value, clipthres + 0.5) and dynrange_source ("serial_prefix",
#'   "user", "file", "assumed", "movisens_assumed" or "rmc.dynamic_range").
#' @keywords internal
#' @noRd
.raw.clip.block.params <- function(info, params = NULL, pass = c("getmeta", "calibrate"),
                                   chunksize = .raw.param(params, "chunksize", 1),
                                   dynrange = .raw.param(params, "dynrange", NULL),
                                   nonwear_range_threshold = .raw.param(params, "nonwear_range_threshold", 150),
                                   rmc.noise = .raw.param(params, "rmc.noise", 13),
                                   rmc.dynamic_range = .raw.param(params, "rmc.dynamic_range", NULL)) {
  pass <- match.arg(pass)
  monc <- info$monc
  dformat <- info$dformc
  sf <- info$sf
  deviceSerialNumber <- if (is.null(info$device_serial)) "" else info$device_serial
  dynrange_source <- if (length(dynrange) > 0) "user" else "assumed"
  if (monc == .RAW_MONITOR[["PARMAY_MTX"]] && length(info$dynrange_file) > 0) {
    # the file's own dynamic range wins
    dynrange <- info$dynrange_file
    dynrange_source <- "file"
  }

  blocksize <- NULL
  if (!is.null(sf)) {
    if (pass == "getmeta") {
      blocksize <- round(14512 * (sf / 50) * chunksize)
      if (monc == .RAW_MONITOR[["GENEA"]]) blocksize <- round(21467 * (sf / 80) * chunksize)
      if (monc == .RAW_MONITOR[["ACTIGRAPH"]] && dformat == .RAW_FORMAT[["CSV"]]) blocksize <- round(blocksize)
      if (monc == .RAW_MONITOR[["ACTIGRAPH"]] && dformat == .RAW_FORMAT[["GT3X"]]) blocksize <- (24 * 3600) * chunksize
      if (monc == .RAW_MONITOR[["AXIVITY"]] && dformat == .RAW_FORMAT[["CWA"]]) {
        v <- .raw.ggirread.version()
        if (is.null(v) || v >= "0.3.1") {
          # 24-hour block; CWA data blocks hold 40, 80 or 120 samples, 80 taken as the average
          blocksize <- round(24 * 3600 * sf / 80 * chunksize)
        } else {
          blocksize <- round(blocksize * 1.0043)
        }
      }
      if (monc == .RAW_MONITOR[["AXIVITY"]] && dformat == .RAW_FORMAT[["CSV"]]) blocksize <- round(blocksize)
      if (monc == .RAW_MONITOR[["MOVISENS"]]) blocksize <- sf * 60 * 1440
      if (monc == .RAW_MONITOR[["VERISENSE"]] && dformat == .RAW_FORMAT[["CSV"]]) blocksize <- round(blocksize)
      if (monc == .RAW_MONITOR[["PARMAY_MTX"]]) blocksize <- round(1440 / 2 * chunksize)
    } else {
      # the calibration pass
      mon <- monc
      if (mon == .RAW_MONITOR[["VERISENSE"]]) mon <- .RAW_MONITOR[["ACTIGRAPH"]]
      blocksize <- round((14512 * (sf / 50)) * (chunksize * 0.5))
      if (mon == .RAW_MONITOR[["MOVISENS"]]) blocksize <- (sf * 60 * 1440) / 2 # Around 12 hours of data for movisens
      if (mon == .RAW_MONITOR[["ACTIGRAPH"]] && dformat == .RAW_FORMAT[["GT3X"]]) blocksize <- (12 * 3600) * chunksize
      if (mon == .RAW_MONITOR[["AXIVITY"]] && dformat == .RAW_FORMAT[["CWA"]]) {
        v <- .raw.ggirread.version()
        if (is.null(v) || v >= "0.3.1") {
          blocksize <- round(12 * 3600 * sf / 80 * chunksize)
        }
      }
      if (mon == .RAW_MONITOR[["PARMAY_MTX"]] && dformat == .RAW_FORMAT[["BIN"]]) {
        # Matrix data packets have ~2 minutes of data
        blocksize <- round(60 * 12 / 2 * chunksize)
      }
    }
  }

  if (monc == .RAW_MONITOR[["ACTIGRAPH"]]) {
    # If Actigraph then try to specify dynamic range based on Actigraph model
    if (length(grep(pattern = "CLE", x = deviceSerialNumber)) == 1) {
      dynrange <- 6
      dynrange_source <- "serial_prefix"
    } else if (length(grep(pattern = "MOS", x = deviceSerialNumber)) == 1) {
      dynrange <- 8
      dynrange_source <- "serial_prefix"
    } else if (length(grep(pattern = "NEO", x = deviceSerialNumber)) == 1) {
      dynrange <- 6
      dynrange_source <- "serial_prefix"
    }
  }

  # Clipping threshold
  if (length(dynrange) > 0) {
    clipthres <- dynrange - 0.5
  } else {
    clipthres <- 7.5 # hard-coded assumption that dynamic range is 8g
    if (monc == .RAW_MONITOR[["MOVISENS"]]) {
      clipthres <- 15.5 # hard coded assumption that dynamic range is 16g
      dynrange_source <- "movisens_assumed"
    } else if (monc == .RAW_MONITOR[["AD_HOC"]]) {
      clipthres <- rmc.dynamic_range
      dynrange_source <- "rmc.dynamic_range"
    }
  }
  # Nonwear threshold: non-wear criteria are monitor-specific
  racriter <- nonwear_range_threshold / 1000
  sdcriter <- 0.013
  if (monc == .RAW_MONITOR[["VERISENSE"]]) {
    racriter <- 0.20
  } else if (monc == .RAW_MONITOR[["AD_HOC"]]) {
    if (length(rmc.noise) == 0) {
      stop("Argument rmc.noise not specified, please specify expected noise level in g-units")
    }
    sdcriter <- rmc.noise * 1.2
  }
  invisible(list(clipthres = clipthres, blocksize = blocksize, sdcriter = sdcriter, racriter = racriter,
                 dynrange = if (length(clipthres) > 0) clipthres + 0.5 else NULL,
                 dynrange_source = dynrange_source))
}

# INSPECTION

#' Inspect a Raw Accelerometer File
#'
#' Identifies the brand, file format, sample frequency, header, device serial, participant ID,
#' dynamic range and the block sizes of a raw accelerometer recording exactly as GGIR part 1
#' does before it reads any data. The header is formatted so that it is identical() to the
#' header slot of GGIR's inspection object.
#'
#' @param path Path to one file (.gt3x, .bin, .cwa, .csv, optionally .gz-wrapped). Backslashes
#'   are accepted.
#' @param params A parameter list from raw.params() when that exists; NULL uses GGIR's
#'   defaults. Members read here: desiredtz, configtz, idloc, dynrange, minimumFileSizeMB,
#'   chunksize, nonwear_range_threshold, rmc.* (ad-hoc csv), ggir_exact, rename_uppercase,
#'   skip_small_files.
#' @param ... Individual parameter overrides, e.g. desiredtz = "America/Anchorage".
#'
#' @return An object of class "canhrActi_raw_info": a list with path, filename, size_bytes,
#'   read_path (the path the readers must use: the original, the renamed file, or the temporary
#'   lowercase stand-in of a .GT3X), monc, monn, dformc, dformn, sf, decn, header (GGIR's
#'   one-column data.frame with row names, or "no header", or NULL for a corrupt file),
#'   header_list (the parsed header as a plain list; for gt3x the dates are POSIXct labelled GMT
#'   built from the numeric ticks so they do not depend on the read.gt3x version), header_vars
#'   (GGIR's ID, iID, HN, sensor.location, SX, deviceSerialNumber), id, device_serial,
#'   serial_prefix, firmware, header_timezone, dynrange, dynrange_source, clipthres, sdcriter,
#'   racriter, blocksize_calibrate, blocksize_getmeta, corrupt (sf is NULL), too_small,
#'   uppercase_extension, ggir_accepts_extension, renamed, skipped, messages (GGIR's warning
#'   texts plus canhrActi remarks), tz (desiredtz, configtz) and params (the resolved list).
#'
#' @details
#' Reproduces GGIR's g.inspectfile, g.dotorcomma, g.extractheadervars, extractID and
#' get_nw_clip_block_params together with the g.calibrate block size; the size floor is
#' g.part1's.
#'
#' Differences from GGIR: extensions are matched case-insensitively; a .GT3X file is read
#' through a temporary lowercase stand-in and never renamed unless rename_uppercase = TRUE;
#' a file at or below minimumFileSizeMB is inspected and flagged too_small unless
#' skip_small_files = TRUE, which returns a skipped object carrying GGIR's warning text; an
#' ActiGraph csv whose first line has no "at NN Hz" (an epoch export) raises a clear error
#' where GGIR fails with "missing value where TRUE/FALSE needed". GGIR's warnings are
#' collected into messages rather than raised, and recorded once where GGIR raises them
#' twice; where GGIR stops, this function stops with GGIR's text as a condition of class
#' "canhrActi_raw_inspect_error" that carries the path.
#'
#' desiredtz is forwarded to read.gt3x::parse_gt3x_info (ignored by read.gt3x 1.2.0; shifts
#' the displayed header dates in 1.3.0) and to GGIRread::readAxivity. configtz is not used
#' at this stage. Take timing from header_list, never from the header strings.
#'
#' @examples
#' \dontrun{
#' info <- raw.inspect("subject01.gt3x", desiredtz = "America/Anchorage")
#' info$sf
#' info$header
#' }
#' @export
raw.inspect <- function(path, params = NULL, ...) {
  params <- .raw.inspect.params(params, list(...))
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop("path must be a single file path", call. = FALSE)
  }
  path <- gsub("\\\\", "/", path)
  # a file replaced with the same size and mtime must not reach an old extraction
  .raw.gt3x.forget(path)
  if (!file.exists(path)) .raw.stop(paste0("File not found: ", path), path)
  if (dir.exists(path)) {
    .raw.stop(paste0(path, " is a directory; use raw.discover() to list the files in it"), path)
  }
  if (file.access(path, 4) != 0) .raw.stop(paste0("No read permission for ", path), path)

  filename <- .raw.filename(path)
  size_bytes <- file.size(path)
  minimumFileSizeMB <- params$minimumFileSizeMB
  too_small <- !(size_bytes / 1e6 > minimumFileSizeMB) # GGIR's size floor, negated
  messages <- character()
  collect <- function(expr) {
    withCallingHandlers(expr, warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  }
  monnames <- .RAW_MONITOR_NAMES
  fornames <- .RAW_FORMAT_NAMES
  base <- list(path = path, filename = filename, size_bytes = size_bytes, read_path = path)

  if (too_small && isTRUE(params$skip_small_files)) {
    messages <- c(messages, paste0("\nSkipping files that are too small for analysis: ", toString(filename),
                                   " (configurable with parameter minimumFileSizeMB)."))
    out <- c(base, list(
      monc = NULL, monn = NULL, dformc = NULL, dformn = NULL, sf = NULL, decn = NULL,
      header = NULL, header_list = NULL, header_vars = NULL, id = filename, device_serial = NULL,
      serial_prefix = NA_character_, firmware = NA_character_, header_timezone = NA_character_,
      dynrange = NULL, dynrange_source = NA_character_, clipthres = NULL, sdcriter = NULL,
      racriter = NULL, blocksize_calibrate = NULL, blocksize_getmeta = NULL,
      corrupt = FALSE, too_small = too_small, uppercase_extension = NA,
      ggir_accepts_extension = NA, renamed = FALSE, skipped = TRUE, messages = messages,
      tz = list(desiredtz = params$desiredtz, configtz = params$configtz), params = params))
    return(structure(out, class = "canhrActi_raw_info"))
  }
  own <- .raw.gt3x.stage.owner(path, params)
  if (!is.null(own)) on.exit(try(.raw.gt3x.extract.cleanup(own$path, exdir = own$dir), silent = TRUE), add = TRUE)

  adhoc_header <- NULL
  if (length(params[["rmc.firstrow.acc"]]) == 0) {
    INFI <- collect(.raw.brand(path, filename = filename, desiredtz = params$desiredtz,
                               unzip_once = .raw.param(params, "unzip_once", FALSE),
                               rename_uppercase = isTRUE(params$rename_uppercase)))
    mon <- INFI$mon
    dformat <- INFI$dformat
    sf <- INFI$sf
    datafile <- INFI$datafile
    raw_header <- INFI$raw_header
    messages <- c(messages, INFI$notes)
  } else {
    # ad-hoc csv through the read.myacc.csv port
    dformat <- .RAW_FORMAT[["AD_HOC_CSV"]]
    mon <- .RAW_MONITOR[["AD_HOC"]]
    datafile <- path
    raw_header <- NULL
    INFI <- list(extension = .raw.extension(filename), uppercase_extension = FALSE,
                 ggir_accepts = TRUE, renamed = FALSE, stand_in_method = NA_character_)
    reader <- .raw.read.myacc.csv
    # GGIR passes rmc.headername.sn as the recording id name; reproduced under ggir_exact
    recordingid_name <- if (isTRUE(params$ggir_exact)) params[["rmc.headername.sn"]] else params[["rmc.headername.recordingid"]]
    Pusercsvformat <- collect(reader(rmc.file = datafile,
                                     rmc.nrow = 5,
                                     rmc.dec = params[["rmc.dec"]],
                                     rmc.firstrow.acc = params[["rmc.firstrow.acc"]],
                                     rmc.firstrow.header = params[["rmc.firstrow.header"]],
                                     rmc.header.length = params[["rmc.header.length"]],
                                     rmc.col.acc = params[["rmc.col.acc"]],
                                     rmc.col.temp = params[["rmc.col.temp"]],
                                     rmc.col.time = params[["rmc.col.time"]],
                                     rmc.unit.acc = params[["rmc.unit.acc"]],
                                     rmc.unit.temp = params[["rmc.unit.temp"]],
                                     rmc.unit.time = params[["rmc.unit.time"]],
                                     rmc.format.time = params[["rmc.format.time"]],
                                     rmc.bitrate = params[["rmc.bitrate"]],
                                     rmc.dynamic_range = params[["rmc.dynamic_range"]],
                                     rmc.unsignedbit = params[["rmc.unsignedbit"]],
                                     rmc.origin = params[["rmc.origin"]],
                                     rmc.desiredtz = params[["rmc.desiredtz"]],
                                     rmc.configtz = params[["rmc.configtz"]],
                                     rmc.sf = params[["rmc.sf"]],
                                     rmc.headername.sf = params[["rmc.headername.sf"]],
                                     rmc.headername.sn = params[["rmc.headername.sn"]],
                                     rmc.headername.recordingid = recordingid_name,
                                     rmc.header.structure = params[["rmc.header.structure"]],
                                     rmc.check4timegaps = params[["rmc.check4timegaps"]],
                                     rmc.scalefactor.acc = params[["rmc.scalefactor.acc"]],
                                     desiredtz = params$desiredtz,
                                     configtz = params$configtz))
    if (inherits(Pusercsvformat$header, "character") && Pusercsvformat$header == "no header") {
      sf <- params[["rmc.sf"]]
    } else {
      sf <- as.numeric(Pusercsvformat$header["sample_rate", 1])
    }
    if (is.null(sf) || is.na(sf)) {
      .raw.stop(paste0("\nFile header doesn't specify sample rate. Please provide rmc.sf value to process ", path), path)
    } else if (sf == 0) {
      .raw.stop(paste0("\nFile header doesn't specify sample rate. Please provide a non-zero rmc.sf value to process ", path), path)
    }
    adhoc_header <- Pusercsvformat$header
  }

  if (mon == .RAW_MONITOR[["GENEACTIV"]] && dformat == .RAW_FORMAT[["CSV"]]) {
    .raw.stop(paste0("The GENEActiv csv reading functionality is deprecated in",
                     " GGIR from version 2.6-4 onwards. Please, use either",
                     " the GENEActiv bin files or try to read the csv files with",
                     " GGIR::read.myacc.csv"), path)
  }
  # an ActiGraph csv with no "at NN Hz" in its first line is an epoch export; GGIR fails on the NA
  if (!is.null(sf) && anyNA(sf)) {
    .raw.stop(paste0("File ", filename, " does not look like raw acceleration data: its ActiGraph csv header ",
                     "has no 'at NN Hz' sample rate, which is the signature of an epoch (count) export. ",
                     "The raw pipeline needs the raw acceleration export or the .gt3x file (", path, ")."), path)
  }

  fh <- collect(.raw.format.header(dformat, mon, sf, raw_header = raw_header, datafile = datafile,
                                   filename = filename, label_path = path, adhoc_header = adhoc_header))
  header <- fh$header
  sf <- fh$sf
  mon <- fh$mon
  filename <- fh$filename

  if (!is.null(sf)) {
    # detect dot or comma separator in the data file
    decn <- suppressWarnings(.raw.decimal(datafile, dformat, mon, rmc.dec = params[["rmc.dec"]],
                                          unzip_once = .raw.param(params, "unzip_once", FALSE),
                                          stream_gt3x = .raw.param(params, "stream_gt3x", FALSE)))
  } else {
    decn <- "."
  }
  monc <- mon
  monn <- ifelse(mon > 0, monnames[mon], "unknown")
  dformc <- dformat
  dformn <- fornames[dformat]
  corrupt <- is.null(sf)

  info <- c(base, list(monc = monc, monn = monn, dformc = dformc, dformn = dformn, sf = sf,
                       decn = decn, header = header))
  info$read_path <- datafile
  info$filename <- filename # the parent folder name for Movisens

  # header variables and ID
  if (corrupt) {
    hvars <- list(ID = filename, iID = "not extracted", HN = "not extracted",
                  sensor.location = "not extracted", SX = "not available",
                  deviceSerialNumber = "not extracted")
  } else {
    hvars <- .raw.header.vars(info)
  }
  id <- collect(.raw.extract.id(hvars, idloc = params$idloc, fname = filename))
  device_serial <- hvars$deviceSerialNumber

  # header as a version-independent list
  header_list <- NULL
  if (dformat == .RAW_FORMAT[["GT3X"]] && !is.null(raw_header)) {
    header_list <- unclass(raw_header)
    for (nm in names(header_list)) {
      if (inherits(header_list[[nm]], "POSIXct")) {
        header_list[[nm]] <- .POSIXct(as.numeric(header_list[[nm]]), tz = "GMT")
      }
    }
  } else if (dformat %in% c(.RAW_FORMAT[["BIN"]], .RAW_FORMAT[["CWA"]]) && !is.null(raw_header)) {
    header_list <- as.list(raw_header)
  }

  firmware <- NA_character_
  serial_prefix <- NA_character_
  header_timezone <- NA_character_
  if (monc %in% c(.RAW_MONITOR[["ACTIGRAPH"]], .RAW_MONITOR[["VERISENSE"]])) {
    if (grepl("_firmware_", device_serial, fixed = TRUE)) {
      firmware <- sub("^.*_firmware_", "", device_serial)
    }
    serial_only <- sub("_firmware_.*$", "", device_serial)
    if (!identical(serial_only, "not extracted")) {
      serial_prefix <- substr(trimws(serial_only), 1, 3)
    }
    if (!is.null(header_list) && !is.null(header_list[["TimeZone"]])) {
      header_timezone <- as.character(header_list[["TimeZone"]])
    }
  } else if (monc == .RAW_MONITOR[["GENEACTIV"]] && !is.null(header_list)) {
    if (!is.null(header_list[["firmware"]])) firmware <- as.character(header_list[["firmware"]])
    if (!is.null(header_list[["tzone"]])) header_timezone <- as.character(header_list[["tzone"]])
  } else if (monc == .RAW_MONITOR[["AXIVITY"]] && !is.null(header_list)) {
    if (!is.null(header_list[["firmwareVersion"]])) firmware <- as.character(header_list[["firmwareVersion"]])
  }

  # Parmay dynamic range is read from the file
  dynrange_file <- NULL
  if (monc == .RAW_MONITOR[["PARMAY_MTX"]] && !corrupt) {
    dynrange_file <- GGIRread::readParmayMatrix(datafile, output = "dynrange")
  }

  info$header_list <- header_list
  info$header_vars <- hvars
  info$id <- id
  info$device_serial <- device_serial
  info$serial_prefix <- serial_prefix
  info$firmware <- firmware
  info$header_timezone <- header_timezone
  info$dynrange_file <- dynrange_file

  ncb <- .raw.clip.block.params(info, params = params, pass = "getmeta")
  ncb_cal <- .raw.clip.block.params(info, params = params, pass = "calibrate")
  info$dynrange <- ncb$dynrange
  info$dynrange_source <- ncb$dynrange_source
  info$clipthres <- ncb$clipthres
  info$sdcriter <- ncb$sdcriter
  info$racriter <- ncb$racriter
  info$blocksize_calibrate <- ncb_cal$blocksize
  info$blocksize_getmeta <- ncb$blocksize
  info$corrupt <- corrupt
  info$too_small <- too_small
  info$uppercase_extension <- INFI$uppercase_extension
  info$ggir_accepts_extension <- INFI$ggir_accepts
  info$renamed <- INFI$renamed
  info$stand_in_method <- INFI$stand_in_method
  info$skipped <- FALSE
  if (too_small) {
    messages <- c(messages, paste0("File size ", format(size_bytes, big.mark = ","), " bytes is at or below GGIR's ",
                                   minimumFileSizeMB, " MB floor (minimumFileSizeMB); GGIR would skip it."))
  }
  info$messages <- messages
  info$tz <- list(desiredtz = params$desiredtz, configtz = params$configtz)
  info$params <- params
  structure(info, class = "canhrActi_raw_info")
}

#' GGIR-Shaped Inspection Object
#'
#' The eight fields of GGIR's I object in GGIR's order, so that identical() against
#' GGIR::g.inspectfile() or a stored milestone I holds.
#'
#' @param info A canhrActi_raw_info.
#' @return list(header, monc, monn, dformc, dformn, sf, decn, filename).
#' @keywords internal
#' @noRd
.raw.ggir.I <- function(info) {
  list(header = info$header, monc = info$monc, monn = info$monn,
       dformc = info$dformc, dformn = info$dformn, sf = info$sf, decn = info$decn,
       filename = info$filename)
}

#' Print a Raw File Inspection
#'
#' @param x A canhrActi_raw_info object from raw.inspect().
#' @param ... Ignored.
#' @return x, invisibly.
#' @export
print.canhrActi_raw_info <- function(x, ...) {
  cat("canhrActi raw file inspection\n")
  cat("  file:      ", x$filename, " (", format(x$size_bytes, big.mark = ","), " bytes)\n", sep = "")
  if (isTRUE(x$skipped)) {
    cat("  skipped:    below minimumFileSizeMB\n")
  } else {
    cat("  brand:     ", x$monn, " (", x$dformn, ")\n", sep = "")
    cat("  sf:        ", if (is.null(x$sf)) "NULL (corrupt)" else x$sf, " Hz\n", sep = "")
    cat("  serial:    ", x$device_serial, "\n", sep = "")
    cat("  id:        ", x$id, "\n", sep = "")
    cat("  dynrange:  ", if (is.null(x$dynrange)) "NULL" else x$dynrange, " g (", x$dynrange_source,
        "), clipthres ", if (is.null(x$clipthres)) "NULL" else x$clipthres, "\n", sep = "")
    cat("  blocks:    ", if (is.null(x$blocksize_calibrate)) "NULL" else x$blocksize_calibrate,
        " (calibrate) / ", if (is.null(x$blocksize_getmeta)) "NULL" else x$blocksize_getmeta,
        " (getmeta)\n", sep = "")
    cat("  flags:     corrupt ", x$corrupt, ", too_small ", x$too_small,
        ", uppercase_extension ", x$uppercase_extension, "\n", sep = "")
  }
  if (length(x$messages) > 0) {
    cat("  messages:\n")
    for (m in x$messages) cat("    - ", trimws(m), "\n", sep = "")
  }
  invisible(x)
}

# DISCOVERY

#' Discover Raw Accelerometer Files
#'
#' Lists the files GGIR part 1 would consider under a directory or among explicit paths,
#' with a reason for every file it would not process.
#'
#' @param paths_or_dir One or more directories or file paths.
#' @param params A parameter list from raw.params() when that exists; NULL uses GGIR's
#'   defaults. Members read here: minimumFileSizeMB, skip_small_files.
#' @param ... Individual parameter overrides.
#'
#' @return A data.frame with one row per file found: path, filename (for Movisens the folder
#'   name, as GGIR reports it), size_bytes, recognised, reason, too_small. Recognised rows come
#'   first in GGIR's listing order (csv, bin, wav, cwa, gt3x groups, each in dir() order),
#'   unrecognised files after them. When no recognised row exists the attribute "message" holds
#'   "no accelerometer files found ...".
#'
#' @details
#' Reproduces GGIR's datadir2fnames and the size and read-permission checks of g.part1.
#' GGIR's dir() patterns are replaced by a test on the final extension after one .gz peel,
#' which is their intent and avoids the bin pattern matching names such as
#' "acc.binary.txt". Movisens directories (a unisens.xml next to the data) are listed by
#' their acc.bin files with the folder name as filename, and folders without acc.bin get a
#' row carrying GGIR's warning text. GGIR's re-entry from its own RData milestones has no
#' equivalent because canhrActi keeps no output folder, and a directory is always a
#' directory here (GGIR's isfilelist treats a single path containing ".gt" or ".cs" as a
#' file list). Files at or below minimumFileSizeMB stay recognised with too_small TRUE
#' unless skip_small_files = TRUE, which marks them unrecognised with GGIR's skip text.
#' .wav files are listed but marked unrecognised with GGIR's stop text. Missing paths and
#' unreadable files are reported as rows, never dropped.
#'
#' @examples
#' \dontrun{
#' raw.discover("C:/data/actigraph")
#' }
#' @export
raw.discover <- function(paths_or_dir, params = NULL, ...) {
  params <- .raw.inspect.params(params, list(...))
  minimumFileSizeMB <- params$minimumFileSizeMB
  skip_small <- isTRUE(params$skip_small_files)
  if (!is.character(paths_or_dir) || length(paths_or_dir) == 0) {
    stop("paths_or_dir must be a character vector of directories or file paths", call. = FALSE)
  }
  paths_or_dir <- gsub("\\\\", "/", paths_or_dir)

  classify <- function(full, filename = basename(full), movisens = FALSE) {
    n <- length(full)
    if (n == 0) return(NULL)
    ext <- tolower(.raw.extension(basename(full)))
    exists <- file.exists(full)
    size <- ifelse(exists, file.size(full), NA_real_)
    readable <- exists & (file.access(full, mode = 4) == 0)
    recognised <- exists & readable & (ext %in% .RAW_EXTENSIONS | movisens)
    reason <- ifelse(recognised, paste0("extension .", ext, " is one GGIR reads"), "")
    if (movisens) reason[recognised] <- "movisens acc.bin recording"
    reason[!exists] <- "file does not exist"
    reason[exists & !readable] <- "no read permission"
    reason[exists & readable & !recognised & !movisens] <- paste0(
      "extension .", ext[exists & readable & !recognised & !movisens],
      " is not one GGIR reads (csv, bin, wav, cwa, gt3x)")
    wav <- recognised & ext == "wav"
    if (any(wav)) {
      recognised[wav] <- FALSE
      reason[wav] <- paste0("\nError processing ", filename[wav], ": Axivity .wav file format is no longer supported.\n")
    }
    too_small <- exists & !(size / 1e6 > minimumFileSizeMB)
    if (skip_small) {
      hit <- recognised & too_small
      if (any(hit)) {
        recognised[hit] <- FALSE
        reason[hit] <- paste0("\nSkipping files that are too small for analysis: ", filename[hit],
                              " (configurable with parameter minimumFileSizeMB).")
      }
    } else {
      hit <- recognised & too_small
      reason[hit] <- paste0(reason[hit], "; at or below minimumFileSizeMB (GGIR would skip it)")
    }
    data.frame(path = full, filename = filename, size_bytes = size, recognised = recognised,
               reason = reason, too_small = too_small, stringsAsFactors = FALSE)
  }

  rows <- list()
  for (p in paths_or_dir) {
    if (dir.exists(p)) {
      if (.raw.is.movisens(p)) {
        fnamesfull <- dir(p, recursive = TRUE, pattern = "acc.bin$", full.names = TRUE)
        foldersWithAccBin <- dirname(fnamesfull)
        fnames <- basename(foldersWithAccBin)
        rows[[length(rows) + 1]] <- classify(fnamesfull, filename = fnames, movisens = TRUE)
        allfolders <- list.dirs(p, recursive = FALSE, full.names = TRUE)
        noAccBin <- c()
        for (fld in allfolders) {
          if (fld %in% foldersWithAccBin) next # folder contains acc.bin
          # do any subfolders contain acc.bin?
          if (!any(grepl(paste(fld, '/', sep = ''), foldersWithAccBin))) {
            noAccBin <- c(noAccBin, fld)
          }
        }
        if (length(noAccBin) > 0) {
          rows[[length(rows) + 1]] <- data.frame(
            path = noAccBin, filename = basename(noAccBin), size_bytes = NA_real_,
            recognised = FALSE,
            reason = paste0("The following movisens data folders do not contain the ",
                            "acc.bin file with the accelerometer recording, and ",
                            "therefore cannot be processed in GGIR: ", noAccBin),
            too_small = FALSE, stringsAsFactors = FALSE)
        }
      } else {
        all_files <- dir(p, recursive = TRUE, full.names = TRUE)
        if (length(all_files) == 0) next
        ext <- tolower(.raw.extension(basename(all_files)))
        ordered <- c()
        for (e in .RAW_EXTENSIONS) ordered <- c(ordered, all_files[ext == e])
        ordered <- c(ordered, all_files[!(ext %in% .RAW_EXTENSIONS)])
        rows[[length(rows) + 1]] <- classify(ordered)
      }
    } else {
      rows[[length(rows) + 1]] <- classify(p)
    }
  }
  out <- if (length(rows) == 0) NULL else do.call(rbind, rows)
  if (is.null(out) || nrow(out) == 0) {
    out <- data.frame(path = character(0), filename = character(0), size_bytes = numeric(0),
                      recognised = logical(0), reason = character(0), too_small = logical(0),
                      stringsAsFactors = FALSE)
  }
  rownames(out) <- NULL
  if (!any(out$recognised)) {
    attr(out, "message") <- paste0("no accelerometer files found in ", paste(paths_or_dir, collapse = ", "))
  }
  out
}
