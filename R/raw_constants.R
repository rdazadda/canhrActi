# Ported from GGIR 3.3-9 R/monitor_types.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees et al.; Medical Research Council UK; Accelting; French
# National Research Agency; and others).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. The two code tables are plain named
# integer vectors instead of locked environments; names and numbers are GGIR's. Two
# reverse-lookup helpers were added.

#' Monitor Brand Codes Used by the Raw Accelerometer Pipeline
#'
#' Named integer vector, GGIR's \code{MONITOR} table (\code{I$monc}). GENEA is
#' deprecated in GGIR and kept so the codes line up.
#'
#' @format A named integer vector of length 8 with values 0 to 7.
#' @keywords internal
#' @noRd
.RAW_MONITOR <- setNames(0:7, c("AD_HOC", "GENEA", "GENEACTIV", "ACTIGRAPH", "AXIVITY",  "MOVISENS", "VERISENSE", "PARMAY_MTX"))

#' File Format Codes Used by the Raw Accelerometer Pipeline
#'
#' Named integer vector, GGIR's \code{FORMAT} table (\code{I$dformc}). WAV is
#' deprecated in GGIR and kept so the codes line up.
#'
#' @format A named integer vector of length 6 with values 1 to 6.
#' @keywords internal
#' @noRd
.RAW_FORMAT <- setNames(1:6, c("BIN", "CSV", "WAV", "CWA", "AD_HOC_CSV", "GT3X"))

#' Look Up a Monitor Brand Name From Its Code
#'
#' @param code Integer (or numeric) monitor code(s) as stored in \code{I$monc}.
#' @return Character vector of names, \code{NA_character_} where the code is not in
#'   the table.
#' @keywords internal
#' @noRd
.raw.monitor.name <- function(code) {
  .raw.code.name(code, .RAW_MONITOR)
}

#' Look Up a File Format Name From Its Code
#'
#' @param code Integer (or numeric) format code(s) as stored in \code{I$dformc}.
#' @return Character vector of names, \code{NA_character_} where the code is not in
#'   the table.
#' @keywords internal
#' @noRd
.raw.format.name <- function(code) {
  .raw.code.name(code, .RAW_FORMAT)
}

#' Shared Reverse Lookup for the Code Tables
#'
#' @param code Integer or numeric code(s).
#' @param table Named integer vector (\code{.RAW_MONITOR} or \code{.RAW_FORMAT}).
#' @return Character vector of names, \code{NA_character_} where not found or where
#'   the code is not a whole number.
#' @keywords internal
#' @noRd
.raw.code.name <- function(code, table) {
  if (length(code) == 0) return(character(0))
  if (!is.numeric(code) && !is.logical(code)) {
    stop("code must be numeric", call. = FALSE)
  }
  whole <- !is.na(code) & code == round(code)
  key <- rep(NA_integer_, length(code))
  key[whole] <- as.integer(code[whole])
  names(table)[match(key, table)]
}
