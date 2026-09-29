# Ported from GGIR 3.3-9 R/g.readaccfile.R, R/updateBlocksize.R, R/read.myacc.csv.R and
# R/g.readtemp_movisens.R, with the block size formulas of g.calibrate.R and
# get_nw_clip_block_params.R (https://github.com/wadpac/GGIR).
# Copyright (c) the GGIR authors and contributors, as listed in GGIR's DESCRIPTION
# (Vincent T. van Hees, Jairo H. Migueles and contributors; copyright holders
# Medical Research Council UK, Accelting and the French National Research Agency).
# Licensed under the Apache License, Version 2.0; a copy is at inst/LICENSE.GGIR.
# This file is a MODIFIED version of the original. Changes: the GGIR parameter objects
# became named arguments that default from a canhrActi params list; the return is a flat
# list; GGIR's warnings are collected into a messages vector instead of raised, and a
# read.gt3x error other than end-of-file is recorded there too; options(warn) toggles
# became suppressWarnings; the optional gt3x read paths (unzip once, decode once, and
# the stream reader of raw_gt3x_stream.R) are additions, switched in raw.params().

# STATE AND BLOCK SIZE HELPERS

#' Fresh File Quality Record for a Block Loop
#'
#' The data.frame GGIR's g.getmeta initialises before its first g.readaccfile call and that
#' every call updates. g.calibrate initialises the same frame without NFilePagesSkipped;
#' the extra column is harmless there.
#'
#' @return A one-row data.frame: filetooshort FALSE, filecorrupt FALSE, filedoesnotholdday
#'   FALSE, NFilePagesSkipped 0.
#' @keywords internal
#' @noRd
.raw.filequality <- function() {
  data.frame(filetooshort = FALSE, filecorrupt = FALSE,
             filedoesnotholdday = FALSE, NFilePagesSkipped = 0)
}

#' Block Size for One Pass of the Block Loop
#'
#' The number of pages (gt3x activity records, GENEActiv 300-sample pages, cwa 512-byte
#' blocks, Parmay packets, Movisens samples, or csv rows before the x300 applied inside
#' \code{.raw.read.block}) that one call of \code{.raw.read.block} asks the reader for:
#' GGIR's 24-hour blocks for pass "getmeta" and 12-hour blocks for pass "calibrate". The
#' arithmetic lives in \code{.raw.clip.block.params}.
#'
#' @param info A canhrActi_raw_info from \code{raw.inspect()} (needs monc, dformc and sf).
#' @param pass "getmeta" or "calibrate".
#' @param chunksize GGIR's chunksize (default 1; GGIR floors it at 0.1 in check_params).
#' @return A single number, or NULL when info$sf is NULL (corrupt file).
#' @keywords internal
#' @noRd
.raw.blocksize <- function(info, pass = c("getmeta", "calibrate"), chunksize = 1) {
  pass <- match.arg(pass)
  .raw.clip.block.params(info, params = NULL, pass = pass, chunksize = chunksize)$blocksize
}

#' Shrink the Block Size When R's Memory Use Is High
#'
#' Called by both GGIR block loops after every processed block. Logs the current Vcells use
#' (in MB, from \code{gc()}) with a timestamp and multiplies the block size by 0.8 when the
#' use exceeds 4000 MB and fewer than five rows have been logged. Verbatim from GGIR's
#' updateBlocksize.
#'
#' @param blocksize Current block size.
#' @param bsc_qc The log so far: a data.frame with columns time (character) and size (numeric),
#'   or the zero-row frame \code{data.frame(time = c(), size = c())}.
#' @return list(blocksize, bsc_qc).
#' @keywords internal
#' @noRd
.raw.update.blocksize <- function(blocksize = c(),
                                  bsc_qc = data.frame(time = c(), size = c())) {
  if (length(blocksize) == 0) {
    warning("Blocksize is zero, please contact maintainers")
  }
  gco <- gc()
  memuse <- gco[2, 2] # memuse in mb
  bsc_qc_new_row <- data.frame(
    time = format(Sys.time()),
    size = memuse,
    stringsAsFactors = FALSE
  )
  if (nrow(bsc_qc) == 0) {
    bsc_qc <- bsc_qc_new_row
  } else {
    bsc_qc <- rbind(bsc_qc, bsc_qc_new_row)
  }
  if (memuse > 4000) {
    if (nrow(bsc_qc) < 5) {
      blocksize <- round(blocksize * 0.8)
    }
  }
  blocksize <- round(blocksize)
  return(list(blocksize = blocksize, bsc_qc = bsc_qc))
}

#' Signal a Classed Block-Read Error
#'
#' @param msg Message text (GGIR's text where GGIR stops).
#' @param path The file the error is about.
#' @param block The block number.
#' @keywords internal
#' @noRd
.raw.read.stop <- function(msg, path = NULL, block = NULL) {
  stop(structure(class = c("canhrActi_raw_read_error", "error", "condition"),
                 list(message = msg, call = NULL, path = path, block = block)))
}

# OPTIONAL GT3X READ PATHS

#' Extract a .gt3x Archive Once and Return the Directory
#'
#' read.gt3x unzips the archive into tempdir() on every call and removes the extraction
#' when it returns, but also accepts a directory that already holds info.txt and log.bin.
#' This extracts the archive once into a private cache directory keyed by the file's
#' content hash, reuses an existing complete extraction, and returns the directory. The
#' cache lives under tempdir()/canhrActi_raw/gt3x/<key>/ so it cannot collide with
#' read.gt3x's own tempdir()/<basename> extraction, which read.gt3x deletes on exit.
#' canhrActi addition, no GGIR counterpart; the bytes are the same as read.gt3x's own
#' extraction (see \code{.raw.unzip}).
#'
#' @param filename Path to the .gt3x file (or an already extracted directory, returned as is).
#' @param location Cache root; default tempdir()/canhrActi_raw/gt3x.
#' @return The extraction directory (forward slashes).
#' @keywords internal
#' @noRd
.raw.gt3x.extract <- function(filename, location = NULL) {
  filename <- gsub("\\\\", "/", filename)
  if (dir.exists(filename)) return(filename)
  if (!file.exists(filename)) stop("gt3x file not found: ", filename, call. = FALSE)
  if (is.null(location)) {
    location <- file.path(gsub("\\\\", "/", tempdir()), "canhrActi_raw", "gt3x")
  }
  key <- .raw.gt3x.extract.key(filename)
  exdir <- file.path(location, key)
  # the marker is written only after a complete extraction; info.txt alone lands first
  # and would make an interrupted extraction look reusable
  done <- file.path(exdir, ".canhrActi_extract_complete")
  if (dir.exists(exdir) && file.exists(done) && file.exists(file.path(exdir, "info.txt"))) {
    return(exdir)
  }
  unlink(exdir, recursive = TRUE)   # drop any half-finished attempt
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  # any unzip warning means a damaged archive: the caller then reads the archive itself,
  # so the warning GGIR's run would not raise does not reach the messages
  warned <- FALSE
  extracted <- tryCatch(withCallingHandlers(.raw.unzip(filename, exdir = exdir), warning = function(w) {
    warned <<- TRUE
    invokeRestart("muffleWarning")
  }), error = function(e) character(0))
  if (warned || length(extracted) == 0 || !file.exists(file.path(exdir, "info.txt"))) {
    unlink(exdir, recursive = TRUE)
    stop("Could not extract info.txt from ", filename, call. = FALSE)
  }
  file.create(done)
  exdir
}

#' Extract a zip Archive, Copying Stored Members Straight Out
#'
#' A stored member's bytes are the file, so a copy whose CRC-32 matches the archive's is what
#' utils::unzip writes; any other archive, a failed copy or a CRC mismatch goes to utils::unzip.
#' @return The paths written, as utils::unzip returns them.
#' @keywords internal
#' @noRd
.raw.unzip <- function(zipfile, exdir) {
  mem <- tryCatch(.raw.zip.stored(zipfile), warning = function(w) NULL, error = function(e) NULL)
  if (!is.null(mem)) {
    out <- file.path(exdir, mem$name)
    ok <- TRUE
    for (k in seq_along(out)) {
      crc <- tryCatch(gt3x_copy_stored_cpp(enc2native(zipfile), enc2native(out[k]),
                                           mem$offset[k], mem$size[k]),
                      warning = function(w) -1, error = function(e) -1)
      if (!identical(crc, mem$crc[k])) {
        ok <- FALSE
        break
      }
    }
    if (ok) return(out)
    unlink(out)
  }
  utils::unzip(zipfile, exdir = exdir)
}

#' Members of a zip Archive Whose Bytes Can Be Copied Out, or NULL
#'
#' Name, CRC-32, data offset and size of every member, when the archive is a single-disk,
#' non-zip64 one and every member is stored, unencrypted and plainly named; NULL otherwise.
#' @keywords internal
#' @noRd
.raw.zip.stored <- function(zipfile) {
  fs <- file.size(zipfile)
  if (is.na(fs) || fs < 22) return(NULL)
  con <- file(zipfile, "rb")
  on.exit(close(con))
  u16 <- function(b, i) as.numeric(b[i]) + 256 * as.numeric(b[i + 1])
  u32 <- function(b, i) u16(b, i) + 65536 * u16(b, i + 2)
  is_sig <- function(b, i, s) identical(b[i:(i + 3)], as.raw(c(0x50, 0x4b, s)))
  # the end of central directory record is the last one whose comment ends the file
  nt <- min(fs, 22 + 65535)
  seek(con, fs - nt)
  tb <- readBin(con, "raw", nt)
  if (length(tb) != nt) return(NULL)
  e <- which(tb[1:(nt - 3)] == as.raw(0x50) & tb[2:(nt - 2)] == as.raw(0x4b) &
               tb[3:(nt - 1)] == as.raw(0x05) & tb[4:nt] == as.raw(0x06))
  e <- e[e + 21 <= nt]
  e <- e[e + 21 + u16(tb, e + 20) == nt]
  if (length(e) == 0) return(NULL)
  e <- max(e)
  nent <- u16(tb, e + 10)
  cdsize <- u32(tb, e + 12)
  cdoff <- u32(tb, e + 16)
  # one disk, and the central directory right before this record (no zip64 records between)
  if (u16(tb, e + 4) != 0 || u16(tb, e + 6) != 0 || u16(tb, e + 8) != nent || nent == 0 ||
      cdoff + cdsize != fs - nt + e - 1) return(NULL)
  seek(con, cdoff)
  cd <- readBin(con, "raw", cdsize)
  if (length(cd) != cdsize) return(NULL)
  mem <- data.frame(name = character(nent), crc = numeric(nent), offset = numeric(nent),
                    size = numeric(nent), stringsAsFactors = FALSE)
  p <- 1
  for (k in seq_len(nent)) {
    if (p + 45 > cdsize || !is_sig(cd, p, c(1, 2))) return(NULL)
    flags <- u16(cd, p + 8)
    nl <- u16(cd, p + 28)
    # stored, no encryption bits, 32-bit sizes and offset, first disk
    if (u16(cd, p + 10) != 0 || bitwAnd(flags, 0x2041) != 0 ||
        u32(cd, p + 20) != u32(cd, p + 24) || u32(cd, p + 24) == 4294967295 ||
        u32(cd, p + 42) == 4294967295 || u16(cd, p + 34) != 0 ||
        nl == 0 || p + 45 + nl > cdsize) return(NULL)
    name <- rawToChar(cd[(p + 46):(p + 45 + nl)])
    if (!grepl("^[A-Za-z0-9_][A-Za-z0-9._-]*$", name)) return(NULL)
    loff <- u32(cd, p + 42)
    seek(con, loff)
    lh <- readBin(con, "raw", 30 + nl)
    if (length(lh) != 30 + nl || !is_sig(lh, 1, c(3, 4)) || u16(lh, 9) != 0 ||
        u16(lh, 27) != nl || !identical(rawToChar(lh[31:(30 + nl)]), name)) return(NULL)
    mem$name[k] <- name
    mem$crc[k] <- u32(cd, p + 16)
    mem$offset[k] <- loff + 30 + nl + u16(lh, 29)
    mem$size[k] <- u32(cd, p + 24)
    if (mem$offset[k] + mem$size[k] > cdoff) return(NULL)
    p <- p + 46 + nl + u16(cd, p + 30) + u16(cd, p + 32)
  }
  if (p != cdsize + 1 || anyDuplicated(tolower(mem$name))) return(NULL)
  mem
}

#' Cache Key of a gt3x Extraction
#'
#' The sanitised base name plus the file's md5 hash. A name, size and mtime key is not an
#' identity (mtime has one-second resolution and the key carries no directory), so two
#' participants' files could share an extraction; a content hash cannot. md5sum costs
#' about a second on 400 MB, and every block looks the extraction up, so the hash is kept
#' for the session under the full path, size and mtime until the extraction is removed.
#' @keywords internal
#' @noRd
.raw.gt3x.extract.key <- function(filename) {
  fi <- suppressWarnings(file.info(filename, extra_cols = FALSE))
  memo <- paste(filename, fi$size, format(as.numeric(fi$mtime), digits = 17), sep = "|")
  h <- .raw.gt3x.key.env[[memo]]
  if (is.null(h)) {
    h <- tryCatch(unname(tools::md5sum(filename)), error = function(e) NA_character_)
    if (length(h) != 1 || is.na(h)) {
      # no hash means no identity; refuse rather than risk serving the wrong recording
      stop("Could not hash ", filename, " to key its extraction", call. = FALSE)
    }
    .raw.gt3x.key.env[[memo]] <- h
  }
  paste0(gsub("[^A-Za-z0-9_.-]", "_", tools::file_path_sans_ext(basename(filename))), "_", h)
}

# Per-session md5 hashes of .gt3x files, keyed by path, size and mtime.
.raw.gt3x.key.env <- new.env(parent = emptyenv())

#' Remove the Cached Extraction of a .gt3x File
#'
#' @param filename The .gt3x path given to \code{.raw.gt3x.extract}; NULL removes the whole
#'   cache root.
#' @param location Cache root as in \code{.raw.gt3x.extract}.
#' @param exdir The extraction directory when already known, so the file is not hashed again.
#' @return TRUE invisibly.
#' @keywords internal
#' @noRd
.raw.gt3x.extract.cleanup <- function(filename = NULL, location = NULL, exdir = NULL) {
  if (is.null(location)) {
    location <- file.path(gsub("\\\\", "/", tempdir()), "canhrActi_raw", "gt3x")
  }
  target <- if (!is.null(exdir)) exdir else if (is.null(filename)) location else file.path(location, .raw.gt3x.extract.key(gsub("\\\\", "/", filename)))
  # the decoded store and the stream index live inside the extraction; drop their entries too
  for (env in list(.raw.gt3x.store.env, .raw.gt3x.index.env)) {
    for (k in ls(env, all.names = TRUE)) {
      if (startsWith(gsub("\\\\", "/", k), gsub("\\\\", "/", target))) rm(list = k, envir = env)
    }
  }
  memo <- ls(.raw.gt3x.key.env, all.names = TRUE)
  if (is.null(filename) && is.null(exdir)) {
    rm(list = memo, envir = .raw.gt3x.key.env)
  } else if (!is.null(filename)) {
    .raw.gt3x.forget(filename)
  }
  unlink(target, recursive = TRUE)
  invisible(TRUE)
}

#' Drop the Cached Hash of a .gt3x Path, So the Next Read Hashes the File Again
#' @keywords internal
#' @noRd
.raw.gt3x.forget <- function(filename) {
  memo <- ls(.raw.gt3x.key.env, all.names = TRUE)
  rm(list = memo[startsWith(memo, paste0(gsub("\\\\", "/", filename), "|"))], envir = .raw.gt3x.key.env)
  invisible(TRUE)
}

# read.raw.accelerometer runs in progress; while one runs, its stages share one extraction.
.raw.gt3x.hold.env <- new.env(parent = emptyenv())
.raw.gt3x.hold.env$depth <- 0L

#' The Extraction a Stage Called on Its Own Must Remove When It Ends, or NULL
#'
#' NULL inside read.raw.accelerometer, for a file the gt3x read paths do not extract, and
#' when a complete extraction was already there (it belongs to whoever made it).
#' @param path The path the stage reads.
#' @param params The stage's parameters (unzip_once, stream_gt3x).
#' @return list(path, dir) or NULL.
#' @keywords internal
#' @noRd
.raw.gt3x.stage.owner <- function(path, params) {
  if (.raw.gt3x.hold.env$depth > 0) return(NULL)
  if (!is.character(path) || length(path) != 1 || is.na(path)) return(NULL)
  if (!isTRUE(.raw.param(params, "unzip_once", FALSE)) && !isTRUE(.raw.param(params, "stream_gt3x", FALSE))) {
    return(NULL)
  }
  path <- gsub("\\\\", "/", path)
  if (!grepl("\\.gt3x$", path, ignore.case = TRUE) || !file.exists(path) || dir.exists(path)) return(NULL)
  location <- file.path(gsub("\\\\", "/", tempdir()), "canhrActi_raw", "gt3x")
  dir <- tryCatch(file.path(location, .raw.gt3x.extract.key(path)), error = function(e) NULL)
  if (is.null(dir) || file.exists(file.path(dir, ".canhrActi_extract_complete"))) return(NULL)
  list(path = path, dir = dir)
}

# Blocks read with stream_gt3x on since the last reset, and how many the stream reader served.
.raw.gt3x.tally.env <- new.env(parent = emptyenv())

#' Reset or Advance the Stream Reader's Block Count
#' @keywords internal
#' @noRd
.raw.gt3x.tally <- function(direct = NULL) {
  if (is.null(direct)) {
    .raw.gt3x.tally.env$n <- 0L
    .raw.gt3x.tally.env$served <- 0L
  } else if (!is.null(.raw.gt3x.tally.env$n)) {
    .raw.gt3x.tally.env$n <- .raw.gt3x.tally.env$n + 1L
    .raw.gt3x.tally.env$served <- .raw.gt3x.tally.env$served + as.integer(isTRUE(direct))
  }
  invisible(NULL)
}

#' The Path a gt3x Reader Should Be Handed
#'
#' The cached extraction when unzip_once is on and it can be made, otherwise the archive.
#' Used by the block reader and by the two reads raw.inspect makes, so all three go to the
#' same place; read.gt3x on an archive path unzips the whole file for every call,
#' including parse_gt3x_info.
#' @keywords internal
#' @noRd
.raw.gt3x.read.path <- function(filename, unzip_once = FALSE) {
  if (!isTRUE(unzip_once)) return(filename)
  if (dir.exists(filename)) return(filename)
  if (!grepl("\\.gt3x$", filename, ignore.case = TRUE)) return(filename)
  ex <- tryCatch(.raw.gt3x.extract(filename), error = function(e) NULL)
  if (!is.null(ex) && file.exists(file.path(ex, "log.bin")) && file.exists(file.path(ex, "info.txt"))) ex else filename
}

# DECODE ONCE, SLICE BY RECORD RANK

# Every read.gt3x call allocates the whole file's worth of samples and scans log.bin from
# byte 0. This decodes the recording once, in 24 h batches, into four files of doubles
# beside the extraction (time, X, Y, Z) and records where each device-second record
# starts; a block is then a row range read with seek + readBin and built the way
# read.gt3x's as.data.frame.activity builds it. read.gt3x counts one data-bearing
# activity record per device second, and sample i of a record carries time index
# (sec + i/sf) * 100, so a new record begins where the whole second changes or the
# sub-second offset stops increasing (the second test guards a duplicated second).

# Per-process registry of decoded stores, keyed by the extraction directory.
.raw.gt3x.store.env <- new.env(parent = emptyenv())

#' Record Boundaries of One Decoded Matrix, as Row Indices
#' @keywords internal
#' @noRd
.raw.gt3x.records <- function(ti) {
  n <- length(ti)
  if (n == 0) return(list(rstart = integer(0), rend = integer(0)))
  sec <- floor(ti / 100)
  sub <- ti - sec * 100
  newrec <- c(TRUE, sec[-1] != sec[-n] | sub[-1] <= sub[-n])
  rstart <- which(newrec)
  list(rstart = rstart, rend = c(rstart[-1] - 1L, n))
}

#' Build the Decoded Store for an Extraction, or Return the One Already Built
#'
#' Decodes in batches of 86,400 records (one getmeta block) appended to four column files;
#' read.gt3x batches are record-aligned, so concatenating them reproduces the whole-file
#' read row for row. Returns NULL, never a partial store, when anything fails or an
#' invariant does not hold, so the caller falls back to the directory batch read.
#' @keywords internal
#' @noRd
.raw.gt3x.decode.store <- function(exdir) {
  key <- gsub("\\\\", "/", exdir)
  w <- .raw.gt3x.store.env[[key]]
  if (!is.null(w) && all(file.exists(w$files))) return(w)
  cols <- c("time", "X", "Y", "Z")
  files <- stats::setNames(file.path(exdir, paste0("decoded_", cols, ".dbl")), cols)
  parts <- paste0(files, ".part")
  ok <- FALSE
  on.exit(if (!ok) unlink(c(parts, files)), add = TRUE)
  sf <- tryCatch(as.numeric(read.gt3x::parse_gt3x_info(exdir)[["Sample Rate"]]),
                 error = function(e) NA_real_)
  if (!is.finite(sf) || sf <= 0) return(NULL)
  cons <- lapply(parts, function(p) file(p, "wb"))
  on.exit(for (cn in cons) try(close(cn), silent = TRUE), add = TRUE)
  batch <- 86400L
  page <- 1L; n <- 0L; nrec <- 0L
  rstart <- list(); rend <- list()
  repeat {
    m <- tryCatch(read.gt3x::read.gt3x(path = exdir, batch_begin = page,
                                        batch_end = page + batch - 1L,
                                        asDataFrame = FALSE, verbose = FALSE),
                  error = function(e) e)
    if (inherits(m, "error")) {
      # Past the end of the file is how the loop ends; anything else is a fault.
      if (grepl("upper value must be greater than lower value", conditionMessage(m), fixed = TRUE)) break
      return(NULL)
    }
    if (!is.matrix(m) || nrow(m) == 0) break
    ti <- attr(m, "time_index")
    st <- as.numeric(attr(m, "start_time"))
    if (is.null(ti) || length(st) != 1 || is.na(st)) return(NULL)
    rec <- .raw.gt3x.records(ti)
    if (length(rec$rstart) == 0) return(NULL)
    # Invariant: no record may hold more than one second of samples.
    if (any(rec$rend - rec$rstart + 1L > sf)) return(NULL)
    rstart[[length(rstart) + 1L]] <- rec$rstart + n
    rend[[length(rend) + 1L]] <- rec$rend + n
    writeBin(st + ti / 100, cons[[1]])          # read.gt3x's own time expression
    writeBin(as.double(m[, "X"]), cons[[2]])
    writeBin(as.double(m[, "Y"]), cons[[3]])
    writeBin(as.double(m[, "Z"]), cons[[4]])
    n <- n + nrow(m)
    nrec <- nrec + length(rec$rstart)
    rm(m, ti); gc(FALSE)
    if (length(rec$rstart) < batch) break       # a short batch is the last one
    page <- page + batch
  }
  for (cn in cons) close(cn)
  cons <- list()
  if (n == 0L || nrec == 0L) return(NULL)
  rstart <- unlist(rstart, use.names = FALSE); rend <- unlist(rend, use.names = FALSE)
  if (length(rstart) != nrec || length(rend) != nrec) return(NULL)
  if (!all(file.rename(parts, files))) return(NULL)
  ok <- TRUE
  w <- list(files = files, rstart = rstart, rend = rend, nrec = nrec, n = n)
  .raw.gt3x.store.env[[key]] <- w
  w
}

#' One Block From the Decoded Store, in read.gt3x's asDataFrame = TRUE Shape
#' @keywords internal
#' @noRd
.raw.gt3x.slice <- function(w, startpage, endpage) {
  if (startpage > w$nrec) {
    # .raw.read.block keys the end-of-file rule on this exact text (read.gt3x's
    # own Rcpp Range error on zero records), so raise it, not an empty frame.
    stop("upper value must be greater than lower value", call. = FALSE)
  }
  r1 <- w$rstart[startpage]; r2 <- w$rend[min(endpage, w$nrec)]
  len <- r2 - r1 + 1L
  col <- function(nm) {
    con <- file(w$files[[nm]], "rb"); on.exit(close(con))
    seek(con, (r1 - 1) * 8)
    readBin(con, "double", n = len)
  }
  # Built as read.gt3x's as.data.frame.activity builds it, with structure() rather than
  # class<- so row.names keep the c(NA, +n) form; as.matrix() and serialize() see the sign.
  x <- as.data.frame(cbind(X = col("X"), Y = col("Y"), Z = col("Z")))
  x$time <- col("time")
  x <- x[, c("time", setdiff(colnames(x), "time"))]
  x$time <- as.POSIXct(x$time, origin = "1970-01-01", tz = "GMT")
  structure(x, class = c("activity_df", "data.frame"))
}

#' Read a gt3x Batch as GGIR Does, or Through the Optional Paths
#'
#' GGIR's call is \code{read.gt3x::read.gt3x(path = filename, batch_begin = startpage,
#' batch_end = endpage, asDataFrame = TRUE)}. With \code{unzip_once} the path is the cached
#' extraction directory; with \code{decode_once} the block is a slice of the decoded store;
#' with \code{stream_gt3x} the block is read from the extraction's log.bin by
#' \code{.raw.gt3x.stream.batch}, which falls back to read.gt3x on anything it declines;
#' with \code{as_data_frame_false} the batch is read as the activity matrix and the
#' data.frame is built here as read.gt3x's as.data.frame.activity builds it. All paths
#' return the same block.
#'
#' @param filename gt3x path or extracted directory.
#' @param startpage,endpage batch_begin and batch_end (data-bearing activity records, 1-based,
#'   both inclusive).
#' @param unzip_once,as_data_frame_false,decode_once,stream_gt3x The optional paths.
#'   stream_gt3x implies unzip_once.
#' @return The data.frame read.gt3x would return with asDataFrame = TRUE (time POSIXct GMT,
#'   X, Y, Z), or an error.
#' @keywords internal
#' @noRd
.raw.gt3x.read.batch <- function(filename, startpage, endpage, unzip_once = FALSE,
                                 as_data_frame_false = FALSE, decode_once = FALSE,
                                 stream_gt3x = FALSE) {
  if (isTRUE(stream_gt3x)) unzip_once <- TRUE
  # a .gt3x.gz is gzip around the archive, not a ZIP, so it stays on GGIR's own path
  if (unzip_once && !grepl("\\.gt3x$", filename, ignore.case = TRUE)) unzip_once <- FALSE
  # Fall back to the archive when the extraction cannot be made or has no log.bin, so a
  # broken recording or a full tempdir gets GGIR's own diagnosis rather than "too short".
  path <- filename
  if (unzip_once) {
    ex <- tryCatch(.raw.gt3x.extract(filename), error = function(e) NULL)
    if (!is.null(ex) && file.exists(file.path(ex, "log.bin"))) path <- ex
  }
  # a block counts once it has data; count() is a no-op unless the reader is on
  count <- function(direct) if (isTRUE(stream_gt3x)) .raw.gt3x.tally(direct)
  # the decoded store only when it could be built and passed its invariants
  if (isTRUE(decode_once) && !identical(path, filename)) {
    w <- tryCatch(.raw.gt3x.decode.store(path), error = function(e) NULL)
    if (!is.null(w)) {
      x <- .raw.gt3x.slice(w, startpage, endpage)
      count(FALSE)
      return(x)
    }
  }
  if (isTRUE(stream_gt3x) && !as_data_frame_false && dir.exists(path)) {
    x <- tryCatch(.raw.gt3x.stream.batch(path, startpage, endpage), error = function(e) NULL)
    if (identical(x, "eof")) .raw.gt3x.eof()
    if (is.data.frame(x)) {
      count(TRUE)
      return(x)
    }
  }
  if (!as_data_frame_false) {
    x <- read.gt3x::read.gt3x(path = path, batch_begin = startpage,
                              batch_end = endpage, asDataFrame = TRUE)
    count(FALSE)
    return(x)
  }
  accdata <- read.gt3x::read.gt3x(path = path, batch_begin = startpage,
                                  batch_end = endpage, asDataFrame = FALSE)
  count(FALSE)
  time_index <- attr(accdata, "time_index")
  start_time <- as.numeric(attr(accdata, "start_time"))
  divider <- 100L
  m <- accdata
  attributes(m) <- list(dim = dim(accdata), dimnames = dimnames(accdata))
  x <- as.data.frame(m)
  x$time <- start_time + time_index / divider
  x <- x[, c("time", setdiff(colnames(x), "time"))]
  x$time <- as.POSIXct(x$time, origin = "1970-01-01", tz = "GMT")
  class(x) <- c("activity_df", "data.frame")
  x
}

# MOVISENS TEMPERATURE

#' Movisens Temperature Resampled to the Acceleration Block Length
#'
#' Verbatim from GGIR's g.readtemp_movisens: temp.bin is read over the sample range that
#' corresponds to the acceleration range at an assumed 1 Hz, re-read if the file's
#' sampleRate attribute differs, then stretched onto \code{seq(1, n, length.out =
#' acc_length)} with \code{GGIRread::resample} (index based, no real timestamps).
#'
#' @param datafile Path of the acc.bin file (the unisens folder is its dirname).
#' @param from,to First and last acceleration sample index of the block.
#' @param acc_sf Acceleration sample frequency.
#' @param acc_length Number of acceleration rows in the block.
#' @param interpolationType 1 linear, 2 nearest neighbour.
#' @return A one-column numeric matrix of length acc_length, invisibly.
#' @keywords internal
#' @noRd
.raw.readtemp.movisens <- function(datafile, from = c(), to = c(), acc_sf, acc_length, interpolationType = 1) {
  .raw.require("unisensR", "Movisens files")
  .raw.require("GGIRread", "Movisens temperature")
  # temperature is sampled at a different rate, so resample it to the acceleration rate

  temp_sf <- 1 # assumed 1 Hz, checked below

  temp_from <- ceiling(from / acc_sf * temp_sf)
  temp_to <- ceiling(to / acc_sf * temp_sf)

  temperature <- unisensR::readUnisensSignalEntry(dirname(datafile), "temp.bin",
                                                  startIndex = temp_from, endIndex = temp_to)
  new_temp_sf <- attr(temperature, "sampleRate")

  # re-read at the file's own rate if the guess was wrong
  if (temp_sf != new_temp_sf) {
    temp_from <- ceiling(from / acc_sf * new_temp_sf)
    temp_to <- ceiling(to / acc_sf * new_temp_sf)

    temperature <- unisensR::readUnisensSignalEntry(dirname(datafile), "temp.bin",
                                                    startIndex = temp_from, endIndex = temp_to)
  }

  temperature <- temperature$temp

  # index-based time: the timestamps are discarded
  rawTime <- seq_len(length(temperature))
  timeRes <- seq(from = 1, to = rawTime[length(rawTime)], length.out = acc_length)

  temperature <- GGIRread::resample(as.matrix(temperature), rawTime, timeRes, length(temperature), type = interpolationType)

  invisible(temperature)
}

# AD-HOC CSV

#' Read a Block of an Ad-Hoc csv Accelerometer File
#'
#' GGIR's reader for csv files of any layout, driven by the rmc.* arguments: header block
#' parsing (sample rate, serial number, recording id), column selection, timestamp conversion
#' in five conventions, unit conversion (mg, bit, scale factor, Kelvin, Fahrenheit), the wear
#' column, optional gap imputation and optional resampling.
#'
#' @details Transcribed from GGIR's read.myacc.csv; the gap imputation goes to
#'   \code{.raw.impute.timegaps} and the header fread is wrapped in suppressWarnings. GGIR's
#'   Kelvin conversion adds 272.15 and is kept. The vignette convention rmc.firstrow.acc = 2
#'   with a non-numeric first column loses one data row on the first block; that skip is
#'   applied by the caller, not here.
#'
#' @param rmc.file Path.
#' @param rmc.nrow,rmc.skip Rows to read and rows to skip before the first data row (on top of
#'   rmc.firstrow.acc - 1).
#' @param rmc.dec Decimal separator.
#' @param rmc.firstrow.acc,rmc.firstrow.header,rmc.header.length Row numbers (1-based) of the
#'   first acceleration row and of the header block, and the header length.
#' @param rmc.col.acc,rmc.col.temp,rmc.col.time,rmc.col.wear Column indices.
#' @param rmc.unit.acc "g", "mg" or "bit"; rmc.unit.temp "C", "F" or "K"; rmc.unit.time
#'   "POSIX", "character", "UNIXsec", "UNIXmsec" or "ActivPAL".
#' @param rmc.format.time strptime format for POSIX and character timestamps.
#' @param rmc.bitrate,rmc.dynamic_range,rmc.unsignedbit Bit conversion.
#' @param rmc.origin Origin for UNIX timestamps.
#' @param rmc.desiredtz,rmc.configtz Deprecated aliases of desiredtz and configtz.
#' @param rmc.sf Sample frequency when the header has none.
#' @param rmc.headername.sf,rmc.headername.sn,rmc.headername.recordingid Header row names.
#' @param rmc.header.structure Separator when name and value share one column.
#' @param rmc.check4timegaps Run gap imputation on the block.
#' @param rmc.doresample Resample onto a 1/sf grid.
#' @param rmc.scalefactor.acc Multiplier applied to the acceleration.
#' @param interpolationType 1 linear, 2 nearest neighbour.
#' @param PreviousLastValue,PreviousLastTime State carried from the previous block (used only
#'   with rmc.check4timegaps).
#' @param desiredtz,configtz Timezones.
#' @param header A header already parsed for this file, or NULL to parse it.
#' @return list(data, header, PreviousLastValue, PreviousLastTime) as GGIR returns it.
#' @keywords internal
#' @noRd
.raw.read.myacc.csv <- function(rmc.file = c(), rmc.nrow = Inf, rmc.skip = c(), rmc.dec = ".",
                                rmc.firstrow.acc = c(), rmc.firstrow.header = c(),
                                rmc.header.length = c(),
                                rmc.col.acc = 1:3, rmc.col.temp = c(), rmc.col.time = c(),
                                rmc.unit.acc = "g", rmc.unit.temp = "C",
                                rmc.unit.time = "POSIX",
                                rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                                rmc.bitrate = c(), rmc.dynamic_range = c(),
                                rmc.unsignedbit = TRUE,
                                rmc.origin = "1970-01-01",
                                rmc.desiredtz = NULL,
                                rmc.configtz = NULL,
                                rmc.sf = c(),
                                rmc.headername.sf = c(),
                                rmc.headername.sn = c(),
                                rmc.headername.recordingid = c(),
                                rmc.header.structure = c(),
                                rmc.check4timegaps = FALSE,
                                rmc.col.wear = c(),
                                rmc.doresample = FALSE,
                                rmc.scalefactor.acc = 1,
                                interpolationType = 1,
                                PreviousLastValue = c(0, 0, 1),
                                PreviousLastTime = NULL,
                                desiredtz = NULL,
                                configtz = NULL,
                                header = NULL) {
  .raw.require("data.table", "csv files")

  if (length(rmc.col.time) > 0 && !(rmc.unit.time %in% c("POSIX", "character", "UNIXsec", "UNIXmsec", "ActivPAL"))) {
    stop(paste0("\nUnrecognized rmc.col.time value. The only accepted values are \"POSIX\", ",
                "\"character\", \"UNIXsec\", \"UNIXmsec\", and \"ActivPAL\"."), call. = FALSE)
  }

  if (!is.null(rmc.desiredtz) || !is.null(rmc.configtz)) {
    generalWarning <- paste0("Argument rmc.desiredtz and rmc.configtz are scheduled to be deprecated",
                             " and will be replaced by the existing arguments desiredtz and configtz, respectively.")

    # Check if both types of tz are provided:
    if (!is.null(desiredtz) && desiredtz != "" && !is.null(rmc.desiredtz)) {
      if (rmc.desiredtz != desiredtz) { # if different --> error (don't know which one to use)
        stop(paste0("\n", generalWarning, "Please, specify only desiredtz and set ",
                    "rmc.desiredtz to NULL to ensure it is no longer used."))
      }
    }
    if (!is.null(configtz) && !is.null(rmc.configtz)) { # then both provided
      if (rmc.configtz != configtz) { # if different --> error (don't know which one to use)
        stop(paste0("\n", generalWarning, "Please, specify only configtz and set ",
                    "rmc.configtz to NULL to ensure it is no longer used."))
      }
    }
    warning(paste0("\n", generalWarning))

    # until deprecation the rmc. values overwrite the normal tz
    if (is.null(desiredtz)) desiredtz <- rmc.desiredtz
    if (desiredtz == "" && !is.null(rmc.desiredtz)) desiredtz <- rmc.desiredtz
    if (is.null(configtz)) configtz <- rmc.configtz

  }
  # check if none of desiredtz and rmc.desiredtz are provided
  if (is.null(desiredtz) && is.null(rmc.desiredtz)) {
    stop(paste0("Timezone not specified, please provide at least desiredtz",
                " and consider specifying configtz."))
  }

  if (is.null(rmc.firstrow.acc) || rmc.firstrow.acc < 1) {
    stop(paste0("\nParameter rmc.firstrow.acc always need to be specified ",
                "when working with ad-hoc csv format data"))
  }
  skip <- rmc.firstrow.acc - 1
  if (!is.null(rmc.skip) && length(rmc.skip) > 0) {
    skip <- skip + rmc.skip
  }

  # only extract the header if it hasn't been extracted for this file before
  if (is.null(header)) {
    # bitrate is either a header item name or the numeric bit rate
    if (length(rmc.firstrow.header) == 0) { # no header block
      sf <- rmc.sf
      header <- "no header"
    } else {
      # extract header information:
      if (length(rmc.header.length) == 0) {
        rmc.header.length <- rmc.firstrow.acc - 1
      }

      # fread complains about quote in first row for some file types
      header_tmp <- suppressWarnings(data.table::fread(file = rmc.file,
                                                       nrows = rmc.header.length,
                                                       skip = rmc.firstrow.header - 1,
                                                       dec = rmc.dec, showProgress = FALSE, header = FALSE,
                                                       blank.lines.skip = TRUE,
                                                       data.table = FALSE, stringsAsFactors = FALSE))
      validrows <- which(is.na(header_tmp[, 1]) == FALSE & header_tmp[, 1] != "")
      header_tmp <- header_tmp[validrows, 1:2]

      if (length(rmc.header.structure) != 0) { # header is stored in 1 column, with strings that need to be split
        if (length(header_tmp) == 1) { # one header item
          header_tmp <- as.matrix(unlist(strsplit(as.character(header_tmp[, 1]), rmc.header.structure)))
        } else { # multiple header items
          mysplit <- function(x) {
            tmp <- strsplit(as.character(x), rmc.header.structure)
            tmp <- unlist(tmp)
            return(tmp)
          }
          header_tmp0 <- header_tmp
          header_tmp <- unlist(lapply(header_tmp[, 1], FUN = mysplit))
          if (length(header_tmp) > 2) {
            header_tmp <- data.frame(matrix(unlist(header_tmp), nrow = nrow(header_tmp0), byrow = T), stringsAsFactors = FALSE)
            colnames(header_tmp) <- NULL
          } else {
            header_tmp <- data.frame(matrix(unlist(header_tmp), nrow = 1, byrow = T), stringsAsFactors = FALSE)
            colnames(header_tmp) <- NULL
          }
        }
        if (ncol(header_tmp) == 1) header_tmp <- t(header_tmp)
        header_tmp2 <- as.data.frame(as.character(unlist(header_tmp[, 2])), stringsAsFactors = FALSE)
        row.names(header_tmp2) <- header_tmp[, 1]
        colnames(header_tmp2) <- NULL
        header <- header_tmp2
      } else { # column 1 is header name, column 2 is header value
        colnames(header_tmp) <- NULL
        header_tmp2 <- as.data.frame(header_tmp[, 2], stringsAsFactors = FALSE)
        row.names(header_tmp2) <- header_tmp[, 1]
        colnames(header_tmp2) <- NULL
        header <- header_tmp2
      }
      # assess whether accelerometer data conversion is needed
      if (length(rmc.bitrate) > 0 && length(rmc.dynamic_range) > 0 && rmc.unit.acc == "bit") {
        if (is.character(rmc.bitrate[1]) == TRUE) { # extract bitrate if it is in the header
          rmc.bitrate <- as.numeric(header[which(row.names(header) == rmc.bitrate[1]), 1])
        }
        if (is.character(rmc.dynamic_range[1]) == TRUE) { # extract dynamic range if it is in the header
          rmc.dynamic_range <- as.numeric(header[which(row.names(header) == rmc.dynamic_range[1]), 1])
        }
      }
      # extract sample frequency:
      sf <- as.numeric(header[which(row.names(header) == rmc.headername.sf[1]), 1])

      if (is.na(sf)) { # sf not retrieved from header
        # maybe sf is in the header under the default name
        sf <- as.numeric(header[which(row.names(header) == "sample_rate"), 1])
        if (is.na(sf)) {
          sf <- rmc.sf # may be NULL
          if (!is.null(sf)) {
            header <- rbind(header, sf) # also add it to the header
            row.names(header)[nrow(header)] <- "sample_rate"
          }
        }
      }

      # standardise key header names to ease use elsewhere in GGIR:
      if (length(rmc.headername.sf) > 0) {
        row.names(header)[which(row.names(header) == rmc.headername.sf[1])] <- "sample_rate"
      }
      if (length(rmc.headername.sn) > 0) {
        row.names(header)[which(row.names(header) == rmc.headername.sn[1])] <- "device_serial_number"
      }
      if (length(rmc.headername.recordingid) > 0) {
        row.names(header)[which(row.names(header) == rmc.headername.recordingid[1])] <- "recordingID"
      }
    }
  }
  # read data from file
  P <- data.table::fread(rmc.file, nrows = rmc.nrow, skip = skip,
                         dec = rmc.dec, showProgress = FALSE, header = "auto",
                         data.table = FALSE, stringsAsFactors = FALSE)

  if (length(configtz) == 0) {
    configtz <- desiredtz
  }
  if (length(rmc.col.wear) > 0) {
    wearIndicator <- P[, rmc.col.wear] # keep wear channel seperately and reinsert at the end
  }
  # select relevant columns, add standard column names
  P <- P[, c(rmc.col.time, rmc.col.acc, rmc.col.temp)]
  if (length(rmc.col.time) > 0 && length(rmc.col.temp) > 0) {
    colnames(P) <- c("time", "x", "y", "z", "temperature")
  } else if (length(rmc.col.time) > 0 && length(rmc.col.temp) == 0) {
    colnames(P) <- c("time", "x", "y", "z")
  } else if (length(rmc.col.time) == 0 && length(rmc.col.temp) > 0) {
    colnames(P) <- c("x", "y", "z", "temperature")
  } else if (length(rmc.col.time) == 0 && length(rmc.col.temp) == 0) {
    colnames(P) <- c("x", "y", "z")
  }
  # acceleration and temperature as numeric
  P$x <- as.numeric(P$x)
  P$y <- as.numeric(P$y)
  P$z <- as.numeric(P$z)
  if (length(rmc.col.temp) > 0) P$temperature <- as.numeric(P$temperature)
  # Convert timestamps
  if (length(rmc.col.time) > 0) {
    if (rmc.unit.time == "POSIX") {
      P$time <- as.POSIXct(format(P$time), origin = rmc.origin, tz = configtz, format = rmc.format.time)
      checkdec <- function(x) {
        return(length(unlist(strsplit(as.character(x), "[.]|[,]"))) == 1)
      }
      first_chunk_time <- P$time[1:pmin(nrow(P), 1000)]
      checkMissingDecPlaces <- unlist(lapply(first_chunk_time, FUN = checkdec))
      if (all(checkMissingDecPlaces) &&
          !is.null(sf) && sf != 0 &&
          length(which(duplicated(first_chunk_time) == TRUE)) > 0) {
        # no decimal places and duplicated timestamps, so synthesise sub-second offsets
        trans <- unique(c(1, which(diff(P$time) > 0), nrow(P)))
        sf_tmp <- diff(trans)
        timeIncrement <- seq(from = 0, length.out = sf, by = 1/sf) # expected time increment per second

        # All seconds with exactly the sample frequency
        trans_1 <- trans[which(sf_tmp == sf)]
        indices_1 <- sort(unlist(lapply(trans_1, FUN = function(x) {x + (1:sf)})))
        P$time[indices_1] <- P$time[indices_1] + rep(timeIncrement, length(trans_1))
        # First second
        if (sf_tmp[1] != sf) {
          indices_2 <- 1:trans[2]
          P$time[indices_2] <- P$time[indices_2] + seq(1 - (trans[2]/sf), 1 - 1/sf, by = 1/sf)
        }
        # Last second
        if (sf_tmp[length(sf_tmp)] != sf) {
          indices_3 <- (trans[length(trans) - 1] + 1):trans[length(trans)]
          P$time[indices_3] <- P$time[indices_3] + timeIncrement[1:length(indices_3)]
        }
        # Other seconds: assume most samples were taken at the correct rate and a few were
        # dropped or doubled, rather than that the whole second ran at another rate
        if (length(trans) > 4) {
          trans_cut <- trans[2:(length(trans) - 1)]
          sf_tmp_cut <- sf_tmp[2:(length(sf_tmp) - 1)]
          sf_tmp_odd <- unique(sf_tmp_cut[which(sf_tmp_cut != sf)])
          if (length(sf_tmp_odd) > 0) {
            for (ji in 1:length(sf_tmp_odd)) {
              sf2 <- sf_tmp_odd[ji]
              trans_4 <- trans_cut[which(sf_tmp_cut == sf2)]
              indices_4 <- sort(unlist(lapply(trans_4, FUN = function(x) {x + (1:sf2)})))
              if (length(timeIncrement) > sf2) {
                timeIncrement2 <- timeIncrement[1:sf2]
              } else if (length(timeIncrement) < sf2) {
                timeIncrement2 <- c(timeIncrement, rep(timeIncrement[sf], sf2 - sf))
              }
              P$time[indices_4] <- P$time[indices_4] + rep(timeIncrement2, length(trans_4))
            }
          }
        }
      }
    } else if (rmc.unit.time == "character") {
      P$time <- as.POSIXct(P$time, format = rmc.format.time, origin = rmc.origin, tz = configtz)
    } else if (rmc.unit.time == "UNIXsec" || rmc.unit.time == "UNIXmsec") {
      if (rmc.unit.time == "UNIXmsec") {
        P$time <- P$time / 1000
      }
      if (rmc.origin != "1970-01-01") {
        P$time <- as.POSIXct(P$time, origin = rmc.origin, tz = desiredtz)
      }
    } else if (rmc.unit.time == "ActivPAL") {
      # origin should be specified as: "1899-12-30"
      P$time <- lubridate::force_tz(as.POSIXct(P$time * 86400, origin = "1899-12-30", tz = "UTC"), tz = desiredtz)
    }
    if (length(which(is.na(P$time) == FALSE)) == 0) {
      stop("\nExtraction of timestamps unsuccesful, check timestamp format arguments")
    }
    if (!is.numeric(P$time)) { # we'll return Unix timestamps
      P$time <- as.numeric(P$time)
    }
  }

  # If acceleration is stored in mg units then convert to gravitational units
  if (rmc.unit.acc == "mg") {
    P$x <- P$x / 1000
    P$y <- P$y / 1000
    P$z <- P$z / 1000
  }
  if (rmc.scalefactor.acc != 1) {
    P$x <- P$x * rmc.scalefactor.acc
    P$y <- P$y * rmc.scalefactor.acc
    P$z <- P$z * rmc.scalefactor.acc
  }
  # If acceleration is stored in bit values then convert to gravitational unit
  if (length(rmc.bitrate) > 0 && length(rmc.dynamic_range) > 0 && rmc.unit.acc == "bit") {
    if (rmc.unsignedbit == TRUE) {
      P$x <- ((P$x / (2^rmc.bitrate)) - 0.5) * 2 * rmc.dynamic_range
      P$y <- ((P$y / (2^rmc.bitrate)) - 0.5) * 2 * rmc.dynamic_range
      P$z <- ((P$z / (2^rmc.bitrate)) - 0.5) * 2 * rmc.dynamic_range
    } else if (rmc.unsignedbit == FALSE) { # signed bit
      P$x <- (P$x / ((2^rmc.bitrate)/2)) * rmc.dynamic_range
      P$y <- (P$y / ((2^rmc.bitrate)/2)) * rmc.dynamic_range
      P$z <- (P$z / ((2^rmc.bitrate)/2)) * rmc.dynamic_range
    }
  }
  # Convert temperature units
  if (rmc.unit.temp == "K") {
    P$temperature <- P$temperature + 272.15 # From Kelvin to Celsius
  } else if (rmc.unit.temp == "F") {
    P$temperature <- (P$temperature - 32) * (5/9) # From Fahrenheit to Celsius
  }
  if (length(rmc.col.wear) > 0) { # reinsert the nonwear channel
    P$wear <- wearIndicator
  }
  # check for jumps in time and impute
  if (rmc.check4timegaps == TRUE && ("time" %in% colnames(P))) {
    sfBackup <- sf
    if (is.null(sf) || sf == 0) { # estimate sample frequency if not given in header
      deltatime <- abs(diff(as.numeric(P$time)))
      gapsi <- which(deltatime > 0.25)
      sf <- (P$time[gapsi[1]] - P$time[1]) / (gapsi[1] - 1)
    }
    P <- .raw.impute.timegaps(P, sf = sf, k = 0.25,
                              previous_last_value = PreviousLastValue,
                              previous_last_time = PreviousLastTime, epochsize = NULL)
    sf <- sfBackup
    P <- P$x
    PreviousLastValue <- P[nrow(P), c("x", "y", "z")]
    PreviousLastTime <- as.POSIXct(P[nrow(P), "time"], origin = "1970-01-01")
  }
  if (rmc.doresample == TRUE && ("time" %in% colnames(P)) && !is.null(sf) && sf != 0) { # resample
    .raw.require("GGIRread", "resampling (rmc.doresample)")
    rawTime <- P$time
    rawAccel <- as.matrix(P[, -c(which(colnames(P) == "time"))])
    timeRes <- seq(from = rawTime[1], to = rawTime[length(rawTime)], by = 1/sf)
    accelRes <- GGIRread::resample(rawAccel, rawTime, timeRes, nrow(rawAccel), interpolationType) # this is now the resampled acceleration data
    colnamesP <- colnames(P)[-which(colnames(P) == "time")]
    P <- as.data.frame(accelRes, stringsAsFactors = FALSE)
    colnames(P) <- colnamesP
    P$time <- timeRes
  }
  return(list(data = P, header = header,
              PreviousLastValue = PreviousLastValue,
              PreviousLastTime = PreviousLastTime))
}

#' lubridate::force_tz Once per Whole Second of a Block
#'
#' timechange returns the shifted whole second plus t - floor(t), so shifting each second once
#' gives the same numbers; NA or a UTC offset that changes within the block goes to force_tz.
#' @keywords internal
#' @noRd
.raw.force.tz <- function(time, tz) {
  v <- unclass(time)
  attributes(v) <- NULL
  n <- length(v)
  if (!inherits(time, "POSIXct") || !all(names(attributes(time)) %in% c("class", "tzone")) ||
      n < 2L || !is.double(v) || anyNA(v)) {
    return(lubridate::force_tz(time, tz))
  }
  s <- floor(v)
  first <- which(c(TRUE, s[-1L] != s[-n]))
  u <- s[first]
  fu <- as.numeric(lubridate::force_tz(.POSIXct(u, tz = attr(time, "tzone")), tz))
  off <- fu - u
  if (anyNA(off) || any(off != off[1L])) return(lubridate::force_tz(time, tz))
  out <- rep.int(fu, diff(c(first, n + 1L))) + (v - s)
  attributes(out) <- attributes(lubridate::force_tz(time[1L], tz))
  out
}

# ONE BLOCK OF RAW DATA

#' Read One Block of a Raw Accelerometer File as GGIR's Block Loops Do
#'
#' One call of GGIR's g.readaccfile: computes the page range of block \code{blocknumber}
#' in the brand's page convention, calls the brand's reader (read.gt3x for .gt3x, GGIRread
#' for GENEActiv .bin, Axivity .cwa and Parmay .BIN, data.table::fread for ActiGraph and
#' Axivity csv, unisensR for Movisens, \code{.raw.read.myacc.csv} for ad-hoc csv), applies
#' the reader-specific fixes, decides whether this is the last block, and normalises the
#' columns to x, y, z, time, light, temperature, wear with numeric time. Both GGIR loops
#' (g.calibrate with 12-hour blocks, g.getmeta with 24-hour blocks) call it once per block.
#'
#' @details
#' Page conventions: inclusive for GENEActiv .bin, gt3x, Movisens and Parmay (startpage =
#' previous end page + 1); exclusive for cwa and every csv (startpage = previous end page,
#' plus the 10-line ActiGraph csv header on block 1). gt3x batches count data-bearing
#' activity records, so an 86400-record block spans however much wall clock idle sleep
#' stretched it over. The time column is numeric Unix seconds after force_tz to configtz
#' (configtz defaults to desiredtz, and "" is the machine timezone); acceleration is in g.
#' The first block must hold at least sf * ws * 2 + 1 rows or the file is too short. gt3x,
#' cwa and the csv formats make one extra empty read at the end; only GENEActiv, Axivity
#' csv, Movisens and Parmay set the last-block flag from the reader. Parmay's reader
#' ignores endpage and returns the whole file with lastchunk TRUE on the first call.
#'
#' Deviations from GGIR: the two warnings ("File empty, possibly corrupt." and the Axivity
#' csv slow-timestamp warning) are recorded in \code{messages} rather than raised; a
#' read.gt3x error other than "upper value must be greater than lower value" (the end of
#' the file, silent as in GGIR) is recorded there too, as are errors of the other readers
#' inside their try(); GGIR's stops keep their text and carry the path and block number as
#' a condition of class "canhrActi_raw_read_error"; the rmc.headername.recordingid slip
#' (GGIR passes rmc.headername.sn) is reproduced under ggir_exact = TRUE; the reader's
#' header is returned in \code{header}, but GGIR's loops always pass NULL back and a reused
#' Axivity header changes readAxivity's first-block behaviour, so keep header = NULL for
#' GGIR's numbers. The Axivity csv branch initialises rawData before its try() so a fread
#' failure yields an empty block rather than an unbound variable.
#'
#' @param info A canhrActi_raw_info from \code{raw.inspect()}, or any list with monc, dformc,
#'   sf, decn and read_path (or path).
#' @param blocksize Pages per block, from \code{.raw.blocksize(info, pass)} (or
#'   info$blocksize_getmeta / info$blocksize_calibrate). The csv formats are multiplied by 300
#'   inside, as in GGIR.
#' @param blocknumber 1-based block counter (values below 1 are treated as 1).
#' @param previous_end_page endpage returned by the previous call; \code{c()} for the first.
#' @param ws The long window in seconds (\code{windowsizes[3]}) that sets the 2-hour floor.
#' @param params Optional params list (\code{raw.params()} or info$params); the explicit
#'   arguments below default from it.
#' @param previous_last_value,previous_last_time State for the ad-hoc csv gap imputation;
#'   the g.getmeta initial values are c(0, 0, 1) and NULL.
#' @param filequality The running file quality record (see \code{.raw.filequality}).
#' @param header Header from the previous block for readAxivity and the ad-hoc reader. GGIR
#'   always passes NULL; keep NULL for GGIR-exact numbers.
#' @param filename Path the reader opens; default info$read_path, then info$path. For gt3x a
#'   directory holding info.txt and log.bin is accepted too (read.gt3x reads it directly).
#' @param desiredtz,configtz Timezones (GGIR's params_general members).
#' @param interpolationType,frequency_tol GGIR's params_rawdata members for GGIRread.
#' @param rmc.dec,rmc.firstrow.acc,rmc.firstrow.header,rmc.header.length,rmc.col.acc,rmc.col.temp,rmc.col.time,rmc.unit.acc,rmc.unit.temp,rmc.unit.time,rmc.format.time,rmc.bitrate,rmc.dynamic_range,rmc.unsignedbit,rmc.origin,rmc.desiredtz,rmc.configtz,rmc.sf,rmc.headername.sf,rmc.headername.sn,rmc.headername.recordingid,rmc.header.structure,rmc.check4timegaps,rmc.col.wear,rmc.doresample,rmc.scalefactor.acc
#'   GGIR's ad-hoc csv parameters (see \code{.raw.read.myacc.csv}).
#' @param ggir_exact TRUE reproduces GGIR's rmc.headername.recordingid slip.
#' @param unzip_once,as_data_frame_false,decode_once,stream_gt3x The optional gt3x read
#'   paths (see \code{.raw.gt3x.read.batch}); without params all four are FALSE.
#'
#' @return A list: data (data.frame with the surviving columns among x, y, z, time, light,
#'   temperature, wear, or NULL when the block is empty or discarded), qclog (the reader's
#'   QClog for cwa and Parmay, else NULL; gt3x gap logging happens later in
#'   \code{.raw.impute.timegaps}), filequality (updated), is_last_block, endpage, startpage,
#'   previous_last_value and previous_last_time (the reader's values when it returned them,
#'   else the inputs), header (the reader's header or NULL), messages (character), P (GGIR's
#'   P list exactly as g.readaccfile leaves it, for parity tests), blocknumber and filename.
#' @keywords internal
#' @noRd
.raw.read.block <- function(info, blocksize, blocknumber, previous_end_page = c(), ws = 3600,
                            params = NULL,
                            previous_last_value = c(0, 0, 1), previous_last_time = NULL,
                            filequality = .raw.filequality(), header = NULL,
                            filename = NULL,
                            desiredtz = .raw.param(params, "desiredtz", ""),
                            configtz = .raw.param(params, "configtz", NULL),
                            interpolationType = .raw.param(params, "interpolationType", 1),
                            frequency_tol = .raw.param(params, "frequency_tol", 0.1),
                            rmc.dec = .raw.param(params, "rmc.dec", "."),
                            rmc.firstrow.acc = .raw.param(params, "rmc.firstrow.acc", NULL),
                            rmc.firstrow.header = .raw.param(params, "rmc.firstrow.header", NULL),
                            rmc.header.length = .raw.param(params, "rmc.header.length", NULL),
                            rmc.col.acc = .raw.param(params, "rmc.col.acc", 1:3),
                            rmc.col.temp = .raw.param(params, "rmc.col.temp", NULL),
                            rmc.col.time = .raw.param(params, "rmc.col.time", NULL),
                            rmc.unit.acc = .raw.param(params, "rmc.unit.acc", "g"),
                            rmc.unit.temp = .raw.param(params, "rmc.unit.temp", "C"),
                            rmc.unit.time = .raw.param(params, "rmc.unit.time", "POSIX"),
                            rmc.format.time = .raw.param(params, "rmc.format.time", "%Y-%m-%d %H:%M:%OS"),
                            rmc.bitrate = .raw.param(params, "rmc.bitrate", NULL),
                            rmc.dynamic_range = .raw.param(params, "rmc.dynamic_range", NULL),
                            rmc.unsignedbit = .raw.param(params, "rmc.unsignedbit", TRUE),
                            rmc.origin = .raw.param(params, "rmc.origin", "1970-01-01"),
                            rmc.desiredtz = .raw.param(params, "rmc.desiredtz", NULL),
                            rmc.configtz = .raw.param(params, "rmc.configtz", NULL),
                            rmc.sf = .raw.param(params, "rmc.sf", NULL),
                            rmc.headername.sf = .raw.param(params, "rmc.headername.sf", NULL),
                            rmc.headername.sn = .raw.param(params, "rmc.headername.sn", NULL),
                            rmc.headername.recordingid = .raw.param(params, "rmc.headername.recordingid", NULL),
                            rmc.header.structure = .raw.param(params, "rmc.header.structure", NULL),
                            rmc.check4timegaps = .raw.param(params, "rmc.check4timegaps", FALSE),
                            rmc.col.wear = .raw.param(params, "rmc.col.wear", NULL),
                            rmc.doresample = .raw.param(params, "rmc.doresample", FALSE),
                            rmc.scalefactor.acc = .raw.param(params, "rmc.scalefactor.acc", 1),
                            ggir_exact = .raw.param(params, "ggir_exact", TRUE),
                            unzip_once = .raw.param(params, "unzip_once", FALSE),
                            as_data_frame_false = FALSE,
                            decode_once = .raw.param(params, "decode_once", FALSE),
                            stream_gt3x = .raw.param(params, "stream_gt3x", FALSE)) {
  messages <- character()
  if (is.null(filename)) {
    filename <- if (!is.null(info$read_path)) info$read_path else info$path
  }
  if (is.null(filename)) filename <- info$filename
  if (is.null(filename) || !is.character(filename) || length(filename) != 1) {
    stop("filename could not be determined from info; pass filename = explicitly", call. = FALSE)
  }
  filename <- gsub("\\\\", "/", filename)
  if (length(configtz) == 0) configtz <- desiredtz

  I <- info
  mon <- I$monc
  if (is.null(mon) || is.null(I$dformc)) {
    stop("info must carry monc and dformc (see raw.inspect())", call. = FALSE)
  }
  if (mon == .RAW_MONITOR[["VERISENSE"]]) mon <- .RAW_MONITOR[["ACTIGRAPH"]]
  dformat <- I$dformc
  sf <- I$sf
  decn <- I$decn
  if (is.null(sf) && !(mon == .RAW_MONITOR[["AD_HOC"]] && dformat == .RAW_FORMAT[["AD_HOC_CSV"]])) {
    # GGIR returns before the loop when inspection found no sample frequency
    .raw.read.stop(paste0("info$sf is NULL (the file could not be inspected, GGIR treats it as corrupt): ",
                          filename), filename, blocknumber)
  }

  if ((mon == .RAW_MONITOR[["ACTIGRAPH"]] && dformat == .RAW_FORMAT[["CSV"]]) ||
      (mon == .RAW_MONITOR[["AXIVITY"]] && dformat == .RAW_FORMAT[["CSV"]]) ||
      dformat == .RAW_FORMAT[["AD_HOC_CSV"]]) {
    blocksize <- blocksize * 300
  }

  if (blocknumber < 1) blocknumber <- 1

  # after block 1 the startpage derives from the previous endpage and the blocksize
  PreviousEndPage <- previous_end_page

  if ((mon == .RAW_MONITOR[["GENEACTIV"]] && dformat == .RAW_FORMAT[["BIN"]]) || dformat == .RAW_FORMAT[["GT3X"]] ||
      (mon == .RAW_MONITOR[["MOVISENS"]] && dformat == .RAW_FORMAT[["BIN"]]) ||
      (mon == .RAW_MONITOR[["PARMAY_MTX"]] && dformat == .RAW_FORMAT[["BIN"]])) {
    # page selection includes the end page for these formats
    if (blocknumber > 1 && length(PreviousEndPage) != 0) {
      startpage <- PreviousEndPage + 1
    } else {
      startpage <- blocksize * (blocknumber - 1) + 1 # pages are numbered starting with page 1
    }
    endpage <- startpage + blocksize - 1 # both pages are read, so -1 gives blocksize pages
  } else {
    # the other formats exclude the end page, so a block starts at the previous end page
    if (blocknumber > 1 && length(PreviousEndPage) != 0) {
      startpage <- PreviousEndPage
    } else {
      startpage <- blocksize * (blocknumber - 1) # pages are numbered starting with page 0

      if (mon == .RAW_MONITOR[["ACTIGRAPH"]] && dformat == .RAW_FORMAT[["CSV"]]) {
        headerlength <- 10
        startpage <- startpage + headerlength
      }
    }
    endpage <- startpage + blocksize
  }

  P <- c()
  isLastBlock <- FALSE
  PreviousLastValue <- previous_last_value
  PreviousLastTime <- previous_last_time
  # record the error text of a reader call that GGIR's try(silent = TRUE) would swallow
  note_error <- function(x, what) {
    if (inherits(x, "try-error")) {
      messages <<- c(messages, paste0(what, " failed on block ", blocknumber, " (pages ",
                                      startpage, " to ", endpage, "): ",
                                      conditionMessage(attr(x, "condition"))))
    }
    invisible(x)
  }

  if (mon == .RAW_MONITOR[["GENEACTIV"]] && dformat == .RAW_FORMAT[["BIN"]]) {
    .raw.require("GGIRread", "GENEActiv .bin files")
    note_error(try(expr = {P <- GGIRread::readGENEActiv(filename = filename, start = startpage,
                                                        end = endpage, desiredtz = desiredtz,
                                                        configtz = configtz)}, silent = TRUE),
               "GGIRread::readGENEActiv")
    if (length(P) > 0 && ("data.out" %in% names(P))) {
      names(P)[names(P) == "data.out"] <- "data"

      # a fractional GENEActiv rate is resampled to an integer rate
      if (P$header$SampleRate != round(P$header$SampleRate)) {
        coln_original <- colnames(P$data)
        rawData <- P$data[, 2:ncol(P$data)]
        rawTime <- P$data$time
        P$header$SampleRate <- round(P$header$SampleRate)
        if (sf != P$header$SampleRate) .raw.read.stop("sampling rate inconsistency, please contact maintainer", filename, blocknumber)
        rawAccel <- as.matrix(rawData)
        step <- 1/sf
        timeRes <- seq(rawTime[1], rawTime[length(rawTime)], step)
        timeRes <- timeRes[1 : (length(timeRes) - 1)]
        accelRes <- GGIRread::resample(rawAccel, rawTime, timeRes, nrow(rawAccel), interpolationType) # this is now the resampled acceleration data
        P$data <- data.frame(timeRes, accelRes)
        colnames(P$data) <- coln_original
      }
      if (nrow(P$data) < (blocksize*300)) {
        isLastBlock <- TRUE
      }
    }
  } else if (mon == .RAW_MONITOR[["ACTIGRAPH"]] && dformat == .RAW_FORMAT[["CSV"]]) {
    .raw.require("data.table", "csv files")
    # rows 11:13 show whether the file has a header; fread's complaints about the format are silenced
    quiet <- function(x) {
      # from https://stackoverflow.com/a/54136863/5311763
      sink(tempfile())
      on.exit(sink())
      invisible(force(x))
    }

    # skip 1 more row only if the file has a header. Only the first chunk of data can have a header.
    if (blocknumber == 1) {
      testheader <- quiet(data.table::fread(filename, nrows = 2, skip = 10,
                                            dec = decn, showProgress = FALSE,
                                            header = TRUE, data.table = FALSE, stringsAsFactors = FALSE))
      if (suppressWarnings(is.na(as.numeric(colnames(testheader)[1])))) { # first value is *not* a number, so file starts with a header
        startpage <- startpage + 1
        endpage <- endpage + 1
      }
    }

    note_error(try(expr = {
      P$data <- quiet(data.table::fread(filename, nrows = blocksize, skip = startpage,
                                        dec = decn, showProgress = FALSE,
                                        header = FALSE, # so chunks 2 and on are not mistaken for a header
                                        data.table = FALSE, stringsAsFactors = FALSE))
    }, silent = TRUE), "data.table::fread")
    if (length(P$data) > 0) {
      if (ncol(P$data) < 3) {
        P$data <- c()
      } else {
        if (ncol(P$data) > 3) {
          P$data <- P$data[, 2:4] # remove timestamp column, keep only XYZ columns
        }
        colnames(P$data) <- c("x", "y", "z")
      }
    }
  } else if (mon == .RAW_MONITOR[["AXIVITY"]] && dformat == .RAW_FORMAT[["CWA"]]) {
    .raw.require("GGIRread", "Axivity .cwa files")
    if (utils::packageVersion("GGIRread") < "0.3.0") {
      # ignore frequency_tol parameter
      apply_readAxivity <- function(bstart, bend) {
        note_error(try(expr = {P <- GGIRread::readAxivity(filename = filename, start = bstart, end = bend,
                                                          progressBar = FALSE,
                                                          desiredtz = desiredtz,
                                                          configtz = configtz,
                                                          interpolationType = interpolationType,
                                                          header = header)
        }, silent = TRUE), "GGIRread::readAxivity")
        return(P)
      }
    } else {
      # pass on frequency_tol parameter to GGIRread::readAxivity function
      apply_readAxivity <- function(bstart, bend) {
        note_error(try(expr = {P <- GGIRread::readAxivity(filename = filename, start = bstart, end = bend,
                                                          progressBar = FALSE,
                                                          desiredtz = desiredtz,
                                                          configtz = configtz,
                                                          interpolationType = interpolationType,
                                                          frequency_tol = frequency_tol,
                                                          header = header)
        }, silent = TRUE), "GGIRread::readAxivity")
        return(P)
      }
    }

    P <- apply_readAxivity(bstart = startpage, bend = endpage)
    if (length(P) == 0) {
      # if the read failed, look for a bad first page and skip past it
      PtestLastPage <- PtestStartPage <- NULL
      PtestLastPage <- apply_readAxivity(bstart = endpage, bend = endpage)
      if (length(PtestLastPage) > 1) {
        # Last page exist, so there must be something wrong with the first page
        NFilePagesSkipped <- 0
        while (length(PtestStartPage) == 0) { # Try loading the first page of the block by iteratively skipping a page
          NFilePagesSkipped <- NFilePagesSkipped + 1
          startpage <- startpage + NFilePagesSkipped
          PtestStartPage <- apply_readAxivity(bstart = startpage, bend = startpage)
          if (NFilePagesSkipped == 10 & length(PtestStartPage) == 0) PtestStartPage <- FALSE # stop after 10 attempts
        }
      }
      if (length(PtestStartPage) > 1) {
        # retry the whole block from the first good page
        P <- apply_readAxivity(bstart = startpage, bend = endpage)
        if (length(P) > 1) { # data reading succesful
          filequality$NFilePagesSkipped <- NFilePagesSkipped # store number of pages jumped

          # pad the front with copies of the test page so the block keeps its length
          P$data <- rbind(do.call("rbind",
                                  replicate(NFilePagesSkipped, PtestStartPage$data, simplify = FALSE)),
                          P$data)
        }
      }
    }
    if ("temp" %in% colnames(P$data)) {
      colnames(P$data)[colnames(P$data) == "temp"] <- "temperature"
    }
  } else if (mon == .RAW_MONITOR[["AXIVITY"]] && dformat == .RAW_FORMAT[["CSV"]]) {
    .raw.require("data.table", "csv files")
    .raw.require("GGIRread", "Axivity csv resampling")
    rawData <- c()
    note_error(try(expr = {
      rawData <- data.table::fread(filename, nrows = blocksize,
                                   skip = startpage,
                                   dec = decn, showProgress = FALSE, header = FALSE,
                                   data.table = FALSE, stringsAsFactors = FALSE)
    }, silent = TRUE), "data.table::fread")
    if (length(rawData) > 0) {
      if (nrow(rawData) < blocksize) {
        isLastBlock <- TRUE
      }

      rawTime <- rawData[, 1]

      if (class(rawTime)[1] == "character") {
        # fread returns badly formed timestamps (such as a :60 second) as strings;
        # as.POSIXct is slower but more forgiving
        rawTime <- as.POSIXct(rawTime, tz = configtz, origin = "1970-01-01")

        if (class(rawTime)[1] != "POSIXct") {
          .raw.read.stop(paste0("Corrupt timestamp data in ", filename), filename, blocknumber)
        } else {
          messages <- c(messages, paste0("Corrupt timestamp data in ", filename,
                                         ". This will greatly slow down processing. To avoid this, use the original .cwa file, ",
                                         "or export your data with Unix timestamps instead."))
        }
      } else {
        # fread assumes the machine timezone for formatted timestamps; force configtz instead
        if (!is.numeric(rawTime) && configtz != "") {
          rawTime <- lubridate::force_tz(rawTime, configtz)
        }

        # OmGui writes Unix timestamps as if the device time were UTC; force them into configtz
        if (is.numeric(rawTime)) {
          rawTime <- as.POSIXct(rawTime, tz = "UTC", origin = "1970-01-01")
          rawTime <- lubridate::force_tz(rawTime, configtz)
        }
      }

      rawTime <- as.numeric(rawTime)

      # resample the acceleration data, because AX3 data is stored at irregular time points
      rawAccel <- as.matrix(rawData[, 2:4])
      step <- 1/sf
      timeRes <- seq(rawTime[1], rawTime[length(rawTime)], step)
      timeRes <- timeRes[1 : (length(timeRes) - 1)]

      accelRes <- GGIRread::resample(rawAccel, rawTime, timeRes, nrow(rawAccel), interpolationType) # this is now the resampled acceleration data
      P$data <- data.frame(timeRes, accelRes)
      colnames(P$data) <- c("time", "x", "y", "z")
    }
  } else if (mon == .RAW_MONITOR[["MOVISENS"]] && dformat == .RAW_FORMAT[["BIN"]]) {
    .raw.require("unisensR", "Movisens files")
    file_length <- unisensR::getUnisensSignalSampleCount(dirname(filename), "acc.bin")
    if (endpage > file_length) {
      endpage <- file_length
      isLastBlock <- TRUE
    }
    P$data <- unisensR::readUnisensSignalEntry(dirname(filename), "acc.bin",
                                               startIndex = startpage,
                                               endIndex = endpage)
    if (length(P$data) > 0) {
      if (ncol(P$data) < 3) {
        P$data <- c()
      } else {
        colnames(P$data) <- c("x", "y", "z")
        # there may or may not be a temp.bin file containing temperature
        try(expr = {P$data$temperature <- .raw.readtemp.movisens(filename,
                                                                 from = startpage, to = endpage,
                                                                 acc_sf = sf, acc_length = nrow(P$data),
                                                                 interpolationType = interpolationType)
        }, silent = TRUE)
      }
    }
  } else if (mon == .RAW_MONITOR[["ACTIGRAPH"]] && dformat == .RAW_FORMAT[["GT3X"]]) {
    .raw.require("read.gt3x", ".gt3x files")
    P$data <- try(expr = {.raw.gt3x.read.batch(filename, startpage, endpage,
                                               unzip_once = unzip_once,
                                               as_data_frame_false = as_data_frame_false,
                                               decode_once = decode_once,
                                               stream_gt3x = stream_gt3x)}, silent = TRUE)
    if (inherits(P$data, "try-error")) {
      # A batch past the end of the file ends the gt3x loop: read.gt3x fails with "upper value
      # must be greater than lower value" and GGIR's try() swallows it. Other errors are recorded.
      emsg <- conditionMessage(attr(P$data, "condition"))
      if (!grepl("upper value must be greater than lower value", emsg, fixed = TRUE)) {
        messages <- c(messages, paste0("read.gt3x::read.gt3x failed on block ", blocknumber,
                                       " (records ", startpage, " to ", endpage, "): ", emsg))
      }
    }
    if (length(P$data) == 0 || inherits(P$data, "try-error") == TRUE) { # too short or no data at all
      P$data <- c()
    } else { # If data passes these checks then it is usefull
      colnames(P$data)[colnames(P$data) == "X"] <- "x"
      colnames(P$data)[colnames(P$data) == "Y"] <- "y"
      colnames(P$data)[colnames(P$data) == "Z"] <- "z"

      # read.gt3x labels the device's local time as GMT; force configtz, keeping the clock time
      P$data$time <- .raw.force.tz(P$data$time, configtz)
    }
  } else if (mon == .RAW_MONITOR[["AD_HOC"]] && dformat == .RAW_FORMAT[["AD_HOC_CSV"]]) { # user-specified csv format
    .raw.require("data.table", "csv files")
    # skip one more row on block 1 if rmc.firstrow.acc points at a column-name row
    if (blocknumber == 1) {
      testheader <- data.table::fread(filename, nrows = 2, skip = rmc.firstrow.acc - 1,
                                      dec = decn, showProgress = FALSE,
                                      header = TRUE, data.table = FALSE, stringsAsFactors = FALSE)
      if (suppressWarnings(is.na(as.numeric(colnames(testheader)[1])))) { # first value is *not* a number, so file starts with a header
        startpage <- startpage + 1
        endpage <- endpage + 1
      }
    }
    # GGIR passes rmc.headername.sn as the recording id name; reproduced under ggir_exact
    recordingid_name <- if (isTRUE(ggir_exact)) rmc.headername.sn else rmc.headername.recordingid

    note_error(try(expr = {P <- .raw.read.myacc.csv(rmc.file = filename,
                                                    rmc.nrow = blocksize, rmc.skip = startpage,
                                                    rmc.dec = rmc.dec,
                                                    rmc.firstrow.acc = rmc.firstrow.acc,
                                                    rmc.firstrow.header = rmc.firstrow.header,
                                                    rmc.header.length = rmc.header.length,
                                                    rmc.col.acc = rmc.col.acc,
                                                    rmc.col.temp = rmc.col.temp,
                                                    rmc.col.time = rmc.col.time,
                                                    rmc.unit.acc = rmc.unit.acc,
                                                    rmc.unit.temp = rmc.unit.temp,
                                                    rmc.unit.time = rmc.unit.time,
                                                    rmc.format.time = rmc.format.time,
                                                    rmc.bitrate = rmc.bitrate,
                                                    rmc.dynamic_range = rmc.dynamic_range,
                                                    rmc.unsignedbit = rmc.unsignedbit,
                                                    rmc.origin = rmc.origin,
                                                    rmc.desiredtz = rmc.desiredtz,
                                                    rmc.configtz = rmc.configtz,
                                                    rmc.sf = rmc.sf,
                                                    rmc.headername.sf = rmc.headername.sf,
                                                    rmc.headername.sn = rmc.headername.sn,
                                                    rmc.headername.recordingid = recordingid_name,
                                                    rmc.header.structure = rmc.header.structure,
                                                    rmc.check4timegaps = rmc.check4timegaps,
                                                    rmc.col.wear = rmc.col.wear,
                                                    rmc.doresample = rmc.doresample,
                                                    rmc.scalefactor.acc = rmc.scalefactor.acc,
                                                    interpolationType = interpolationType,
                                                    PreviousLastValue = PreviousLastValue,
                                                    PreviousLastTime = PreviousLastTime,
                                                    desiredtz = desiredtz,
                                                    configtz = configtz,
                                                    header = header)
    }, silent = TRUE), ".raw.read.myacc.csv")
    if (length(sf) == 0) sf <- rmc.sf
  } else if (mon == .RAW_MONITOR[["PARMAY_MTX"]] && dformat == .RAW_FORMAT[["BIN"]]) {
    .raw.require("GGIRread", "Parmay Matrix .BIN files")
    note_error(try(expr = {P <- GGIRread::readParmayMatrix(filename = filename, output = "all",
                                                           start = startpage, end = endpage,
                                                           desiredtz = desiredtz, configtz = configtz,
                                                           interpolationType = interpolationType)}, silent = TRUE),
               "GGIRread::readParmayMatrix")
    # fix colnames to match expectations of GGIR
    colnames(P$data) <- gsub("acc_", "", colnames(P$data))
    colnames(P$data) <- gsub("ambient_temp", "temperature", colnames(P$data))
    if (P$lastchunk) {
      isLastBlock <- TRUE
    }
  } else {
    .raw.read.stop(paste0("No reader for monitor code ", mon, " and format code ", dformat, ": ", filename),
                   filename, blocknumber)
  }

  # if first block isn't read then the file is probably corrupt
  if (length(P$data) <= 1 || nrow(P$data) == 0) {
    P <- c()
    isLastBlock <- TRUE
    if (blocknumber == 1) {
      messages <- c(messages, '\nFile empty, possibly corrupt.\n')
      filequality$filetooshort <- TRUE
      filequality$filecorrupt <- TRUE
    }
  } else if (nrow(P$data) < (sf * ws * 2 + 1)) {
    # a shorter chunk of data than expected was read
    isLastBlock <- TRUE

    if (blocknumber == 1) {
      # not enough data for analysis
      P <- c()
      filequality$filetooshort <- TRUE
    }
  }

  # remove any columns we don't need/expect
  P$data <- P$data[, which(colnames(P$data) %in% c("x", "y", "z", "time", "light", "temperature", "wear"))]

  # every column except time and wear must be numeric
  for (col in c("x", "y", "z", "light", "temperature")) {
    if ((col %in% colnames(P$data)) && !is.numeric(P$data[, col])) {
      .raw.read.stop(paste0("Corrupt file. ", col, " column contains non-numeric data."), filename, blocknumber)
    }
  }

  # wear is coerced to numeric so it can join the numeric matrix later
  if ("wear" %in% colnames(P$data)) {
    if (!is.logical(P$data$wear)) {
      .raw.read.stop("Corrupt file. The wear column should contail TRUE/FALSE values.", filename, blocknumber)
    }
    P$data$wear <- as.numeric(P$data$wear)
  }

  # POSIXct time becomes Unix seconds so it can join the numeric matrix later
  if (("time" %in% colnames(P$data)) && !is.numeric(P$data$time)) {
    P$data$time <- as.numeric(P$data$time)
  }

  # the state is taken from the reader only when it returned it
  if ("PreviousLastValue" %in% names(P)) {
    previous_last_value <- P$PreviousLastValue
    previous_last_time <- P$PreviousLastTime
  }

  list(data = if (length(P) > 0) P$data else NULL,
       qclog = if (length(P) > 0) P$QClog else NULL,
       filequality = filequality,
       is_last_block = isLastBlock,
       endpage = endpage, startpage = startpage,
       previous_last_value = previous_last_value,
       previous_last_time = previous_last_time,
       header = if (length(P) > 0) P$header else NULL,
       messages = messages,
       P = P,
       blocknumber = blocknumber,
       filename = filename)
}

#' GGIR-Shaped Result of One Block Read
#'
#' The list g.readaccfile returns, built from a \code{.raw.read.block} result, so that
#' identical() against GGIR:::g.readaccfile holds.
#'
#' @param res A \code{.raw.read.block} result.
#' @return list(P, filequality, isLastBlock, endpage, startpage).
#' @keywords internal
#' @noRd
.raw.ggir.accread <- function(res) {
  list(P = res$P,
       filequality = res$filequality,
       isLastBlock = res$is_last_block,
       endpage = res$endpage, startpage = res$startpage)
}
