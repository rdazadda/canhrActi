# The gt3x stream reader of R/raw_gt3x_stream.R against read.gt3x: every batch the
# pipeline asks for on the MOS2 sample, edge batches, files it must decline, and an
# optional check on a large recording named by CANHRACTI_BIG_GT3X.

# the reader serves nothing under a read.gt3x other than the verified 1.2.0
skip_if_reader_off <- function() {
  if (utils::packageVersion("read.gt3x") != "1.2.0") testthat::skip("the stream reader is verified on read.gt3x 1.2.0 only")
}

stream_sample <- function() {
  p <- system.file("shiny", "canhrActi_dashboard", "data", "MOS2E39230594.gt3x", package = "canhrActi")
  ref <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")
  if (!nzchar(p) && nzchar(ref)) p <- file.path(sub("/+$", "", ref), "din", "MOS2E39230594.gt3x")
  if (!nzchar(p) || !file.exists(p)) testthat::skip("the MOS2 sample recording is not available")
  gsub("\\\\", "/", p)
}

rgt3x <- function(path, b, e) {
  tryCatch(read.gt3x::read.gt3x(path, batch_begin = b, batch_end = e, asDataFrame = TRUE, verbose = FALSE),
           error = function(err) err)
}
via_reader <- function(path, b, e) {
  tryCatch(.raw.gt3x.read.batch(path, b, e, stream_gt3x = TRUE), error = function(err) err)
}
# identical data, or the same error class and text
same_read <- function(x, y) {
  if (inherits(y, "error")) {
    return(inherits(x, "error") && identical(class(x), class(y)) &&
             identical(conditionMessage(x), conditionMessage(y)))
  }
  identical(x, y)
}

# a small extraction built from the first records of the sample, so read.gt3x's whole-file
# buffer stays small: info.txt with the last sample time one hour after the start
cut_sample <- function(n_records = 200, edit = NULL) {
  ex <- .raw.gt3x.extract(stream_sample(), location = tempfile("stream_src"))
  lb <- file.path(ex, "log.bin")
  x <- gt3x_log_index_cpp(lb)
  bytes <- readBin(lb, "raw", n = x$off[n_records])
  info <- readLines(file.path(ex, "info.txt"))
  if (!("Start Date: 638954656800000000" %in% info)) testthat::skip("unexpected sample info.txt")
  info[startsWith(info, "Last Sample Time:")] <- "Last Sample Time: 638954692800000000"
  unlink(dirname(ex), recursive = TRUE)
  if (!is.null(edit)) bytes <- edit(bytes, x$off)
  d <- tempfile("stream_cut")
  dir.create(d)
  writeLines(info, file.path(d, "info.txt"))
  writeBin(bytes, file.path(d, "log.bin"))
  gsub("\\\\", "/", d)
}

test_that("every batch the pipeline asks for on MOS2 is identical() to read.gt3x", {
  skip_if_reader_off()
  f <- stream_sample()
  loc <- tempfile("stream")
  ex <- .raw.gt3x.extract(f, location = loc)
  on.exit({ .raw.gt3x.extract.cleanup(f, location = loc); unlink(loc, recursive = TRUE) }, add = TRUE)
  w <- .raw.gt3x.stream.index(ex)
  expect_false(is.null(w))
  expect_identical(w$n, 147637L)
  # the decimal probe, then the 12 h calibration and 24 h metric blocks up to the empty read
  batches <- list(c(1, 10))
  for (bs in c(43200, 86400)) {
    for (k in seq_len(ceiling(w$n / bs) + 1)) batches[[length(batches) + 1]] <- c((k - 1) * bs + 1, k * bs)
  }
  for (be in batches) {
    s <- .raw.gt3x.stream.batch(ex, be[1], be[2])
    lab <- paste(be, collapse = "-")
    if (be[1] > w$n) {
      expect_identical(s, "eof", label = lab)
    } else {
      expect_true(is.data.frame(s), label = lab)
      expect(identical(s, rgt3x(ex, be[1], be[2])), paste("batch", lab, "differs from read.gt3x"))
    }
    expect(same_read(via_reader(ex, be[1], be[2]), rgt3x(ex, be[1], be[2])), paste("reader", lab))
  }
})

test_that("edge batches: first and last record, past the end, and the archive path", {
  f <- stream_sample()
  n <- 147637
  for (be in list(c(1, 1), c(n, n), c(n - 1, n + 10), c(n + 1, n + 10), c(700, 4000))) {
    expect(same_read(via_reader(f, be[1], be[2]), rgt3x(f, be[1], be[2])),
           paste("batch", paste(be, collapse = "-")))
  }
  err <- via_reader(f, n + 1, n + 10)
  expect_s3_class(err, "std::range_error")
  expect_identical(conditionMessage(err), "upper value must be greater than lower value")
  .raw.gt3x.extract.cleanup(f)
})

test_that("the index is built once per log.bin and goes with the extraction", {
  f <- stream_sample()
  ex <- .raw.gt3x.extract(f)
  w1 <- .raw.gt3x.stream.index(ex)
  n_before <- length(ls(.raw.gt3x.index.env))
  w2 <- .raw.gt3x.stream.index(ex)
  expect_identical(w1, w2)
  expect_identical(length(ls(.raw.gt3x.index.env)), n_before)
  .raw.gt3x.extract.cleanup(f)
  expect_false(dir.exists(ex))
  expect_false(any(startsWith(ls(.raw.gt3x.index.env), ex)))
})

test_that("a cut-down log.bin is served and matches read.gt3x", {
  skip_if_reader_off()
  d <- cut_sample()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_false(is.null(.raw.gt3x.stream.index(d)))
  for (be in list(c(1, 50), c(150, 199), c(190, 300), c(200, 210))) {
    expect(same_read(via_reader(d, be[1], be[2]), rgt3x(d, be[1], be[2])),
           paste("cut batch", paste(be, collapse = "-")))
  }
})

test_that("a data record that fails its checksum is served as read.gt3x reads it", {
  skip_if_reader_off()
  flip <- function(bytes, at) { bytes[at] <- xor(bytes[at], as.raw(0xFF)); bytes }
  d0 <- cut_sample()
  d <- cut_sample(edit = function(b, off) flip(b, off[100] + 8 + 11))
  on.exit(unlink(c(d0, d), recursive = TRUE), add = TRUE)
  w <- .raw.gt3x.stream.index(d)
  expect_false(is.null(w))
  expect_identical(w$bad, 100)
  for (be in list(c(90, 110), c(100, 100), c(1, 50), c(1, 200))) {
    lab <- paste("bad checksum batch", paste(be, collapse = "-"))
    s <- .raw.gt3x.stream.batch(d, be[1], be[2])
    expect_true(is.data.frame(s), label = lab)
    expect(identical(s, rgt3x(d, be[1], be[2])), lab)
    expect(same_read(via_reader(d, be[1], be[2]), rgt3x(d, be[1], be[2])), paste(lab, "through the reader"))
  }
  # the flipped byte is in the served data
  expect_false(identical(.raw.gt3x.stream.batch(d, 90, 110), .raw.gt3x.stream.batch(d0, 90, 110)))
})

test_that("files and batches the reader must refuse fall back to read.gt3x", {
  skip_if_reader_off()
  # a record other than a data record that fails its checksum (the EVENT record at byte 1022)
  d <- cut_sample(edit = function(b, off) {
    if (b[1023] != as.raw(0x1E) || b[1024] != as.raw(0x03)) testthat::skip("unexpected record layout")
    at <- 1031 + as.integer(b[1029]) + 256L * as.integer(b[1030])
    b[at] <- xor(b[at], as.raw(0xFF))
    b
  })
  expect_identical(gt3x_log_index_cpp(file.path(d, "log.bin"))$bad_other, 1)
  expect_null(.raw.gt3x.stream.index(d))
  expect(same_read(via_reader(d, 1, 50), rgt3x(d, 1, 50)), "bad checksum outside the data records")
  unlink(d, recursive = TRUE)
  # a record type the reader was not checked on (the EVENT record at byte 1022 made a TAG)
  d <- cut_sample(edit = function(b, off) {
    if (b[1023] != as.raw(0x1E) || b[1024] != as.raw(0x03)) testthat::skip("unexpected record layout")
    b[1024] <- as.raw(0x07)
    b[1022 + 8 + 3 + 1] <- xor(b[1022 + 8 + 3 + 1], as.raw(0x04))
    b
  })
  expect_null(.raw.gt3x.stream.index(d))
  expect(same_read(via_reader(d, 1, 50), rgt3x(d, 1, 50)), "unknown record type")
  unlink(d, recursive = TRUE)
  # a stray byte after the last record
  d <- cut_sample(edit = function(b, off) c(b, as.raw(0)))
  expect_null(.raw.gt3x.stream.index(d))
  expect(same_read(via_reader(d, 1, 50), rgt3x(d, 1, 50)), "stray byte")
  unlink(d, recursive = TRUE)
  # the old activity.bin layout, and no log.bin at all
  d <- cut_sample()
  file.create(file.path(d, "activity.bin"))
  expect_null(.raw.gt3x.stream.index(d))
  unlink(file.path(d, c("activity.bin", "log.bin")))
  expect_null(.raw.gt3x.stream.index(d))
  expect_null(.raw.gt3x.stream.batch(d, 1, 10))
  unlink(d, recursive = TRUE)
})

test_that("MOS2 blocks through stream_gt3x are identical() to the unzip_once path", {
  f <- stream_sample()
  info <- raw.inspect(f, desiredtz = "America/Anchorage")
  p <- info$params
  a <- list(); b <- list(); ea <- c(); eb <- c(); fa <- .raw.filequality(); fb <- fa
  for (i in 1:3) {
    a[[i]] <- .raw.read.block(info, 86400, i, ea, 3600, params = p, filequality = fa,
                              unzip_once = TRUE, stream_gt3x = FALSE)
    b[[i]] <- .raw.read.block(info, 86400, i, eb, 3600, params = p, filequality = fb,
                              unzip_once = TRUE, stream_gt3x = TRUE)
    ea <- a[[i]]$endpage; eb <- b[[i]]$endpage; fa <- a[[i]]$filequality; fb <- b[[i]]$filequality
    expect_identical(b[[i]], a[[i]], label = paste("block", i))
  }
  expect_null(b[[3]]$data)
  .raw.gt3x.extract.cleanup(f)
})

# start byte (1-based), type and payload size of every record in a log.bin byte vector
gt3x_records <- function(b) {
  out <- list()
  pos <- 1
  while (pos < length(b)) {
    sz <- as.integer(b[pos + 6]) + 256L * as.integer(b[pos + 7])
    out[[length(out) + 1]] <- c(pos, as.integer(b[pos + 1]), sz)
    pos <- pos + 9 + sz
  }
  m <- do.call(rbind, out)
  data.frame(pos = m[, 1], type = m[, 2], size = m[, 3])
}

test_that("a PARAMETERS record after an ACTIVITY record declines the whole file", {
  d0 <- cut_sample()
  expect_identical(gt3x_log_index_cpp(file.path(d0, "log.bin"))$act_before_param, 0)
  unlink(d0, recursive = TRUE)
  # the PARAMETERS record moved to just after data record 10
  d <- cut_sample(edit = function(b, off) {
    r <- gt3x_records(b)
    k <- which(r$type == 0x15)
    if (length(k) != 1 || any(r$type[seq_len(k)] == 0)) testthat::skip("unexpected record layout")
    ps <- r$pos[k]
    pe <- ps + 8 + r$size[k]
    at <- off[11] + 1
    c(b[seq_len(ps - 1)], b[(pe + 1):(at - 1)], b[ps:pe], b[at:length(b)])
  })
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  x <- gt3x_log_index_cpp(file.path(d, "log.bin"))
  expect_identical(x$n_param, 1L)
  expect_identical(x$act_before_param, 10)
  expect_null(.raw.gt3x.stream.index(d))
  # read.gt3x returns a random "features" attribute for a batch that ends before PARAMETERS
  nofeat <- function(x) { if (is.data.frame(x)) attr(x, "features") <- NULL; x }
  for (be in list(c(1, 5), c(1, 20), c(11, 30), c(1, 199), c(190, 300))) {
    a <- via_reader(d, be[1], be[2])
    b <- rgt3x(d, be[1], be[2])
    if (be[2] <= 10) { a <- nofeat(a); b <- nofeat(b) }
    expect(same_read(a, b), paste("late PARAMETERS batch", paste(be, collapse = "-")))
  }
})

# timestamp of the record starting at byte at (1-based)
rec_ts <- function(b, at) sum(as.integer(b[at + 2:5]) * 256^(0:3))
# a record with a valid checksum, and the same with the checksum byte chosen instead
gt3x_record <- function(type, t, payload) {
  sz <- length(payload)
  hdr <- as.raw(c(0x1E, type, (t %/% 256^(0:3)) %% 256, sz %% 256, sz %/% 256))
  cs <- bitwAnd(bitwNot(Reduce(bitwXor, as.integer(c(hdr, payload)), 0L)), 255L)
  c(hdr, payload, as.raw(cs))
}
usb_record <- function(t, cs) {
  h <- Reduce(bitwXor, as.integer(gt3x_record(0, t, raw(1))[1:8]), 0L)
  gt3x_record(0, t, as.raw(bitwXor(h, bitwAnd(bitwNot(cs), 255L))))
}
# the PARAMETERS record rebuilt around a new payload
edit_params <- function(b, fun) {
  r <- gt3x_records(b)
  k <- which(r$type == 0x15)
  if (length(k) != 1) testthat::skip("unexpected record layout")
  ps <- r$pos[k]
  pe <- ps + 8 + r$size[k]
  pl <- fun(b[(ps + 8):(pe - 1)])
  c(b[seq_len(ps - 1)], gt3x_record(0x15, rec_ts(b, ps), pl), b[(pe + 1):length(b)])
}

test_that("a USB event whose checksum byte is 0x1E declines the file when records follow it", {
  skip_if_reader_off()
  # data records 101 to 104 dropped and a USB event put 2 s into the gap
  mid <- function(cs) function(b, off) {
    u <- usb_record(rec_ts(b, off[101] + 1) + 2, cs)
    c(b[seq_len(off[101])], u, b[(off[105] + 1):length(b)])
  }
  d55 <- cut_sample(edit = mid(0x55))
  d1e <- cut_sample(edit = mid(0x1E))
  d1e_off <- cut_sample(edit = mid(0x1E))
  # the same event as the last record of the file
  dend <- cut_sample(edit = function(b, off) c(b, usb_record(rec_ts(b, off[199] + 1) + 2, 0x1E)))
  on.exit(unlink(c(d55, d1e, d1e_off, dend), recursive = TRUE), add = TRUE)
  expect_identical(gt3x_log_index_cpp(file.path(d1e, "log.bin"))$short_cs, 30)
  batches <- list(c(1, 50), c(1, 196), c(90, 196), c(1, 300))
  expect_false(is.null(.raw.gt3x.stream.index(d55)))
  expect_null(.raw.gt3x.stream.index(d1e))
  expect_false(is.null(.raw.gt3x.stream.index(dend)))
  for (be in batches) {
    lab <- paste(be, collapse = "-")
    expect(identical(.raw.gt3x.stream.batch(d55, be[1], be[2]), rgt3x(d55, be[1], be[2])),
           paste("checksum 0x55 batch", lab))
    expect(same_read(via_reader(d1e, be[1], be[2]), rgt3x(d1e, be[1], be[2])), paste("checksum 0x1E batch", lab))
    expect(identical(.raw.gt3x.stream.batch(dend, be[1], be[2]), rgt3x(dend, be[1], be[2])),
           paste("checksum 0x1E at the end, batch", lab))
  }
  # without the check the reader would serve the records read.gt3x loses
  real_index <- gt3x_log_index_cpp
  local_mocked_bindings(gt3x_log_index_cpp = function(path) {
    x <- real_index(path)
    x$short_cs[] <- 0x55
    x
  })
  s <- .raw.gt3x.stream.batch(d1e_off, 1, 196)
  r <- rgt3x(d1e_off, 1, 196)
  expect_true(is.data.frame(s) && is.data.frame(r))
  expect_identical(c(nrow(s), nrow(r)), c(5850L, 3000L))
})

test_that("missingness row names print a time on a multiple of 1e6 s as read.gt3x does", {
  skip_if_reader_off()
  f <- stream_sample()
  loc <- tempfile("stream")
  ex <- .raw.gt3x.extract(f, location = loc)
  on.exit({ .raw.gt3x.extract.cleanup(f, location = loc); unlink(loc, recursive = TRUE) }, add = TRUE)
  w <- .raw.gt3x.stream.index(ex)
  # record 58144 is at 1759999999 s, so a batch ending there closes at 1760000000
  expect_identical(w$ts[58144], 1759999999)
  for (be in list(c(58101, 58144), c(58144, 58144))) {
    s <- .raw.gt3x.stream.batch(ex, be[1], be[2])
    expect_identical(tail(rownames(attr(s, "missingness")), 1), "1760000000")
    expect(identical(s, rgt3x(ex, be[1], be[2])), paste("batch", paste(be, collapse = "-")))
  }
})

test_that("a PARAMETERS record whose size is not a multiple of 8 declines the file", {
  skip_if_reader_off()
  # four bytes past the last pair; read.gt3x takes the first for the checksum and the
  # second, 0x1E, for the start of a record
  pad <- function(b, off) edit_params(b, function(pl) c(pl, as.raw(c(0x00, 0x1E, 0x02, 0x00))))
  d <- cut_sample(edit = pad)
  d_off <- cut_sample(edit = pad)
  on.exit(unlink(c(d, d_off), recursive = TRUE), add = TRUE)
  x <- gt3x_log_index_cpp(file.path(d, "log.bin"))
  expect_identical(x$par_size %% 8, 4)
  expect_null(.raw.gt3x.stream.index(d))
  for (be in list(c(1, 50), c(1, 199), c(150, 300))) {
    expect(same_read(via_reader(d, be[1], be[2]), rgt3x(d, be[1], be[2])),
           paste("PARAMETERS size batch", paste(be, collapse = "-")))
  }
  # without the check the reader would serve data read.gt3x does not read
  real_index <- gt3x_log_index_cpp
  local_mocked_bindings(gt3x_log_index_cpp = function(path) {
    x <- real_index(path)
    x$par_size[] <- 320
    x
  })
  s <- .raw.gt3x.stream.batch(d_off, 1, 199)
  expect_true(is.data.frame(s))
  expect_false(identical(s, rgt3x(d_off, 1, 199)))
})

test_that("FEATURE_ENABLE from 2^31 declines the file; higher bits below it are served", {
  skip_if_reader_off()
  # the sample's own FEATURE_ENABLE is 388: sleep mode plus two undocumented bits
  set_fe <- function(v) function(b, off) edit_params(b, function(pl) {
    m <- matrix(pl, nrow = 8)
    k <- which(m[1, ] == as.raw(1) & m[2, ] == as.raw(0) & m[3, ] == as.raw(2) & m[4, ] == as.raw(0))
    if (length(k) != 1 || rec_ts(c(raw(2), m[5:8, k]), 1) != 388) testthat::skip("unexpected FEATURE_ENABLE")
    m[5:8, k] <- as.raw((v %/% 256^(0:3)) %% 256)
    as.vector(m)
  })
  d68 <- cut_sample(edit = set_fe(68))
  dhi <- cut_sample(edit = set_fe(2^31 + 4))
  on.exit(unlink(c(d68, dhi), recursive = TRUE), add = TRUE)
  expect_false(is.null(.raw.gt3x.stream.index(d68)))
  expect_null(.raw.gt3x.stream.index(dhi))
  for (be in list(c(1, 50), c(150, 300))) {
    lab <- paste(be, collapse = "-")
    s <- .raw.gt3x.stream.batch(d68, be[1], be[2])
    expect_identical(attr(s, "features"), "sleep mode")
    expect(identical(s, rgt3x(d68, be[1], be[2])), paste("FEATURE_ENABLE 68 batch", lab))
    # read.gt3x warns as it takes 2^31 + 4 for NA
    a <- suppressWarnings(via_reader(dhi, be[1], be[2]))
    b <- suppressWarnings(rgt3x(dhi, be[1], be[2]))
    expect_identical(attr(b, "features"), "none")
    expect(same_read(a, b), paste("FEATURE_ENABLE 2^31 + 4 batch", lab))
  }
})

test_that("a stage called on its own removes the extraction it made; the pipeline makes one", {
  f <- stream_sample()
  root <- file.path(gsub("\\\\", "/", tempdir()), "canhrActi_raw", "gt3x")
  nfiles <- function() length(list.files(root, recursive = TRUE, all.files = TRUE))
  .raw.gt3x.extract.cleanup(f)
  n0 <- nfiles()
  # every extraction goes through .raw.unzip, whichever way it then copies the members
  real_unzip <- .raw.unzip
  unzips <- 0L
  local_mocked_bindings(.raw.unzip = function(...) {
    unzips <<- unzips + 1L
    real_unzip(...)
  })
  info <- raw.inspect(f, desiredtz = "America/Anchorage")
  expect_identical(nfiles(), n0)
  cal <- raw.calibrate(info)
  expect_identical(nfiles(), n0)
  meta <- raw.getmeta(info, cal, daylimit = 1)
  expect_identical(nfiles(), n0)
  expect_identical(unzips, 3L)
  expect_error(raw.getmeta(info, cal, progress = function(stage, i, n, message) if (identical(i, 2L)) stop("stopped")),
               "stopped")
  expect_identical(nfiles(), n0)
  # an extraction that was already there is left alone
  ex <- .raw.gt3x.extract(f)
  info2 <- raw.inspect(f, desiredtz = "America/Anchorage")
  expect_true(file.exists(file.path(ex, "log.bin")))
  .raw.gt3x.extract.cleanup(f)
  # the pipeline extracts once for all its stages and removes it at the end
  unzips <- 0L
  x <- read.raw.accelerometer(f, desiredtz = "America/Anchorage", sleep = FALSE)
  expect_s3_class(x, "canhrActi_raw")
  expect_identical(unzips, 1L)
  expect_identical(nfiles(), n0)
})

test_that("stored members are copied out with their CRC-32 checked; anything else goes to utils::unzip", {
  skip_if_not_installed("zip")
  f <- stream_sample()
  real_unzip <- utils::unzip
  calls <- 0L
  local_mocked_bindings(unzip = function(...) {
    calls <<- calls + 1L
    real_unzip(...)
  }, .package = "utils")
  work <- tempfile("unzip_cases")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  files_in <- function(d) {
    l <- sort(list.files(d, all.files = TRUE, no.. = TRUE))
    stats::setNames(lapply(file.path(d, l), function(p) readBin(p, "raw", file.size(p))), l)
  }
  with_warnings <- function(expr) {
    w <- character()
    v <- withCallingHandlers(expr, warning = function(c) {
      w <<- c(w, conditionMessage(c))
      invokeRestart("muffleWarning")
    })
    list(value = v, warnings = w)
  }
  # the same archive through utils::unzip alone and through .raw.unzip
  compare <- function(zf) {
    a <- tempfile("plain", tmpdir = work)
    b <- tempfile("raw", tmpdir = work)
    dir.create(a)
    dir.create(b)
    pa <- with_warnings(real_unzip(zf, exdir = a))
    calls <<- 0L
    pb <- with_warnings(.raw.unzip(zf, exdir = b))
    out <- list(calls = calls, files = identical(files_in(a), files_in(b)),
                paths = identical(sort(basename(pa$value)), sort(basename(pb$value))),
                warnings = identical(pa$warnings, pb$warnings), n = length(files_in(b)))
    unlink(c(a, b), recursive = TRUE)
    out
  }
  # .raw.gt3x.extract as it was (utils::unzip only) and as it is
  extract_as <- function(zf, old) {
    if (old) local_mocked_bindings(.raw.unzip = function(zipfile, exdir) utils::unzip(zipfile, exdir = exdir))
    loc <- tempfile("loc", tmpdir = work)
    r <- tryCatch(.raw.gt3x.extract(zf, location = loc), error = function(e) e)
    out <- if (inherits(r, "error")) conditionMessage(r) else list(basename(r), files_in(r))
    .raw.gt3x.extract.cleanup(zf, location = loc)
    unlink(loc, recursive = TRUE)
    out
  }
  same_extract <- function(zf) identical(extract_as(zf, old = TRUE), extract_as(zf, old = FALSE))

  # the sample: three stored members, copied without utils::unzip
  m <- .raw.zip.stored(f)
  expect_identical(m$name, c("log.bin", "info.txt", "calibration.json"))
  expect_identical(m$offset[1], 93)
  r <- compare(f)
  expect_identical(r$calls, 0L)
  expect_true(r$files && r$paths && r$warnings)
  expect_identical(r$n, 3L)
  expect_true(same_extract(f))

  src <- file.path(work, "members")
  dir.create(src)
  real_unzip(f, exdir = src)
  # stored by another zip writer, with tails of every length the CRC loop can leave
  set.seed(3)
  lens <- c(0, 1, 7, 8, 9, 15, 16, 17, 1000003)
  for (len in lens) {
    writeBin(as.raw(sample(0:255, len, replace = TRUE)), file.path(src, paste0("f", len, ".bin")))
  }
  stored_files <- c("info.txt", "log.bin", "calibration.json", paste0("f", lens, ".bin"))
  zs <- file.path(work, "stored.gt3x")
  zip::zip(zs, stored_files, root = src, compression_level = 0)
  expect_false(is.null(.raw.zip.stored(zs)))
  r <- compare(zs)
  expect_identical(r$calls, 0L)
  expect_true(r$files && r$paths && r$warnings)
  expect_identical(r$n, 12L)

  # deflated: utils::unzip
  zd <- file.path(work, "deflated.gt3x")
  zip::zip(zd, c("log.bin", "info.txt", "calibration.json"), root = src, compression_level = 6)
  expect_null(.raw.zip.stored(zd))
  r <- compare(zd)
  expect_identical(r$calls, 1L)
  expect_true(r$files && r$paths && r$warnings)
  expect_true(same_extract(zd))

  # a byte of the second member changed: its CRC fails, the copies are dropped, utils::unzip
  zc <- file.path(work, "crc.gt3x")
  zip::zip(zc, c("info.txt", "log.bin", "calibration.json"), root = src, compression_level = 0)
  mc <- .raw.zip.stored(zc)
  expect_identical(mc$name[2], "log.bin")
  b <- readBin(zc, "raw", file.size(zc))
  at <- mc$offset[2] + 5001
  b[at] <- xor(b[at], as.raw(0x10))
  writeBin(b, zc)
  expect_false(is.null(.raw.zip.stored(zc)))
  r <- compare(zc)
  expect_identical(r$calls, 1L)
  expect_true(r$files && r$paths && r$warnings)
  expect_true(same_extract(zc))
})

test_that("the cached index follows info.txt as well as log.bin", {
  d <- cut_sample()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  x1 <- via_reader(d, 1, 50)
  ip <- file.path(d, "info.txt")
  inf <- readLines(ip)
  if (!("Subject Name: KL" %in% inf)) testthat::skip("unexpected sample info.txt")
  # same size, new mtime
  writeLines(sub("^Subject Name: KL$", "Subject Name: XY", inf), ip)
  Sys.setFileTime(ip, Sys.time() + 5)
  x2 <- via_reader(d, 1, 50)
  expect_identical(attr(x2, "subject_name"), "XY")
  expect(same_read(x2, rgt3x(d, 1, 50)), "same-size info.txt edit")
  # new size
  writeLines(sub("^Subject Name: KL$", "Subject Name: SOMEONE_ELSE", inf), ip)
  x3 <- via_reader(d, 1, 50)
  expect_identical(attr(x3, "subject_name"), "SOMEONE_ELSE")
  expect(same_read(x3, rgt3x(d, 1, 50)), "longer info.txt edit")
  expect_identical(attr(x1, "subject_name"), "KL")
})

test_that("a .gt3x replaced with the same size and mtime is hashed again by raw.inspect", {
  skip_if_not_installed("zip")
  d <- cut_sample()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  stored <- function(to) zip::zip(to, c("info.txt", "log.bin"), root = d, compression_level = 0)
  a <- tempfile(fileext = ".gt3x")
  stored(a)
  # the second archive: data record 5 holds 100 on every axis
  lb <- file.path(d, "log.bin")
  b <- readBin(lb, "raw", file.size(lb))
  at <- gt3x_log_index_cpp(lb)$off[5] + 1
  sz <- as.integer(b[at + 6]) + 256L * as.integer(b[at + 7])
  pl <- rep(as.raw(c(0x06, 0x40, 0x64)), sz / 3)
  cs <- Reduce(bitwXor, as.integer(c(b[at:(at + 7)], pl)), 0L)
  b[(at + 8):(at + 8 + sz)] <- c(pl, as.raw(bitwAnd(bitwNot(cs), 255L)))
  writeBin(b, lb)
  a2 <- tempfile(fileext = ".gt3x")
  stored(a2)
  expect_identical(file.size(a), file.size(a2))
  p <- file.path(gsub("\\\\", "/", tempfile("replaced")), "P01.gt3x")
  dir.create(dirname(p))
  file.copy(a, p)
  old_ex <- file.path(gsub("\\\\", "/", tempdir()), "canhrActi_raw", "gt3x",
                      paste0("P01_", unname(tools::md5sum(a))))
  on.exit(unlink(c(dirname(p), a, a2, old_ex), recursive = TRUE), add = TRUE)
  mt <- file.mtime(p)
  x_old <- via_reader(p, 1, 50)
  writeBin(readBin(a2, "raw", file.size(a2)), p)
  Sys.setFileTime(p, mt)
  expect_identical(file.mtime(p), mt)
  # the cached hash still points at the first archive's extraction
  expect_identical(via_reader(p, 1, 50), x_old)
  info <- raw.inspect(p, desiredtz = "America/Anchorage")
  x_new <- via_reader(p, 1, 50)
  expect_false(identical(x_new, x_old))
  expect_identical(x_new$X[4 * 30 + 1:30], rep(0.391, 30))
  expect(same_read(x_new, rgt3x(p, 1, 50)), "the replaced archive against read.gt3x")
  .raw.gt3x.extract.cleanup(p)
})

test_that("the reader declines unless read.gt3x is the verified 1.2.0", {
  skip_if_reader_off()
  d <- cut_sample()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_false(is.null(.raw.gt3x.stream.index(d)))
  local_mocked_bindings(packageVersion = function(pkg, lib.loc = NULL) package_version("1.3.0"),
                        .package = "utils")
  expect_null(.raw.gt3x.stream.index(d))
  expect_null(.raw.gt3x.stream.batch(d, 1, 50))
  expect(same_read(via_reader(d, 1, 50), rgt3x(d, 1, 50)), "read.gt3x under another version")
})

test_that("optional: every block of a large recording (CANHRACTI_BIG_GT3X) matches read.gt3x", {
  big <- Sys.getenv("CANHRACTI_BIG_GT3X", unset = "")
  if (!nzchar(big) || !file.exists(big)) testthat::skip("CANHRACTI_BIG_GT3X is unset or not a file")
  loc <- tempfile("stream_big")
  big <- gsub("\\\\", "/", big)
  ex <- .raw.gt3x.extract(big, location = loc)
  on.exit({ .raw.gt3x.extract.cleanup(big, location = loc); unlink(loc, recursive = TRUE) }, add = TRUE)
  w <- .raw.gt3x.stream.index(ex)
  skip_if(is.null(w), "the stream reader declines this file")
  # read.gt3x holds the whole recording for every call, so one block at a time
  for (bs in c(43200, 86400)) {
    for (k in seq_len(ceiling(w$n / bs) + 1)) {
      b <- (k - 1) * bs + 1
      ok <- same_read(via_reader(ex, b, b + bs - 1), rgt3x(ex, b, b + bs - 1))
      expect(ok, paste("block", b, "to", b + bs - 1))
      gc(FALSE)
    }
  }
})
