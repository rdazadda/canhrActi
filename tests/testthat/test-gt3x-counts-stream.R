# gt3x.counts block by block (R/gt3x_counts.R) against the whole-signal computation it
# replaced: synthetic signals at every sample rate, cut-down recordings with gaps, a USB
# event, a bad checksum and a zero sample, the MOS2 sample against digests of the old
# output and its .agd bytes, the files it must decline, and two optional checks: MOS2
# against the whole-signal path under CANHRACTI_LONG_TESTS (about 5 GB), and a large
# recording named by CANHRACTI_BIG_GT3X.

skip_if_no_counts <- function() {
  for (p in c("agcounts", "read.gt3x", "gsignal", "RSQLite")) {
    if (!requireNamespace(p, quietly = TRUE)) testthat::skip(paste(p, "is not installed"))
  }
  if (utils::packageVersion("agcounts") != "0.7.0") testthat::skip("the block path is verified on agcounts 0.7.0 only")
  if (utils::packageVersion("read.gt3x") != "1.2.0") testthat::skip("the stream reader is verified on read.gt3x 1.2.0 only")
}

counts_sample <- function() {
  p <- system.file("shiny", "canhrActi_dashboard", "data", "MOS2E39230594.gt3x", package = "canhrActi")
  if (!nzchar(p) || !file.exists(p)) testthat::skip("the MOS2 sample recording is not available")
  gsub("\\\\", "/", p)
}

# the counts as gt3x.counts computed them before the block path
old_counts <- function(path, epoch, lfe) {
  s <- .gt3x.counts.whole(read.gt3x::read.gt3x(path, asDataFrame = FALSE, imputeZeroes = TRUE), epoch, lfe)
  .gt3x.counts.frame(s, epoch, "UTC")
}
mat_src <- function(x, f) function(s0, s1) x[(s0 * f + 1):(s1 * f), , drop = FALSE]

# records of a log.bin: start byte, type and payload size
gt3x_recs <- function(b) {
  pos <- integer(0)
  i <- 1L
  while (i <= length(b)) {
    if (b[i] != as.raw(0x1E)) stop("not a record at byte ", i)
    pos <- c(pos, i)
    i <- i + 9L + as.integer(b[i + 6]) + 256L * as.integer(b[i + 7])
  }
  data.frame(pos = pos, type = as.integer(b[pos + 1]),
             size = as.integer(b[pos + 6]) + 256L * as.integer(b[pos + 7]))
}
rec_time <- function(b, at) sum(as.integer(b[at + 2:5]) * 256^(0:3))
make_rec <- function(type, t, payload) {
  sz <- length(payload)
  hdr <- as.raw(c(0x1E, type, (t %/% 256^(0:3)) %% 256, sz %% 256, sz %/% 256))
  cs <- bitwAnd(bitwNot(Reduce(bitwXor, as.integer(c(hdr, payload)), 0L)), 255L)
  c(hdr, payload, as.raw(cs))
}

# the first n_records data records of MOS2 in an extraction whose last sample time is
# secs after the start
cut_counts <- function(n_records = 2000, secs = 10800, edit = NULL) {
  ex <- .raw.gt3x.extract(counts_sample(), location = tempfile("counts_src"))
  lb <- file.path(ex, "log.bin")
  x <- gt3x_log_index_cpp(lb)
  bytes <- readBin(lb, "raw", n = x$off[n_records + 1])
  info <- readLines(file.path(ex, "info.txt"))
  if (!("Start Date: 638954656800000000" %in% info)) testthat::skip("unexpected sample info.txt")
  info[startsWith(info, "Last Sample Time:")] <- sprintf("Last Sample Time: %.0f", 638954656800000000 + secs * 1e7)
  unlink(dirname(ex), recursive = TRUE)
  if (!is.null(edit)) bytes <- edit(bytes, x$off + 1)
  d <- tempfile("counts_cut")
  dir.create(d)
  writeLines(info, file.path(d, "info.txt"))
  writeBin(bytes, file.path(d, "log.bin"))
  gsub("\\\\", "/", d)
}

test_that("blocks match the whole signal on synthetic signals at every sample rate", {
  skip_if_no_counts()
  set.seed(11)
  for (f in c(30, 40, 50, 60, 70, 80, 90, 100)) {
    secs <- 733
    n <- secs * f
    x <- round(cbind(X = rnorm(n, 0, 0.3) + sin(seq_len(n) / f * 4), Y = rnorm(n, -0.5, 0.4),
                     Z = rnorm(n, 0.2, 0.2)), 3)
    for (s in sort(sample(n - 5000, 8))) x[s:(s + sample(20:4000, 1)), ] <- 0
    x[1:(2 * f + 7), ] <- 0
    x[sample(n, 30), ] <- 0
    m <- structure(x, sample_rate = f, start_time = 0, time_index = 0)
    for (ep in c(1, 5, 60)) for (lfe in c(FALSE, TRUE)) {
      whole <- .gt3x.counts.whole(m, ep, lfe)$ec
      for (cs in unique(c(2 * ep, 7 * ep, ep * ceiling(301 / ep)))) {
        expect(identical(.gt3x.counts.chunked(mat_src(x, f), secs, f, ep, lfe, cs), whole),
               sprintf("%d Hz, epoch %d, lfe %s, blocks of %d s", f, ep, lfe, cs))
      }
    }
    expect_gt(sum(whole), 0)
  }
})

test_that("cut-down recordings: zero-imputed rows and counts match read.gt3x and the whole path", {
  skip_if_no_counts()
  # timestamps of every ACTIVITY record moved back 4 s, so the first record is at the start
  shift <- function(b, off) {
    r <- gt3x_recs(b)
    parts <- lapply(seq_len(nrow(r)), function(k) {
      at <- r$pos[k]
      rec <- b[at:(at + 8 + r$size[k])]
      if (r$type[k] != 0) return(rec)
      make_rec(0, rec_time(b, at) - 4, rec[8 + seq_len(r$size[k])])
    })
    do.call(c, parts)
  }
  # data records 101 to 104 dropped and a USB event put 2 s into the gap
  usb <- function(b, off) {
    u <- make_rec(0, rec_time(b, off[101]) + 2, as.raw(0x55))
    c(b[seq_len(off[101] - 1)], u, b[off[105]:length(b)])
  }
  flip <- function(b, off) { b[off[100] + 19] <- xor(b[off[100] + 19], as.raw(0xFF)); b }
  # the first sample of data record 150 read as 0, 0, 0
  zero <- function(b, off) { b[off[150] + 8 + 0:4] <- as.raw(0); b }
  cases <- list(plain = NULL, start = shift, usb = usb, checksum = flip, zero = zero)
  for (nm in names(cases)) {
    d <- cut_counts(edit = cases[[nm]])
    w <- .raw.gt3x.stream.index(d)
    expect_false(is.null(w), label = nm)
    m <- read.gt3x::read.gt3x(d, asDataFrame = FALSE, imputeZeroes = TRUE)
    x <- .gt3x.counts.source(w)(0, w$max_samples / w$sf)
    expect(identical(x, unname(unclass(m)[, c("X", "Y", "Z")])), paste(nm, "zero-imputed rows"))
    expect_identical(as.numeric(attr(m, "start_time")) + attr(m, "time_index")[1] / 100, w$start)
    for (ep in c(1, 60)) for (lfe in c(FALSE, TRUE)) {
      whole <- .gt3x.counts.whole(m, ep, lfe)
      expect(identical(.gt3x.counts.stream(d, ep, lfe, chunk_sec = 120), whole),
             sprintf("%s, epoch %d, lfe %s", nm, ep, lfe))
    }
    expect(identical(gt3x.counts(d, epoch = 60), old_counts(d, 60, FALSE)), paste(nm, "gt3x.counts"))
    if (nm == "plain") expect_gt(sum(m[, "X"] == 0 & m[, "Y"] == 0 & m[, "Z"] == 0), 0)
    if (nm == "start") expect_identical(w$ts[1], w$start)
    if (nm == "usb") expect_length(w$short_ts, 1)
    if (nm == "checksum") expect_identical(w$bad, 100)
    unlink(d, recursive = TRUE)
  }
})

test_that("MOS2 gives the counts and .agd bytes the whole-signal path gave", {
  skip_if_no_counts()
  skip_if_not_installed("digest")
  p <- counts_sample()
  cache <- file.path(tempdir(), "canhrActi_raw", "gt3x")
  before <- list.files(cache)
  # md5 digests of the old gt3x.counts output (canhrActi before the block path)
  pinned <- list(c(60, 0, "a9a688f214fc801615b66be342014a3c"), c(60, 1, "4e72693aeb7665afde546c4934098731"),
                 c(15, 1, "4ebc7e00fd9933ab0ae47a0ecdad7472"))
  for (pn in pinned) {
    ep <- as.numeric(pn[1])
    lfe <- pn[2] == "1"
    ct <- gt3x.counts(p, epoch = ep, lfe = lfe)
    expect(identical(digest::digest(ct, algo = "md5"), pn[3]), sprintf("MOS2 epoch %g, lfe %s", ep, lfe))
    if (ep != 60) next
    expect_identical(nrow(ct), 9919L)
    # gt3x.to.agd as it wrote these counts before, header from parse_gt3x_info() on the archive
    info <- read.gt3x::parse_gt3x_info(p)
    get <- function(k, d) { v <- info[[k]]; if (is.null(v) || length(v) == 0) d else v }
    a <- tempfile(fileext = ".agd")
    b <- tempfile(fileext = ".agd")
    gt3x.to.agd(p, agd_path = a, epoch = 60, lfe = lfe)
    write.agd(data.frame(timestamp = ct$time, axis1 = ct$axis1, axis2 = ct$axis2, axis3 = ct$axis3),
              path = b, epoch_length = 60, start_time = ct$time[1],
              device_serial = as.character(get("Serial Number", "")),
              device_name = as.character(get("Device Type", "wGT3XBT")),
              subject_name = as.character(get("Subject Name", "")),
              sample_rate = as.numeric(get("Sample Rate", 30)))
    expect_identical(unname(tools::md5sum(a)), unname(tools::md5sum(b)))
    expect_identical(.gt3x.info(p), info)
    unlink(c(a, b))
  }
  expect_identical(list.files(cache), before)
})

test_that("optional: MOS2 against the whole-signal path at every epoch and filter (CANHRACTI_LONG_TESTS)", {
  skip_if_no_counts()
  if (!canhr_flag("CANHRACTI_LONG_TESTS")) {
    testthat::skip("long-running test; set CANHRACTI_LONG_TESTS=1 to run it")
  }
  p <- counts_sample()
  for (lfe in c(FALSE, TRUE)) {
    one <- old_counts(p, 1, lfe)
    gc(FALSE)
    for (ep in c(1, 5, 10, 15, 30, 60)) {
      # epoch sums of whole numbers: the old epoch counts are sums of its 1 s counts
      n <- nrow(one) %/% ep
      g <- rep(seq_len(n), each = ep)
      k <- seq_len(n * ep)
      ec <- data.frame(X = as.numeric(rowsum(one$axis2[k], g)), Y = as.numeric(rowsum(one$axis1[k], g)),
                       Z = as.numeric(rowsum(one$axis3[k], g)))
      old <- .gt3x.counts.frame(list(t0 = as.numeric(one$time[1]), ec = ec), ep, "UTC")
      expect(identical(gt3x.counts(p, epoch = ep, lfe = lfe), old), sprintf("MOS2 epoch %d, lfe %s", ep, lfe))
    }
  }
  # the old path run directly at 60 s, as the reference for the sums above
  expect_identical(gt3x.counts(p, epoch = 60), old_counts(p, 60, FALSE))
})

test_that("what the block path is not sure about goes to the whole-signal path", {
  skip_if_no_counts()
  d <- cut_counts()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_false(is.null(.gt3x.counts.stream(d, 60, FALSE)))
  expect_null(.gt3x.counts.stream(d, 2.5, FALSE))
  expect_null(.gt3x.counts.stream(d, 0, FALSE))
  expect_null(.gt3x.counts.stream(d, 60, NA))
  expect_null(.gt3x.counts.stream(d, 60, 1))
  expect_null(.gt3x.counts.stream(d, 60, FALSE, chunk_sec = 90))
  # shorter than two blocks
  expect_null(.gt3x.counts.stream(d, 60, FALSE, chunk_sec = 7200))
  expect_null(.gt3x.counts.stream(file.path(d, "log.bin"), 60, FALSE))
  expect_null(.gt3x.counts.stream(c(d, d), 60, FALSE))
  # a self-test that failed, and an agcounts other than the verified 0.7.0
  key <- paste(30L, FALSE)
  was <- .gt3x.counts.env[[key]]
  .gt3x.counts.env[[key]] <- FALSE
  expect_null(.gt3x.counts.stream(d, 60, FALSE))
  expect(identical(gt3x.counts(d, epoch = 60), old_counts(d, 60, FALSE)), "self-test off")
  .gt3x.counts.env[[key]] <- was
  real <- utils::packageVersion
  local({
    local_mocked_bindings(packageVersion = function(pkg, lib.loc = NULL) {
      if (identical(pkg, "agcounts")) package_version("0.7.1") else real(pkg, lib.loc)
    }, .package = "utils")
    expect_false(is.null(.raw.gt3x.stream.index(d)))
    expect_null(.gt3x.counts.stream(d, 60, FALSE))
  })
  # a file the stream reader declines (a stray byte after the last record)
  d2 <- cut_counts(edit = function(b, off) c(b, as.raw(0)))
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  expect_null(.raw.gt3x.stream.index(d2))
  expect_null(.gt3x.counts.stream(d2, 60, FALSE))
  expect(identical(gt3x.counts(d2, epoch = 60), old_counts(d2, 60, FALSE)), "declined file")
  # a read that fails half way falls back as a whole
  local_mocked_bindings(gt3x_log_block_cpp = function(...) list(ok = FALSE))
  expect(identical(gt3x.counts(d, epoch = 60), old_counts(d, 60, FALSE)), "failed block read")
})

test_that("optional: spans of a large recording (CANHRACTI_BIG_GT3X) match the whole-signal path", {
  skip_if_no_counts()
  big <- Sys.getenv("CANHRACTI_BIG_GT3X", unset = "")
  if (!nzchar(big) || !file.exists(big)) testthat::skip("CANHRACTI_BIG_GT3X is unset or not a file")
  # the whole-signal path needs about 150 MB per hour of an 80 Hz span
  hours <- as.numeric(Sys.getenv("CANHRACTI_BIG_GT3X_HOURS", unset = "12"))
  big <- gsub("\\\\", "/", big)
  loc <- tempfile("counts_big")
  ex <- .raw.gt3x.extract(big, location = loc)
  on.exit({ .raw.gt3x.extract.cleanup(big, location = loc); unlink(loc, recursive = TRUE) }, add = TRUE)
  w <- .raw.gt3x.stream.index(ex)
  skip_if(is.null(w), "the stream reader declines this file")
  f <- w$sf
  n_sec <- w$max_samples / f
  secs <- min(hours * 3600, n_sec)
  # the first span, and one around the longest gap
  gap <- if (length(w$ent_n)) w$ent_t[which.max(w$ent_n)] - w$start else n_sec / 2
  starts <- unique(c(0, max(0, min(n_sec - secs, round(gap - secs / 2)))))
  src <- .gt3x.counts.source(w)
  first <- NULL
  for (s0 in starts) {
    x <- src(s0, s0 + secs)
    colnames(x) <- c("X", "Y", "Z")
    old <- .gt3x.counts.whole(structure(x, sample_rate = f, start_time = 0, time_index = 0), 60, FALSE)$ec
    gc(FALSE)
    for (cs in c(3600, 420)) {
      expect(identical(.gt3x.counts.chunked(mat_src(x, f), secs, f, 60, FALSE, cs), old),
             sprintf("span at %g s, blocks of %d s", s0, cs))
    }
    if (s0 == 0) first <- old
    rm(x)
    gc(FALSE)
  }
  # the whole recording converted block by block starts as the old path does
  ct <- gt3x.counts(big, epoch = 60)
  expect_identical(nrow(ct), as.integer(n_sec %/% 60))
  k <- seq_len(nrow(first))
  expect_identical(ct$axis1[k], first$Y)
  expect_identical(ct$axis2[k], first$X)
  expect_identical(ct$axis3[k], first$Z)
})
