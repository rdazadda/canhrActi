# Parity tests for R/raw_impute_timegaps.R against GGIR's g.imputeTimegaps.
# Reference data live in the folder named by CANHRACTI_GGIR_REF; tests that need
# them skip when it is unset. The EE 100 Hz file runs only when
# CANHRACTI_LONG_TESTS is "true".

ref_dir <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")
has_ref <- nzchar(ref_dir) && dir.exists(ref_dir)
has_ggir <- requireNamespace("GGIR", quietly = TRUE)
run_long <- canhr_flag("CANHRACTI_LONG_TESTS")

mos2_file <- file.path(ref_dir, "din", "MOS2E39230594.gt3x")
mos2_meta <- file.path(ref_dir, "out", "output_din", "meta", "basic",
                       "meta_MOS2E39230594.gt3x.RData")
ee_file <- file.path(ref_dir, "EE_left_29.5.2017-05-30.gt3x")
ee_meta <- file.path(ref_dir, "timing_out", "output_timing", "meta", "basic",
                     "meta_EE_left_29.5.2017-05-30.gt3x.RData")

skip_unless_ref <- function(...) {
  if (!has_ref) {
    skip("CANHRACTI_GGIR_REF is unset or is not an existing folder")
  }
  for (f in c(...)) {
    if (!file.exists(f)) skip(paste("reference file missing:", f))
  }
}
skip_unless_ggir <- function() {
  if (!has_ggir) skip("GGIR is not installed; live GGIR parity comparison skipped")
}

# First difference between two objects, or "" when identical.
first_difference <- function(a, b) {
  if (identical(a, b)) return("")
  fmt <- function(v) paste(format(v, digits = 17), collapse = ",")
  if (is.data.frame(a) && is.data.frame(b)) {
    if (!identical(dim(a), dim(b))) {
      return(sprintf("dim %s vs %s", paste(dim(a), collapse = "x"), paste(dim(b), collapse = "x")))
    }
    if (!identical(names(a), names(b))) {
      return(sprintf("names [%s] vs [%s]", paste(names(a), collapse = " "), paste(names(b), collapse = " ")))
    }
    for (nm in names(a)) {
      msg <- first_difference(a[[nm]], b[[nm]])
      if (nzchar(msg)) return(sprintf("column %s: %s", nm, msg))
    }
    if (!identical(attr(a, "row.names"), attr(b, "row.names"))) {
      ra <- attr(a, "row.names"); rb <- attr(b, "row.names")
      return(sprintf("row.names differ (class %s vs %s; head %s vs %s)", class(ra)[1], class(rb)[1],
                     fmt(utils::head(ra, 3)), fmt(utils::head(rb, 3))))
    }
    return("data.frame attributes differ")
  }
  if (!identical(class(a), class(b))) {
    return(sprintf("class %s vs %s", paste(class(a), collapse = "/"), paste(class(b), collapse = "/")))
  }
  if (!identical(typeof(a), typeof(b))) return(sprintf("typeof %s vs %s", typeof(a), typeof(b)))
  if (length(a) != length(b)) return(sprintf("length %d vs %d", length(a), length(b)))
  if (is.atomic(a)) {
    va <- unclass(a); vb <- unclass(b)
    d <- which(is.na(va) != is.na(vb) | (!is.na(va) & !is.na(vb) & va != vb))
    if (length(d) == 0) {
      return(sprintf("values equal, attributes differ: %s vs %s",
                     paste(names(attributes(a)), collapse = ","), paste(names(attributes(b)), collapse = ",")))
    }
    i <- d[1]
    return(sprintf("first differs at index %d: %s vs %s (%d indices differ)", i, fmt(va[i]), fmt(vb[i]), length(d)))
  }
  "objects differ (non-atomic)"
}

expect_same_as_ggir <- function(ours, ggir, label) {
  msg <- first_difference(ours, ggir)
  expect(msg == "", sprintf("%s: not identical() to GGIR; %s", label, msg))
  invisible(msg == "")
}

run_both <- function(x, sf, k = 0.25, impute = TRUE, plv = c(0, 0, 1), plt = NULL, epochsize = NULL) {
  ours <- .raw.impute.timegaps(x, sf = sf, k = k, impute = impute,
                               previous_last_value = plv, previous_last_time = plt,
                               epochsize = epochsize)
  ggir <- if (has_ggir) {
    GGIR:::g.imputeTimegaps(x, sf = sf, k = k, impute = impute,
                            PreviousLastValue = plv, PreviousLastTime = plt,
                            epochsize = epochsize)
  } else NULL
  list(ours = ours, ggir = ggir)
}

check_pair <- function(x, sf, label, k = 0.25, impute = TRUE, plv = c(0, 0, 1), plt = NULL,
                       epochsize = NULL, compare_start = TRUE) {
  r <- run_both(x, sf, k = k, impute = impute, plv = plv, plt = plt, epochsize = epochsize)
  expect_named(r$ours, c("x", "qclog"))
  if (has_ggir) {
    expect_same_as_ggir(r$ours$x, r$ggir$x, paste(label, "data"))
    if (compare_start) {
      expect_same_as_ggir(r$ours$qclog, r$ggir$QClog, paste(label, "qclog"))
    } else {
      keep <- c("imputed", "blockLengthSeconds", "timegaps_n", "timegaps_min")
      expect_same_as_ggir(r$ours$qclog[, keep], r$ggir$QClog[, keep], paste(label, "qclog (start/end are Sys.time() based)"))
    }
  }
  r$ours
}

# One gt3x block as g.getmeta reads it; falls back to read.gt3x when GGIR is absent.
read_gt3x_block <- function(f, blocknumber, previous_end_page, tz, plv = c(0, 0, 1), plt = NULL,
                            blocksize = 86400) {
  if (has_ggir) {
    params <- GGIR:::extract_params(params2check = c("metrics", "rawdata", "general", "cleaning"))
    params$params_general$desiredtz <- tz
    I <- GGIR::g.inspectfile(f, desiredtz = tz, params_rawdata = params$params_rawdata, configtz = c())
    fq <- data.frame(filetooshort = FALSE, filecorrupt = FALSE, filedoesnotholdday = FALSE, NFilePagesSkipped = 0)
    acc <- GGIR:::g.readaccfile(filename = f, blocksize = blocksize, blocknumber = blocknumber,
                                filequality = fq, ws = 3600, PreviousEndPage = previous_end_page,
                                inspectfileobject = I, PreviousLastValue = plv, PreviousLastTime = plt,
                                params_rawdata = params$params_rawdata,
                                params_general = params$params_general, header = NULL)
    return(list(data = acc$P$data, endpage = acc$endpage, startpage = acc$startpage,
                is_last = acc$isLastBlock, sf = I$sf))
  }
  startpage <- if (blocknumber > 1 && length(previous_end_page) != 0) previous_end_page + 1 else blocksize * (blocknumber - 1) + 1
  endpage <- startpage + blocksize - 1
  d <- try(read.gt3x::read.gt3x(path = f, batch_begin = startpage, batch_end = endpage, asDataFrame = TRUE), silent = TRUE)
  if (inherits(d, "try-error") || length(d) == 0) {
    return(list(data = NULL, endpage = endpage, startpage = startpage, is_last = TRUE, sf = NA))
  }
  sf <- attr(d, "sample_rate")
  colnames(d)[colnames(d) == "X"] <- "x"
  colnames(d)[colnames(d) == "Y"] <- "y"
  colnames(d)[colnames(d) == "Z"] <- "z"
  d$time <- lubridate::force_tz(d$time, tz)
  is_last <- nrow(d) < (sf * 3600 * 2 + 1)
  d <- d[, which(colnames(d) %in% c("x", "y", "z", "time", "light", "temperature", "wear"))]
  d$time <- as.numeric(d$time)
  list(data = d, endpage = endpage, startpage = startpage, is_last = is_last, sf = sf)
}

# MOS2 blocks 1 and 2, read once and cached.
mos2 <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    tz <- "America/Anchorage"
    b1 <- read_gt3x_block(mos2_file, 1, c(), tz)
    d1 <- b1$data
    r1 <- run_both(d1, sf = 30, k = 0.25, plv = c(0, 0, 1), plt = NULL, epochsize = c(5, 900))
    # carried state as g.getmeta derives it from the imputed output
    plv <- r1$ours$x[nrow(r1$ours$x), c("x", "y", "z")]
    plt <- as.POSIXct(r1$ours$x$time[nrow(r1$ours$x)], origin = "1970-1-1")
    b2 <- read_gt3x_block(mos2_file, 2, b1$endpage, tz, plv = plv, plt = plt)
    d2 <- b2$data
    r2 <- run_both(d2, sf = 30, k = 0.25, plv = plv, plt = plt, epochsize = c(5, 900))
    cache <<- list(d1 = d1, d2 = d2, b1 = b1, b2 = b2, r1 = r1, r2 = r2, plv = plv, plt = plt)
    cache
  }
})

sf10_base <- function() {
  sf <- 10
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  N <- 100
  data.frame(time = t0 + (0:(N - 1))/sf,
             x = round(0.1 * sin(1:N/5), 4),
             y = round(0.2 * cos(1:N/7), 4),
             z = round(0.95 + 0.05 * sin(1:N/3), 4))
}

test_that("P3a: MOS2 block 1 and 2 qclog rows are identical() to the stored M$QClog", {
  skip_unless_ref(mos2_file, mos2_meta)
  m <- mos2()
  e <- new.env()
  load(mos2_meta, envir = e)
  stored <- e$M$QClog

  expect_identical(nrow(m$d1), 2592000L)
  expect_identical(nrow(m$d2), 1837110L)
  expect_identical(m$b1$startpage, 1)
  expect_identical(m$b1$endpage, 86400)
  expect_identical(m$b2$startpage, 86401)
  expect_identical(m$b2$endpage, 172800)

  q1 <- m$r1$ours$qclog
  q2 <- m$r2$ours$qclog
  expect_identical(nrow(m$r1$ours$x), 5036460L)
  expect_identical(nrow(m$r2$ours$x), 4137180L)   # 137906 s * 30 Hz

  expect_identical(q1$imputed, TRUE)
  expect_identical(q2$imputed, TRUE)
  expect_identical(q1$start, 1759897684)
  expect_identical(q2$start, 1760065566)
  expect_identical(q1$end, 1764934144)
  expect_identical(q2$end, 1764202746)
  expect_identical(q1$blockLengthSeconds, 167882)
  expect_identical(q2$blockLengthSeconds, 137906)
  expect_identical(q1$timegaps_n, 915L)
  expect_identical(q2$timegaps_n, 525L)
  expect_identical(q1$timegaps_min, stored$timegaps_min[1])
  expect_identical(q2$timegaps_min, stored$timegaps_min[2])
  expect_equal(q1$timegaps_min, 1358.54166667, tolerance = 1e-11)
  expect_equal(q2$timegaps_min, 6093.10833333, tolerance = 1e-11)
  # GGIR sets end to start + nrow, not start + seconds
  expect_identical(q1$end - q1$start, as.numeric(nrow(m$r1$ours$x)))
  expect_identical(q2$end - q2$start, as.numeric(nrow(m$r2$ours$x)))

  both <- rbind(q1, q2)
  msg <- first_difference(both, stored)
  expect(msg == "", paste("rbind(qclog block 1, block 2) is not identical() to stored M$QClog;", msg))
})

test_that("P3b: MOS2 block 1 output is identical() to GGIR on the same input", {
  skip_unless_ref(mos2_file)
  skip_unless_ggir()
  m <- mos2()
  expect_same_as_ggir(m$r1$ours$x, m$r1$ggir$x, "MOS2 block 1 data.frame")
  expect_same_as_ggir(m$r1$ours$qclog, m$r1$ggir$QClog, "MOS2 block 1 qclog")
  expect_identical(colnames(m$r1$ours$x), c("time", "x", "y", "z"))   # no gap > 90 min in block 1
  expect_identical(m$r1$ours$x$time[1], 1759897684)
  gplv <- m$r1$ggir$x[nrow(m$r1$ggir$x), c("x", "y", "z")]
  gplt <- as.POSIXct(m$r1$ggir$x$time[nrow(m$r1$ggir$x)], origin = "1970-1-1")
  expect_identical(m$plv, gplv)
  expect_identical(m$plt, gplt)
})

test_that("P3b: MOS2 block 2 output is identical() to GGIR, including remaining_epochs", {
  skip_unless_ref(mos2_file)
  skip_unless_ggir()
  m <- mos2()
  expect_same_as_ggir(m$r2$ours$x, m$r2$ggir$x, "MOS2 block 2 data.frame")
  expect_same_as_ggir(m$r2$ours$qclog, m$r2$ggir$QClog, "MOS2 block 2 qclog")
  expect_identical(colnames(m$r2$ours$x), c("time", "x", "y", "z", "remaining_epochs"))
  re <- m$r2$ours$x$remaining_epochs
  expect_identical(re[re != 1],
                   c(1261, 5041, 1981, 3781, 1261, 1441, 7021, 9001, 5581, 6841, 1621, 1081, 8461, 1261, 2161))
  expect_identical(sum(re != 1), 15L)
  # no gap at the block boundary, so nothing was prepended
  expect_identical(m$r2$ours$x$time[1], m$d2$time[1])
  expect_identical(m$r2$ours$x$time[1], 1760065566)
})

test_that("P3c case A: one missing second at sf 10 is refilled with the pre-gap value", {
  base <- sf10_base()
  xa <- base[-(31:40), ]
  o <- check_pair(xa, 10, "case A")
  expect_identical(nrow(o$x), 100L)
  expect_identical(o$qclog$imputed, TRUE)
  expect_identical(o$qclog$timegaps_n, 1L)
  expect_identical(o$qclog$timegaps_min, 11/10/60)   # round(1.1 * 10) = 11 samples
  expect_identical(o$qclog$blockLengthSeconds, 10)
  expect_identical(o$qclog$end - o$qclog$start, 100)
  # pre-gap norm 0.927 is carried without rescaling
  expect_lt(sqrt(sum(xa[30, c("x", "y", "z")]^2)), 0.995)
  for (r in 31:40) expect_identical(unname(unlist(o$x[r, c("x", "y", "z")])), unname(unlist(xa[30, c("x", "y", "z")])))
  expect_equal(as.numeric(o$x$time[31:40]), as.numeric(base$time[30]) + (1:10)/10, tolerance = 1e-9)
})

test_that("P3c case B: a run of 5 zero rows becomes a gap and the pre-gap sample (norm 1.0057) is rescaled", {
  base <- sf10_base()
  xb <- base
  xb[61:65, c("x", "y", "z")] <- 0
  en60 <- sqrt(sum(xb[60, c("x", "y", "z")]^2))
  expect_gt(en60, 1.005)
  expect_equal(en60, 1.0057, tolerance = 1e-4)
  o <- check_pair(xb, 10, "case B")
  expect_identical(nrow(o$x), 100L)
  expect_identical(o$qclog$timegaps_n, 1L)
  expect_identical(o$qclog$timegaps_min, 6/10/60)    # round(0.6 * 10) = 6
  expect_equal(unname(sqrt(rowSums(o$x[60:65, c("x", "y", "z")]^2))), rep(1, 6), tolerance = 1e-12)
  # k = 2/sf, as in GGIR's own test
  o2 <- check_pair(xb, 10, "case B2", k = 2/10)
  expect_identical(o2$qclog, o$qclog)
})

test_that("P3c case B3: a single zero row is dropped and NOT refilled (0.2 s gap < k)", {
  base <- sf10_base()
  xb3 <- base
  xb3[61, c("x", "y", "z")] <- 0
  o <- check_pair(xb3, 10, "case B3")
  expect_identical(nrow(o$x), 99L)
  expect_identical(o$qclog$imputed, FALSE)
  expect_identical(o$qclog$timegaps_n, 0L)
  expect_identical(o$qclog$timegaps_min, 0)
  expect_identical(o$qclog$blockLengthSeconds, 9.9)
  # POSIXct doubles near 1.7e9 put the dt about 2e-7 below 0.2
  expect_equal(as.numeric(diff(o$x$time[60:61])), 0.2, tolerance = 1e-5)
  # the float dt is still below k = 2/sf = 0.2, so that refills nothing either
  o2 <- check_pair(xb3, 10, "case B3 k=2/sf", k = 2/10)
  expect_lt(as.numeric(diff(xb3$time[c(60, 62)])), 2/10)
  expect_identical(nrow(o2$x), 99L)
  expect_identical(o2$qclog$timegaps_n, 0L)
  # on an exact grid (sf 8) the dt is 0.25 == k, which counts as a gap
  xb3n <- xb3
  xb3n$time <- (0:99)/8
  expect_identical(diff(xb3n$time[c(60, 62)]), 0.25)
  o3 <- check_pair(xb3n, 8, "case B3 sf 8 exact boundary")
  expect_identical(nrow(o3$x), 100L)
  expect_identical(o3$qclog$timegaps_n, 1L)
  expect_identical(o3$qclog$timegaps_min, 2/8/60)
})

test_that("P3c case C: an off-grid +0.03 s sample passes through untouched", {
  base <- sf10_base()
  xc <- base
  xc$time[80] <- xc$time[80] + 0.03
  o <- check_pair(xc, 10, "case C")
  expect_identical(nrow(o$x), 100L)
  expect_identical(o$qclog$timegaps_n, 0L)
  expect_identical(o$x$time, xc$time)
  expect_equal(as.numeric(diff(o$x$time[79:81])), c(0.13, 0.07), tolerance = 1e-5)
})

test_that("P3c case C2: a +0.5 s shifted sample adds 6 rows and leaves the -0.4 s step", {
  base <- sf10_base()
  xc2 <- base
  xc2$time[80] <- xc2$time[80] + 0.5
  o <- check_pair(xc2, 10, "case C2")
  expect_identical(nrow(o$x), 105L)
  expect_identical(o$qclog$timegaps_n, 1L)
  expect_identical(o$qclog$timegaps_min, 6/10/60)
  dt <- as.numeric(diff(o$x$time))
  expect_equal(min(dt), -0.4, tolerance = 1e-5)
  expect_identical(sum(dt < 0), 1L)
})

test_that("P3c case D: norm 1.3 is rescaled in place, norm 0.5 is carried as is", {
  base <- sf10_base()
  xd <- base
  xd[30, c("x", "y", "z")] <- c(0.3, 0.4, 1.2)
  xd[70, c("x", "y", "z")] <- c(0.1, 0.2, 0.4472)
  xd <- xd[-c(31:35, 71:75), ]
  o <- check_pair(xd, 10, "case D")
  expect_identical(nrow(o$x), 100L)
  expect_identical(o$qclog$timegaps_n, 2L)
  expect_equal(sqrt(sum(o$x[30, c("x", "y", "z")]^2)), 1, tolerance = 1e-12)
  expect_equal(unname(unlist(o$x[31, c("x", "y", "z")])), c(0.3, 0.4, 1.2)/1.3, tolerance = 1e-12)
  i70 <- which(as.numeric(o$x$time) == as.numeric(base$time[70]))
  expect_identical(unname(unlist(o$x[i70 + 1, c("x", "y", "z")])), c(0.1, 0.2, 0.4472))
  expect_lt(sqrt(sum(o$x[i70 + 1, c("x", "y", "z")]^2)), 0.995)
})

test_that("P3c case E: previous_last_time 2 s earlier prepends 20 rows with previous_last_value", {
  base <- sf10_base()
  xe <- base[1:20, ]
  o <- check_pair(xe, 10, "case E", plv = c(0.5, 0.5, 0.5), plt = base$time[1] - 2)
  expect_identical(nrow(o$x), 40L)
  expect_identical(o$qclog$timegaps_n, 1L)
  expect_identical(o$qclog$timegaps_min, 20/10/60)
  expect_identical(as.numeric(o$x$time[1]), as.numeric(base$time[1]) - 2)
  # norm 0.866 is carried as is, replicated round(2 * sf) = 20 times
  for (r in 1:20) expect_identical(unname(unlist(o$x[r, c("x", "y", "z")])), c(0.5, 0.5, 0.5))
  expect_identical(unname(unlist(o$x[21, c("x", "y", "z")])), unname(unlist(xe[1, c("x", "y", "z")])))
  expect_identical(as.numeric(o$x$time[21]), as.numeric(base$time[1]))
  o2 <- check_pair(xe, 10, "case E near", plv = c(0.5, 0.5, 0.5), plt = base$time[1] - 0.1)
  expect_identical(nrow(o2$x), 20L)
  expect_identical(o2$qclog$timegaps_n, 0L)
})

test_that("P3c cases E2 and E3: a zero first row takes previous_last_value, a zero last row copies the row before", {
  base <- sf10_base()
  xe2 <- base[1:10, ]
  xe2[1, c("x", "y", "z")] <- 0
  o <- check_pair(xe2, 10, "case E2", plv = c(0.11, 0.22, 0.97))
  expect_identical(nrow(o$x), 10L)
  expect_identical(unname(unlist(o$x[1, c("x", "y", "z")])), c(0.11, 0.22, 0.97))
  expect_identical(o$qclog$timegaps_n, 0L)

  xe3 <- base[1:10, ]
  xe3[10, c("x", "y", "z")] <- 0
  o3 <- check_pair(xe3, 10, "case E3")
  expect_identical(nrow(o3$x), 10L)
  expect_identical(unname(unlist(o3$x[10, c("x", "y", "z")])), unname(unlist(xe3[9, c("x", "y", "z")])))
  expect_identical(o3$qclog$blockLengthSeconds, 1)

  xe4 <- base[1:10, ]
  xe4[c(1, 10), c("x", "y", "z")] <- 0
  o4 <- check_pair(xe4, 10, "case E2+E3", plv = c(0, 0, 1))
  expect_identical(nrow(o4$x), 10L)
  expect_identical(unname(unlist(o4$x[1, c("x", "y", "z")])), c(0, 0, 1))
})

test_that("P3c case F: impute = FALSE removes zero rows (first and last included) and fills nothing", {
  base <- sf10_base()
  xf <- base[, c("x", "y", "z")]
  xf[61:65, ] <- 0
  xf[1, ] <- 0
  xf[100, ] <- 0
  o <- check_pair(xf, 10, "case F", impute = FALSE, compare_start = FALSE)
  expect_identical(nrow(o$x), 93L)
  expect_identical(colnames(o$x), c("x", "y", "z"))
  expect_identical(o$qclog$imputed, FALSE)
  # timegaps_n stays a double 0 on this path, integer on the impute path
  expect_identical(o$qclog$timegaps_n, 0)
  expect_identical(o$qclog$blockLengthSeconds, 9.3)
  # as in GGIR, the imputelast step still fires after the trailing zero row is
  # removed, so the new last row is a copy of original row 98
  kept <- xf[-c(1, 61:65, 100), ]
  expect_identical(o$x$x[1:92], kept$x[1:92])
  expect_identical(unname(unlist(o$x[93, ])), unname(unlist(xf[98, ])))
  expect_false(identical(o$x$x[93], xf$x[99]))
  xf2 <- base[-(31:40), ]
  o2 <- check_pair(xf2, 10, "case F time", impute = FALSE)
  expect_identical(nrow(o2$x), 90L)
  expect_identical(colnames(o2$x), c("time", "x", "y", "z"))
})

test_that("P3c case G: no time column gets a synthetic axis, zeros are refilled, time removed at the end", {
  base <- sf10_base()
  xg <- base[, c("x", "y", "z")]
  xg[61:65, ] <- 0
  t_before <- as.numeric(Sys.time())
  o <- check_pair(xg, 10, "case G", compare_start = FALSE)
  expect_identical(nrow(o$x), 100L)
  expect_identical(colnames(o$x), c("x", "y", "z"))
  expect_identical(o$qclog$imputed, TRUE)
  expect_identical(o$qclog$timegaps_n, 1L)
  expect_identical(o$qclog$blockLengthSeconds, 10)
  # start is a Sys.time() value taken inside the call, not a recording time
  expect_gte(o$qclog$start, t_before - 1)
  expect_lte(o$qclog$start, as.numeric(Sys.time()) + 1)
  expect_equal(unname(sqrt(rowSums(o$x[60:65, ]^2))), rep(1, 6), tolerance = 1e-12)
})

test_that("P3c case H: a 100-min gap with epochsize c(5, 900) is raw-filled to the 00:30 cut, remaining_epochs 1081", {
  sfh <- 10
  th <- as.numeric(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"))
  N1 <- 20 * 60 * sfh
  xh1 <- data.frame(time = th + (0:(N1 - 1))/sfh, x = 0.1, y = 0.2, z = 1.1)
  xh2 <- data.frame(time = th + 120 * 60 + (0:(N1 - 1))/sfh, x = 0.3, y = 0.1, z = 0.9)
  xh <- rbind(xh1, xh2)
  o <- check_pair(xh, sfh, "case H", epochsize = c(5, 900))
  expect_identical(nrow(o$x), 30000L)
  expect_identical(colnames(o$x), c("time", "x", "y", "z", "remaining_epochs"))
  ii <- which(o$x$remaining_epochs != 1)
  expect_identical(ii, 18000L)
  expect_identical(o$x$remaining_epochs[ii], 1081)
  expect_equal(o$x$time[ii], th + 30 * 60 - 0.1, tolerance = 1e-9)     # 00:29:59.9
  expect_identical(o$x$time[ii + 1], th + 120 * 60)                      # real 02:00:00 sample
  expect_identical(o$qclog$timegaps_n, 1L)
  expect_identical(o$qclog$timegaps_min, 60001/sfh/60)                   # full gap, before shortening
  expect_identical(o$qclog$blockLengthSeconds, 3000)                     # raw rows only
  # pre-gap norm 1.118 is rescaled before the fill
  expect_equal(unname(unlist(o$x[18000, c("x", "y", "z")])), c(0.1, 0.2, 1.1)/sqrt(0.01 + 0.04 + 1.21), tolerance = 1e-12)
  xhp <- xh
  xhp$time <- as.POSIXct(xhp$time, origin = "1970-01-01", tz = "UTC")
  op <- check_pair(xhp, sfh, "case H POSIXct", epochsize = c(5, 900))
  expect_identical(nrow(op$x), 30000L)
  expect_identical(op$x$remaining_epochs[op$x$remaining_epochs != 1], 1081)
  expect_s3_class(op$x$time, "POSIXct")
  on <- check_pair(xh, sfh, "case H no epochsize", epochsize = NULL)
  expect_identical(nrow(on$x), 24000L + 60000L)
  expect_identical(colnames(on$x), c("time", "x", "y", "z"))
  expect_identical(on$qclog$timegaps_min, o$qclog$timegaps_min)
})

test_that("P3c case H2: an unaligned 107.5-min gap raw-fills 600 s + 450 s (10500 rows) with the pre-gap value", {
  sfh <- 10
  th <- as.numeric(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"))
  N1 <- 20 * 60 * sfh
  xh1 <- data.frame(time = th + (0:(N1 - 1))/sfh, x = 0.1, y = 0.2, z = 1.1)
  xh3 <- data.frame(time = th + 127.5 * 60 + (0:(N1 - 1))/sfh, x = 0.3, y = 0.1, z = 0.9)
  xhb <- rbind(xh1, xh3)
  o <- check_pair(xhb, sfh, "case H2", epochsize = c(5, 900))
  expect_identical(nrow(o$x), 24000L + 10500L)
  ii <- which(o$x$remaining_epochs != 1)
  # a whole number of short epochs, so the marker sits on the last replicated row
  expect_identical(ii, 22500L)
  expect_identical(o$x$remaining_epochs[ii], 1081)
  # fill (b) follows fill (a) directly, carrying the pre-gap value from 00:30:00
  expect_equal(unname(unlist(o$x[22500, c("x", "y", "z")])), c(0.1, 0.2, 1.1)/sqrt(0.01 + 0.04 + 1.21), tolerance = 1e-12)
  expect_equal(o$x$time[18001], th + 30 * 60, tolerance = 1e-9)
  expect_identical(o$x$time[22501], th + 127.5 * 60)
  expect_identical(unname(unlist(o$x[22501, c("x", "y", "z")])), c(0.3, 0.1, 0.9))
  expect_identical(o$qclog$timegaps_min, 64501/sfh/60)
  expect_identical(o$qclog$blockLengthSeconds, 3450)
})

test_that("P3c case I: gap sizes on the k boundary follow round() half-even and float arithmetic", {
  base <- sf10_base()
  # dt 0.3 -> round(3) = 3 samples -> 2 rows added
  xi <- base[-c(31, 32), ]
  o <- check_pair(xi, 10, "case I dt 0.3")
  expect_identical(o$qclog$timegaps_n, 1L)
  expect_identical(nrow(o$x), 100L)
  expect_identical(o$qclog$timegaps_min, 3/10/60)
  # dt exactly 0.25 -> round(2.5) = 2 samples -> 1 row added
  xi2 <- base
  xi2$time[31:100] <- xi2$time[31:100] + 0.15
  o2 <- check_pair(xi2, 10, "case I dt 0.25")
  expect_identical(o2$qclog$timegaps_n, 1L)
  expect_identical(nrow(o2$x), 101L)
  expect_identical(o2$qclog$timegaps_min, 2/10/60)
  # dt "0.35" is 0.3499999 in POSIXct doubles, so 3 samples and 2 rows
  xi3 <- base
  xi3$time[31:100] <- xi3$time[31:100] + 0.25
  dt35 <- as.numeric(diff(xi3$time[30:31]))
  expect_lt(dt35, 0.35)
  expect_identical(round(dt35 * 10), 3)
  o3 <- check_pair(xi3, 10, "case I dt 0.35")
  expect_identical(o3$qclog$timegaps_n, 1L)
  expect_identical(nrow(o3$x), 102L)
  expect_identical(o3$qclog$timegaps_min, 3/10/60)
  # on an exact numeric axis the dt is 0.35000000000000009, so 4 samples and 3 rows
  xi4 <- base
  xi4$time <- (0:99)/10
  xi4$time[31:100] <- xi4$time[31:100] + 0.25
  o4 <- check_pair(xi4, 10, "case I dt 0.35 numeric")
  expect_identical(o4$qclog$timegaps_n, 1L)
  expect_identical(round(as.numeric(diff(xi4$time[30:31])) * 10), 4)
  expect_identical(nrow(o4$x), 103L)
})

test_that("P3c: extra columns (light, temperature, wear) are replicated with the gap rows", {
  base <- sf10_base()
  xl <- base[-(31:40), ]
  xl$light <- seq_len(nrow(xl))
  xl$temperature <- 20 + seq_len(nrow(xl))/100
  xl$wear <- 1
  o <- check_pair(xl, 10, "extra columns")
  expect_identical(nrow(o$x), 100L)
  expect_identical(colnames(o$x), c("time", "x", "y", "z", "light", "temperature", "wear"))
  expect_identical(o$x$light[30:41], c(rep(30L, 11), 31L))
})

test_that("P3d: .raw.clock.seconds reproduces data.table::hour/minute/second on numeric and POSIXct input", {
  skip_if_not_installed("data.table")
  dt_seconds <- function(t) {
    data.table::hour(t) * 60^2 + data.table::minute(t) * 60 + data.table::second(t)
  }
  th <- as.numeric(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"))
  nums <- c(1759897684 + 29/30,                       # gt3x pre-gap sample, fractional second
            1759897684, 1760065566,
            th, th + 0.999, th + 1199.9, th + 7200,
            as.numeric(as.POSIXct("2024-11-03 01:30:00", tz = "America/Anchorage")),
            as.numeric(as.POSIXct("2024-11-03 01:30:00", tz = "America/Anchorage")) + 3600,
            as.numeric(as.POSIXct("2024-03-10 03:00:00", tz = "America/Anchorage")) - 0.5,
            0, -1.5, 86399.5)
  for (t in nums) {
    expect_identical(.raw.clock.seconds(t), dt_seconds(t), label = sprintf("numeric %s", format(t, digits = 17)))
  }
  expect_identical(.raw.clock.seconds(nums), dt_seconds(nums))
  # seconds are truncated: 4.9667 counts as 4
  expect_identical(.raw.clock.seconds(1759897684 + 29/30) %% 60, 4)
  expect_identical(as.POSIXlt(1759897684 + 29/30)$sec > 4.9, TRUE)
  for (tz in c("UTC", "GMT", "Europe/Helsinki", "America/Anchorage", "Asia/Kolkata")) {
    p <- as.POSIXct(nums[1:7], origin = "1970-01-01", tz = tz)
    expect_identical(.raw.clock.seconds(p), dt_seconds(p), label = paste("POSIXct", tz))
  }
  expect_identical(.raw.clock.seconds(nums[1]),
                   as.POSIXlt(nums[1], tz = "")$hour * 3600 + as.POSIXlt(nums[1], tz = "")$min * 60 +
                     as.integer(as.POSIXlt(nums[1], tz = "")$sec))
})

test_that("P3d: the 100-min and 107.5-min cases and DST-crossing gaps are identical() to GGIR", {
  skip_unless_ggir()
  # the three DST cases need the Anchorage clock
  withr::local_timezone("America/Anchorage")
  sfh <- 10
  N1 <- 20 * 60 * sfh
  run_case <- function(t_start, t_resume, label) {
    x1 <- data.frame(time = t_start + (0:(N1 - 1))/sfh, x = 0.1, y = 0.2, z = 1.1)
    x2 <- data.frame(time = t_resume + (0:(N1 - 1))/sfh, x = 0.3, y = 0.1, z = 0.9)
    check_pair(rbind(x1, x2), sfh, label, epochsize = c(5, 900))
  }
  th <- as.numeric(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"))
  o100 <- run_case(th, th + 120 * 60, "100 min")
  expect_identical(nrow(o100$x), 30000L)
  o107 <- run_case(th, th + 127.5 * 60, "107.5 min")
  expect_identical(nrow(o107$x), 34500L)

  # fall back: 2024-11-03 02:00 AKDT -> 01:00 AKST
  fb_start <- as.numeric(as.POSIXct("2024-11-03 00:00:00", tz = "America/Anchorage"))
  fb_resume <- as.numeric(as.POSIXct("2024-11-03 03:07:30", tz = "America/Anchorage"))
  expect_identical(fb_resume - fb_start, 247.5 * 60)   # 187.5 wall-clock min + the repeated hour
  ofb <- run_case(fb_start, fb_resume, "DST fall back")
  expect_identical(ofb$qclog$timegaps_n, 1L)
  # 227.5 min + one sample
  expect_identical(ofb$qclog$timegaps_min, 136501/sfh/60)
  ii <- which(ofb$x$remaining_epochs != 1)
  expect_length(ii, 1L)
  # fill (a) 00:20 -> 00:30 (6000 rows), fill (b) 03:00 -> 03:07:30 (4500 rows),
  # remaining (136501 - 10501)/50 + 1
  expect_identical(nrow(ofb$x), 24000L + 6000L + 4500L)
  expect_identical(ii, 22500L)
  expect_identical(ofb$x$remaining_epochs[ii], 2521)

  # spring forward: 2024-03-10 02:00 AKST -> 03:00 AKDT
  sf_start <- as.numeric(as.POSIXct("2024-03-10 00:00:00", tz = "America/Anchorage"))
  sf_resume <- as.numeric(as.POSIXct("2024-03-10 04:07:30", tz = "America/Anchorage"))
  expect_identical(sf_resume - sf_start, 187.5 * 60)   # 247.5 wall-clock min minus the skipped hour
  osf <- run_case(sf_start, sf_resume, "DST spring forward")
  expect_identical(osf$qclog$timegaps_n, 1L)
  expect_identical(osf$qclog$timegaps_min, 100501/sfh/60)   # 167.5 min + one sample
  ii <- which(osf$x$remaining_epochs != 1)
  expect_length(ii, 1L)
  # fill (a) 6000 rows, fill (b) 04:00 -> 04:07:30 (4500 rows), remaining (100501 - 10501)/50 + 1
  expect_identical(nrow(osf$x), 24000L + 6000L + 4500L)
  expect_identical(ii, 22500L)
  expect_identical(osf$x$remaining_epochs[ii], 1801)

  # a gap that ends inside the repeated hour
  rh_start <- as.numeric(as.POSIXct("2024-11-03 00:50:00", tz = "America/Anchorage"))
  orh <- run_case(rh_start, rh_start + 200 * 60, "DST gap across the repeated hour")
  expect_identical(orh$qclog$timegaps_n, 1L)
  expect_identical(orh$qclog$timegaps_min, 108001/sfh/60)
  ii <- which(orh$x$remaining_epochs != 1)
  expect_length(ii, 1L)
  # fill (a) 01:10 -> 01:15 AKDT (3000 rows), fill (b) 03:00 -> 03:10 AKST (6000 rows)
  expect_identical(nrow(orh$x), 24000L + 3000L + 6000L)
  expect_identical(ii, 21000L)
  expect_identical(orh$x$remaining_epochs[ii], 1981)
})

test_that("P3d: two long gaps in one block and a long gap with a fractional resume second", {
  skip_unless_ggir()
  sfh <- 10
  th <- as.numeric(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"))
  seg <- function(t0, minutes, v) data.frame(time = t0 + (0:(minutes * 60 * sfh - 1))/sfh, x = v[1], y = v[2], z = v[3])
  x <- rbind(seg(th, 20, c(0.1, 0.2, 1.1)),
             seg(th + 120 * 60, 10, c(0.3, 0.1, 0.9)),
             seg(th + 300 * 60 + 7.3, 10, c(0.2, 0.2, 0.95)))
  o <- check_pair(x, sfh, "two long gaps", epochsize = c(5, 900))
  expect_identical(o$qclog$timegaps_n, 2L)
  expect_identical(sum(o$x$remaining_epochs != 1), 2L)
  # the second gap's 7.3 s remainder moves its marker next_epoch_delay rows back
  expect_identical(nrow(o$x), 33073L)
  expect_identical(which(o$x$remaining_epochs != 1), c(18000L, 27070L))
  expect_identical(o$x$remaining_epochs[c(18000, 27070)], c(1081, 1981))
  x2 <- rbind(seg(th, 20, c(0.1, 0.2, 1.1)),
              seg(th + 120 * 60, 10, c(0.3, 0.1, 0.9)),
              seg(th + 130 * 60 + 2, 10, c(0.3, 0.1, 0.9)),
              seg(th + 300 * 60, 10, c(0.2, 0.2, 0.95)))
  o2 <- check_pair(x2, sfh, "short gap between long gaps", epochsize = c(5, 900))
  expect_identical(o2$qclog$timegaps_n, 3L)
  expect_identical(sum(o2$x$remaining_epochs != 1), 2L)
})

test_that("P3e: EE blocks reproduce the stored 8-row M$QClog and match GGIR block by block (long running)", {
  skip_unless_ref(ee_file, ee_meta)
  skip_unless_ggir()
  if (!run_long) skip("long-running test; set CANHRACTI_LONG_TESTS=true to run the EE 7-day file")
  e <- new.env()
  load(ee_meta, envir = e)
  stored <- e$M$QClog
  tz <- e$desiredtz_part1
  expect_identical(tz, "Europe/Helsinki")

  qlog <- NULL
  plv <- c(0, 0, 1)
  plt <- NULL
  prev_end <- c()
  i <- 1
  repeat {
    b <- read_gt3x_block(ee_file, i, prev_end, tz, plv = plv, plt = plt)
    if (length(b$data) == 0) break
    r <- run_both(b$data, sf = 100, k = 0.25, plv = plv, plt = plt, epochsize = c(5, 900))
    expect_same_as_ggir(r$ours$x, r$ggir$x, paste("EE block", i, "data.frame"))
    expect_same_as_ggir(r$ours$qclog, r$ggir$QClog, paste("EE block", i, "qclog"))
    qlog <- rbind(qlog, r$ours$qclog)
    plv <- r$ours$x[nrow(r$ours$x), c("x", "y", "z")]
    plt <- as.POSIXct(r$ours$x$time[nrow(r$ours$x)], origin = "1970-1-1")
    prev_end <- b$endpage
    is_last <- isTRUE(b$is_last)
    rm(r, b)
    gc()
    if (is_last) break
    i <- i + 1
  }
  expect_identical(nrow(qlog), 8L)
  expect_identical(qlog$blockLengthSeconds, c(86401, 86400, 86400, 86400, 86400, 86401, 86400, 2042))
  expect_identical(qlog$timegaps_n, c(1L, 0L, 0L, 0L, 0L, 1L, 0L, 0L))
  expect_identical(qlog$timegaps_min[c(1, 6)], rep(101/100/60, 2))
  msg <- first_difference(qlog, stored)
  expect(msg == "", paste("EE qclog is not identical() to stored M$QClog;", msg))
})
