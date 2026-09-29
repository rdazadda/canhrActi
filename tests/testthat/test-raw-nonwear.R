# .raw.nonwear.clipping against GGIR's detect_nonwear_clipping. Reference data come from
# CANHRACTI_GGIR_REF; tests skip when a file is missing and live comparisons when GGIR is
# not installed. GGIR wrote the reference outputs in an America/Anchorage session, so the
# file runs in that zone.

withr::local_timezone("America/Anchorage")

ggir_ref_dir <- function() {
  ref <- Sys.getenv("CANHRACTI_GGIR_REF", unset = "")
  if (!nzchar(ref)) {
    testthat::skip("CANHRACTI_GGIR_REF is not set; GGIR reference data unavailable")
  }
  if (!dir.exists(ref)) {
    testthat::skip(paste0("CANHRACTI_GGIR_REF does not exist: ", ref))
  }
  ref
}

ggir_ref_file <- function(...) {
  f <- file.path(ggir_ref_dir(), ...)
  if (!file.exists(f)) testthat::skip(paste0("Reference file missing: ", f))
  f
}

skip_if_no_ggir <- function() {
  testthat::skip_if_not_installed("GGIR")
}

# Synthetic chunk at ws2 = 900 s: epochs 1..2 moving, 3..6 still, 7 x still with y and z
# moving, 8..11 still, 12 moving, then `extra` trailing moving rows short of a long epoch.
# Planted clipping in the moving epochs only: epoch 1 z 50 and x 30 samples at 8 g, epoch 2
# x 100 samples at 8 g, epoch 12 y one sample at 12 g (over 1.5 * 7.5, so the whole epoch
# counts).
make_nw_chunk <- function(sf, extra = 10000, seed = 1) {
  set.seed(seed)
  n_ep <- 12
  m <- 900 * sf
  n <- n_ep * m + extra
  still <- c(0.01, 0.02, 0.98)
  moving <- function(k) cbind(rnorm(k, 0.1, 0.2), rnorm(k, -0.2, 0.2), rnorm(k, 0.9, 0.2))
  quiet <- function(k) cbind(rnorm(k, still[1], 0.002), rnorm(k, still[2], 0.002), rnorm(k, still[3], 0.002))
  d <- matrix(0, n, 3)
  rows <- function(e) ((e - 1) * m + 1):(e * m)
  for (e in 1:2) d[rows(e), ] <- moving(m)
  for (e in 3:6) d[rows(e), ] <- quiet(m)
  d[rows(7), ] <- cbind(rnorm(m, still[1], 0.002), rnorm(m, -0.2, 0.2), rnorm(m, 0.9, 0.2))
  for (e in 8:11) d[rows(e), ] <- quiet(m)
  d[rows(12), ] <- moving(m)
  if (extra > 0) d[(n_ep * m + 1):n, ] <- moving(extra)
  # planted clipping
  d[rows(2)[1:100], 1] <- 8
  d[rows(1)[201:250], 3] <- 8
  d[rows(1)[301:330], 1] <- 8
  d[rows(12)[5000], 2] <- 12
  colnames(d) <- c("x", "y", "z")
  d
}

# Second layout, for the edge skip: epoch 1 x still with y and z moving, 2..5 still, 6 moving.
make_nw_chunk_edge <- function(sf, seed = 2) {
  set.seed(seed)
  m <- 900 * sf
  n <- 6 * m
  still <- c(0.01, 0.02, 0.98)
  d <- matrix(0, n, 3)
  rows <- function(e) ((e - 1) * m + 1):(e * m)
  d[rows(1), ] <- cbind(rnorm(m, still[1], 0.002), rnorm(m, -0.2, 0.2), rnorm(m, 0.9, 0.2))
  for (e in 2:5) d[rows(e), ] <- cbind(rnorm(m, still[1], 0.002), rnorm(m, still[2], 0.002), rnorm(m, still[3], 0.002))
  d[rows(6), ] <- cbind(rnorm(m, 0.1, 0.2), rnorm(m, -0.2, 0.2), rnorm(m, 0.9, 0.2))
  colnames(d) <- c("x", "y", "z")
  d
}

ws_default <- c(5, 900, 3600)

test_that("synthetic 30 Hz chunk: 2023 approach matches GGIR and the planted layout", {
  skip_if_no_ggir()
  sf <- 30
  d <- make_nw_chunk(sf)
  expect_equal(nrow(d), 12 * 27000 + 10000)
  ours <- .raw.nonwear.clipping(d, ws_default, sf, clipthres = 7.5, sdcriter = 0.013,
                                racriter = 0.15, approach = "2023")
  ref <- GGIR:::detect_nonwear_clipping(data = d, windowsizes = ws_default, sf = sf,
                                        clipthres = 7.5, sdcriter = 0.013, racriter = 0.15,
                                        nonwear_approach = "2023")
  expect_identical(ours$nonwear, ref$NWav)
  expect_identical(ours$clipping, ref$CWav)
  expect_identical(ours$nmin, ref$nmin)
  expect_identical(ours$nmin, 12)
  # the planted layout: epoch 7 is a lone 1 between two 3s and becomes 2
  expect_identical(ours$nonwear, c(0, 0, 3, 3, 3, 3, 2, 3, 3, 3, 3, 0))
  # clipping fractions are k / (ws2 * sf), unrounded; 12 g in epoch 12 clips the whole epoch
  expect_identical(ours$clipping,
                   c(50 / 27000, 100 / 27000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
})

test_that("synthetic 30 Hz chunk: 2013 approach matches GGIR and the centred-window rule", {
  skip_if_no_ggir()
  sf <- 30
  d <- make_nw_chunk(sf)
  ours <- .raw.nonwear.clipping(d, ws_default, sf, 7.5, 0.013, 0.15, approach = "2013")
  ref <- GGIR:::detect_nonwear_clipping(data = d, windowsizes = ws_default, sf = sf,
                                        clipthres = 7.5, sdcriter = 0.013, racriter = 0.15,
                                        nonwear_approach = "2013")
  expect_identical(ours$nonwear, ref$NWav)
  expect_identical(ours$clipping, ref$CWav)
  expect_identical(ours$nmin, ref$nmin)
  # the centred windows h = 5..8 see x still but y and z moving in epoch 7; the others reach
  # a moving epoch
  expect_identical(ours$nonwear, c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0))
  expect_identical(ours$clipping,
                   c(50 / 27000, 100 / 27000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
})

test_that("synthetic 100 Hz chunk: both approaches match GGIR (stride 20)", {
  skip_if_no_ggir()
  sf <- 100
  d <- make_nw_chunk(sf, extra = 7000, seed = 3)
  expect_equal(nrow(d), 12 * 90000 + 7000)
  for (ap in c("2023", "2013")) {
    ours <- .raw.nonwear.clipping(d, ws_default, sf, 7.5, 0.013, 0.15, approach = ap)
    ref <- GGIR:::detect_nonwear_clipping(data = d, windowsizes = ws_default, sf = sf,
                                          clipthres = 7.5, sdcriter = 0.013, racriter = 0.15,
                                          nonwear_approach = ap)
    expect_identical(ours$nonwear, ref$NWav, info = ap)
    expect_identical(ours$clipping, ref$CWav, info = ap)
    expect_identical(ours$nmin, 12, info = ap)
  }
  ours23 <- .raw.nonwear.clipping(d, ws_default, sf, 7.5, 0.013, 0.15, approach = "2023")
  expect_identical(ours23$nonwear, c(0, 0, 3, 3, 3, 3, 2, 3, 3, 3, 3, 0))
  expect_identical(ours23$clipping,
                   c(50 / 90000, 100 / 90000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
})

test_that("a lone 1 at the first epoch is left alone (edge skip), both approaches match GGIR", {
  skip_if_no_ggir()
  sf <- 30
  d <- make_nw_chunk_edge(sf)
  ours <- .raw.nonwear.clipping(d, ws_default, sf, 7.5, 0.013, 0.15, approach = "2023")
  ref <- GGIR:::detect_nonwear_clipping(data = d, windowsizes = ws_default, sf = sf,
                                        clipthres = 7.5, sdcriter = 0.013, racriter = 0.15,
                                        nonwear_approach = "2023")
  expect_identical(ours$nonwear, ref$NWav)
  expect_identical(ours$nonwear, c(1, 3, 3, 3, 3, 0))
  expect_identical(ours$clipping, rep(0, 6))
  ours13 <- .raw.nonwear.clipping(d, ws_default, sf, 7.5, 0.013, 0.15, approach = "2013")
  ref13 <- GGIR:::detect_nonwear_clipping(data = d, windowsizes = ws_default, sf = sf,
                                          clipthres = 7.5, sdcriter = 0.013, racriter = 0.15,
                                          nonwear_approach = "2013")
  expect_identical(ours13$nonwear, ref13$NWav)
  expect_identical(ours13$nonwear, c(1, 1, 1, 0, 0, 0))
})

test_that("data.frame input, a wear column and an unknown approach behave as in GGIR", {
  skip_if_no_ggir()
  sf <- 30
  d <- make_nw_chunk_edge(sf)
  df <- as.data.frame(d)
  df$time <- seq_len(nrow(df))
  ours <- .raw.nonwear.clipping(df, ws_default, sf, 7.5, 0.013, 0.15, approach = "2023")
  ref <- GGIR:::detect_nonwear_clipping(data = df, windowsizes = ws_default, sf = sf,
                                        clipthres = 7.5, sdcriter = 0.013, racriter = 0.15,
                                        nonwear_approach = "2023")
  expect_identical(ours$nonwear, ref$NWav)
  expect_identical(ours$nonwear, c(1, 3, 3, 3, 3, 0))
  # wear column: the score is 3 times the majority wear value; the switch sits one row past
  # the epoch 3 boundary so window h = 2 is a strict majority of 0
  df$wear <- as.numeric(seq_len(nrow(df)) > 3 * 27000 + 1)
  ours_w <- .raw.nonwear.clipping(df, ws_default, sf, 7.5, 0.013, 0.15, approach = "2023")
  ref_w <- GGIR:::detect_nonwear_clipping(data = df, windowsizes = ws_default, sf = sf,
                                          clipthres = 7.5, sdcriter = 0.013, racriter = 0.15,
                                          nonwear_approach = "2023")
  expect_identical(ours_w$nonwear, ref_w$NWav)
  expect_identical(ours_w$nonwear, c(0, 0, 3, 3, 3, 3))
  # unknown approach: no loop runs, all scores stay zero
  ours_u <- .raw.nonwear.clipping(d, ws_default, sf, 7.5, 0.013, 0.15, approach = "1999")
  ref_u <- GGIR:::detect_nonwear_clipping(data = d, windowsizes = ws_default, sf = sf,
                                          clipthres = 7.5, sdcriter = 0.013, racriter = 0.15,
                                          nonwear_approach = "1999")
  expect_identical(ours_u$nonwear, ref_u$NWav)
  expect_identical(ours_u$nonwear, rep(0, 6))
  expect_identical(ours_u$clipping, rep(0, 6))
  expect_identical(ours_u$nmin, 6)
})

test_that("stored MOS2 milestone: whole-file non-wear and clipping distribution (T1)", {
  rdata <- ggir_ref_file("out", "output_din", "meta", "basic", "meta_MOS2E39230594.gt3x.RData")
  e <- new.env()
  load(rdata, envir = e)
  nw <- e$M$metalong$nonwearscore
  cs <- e$M$metalong$clippingscore
  expect_identical(length(nw), 660L)
  expect_identical(sum(nw == 0), 308L)
  expect_identical(sum(nw == 1), 9L)
  expect_identical(sum(nw == 2), 0L)
  expect_identical(sum(nw == 3), 343L)
  nz <- which(cs != 0)
  expect_identical(nz, c(45L, 85L, 86L, 87L, 88L, 236L, 237L, 262L, 271L, 294L))
  # the stored values are k/27000 after g.getmeta's character round trip
  k <- round(cs[nz] * 27000)
  expect_identical(k, c(1, 5, 2, 1, 1, 2, 2, 1, 1, 3))
  expect_identical(cs[nz], as.numeric(as.character(k / 27000)))
})

test_that("MOS2 chunk 1 replication: nonwearscore identical to stored [1:186] and clipping k/27000 (T1, T2)", {
  skip_if_no_ggir()
  f <- ggir_ref_file("din", "MOS2E39230594.gt3x")
  rdata <- ggir_ref_file("out", "output_din", "meta", "basic", "meta_MOS2E39230594.gt3x.RData")
  e <- new.env()
  load(rdata, envir = e)
  C <- e$C
  M <- e$M
  tz <- "America/Anchorage"
  sf <- 30; ws3 <- 5; ws2 <- 900; ws <- 3600

  # block 1 read and imputed with GGIR, trimmed, cut to whole 15 min epochs and calibrated,
  # then scored with both functions
  params <- GGIR:::extract_params(params2check = c("metrics", "rawdata", "general", "cleaning"))
  I <- suppressWarnings(GGIR::g.inspectfile(f, desiredtz = tz,
                                            params_rawdata = params$params_rawdata,
                                            configtz = c()))
  expect_identical(I$sf, 30)
  ncb <- GGIR:::get_nw_clip_block_params(monc = I$monc, dformat = I$dformc,
                                         deviceSerialNumber = GGIR:::g.extractheadervars(I)$deviceSerialNumber,
                                         sf = sf, params_rawdata = params$params_rawdata)
  expect_identical(ncb$blocksize, 86400)
  expect_identical(ncb$clipthres, 7.5)
  expect_identical(ncb$sdcriter, 0.013)
  expect_identical(ncb$racriter, 0.15)
  fq <- data.frame(filetooshort = FALSE, filecorrupt = FALSE, filedoesnotholdday = FALSE,
                   NFilePagesSkipped = 0)
  acc <- suppressWarnings(GGIR:::g.readaccfile(filename = f, blocksize = ncb$blocksize, blocknumber = 1,
                                               filequality = fq, ws = ws, PreviousEndPage = c(),
                                               inspectfileobject = I, PreviousLastValue = c(0, 0, 1),
                                               PreviousLastTime = NULL,
                                               params_rawdata = params$params_rawdata,
                                               params_general = params$params_general, header = NULL))
  d <- acc$P$data
  expect_identical(nrow(d), 2592000L)
  P <- GGIR:::g.imputeTimegaps(d, sf = sf, k = 0.25, PreviousLastValue = c(0, 0, 1),
                               PreviousLastTime = NULL, epochsize = c(ws3, ws2))
  x <- P$x
  expect_identical(nrow(x), 5036460L)
  xm <- as.matrix(x, rownames.force = FALSE)
  expect_identical(typeof(xm), "double")
  SW <- GGIR:::get_starttime_weekday_truncdata(I$monc, I$dformc, xm, NULL, desiredtz = tz, sf, f, ws2,
                                               configtz = NULL)
  xm <- SW$data
  expect_identical(nrow(xm), 5032980L)
  expect_identical(unname(format(SW$starttime, "%Y-%m-%d %H:%M:%S")), "2025-10-07 20:30:00")
  LD <- nrow(xm)
  use <- floor(LD / (ws2 * sf)) * (ws2 * sf)
  expect_identical(use, 5022000)
  xm <- xm[1:use, ]
  if ("remaining_epochs" %in% colnames(xm)) xm <- xm[, colnames(xm) != "remaining_epochs"]
  # scale(), as g.getmeta does, so the calibrated values are bit-identical
  xm[, c("x", "y", "z")] <- scale(xm[, c("x", "y", "z")], center = -C$offset, scale = 1 / C$scale)

  ours <- .raw.nonwear.clipping(xm, c(ws3, ws2, ws), sf, clipthres = ncb$clipthres,
                                sdcriter = ncb$sdcriter, racriter = ncb$racriter, approach = "2023")
  expect_identical(ours$nmin, 186)
  expect_identical(length(ours$nonwear), 186L)
  stored_nw <- M$metalong$nonwearscore[1:186]
  if (!identical(ours$nonwear, stored_nw)) {
    i <- which(ours$nonwear != stored_nw)[1]
    fail(sprintf("nonwear differs from stored at epoch %d: ours %s, stored %s",
                 i, ours$nonwear[i], stored_nw[i]))
  }
  expect_identical(ours$nonwear, stored_nw)
  expect_identical(as.vector(table(ours$nonwear)), c(181L, 5L))
  expect_identical(names(table(ours$nonwear)), c("0", "3"))
  expect_identical(which(ours$nonwear == 3), 19:23)

  # ours is the unrounded k/27000; the stored metalong carries g.getmeta's character round trip
  k <- ours$clipping * 27000
  expect_identical(k, round(k))
  expect_identical(ours$clipping, round(k) / 27000)
  expect_identical(which(ours$clipping != 0), c(45L, 85L, 86L, 87L, 88L))
  expect_identical(round(k)[c(45, 85, 86, 87, 88)], c(1, 5, 2, 1, 1))
  expect_identical(as.numeric(as.character(ours$clipping)), M$metalong$clippingscore[1:186])

  # live GGIR on the same matrix
  ref <- GGIR:::detect_nonwear_clipping(data = xm, windowsizes = c(ws3, ws2, ws), sf = sf,
                                        clipthres = ncb$clipthres, sdcriter = ncb$sdcriter,
                                        racriter = ncb$racriter, nonwear_approach = "2023",
                                        params_rawdata = params$params_rawdata)
  expect_identical(ours$nonwear, ref$NWav)
  expect_identical(ours$clipping, ref$CWav)
  expect_identical(ours$nmin, ref$nmin)

  # "2013" differs from "2023" in exactly 4 of the 186 chunk-1 epochs
  ours13 <- .raw.nonwear.clipping(xm, c(ws3, ws2, ws), sf, clipthres = ncb$clipthres,
                                  sdcriter = ncb$sdcriter, racriter = ncb$racriter, approach = "2013")
  ref13 <- GGIR:::detect_nonwear_clipping(data = xm, windowsizes = c(ws3, ws2, ws), sf = sf,
                                          clipthres = ncb$clipthres, sdcriter = ncb$sdcriter,
                                          racriter = ncb$racriter, nonwear_approach = "2013",
                                          params_rawdata = params$params_rawdata)
  expect_identical(ours13$nonwear, ref13$NWav)
  expect_identical(ours13$clipping, ref13$CWav)
  expect_identical(sum(ours13$nonwear != ours$nonwear), 4L)
  expect_identical(as.vector(table(ours13$nonwear)), c(184L, 1L, 1L))
  expect_identical(names(table(ours13$nonwear)), c("0", "2", "3"))
})
