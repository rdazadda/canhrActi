# The block-by-block path restates the steps of agcounts 0.7.0 (.resample, .bpf_filter,
# .resample_10hz and .sum_counts, https://github.com/bhelsel/agcounts) so they can carry
# their state from one block to the next. agcounts is Copyright (c) 2024 University of
# Kansas, licensed under the MIT licence; a copy is at inst/LICENSE.agcounts.

#' Activity Counts from a Raw .gt3x File
#'
#' Compute activity counts from a raw \code{.gt3x} accelerometer file. Requires
#' the \pkg{agcounts} and \pkg{read.gt3x} packages.
#'
#' @param path Path to a \code{.gt3x} file.
#' @param epoch Epoch length in seconds (default 60).
#' @param lfe Use the low-frequency extension filter (default \code{FALSE}).
#' @param tz Time zone for the timestamps (default \code{"UTC"}).
#'
#' @return Data frame with \code{time}, \code{axis1}, \code{axis2}, \code{axis3}
#'   and \code{vm}, one row per epoch.
#'
#' @references
#' Neishabouri A, et al. (2022). \emph{Scientific Reports}, 12:11958.
#'
#' @export
gt3x.counts <- function(path, epoch = 60, lfe = FALSE, tz = "UTC") {
  if (!requireNamespace("agcounts", quietly = TRUE)) {
    stop("gt3x.counts() requires the 'agcounts' package: install.packages('agcounts')")
  }
  if (!requireNamespace("read.gt3x", quietly = TRUE)) {
    stop("gt3x.counts() requires the 'read.gt3x' package")
  }
  ns   <- asNamespace("agcounts")
  need <- c(".resample", ".bpf_filter", ".resample_10hz", ".sum_counts")
  miss <- need[!vapply(need, exists, logical(1), envir = ns, inherits = FALSE)]
  if (length(miss)) {
    stop("Unsupported 'agcounts' version (missing: ", paste(miss, collapse = ", "), ")")
  }

  # a file the stream reader takes is counted an hour at a time, anything else whole
  s <- tryCatch(.gt3x.counts.stream(path, epoch, lfe), error = function(e) NULL)
  if (is.null(s)) {
    # Read X/Y/Z directly (skip agread's data.frame coercion + full POSIXct build).
    s <- .gt3x.counts.whole(read.gt3x::read.gt3x(path, asDataFrame = FALSE, imputeZeroes = TRUE),
                            epoch, lfe)
  }
  .gt3x.counts.frame(s, epoch, tz)
}

#' Counts per Epoch From the Whole Signal at Once
#'
#' @param mat X, Y, Z matrix as read.gt3x returns it with imputeZeroes = TRUE, with its
#'   sample_rate, start_time and time_index attributes.
#' @return list(t0, ec): the start in seconds and a data.frame of X, Y, Z counts.
#' @keywords internal
#' @noRd
.gt3x.counts.whole <- function(mat, epoch, lfe) {
  fn <- function(x) get(x, envir = asNamespace("agcounts"), inherits = FALSE)
  freq  <- attr(mat, "sample_rate")
  t0    <- as.numeric(attr(mat, "start_time")) + attr(mat, "time_index")[1] / 100
  m   <- unclass(mat)
  raw <- data.frame(X = m[, "X"], Y = m[, "Y"], Z = m[, "Z"])
  rm(m, mat)

  # Idle-sleep = all-zero rows (literal zeros from imputeZeroes): carry the last
  # value forward over each gap; a leading gap becomes 0.
  is_sleep <- raw$X == 0 & raw$Y == 0 & raw$Z == 0
  if (any(is_sleep)) {
    d      <- diff(c(FALSE, is_sleep, FALSE))
    starts <- which(d == 1L)
    ends   <- which(d == -1L) - 1L
    for (ax in c("X", "Y", "Z")) {
      v <- raw[[ax]]
      for (k in seq_along(starts)) {
        s <- starts[k]
        v[s:ends[k]] <- if (s == 1L) 0 else v[s - 1L]
      }
      raw[[ax]] <- v
    }
  }

  # Resample to 30 Hz (no-op at 30 Hz; stock resampler for other rates).
  if (isTRUE(freq == 30)) {
    ds <- t(as.matrix(raw[c("X", "Y", "Z")]))
    rownames(ds) <- c("X", "Y", "Z")
  } else {
    ds <- fn(".resample")(raw, freq)
  }
  rm(raw)

  bp <- fn(".bpf_filter")(ds)
  rm(ds)

  # Trim: rectify, dead-band, clip to 128, floor (+ LFE -1 correction).
  mn <- if (lfe) 1 else 4
  a  <- abs(bp)
  rm(bp)
  a[a < mn]  <- 0
  a[a > 128] <- 128
  if (lfe) { msk <- a < 4 & a >= mn; a[msk] <- a[msk] - 1 }
  tr <- floor(a)
  rm(a)

  r10 <- fn(".resample_10hz")(tr)
  rm(tr)
  ec  <- data.frame(t(fn(".sum_counts")(r10, epoch)))
  rm(r10)
  gc(FALSE)
  list(t0 = t0, ec = ec)
}

#' The Counts Data Frame From the Start and the Per-Epoch Counts
#' @keywords internal
#' @noRd
.gt3x.counts.frame <- function(s, epoch, tz) {
  start <- .floor_epoch(as.POSIXct(s$t0, origin = "1970-01-01", tz = tz), epoch, tz)
  ec <- s$ec
  data.frame(
    time  = seq(start, by = epoch, length.out = nrow(ec)),
    axis1 = ec$Y, axis2 = ec$X, axis3 = ec$Z,   # A1=Y, A2=X, A3=Z
    vm    = round(sqrt(ec$Y^2 + ec$X^2 + ec$Z^2))
  )
}

# Floor a POSIXct down to the epoch boundary.
.floor_epoch <- function(t, epoch, tz) {
  as.POSIXct(floor(as.numeric(t) / epoch) * epoch, origin = "1970-01-01", tz = tz)
}

# Self-test results for the session, by sample rate and filter.
.gt3x.counts.env <- new.env(parent = emptyenv())

#' Counts From the Stream Reader, Block by Block, or NULL When It Declines the File
#'
#' The signal read.gt3x builds with imputeZeroes = TRUE has one row per sample from the
#' start to the last sample time, zero where no record was written. Here each block of
#' seconds is built from the records that fall in it, so memory does not grow with the
#' recording, and the counts are identical() to .gt3x.counts.whole on the whole signal.
#' @param chunk_sec Block length in seconds, a multiple of the epoch; NULL for about an hour.
#' @return list(t0, ec) as .gt3x.counts.whole, or NULL.
#' @keywords internal
#' @noRd
.gt3x.counts.stream <- function(path, epoch, lfe, chunk_sec = NULL) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) return(NULL)
  if (!isTRUE(lfe) && !isFALSE(lfe)) return(NULL)
  if (!is.numeric(epoch) || length(epoch) != 1 || !is.finite(epoch) || epoch < 1 || epoch %% 1 != 0) {
    return(NULL)
  }
  if (is.null(chunk_sec)) chunk_sec <- epoch * max(2, ceiling(3600 / epoch))
  if (chunk_sec %% epoch != 0 || chunk_sec < 2 * epoch) return(NULL)
  # checked against agcounts 0.7.0 only
  if (!isTRUE(utils::packageVersion("agcounts") == "0.7.0")) return(NULL)
  p <- gsub("\\\\", "/", path)
  if (!dir.exists(p)) {
    if (!grepl("\\.gt3x$", p, ignore.case = TRUE) || !file.exists(p)) return(NULL)
    own <- .raw.gt3x.stage.owner(p, list(stream_gt3x = TRUE))
    if (!is.null(own)) on.exit(try(.raw.gt3x.extract.cleanup(own$path, exdir = own$dir), silent = TRUE), add = TRUE)
    p <- .raw.gt3x.extract(p)
  }
  w <- .raw.gt3x.stream.index(p)
  if (is.null(w) || w$max_samples %% w$sf != 0) return(NULL)
  n_sec <- w$max_samples %/% w$sf
  if (n_sec < 2 * chunk_sec || !.gt3x.counts.selftest(w$sf, lfe)) return(NULL)
  list(t0 = w$start, ec = .gt3x.counts.chunked(.gt3x.counts.source(w), n_sec, w$sf, epoch, lfe, chunk_sec))
}

#' Rows From s0 to s1 Seconds of the Zero-Imputed Signal, Read From the Records in Them
#' @keywords internal
#' @noRd
.gt3x.counts.source <- function(w) {
  rel <- w$ts - w$start
  sf <- w$sf
  function(s0, s1) {
    x <- matrix(0, (s1 - s0) * sf, 3)
    lo <- findInterval(s0, rel, left.open = TRUE) + 1L
    hi <- findInterval(s1, rel, left.open = TRUE)
    if (hi >= lo) {
      i <- lo:hi
      b <- gt3x_log_block_cpp(w$logbin, w$off[i], w$ts[i], sf, w$scale, w$start)
      if (!isTRUE(b$ok) || length(b$X) != length(i) * sf) stop("gt3x records could not be read")
      r <- rep((rel[i] - s0) * sf, each = sf) + seq_len(sf)
      x[r, 1] <- b$X
      x[r, 2] <- b$Y
      x[r, 3] <- b$Z
    }
    x
  }
}

#' Counts per Epoch, Block by Block
#'
#' Blocks start on whole multiples of chunk_sec, so every block holds whole seconds and
#' whole epochs; the last block runs to the end. Across a block edge the idle-sleep carry,
#' the resampler's recursion and the band-pass filter's state go on where they stopped.
#' @param src function(s0, s1) returning the X, Y, Z rows of seconds s0 to s1.
#' @param n_sec Length of the signal in seconds.
#' @return A data.frame of X, Y, Z counts, as .gt3x.counts.whole's ec.
#' @keywords internal
#' @noRd
.gt3x.counts.chunked <- function(src, n_sec, freq, epoch, lfe, chunk_sec) {
  ag <- asNamespace("agcounts")
  ca <- as.numeric(ag$.coefficients$output_coefficients)
  cb <- as.numeric(ag$.coefficients$input_coefficients)
  zi <- gsignal::filter_zi(filt = cb, a = ca)
  if (freq != 30) {
    f <- ag$.factors(freq)
    L <- f$upsample_factor
    M <- f$downsample_factor
    a_fp <- pi / (pi + 2 * L)
    b_fp <- (pi - 2 * L) / (pi + 2 * L)
  }
  mn <- if (lfe) 1 else 4
  cuts <- seq(0, n_sec, by = chunk_sec)
  cuts <- c(cuts[cuts <= n_sec - chunk_sec], n_sec)
  carry <- c(0, 0, 0)
  up <- c(0, 0, 0)
  zf <- vector("list", 3)
  n_idx <- NULL
  out <- rep(list(vector("list", 3)), length(cuts) - 1)

  for (k in seq_along(out)) {
    x <- src(cuts[k], cuts[k + 1])
    # all-zero rows take the last row that was not, from this block or before
    z <- x[, 1] == 0 & x[, 2] == 0 & x[, 3] == 0
    if (any(z)) {
      prev <- cummax(seq_len(nrow(x)) * !z)
      h <- z & prev > 0
      x[h, ] <- x[prev[h], ]
      h <- z & prev == 0
      x[h, ] <- rep(carry, each = sum(h))
    }
    carry <- x[nrow(x), ]
    if (freq != 30 && !identical(nrow(x), n_idx)) {
      n_idx <- nrow(x)
      nu <- n_idx * L
      # positions in upsampleC's input, whose first sample is the state
      i1 <- 1 + seq(1, nu, L)
      i2 <- 1 + seq(2, nu, L)
      idn <- 1 + seq(1, nu, M)
    }

    for (r in 1:3) {
      v <- x[, r]
      if (freq != 30) {
        if (!freq %in% c(60, 90)) {
          # .resample puts each value in every L-th sample and adds the sample before it, so a
          # value and the sample after it hold (a_fp * L) times it; the sample before a block is 0
          s <- (a_fp * L) * v
          u <- numeric(nu + 1)
          u[1] <- up[r]
          u[i1] <- s
          u[i2] <- s
          dim(u) <- c(1L, nu + 1)
          y <- ag$upsampleC(u, b_fp)
          up[r] <- y[nu + 1]
          v <- y[idn]
          rm(s, u, y)
        } else {
          v <- v[idn - 1]
        }
        v <- round(v, 3)
      }
      # the first block starts the filter as .bpf_filter does; later blocks go on from zf
      fr <- gsignal::filter(filt = cb, a = ca, x = v, zi = if (k == 1) zi * v[1] else zf[[r]])
      zf[[r]] <- fr$zf
      a <- abs(((3 / 4096) / (2.6 / 256) * 237.5) * fr$y)
      rm(fr, v)
      a[a < mn]  <- 0
      a[a > 128] <- 128
      if (lfe) { msk <- a < 4 & a >= mn; a[msk] <- a[msk] - 1 }
      # 10 Hz and epoch sums of whole numbers are exact, as in .resample_10hz and .sum_counts
      g <- floor(.colSums(floor(a), 3, length(a) %/% 3) / 3)
      rm(a)
      ne <- length(g) %/% (10 * epoch)
      out[[k]][[r]] <- .colSums(g[seq_len(ne * 10 * epoch)], 10 * epoch, ne)
    }
  }
  axis <- function(r) unlist(lapply(out, `[[`, r))
  data.frame(X = axis(1), Y = axis(2), Z = axis(3))
}

#' Whether the Block Path Matches the Whole-Signal Path Here, at This Rate and Filter
#'
#' Runs both on a two-minute signal with idle-sleep runs across block edges, once per
#' session; a gsignal or agcounts that computes differently turns the block path off.
#' @keywords internal
#' @noRd
.gt3x.counts.selftest <- function(freq, lfe) {
  key <- paste(freq, lfe)
  hit <- .gt3x.counts.env[[key]]
  if (!is.null(hit)) return(hit)
  ok <- tryCatch({
    n <- 120 * freq
    tt <- seq_len(n) / freq
    x <- round(cbind(X = sin(tt * 5.1) * 0.8, Y = cos(tt * 2.3) * 1.1 - 0.5, Z = sin(tt * 0.7 + 1)), 3)
    x[c(1:(3 * freq), (20 * freq):(37 * freq + 5), (70 * freq - 3):(71 * freq)), ] <- 0
    m <- structure(x, sample_rate = freq, start_time = 0, time_index = 0)
    whole <- .gt3x.counts.whole(m, 1, lfe)$ec
    part <- .gt3x.counts.chunked(function(s0, s1) x[(s0 * freq + 1):(s1 * freq), , drop = FALSE],
                                 120, freq, 1, lfe, 17)
    # A second run starting on a non-zero sample, with a partial last epoch, sees
    # changes to the filter's starting state and to how the last epoch is dropped.
    x2 <- x[(3 * freq + 1):n, , drop = FALSE]
    m2 <- structure(x2, sample_rate = freq, start_time = 0, time_index = 0)
    whole2 <- .gt3x.counts.whole(m2, 7, lfe)$ec
    part2 <- .gt3x.counts.chunked(function(s0, s1) x2[(s0 * freq + 1):(s1 * freq), , drop = FALSE],
                                  117, freq, 7, lfe, 14)
    identical(part, whole) && sum(whole) > 0 && identical(part2, whole2) && sum(whole2) > 0
  }, error = function(e) FALSE)
  .gt3x.counts.env[[key]] <- ok
  ok
}
