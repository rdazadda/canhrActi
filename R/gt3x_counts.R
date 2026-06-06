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
  fn <- function(x) get(x, envir = ns, inherits = FALSE)

  # Read X/Y/Z directly (skip agread's data.frame coercion + full POSIXct build).
  mat   <- read.gt3x::read.gt3x(path, asDataFrame = FALSE, imputeZeroes = TRUE)
  freq  <- attr(mat, "sample_rate")
  t0    <- as.numeric(attr(mat, "start_time")) + attr(mat, "time_index")[1] / 100
  start <- .floor_epoch(as.POSIXct(t0, origin = "1970-01-01", tz = tz), epoch, tz)
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
