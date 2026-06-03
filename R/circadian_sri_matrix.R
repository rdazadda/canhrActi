#' Sleep Regularity Index (SRI) - Phillips (2017) Epoch-of-Day x Day Matrix
#'
#' Computes the Sleep Regularity Index exactly as defined by Phillips et al.
#' (2017) using the full epoch-of-day by day concordance matrix. This is the
#' canonical, published form of the SRI and is more robust than a single
#' fixed 24-hour-lag comparison: every epoch is binned by its real clock time
#' (epoch-of-day) and by calendar date, a binary sleep/wake matrix is built,
#' and the index is the average agreement between the SAME clock time on
#' CONSECUTIVE calendar days, rescaled to the interval \[-100, 100\].
#'
#' @details
#' Let `M = 86400 / epoch_length` be the number of epochs in a day and let `N`
#' be the number of calendar days spanned by the recording. Each epoch is
#' assigned an epoch-of-day index `i` (1..M) from its clock time
#' (hour/minute/second) and a day index `j` (1..N) from its calendar date.
#' These two indices populate a binary matrix `s[i, j]` where 1 = sleep and
#' 0 = wake. The SRI is then
#'
#' \deqn{SRI = -100 + \frac{200}{M (N - 1)} \sum_{i=1}^{M} \sum_{j=1}^{N-1}
#'   \mathbf{1}\{ s_{i,j} = s_{i,j+1} \}}
#'
#' i.e. the proportion of clock-time epochs that hold the same state on two
#' consecutive days, mapped linearly so that perfect regularity scores +100,
#' chance-level (independent) scoring ~0, and perfect anti-regularity -100.
#'
#' Robustness to gaps and partial wear: any consecutive-day pair
#' `(i, j) -> (i, j + 1)` in which either cell is `NA` (missing epoch,
#' non-wear, or a calendar day not represented in the data) is skipped and
#' NOT counted. The sum is divided by the actual number of valid pairs rather
#' than the theoretical `M * (N - 1)`, so missing data reduce statistical
#' power but do not bias the estimate.
#'
#' Days are aligned by true clock time using [as.POSIXlt()] (hour, minute,
#' second give the epoch-of-day; the calendar date gives the day index), so the
#' result is correct even when a recording does not start at midnight.
#'
#' Edge cases are handled gracefully: a `SRI` of `NA_real_` is returned (never
#' an error) when there are fewer than two days, no valid consecutive-day
#' pairs, or no usable input. The return structure is always the same list.
#'
#' @param sleep_state Sleep/wake state per epoch. Either a character vector of
#'   `"S"` (sleep) / `"W"` (wake), or a numeric/integer/logical vector where
#'   1/TRUE = sleep and 0/FALSE = wake. `NA` marks unscored / non-wear epochs
#'   and is excluded from the concordance count. Must be the same length as
#'   `timestamps`.
#' @param timestamps POSIXct vector of epoch start times, one per element of
#'   `sleep_state`. Need not start at midnight and need not be perfectly
#'   contiguous; alignment is by real clock time.
#' @param epoch_length Epoch length in seconds (default 60). Must divide 86400
#'   evenly; `M = 86400 / epoch_length` is the number of epochs per day.
#'
#' @return A list with components:
#'   \describe{
#'     \item{SRI}{Numeric Sleep Regularity Index in \[-100, 100\] (rounded to
#'       2 dp), or `NA_real_` if it cannot be computed.}
#'     \item{n_days}{Integer number of distinct calendar days (N) spanned.}
#'     \item{n_valid_pairs}{Integer number of consecutive-day epoch pairs that
#'       were actually compared (both cells non-NA).}
#'     \item{method}{Character constant `"phillips_matrix"`.}
#'   }
#'
#' @references
#' Phillips AJK, Clerx WM, O'Brien CS, Sano A, Barger LK, Picard RW,
#' Lockley SW, Klerman EB, Czeisler CA (2017). Irregular sleep/wake patterns
#' are associated with poorer academic performance and delayed circadian and
#' sleep/wake timing. \emph{Scientific Reports}, 7(1):3216.
#' \doi{10.1038/s41598-017-03171-4}
#'
#' @examples
#' # A perfectly regular sleeper: same 8h sleep block every day -> SRI ~ 100
#' ts <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
#'           by = 60, length.out = 1440 * 4)
#' tod <- as.POSIXlt(ts)$hour
#' state <- ifelse(tod < 8, "S", "W")   # asleep 00:00-08:00 every day
#' sri.matrix(state, ts, epoch_length = 60)$SRI
#'
#' @export
sri.matrix <- function(sleep_state, timestamps, epoch_length = 60) {

  # --- Result skeleton (always returned with the same structure) ------------
  na_result <- function(n_days = 0L, n_valid_pairs = 0L) {
    list(
      SRI = NA_real_,
      n_days = as.integer(n_days),
      n_valid_pairs = as.integer(n_valid_pairs),
      method = "phillips_matrix"
    )
  }

  # --- Input validation (never error on bad/edge input) ---------------------
  if (is.null(sleep_state) || is.null(timestamps)) {
    return(na_result())
  }
  n <- length(sleep_state)
  if (n == 0L || length(timestamps) != n) {
    return(na_result())
  }
  if (!inherits(timestamps, "POSIXct")) {
    timestamps <- tryCatch(as.POSIXct(timestamps),
                           error = function(e) NULL)
    if (is.null(timestamps)) return(na_result())
  }
  if (!is.numeric(epoch_length) || length(epoch_length) != 1L ||
      is.na(epoch_length) || epoch_length <= 0) {
    return(na_result())
  }

  # Epochs per day (M). Require it to divide the day evenly so epoch-of-day
  # indexing is well defined.
  M <- 86400 / epoch_length
  if (abs(M - round(M)) > 1e-8) {
    return(na_result())
  }
  M <- as.integer(round(M))

  # --- Coerce sleep_state to binary 1 = sleep, 0 = wake, NA = unscored ------
  state <- .sri.to.binary(sleep_state)

  # --- Epoch-of-day index i (1..M) and day index j (1..N) -------------------
  lt <- as.POSIXlt(timestamps)

  # Drop epochs whose timestamp is NA: they cannot be placed in the matrix.
  ts_ok <- !is.na(lt$hour)

  # Seconds since local midnight -> epoch-of-day (0-based floor, then +1).
  secs_of_day <- lt$hour * 3600 + lt$min * 60 + lt$sec
  i_idx <- floor(secs_of_day / epoch_length) + 1L
  i_idx[i_idx < 1L] <- 1L
  i_idx[i_idx > M] <- M  # guard rounding/leap-second edge to last bin

  # Calendar date string -> day index j. Use local date so DST/non-midnight
  # starts are handled by real clock time, not elapsed seconds.
  date_chr <- format(timestamps, "%Y-%m-%d")
  valid_row <- ts_ok & !is.na(date_chr)

  if (!any(valid_row)) {
    return(na_result())
  }

  # Map distinct dates to consecutive day indices preserving chronological
  # order. Note: gaps in the calendar (a missing day in the middle) become an
  # all-NA column, so the pairs spanning that gap are simply never counted -
  # exactly the desired behaviour.
  uniq_dates <- sort(unique(date_chr[valid_row]))
  N <- length(uniq_dates)

  if (N < 2L) {
    # Fewer than two days: no consecutive-day pair exists.
    return(na_result(n_days = N, n_valid_pairs = 0L))
  }

  # Build a full N-day calendar (including any internal missing days) so that
  # "consecutive" means consecutive calendar days, not consecutive present
  # days. A missing internal day yields an all-NA column and breaks the chain
  # of comparisons across it (no spurious concordance is credited).
  d0 <- as.Date(uniq_dates[1])
  d1 <- as.Date(uniq_dates[N])
  all_dates <- as.character(seq(d0, d1, by = "day"))
  N <- length(all_dates)
  date_to_j <- match(date_chr, all_dates)

  # --- Populate the M x N matrix --------------------------------------------
  # Initialise to NA; fill only valid (non-NA timestamp) epochs. If two epochs
  # ever map to the same (i, j) cell (e.g. duplicate timestamps), the later
  # assignment wins - acceptable and rare for clean epoch grids.
  s <- matrix(NA_real_, nrow = M, ncol = N)
  fill <- valid_row & !is.na(date_to_j)
  s[cbind(i_idx[fill], date_to_j[fill])] <- state[fill]

  # --- Concordance between consecutive days ---------------------------------
  left  <- s[, 1:(N - 1), drop = FALSE]   # day j
  right <- s[, 2:N,       drop = FALSE]   # day j + 1

  both_present <- !is.na(left) & !is.na(right)
  n_valid_pairs <- sum(both_present)

  if (n_valid_pairs == 0L) {
    return(na_result(n_days = N, n_valid_pairs = 0L))
  }

  agree <- sum(left[both_present] == right[both_present])

  sri <- -100 + (200 * agree) / n_valid_pairs

  list(
    SRI = round(sri, 2),
    n_days = as.integer(N),
    n_valid_pairs = as.integer(n_valid_pairs),
    method = "phillips_matrix"
  )
}


#' Coerce sleep/wake state to binary (1 = sleep, 0 = wake, NA = unscored)
#'
#' Internal helper for [sri.matrix()]. Accepts the common encodings used across
#' the package: character `"S"`/`"W"` (case-insensitive), numeric/integer
#' 1/0, or logical TRUE/FALSE. Anything not recognised as sleep or wake (and
#' any `NA`) becomes `NA_real_`.
#'
#' @param x Vector of sleep states (character, numeric, integer, or logical).
#'
#' @return Numeric vector the same length as `x` with values 1, 0, or
#'   `NA_real_`.
#'
#' @keywords internal
.sri.to.binary <- function(x) {

  n <- length(x)
  out <- rep(NA_real_, n)

  if (is.logical(x)) {
    out[!is.na(x) & x]  <- 1
    out[!is.na(x) & !x] <- 0
    return(out)
  }

  if (is.numeric(x)) {
    out[!is.na(x) & x == 1] <- 1
    out[!is.na(x) & x == 0] <- 0
    return(out)
  }

  # Character / factor: treat "S"/"s"/"sleep" as sleep, "W"/"w"/"wake" as wake.
  xc <- toupper(trimws(as.character(x)))
  out[xc == "S" | xc == "SLEEP" | xc == "1"] <- 1
  out[xc == "W" | xc == "WAKE"  | xc == "0"] <- 0
  out
}
