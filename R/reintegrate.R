#' Reintegrate Epoch Data to a Coarser Epoch Length
#'
#' Aggregates a regular activity-count series (and its aligned timestamps and
#' optional wear flags) up to a longer epoch by summing consecutive epochs - the
#' standard ActiLife "reintegration". This is used to score sleep at 60 s when
#' the source epoch is shorter, because the Cole-Kripke and Sadeh algorithms are
#' defined and validated for 1-minute epochs.
#'
#' @param counts Numeric activity counts on a regular epoch grid.
#' @param timestamps A \code{POSIXct} vector aligned to \code{counts}; the first
#'   timestamp of each aggregated block is kept.
#' @param wear Optional logical wear vector aligned to \code{counts}. A block is
#'   marked worn when the majority of its sub-epochs are worn.
#' @param from_epoch Source epoch length in seconds.
#' @param to_epoch Target epoch length in seconds (default 60).
#'
#' @return A list with \code{counts}, \code{timestamps}, \code{wear}, and
#'   \code{epoch_length}. If \code{from_epoch} is missing/invalid, is already
#'   \code{>= to_epoch}, or \code{to_epoch} is not a whole multiple of
#'   \code{from_epoch}, the inputs are returned unchanged.
#'
#' @examples
#' x <- reintegrate.epochs(rep(10, 240),
#'                         as.POSIXct("2024-01-01", tz = "UTC") + (0:239) * 15,
#'                         from_epoch = 15, to_epoch = 60)
#' length(x$counts)   # 60 one-minute epochs from 240 15-second epochs
#'
#' @export
reintegrate.epochs <- function(counts, timestamps, wear = NULL,
                               from_epoch, to_epoch = 60) {
  if (is.null(from_epoch) || is.na(from_epoch) || from_epoch <= 0 ||
      from_epoch >= to_epoch || (to_epoch %% from_epoch) != 0) {
    return(list(counts = counts, timestamps = timestamps, wear = wear,
                epoch_length = from_epoch))
  }

  k <- as.integer(to_epoch / from_epoch)
  n <- length(counts)
  grp <- ((seq_len(n) - 1L) %/% k) + 1L

  new_counts <- as.numeric(tapply(as.numeric(counts), grp, sum, na.rm = TRUE))
  keep <- !duplicated(grp)
  new_ts <- timestamps[keep]

  new_wear <- NULL
  if (!is.null(wear) && length(wear) == n) {
    new_wear <- as.logical(tapply(as.logical(wear), grp,
                                  function(z) mean(z, na.rm = TRUE) >= 0.5))
  }

  list(counts = new_counts, timestamps = new_ts, wear = new_wear,
       epoch_length = to_epoch)
}


#' Prepare a counts series for 60s sleep scoring
#'
#' Internal helper: reintegrates sub-minute counts/timestamps to 60s for scoring
#' and returns an \code{upsample} index that maps each NATIVE epoch back to its
#' 60s block, so a 60s-scored state vector can be expanded to align with the
#' original native-epoch data. A no-op (identity \code{upsample}) at 60s or when
#' the epoch does not divide 60.
#'
#' @keywords internal
.reintegrate.for.sleep <- function(counts, timestamps, epoch_length) {
  if (is.null(epoch_length) || is.na(epoch_length) || epoch_length <= 0 ||
      epoch_length >= 60 || (60 %% epoch_length) != 0) {
    return(list(counts = counts, timestamps = timestamps,
                epoch_length = epoch_length, upsample = seq_along(counts)))
  }
  ri <- reintegrate.epochs(counts, timestamps, from_epoch = epoch_length, to_epoch = 60)
  k <- as.integer(60 / epoch_length)
  upsample <- pmin(((seq_along(counts) - 1L) %/% k) + 1L, length(ri$counts))
  list(counts = ri$counts, timestamps = ri$timestamps,
       epoch_length = 60, upsample = upsample)
}
