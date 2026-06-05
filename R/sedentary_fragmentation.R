#' @title Sedentary Fragmentation Analysis
#'
#' @description
#' Sedentary behavior fragmentation analysis implementing validated methods
#' from peer-reviewed literature. Includes bout detection, transition probabilities,
#' power-law analysis with proper xmin estimation, and survival analysis.
#'
#' @name sedentary-fragmentation
#'
#' @references
#' \strong{Foundational Methods:}
#' \itemize{
#'   \item Chastin SFM, Granat MH. (2010). Methods for objective measure, quantification
#'     and analysis of sedentary behaviour and inactivity. Gait & Posture, 31(1):82-86.
#'   \item Chastin SFM, et al. (2015). The pattern of habitual sedentary behavior is
#'     different in advanced Parkinson's disease. Movement Disorders, 25(13):2114-2120.
#' }
#'
#' \strong{Transition Probabilities (ASTP/SATP):}
#' \itemize{
#'   \item Wanigatunga AA, et al. (2019). Active-to-Sedentary Behavior Transitions,
#'     Fatigability, and Physical Functioning in Older Adults.
#'     J Gerontol A Biol Sci Med Sci, 74(4):560-567.
#'   \item Di J, et al. (2017). Joint and Individual Representation of Domains of
#'     Physical Activity, Sleep, and Circadian Rhythmicity.
#'     Statistics in Biosciences, 9(2):371-402.
#' }
#'
#' \strong{Power-Law Analysis:}
#' \itemize{
#'   \item Clauset A, Shalizi CR, Newman MEJ. (2009). Power-law distributions in
#'     empirical data. SIAM Review, 51(4):661-703.
#'   \item Chastin SFM, et al. (2010). Gait & Posture, 31(1):82-86.
#' }
#'
#' \strong{Fragmentation Metrics:}
#' \itemize{
#'   \item Healy GN, et al. (2008). Breaks in sedentary time: beneficial associations
#'     with metabolic risk. Diabetes Care, 31(4):661-666.
#'   \item Dunstan DW, et al. (2012). Breaking up prolonged sitting reduces postprandial
#'     glucose and insulin responses. Diabetes Care, 35(5):976-983.
#' }
NULL


#' Sedentary Fragmentation Analysis
#'
#' Fragmentation analysis using validated methods from Chastin & Granat
#' (2010), Wanigatunga et al. (2019), and related literature. Implements ASTP/SATP
#' transition probabilities, robust power-law alpha estimation, usual bout duration,
#' and survival analysis.
#'
#' @param intensity Factor or character vector of intensity classifications
#' @param timestamps POSIXct vector of timestamps
#' @param wear_time Optional logical vector indicating wear time (TRUE = worn)
#' @param sleep_mask Optional vector indicating sleep status. Can be:
#'   - Logical vector (TRUE = asleep, FALSE = awake)
#'   - Character vector ("S" or "sleep" = asleep, anything else = awake)
#'   When provided, sleep periods are EXCLUDED from sedentary analysis per
#'   SBRN consensus that sedentary behavior is "waking behavior" only.
#'   This prevents sleep from being misclassified as prolonged sedentary bouts.
#'   Reference: Tremblay et al. (2017) SBRN Terminology Consensus Project.
#' @param epoch_length Epoch length in seconds (default 60)
#' @param min_break_length Minimum duration (minutes) for an active period to count as a
#'   "break" in sedentary time (default 5). Based on Healy et al. (2008) and GGIR methodology,
#'   shorter active periods are treated as continued sedentary time (gap bridging).
#'   Higher values = more conservative break counting (filters noise/fidgeting).
#'   Lower values (1-2) may inflate break counts with non-meaningful movements.
#' @param robust_alpha Use robust alpha estimation with xmin optimization? (default TRUE)
#' @param compare_distributions Compare power-law vs exponential fit? (default TRUE)
#' @param bootstrap_gof Run the Clauset et al. (2009) semiparametric bootstrap
#'   power-law goodness-of-fit test? (default FALSE). Off by default to keep the
#'   interactive dashboard fast. When FALSE, only an asymptotic KS approximation
#'   (\code{alpha_ks_pvalue_approx}) is reported - it is NOT a valid Clauset GoF
#'   p-value. When TRUE, \code{alpha_gof_pvalue} holds the bootstrap p-value.
#'
#' @return A list with class "canhrActi_fragmentation" containing:
#'   \describe{
#'     \item{total_sedentary_min, total_sedentary_hours}{Total sedentary time}
#'     \item{sedentary_percent}{Percentage of wear time spent sedentary}
#'     \item{total_bouts}{Number of sedentary bouts}
#'     \item{mean_bout_duration, median_bout_duration}{Central tendency of bout durations}
#'     \item{ASTP}{Active-to-Sedentary Transition Probability (validated)}
#'     \item{SATP}{Sedentary-to-Active Transition Probability}
#'     \item{W50}{Usual bout duration - weighted median (Chastin method)}
#'     \item{alpha}{Power-law exponent with robust xmin estimation}
#'     \item{gini}{Gini coefficient for bout inequality}
#'     \item{breaks_per_sed_hour}{Breaks per sedentary hour}
#'     \item{pct_time_30min_bouts, pct_time_60min_bouts}{Prolonged sedentary percentage}
#'     \item{distribution_fit}{Power-law vs exponential comparison results}
#'     \item{survival_analysis}{Bout survival/hazard analysis results}
#'   }
#'
#' @details
#' \strong{Transition Probabilities (Wanigatunga et al., 2019):}
#'
#' ASTP (Active-to-Sedentary Transition Probability) is the reciprocal of mean
#' active bout duration. Higher ASTP indicates more fragmented activity patterns.
#' ASTP has been strongly associated with fatigability, functional decline, and
#' mortality in older adults.
#'
#' SATP (Sedentary-to-Active Transition Probability) is the reciprocal of mean
#' sedentary bout duration. Higher SATP indicates more fragmented sedentary time.
#'
#' \strong{Usual Bout Duration - W50 (Chastin & Granat, 2010):}
#'
#' The bout duration at which 50% of total sedentary time is accumulated. Unlike
#' simple median (dominated by many short bouts), W50 reflects the typical duration
#' experienced when sedentary time is weighted by duration.
#'
#' \strong{Breaks in Sedentary Time (Healy et al., 2008):}
#'
#' A "break" is a transition from sedentary (<100 CPM) to non-sedentary (>=100 CPM)
#' that is sustained for a meaningful duration. Brief movements (fidgeting, posture
#' adjustments) should not count as breaks. This implementation uses a minimum
#' break duration threshold (default 5 minutes) - active periods shorter than this
#' are "bridged" and treated as continued sedentary time. This aligns with GGIR
#' methodology and literature recommendations (Chastin 2015, Winkler 2012).
#'
#' Expected values:
#' - Typical healthy adults: 2-5 breaks per sedentary hour
#' - Very sedentary individuals: 0.5-2 breaks per sedentary hour
#' - Active individuals: 5-10 breaks per sedentary hour
#'
#' \strong{Power-Law Alpha (Clauset et al., 2009):}
#'
#' The scaling exponent of the bout duration distribution. This implementation uses
#' the Clauset method which estimates the optimal xmin rather than assuming xmin=1.
#' Lower alpha indicates more time in prolonged bouts; higher alpha indicates more
#' fragmented patterns.
#'
#' @examples
#' \dontrun{
#' results <- canhrActi("participant.agd")
#' frag <- sedentary.fragmentation(
#'   results$epoch_data$intensity,
#'   results$epoch_data$timestamp,
#'   results$epoch_data$wear_time
#' )
#' print(frag)
#' plot(frag, type = "distribution")
#' plot(frag, type = "survival")
#' }
#'
#' @seealso
#' \code{\link{transition.probabilities}} for standalone ASTP/SATP calculation,
#' \code{\link{usual.bout.duration}} for W50 calculation,
#' \code{\link{detect.sedentary.bouts}} for bout detection
#'
#' @export
sedentary.fragmentation <- function(intensity,
                                     timestamps,
                                     wear_time = NULL,
                                     sleep_mask = NULL,
                                     epoch_length = 60,
                                     min_break_length = 5,
                                     robust_alpha = TRUE,
                                     compare_distributions = TRUE,
                                     bootstrap_gof = FALSE) {

  # Input validation
  if (!is.factor(intensity) && !is.character(intensity)) {
    stop("intensity must be a factor or character vector")
  }
  if (!inherits(timestamps, "POSIXct")) {
    stop("timestamps must be POSIXct")
  }
  if (length(intensity) != length(timestamps)) {
    stop("intensity and timestamps must have the same length")
  }

  n <- length(intensity)
  epoch_min <- epoch_length / 60

  # Apply wear time filter
  is_sedentary <- as.character(intensity) == "sedentary"
  if (!is.null(wear_time)) {
    if (length(wear_time) != n) {
      stop("wear_time must have the same length as intensity")
    }
    is_sedentary <- is_sedentary & wear_time
    total_wear_min <- sum(wear_time) * epoch_min
  } else {
    total_wear_min <- n * epoch_min
  }

  #  Exclude sleep periods from sedentary analysis
  # Per SBRN consensus: sedentary behavior is "WAKING behavior" only
  # Reference: Tremblay et al. (2017) SBRN Terminology Consensus Project
  if (!is.null(sleep_mask)) {
    if (length(sleep_mask) != n) {
      stop("sleep_mask must have the same length as intensity")
    }
    # Convert to logical if character (e.g., "S"/"W" sleep states)
    #  Handle NA values properly - treat NA as "not sleep" (FALSE)
    # NA values occur when wear_time masking has been applied to sleep_state
    if (is.character(sleep_mask)) {
      # Use %in% instead of == to handle NA values: NA %in% "S" returns FALSE
      is_sleep <- sleep_mask %in% c("S", "sleep") | tolower(sleep_mask) %in% "sleep"
      is_sleep[is.na(is_sleep)] <- FALSE
    } else {
      is_sleep <- as.logical(sleep_mask)
      # Replace NA with FALSE - NA means "unknown" which we treat as "not sleep"
      is_sleep[is.na(is_sleep)] <- FALSE
    }
    # Exclude sleep epochs from sedentary classification
    is_sedentary <- is_sedentary & !is_sleep
    # Adjust total wear time to waking wear time only
    if (!is.null(wear_time)) {
      total_wear_min <- sum(wear_time & !is_sleep, na.rm = TRUE) * epoch_min
    } else {
      total_wear_min <- sum(!is_sleep, na.rm = TRUE) * epoch_min
    }
  }

  total_sedentary_min <- sum(is_sedentary) * epoch_min

  #
  # min_break_length parameter (default 5 min): short active periods are bridged
  # Based on Healy et al. (2008), Chastin et al. (2015), and GGIR methodology:
  # - A true "break" should be sustained activity, not brief movements/fidgeting
  # - Studies recommend 5-10 min minimum to distinguish real breaks from noise
  # - This prevents artificially inflated break counts
  # - Expected: sedentary person = 0.5-2 breaks/hr; active = 5-10 breaks/hr
  bouts <- detect.sedentary.bouts(intensity, timestamps, wear_time,
                                   sleep_mask = sleep_mask,
                                   min_bout_length = 1, epoch_length = epoch_length,
                                   min_break_length = min_break_length)

  durations <- bouts$duration_min
  n_bouts <- length(durations)

  # Handle case with no bouts
  if (n_bouts == 0) {
    result <- .create.empty.fragmentation.result(total_sedentary_min, total_wear_min, timestamps)
    return(result)
  }

  # Note on long sedentary bouts:
  # Long sedentary bouts (even 4+ hours) are VALID sedentary behavior for many individuals.
  # The standard approach uses WEAR TIME to identify valid measurement periods:
  # - Choi/Troiano algorithms detect non-wear (including overnight if device removed)
  # - Sedentary time = low activity during valid wear time
  # - No automatic sleep exclusion is applied
  #
  # If 24-hour wear with explicit sleep periods (from ActiLife or sleep diary),

  # users can optionally pass sleep_mask parameter to exclude sleep.
  # Reference: NHANES methodology, Choi et al. (2011), Chastin & Granat (2010)

  # Calculate transition probabilities with consistent gap bridging

  # to ensure consistency with bout detection metrics (W50, alpha, Gini)
  #
  # We need to pass the gap-bridged, sleep-excluded intensity to transition.probabilities()
  # Option 1: Calculate from already-detected bouts (more accurate)
  # Option 2: Apply same preprocessing to intensity vector
  #
  # Using Option 1: Calculate SATP/ASTP directly from detected bout durations,
  # and derive active bouts from a vector bridged with the SAME two-sided
  # flanking rule as detect.sedentary.bouts(), keeping ASTP/SATP consistent
  # with W50/alpha/Gini.

  # Calculate SATP from sedentary bout durations (already gap-bridged)
  mean_sed_bout_epochs <- mean(durations) / epoch_min  # Convert minutes to epochs
  SATP <- if (mean_sed_bout_epochs > 0) 1 / mean_sed_bout_epochs else NA_real_

  # For ASTP, we need active bout durations with same preprocessing
  # Detect active bouts using gap-bridged sedentary classification
  is_sedentary_processed <- as.character(intensity) == "sedentary"
  if (!is.null(wear_time)) {
    is_sedentary_processed <- is_sedentary_processed & wear_time
  }
  if (!is.null(sleep_mask)) {
    if (is.character(sleep_mask)) {
      is_sleep <- sleep_mask %in% c("S", "sleep") | tolower(sleep_mask) %in% "sleep"
      is_sleep[is.na(is_sleep)] <- FALSE
    } else {
      is_sleep <- as.logical(sleep_mask)
      is_sleep[is.na(is_sleep)] <- FALSE
    }
    is_sedentary_processed <- is_sedentary_processed & !is_sleep
  }

  # Apply gap bridging to the processed intensity using the IDENTICAL two-sided
  # flanking rule as detect.sedentary.bouts(): only bridge a short non-sedentary
  # gap when it is flanked by sedentary periods on BOTH sides. This guarantees
  # ASTP/SATP are derived from the same bridged vector used for W50/alpha/Gini.
  min_break_epochs <- ceiling(min_break_length * 60 / epoch_length)
  if (min_break_epochs > 0 && length(is_sedentary_processed) > 0) {
    rle_sed <- rle(is_sedentary_processed)
    n_runs <- length(rle_sed$lengths)
    if (n_runs > 1) {
      end_indices_temp <- cumsum(rle_sed$lengths)
      start_indices_temp <- c(1, end_indices_temp[-n_runs] + 1)
      for (i in seq_len(n_runs)) {
        # Only consider short non-sedentary runs (potential gaps)
        if (!rle_sed$values[i] && rle_sed$lengths[i] <= min_break_epochs) {
          # Require sedentary flanks on BOTH sides (two-sided rule)
          has_sed_before <- i > 1 && rle_sed$values[i - 1]
          has_sed_after <- i < n_runs && rle_sed$values[i + 1]
          if (has_sed_before && has_sed_after) {
            is_sedentary_processed[start_indices_temp[i]:end_indices_temp[i]] <- TRUE
          }
        }
      }
    }
  }

  # Now calculate active bout statistics from the gap-bridged data
  rle_processed <- rle(is_sedentary_processed)
  active_lengths <- rle_processed$lengths[!rle_processed$values]
  n_active_bouts <- length(active_lengths)
  mean_active_bout_epochs <- if (n_active_bouts > 0) mean(active_lengths) else NA_real_
  median_active_bout_epochs <- if (n_active_bouts > 0) median(active_lengths) else NA_real_

  ASTP <- if (!is.na(mean_active_bout_epochs) && mean_active_bout_epochs > 0) {
    1 / mean_active_bout_epochs
  } else NA_real_

  # Build trans_probs object for compatibility
  trans_probs <- list(
    ASTP = round(ASTP, 5),
    SATP = round(SATP, 5),
    mean_active_bout = round(mean_active_bout_epochs * epoch_min, 2),  # Convert to minutes
    median_active_bout = round(median_active_bout_epochs * epoch_min, 2),
    n_active_bouts = n_active_bouts,
    total_transitions = n_bouts + n_active_bouts - 1
  )

  # 
  w50 <- usual.bout.duration(durations)
  w_percentiles <- usual.bout.percentiles(durations, c(25, 50, 75, 90))

  #
  if (robust_alpha && n_bouts >= 10) {
    alpha_result <- .calculate.alpha.robust(durations, bootstrap_n = 100,
                                            bootstrap_gof = bootstrap_gof)
  } else {
    alpha_result <- list(
      alpha = .calculate.alpha.simple(durations, xmin = 1),
      alpha_se = NA_real_,
      alpha_ci = c(NA_real_, NA_real_),
      xmin = 1,
      n_tail = n_bouts,
      n_total = n_bouts,
      pct_in_tail = 100,
      ks_stat = NA_real_,
      gof_pvalue = NA_real_,
      ks_pvalue_approx = NA_character_
    )
  }

  # 
  gini <- .calculate.gini(durations)

  # 
  prolonged <- prolonged.sedentary(durations, c(20, 30, 60))

  #
  survival <- bout.survival.analysis(durations)

  # Weibull shape of the bout-duration distribution (hazard direction) and the
  # day-to-day regularity of the sedentary/active pattern (adapted SRI).
  weibull <- tryCatch(survival.weibull(durations), error = function(e) NULL)
  sed_sri <- tryCatch(sedentary.regularity.index(intensity, timestamps, wear_time),
                      error = function(e) NA_real_)

  #
  dist_comparison <- NULL
  if (compare_distributions && n_bouts >= 10) {
    dist_comparison <- compare.bout.distributions(durations, bootstrap_gof = bootstrap_gof)
  }

  # 
  # A "break" is when a sedentary bout ends (transition to non-sedentary)
  # Count based on detected bouts, not raw transitions (filters noise)
  # Each bout that ended = 1 break (except last bout if still ongoing at end of data)

  # Check if the last detected bout extends to the end of the VALID data.
  # The ongoing-bout test must reference the last wear & waking (valid) epoch,
  # not the raw length n: trailing non-wear/sleep epochs are not real "active"
  # time and must not turn an end-of-data sedentary bout into a counted break.
  valid_epoch <- rep(TRUE, n)
  if (!is.null(wear_time)) {
    wear_valid <- as.logical(wear_time)
    wear_valid[is.na(wear_valid)] <- FALSE  # NA wear treated as non-valid
    valid_epoch <- valid_epoch & wear_valid
  }
  if (!is.null(sleep_mask)) {
    # is_sleep was derived above with NA -> FALSE handling
    valid_epoch <- valid_epoch & !is_sleep
  }
  last_valid_index <- if (any(valid_epoch)) max(which(valid_epoch)) else n

  last_bout_ongoing <- FALSE
  if (n_bouts > 0) {
    last_bout_end <- bouts$end_index[n_bouts]
    # Bout is "ongoing" if it ends at (or within a small gap of) the last valid
    # epoch, so no sustained active period follows it within the measurement.
    last_bout_ongoing <- (last_valid_index - last_bout_end) <= 2
  }

  # Breaks = number of completed sedentary bouts
  breaks_total <- if (last_bout_ongoing) max(0L, n_bouts - 1L) else n_bouts

  sed_hours <- total_sedentary_min / 60
  breaks_per_sed_hour <- if (sed_hours > 0) breaks_total / sed_hours else NA_real_

  # 
  breaks_cat <- c(0, 5, 10, 20, 30, 60, Inf)
  labels_cat <- c("1-5 min", "5-10 min", "10-20 min", "20-30 min", "30-60 min", ">60 min")
  bout_cats <- cut(durations, breaks = breaks_cat, labels = labels_cat,
                   right = TRUE, include.lowest = TRUE)
  bout_table <- table(factor(bout_cats, levels = labels_cat))

  # Mid-values for each category (used for total_time calculation in plotting)
  mid_values <- c(2.5, 7.5, 15, 25, 45, 90)  # Midpoints for: 1-5, 5-10, 10-20, 20-30, 30-60, >60 min

  bout_distribution <- data.frame(
    category = labels_cat,
    count = as.integer(bout_table),
    percent = round(as.numeric(bout_table) / n_bouts * 100, 1),
    mid_value = mid_values,
    stringsAsFactors = FALSE
  )

  # Per-day breakdown using the SAME gap-bridging + sleep exclusion as the totals.
  daily_frag <- .calculate.daily.fragmentation(intensity, timestamps, wear_time, epoch_length,
                                               min_break_length = min_break_length,
                                               sleep_mask = sleep_mask)

  # 
  n_days <- length(unique(as.Date(timestamps)))

  result <- list(
    # Summary
    total_sedentary_min = round(total_sedentary_min, 1),
    total_sedentary_hours = round(total_sedentary_min / 60, 2),
    total_wear_min = round(total_wear_min, 1),
    total_wear_hours = round(total_wear_min / 60, 2),
    sedentary_percent = round(100 * total_sedentary_min / total_wear_min, 1),
    n_days_analyzed = n_days,

    # Bout counts and central tendency
    total_bouts = n_bouts,
    mean_bout_duration = round(mean(durations), 2),
    median_bout_duration = round(median(durations), 2),
    sd_bout_duration = round(sd(durations), 2),
    max_bout_duration = round(max(durations), 1),

    #  Transition probabilities (Wanigatunga et al., 2019)
    ASTP = trans_probs$ASTP,
    SATP = trans_probs$SATP,
    mean_active_bout = trans_probs$mean_active_bout,
    median_active_bout = trans_probs$median_active_bout,
    n_active_bouts = trans_probs$n_active_bouts,
    total_transitions = trans_probs$total_transitions,

    #  Usual bout duration (Chastin & Granat, 2010)
    W50 = w50,
    W25 = unname(w_percentiles["W25"]),
    W75 = unname(w_percentiles["W75"]),
    W90 = unname(w_percentiles["W90"]),

    # Breaks
    breaks_total = breaks_total,
    breaks_per_sed_hour = round(breaks_per_sed_hour, 2),

    # Distribution shape (Clauset et al., 2009)
    alpha = alpha_result$alpha,
    alpha_se = alpha_result$alpha_se,
    alpha_ci_lower = alpha_result$alpha_ci[1],
    alpha_ci_upper = alpha_result$alpha_ci[2],
    alpha_xmin = alpha_result$xmin,
    alpha_n_tail = alpha_result$n_tail,
    alpha_pct_in_tail = alpha_result$pct_in_tail,
    alpha_ks_stat = alpha_result$ks_stat,
    # Clauset semiparametric bootstrap GoF p-value (NA unless bootstrap_gof=TRUE);
    # ks_pvalue_approx is only an asymptotic KS approximation, NOT a Clauset p-value.
    alpha_gof_pvalue = alpha_result$gof_pvalue,
    alpha_ks_pvalue_approx = alpha_result$ks_pvalue_approx,
    gini = round(gini, 4),

    # Prolonged sedentary
    pct_time_20min_bouts = prolonged$pct_time[prolonged$threshold == 20],
    pct_time_30min_bouts = prolonged$pct_time[prolonged$threshold == 30],
    pct_time_60min_bouts = prolonged$pct_time[prolonged$threshold == 60],
    prolonged_summary = prolonged,

    # Survival analysis
    median_bout_survival = survival$median_survival,
    hazard_rate = survival$hazard_rate,
    survival_curve = survival$survival_curve,
    weibull_shape = if (!is.null(weibull)) weibull$shape else NA_real_,
    weibull_scale = if (!is.null(weibull)) weibull$scale else NA_real_,
    weibull_hazard = if (!is.null(weibull) && !is.null(weibull$hazard_interpretation))
      weibull$hazard_interpretation else NA_character_,

    # Day-to-day regularity of the sedentary/active pattern
    sedentary_regularity_index = sed_sri,

    # Distribution comparison
    distribution_fit = dist_comparison,

    # Daily patterns
    daily_fragmentation = daily_frag,

    # Raw data
    bout_distribution = bout_distribution,
    bouts = bouts,

    # Metadata
    analysis_method = "canhrActi_v2_fragmentation",
    min_break_length_used = min_break_length
  )

  class(result) <- c("canhrActi_fragmentation", "list")
  return(result)
}


#' Detect Sedentary Bouts
#'
#' Identifies continuous periods of sedentary behavior from intensity classifications.
#' Uses gap bridging methodology from GGIR and literature (Chastin 2015, Winkler 2012)
#' to filter out brief movements/fidgeting that don't represent true breaks.
#'
#' @param intensity Factor or character vector of intensity classifications
#' @param timestamps POSIXct vector of timestamps
#' @param wear_time Optional logical vector indicating wear time (TRUE = worn)
#' @param sleep_mask Optional vector indicating sleep status. Can be:
#'   - Logical vector (TRUE = asleep, FALSE = awake)
#'   - Character vector ("S" or "sleep" = asleep, anything else = awake)
#'   When provided, sleep periods are EXCLUDED from sedentary analysis per
#'   SBRN consensus that sedentary behavior is "waking behavior" only.
#'   Reference: Tremblay et al. (2017) SBRN Terminology Consensus Project.
#' @param min_bout_length Minimum bout length in minutes to include (default 1)
#' @param epoch_length Epoch length in seconds (default 60)
#' @param min_break_length Minimum break duration in minutes for a break to count (default 1).
#'   Active periods shorter than or equal to this are "bridged" - treated as continued
#'   sedentary time. Based on Healy et al. (2008) and GGIR methodology, recommended
#'   values are 5-10 minutes to filter noise and fidgeting. Lower values (1-2 min)
#'   will result in higher break counts that may include non-meaningful movements.
#'
#' @return A data frame with columns:
#'   \item{bout_id}{Unique identifier for each bout}
#'   \item{start_time}{Start timestamp of the bout}
#'   \item{end_time}{End timestamp of the bout}
#'   \item{duration_min}{Duration in minutes}
#'   \item{start_index}{Starting row index}
#'   \item{end_index}{Ending row index}
#'
#' @export
detect.sedentary.bouts <- function(intensity, timestamps, wear_time = NULL,
                                    sleep_mask = NULL,
                                    min_bout_length = 1, epoch_length = 60,
                                    min_break_length = 1) {

  if (!is.factor(intensity) && !is.character(intensity)) {
    stop("intensity must be a factor or character vector")
  }
  if (!inherits(timestamps, "POSIXct")) {
    stop("timestamps must be POSIXct")
  }
  if (length(intensity) != length(timestamps)) {
    stop("intensity and timestamps must have the same length")
  }

  n <- length(intensity)
  if (n == 0) {
    return(.create.empty.bouts.df())
  }

  # Apply wear time filter
  is_sedentary <- as.character(intensity) == "sedentary"
  if (!is.null(wear_time)) {
    if (length(wear_time) != n) {
      stop("wear_time must have the same length as intensity")
    }
    is_sedentary <- is_sedentary & wear_time
  }

  #  Exclude sleep periods from sedentary analysis
  # Per SBRN consensus: sedentary behavior is "WAKING behavior" only
  # Sleep should NOT be classified as sedentary time
  if (!is.null(sleep_mask)) {
    if (length(sleep_mask) != n) {
      stop("sleep_mask must have the same length as intensity")
    }
    # Convert to logical if character (e.g., "S"/"W" sleep states)
    #  Handle NA values properly - treat NA as "not sleep" (FALSE)
    # NA values occur when wear_time masking has been applied to sleep_state
    if (is.character(sleep_mask)) {
      # Use %in% instead of == to handle NA values: NA %in% "S" returns FALSE
      is_sleep <- sleep_mask %in% c("S", "sleep") | tolower(sleep_mask) %in% "sleep"
      is_sleep[is.na(is_sleep)] <- FALSE
    } else {
      is_sleep <- as.logical(sleep_mask)
      # Replace NA with FALSE - NA means "unknown" which we treat as "not sleep"
      is_sleep[is.na(is_sleep)] <- FALSE
    }
    # Exclude sleep epochs from sedentary classification
    is_sedentary <- is_sedentary & !is_sleep
  }

  # Run-length encoding for efficient bout detection
  rle_sed <- rle(is_sedentary)

  # Calculate bout boundaries
  end_indices <- cumsum(rle_sed$lengths)
  start_indices <- c(1, end_indices[-length(end_indices)] + 1)

  # Bridge short gaps: merge sedentary bouts separated by short active periods
  # This filters out noise/fidgeting that would artificially increase break count
  # A real "break" should be at least min_break_length minutes of sustained activity
  min_break_epochs <- ceiling(min_break_length * 60 / epoch_length)

  if (min_break_epochs > 0 && length(rle_sed$lengths) > 1) {
    # Find short non-sedentary runs (gaps) that are BETWEEN sedentary bouts
    # Only bridge gaps that are flanked by sedentary periods on both sides
    # This prevents incorrectly bridging active periods at start/end of data
    is_sedentary_bridged <- is_sedentary
    n_runs <- length(rle_sed$lengths)

    for (i in seq_len(n_runs)) {
      # Only consider non-sedentary runs (potential gaps)
      if (!rle_sed$values[i]) {
        # Check if this gap is short enough to bridge
        if (rle_sed$lengths[i] <= min_break_epochs) {
          # Check if flanked by sedentary bouts on BOTH sides
          has_sed_before <- i > 1 && rle_sed$values[i - 1]
          has_sed_after <- i < n_runs && rle_sed$values[i + 1]

          if (has_sed_before && has_sed_after) {
            # Bridge this gap
            gap_start <- start_indices[i]
            gap_end <- end_indices[i]
            is_sedentary_bridged[gap_start:gap_end] <- TRUE
          }
        }
      }
    }

    # Re-run RLE with bridged data if any bridging occurred
    if (!identical(is_sedentary, is_sedentary_bridged)) {
      rle_sed <- rle(is_sedentary_bridged)
      end_indices <- cumsum(rle_sed$lengths)
      start_indices <- c(1, end_indices[-length(end_indices)] + 1)
    }
  }

  # Filter to sedentary bouts only
  sed_mask <- rle_sed$values
  bout_starts <- start_indices[sed_mask]
  bout_ends <- end_indices[sed_mask]

  if (length(bout_starts) == 0) {
    return(.create.empty.bouts.df())
  }

  # Calculate durations
  bout_lengths <- bout_ends - bout_starts + 1
  duration_min <- bout_lengths * (epoch_length / 60)

  # Filter by minimum bout length
  valid_bouts <- duration_min >= min_bout_length

  if (sum(valid_bouts) == 0) {
    return(.create.empty.bouts.df())
  }

  data.frame(
    bout_id = seq_len(sum(valid_bouts)),
    start_time = timestamps[bout_starts[valid_bouts]],
    end_time = timestamps[bout_ends[valid_bouts]],
    duration_min = duration_min[valid_bouts],
    start_index = bout_starts[valid_bouts],
    end_index = bout_ends[valid_bouts],
    stringsAsFactors = FALSE
  )
}


#' Calculate Transition Probabilities (ASTP/SATP)
#'
#' Validated fragmentation metrics from aging research. ASTP (Active-to-Sedentary
#' Transition Probability) has been strongly associated with fatigability, functional
#' decline, and mortality in older adults.
#'
#' @param intensity Factor or character vector of intensity classifications
#' @param wear_time Optional logical vector indicating wear time
#' @param sedentary_threshold Character. What counts as sedentary? (default "sedentary")
#' @param min_break_length Numeric. Minimum sustained active minutes to count as a
#'   true break; shorter active runs flanked by sedentary on both sides are bridged
#'   (Healy/GGIR), matching \code{\link{sedentary.fragmentation}} (default 5).
#' @param epoch_length Numeric. Epoch length in seconds (default 60).
#' @param sleep_mask Optional logical/character vector marking sleep epochs to
#'   exclude (same convention as \code{\link{detect.sedentary.bouts}}).
#' @param min_bout_length Numeric. Minimum sedentary bout duration in minutes;
#'   shorter runs are dropped before computing SATP, matching
#'   \code{\link{detect.sedentary.bouts}} (default 1).
#'
#' @return List with ASTP, SATP, and related metrics
#'
#' @details
#' \strong{ASTP} = 1 / mean(active bout duration)
#' Probability of transitioning from active to sedentary per epoch.
#' Higher ASTP indicates more fragmented activity patterns.
#'
#' \strong{SATP} = 1 / mean(sedentary bout duration)
#' Probability of transitioning from sedentary to active per epoch.
#' Higher SATP indicates more fragmented sedentary time (generally healthier).
#'
#' @references
#' Wanigatunga AA, et al. (2019). Active-to-Sedentary Behavior Transitions,
#' Fatigability, and Physical Functioning in Older Adults.
#' J Gerontol A Biol Sci Med Sci, 74(4):560-567.
#'
#' @export
transition.probabilities <- function(intensity, wear_time = NULL,
                                     sedentary_threshold = "sedentary",
                                     min_break_length = 5, epoch_length = 60,
                                     sleep_mask = NULL, min_bout_length = 1) {

  # Mask wear + sleep, treat non-wear/sleep as non-sedentary (FALSE) rather than
  # dropping epochs, then apply the SAME two-sided gap bridge as
  # detect.sedentary.bouts() so ASTP/SATP match sedentary.fragmentation()'s.
  is_sed <- as.character(intensity) == sedentary_threshold
  if (!is.null(wear_time)) is_sed <- is_sed & as.logical(wear_time)
  if (!is.null(sleep_mask)) {
    is_sleep <- if (is.character(sleep_mask)) {
      sleep_mask %in% c("S", "sleep") | tolower(sleep_mask) %in% "sleep"
    } else as.logical(sleep_mask)
    is_sleep[is.na(is_sleep)] <- FALSE
    is_sed <- is_sed & !is_sleep
  }
  is_sed[is.na(is_sed)] <- FALSE

  if (length(is_sed) < 2) {
    return(list(
      ASTP = NA_real_,
      SATP = NA_real_,
      mean_active_bout = NA_real_,
      median_active_bout = NA_real_,
      mean_sedentary_bout = NA_real_,
      median_sedentary_bout = NA_real_,
      n_active_bouts = NA_integer_,
      n_sedentary_bouts = NA_integer_,
      total_transitions = NA_integer_
    ))
  }

  # Two-sided gap bridge: fill a short non-sedentary run only when flanked by
  # sedentary on both sides.
  min_break_epochs <- ceiling(min_break_length * 60 / epoch_length)
  if (min_break_epochs > 0) {
    rle_b <- rle(is_sed)
    n_runs <- length(rle_b$lengths)
    if (n_runs > 1) {
      end_b <- cumsum(rle_b$lengths)
      start_b <- c(1, end_b[-n_runs] + 1)
      for (i in seq_len(n_runs)) {
        if (!rle_b$values[i] && rle_b$lengths[i] <= min_break_epochs &&
            i > 1 && rle_b$values[i - 1] && i < n_runs && rle_b$values[i + 1]) {
          is_sed[start_b[i]:end_b[i]] <- TRUE
        }
      }
    }
  }

  # Run-length encoding for bout detection (bout lengths are in epochs)
  rle_result <- rle(is_sed)

  # Sedentary bouts. Drop runs below min_bout_length minutes, matching
  # detect.sedentary.bouts() (duration_min >= min_bout_length) so SATP agrees
  # with sedentary.fragmentation() at sub-minute epochs, not only at 60 s.
  sed_lengths <- rle_result$lengths[rle_result$values == TRUE]
  sed_lengths <- sed_lengths[(sed_lengths * epoch_length / 60) >= min_bout_length]
  n_sed_bouts <- length(sed_lengths)
  mean_sed_bout <- if (n_sed_bouts > 0) mean(sed_lengths) else NA_real_
  median_sed_bout <- if (n_sed_bouts > 0) median(sed_lengths) else NA_real_

  # Active bouts
  active_lengths <- rle_result$lengths[rle_result$values == FALSE]
  n_active_bouts <- length(active_lengths)
  mean_active_bout <- if (n_active_bouts > 0) mean(active_lengths) else NA_real_
  median_active_bout <- if (n_active_bouts > 0) median(active_lengths) else NA_real_

  # Transition probabilities (reciprocal of mean bout length in epochs)
  ASTP <- if (!is.na(mean_active_bout) && mean_active_bout > 0) {
    1 / mean_active_bout
  } else NA_real_

  SATP <- if (!is.na(mean_sed_bout) && mean_sed_bout > 0) {
    1 / mean_sed_bout
  } else NA_real_

  # Count transitions
  transitions <- diff(as.integer(is_sed))
  n_sed_to_active <- sum(transitions == -1, na.rm = TRUE)
  n_active_to_sed <- sum(transitions == 1, na.rm = TRUE)

  list(
    ASTP = round(ASTP, 5),
    SATP = round(SATP, 5),
    mean_active_bout = round(mean_active_bout, 2),
    median_active_bout = round(median_active_bout, 2),
    mean_sedentary_bout = round(mean_sed_bout, 2),
    median_sedentary_bout = round(median_sed_bout, 2),
    n_active_bouts = n_active_bouts,
    n_sedentary_bouts = n_sed_bouts,
    n_transitions_sed_to_active = n_sed_to_active,
    n_transitions_active_to_sed = n_active_to_sed,
    total_transitions = n_sed_to_active + n_active_to_sed
  )
}


#' Distribution-Shape Fragmentation Metrics from Bout Durations
#'
#' Computes the bout-duration-distribution fragmentation metrics (power-law
#' alpha, Gini, usual bout duration W50 + weighted percentiles, central tendency,
#' and the sedentary-to-active transition probability) directly from a vector of
#' bout durations. Useful for estimating COHORT-level fragmentation from a pooled
#' bout pool rather than averaging per-recording statistics.
#'
#' @param durations Numeric vector of sedentary bout durations in minutes.
#' @param epoch_length Numeric. Epoch length in seconds, used to express SATP per
#'   epoch (default 60).
#' @param bootstrap_gof Logical. Run the Clauset (2009) semiparametric bootstrap
#'   goodness-of-fit for the power-law alpha (default FALSE).
#'
#' @return A named list: \code{alpha}, \code{alpha_ci_lower}, \code{alpha_ci_upper},
#'   \code{gini}, \code{W50}, \code{W25}, \code{W75}, \code{W90}, \code{mean_bout},
#'   \code{median_bout}, \code{max_bout}, \code{SATP}, \code{n_bouts}. All-NA with
#'   \code{n_bouts = 0} if no valid durations are supplied.
#' @export
bout.distribution.metrics <- function(durations, epoch_length = 60,
                                      bootstrap_gof = FALSE) {
  durations <- durations[is.finite(durations) & durations > 0]
  n <- length(durations)
  na_out <- list(
    alpha = NA_real_, alpha_ci_lower = NA_real_, alpha_ci_upper = NA_real_,
    gini = NA_real_, W50 = NA_real_, W25 = NA_real_, W75 = NA_real_, W90 = NA_real_,
    mean_bout = NA_real_, median_bout = NA_real_, max_bout = NA_real_,
    SATP = NA_real_, n_bouts = n
  )
  if (n == 0) return(na_out)

  epoch_min <- epoch_length / 60
  alpha_fit <- if (n >= 10) {
    .calculate.alpha.robust(durations, bootstrap_n = 100, bootstrap_gof = bootstrap_gof)
  } else {
    list(alpha = .calculate.alpha.simple(durations, xmin = 1),
         alpha_ci = c(NA_real_, NA_real_))
  }
  wp <- usual.bout.percentiles(durations, c(25, 50, 75, 90))
  mean_epochs <- mean(durations) / epoch_min

  list(
    alpha = alpha_fit$alpha,
    alpha_ci_lower = alpha_fit$alpha_ci[1],
    alpha_ci_upper = alpha_fit$alpha_ci[2],
    gini = round(.calculate.gini(durations), 4),
    W50 = usual.bout.duration(durations),
    W25 = unname(wp["W25"]), W75 = unname(wp["W75"]), W90 = unname(wp["W90"]),
    mean_bout = round(mean(durations), 2),
    median_bout = round(median(durations), 2),
    max_bout = round(max(durations), 1),
    SATP = if (mean_epochs > 0) round(1 / mean_epochs, 5) else NA_real_,
    n_bouts = n
  )
}


#' Calculate Usual Bout Duration (W50)
#'
#' The bout duration at which 50% of total sedentary time is accumulated.
#' This is the weighted median, which is more robust than mean or simple median.
#'
#' @param bout_durations Numeric vector of bout durations (minutes)
#'
#' @return Numeric W50 value
#'
#' @details
#' Unlike the simple median (dominated by many short bouts), W50 asks:
#' "What is the typical bout duration when weighted by time?"
#'
#' Example interpretation:
#' \itemize{
#'   \item 100 bouts of 1 min + 1 bout of 100 min
#'   \item Simple median = 1 min (misleading - most bouts are short)
#'   \item W50 ~ 50 min (half of sedentary time is in bouts >= 50 min)
#' }
#'
#' @references
#' Chastin SFM, Granat MH. (2010). Methods for objective measure, quantification
#' and analysis of sedentary behaviour and inactivity. Gait & Posture, 31(1):82-86.
#'
#' @export
usual.bout.duration <- function(bout_durations) {

  if (length(bout_durations) == 0 || all(is.na(bout_durations))) {
    return(NA_real_)
  }

  bout_durations <- bout_durations[!is.na(bout_durations)]
  n <- length(bout_durations)

  if (n == 0) return(NA_real_)
  if (n == 1) return(bout_durations)

  total_time <- sum(bout_durations)

  # Sort from longest to shortest
  sorted_desc <- sort(bout_durations, decreasing = TRUE)
  cumsum_time <- cumsum(sorted_desc)

  # Find where cumulative time reaches 50%
  idx_50 <- which(cumsum_time >= total_time / 2)[1]

  if (is.na(idx_50)) return(NA_real_)

  # W50 is the duration at which 50% threshold is crossed
  round(sorted_desc[idx_50], 1)
}


#' Calculate Multiple Usual Bout Percentiles
#'
#' Time-weighted percentiles of the bout-duration distribution. WX is the bout
#' duration at which X\% of total sedentary time has accumulated when bouts are
#' considered from SHORTEST to LONGEST. Defined this way the percentiles are
#' monotonically INCREASING (W25 <= W50 <= W75 <= W90), so larger W corresponds
#' to a longer-bout cutoff, matching the "25th/75th/90th weighted percentile"
#' labels surfaced in the dashboard and print method. W50 is the time-weighted
#' median (companion to \code{usual.bout.duration()}).
#'
#' @param bout_durations Numeric vector of bout durations
#' @param percentiles Percentiles to calculate (default: c(25, 50, 75, 90))
#'
#' @return Named numeric vector with W25, W50, W75, W90 (monotonically increasing)
#'
#' @export
usual.bout.percentiles <- function(bout_durations, percentiles = c(25, 50, 75, 90)) {

  if (length(bout_durations) == 0 || all(is.na(bout_durations))) {
    result <- rep(NA_real_, length(percentiles))
    names(result) <- paste0("W", percentiles)
    return(result)
  }

  # Accumulate time from SHORTEST to LONGEST so that the duration at which X% of
  # sedentary time has accrued increases with X (proper time-weighted percentile).
  bout_durations <- sort(bout_durations[!is.na(bout_durations)], decreasing = FALSE)
  total_time <- sum(bout_durations)
  cumsum_time <- cumsum(bout_durations)

  result <- sapply(percentiles, function(p) {
    threshold <- total_time * p / 100
    idx <- which(cumsum_time >= threshold)[1]
    if (is.na(idx)) NA_real_ else bout_durations[idx]
  })

  names(result) <- paste0("W", percentiles)
  round(result, 1)
}


#' Calculate Prolonged Sedentary Time Metrics
#'
#' Percentage of sedentary time accumulated in prolonged bouts.
#' Prolonged sedentary bouts (>30 min) are associated with worse health outcomes.
#'
#' @param bout_durations Numeric vector of bout durations (minutes)
#' @param thresholds Thresholds defining "prolonged" (default: c(20, 30, 60))
#'
#' @return Data frame with prolonged bout metrics for each threshold
#'
#' @references
#' Healy GN, et al. (2008). Breaks in sedentary time: beneficial associations
#' with metabolic risk. Diabetes Care, 31(4):661-666.
#'
#' @export
prolonged.sedentary <- function(bout_durations, thresholds = c(20, 30, 60)) {

  total_time <- sum(bout_durations, na.rm = TRUE)
  n_bouts <- sum(!is.na(bout_durations))

  if (total_time == 0 || n_bouts == 0) {
    return(data.frame(
      threshold = thresholds,
      n_bouts = rep(0L, length(thresholds)),
      pct_bouts = rep(0, length(thresholds)),
      total_time_min = rep(0, length(thresholds)),
      pct_time = rep(0, length(thresholds)),
      stringsAsFactors = FALSE
    ))
  }

  results <- lapply(thresholds, function(thresh) {
    prolonged <- bout_durations >= thresh
    data.frame(
      threshold = thresh,
      n_bouts = sum(prolonged, na.rm = TRUE),
      pct_bouts = round(100 * sum(prolonged, na.rm = TRUE) / n_bouts, 1),
      total_time_min = round(sum(bout_durations[prolonged], na.rm = TRUE), 1),
      pct_time = round(100 * sum(bout_durations[prolonged], na.rm = TRUE) / total_time, 1),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}


#' Robust Power-Law Alpha with xmin Estimation
#'
#' Estimates the power-law exponent using the Clauset et al. (2009) method,
#' which finds the optimal xmin rather than assuming xmin=1.
#'
#' @param durations Numeric vector of bout durations
#' @param xmin_candidates Candidate xmin values to test (default: unique durations)
#' @param bootstrap_n Number of bootstrap iterations for CI (default: 100)
#' @param bootstrap_gof Run Clauset et al. (2009, Sec 4.2) semiparametric Monte-Carlo
#'   bootstrap goodness-of-fit test? (default FALSE). When FALSE (the default, kept
#'   off so the interactive dashboard stays fast) no Clauset p-value is produced;
#'   instead an asymptotic Kolmogorov-Smirnov approximation (\code{ks_pvalue_approx})
#'   is reported and clearly labelled as an approximation, not a Clauset GoF p-value.
#'   When TRUE, \code{gof_pvalue} is the fraction of synthetic KS statistics that
#'   meet or exceed the empirical KS statistic.
#' @param gof_bootstrap_n Number of synthetic datasets for the GoF bootstrap when
#'   \code{bootstrap_gof = TRUE} (default 200).
#'
#' @return List with alpha, xmin, KS statistic, confidence intervals, and either a
#'   Clauset GoF p-value (\code{gof_pvalue}) or an asymptotic KS approximation
#'   (\code{ks_pvalue_approx}).
#'
#' @details
#' The naive Hill estimator (alpha = 1 + n/sum(log(x/xmin))) with fixed xmin
#' produces biased estimates. This function:
#' \enumerate{
#'   \item Tests multiple xmin values
#'   \item Selects xmin that minimizes KS statistic
#'   \item Provides bootstrap confidence intervals
#'   \item Optionally runs the Clauset semiparametric GoF bootstrap
#' }
#'
#' The asymptotic KS critical-value lookup (1.36/sqrt(n)) is NOT a valid power-law
#' goodness-of-fit test (it is anti-conservative because alpha and xmin are estimated
#' from the same data); it is surfaced only as \code{ks_pvalue_approx}. Set
#' \code{bootstrap_gof = TRUE} for the Clauset (2009) semiparametric bootstrap p-value.
#'
#' @references
#' Clauset A, Shalizi CR, Newman MEJ. (2009). Power-law distributions in
#' empirical data. SIAM Review, 51(4):661-703.
#'
#' @keywords internal
.calculate.alpha.robust <- function(durations, xmin_candidates = NULL, bootstrap_n = 100,
                                    bootstrap_gof = FALSE, gof_bootstrap_n = 200) {

  durations <- durations[!is.na(durations) & durations > 0]
  n_total <- length(durations)

  if (n_total < 10) {
    return(list(
      alpha = NA_real_,
      alpha_se = NA_real_,
      alpha_ci = c(NA_real_, NA_real_),
      xmin = NA_real_,
      n_tail = NA_integer_,
      n_total = n_total,
      pct_in_tail = NA_real_,
      ks_stat = NA_real_,
      gof_pvalue = NA_real_,
      ks_pvalue_approx = NA_character_
    ))
  }

  # Default xmin candidates: unique values up to median
  if (is.null(xmin_candidates)) {
    xmin_candidates <- sort(unique(durations))
    xmin_candidates <- xmin_candidates[xmin_candidates <= quantile(durations, 0.5)]
    if (length(xmin_candidates) > 50) {
      xmin_candidates <- xmin_candidates[seq(1, length(xmin_candidates), length.out = 50)]
    }
    if (length(xmin_candidates) == 0) {
      xmin_candidates <- min(durations)
    }
  }

  # Test each xmin candidate
  results <- lapply(xmin_candidates, function(xm) {
    x <- durations[durations >= xm]
    n <- length(x)

    if (n < 5) return(list(xmin = xm, alpha = NA_real_, ks = Inf, n = n))

    # MLE for alpha (Hill estimator) with continuous correction
    # Using (xmin - 0.5) for discrete data per Clauset et al. (2009)
    # This matches the C++ implementation for consistency
    xm_corrected <- max(0.5, xm - 0.5)
    log_sum <- sum(log(x / xm_corrected))
    if (log_sum <= 0) return(list(xmin = xm, alpha = NA_real_, ks = Inf, n = n))

    alpha <- 1 + n / log_sum

    # KS statistic: compare empirical CDF to theoretical power-law CDF
    # Use the same xmin (xm_corrected) that was used for alpha estimation
    # to ensure consistency between the estimated alpha and theoretical CDF
    x_sorted <- sort(x)
    empirical_cdf <- (1:n) / n
    theoretical_cdf <- 1 - (xm_corrected / x_sorted)^(alpha - 1)

    ks_stat <- max(abs(empirical_cdf - theoretical_cdf))

    list(xmin = xm, alpha = alpha, ks = ks_stat, n = n)
  })

  # Find best xmin (minimum KS)
  ks_values <- sapply(results, function(r) if (is.na(r$alpha)) Inf else r$ks)
  best_idx <- which.min(ks_values)

  if (length(best_idx) == 0 || is.infinite(ks_values[best_idx])) {
    return(list(
      alpha = NA_real_,
      alpha_se = NA_real_,
      alpha_ci = c(NA_real_, NA_real_),
      xmin = NA_real_,
      n_tail = NA_integer_,
      n_total = n_total,
      pct_in_tail = NA_real_,
      ks_stat = NA_real_,
      gof_pvalue = NA_real_,
      ks_pvalue_approx = NA_character_
    ))
  }

  best <- results[[best_idx]]

  # Standard error of alpha (analytical Clauset/Hill SE at the fixed best xmin).
  # Used as a fallback when the bootstrap is unavailable.
  se_alpha <- (best$alpha - 1) / sqrt(best$n)

  # Bootstrap CI (Clauset et al. 2009): resample the FULL duration vector and
  # re-run the whole xmin-selection + alpha-estimation procedure on each
  # replicate, so the dominant source of variance (xmin selection) is
  # propagated rather than holding xmin fixed at the point estimate.
  alpha_ci <- c(NA_real_, NA_real_)
  if (bootstrap_n > 0 && n_total >= 10) {
    boot_alphas <- tryCatch({
      replicate(bootstrap_n, {
        boot_sample <- sample(durations, replace = TRUE)
        # bootstrap_n = 0 prevents infinite recursion; xmin is re-selected here.
        boot_fit <- .calculate.alpha.robust(boot_sample,
                                             xmin_candidates = xmin_candidates,
                                             bootstrap_n = 0)
        if (is.null(boot_fit$alpha)) NA_real_ else boot_fit$alpha
      })
    }, error = function(e) rep(NA_real_, bootstrap_n))

    boot_alphas <- boot_alphas[!is.na(boot_alphas)]
    if (length(boot_alphas) >= 10) {
      alpha_ci <- quantile(boot_alphas, c(0.025, 0.975))
      # Propagate xmin-selection variance into the reported SE.
      se_alpha <- sd(boot_alphas)
    }
  }

  # Asymptotic KS approximation (NOT a Clauset GoF p-value).
  # The 1.36/sqrt(n) critical value assumes a fully-specified null distribution;
  # because alpha and xmin are estimated from the same data this is
  # anti-conservative and must not be presented as a power-law GoF p-value.
  ks_critical_05 <- 1.36 / sqrt(best$n)
  ks_pvalue_approx <- if (best$ks < ks_critical_05) {
    "> 0.05 (KS approx)"
  } else {
    "< 0.05 (KS approx)"
  }

  # Optional Clauset (2009, Sec 4.2) semiparametric bootstrap GoF p-value.
  # Gated behind bootstrap_gof (default FALSE) to keep the dashboard fast.
  gof_pvalue <- NA_real_
  if (isTRUE(bootstrap_gof)) {
    gof_pvalue <- .powerlaw.gof.bootstrap(
      durations    = durations,
      xmin         = best$xmin,
      empirical_ks = best$ks,
      gof_bootstrap_n = gof_bootstrap_n,
      xmin_candidates = xmin_candidates
    )
  }

  list(
    alpha = round(best$alpha, 3),
    alpha_se = round(se_alpha, 3),
    alpha_ci = round(alpha_ci, 3),
    xmin = best$xmin,
    n_tail = best$n,
    n_total = n_total,
    pct_in_tail = round(100 * best$n / n_total, 1),
    ks_stat = round(best$ks, 4),
    gof_pvalue = if (is.na(gof_pvalue)) NA_real_ else round(gof_pvalue, 4),
    ks_pvalue_approx = ks_pvalue_approx
  )
}


#' Semiparametric Bootstrap Goodness-of-Fit for Power-Law Tail
#'
#' Implements the Clauset et al. (2009, Section 4.2) semiparametric Monte-Carlo
#' bootstrap goodness-of-fit test. For each of \code{gof_bootstrap_n} synthetic
#' datasets, observations below the fitted \code{xmin} are drawn (with replacement)
#' from the empirical body, while observations at/above \code{xmin} are drawn from
#' the fitted power-law tail; xmin and alpha are then RE-ESTIMATED on the synthetic
#' dataset and its KS statistic recorded. The returned p-value is the fraction of
#' synthetic KS statistics greater than or equal to the empirical KS statistic.
#' A large p-value (e.g. > 0.1) means the power-law is a plausible fit; a small
#' p-value means it can be ruled out.
#'
#' @param durations Numeric vector of observed bout durations (>0)
#' @param xmin Fitted lower bound of the power-law tail
#' @param empirical_ks Empirical KS statistic at the fitted xmin/alpha
#' @param gof_bootstrap_n Number of synthetic datasets (default 200)
#' @param xmin_candidates xmin grid reused when re-fitting each synthetic dataset
#'
#' @return Numeric p-value in [0, 1], or NA if it cannot be computed
#'
#' @references
#' Clauset A, Shalizi CR, Newman MEJ. (2009). Power-law distributions in
#' empirical data. SIAM Review, 51(4):661-703.
#'
#' @keywords internal
.powerlaw.gof.bootstrap <- function(durations, xmin, empirical_ks,
                                    gof_bootstrap_n = 200, xmin_candidates = NULL) {

  durations <- durations[!is.na(durations) & durations > 0]
  n_total <- length(durations)
  if (n_total < 10 || is.na(xmin) || is.na(empirical_ks)) {
    return(NA_real_)
  }

  body_vals <- durations[durations < xmin]   # empirical body (below xmin)
  tail_vals <- durations[durations >= xmin]   # power-law tail (at/above xmin)
  n_tail <- length(tail_vals)
  n_body <- length(body_vals)
  if (n_tail < 5) return(NA_real_)

  # Estimate the tail exponent at the fitted xmin (continuous correction, as in
  # the main estimator) to use as the generative model for the synthetic tail.
  xmin_corrected <- max(0.5, xmin - 0.5)
  log_sum <- sum(log(tail_vals / xmin_corrected))
  if (!is.finite(log_sum) || log_sum <= 0) return(NA_real_)
  alpha_fit <- 1 + n_tail / log_sum

  # Continuous power-law tail sampler via inverse-CDF transform:
  # for S(x) = (xmin_corrected / x)^(alpha-1), x = xmin_corrected * U^(-1/(alpha-1)).
  exponent <- alpha_fit - 1
  if (!is.finite(exponent) || exponent <= 0) return(NA_real_)
  p_tail <- n_tail / n_total  # probability an observation falls in the tail

  synth_ks <- replicate(gof_bootstrap_n, {
    # Choose, per observation, whether it comes from the tail or the body,
    # preserving the empirical tail/body mix (semiparametric step).
    in_tail <- stats::runif(n_total) < p_tail
    n_syn_tail <- sum(in_tail)
    n_syn_body <- n_total - n_syn_tail

    syn <- numeric(n_total)
    if (n_syn_tail > 0) {
      u <- stats::runif(n_syn_tail)
      syn[in_tail] <- xmin_corrected * u^(-1 / exponent)
    }
    if (n_syn_body > 0) {
      if (n_body > 0) {
        syn[!in_tail] <- sample(body_vals, n_syn_body, replace = TRUE)
      } else {
        # No empirical body: fall back to resampling the tail for these draws.
        syn[!in_tail] <- sample(tail_vals, n_syn_body, replace = TRUE)
      }
    }

    # Re-estimate xmin + alpha on the synthetic dataset and take its KS stat.
    fit <- .calculate.alpha.robust(syn, xmin_candidates = xmin_candidates,
                                   bootstrap_n = 0, bootstrap_gof = FALSE)
    if (is.null(fit$ks_stat) || is.na(fit$ks_stat)) NA_real_ else fit$ks_stat
  })

  synth_ks <- synth_ks[!is.na(synth_ks)]
  if (length(synth_ks) < 1) return(NA_real_)

  # p = fraction of synthetic KS statistics >= empirical KS statistic.
  mean(synth_ks >= empirical_ks)
}


#' Simple Power-Law Alpha (Hill Estimator)
#'
#' @param durations Numeric vector of bout durations
#' @param xmin Minimum value for fitting (default 1)
#'
#' @return Estimated alpha value
#'
#' @keywords internal
.calculate.alpha.simple <- function(durations, xmin = 1) {

  x <- durations[!is.na(durations) & durations >= xmin]
  n <- length(x)

  if (n < 2) return(NA_real_)

  log_sum <- sum(log(x / xmin))

  if (log_sum <= 0 || is.na(log_sum) || is.nan(log_sum)) return(NA_real_)

  round(1 + n / log_sum, 3)
}


#' Calculate Gini Coefficient
#'
#' Measures inequality in bout duration distribution.
#' 0 = perfect equality (all bouts same length)
#' 1 = perfect inequality (one bout contains all time)
#'
#' @param x Numeric vector of values
#' @param corr Logical; apply finite-sample bias correction? (default TRUE)
#'   Following GGIR methodology which uses ineq::Gini(corr = TRUE)
#'
#' @return Gini coefficient (0-1)
#'
#' @details
#' The formula used is:
#' \code{G = [2 * sum(i * x_i) - (n + 1) * sum(x)] / [n * sum(x)]}
#'
#' With bias correction (recommended for n < 100):
#' \code{G_corrected = G * n / (n - 1)}
#'
#' Reference: Chastin & Granat (2010), GGIR uses ineq::Gini(corr = TRUE)
#'
#' @keywords internal
.calculate.gini <- function(x, corr = TRUE) {

  x <- x[!is.na(x)]
  x <- sort(x)  # ASCENDING order per standard Gini formula

  n <- length(x)

  if (n < 2 || sum(x) == 0) return(NA_real_)

  # Gini formula: G = [2*sum(i*x_i) - (n+1)*sum(x)] / [n*sum(x)]
  numerator <- 2 * sum(seq_len(n) * x) - (n + 1) * sum(x)
  denominator <- n * sum(x)

  gini <- numerator / denominator

  # Finite-sample bias correction (matches GGIR/ineq package)
  # G_corrected = G * n / (n - 1)
  if (corr && n > 1) {
    gini <- gini * n / (n - 1)
  }

  # Ensure result is in valid range [0, 1]
  max(0, min(1, gini))
}


#' Compare Power-Law vs Exponential Distribution Fit
#'
#' Tests whether bout durations follow a power-law or exponential distribution
#' using a normalized likelihood-ratio (Vuong) test. The comparison is run over
#' the SAME tail (durations >= xmin) used to estimate the power-law exponent
#' alpha, per Clauset et al. (2009). Following Clauset, the "power_law" label is
#' only assigned when (a) the Vuong test is significant AND favours the power
#' law, AND (b) the power-law itself is a plausible fit to the tail; otherwise
#' the result is reported as "inconclusive".
#'
#' @param bout_durations Numeric vector of bout durations
#' @param bootstrap_gof Use the Clauset (2009) semiparametric bootstrap GoF test
#'   to decide whether the power-law is a plausible fit? (default FALSE, kept off
#'   for dashboard speed). When FALSE, plausibility is judged by the asymptotic KS
#'   approximation (anti-conservative); when TRUE, by the bootstrap p-value.
#'
#' @return List with model comparison results
#'
#' @details
#' Power-law: P(X >= x) ~ x^(-alpha+1) - "fat tail", many short + few very long
#' Exponential: P(X >= x) ~ exp(-lambda*x) - "memoryless", constant hazard
#'
#' @references
#' Clauset A, et al. (2009). SIAM Review, 51(4):661-703.
#'
#' @export
compare.bout.distributions <- function(bout_durations, bootstrap_gof = FALSE) {

  x_all <- bout_durations[!is.na(bout_durations) & bout_durations > 0]
  n_all <- length(x_all)

  if (n_all < 10) {
    return(list(
      best_model = NA_character_,
      power_law_alpha = NA_real_,
      exponential_lambda = NA_real_,
      log_likelihood_ratio = NA_real_,
      vuong_statistic = NA_real_,
      p_value = NA_real_,
      power_law_fits = NA,
      interpretation = "Insufficient data for comparison"
    ))
  }

  # Use the SAME xmin/tail selected for the alpha estimate (Clauset et al. 2009),
  # not min(x). Both candidate models are then compared on this common tail.
  alpha_fit <- .calculate.alpha.robust(x_all, bootstrap_n = 0,
                                       bootstrap_gof = bootstrap_gof)
  xmin <- if (!is.na(alpha_fit$xmin)) alpha_fit$xmin else min(x_all)

  x <- x_all[x_all >= xmin]
  n <- length(x)
  if (n < 10) {
    # Tail too small to compare reliably
    x <- x_all
    n <- n_all
    xmin <- min(x_all)
  }

  # Power-law MLE on the tail (with division-by-zero guard)
  log_sum <- sum(log(x / xmin))
  alpha <- if (abs(log_sum) > 1e-10) 1 + n / log_sum else NA_real_
  ll_power_law <- if (!is.na(alpha)) {
    n * log(alpha - 1) - n * log(xmin) - alpha * log_sum
  } else {
    NA_real_
  }

  # Exponential MLE on the same tail (shifted, with division-by-zero guard)
  mean_shifted <- mean(x - xmin)
  lambda <- if (abs(mean_shifted) > 1e-10) 1 / mean_shifted else NA_real_
  ll_exponential <- if (!is.na(lambda)) {
    n * log(lambda) - lambda * sum(x - xmin)
  } else {
    NA_real_
  }

  # Handle invalid log-likelihoods
  if (is.na(ll_power_law) || is.infinite(ll_power_law) ||
      is.na(ll_exponential) || is.infinite(ll_exponential)) {
    return(list(
      best_model = NA_character_,
      power_law_alpha = round(alpha, 3),
      exponential_lambda = round(lambda, 4),
      log_likelihood_ratio = NA_real_,
      vuong_statistic = NA_real_,
      p_value = NA_real_,
      power_law_fits = NA,
      interpretation = "Unable to compute valid likelihoods"
    ))
  }

  ll_ratio <- ll_power_law - ll_exponential

  # Per-observation log-likelihoods for the Vuong test (same tail for both)
  ll_pl_i <- log(alpha - 1) - log(xmin) - alpha * log(x / xmin)
  ll_exp_i <- log(lambda) - lambda * (x - xmin)

  diff_i <- ll_pl_i - ll_exp_i

  # Small-sample correction: use the unbiased SD of the per-observation LR
  # differences, and apply the Vuong parameter-count adjustment. Power-law and
  # exponential each have one free parameter, so the BIC-style correction term
  # (k_pl - k_exp) * log(n) / 2 is zero here; it is included explicitly so the
  # formula remains correct if the models gain parameters.
  k_pl <- 1L
  k_exp <- 1L
  correction <- (k_pl - k_exp) * log(n) / 2
  ll_ratio_corrected <- ll_ratio - correction

  sigma <- sd(diff_i)

  if (is.na(sigma) || sigma == 0) {
    vuong_stat <- 0
    p_value <- 1
  } else {
    # Normalized (per Clauset 2009 eqn 27): R / (sqrt(n) * sigma).
    vuong_stat <- ll_ratio_corrected / (sqrt(n) * sigma)
    p_value <- 2 * pnorm(-abs(vuong_stat))
  }

  # Does the power-law plausibly fit the tail? Require this before ever
  # labelling the result "power_law" (Clauset et al. 2009).
  if (bootstrap_gof && !is.na(alpha_fit$gof_pvalue)) {
    power_law_fits <- alpha_fit$gof_pvalue >= 0.10
  } else {
    # Asymptotic KS approximation fallback (anti-conservative): treat as a
    # "non-rejection" only when the approximate KS p-value is not significant.
    power_law_fits <- !is.na(alpha_fit$ks_pvalue_approx) &&
      grepl("^> 0.05", alpha_fit$ks_pvalue_approx)
  }

  # Determine best model: a significant Vuong result favouring the power law is
  # only reported as "power_law" if the power-law also passes the GoF check.
  if (p_value < 0.05) {
    if (ll_ratio_corrected > 0) {
      best_model <- if (isTRUE(power_law_fits)) "power_law" else "inconclusive"
    } else {
      best_model <- "exponential"
    }
  } else {
    best_model <- "inconclusive"
  }

  interpretation <- switch(best_model,
    "power_law" = "Bout durations follow power-law (fat tail: many short + few very long bouts)",
    "exponential" = "Bout durations follow exponential (memoryless: constant probability of ending)",
    "inconclusive" = if (p_value < 0.05 && ll_ratio_corrected > 0 && !isTRUE(power_law_fits)) {
      "Power-law favoured by LR but does not pass goodness-of-fit; treat as inconclusive"
    } else {
      "Cannot statistically distinguish between power-law and exponential"
    }
  )

  list(
    best_model = best_model,
    power_law_alpha = round(alpha, 3),
    power_law_ll = round(ll_power_law, 2),
    exponential_lambda = round(lambda, 5),
    exponential_ll = round(ll_exponential, 2),
    xmin = xmin,
    n_tail = n,
    log_likelihood_ratio = round(ll_ratio_corrected, 2),
    vuong_statistic = round(vuong_stat, 3),
    p_value = round(p_value, 4),
    power_law_fits = power_law_fits,
    interpretation = interpretation
  )
}


#' Sedentary Bout Survival Analysis
#'
#' Kaplan-Meier style survival curve and hazard function for bout termination.
#'
#' @param bout_durations Numeric vector of bout durations
#'
#' @return List with survival curve, median survival, and hazard rate
#'
#' @export
bout.survival.analysis <- function(bout_durations) {

  bout_durations <- bout_durations[!is.na(bout_durations)]
  n <- length(bout_durations)

  if (n < 5) {
    return(list(
      survival_curve = NULL,
      median_survival = NA_real_,
      mean_survival = NA_real_,
      hazard_rate = NA_real_,
      n_bouts = n
    ))
  }

  # Create survival curve (probability bout lasts > t minutes)
  sorted <- sort(bout_durations)
  unique_times <- unique(sorted)

  survival_curve <- data.frame(
    time = unique_times,
    n_at_risk = sapply(unique_times, function(t) sum(sorted >= t)),
    n_ended = sapply(unique_times, function(t) sum(sorted == t)),
    stringsAsFactors = FALSE
  )

  survival_curve$survival_prob <- survival_curve$n_at_risk / n

  # Hazard rate at each time point (with division by zero guard)
  survival_curve$hazard <- with(survival_curve, ifelse(n_at_risk > 0, n_ended / n_at_risk, NA_real_))

  # Median survival (time at which 50% of bouts have ended)
  median_survival <- tryCatch({
    approx(
      x = survival_curve$survival_prob,
      y = survival_curve$time,
      xout = 0.5,
      rule = 2
    )$y
  }, error = function(e) median(bout_durations))

  # Overall hazard rate (inverse of mean)
  hazard_rate <- 1 / mean(bout_durations)

  list(
    survival_curve = survival_curve,
    median_survival = round(median_survival, 1),
    mean_survival = round(mean(bout_durations), 1),
    hazard_rate = round(hazard_rate, 5),
    n_bouts = n
  )
}


#' Calculate Daily Fragmentation Patterns
#'
#' @keywords internal
.calculate.daily.fragmentation <- function(intensity, timestamps, wear_time, epoch_length,
                                           min_break_length = 5, sleep_mask = NULL) {

  dates <- as.Date(timestamps)
  unique_dates <- unique(dates)

  # Normalise the sleep mask once (same rule as detect.sedentary.bouts).
  is_sleep_all <- NULL
  if (!is.null(sleep_mask)) {
    is_sleep_all <- if (is.character(sleep_mask)) {
      sleep_mask %in% c("S", "sleep") | tolower(sleep_mask) %in% "sleep"
    } else {
      as.logical(sleep_mask)
    }
    is_sleep_all[is.na(is_sleep_all)] <- FALSE
  }

  daily_stats <- data.frame(
    date = as.character(unique_dates),
    sedentary_min = NA_real_,
    n_bouts = NA_integer_,
    mean_bout = NA_real_,
    SATP = NA_real_,
    breaks = NA_integer_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(unique_dates)) {
    d <- unique_dates[i]
    day_idx <- dates == d

    day_intensity <- intensity[day_idx]
    day_timestamps <- timestamps[day_idx]
    day_wear <- if (!is.null(wear_time)) wear_time[day_idx] else NULL
    day_sleep <- if (!is.null(is_sleep_all)) is_sleep_all[day_idx] else NULL

    # Get bouts for this day with the SAME bridging + sleep exclusion as the totals
    day_bouts <- tryCatch({
      detect.sedentary.bouts(day_intensity, day_timestamps, day_wear,
                             sleep_mask = day_sleep,
                             min_bout_length = 1, epoch_length = epoch_length,
                             min_break_length = min_break_length)
    }, error = function(e) NULL)

    if (is.null(day_bouts) || nrow(day_bouts) == 0) next

    # Calculate daily metrics (sedentary epochs over wear & waking time)
    is_sed <- as.character(day_intensity) == "sedentary"
    if (!is.null(day_wear)) is_sed <- is_sed & day_wear
    if (!is.null(day_sleep)) is_sed <- is_sed & !day_sleep

    daily_stats$sedentary_min[i] <- sum(is_sed) * (epoch_length / 60)
    daily_stats$n_bouts[i] <- nrow(day_bouts)
    daily_stats$mean_bout[i] <- round(mean(day_bouts$duration_min), 1)
    # SATP should be in epochs per Wanigatunga et al. (2019)
    # Convert mean bout from minutes to epochs: mean_min / (epoch_length/60)
    epoch_min <- epoch_length / 60
    mean_bout_epochs <- mean(day_bouts$duration_min) / epoch_min
    daily_stats$SATP[i] <- round(1 / mean_bout_epochs, 5)
    daily_stats$breaks[i] <- max(0, nrow(day_bouts) - 1)
  }

  daily_stats
}


#' Summarize Sedentary Breaks by Hour
#'
#' Calculates sedentary break statistics for each hour of the day.
#' Uses gap-bridging to match the main sedentary.fragmentation() function,
#' ensuring consistent break counts.
#'
#' @param intensity Factor or character vector of intensity classifications
#' @param timestamps POSIXct vector of timestamps
#' @param wear_time Optional logical vector indicating wear time
#' @param min_break_length Minimum duration (minutes) for a break to be counted.
#'   Shorter active periods are bridged as continued sedentary time. Default: 5
#' @param epoch_length Epoch length in seconds. Default: 60
#'
#' @return Data frame with hourly break statistics:
#'   \describe{
#'     \item{hour}{Hour of day (0-23)}
#'     \item{sedentary_min}{Total sedentary epochs in that hour}
#'     \item{breaks}{Number of valid breaks (>= min_break_length)}
#'     \item{wear_min}{Total wear time epochs in that hour}
#'     \item{breaks_per_hour}{Breaks per sedentary hour}
#'   }
#'
#' @seealso \code{\link{sedentary.fragmentation}} for full fragmentation analysis
#'
#' @export
sedentary.breaks.hourly <- function(intensity, timestamps, wear_time = NULL,
                                     min_break_length = 5, epoch_length = 60) {

  if (length(intensity) != length(timestamps)) {
    stop("intensity and timestamps must have same length")
  }

  df <- data.frame(
    hour = as.integer(format(timestamps, "%H")),
    is_sedentary = as.character(intensity) == "sedentary",
    stringsAsFactors = FALSE
  )

  if (!is.null(wear_time)) {
    df$is_sedentary <- df$is_sedentary & wear_time
    df$wear <- wear_time
  } else {
    df$wear <- TRUE
  }

  # Apply gap-bridging to match main sedentary.fragmentation() function

  # min_break_length specifies minimum duration (in minutes) for a break to be counted
  # Short active periods (< min_break_length) are bridged as continued sedentary behavior
  min_break_epochs <- ceiling(min_break_length * 60 / epoch_length)

  # Use run-length encoding for gap-bridging
  sed_rle <- rle(df$is_sedentary)
  bridged_sed <- df$is_sedentary

  # Bridge short non-sedentary periods (treat as continued sedentary)
  cum_idx <- 0
  for (i in seq_along(sed_rle$lengths)) {
    start_idx <- cum_idx + 1
    end_idx <- cum_idx + sed_rle$lengths[i]

    # If this is a non-sedentary bout shorter than or equal to min_break_epochs, bridge it
    #  Use <= to match detect.sedentary.bouts() for consistency
    if (!sed_rle$values[i] && sed_rle$lengths[i] <= min_break_epochs) {
      # Only bridge if surrounded by sedentary time (not at start/end)
      has_sed_before <- i > 1 && sed_rle$values[i - 1]
      has_sed_after <- i < length(sed_rle$values) && sed_rle$values[i + 1]
      if (has_sed_before && has_sed_after) {
        bridged_sed[start_idx:end_idx] <- TRUE
      }
    }
    cum_idx <- end_idx
  }

  # Detect transitions using bridged sedentary state (true breaks only)
  df$break_start <- c(FALSE, diff(bridged_sed) == -1)

  # Aggregate by hour
  hourly <- aggregate(
    cbind(sedentary_min = is_sedentary, breaks = break_start, wear_min = wear) ~ hour,
    data = df,
    FUN = sum
  )

  hourly$breaks_per_hour <- ifelse(
    hourly$sedentary_min > 0,
    hourly$breaks / (hourly$sedentary_min / 60),
    0
  )

  hourly
}


#' Activity Balance Index (ABI)
#'
#' Calculates the Activity Balance Index, a novel metric quantifying the ratio of
#' time spent in short sedentary bouts vs prolonged sedentary bouts. Based on
#' emerging research showing that the pattern of sedentary accumulation matters
#' as much as total sedentary time.
#'
#' @param bout_durations Numeric vector of bout durations (minutes)
#' @param short_threshold Threshold for "short" bouts (default 10 min)
#' @param long_threshold Threshold for "long" bouts (default 30 min)
#'
#' @return List with ABI value and interpretation:
#'   \describe{
#'     \item{ABI}{Activity Balance Index (0-1, higher = better balance)}
#'     \item{short_time_pct}{Percent of sedentary time in short bouts}
#'     \item{long_time_pct}{Percent of sedentary time in long bouts}
#'     \item{interpretation}{Qualitative interpretation}
#'   }
#'
#' @details
#' ABI = short_time / (short_time + long_time)
#'
#' Where short_time = total time in bouts < short_threshold
#' and long_time = total time in bouts >= long_threshold
#'
#' Interpretation:
#' \itemize{
#'   \item ABI > 0.6: Good balance (sedentary time well distributed)
#'   \item ABI 0.4-0.6: Moderate balance
#'   \item ABI < 0.4: Poor balance (concentrated in prolonged bouts)
#' }
#'
#' @references
#' Danilevicz IM, et al. (2024). Activity balance and cardiovascular health.
#' Journal of Physical Activity and Health.
#'
#' @export
activity.balance.index <- function(bout_durations,
                                    short_threshold = 10,
                                    long_threshold = 30) {

  if (length(bout_durations) == 0 || all(is.na(bout_durations))) {
    return(list(
      ABI = NA_real_,
      short_time_min = NA_real_,
      long_time_min = NA_real_,
      short_time_pct = NA_real_,
      long_time_pct = NA_real_,
      interpretation = "Insufficient data"
    ))
  }

  bout_durations <- bout_durations[!is.na(bout_durations)]
  total_time <- sum(bout_durations)

  if (total_time == 0) {
    return(list(
      ABI = NA_real_,
      short_time_min = 0,
      long_time_min = 0,
      short_time_pct = 0,
      long_time_pct = 0,
      interpretation = "No sedentary time"
    ))
  }

  # Calculate time in short and long bouts
  short_time <- sum(bout_durations[bout_durations < short_threshold])
  long_time <- sum(bout_durations[bout_durations >= long_threshold])

  # ABI: ratio of short time to short + long time
  denominator <- short_time + long_time
  ABI <- if (denominator > 0) short_time / denominator else 0.5  # Neutral if no extreme bouts

  # Interpretation
  interpretation <- if (ABI > 0.6) {
    "Good balance (well-distributed sedentary time)"
  } else if (ABI >= 0.4) {
    "Moderate balance"
  } else {
    "Poor balance (concentrated in prolonged bouts)"
  }

  list(
    ABI = round(ABI, 3),
    short_time_min = round(short_time, 1),
    long_time_min = round(long_time, 1),
    short_time_pct = round(100 * short_time / total_time, 1),
    long_time_pct = round(100 * long_time / total_time, 1),
    interpretation = interpretation
  )
}


#' Sleep Regularity Index for Sedentary Patterns
#'
#' Adapts the Sleep Regularity Index (Phillips et al., 2017) to sedentary behavior,
#' measuring day-to-day consistency in sedentary patterns.
#'
#' @param intensity Factor or character vector of intensity classifications
#' @param timestamps POSIXct vector of timestamps
#' @param wear_time Optional logical vector
#' @param lag_hours Lag for comparison in hours (default 24)
#'
#' @return Sedentary Regularity Index (-1 to 1, higher = more regular)
#'
#' @details
#' SRI_sed = 200 * mean(same_state) - 100
#'
#' Where same_state indicates whether the sedentary/active state at time t
#' matches the state at time t - lag_hours.
#'
#' @references
#' Phillips AJK, et al. (2017). Irregular sleep/wake patterns are associated
#' with poorer academic performance. Scientific Reports, 7:3216.
#'
#' @export
sedentary.regularity.index <- function(intensity, timestamps, wear_time = NULL,
                                        lag_hours = 24) {

  n <- length(intensity)
  if (n < 2) return(NA_real_)

  # Convert to sedentary binary
  is_sedentary <- as.character(intensity) == "sedentary"
  if (!is.null(wear_time)) {
    is_sedentary[!wear_time] <- NA
  }

  # Determine epoch length
  time_diff <- as.numeric(difftime(timestamps[2], timestamps[1], units = "secs"))
  epochs_per_lag <- round(lag_hours * 3600 / time_diff)

  if (epochs_per_lag >= n) {
    return(NA_real_)
  }

  # Calculate proportion of matching states
  current <- is_sedentary[(epochs_per_lag + 1):n]
  lagged <- is_sedentary[1:(n - epochs_per_lag)]

  # Remove pairs where either is NA
  valid <- !is.na(current) & !is.na(lagged)
  if (sum(valid) < 10) return(NA_real_)

  same_state <- current[valid] == lagged[valid]

  # SRI formula: scale from 0-1 to -100 to 100
  sri <- 200 * mean(same_state) - 100

  round(sri, 1)
}


#' Extended Survival Analysis with Weibull Distribution
#'
#' Fits Weibull distribution to bout durations and calculates survival metrics.
#' Weibull is more flexible than exponential, with shape parameter indicating
#' whether bout termination probability increases, decreases, or stays constant.
#'
#' @param bout_durations Numeric vector of bout durations
#'
#' @return List with Weibull parameters and interpretations:
#'   \describe{
#'     \item{shape}{Weibull shape parameter (k); NA if the MLE did not converge}
#'     \item{scale}{Weibull scale parameter (lambda); NA if the MLE did not converge}
#'     \item{median_survival}{Median survival time from Weibull}
#'     \item{mean_survival}{Mean survival time from Weibull}
#'     \item{converged}{Logical; did the shape MLE root-finder converge?}
#'     \item{hazard_interpretation}{Interpretation of hazard function}
#'   }
#'
#' @details
#' The shape parameter k is the root of the Weibull profile score equation
#' \deqn{g(k) = \frac{\sum x^k \log x}{\sum x^k} - \frac{1}{k} - \frac{1}{n}\sum \log x = 0,}
#' which is monotonically increasing in k. It is solved with a bracketed
#' root-finder (\code{\link[stats]{uniroot}}) rather than a fixed-point map. If a
#' root cannot be bracketed/found, shape and scale are returned as NA (with
#' \code{converged = FALSE}) instead of being silently forced to the exponential
#' (k = 1) case.
#'
#' Weibull shape parameter interpretation:
#' \itemize{
#'   \item k < 1: Decreasing hazard (longer bouts more likely to continue)
#'   \item k = 1: Constant hazard (exponential, memoryless)
#'   \item k > 1: Increasing hazard (longer bouts more likely to end)
#' }
#'
#' @references
#' Chastin SFM, Granat MH. (2010). Gait & Posture, 31(1):82-86.
#'
#' @export
survival.weibull <- function(bout_durations) {

  bout_durations <- bout_durations[!is.na(bout_durations) & bout_durations > 0]
  n <- length(bout_durations)

  if (n < 10) {
    return(list(
      shape = NA_real_,
      scale = NA_real_,
      median_survival = NA_real_,
      mean_survival = NA_real_,
      converged = FALSE,
      hazard_interpretation = "Insufficient data"
    ))
  }

  sum_logx <- sum(log(bout_durations))
  mean_logx <- sum_logx / n

  # Weibull shape MLE: root of the (monotone increasing) profile score
  #   g(k) = sum(x^k log x) / sum(x^k) - 1/k - mean(log x)
  # Solve with a bracketed root-finder (uniroot) on the profile score.
  weibull_score <- function(k) {
    xk <- bout_durations^k
    sum_xk <- sum(xk)
    if (!is.finite(sum_xk) || sum_xk <= 0) return(NA_real_)
    sum(xk * log(bout_durations)) / sum_xk - 1 / k - mean_logx
  }

  # Method-of-moments initial guess to seed the bracket.
  cv <- stats::sd(bout_durations) / mean(bout_durations)
  k_init <- if (is.finite(cv) && cv > 0) max(0.2, min(10, 1 / cv)) else 1

  # Expand a bracket [lo, hi] around the initial guess until the score changes
  # sign (g is increasing, so a sign change brackets the unique root).
  lo <- max(1e-3, k_init / 4)
  hi <- min(50, k_init * 4)
  g_lo <- weibull_score(lo)
  g_hi <- weibull_score(hi)
  expand <- 0L
  while (expand < 40 && (is.na(g_lo) || is.na(g_hi) || g_lo * g_hi > 0)) {
    if (!is.na(g_lo) && g_lo > 0) {
      lo <- max(1e-4, lo / 2)
      g_lo <- weibull_score(lo)
    } else if (!is.na(g_hi) && g_hi < 0) {
      hi <- min(200, hi * 2)
      g_hi <- weibull_score(hi)
    } else {
      # One endpoint is NA: nudge both inward/outward and retry.
      lo <- max(1e-4, lo / 2)
      hi <- min(200, hi * 2)
      g_lo <- weibull_score(lo)
      g_hi <- weibull_score(hi)
    }
    expand <- expand + 1L
  }

  k <- NA_real_
  converged <- FALSE
  if (!is.na(g_lo) && !is.na(g_hi) && g_lo * g_hi <= 0) {
    root <- tryCatch(
      stats::uniroot(weibull_score, lower = lo, upper = hi, tol = 1e-6),
      error = function(e) NULL
    )
    if (!is.null(root) && is.finite(root$root) && root$root > 0) {
      k <- root$root
      converged <- TRUE
    }
  }

  # Propagate NA on failure rather than silently substituting k = 1.
  if (!converged || is.na(k)) {
    return(list(
      shape = NA_real_,
      scale = NA_real_,
      median_survival = NA_real_,
      mean_survival = NA_real_,
      converged = FALSE,
      hazard_interpretation = "Weibull shape MLE did not converge"
    ))
  }

  # Scale parameter (closed form given k)
  lambda <- (sum(bout_durations^k) / n)^(1 / k)

  # Derived metrics
  median_survival <- lambda * (log(2))^(1 / k)
  mean_survival <- lambda * gamma(1 + 1 / k)

  # Hazard interpretation
  hazard_interp <- if (k < 0.9) {
    "Decreasing hazard (longer bouts tend to continue)"
  } else if (k > 1.1) {
    "Increasing hazard (longer bouts more likely to end)"
  } else {
    "Approximately constant hazard (memoryless)"
  }

  list(
    shape = round(k, 3),
    scale = round(lambda, 2),
    median_survival = round(median_survival, 1),
    mean_survival = round(mean_survival, 1),
    converged = TRUE,
    hazard_interpretation = hazard_interp
  )
}


#' Compare Bout Survival Curves Across Subjects/Conditions
#'
#' Creates a comparison of survival curves from multiple fragmentation results,
#' useful for comparing subjects or conditions.
#'
#' @param ... Named canhrActi_fragmentation objects to compare
#' @param labels Optional character vector of labels
#'
#' @return A data frame with combined survival curves for plotting
#'
#' @export
compare.survival.curves <- function(..., labels = NULL) {

  frag_list <- list(...)
  n_subjects <- length(frag_list)

  if (n_subjects == 0) {
    stop("At least one fragmentation result required")
  }

  # Get labels
  if (is.null(labels)) {
    labels <- names(frag_list)
    if (is.null(labels)) {
      labels <- paste0("Subject_", seq_len(n_subjects))
    }
  }

  # Combine survival curves
  combined <- NULL
  for (i in seq_len(n_subjects)) {
    frag <- frag_list[[i]]

    if (is.null(frag$survival_curve) || nrow(frag$survival_curve) == 0) {
      next
    }

    sc <- frag$survival_curve
    sc$subject <- labels[i]
    sc$W50 <- frag$W50
    sc$SATP <- frag$SATP

    combined <- rbind(combined, sc)
  }

  if (is.null(combined)) {
    stop("No valid survival curves found")
  }

  combined
}


#' Hourly Fragmentation Pattern
#'
#' Calculates sedentary fragmentation metrics for each hour of the day,
#' revealing temporal patterns in sedentary accumulation. Bouts are detected on
#' the FULL series (with the same gap bridging as
#' \code{detect.sedentary.bouts()}) and each bout is attributed to the hour of
#' its START, so a bout that spans an hour boundary is counted in exactly one
#' hour (matching plot_hourly_heatmap). This avoids the previous behaviour where
#' boundary-spanning bouts were counted in both hours, inflating n_bouts and the
#' fragmentation_index.
#'
#' @param intensity Factor or character vector of intensity classifications
#' @param timestamps POSIXct vector of timestamps
#' @param wear_time Optional logical vector
#' @param epoch_length Epoch length in seconds (default 60)
#' @param min_break_length Minimum break duration in minutes for gap bridging
#'   (default 5), passed through to \code{detect.sedentary.bouts()}.
#'
#' @return Data frame with hourly fragmentation metrics:
#'   \describe{
#'     \item{hour}{Hour of day (0-23)}
#'     \item{sedentary_min}{Total sedentary time}
#'     \item{n_bouts}{Number of sedentary bouts STARTING in that hour}
#'     \item{mean_bout_duration}{Mean duration of bouts starting in that hour}
#'     \item{fragmentation_index}{SATP-like index: bouts-started / sedentary epochs}
#'   }
#'
#' @export
hourly.fragmentation.pattern <- function(intensity, timestamps, wear_time = NULL,
                                          epoch_length = 60, min_break_length = 5) {

  n <- length(intensity)
  if (n == 0) {
    return(data.frame(
      hour = 0:23,
      sedentary_min = rep(0, 24),
      n_bouts = rep(0L, 24),
      mean_bout_duration = rep(NA_real_, 24),
      fragmentation_index = rep(NA_real_, 24)
    ))
  }

  hours <- as.integer(format(timestamps, "%H"))
  epoch_min <- epoch_length / 60

  # Per-epoch sedentary classification (wear-masked) for hourly sedentary TIME.
  is_sedentary <- as.character(intensity) == "sedentary"
  if (!is.null(wear_time)) {
    is_sedentary[!wear_time] <- NA
  }

  # Initialize results
  results <- data.frame(
    hour = 0:23,
    sedentary_min = 0,
    n_bouts = 0L,
    mean_bout_duration = NA_real_,
    fragmentation_index = NA_real_,
    stringsAsFactors = FALSE
  )

  # Sedentary epochs per hour (per-epoch sum; never double-counts).
  for (h in 0:23) {
    sed_epochs <- sum(hours == h & is_sedentary, na.rm = TRUE)
    results$sedentary_min[h + 1] <- sed_epochs * epoch_min
  }

  # Detect bouts ONCE on the full series (with gap bridging) and attribute each
  # to the hour of its start, so boundary-spanning bouts are counted once.
  bouts <- tryCatch(
    detect.sedentary.bouts(intensity, timestamps, wear_time = wear_time,
                           min_bout_length = 1, epoch_length = epoch_length,
                           min_break_length = min_break_length),
    error = function(e) NULL
  )

  if (!is.null(bouts) && nrow(bouts) > 0) {
    start_hours <- as.integer(format(bouts$start_time, "%H"))
    for (h in 0:23) {
      in_hour <- start_hours == h
      n_bouts <- sum(in_hour)
      results$n_bouts[h + 1] <- n_bouts

      sed_epochs <- results$sedentary_min[h + 1] / epoch_min
      if (n_bouts > 0) {
        results$mean_bout_duration[h + 1] <- mean(bouts$duration_min[in_hour])
      }
      if (sed_epochs > 0) {
        # SATP-like fragmentation index: bouts started per sedentary epoch.
        results$fragmentation_index[h + 1] <- n_bouts / sed_epochs
      }
    }
  }

  results
}


#' Create Empty Bouts Data Frame
#' @keywords internal
.create.empty.bouts.df <- function() {
  data.frame(
    bout_id = integer(0),
    start_time = as.POSIXct(character(0)),
    end_time = as.POSIXct(character(0)),
    duration_min = numeric(0),
    start_index = integer(0),
    end_index = integer(0),
    stringsAsFactors = FALSE
  )
}


#' Create Empty Fragmentation Result
#' @keywords internal
.create.empty.fragmentation.result <- function(total_sedentary_min, total_wear_min, timestamps) {

  n_days <- length(unique(as.Date(timestamps)))

  result <- list(
    total_sedentary_min = total_sedentary_min,
    total_sedentary_hours = round(total_sedentary_min / 60, 2),
    total_wear_min = total_wear_min,
    total_wear_hours = round(total_wear_min / 60, 2),
    sedentary_percent = if (total_wear_min > 0) round(100 * total_sedentary_min / total_wear_min, 1) else NA_real_,
    n_days_analyzed = n_days,
    total_bouts = 0L,
    mean_bout_duration = NA_real_,
    median_bout_duration = NA_real_,
    sd_bout_duration = NA_real_,
    max_bout_duration = NA_real_,
    ASTP = NA_real_,
    SATP = NA_real_,
    mean_active_bout = NA_real_,
    median_active_bout = NA_real_,
    n_active_bouts = NA_integer_,
    total_transitions = NA_integer_,
    W50 = NA_real_,
    W25 = NA_real_,
    W75 = NA_real_,
    W90 = NA_real_,
    breaks_total = 0L,
    breaks_per_sed_hour = NA_real_,
    alpha = NA_real_,
    alpha_se = NA_real_,
    alpha_ci_lower = NA_real_,
    alpha_ci_upper = NA_real_,
    alpha_xmin = NA_real_,
    alpha_n_tail = NA_integer_,
    alpha_pct_in_tail = NA_real_,
    alpha_ks_stat = NA_real_,
    alpha_gof_pvalue = NA_real_,
    alpha_ks_pvalue_approx = NA_character_,
    gini = NA_real_,
    pct_time_20min_bouts = 0,
    pct_time_30min_bouts = 0,
    pct_time_60min_bouts = 0,
    prolonged_summary = data.frame(
      threshold = c(20, 30, 60),
      n_bouts = rep(0L, 3),
      pct_bouts = rep(0, 3),
      total_time_min = rep(0, 3),
      pct_time = rep(0, 3)
    ),
    median_bout_survival = NA_real_,
    hazard_rate = NA_real_,
    survival_curve = NULL,
    weibull_shape = NA_real_,
    weibull_scale = NA_real_,
    weibull_hazard = NA_character_,
    sedentary_regularity_index = NA_real_,
    distribution_fit = NULL,
    daily_fragmentation = NULL,
    bout_distribution = data.frame(
      category = c("1-5 min", "5-10 min", "10-20 min", "20-30 min", "30-60 min", ">60 min"),
      count = rep(0L, 6),
      percent = rep(0, 6),
      mid_value = c(2.5, 7.5, 15, 25, 45, 90),
      stringsAsFactors = FALSE
    ),
    bouts = .create.empty.bouts.df(),
    analysis_method = "canhrActi_v2_fragmentation"
  )

  class(result) <- c("canhrActi_fragmentation", "list")
  result
}


#' Print Method for Fragmentation Results
#'
#' @param x A canhrActi_fragmentation object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.canhrActi_fragmentation <- function(x, ...) {

  cat("\nSedentary Fragmentation Analysis\n\n")

  cat("Data Summary\n")
  cat(sprintf("  Days analyzed:            %d\n", x$n_days_analyzed))
  cat(sprintf("  Total wear time:          %.1f hours\n", x$total_wear_hours))
  cat(sprintf("  Total sedentary time:     %.1f hours (%.1f%%)\n",
              x$total_sedentary_hours, x$sedentary_percent))

  cat("\nBout Statistics\n")
  cat(sprintf("  Number of bouts:          %d\n", x$total_bouts))
  cat(sprintf("  Mean bout duration:       %.1f min (SD: %.1f)\n", x$mean_bout_duration, x$sd_bout_duration))
  cat(sprintf("  Median bout duration:     %.1f min\n", x$median_bout_duration))
  cat(sprintf("  Max bout duration:        %.1f min\n", x$max_bout_duration))

  cat("\nTransition Probabilities (Wanigatunga et al., 2019)\n")
  cat(sprintf("  ASTP (Active->Sedentary): %.5f (higher=more fragmented activity)\n", x$ASTP))
  cat(sprintf("  SATP (Sedentary->Active): %.5f (higher=more fragmented sedentary)\n", x$SATP))
  cat(sprintf("  Mean active bout:         %.1f epochs\n", x$mean_active_bout))
  cat(sprintf("  Active bouts:             %d\n", x$n_active_bouts))
  cat(sprintf("  Total transitions:        %d\n", x$total_transitions))

  cat("\nUsual Bout Duration - Weighted Percentiles (Chastin & Granat, 2010)\n")
  cat(sprintf("  W25 (25th weighted pctl): %.1f min\n", x$W25))
  cat(sprintf("  W50 (usual bout dur):     %.1f min\n", x$W50))
  cat(sprintf("  W75 (75th weighted pctl): %.1f min\n", x$W75))
  cat(sprintf("  W90 (90th weighted pctl): %.1f min\n", x$W90))

  cat("\nBreaks in Sedentary Time (Healy et al., 2008)\n")
  min_break <- if (!is.null(x$min_break_length_used)) x$min_break_length_used else 5
  cat(sprintf("  Min break threshold:      %d min (active periods < this are bridged)\n", min_break))
  cat(sprintf("  Total breaks:             %d\n", x$breaks_total))
  cat(sprintf("  Breaks per sed hour:      %.2f\n", x$breaks_per_sed_hour))
  # Interpretation
  interp <- if (is.na(x$breaks_per_sed_hour)) "N/A"
            else if (x$breaks_per_sed_hour < 1) "Very Sedentary"
            else if (x$breaks_per_sed_hour < 2) "Sedentary"
            else if (x$breaks_per_sed_hour <= 5) "Typical"
            else "Active"
  cat(sprintf("  Interpretation:           %s (<1=Very Sed, 1-2=Sed, 2-5=Typical, >5=Active)\n", interp))

  cat("\nDistribution Shape (Clauset et al., 2009)\n")
  if (!is.na(x$alpha)) {
    cat(sprintf("  Alpha (power-law):        %.3f (SE: %.3f)\n", x$alpha, x$alpha_se))
    cat(sprintf("  Alpha 95%% CI:             [%.3f, %.3f]\n", x$alpha_ci_lower, x$alpha_ci_upper))
    cat(sprintf("  Optimal xmin:             %.1f min\n", x$alpha_xmin))
    cat(sprintf("  Data in tail:             %.1f%% (%d bouts)\n", x$alpha_pct_in_tail, x$alpha_n_tail))
    cat(sprintf("  KS statistic:             %.4f\n", x$alpha_ks_stat))
  } else {
    cat("  Alpha: Not calculated (insufficient data)\n")
  }
  cat(sprintf("  Gini coefficient:         %.4f (0=equal, 1=unequal bouts)\n", x$gini))

  if (!is.null(x$distribution_fit) && !is.na(x$distribution_fit$best_model)) {
    cat(sprintf("\n  Distribution comparison:  %s\n", x$distribution_fit$best_model))
    cat(sprintf("  Interpretation:           %s\n", x$distribution_fit$interpretation))
  }

  cat("\nProlonged Sedentary Time\n")
  cat(sprintf("  Time in bouts >= 20 min:  %.1f%% of sedentary time\n", x$pct_time_20min_bouts))
  cat(sprintf("  Time in bouts >= 30 min:  %.1f%% of sedentary time\n", x$pct_time_30min_bouts))
  cat(sprintf("  Time in bouts >= 60 min:  %.1f%% of sedentary time\n", x$pct_time_60min_bouts))

  cat("\nSurvival Analysis\n")
  cat(sprintf("  Median bout survival:     %.1f min (time for 50%% of bouts to end)\n", x$median_bout_survival))
  cat(sprintf("  Mean bout survival:       %.1f min\n", x$mean_bout_duration))
  cat(sprintf("  Hazard rate:              %.5f per epoch\n", x$hazard_rate))

  cat("\nBout Duration Distribution\n")
  print(x$bout_distribution, row.names = FALSE)

  cat("\nReferences: Chastin (2010), Wanigatunga (2019), Clauset (2009), Healy (2008)\n\n")

  invisible(x)
}


#' Plot Sedentary Fragmentation
#'
#' Creates publication-quality visualizations of sedentary bout patterns.
#'
#' @param x A canhrActi_fragmentation object
#' @param type Type of plot: "distribution" (default), "survival", "accumulation", "histogram"
#' @param show_metrics Logical; whether to display metrics on the plot (default TRUE)
#' @param show_powerlaw Logical; whether to show power-law fit line (default TRUE)
#' @param show_reference Logical; whether to show reference lines (default TRUE)
#' @param colorblind_safe Logical; whether to use colorblind-friendly palette (default FALSE)
#' @param ... Additional arguments
#'
#' @return A ggplot2 object
#'
#' @export
plot.canhrActi_fragmentation <- function(x, type = "distribution",
                                          show_metrics = TRUE,
                                          show_powerlaw = TRUE,
                                          show_reference = TRUE,
                                          colorblind_safe = FALSE,
                                          ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting")
  }

  # Color palette (colorblind-safe option)
  if (colorblind_safe) {
    primary_color <- "#0072B2"    # Blue
    secondary_color <- "#D55E00"  # Vermillion
    accent_color <- "#009E73"     # Green
  } else {
    primary_color <- "#236192"
    secondary_color <- "#E74C3C"
    accent_color <- "#27AE60"
  }

  # Helper: Interpret fragmentation metrics
  interpret_satp <- function(satp) {
    if (satp < 0.20) return("Very Low (prolonged sedentary)")
    if (satp < 0.35) return("Low")
    if (satp < 0.50) return("Moderate")
    if (satp < 0.65) return("High")
    return("Very High (frequent breaks)")
  }

  interpret_alpha <- function(alpha) {
    if (alpha < 1.5) return("Low fragmentation")
    if (alpha < 2.0) return("Moderate fragmentation")
    if (alpha < 2.5) return("High fragmentation")
    return("Very high fragmentation")
  }

  if (type == "distribution") {
    df <- x$bout_distribution
    df$category <- factor(df$category, levels = df$category)

    # Add time contribution
    if (!"total_time" %in% names(df)) {
      df$total_time <- df$count * df$mid_value  # Approximate
    }
    df$time_pct <- df$total_time / sum(df$total_time, na.rm = TRUE) * 100

    # Create dual-axis label
    df$label_text <- sprintf("%d\n(%.0f%%)", df$count, df$time_pct)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = category, y = count, fill = count)) +
      ggplot2::geom_col(alpha = 0.9, color = "white", linewidth = 0.5) +
      ggplot2::scale_fill_gradient(low = primary_color, high = secondary_color,
                                   guide = "none") +
      ggplot2::geom_text(ggplot2::aes(label = count), vjust = -0.3, size = 3.5,
                         fontface = "bold")

    # Add SATP/ASTP metrics box
    if (show_metrics) {
      satp_val <- if (!is.null(x$SATP)) x$SATP else NA
      astp_val <- if (!is.null(x$ASTP)) x$ASTP else NA

      metrics_label <- sprintf(
        "SATP: %.3f (%s)\nASTP: %.3f\nW50: %.1f min",
        satp_val, interpret_satp(satp_val),
        astp_val, x$W50
      )

      p <- p +
        ggplot2::annotate("label", x = Inf, y = Inf,
                          label = metrics_label,
                          hjust = 1.1, vjust = 1.1,
                          size = 3, fill = "white", alpha = 0.9,
                          linewidth = 0.5)
    }

    p <- p +
      ggplot2::labs(
        title = "Sedentary Bout Duration Distribution",
        subtitle = sprintf("N=%d bouts | Mean=%.1f min | Median=%.1f min | Total: %.0f min sedentary",
                          x$total_bouts, x$mean_bout_duration,
                          if (!is.null(x$median_bout_duration)) x$median_bout_duration else x$W50,
                          x$total_sedentary_min),
        x = "Bout Duration Category",
        y = "Number of Bouts",
        caption = "Bar height = bout count | Darker = more bouts"
      ) +
      theme_canhrActi() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(color = "gray50"),
        plot.caption = ggplot2::element_text(color = "gray60", size = 9),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    return(p)
  }

  if (type == "survival") {
    if (is.null(x$survival_curve) || nrow(x$survival_curve) == 0) {
      stop("No survival curve data available")
    }

    sc <- x$survival_curve
    median_surv <- if (!is.null(x$median_bout_survival)) x$median_bout_survival else x$W50

    p <- ggplot2::ggplot(sc, ggplot2::aes(x = time, y = survival_prob)) +
      ggplot2::geom_step(color = primary_color, linewidth = 1.2)

    # Add confidence band if available
    if ("lower" %in% names(sc) && "upper" %in% names(sc)) {
      p <- p +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                             alpha = 0.2, fill = primary_color)
    }

    # Add reference lines for clinical thresholds
    if (show_reference) {
      p <- p +
        # 30-minute prolonged sedentary threshold
        ggplot2::geom_vline(xintercept = 30, linetype = "dotted",
                            color = "orange", alpha = 0.7) +
        ggplot2::annotate("text", x = 30, y = 0.95,
                          label = "30 min\n(prolonged)", hjust = -0.1,
                          size = 2.5, color = "orange")
    }

    # Median marker with W50
    p <- p +
      ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed",
                          color = secondary_color, alpha = 0.7) +
      ggplot2::geom_vline(xintercept = median_surv, linetype = "dashed",
                          color = secondary_color, alpha = 0.7) +
      ggplot2::geom_point(ggplot2::aes(x = median_surv, y = 0.5),
                          color = secondary_color, size = 4) +
      ggplot2::annotate("label", x = median_surv, y = 0.5,
                        label = sprintf("W50 = %.1f min", median_surv),
                        hjust = -0.1, vjust = 0.5, size = 3.5,
                        fill = "white", alpha = 0.9)

    # Add SATP annotation
    if (show_metrics && !is.null(x$SATP)) {
      p <- p +
        ggplot2::annotate("label", x = Inf, y = 0.05,
                          label = sprintf("SATP: %.3f\n%s",
                                          x$SATP, interpret_satp(x$SATP)),
                          hjust = 1.1, size = 3, fill = accent_color,
                          color = "white", fontface = "bold")
    }

    p <- p +
      ggplot2::scale_y_continuous(limits = c(0, 1),
                                  labels = function(x) paste0(x * 100, "%")) +
      ggplot2::labs(
        title = "Sedentary Bout Survival Curve",
        subtitle = sprintf("Probability that a bout lasts longer than t minutes (N=%d bouts)",
                          x$total_bouts),
        x = "Time (minutes)",
        y = "Survival Probability P(Bout > t)",
        caption = "W50 = median bout duration where 50% of sedentary time is accumulated"
      ) +
      theme_canhrActi() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(color = "gray50"),
        plot.caption = ggplot2::element_text(color = "gray60", size = 9)
      )

    return(p)
  }

  if (type == "accumulation") {
    if (nrow(x$bouts) == 0) {
      stop("No bouts available for accumulation plot")
    }

    bouts_sorted <- x$bouts[order(x$bouts$duration_min, decreasing = TRUE), ]
    bouts_sorted$cumulative_time <- cumsum(bouts_sorted$duration_min)
    bouts_sorted$cumulative_percent <- bouts_sorted$cumulative_time / x$total_sedentary_min * 100
    bouts_sorted$bout_rank <- seq_len(nrow(bouts_sorted))
    bouts_sorted$bout_percent <- bouts_sorted$bout_rank / nrow(bouts_sorted) * 100

    # Find W50 position on curve
    w50_idx <- which.min(abs(bouts_sorted$cumulative_percent - 50))
    w50_x <- bouts_sorted$bout_percent[w50_idx]

    p <- ggplot2::ggplot(bouts_sorted, ggplot2::aes(x = bout_percent, y = cumulative_percent)) +
      # Equality line (perfect equality)
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                           color = "gray50", alpha = 0.7) +
      # Shaded area for Gini coefficient visualization
      ggplot2::geom_ribbon(ggplot2::aes(ymin = bout_percent, ymax = cumulative_percent),
                           fill = primary_color, alpha = 0.3) +
      # Main curve
      ggplot2::geom_line(color = primary_color, linewidth = 1.2)

    # W50 marker
    p <- p +
      ggplot2::geom_hline(yintercept = 50, linetype = "dotted",
                          color = secondary_color, alpha = 0.7) +
      ggplot2::geom_vline(xintercept = w50_x, linetype = "dotted",
                          color = secondary_color, alpha = 0.7) +
      ggplot2::geom_point(ggplot2::aes(x = w50_x, y = 50),
                          color = secondary_color, size = 4) +
      ggplot2::annotate("label", x = w50_x, y = 50,
                        label = sprintf("%.0f%% of bouts\naccount for 50%%\nof sedentary time", w50_x),
                        hjust = if (w50_x < 50) -0.1 else 1.1, vjust = 0.5,
                        size = 2.8, fill = "white")

    # Gini interpretation
    gini_interp <- if (x$gini < 0.3) "Low inequality" else
                   if (x$gini < 0.5) "Moderate inequality" else
                   if (x$gini < 0.7) "High inequality" else "Very high inequality"

    p <- p +
      ggplot2::annotate("label", x = 75, y = 25,
                        label = sprintf("Gini = %.3f\n(%s)\n\nShaded area\n= inequality", x$gini, gini_interp),
                        size = 3, fill = "white", alpha = 0.9)

    p <- p +
      ggplot2::labs(
        title = "Sedentary Time Accumulation Curve (Lorenz Curve)",
        subtitle = sprintf("Gini=%.3f | W50=%.1f min | Higher Gini = more inequality in bout durations",
                          x$gini, x$W50),
        x = "% of Bouts (longest to shortest)",
        y = "% of Total Sedentary Time",
        caption = "Dashed line = perfect equality (all bouts same length)"
      ) +
      ggplot2::scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
      ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
      ggplot2::coord_fixed() +
      theme_canhrActi() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(color = "gray50"),
        plot.caption = ggplot2::element_text(color = "gray60", size = 9)
      )

    return(p)
  }

  if (type == "histogram" || type == "powerlaw") {
    if (nrow(x$bouts) == 0) {
      stop("No bouts available for histogram")
    }

    # Create histogram with density overlay
    p <- ggplot2::ggplot(x$bouts, ggplot2::aes(x = duration_min)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                              binwidth = 5, fill = primary_color, alpha = 0.7,
                              color = "white", linewidth = 0.3)

    # Add power-law fit if alpha is available and show_powerlaw is TRUE
    if (show_powerlaw && !is.null(x$alpha) && x$alpha > 1) {
      # Generate power-law curve
      x_range <- seq(1, max(x$bouts$duration_min, na.rm = TRUE), length.out = 100)
      # Power-law: P(x) ~ x^(-alpha)
      # Normalized for visualization
      alpha <- x$alpha
      x_min <- min(x$bouts$duration_min[x$bouts$duration_min > 0], na.rm = TRUE)

      # Power-law probability density
      powerlaw_y <- (alpha - 1) / x_min * (x_range / x_min)^(-alpha)

      # Scale to match histogram density
      scale_factor <- max(x$bouts$duration_min) / 100  # Approximate scaling
      powerlaw_df <- data.frame(
        x = x_range,
        y = powerlaw_y * scale_factor
      )

      p <- p +
        ggplot2::geom_line(data = powerlaw_df, ggplot2::aes(x = x, y = y),
                           color = secondary_color, linewidth = 1.2, linetype = "dashed") +
        ggplot2::annotate("text", x = max(x_range) * 0.7, y = max(powerlaw_df$y) * 0.8,
                          label = sprintf("Power-law fit\nalpha = %.2f", alpha),
                          color = secondary_color, size = 3.5, fontface = "bold")
    }

    # W50 marker with interpretation
    w50_interp <- if (x$W50 < 10) "Short bouts (fragmented)" else
                  if (x$W50 < 20) "Moderate bout length" else
                  if (x$W50 < 30) "Long bouts" else "Very long bouts (prolonged sedentary)"

    p <- p +
      ggplot2::geom_vline(xintercept = x$W50, linetype = "solid",
                          color = accent_color, linewidth = 1.2) +
      ggplot2::annotate("label", x = x$W50, y = Inf, vjust = 1.5,
                        label = sprintf("W50 = %.1f min\n(%s)", x$W50, w50_interp),
                        color = "white", fill = accent_color, size = 3,
                        fontface = "bold")

    # Add alpha interpretation
    if (show_metrics && !is.null(x$alpha)) {
      p <- p +
        ggplot2::annotate("label", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1,
                          label = sprintf("Alpha: %.2f\n%s", x$alpha, interpret_alpha(x$alpha)),
                          size = 3, fill = "white", alpha = 0.9)
    }

    p <- p +
      ggplot2::labs(
        title = "Sedentary Bout Duration Distribution with Power-Law Fit",
        subtitle = sprintf("Alpha=%.2f | W50=%.1f min | N=%d bouts | SATP=%.3f",
                          if (!is.null(x$alpha)) x$alpha else NA,
                          x$W50, x$total_bouts,
                          if (!is.null(x$SATP)) x$SATP else NA),
        x = "Bout Duration (minutes)",
        y = "Density",
        caption = "Higher alpha = steeper decay = more fragmented pattern"
      ) +
      theme_canhrActi() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(color = "gray50"),
        plot.caption = ggplot2::element_text(color = "gray60", size = 9)
      )

    return(p)
  }

  if (type == "transition" || type == "astp") {
    # ASTP/SATP visualization - transition probabilities
    if (is.null(x$ASTP) || is.null(x$SATP)) {
      stop("ASTP/SATP values not available in fragmentation results")
    }

    # Create transition probability matrix visualization
    trans_data <- data.frame(
      from = c("Active", "Active", "Sedentary", "Sedentary"),
      to = c("Active", "Sedentary", "Active", "Sedentary"),
      prob = c(1 - x$ASTP, x$ASTP, x$SATP, 1 - x$SATP)
    )
    trans_data$label <- sprintf("%.1f%%", trans_data$prob * 100)

    # Create heatmap
    p <- ggplot2::ggplot(trans_data, ggplot2::aes(x = to, y = from, fill = prob)) +
      ggplot2::geom_tile(color = "white", linewidth = 2) +
      ggplot2::geom_text(ggplot2::aes(label = label), size = 6, fontface = "bold",
                         color = ifelse(trans_data$prob > 0.5, "white", "black")) +
      ggplot2::scale_fill_gradient2(low = "#3498DB", mid = "#F39C12", high = "#E74C3C",
                                    midpoint = 0.5, limits = c(0, 1),
                                    name = "Transition\nProbability") +
      ggplot2::labs(
        title = "Activity State Transition Probabilities",
        subtitle = sprintf("ASTP=%.3f (active->sed) | SATP=%.3f (sed->active)",
                          x$ASTP, x$SATP),
        x = "Transition To",
        y = "Current State",
        caption = sprintf("Higher SATP = more sedentary breaks | Interpretation: %s",
                         interpret_satp(x$SATP))
      ) +
      ggplot2::coord_fixed() +
      theme_canhrActi() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = ggplot2::element_text(color = "gray50", hjust = 0.5),
        plot.caption = ggplot2::element_text(color = "gray60", size = 9),
        legend.position = "right",
        panel.grid = ggplot2::element_blank()
      )

    return(p)
  }

  if (type == "hourly") {
    # Hourly fragmentation pattern visualization
    if (!is.null(x$bouts) && nrow(x$bouts) > 0) {
      # Calculate hourly pattern from bout data
      hours <- as.integer(format(x$bouts$start_time, "%H"))
      hourly_data <- data.frame(
        hour = 0:23,
        n_bouts = sapply(0:23, function(h) sum(hours == h)),
        total_duration = sapply(0:23, function(h) {
          sum(x$bouts$duration_min[hours == h], na.rm = TRUE)
        }),
        stringsAsFactors = FALSE
      )
      hourly_data$mean_duration <- hourly_data$total_duration / pmax(hourly_data$n_bouts, 1)
      hourly_data$fragmentation <- hourly_data$n_bouts / pmax(hourly_data$total_duration, 1)
    } else {
      stop("No bout data available for hourly plot")
    }

    p <- ggplot2::ggplot(hourly_data, ggplot2::aes(x = hour)) +
      ggplot2::geom_col(ggplot2::aes(y = n_bouts, fill = mean_duration),
                        alpha = 0.8, color = "white", linewidth = 0.3) +
      ggplot2::scale_fill_gradient2(low = accent_color, mid = "white", high = secondary_color,
                                    midpoint = median(hourly_data$mean_duration, na.rm = TRUE),
                                    name = "Mean Bout\n(min)") +
      ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 3),
                                  labels = sprintf("%02d:00", seq(0, 23, by = 3))) +
      ggplot2::geom_hline(yintercept = mean(hourly_data$n_bouts), linetype = "dashed",
                          color = "gray50", alpha = 0.7)

    # Add morning/afternoon/evening shading
    p <- p +
      ggplot2::annotate("rect", xmin = 6, xmax = 12, ymin = -Inf, ymax = Inf,
                        fill = "#FFD700", alpha = 0.1) +
      ggplot2::annotate("rect", xmin = 18, xmax = 23, ymin = -Inf, ymax = Inf,
                        fill = "#4169E1", alpha = 0.1) +
      ggplot2::annotate("text", x = 9, y = max(hourly_data$n_bouts) * 0.95,
                        label = "Morning", size = 3, alpha = 0.5) +
      ggplot2::annotate("text", x = 20.5, y = max(hourly_data$n_bouts) * 0.95,
                        label = "Evening", size = 3, alpha = 0.5)

    p <- p +
      ggplot2::labs(
        title = "Hourly Sedentary Bout Pattern",
        subtitle = sprintf("Darker bars = longer average bout duration | Total: %d bouts",
                          x$total_bouts),
        x = "Hour of Day",
        y = "Number of Bouts",
        caption = "Yellow = morning | Blue = evening"
      ) +
      theme_canhrActi() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(color = "gray50"),
        plot.caption = ggplot2::element_text(color = "gray60", size = 9)
      )

    return(p)
  }

  stop("Unknown plot type. Use 'distribution', 'survival', 'accumulation', 'histogram', 'powerlaw', 'transition', 'astp', or 'hourly'")
}


#' Plot Power-Law vs Exponential Distribution Comparison
#'
#' Creates a visualization comparing power-law and exponential fits to
#' bout duration data, with log-log and semi-log views.
#'
#' @param frag A canhrActi_fragmentation object
#' @param show_fits Logical; show fitted curves? (default TRUE)
#' @param log_scale Logical; use log-log scale? (default TRUE)
#' @param colorblind_safe Logical; use colorblind-friendly palette? (default FALSE)
#'
#' @return A ggplot2 object
#'
#' @export
plot_distribution_comparison <- function(frag, show_fits = TRUE, log_scale = TRUE,
                                          colorblind_safe = FALSE) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting")
  }

  if (is.null(frag$bouts) || nrow(frag$bouts) == 0) {
    stop("No bout data available")
  }

  # Colors
  if (colorblind_safe) {
    pl_color <- "#0072B2"   # Blue
    exp_color <- "#D55E00"  # Vermillion
    data_color <- "#009E73" # Green
  } else {
    pl_color <- "#236192"
    exp_color <- "#E74C3C"
    data_color <- "#2ECC71"
  }

  durations <- frag$bouts$duration_min[frag$bouts$duration_min > 0]
  n <- length(durations)

  # Create empirical CDF data
  sorted_d <- sort(durations)
  ecdf_data <- data.frame(
    duration = sorted_d,
    survival = 1 - (seq_len(n) - 0.5) / n,  # Plotting position
    type = "Empirical"
  )

  # Get distribution comparison
  dist_fit <- frag$distribution_fit
  if (is.null(dist_fit)) {
    dist_fit <- compare.bout.distributions(durations)
  }

  # Generate theoretical curves
  x_range <- seq(min(durations), max(durations), length.out = 200)
  xmin <- min(durations)

  # Power-law survival function: S(x) = (xmin/x)^(alpha-1)
  alpha <- dist_fit$power_law_alpha
  if (!is.na(alpha) && alpha > 1) {
    pl_survival <- (xmin / x_range)^(alpha - 1)
    pl_data <- data.frame(
      duration = x_range,
      survival = pl_survival,
      type = "Power-Law"
    )
  } else {
    pl_data <- NULL
  }

  # Exponential survival function: S(x) = exp(-lambda * (x - xmin))
  lambda <- dist_fit$exponential_lambda
  if (!is.na(lambda) && lambda > 0) {
    exp_survival <- exp(-lambda * (x_range - xmin))
    exp_data <- data.frame(
      duration = x_range,
      survival = exp_survival,
      type = "Exponential"
    )
  } else {
    exp_data <- NULL
  }

  # Combine data
  plot_data <- ecdf_data
  if (show_fits && !is.null(pl_data)) {
    plot_data <- rbind(plot_data, pl_data)
  }
  if (show_fits && !is.null(exp_data)) {
    plot_data <- rbind(plot_data, exp_data)
  }

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = duration, y = survival,
                                                color = type, linetype = type)) +
    ggplot2::geom_step(data = subset(plot_data, type == "Empirical"),
                       linewidth = 1.2, alpha = 0.8)

  if (show_fits) {
    if (!is.null(pl_data)) {
      p <- p + ggplot2::geom_line(data = subset(plot_data, type == "Power-Law"),
                                  linewidth = 1.2)
    }
    if (!is.null(exp_data)) {
      p <- p + ggplot2::geom_line(data = subset(plot_data, type == "Exponential"),
                                  linewidth = 1.2)
    }
  }

  p <- p +
    ggplot2::scale_color_manual(
      values = c("Empirical" = data_color, "Power-Law" = pl_color, "Exponential" = exp_color),
      name = "Distribution"
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Empirical" = "solid", "Power-Law" = "dashed", "Exponential" = "dotted"),
      name = "Distribution"
    )

  # Apply log scale if requested
  if (log_scale) {
    p <- p +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_log10()
  }

  # Add model comparison annotation
  best_model <- if (!is.null(dist_fit$best_model)) dist_fit$best_model else "unknown"
  p_value <- if (!is.null(dist_fit$p_value)) dist_fit$p_value else NA

  annotation_label <- sprintf(
    "Best fit: %s\nVuong p-value: %.4f\nAlpha: %.2f | Lambda: %.4f",
    best_model, p_value,
    if (!is.na(alpha)) alpha else NA,
    if (!is.na(lambda)) lambda else NA
  )

  p <- p +
    ggplot2::annotate("label", x = max(durations) * 0.8, y = 0.8,
                      label = annotation_label, size = 3, hjust = 1,
                      fill = "white", alpha = 0.9)

  p <- p +
    ggplot2::labs(
      title = "Bout Duration Distribution: Power-Law vs Exponential",
      subtitle = sprintf("N=%d bouts | %s | SATP=%.3f",
                        n, dist_fit$interpretation, frag$SATP),
      x = if (log_scale) "Bout Duration (minutes, log scale)" else "Bout Duration (minutes)",
      y = if (log_scale) "Survival Probability (log scale)" else "Survival Probability P(X > x)",
      caption = "Power-law: fat-tail (few long bouts) | Exponential: memoryless (constant end probability)"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(color = "gray50"),
      plot.caption = ggplot2::element_text(color = "gray60", size = 9),
      legend.position = "bottom"
    )

  p
}


#' Plot Hourly Fragmentation Heatmap
#'
#' Creates a heatmap visualization of sedentary fragmentation by hour of day
#' and day of recording.
#'
#' @param frag A canhrActi_fragmentation object
#' @param metric Which metric to display: "bout_count", "duration", or "fragmentation"
#'
#' @return A ggplot2 object
#'
#' @export
plot_hourly_heatmap <- function(frag, metric = "bout_count") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting")
  }

  if (is.null(frag$bouts) || nrow(frag$bouts) == 0) {
    stop("No bout data available")
  }

  bouts <- frag$bouts
  bouts$hour <- as.integer(format(bouts$start_time, "%H"))
  bouts$date <- as.Date(bouts$start_time)

  # Aggregate by hour and date
  hourly_daily <- aggregate(
    cbind(bout_count = duration_min, total_duration = duration_min) ~ hour + date,
    data = bouts,
    FUN = function(x) c(length(x), sum(x))
  )

  # Flatten the aggregation
  hourly_data <- data.frame(
    hour = hourly_daily$hour,
    date = hourly_daily$date,
    bout_count = sapply(hourly_daily$bout_count, function(x) x[1]),
    total_duration = sapply(hourly_daily$total_duration, function(x) x[2])
  )
  hourly_data$fragmentation <- hourly_data$bout_count / pmax(hourly_data$total_duration, 1)

  # Select metric
  if (metric == "bout_count") {
    hourly_data$value <- hourly_data$bout_count
    legend_title <- "Bouts"
    subtitle <- "Number of sedentary bouts per hour"
  } else if (metric == "duration") {
    hourly_data$value <- hourly_data$total_duration
    legend_title <- "Duration\n(min)"
    subtitle <- "Total sedentary duration per hour"
  } else {
    hourly_data$value <- hourly_data$fragmentation
    legend_title <- "Fragmentation"
    subtitle <- "Bouts per minute of sedentary time (higher = more fragmented)"
  }

  p <- ggplot2::ggplot(hourly_data, ggplot2::aes(x = hour, y = date, fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::scale_fill_gradient(low = "white", high = "#E74C3C", name = legend_title) +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 3),
                                labels = sprintf("%02d:00", seq(0, 23, by = 3))) +
    ggplot2::labs(
      title = "Hourly Sedentary Pattern Heatmap",
      subtitle = subtitle,
      x = "Hour of Day",
      y = "Date",
      caption = "Darker = higher value"
    ) +
    theme_canhrActi() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(color = "gray50"),
      axis.text.y = ggplot2::element_text(size = 8)
    )

  p
}
