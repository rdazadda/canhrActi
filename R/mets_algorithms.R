#' Calculate METs (Metabolic Equivalent of Task)
#'
#' Converts ActiGraph activity counts to METs using validated prediction equations.
#'
#' @param counts_data Data frame from agd.counts() with axis1, axis2, axis3, timestamp
#' @param algorithm Character. METs prediction algorithm (see Details)
#' @param subject_info List with age and body_mass (required for some algorithms)
#' @param epoch_length Epoch length in seconds (default: 60). Counts are automatically
#'   scaled to CPM (counts per minute) if epoch_length != 60.
#' @param verbose Logical. Print processing messages? (default: FALSE)
#'
#' @return Numeric vector of METs values for each epoch
#'
#' @details
#' Available algorithms:
#' \itemize{
#'   \item \code{"freedson.adult"} - Freedson Adult (1998)
#'   \item \code{"freedson.vm3"} - Freedson VM3 (Sasaki 2011, default)
#'   \item \code{"crouter"} - Crouter Adult (2010)
#'   \item \code{"hendelman.adult"} - Hendelman Adult Overground (2000)
#'   \item \code{"hendelman.lifestyle"} - Hendelman Adult Overground and Lifestyle (2000)
#'   \item \code{"swartz"} - Swartz Adult Overground and Lifestyle (2000)
#'   \item \code{"leenders"} - Leenders Adult Treadmill (2003)
#'   \item \code{"yngve.treadmill"} - Yngve Adult Treadmill (2003)
#'   \item \code{"yngve.overground"} - Yngve Adult Overground (2003)
#'   \item \code{"brooks.overground"} - Brooks Adult Overground (2005)
#'   \item \code{"brooks.bm"} - Brooks Adult Body Mass & Overground (2005)
#'   \item \code{"freedson.children"} - Freedson Children (2005)
#' }
#'
#' @references
#' See ActiGraph documentation for complete references
#'
#' @export
calculate.mets <- function(counts_data,
                           algorithm = c("freedson.vm3", "freedson.adult", "crouter",
                                       "hendelman.adult", "hendelman.lifestyle", "swartz",
                                       "leenders", "yngve.treadmill", "yngve.overground",
                                       "brooks.overground", "brooks.bm", "freedson.children"),
                           subject_info = NULL,
                           epoch_length = 60,
                           verbose = FALSE) {

  algorithm <- match.arg(algorithm)

  # Validate input
  if (!is.data.frame(counts_data)) {
    stop("counts_data must be a data frame")
  }

  required_cols <- c("axis1", "axis2", "axis3")
  missing_cols <- setdiff(required_cols, names(counts_data))
  if (length(missing_cols) > 0) {
    stop("counts_data must contain columns: ", paste(missing_cols, collapse = ", "))
  }

  if (nrow(counts_data) == 0) {
    stop("counts_data is empty (0 rows)")
  }

  # Scale counts to CPM (counts per minute) if epoch_length != 60
  # Most METs algorithms were validated using 1-minute (60-second) epochs
  scale_factor <- 60 / epoch_length
  if (scale_factor != 1) {
    counts_data <- data.frame(
      axis1 = counts_data$axis1 * scale_factor,
      axis2 = counts_data$axis2 * scale_factor,
      axis3 = counts_data$axis3 * scale_factor,
      timestamp = counts_data$timestamp
    )
    if (verbose) {
      cat("Scaling counts to CPM (factor:", scale_factor, ")\n")
    }
  }

  if (verbose) {
    cat("Calculating METs using", algorithm, "algorithm\n")
  }

  # Dispatch to appropriate algorithm function
  mets <- switch(algorithm,
    "freedson.vm3" = mets.freedson.vm3(counts_data),
    "freedson.adult" = mets.freedson.adult(counts_data, subject_info),
    "crouter" = mets.crouter(counts_data, verbose = verbose),
    "hendelman.adult" = mets.hendelman.adult(counts_data),
    "hendelman.lifestyle" = mets.hendelman.lifestyle(counts_data),
    "swartz" = mets.swartz(counts_data),
    "leenders" = mets.leenders(counts_data),
    "yngve.treadmill" = mets.yngve.treadmill(counts_data),
    "yngve.overground" = mets.yngve.overground(counts_data),
    "brooks.overground" = mets.brooks.overground(counts_data),
    "brooks.bm" = mets.brooks.bm(counts_data, subject_info),
    "freedson.children" = mets.freedson.children(counts_data, subject_info),
    stop("Unknown algorithm: ", algorithm)
  )

  mets[mets < 0] <- 0
  mets[is.na(mets)] <- 0

  return(mets)
}


mets.freedson.vm3 <- function(counts_data) {
  # Freedson VM3 METs equation from Sasaki et al. (2011)
  # Reference: Sasaki JE, John D, Freedson PS. Validation and comparison of
  # ActiGraph activity monitors. J Sci Med Sport. 2011;14(5):411-416.
  #
  # Published Sasaki (2011) VM3 MET prediction equation:
  #   METs = 0.000863 * VM3 + 0.668876
  # where VM3 is the triaxial vector magnitude in counts/min. The earlier
  # 0.001064 slope was the kcal energy-expenditure coefficient (not the MET
  # slope) and 0.6569 did not match any published source.
  vm <- vm(counts_data$axis1, counts_data$axis2, counts_data$axis3)
  mets <- 0.000863 * vm + 0.668876
  return(mets)
}


mets.freedson.adult <- function(counts_data, subject_info) {

  cpm <- counts_data$axis1

  # Published Freedson (1998) MET prediction equation (body-mass independent):
  #   METs = 1.439008 + 0.000795 * counts/min (vertical axis)
  # This is the canonical Freedson MET regression (also what ActiLife reports
  # as Freedson METs), distinct from the 1998 kcal work-energy equation.
  # Reference: Freedson PS et al. (1998). Med Sci Sports Exerc. 30(5):777-781
  mets <- 1.439008 + 0.000795 * cpm

  # Ensure minimum of 1.0 MET (resting)
  mets[mets < 1.0] <- 1.0

  return(mets)
}


mets.crouter <- function(counts_data, verbose = FALSE) {
  # Crouter 2-Regression Algorithm (2010)
  #
  # IMPORTANT: The original Crouter algorithm was validated using 10-second epochs.

  # The CV is calculated over a 1-minute window (6 x 10-second epochs).
  # When using 1-minute epochs (standard for canhrActi), this implementation
  # calculates CV over a 6-minute sliding window to approximate the original
  # algorithm's behavior.
  #
  # For exact replication of Crouter's method, use 10-second epoch data.
  #
  # Reference: Crouter SE et al. (2010). Med Sci Sports Exerc. 42(5):1029-1037
  #
  # Algorithm:
  # 1. Calculate coefficient of variation (CV) over sliding window
  # 2. If CV <= 10%: locomotion equation (exponential)
  # 3. If CV > 10%: lifestyle equation (polynomial)
  # 4. If counts per 10-sec <= 8: assign 1.0 MET (sedentary)

  n_epochs <- nrow(counts_data)
  mets <- numeric(n_epochs)
  cpm <- counts_data$axis1

  # Convert CPM to counts per 10-second equivalent for formula compatibility
  # CPM / 6 approximates what a 10-second epoch would show
  cpe <- cpm / 6

  if (verbose) {
    cat("Crouter 2-regression: Processing", n_epochs, "epochs\n")
    cat("Note: Using 1-minute epochs with 6-epoch sliding window for CV\n")
  }

  for (i in 1:n_epochs) {
    # Use a centered 6-epoch window for CV calculation
    # This represents ~6 minutes when using 1-minute epochs. NOTE: the true
    # Crouter CV requires the six 10-second epochs within a minute and is not
    # recoverable from 60-second data; this is a documented approximation (the
    # 2010 regression coefficients below are exact - see the header note).
    start_idx <- max(1, i - 2)
    end_idx <- min(n_epochs, i + 3)
    window_size <- end_idx - start_idx + 1

    if (window_size < 3) {
      # Not enough data for CV, use counts-only prediction
      mets[i] <- mets.crouter.single.epoch(cpe[i])
      next
    }

    # Calculate CV for the window
    window <- cpe[start_idx:end_idx]
    window_mean <- mean(window, na.rm = TRUE)
    window_sd <- sd(window, na.rm = TRUE)

    if (window_mean > 0) {
      cv <- (window_sd / window_mean) * 100
    } else {
      cv <- 0
    }

    mets[i] <- mets.crouter.single.epoch(cpe[i], cv = cv)
  }

  return(mets)
}


mets.crouter.single.epoch <- function(cpe, cv = NULL) {
  if (cpe <= 8) {
    return(1.0)
  }

  if (is.null(cv)) {
    cv <- 100
  }

  if (cv <= 10) {
    mets <- 2.294275 * exp(0.00084679 * cpe)
  } else {
    log_cpe <- log(cpe)
    mets <- 0.749395 + (0.716431 * log_cpe) - (0.179874 * log_cpe^2) + (0.033173 * log_cpe^3)
  }

  return(mets)
}


mets.hendelman.adult <- function(counts_data) {
  cpm <- counts_data$axis1
  mets <- 1.602 + (0.000638 * cpm)
  return(mets)
}


mets.hendelman.lifestyle <- function(counts_data) {
  cpm <- counts_data$axis1
  mets <- 2.922 + (0.000409 * cpm)
  return(mets)
}


mets.swartz <- function(counts_data) {
  cpm <- counts_data$axis1
  mets <- 2.606 + (0.0006863 * cpm)
  return(mets)
}


mets.leenders <- function(counts_data) {
  cpm <- counts_data$axis1
  mets <- 2.240 + (0.0006 * cpm)
  return(mets)
}


mets.yngve.treadmill <- function(counts_data) {
  cpm <- counts_data$axis1
  # Yngve et al. (2003) treadmill equation
  # Reference: Yngve A et al. (2003). Med Sci Sports Exerc. 35(4):635-640
  mets <- 0.751 + (0.0008198 * cpm)
  return(mets)
}


mets.yngve.overground <- function(counts_data) {
  cpm <- counts_data$axis1
  # Yngve et al. (2003) overground equation differs from treadmill
  # Reference: Yngve A et al. (2003). Med Sci Sports Exerc. 35(4):635-640
  mets <- 1.136 + (0.0008249 * cpm)
  return(mets)
}


mets.brooks.overground <- function(counts_data) {
  cpm <- counts_data$axis1
  mets <- 2.32 + (0.000389 * cpm)
  return(mets)
}


mets.brooks.bm <- function(counts_data, subject_info) {
  body_mass <- extract.body.mass(subject_info)
  cpm <- counts_data$axis1
  mets <- 3.33 + (0.000370 * cpm) - (0.012 * body_mass)
  return(mets)
}


mets.freedson.children <- function(counts_data, subject_info) {
  if (is.null(subject_info$age) || is.na(subject_info$age) || subject_info$age <= 0) {
    stop("Freedson Children algorithm requires subject age")
  }

  age <- subject_info$age
  cpm <- counts_data$axis1
  mets <- 2.757 + (0.0015 * cpm) - (0.08957 * age) - (0.000038 * cpm * age)

  return(mets)
}


#' Classify METs into Intensity Categories
#'
#' Converts METs values into standard physical activity intensity levels.
#'
#' @param mets Numeric vector of METs values
#' @return Factor vector with intensity categories (sedentary, light, moderate, vigorous, very_vigorous)
#'
#' @export
classify.mets <- function(mets) {
  intensity <- cut(mets,
                   breaks = c(-Inf, 1.5, 3, 6, 9, Inf),
                   labels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"),
                   right = FALSE)
  return(intensity)
}
