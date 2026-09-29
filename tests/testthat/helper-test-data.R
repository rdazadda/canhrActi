create.test.counts.data <- function(n = 1440, seed = 1) {
  withr::with_seed(seed, data.frame(
    timestamp = seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = n),
    axis1 = sample(0:5000, n, replace = TRUE),
    axis2 = sample(0:4000, n, replace = TRUE),
    axis3 = sample(0:3000, n, replace = TRUE),
    steps = sample(0:150, n, replace = TRUE),
    stringsAsFactors = FALSE
  ))
}

create.nonwear.pattern <- function(n = 1440, nonwear.length = 120, seed = 1) {
  counts <- rep(500, n)
  nonwear.start <- withr::with_seed(seed, sample(100:(n-nonwear.length-100), 1))
  counts[nonwear.start:(nonwear.start + nonwear.length - 1)] <- 0
  counts
}

create.sleep.pattern <- function(n = 1440) {
  counts <- c(
    rep(500, 180),
    rep(10, 600),
    rep(500, 180),
    rep(10, 480)
  )
  counts[1:min(n, length(counts))]
}
