create.test.counts.data <- function(n = 1440) {
  data.frame(
    timestamp = seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = n),
    axis1 = sample(0:5000, n, replace = TRUE),
    axis2 = sample(0:4000, n, replace = TRUE),
    axis3 = sample(0:3000, n, replace = TRUE),
    steps = sample(0:150, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

create.test.agd.data <- function(n = 1440) {
  list(
    data = data.frame(
      dataTimestamp = seq(
        as.numeric(as.POSIXct("2024-01-01 00:00:00", tz = "UTC")) * 10000000 + 62135596800 * 10000000,
        by = 600000000,
        length.out = n
      ),
      axis1 = sample(0:5000, n, replace = TRUE),
      axis2 = sample(0:4000, n, replace = TRUE),
      axis3 = sample(0:3000, n, replace = TRUE),
      steps = sample(0:150, n, replace = TRUE)
    ),
    settings = data.frame(
      settingName = c("subjectname", "sex", "age", "height", "mass"),
      settingValue = c("TestSubject", "Male", "35", "175", "70")
    )
  )
}

create.sedentary.pattern <- function(n = 1440) {
  counts <- rep(0, n)
  active.periods <- sample(1:(n-60), 5)
  for (start in active.periods) {
    counts[start:(start+30)] <- sample(100:500, 31, replace = TRUE)
  }
  counts
}

create.active.pattern <- function(n = 1440) {
  counts <- rep(2000, n)
  sedentary.periods <- sample(1:(n-60), 3)
  for (start in sedentary.periods) {
    counts[start:(start+30)] <- sample(0:50, 31, replace = TRUE)
  }
  counts
}

create.nonwear.pattern <- function(n = 1440, nonwear.length = 120) {
  counts <- rep(500, n)
  nonwear.start <- sample(100:(n-nonwear.length-100), 1)
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

vm <- function(axis1, axis2, axis3) {
  sqrt(axis1^2 + axis2^2 + axis3^2)
}

extract.body.mass <- function(subject.info) {
  if (is.null(subject.info) || is.null(subject.info$body_mass)) {
    if (is.null(subject.info$mass)) {
      return(70)
    }
    return(subject.info$mass)
  }
  subject.info$body_mass
}
