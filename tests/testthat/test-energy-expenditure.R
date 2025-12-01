test_that("calculate.energy.expenditure converts METs to kcal correctly", {
  mets <- c(3.0, 4.0, 5.0)
  body.mass <- 70
  result <- calculate.energy.expenditure(mets, body.mass, epoch_length = 60)

  expected.kcal.per.min <- mets * 3.5 * body.mass / 200
  expect_equal(result$kcal_per_min, expected.kcal.per.min, tolerance = 0.01)
})

test_that("calculate.energy.expenditure calculates total kcal correctly", {
  mets <- rep(3.0, 60)
  body.mass <- 70
  result <- calculate.energy.expenditure(mets, body.mass, epoch_length = 60)

  expected.total <- sum(mets * 3.5 * body.mass / 200)
  expect_equal(result$total_kcal, expected.total, tolerance = 0.01)
})

test_that("calculate.energy.expenditure handles different epoch lengths", {
  mets <- c(3.0)
  body.mass <- 70
  result30 <- calculate.energy.expenditure(mets, body.mass, epoch_length = 30)
  result60 <- calculate.energy.expenditure(mets, body.mass, epoch_length = 60)

  expect_equal(result30$kcal_per_epoch, result60$kcal_per_epoch / 2, tolerance = 0.01)
})

test_that("calculate.energy.expenditure warns for missing body mass", {
  mets <- c(3.0)
  expect_warning(calculate.energy.expenditure(mets, body_mass = NULL), "70 kg")
})

test_that("calculate.energy.expenditure validates body mass range", {
  mets <- c(3.0)
  expect_error(calculate.energy.expenditure(mets, body_mass = 0), "Invalid body mass")
  expect_error(calculate.energy.expenditure(mets, body_mass = 400), "Invalid body mass")
})

test_that("calculate.energy.expenditure uses default body mass", {
  mets <- c(3.0)
  expect_warning(result <- calculate.energy.expenditure(mets, body_mass = NA))
  expect_true(result$total_kcal > 0)
})

test_that("calculate.energy.expenditure.direct uses williams algorithm", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 0, axis3 = 0)
  body.mass <- 70
  result <- calculate.energy.expenditure.direct(counts.data, body.mass, algorithm = "williams")

  expected.kcal.per.min <- 1000 * 0.0000191 * body.mass
  expect_equal(result$kcal_per_min, expected.kcal.per.min, tolerance = 0.01)
})

test_that("calculate.energy.expenditure.direct uses freedson algorithm", {
  counts.data <- data.frame(axis1 = 3000, axis2 = 0, axis3 = 0)
  body.mass <- 70
  result <- calculate.energy.expenditure.direct(counts.data, body.mass, algorithm = "freedson.combination", epoch_length = 60)

  expect_true(result$kcal_per_min > 0)
  expect_equal(length(result$kcal_per_min), 1)
})

test_that("calculate.energy.expenditure.direct freedson returns positive EE for low counts", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 0, axis3 = 0)
  body.mass <- 70
  result <- calculate.energy.expenditure.direct(counts.data, body.mass, algorithm = "freedson")

  # For CPM <= 1951, freedson algorithm returns resting EE
  expect_true(result$kcal_per_min > 0)
  expect_true(result$kcal_per_min < 3)
})

test_that("calculate.energy.expenditure.direct uses freedson.combination", {
  counts.data <- data.frame(axis1 = c(1000, 3000), axis2 = 0, axis3 = 0)
  body.mass <- 70
  result <- calculate.energy.expenditure.direct(counts.data, body.mass, algorithm = "freedson.combination")

  expect_true(result$kcal_per_min[1] > 0)
  expect_true(result$kcal_per_min[2] > result$kcal_per_min[1])
})

test_that("calculate.energy.expenditure.direct uses freedson.vm3", {
  counts.data <- data.frame(axis1 = 3000, axis2 = 2000, axis3 = 1000)
  body.mass <- 70
  result <- calculate.energy.expenditure.direct(counts.data, body.mass, algorithm = "freedson.vm3")

  vm <- sqrt(3000^2 + 2000^2 + 1000^2)
  expect_true(result$kcal_per_min > 0)
})

test_that("calculate.energy.expenditure.direct warns for missing body mass", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 0, axis3 = 0)
  expect_warning(calculate.energy.expenditure.direct(counts.data, body_mass = NULL), "70 kg")
})

test_that("summarize.energy.expenditure aggregates by intensity", {
  kcal <- c(rep(1.0, 50), rep(2.0, 30), rep(3.0, 20))
  intensity <- factor(c(rep("sedentary", 50), rep("light", 30), rep("moderate", 20)),
                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  wear.time <- rep(TRUE, 100)

  result <- summarize.energy.expenditure(kcal, intensity, wear.time)

  expect_equal(nrow(result), 3)
  expect_true("total_kcal" %in% names(result))
  expect_true("percent_kcal" %in% names(result))
})

test_that("summarize.energy.expenditure calculates percentages correctly", {
  kcal <- c(rep(1.0, 50), rep(1.0, 50))
  intensity <- factor(c(rep("sedentary", 50), rep("light", 50)),
                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  wear.time <- rep(TRUE, 100)

  result <- summarize.energy.expenditure(kcal, intensity, wear.time)

  expect_equal(result$percent_kcal[1], 50)
  expect_equal(result$percent_kcal[2], 50)
})

test_that("summarize.energy.expenditure filters by wear time", {
  kcal <- rep(1.0, 100)
  intensity <- factor(rep("light", 100),
                     levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  wear.time <- rep(TRUE, 100)
  wear.time[1:50] <- FALSE

  result <- summarize.energy.expenditure(kcal, intensity, wear.time)

  expect_equal(result$total_kcal, 50)
})

test_that("calculate.average.mets computes overall average", {
  mets <- c(rep(2.0, 50), rep(4.0, 50))
  wear.time <- rep(TRUE, 100)
  timestamp <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 100)

  result <- calculate.average.mets(mets, wear.time, timestamp)

  expect_equal(result$average_mets, 3.0)
})

test_that("calculate.average.mets computes daily averages", {
  mets <- c(rep(2.0, 1440), rep(4.0, 1440))
  wear.time <- rep(TRUE, 2880)
  timestamp <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 2880)

  result <- calculate.average.mets(mets, wear.time, timestamp)

  expect_true(nrow(result$daily_average_mets) >= 2)
  expect_true(all(result$daily_average_mets$average_mets >= 2.0))
  expect_true(all(result$daily_average_mets$average_mets <= 4.0))
})

test_that("calculate.average.mets filters by wear time", {
  mets <- c(rep(2.0, 50), rep(10.0, 50))
  wear.time <- c(rep(TRUE, 50), rep(FALSE, 50))
  timestamp <- seq(as.POSIXct("2024-01-01 00:00:00"), by = 60, length.out = 100)

  result <- calculate.average.mets(mets, wear.time, timestamp)

  expect_equal(result$average_mets, 2.0)
})

test_that("energy expenditure functions handle empty input", {
  result <- calculate.energy.expenditure(numeric(0), 70)
  expect_equal(result$total_kcal, 0)
  expect_equal(length(result$kcal_per_epoch), 0)
})

test_that("energy expenditure functions preserve vector length", {
  mets <- 1:100
  body.mass <- 70
  result <- calculate.energy.expenditure(mets, body.mass)
  expect_equal(length(result$kcal_per_epoch), length(mets))
})

test_that("energy expenditure functions return positive values", {
  mets <- rep(3.0, 100)
  body.mass <- 70
  result <- calculate.energy.expenditure(mets, body.mass)
  expect_true(all(result$kcal_per_epoch > 0))
  expect_true(result$total_kcal > 0)
})
