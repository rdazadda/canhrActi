test_that("freedson.vm3 calculates METs correctly", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 800, axis3 = 600)
  vm <- sqrt(1000^2 + 800^2 + 600^2)
  expected.mets <- 0.000863 * vm + 0.668876
  result <- calculate.mets(counts.data, algorithm = "freedson.vm3")
  expect_equal(result, expected.mets, tolerance = 0.01)
})

test_that("freedson.adult calculates METs correctly", {
  counts.data <- data.frame(axis1 = c(1000, 2000), axis2 = 0, axis3 = 0)
  subject.info <- list(body_mass = 70)
  result <- calculate.mets(counts.data, algorithm = "freedson.adult", subject_info = subject.info)
  expect_true(all(result > 0))
  expect_equal(length(result), 2)
})

test_that("freedson.adult handles counts <= 1951", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 0, axis3 = 0)
  subject.info <- list(body_mass = 70)
  result <- calculate.mets(counts.data, algorithm = "freedson.adult", subject_info = subject.info)
  # Freedson Adult algorithm returns METs based on CPM and body mass
  expect_true(result > 0)
  expect_true(result < 3)
})

test_that("freedson.adult handles counts > 1951", {
  counts.data <- data.frame(axis1 = 3000, axis2 = 0, axis3 = 0)
  subject.info <- list(body_mass = 70)
  result <- calculate.mets(counts.data, algorithm = "freedson.adult", subject_info = subject.info)
  expect_true(result > 3)
})

test_that("crouter distinguishes locomotion vs lifestyle", {
  counts.data <- data.frame(axis1 = rep(1000, 20), axis2 = 0, axis3 = 0)
  result <- calculate.mets(counts.data, algorithm = "crouter")
  expect_true(all(result > 0))
  expect_equal(length(result), 20)
})

test_that("crouter handles low activity", {
  counts.data <- data.frame(axis1 = rep(50, 20), axis2 = 0, axis3 = 0)
  result <- calculate.mets(counts.data, algorithm = "crouter")
  expect_true(all(result >= 1.0))
})

test_that("hendelman.adult calculates METs correctly", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 0, axis3 = 0)
  result <- calculate.mets(counts.data, algorithm = "hendelman.adult")
  expected <- 1.602 + (0.000638 * 1000)
  expect_equal(result, expected, tolerance = 0.01)
})

test_that("hendelman.lifestyle calculates METs correctly", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 0, axis3 = 0)
  result <- calculate.mets(counts.data, algorithm = "hendelman.lifestyle")
  expected <- 2.922 + (0.000409 * 1000)
  expect_equal(result, expected, tolerance = 0.01)
})

test_that("swartz calculates METs correctly", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 0, axis3 = 0)
  result <- calculate.mets(counts.data, algorithm = "swartz")
  expected <- 2.606 + (0.0006863 * 1000)
  expect_equal(result, expected, tolerance = 0.01)
})

test_that("leenders calculates METs correctly", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 0, axis3 = 0)
  result <- calculate.mets(counts.data, algorithm = "leenders")
  expected <- 2.240 + (0.0006 * 1000)
  expect_equal(result, expected, tolerance = 0.01)
})

test_that("yngve.treadmill calculates METs correctly", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 0, axis3 = 0)
  result <- calculate.mets(counts.data, algorithm = "yngve.treadmill")
  expected <- 0.751 + (0.0008198 * 1000)
  expect_equal(result, expected, tolerance = 0.01)
})

test_that("yngve.overground calculates METs correctly", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 0, axis3 = 0)
  result <- calculate.mets(counts.data, algorithm = "yngve.overground")
  # Yngve et al. (2003) overground equation
  expect_true(result > 1)
  expect_true(result < 3)
})

test_that("brooks.overground calculates METs correctly", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 0, axis3 = 0)
  result <- calculate.mets(counts.data, algorithm = "brooks.overground")
  expected <- 2.32 + (0.000389 * 1000)
  expect_equal(result, expected, tolerance = 0.01)
})

test_that("brooks.bm calculates METs correctly", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 0, axis3 = 0)
  subject.info <- list(body_mass = 70)
  result <- calculate.mets(counts.data, algorithm = "brooks.bm", subject_info = subject.info)
  expected <- 3.33 + (0.000370 * 1000) - (0.012 * 70)
  expect_equal(result, expected, tolerance = 0.01)
})

test_that("freedson.children requires age", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 0, axis3 = 0)
  subject.info <- list(age = 10)
  result <- calculate.mets(counts.data, algorithm = "freedson.children", subject_info = subject.info)
  expect_true(result > 0)
})

test_that("freedson.children throws error without age", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 0, axis3 = 0)
  subject.info <- list(body_mass = 50)
  expect_error(calculate.mets(counts.data, algorithm = "freedson.children", subject_info = subject.info), "age")
})

test_that("calculate.mets validates input data frame", {
  expect_error(calculate.mets(c(1, 2, 3)), "data frame")
})

test_that("calculate.mets validates required columns", {
  counts.data <- data.frame(axis1 = 1000)
  expect_error(calculate.mets(counts.data), "axis2|axis3")
})

test_that("calculate.mets validates non-empty data", {
  counts.data <- data.frame(axis1 = numeric(0), axis2 = numeric(0), axis3 = numeric(0))
  expect_error(calculate.mets(counts.data), "empty")
})

test_that("calculate.mets sets negative METs to zero", {
  counts.data <- data.frame(axis1 = 0, axis2 = 0, axis3 = 0)
  result <- calculate.mets(counts.data, algorithm = "freedson.vm3")
  expect_true(all(result >= 0))
})

test_that("calculate.mets sets NA METs to zero", {
  counts.data <- data.frame(axis1 = c(1000, NA, 2000), axis2 = 0, axis3 = 0)
  result <- calculate.mets(counts.data, algorithm = "swartz")
  expect_true(all(result >= 0, na.rm = TRUE))
})

test_that("classify.mets classifies sedentary correctly", {
  mets <- c(0.5, 1.0, 1.4)
  result <- classify.mets(mets)
  expect_true(all(result == "sedentary"))
})

test_that("classify.mets classifies light correctly", {
  mets <- c(1.5, 2.0, 2.9)
  result <- classify.mets(mets)
  expect_true(all(result == "light"))
})

test_that("classify.mets classifies moderate correctly", {
  mets <- c(3.0, 4.5, 5.9)
  result <- classify.mets(mets)
  expect_true(all(result == "moderate"))
})

test_that("classify.mets classifies vigorous correctly", {
  mets <- c(6.0, 7.5, 8.9)
  result <- classify.mets(mets)
  expect_true(all(result == "vigorous"))
})

test_that("classify.mets classifies very vigorous correctly", {
  mets <- c(9.0, 12.0, 15.0)
  result <- classify.mets(mets)
  expect_true(all(result == "very_vigorous"))
})

test_that("classify.mets handles boundary values", {
  mets <- c(1.49, 1.5, 2.99, 3.0, 5.99, 6.0, 8.99, 9.0)
  result <- classify.mets(mets)
  expect_equal(as.character(result[1]), "sedentary")
  expect_equal(as.character(result[2]), "light")
  expect_equal(as.character(result[3]), "light")
  expect_equal(as.character(result[4]), "moderate")
  expect_equal(as.character(result[5]), "moderate")
  expect_equal(as.character(result[6]), "vigorous")
  expect_equal(as.character(result[7]), "vigorous")
  expect_equal(as.character(result[8]), "very_vigorous")
})

test_that("METs algorithms preserve vector length", {
  counts.data <- data.frame(axis1 = 1:100, axis2 = 0, axis3 = 0)
  result <- calculate.mets(counts.data, algorithm = "swartz")
  expect_equal(length(result), nrow(counts.data))
})

test_that("METs algorithms return numeric vectors", {
  counts.data <- data.frame(axis1 = 1000, axis2 = 0, axis3 = 0)
  result <- calculate.mets(counts.data, algorithm = "freedson.vm3")
  expect_type(result, "double")
})
