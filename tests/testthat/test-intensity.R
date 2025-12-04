# Tests for to_cpm function
test_that("to_cpm converts 60-second epochs correctly (no change)", {
  counts <- c(100, 500, 1000)
  result <- to_cpm(counts, epoch_length = 60)
  expect_equal(result, counts)
})

test_that("to_cpm converts 30-second epochs correctly", {
  counts <- c(50, 250, 500)  # Counts per 30-sec epoch
  result <- to_cpm(counts, epoch_length = 30)
  expect_equal(result, c(100, 500, 1000))  # Should double
})

test_that("to_cpm converts 15-second epochs correctly", {
  counts <- c(25, 125, 250)  # Counts per 15-sec epoch
  result <- to_cpm(counts, epoch_length = 15)
  expect_equal(result, c(100, 500, 1000))  # Should quadruple
})

test_that("to_cpm converts 10-second epochs correctly", {
  counts <- c(100, 500, 1000)  # Counts per 10-sec epoch
  result <- to_cpm(counts, epoch_length = 10)
  expect_equal(result, c(600, 3000, 6000))  # Should multiply by 6
})

test_that("to_cpm validates input", {
  expect_error(to_cpm("not numeric", 60), "numeric")
  expect_error(to_cpm(c(100, 200), 0), "positive")
  expect_error(to_cpm(c(100, 200), -10), "positive")
})

test_that("to_cpm handles empty input", {
  result <- to_cpm(numeric(0), epoch_length = 60)
  expect_equal(length(result), 0)
})

# Tests for freedson cutpoints (using ActiLife-standard thresholds)
test_that("freedson classifies sedentary correctly", {
  counts <- c(0, 50, 100)  # 0-100 CPM is sedentary
  result <- freedson(counts)
  expect_true(all(result == "sedentary"))
})

test_that("freedson classifies light correctly", {
  counts <- c(101, 1000, 1951)  # 101-1951 CPM is light
  result <- freedson(counts)
  expect_true(all(result == "light"))
})

test_that("freedson classifies moderate correctly", {
  counts <- c(1952, 3000, 5724)
  result <- freedson(counts)
  expect_true(all(result == "moderate"))
})

test_that("freedson classifies vigorous correctly", {
  counts <- c(5725, 7000, 9498)
  result <- freedson(counts)
  expect_true(all(result == "vigorous"))
})

test_that("freedson classifies very vigorous correctly", {
  counts <- c(9499, 15000, 25000)
  result <- freedson(counts)
  expect_true(all(result == "very_vigorous"))
})

test_that("freedson handles boundary values at 100-101", {
  counts <- c(100, 101)
  result <- freedson(counts)
  expect_equal(as.character(result[1]), "sedentary")  # 100 CPM = sedentary
  expect_equal(as.character(result[2]), "light")       # 101 CPM = light
})

test_that("freedson handles boundary values at 1951-1952", {
  counts <- c(1951, 1952)
  result <- freedson(counts)
  expect_equal(as.character(result[1]), "light")
  expect_equal(as.character(result[2]), "moderate")
})

test_that("freedson returns ordered factor", {
  counts <- c(0, 100, 2000, 6000, 10000)
  result <- freedson(counts)
  expect_s3_class(result, "factor")
  expect_true(is.ordered(result))
})

test_that("CANHR.Cutpoints classifies sedentary correctly", {
  counts <- c(0, 75, 150)
  result <- CANHR.Cutpoints(counts)
  expect_true(all(result == "sedentary"))
})

test_that("CANHR.Cutpoints classifies light correctly", {
  counts <- c(151, 1000, 2200)
  result <- CANHR.Cutpoints(counts)
  expect_true(all(result == "light"))
})

test_that("CANHR.Cutpoints classifies moderate correctly", {
  counts <- c(2201, 4000, 6000)
  result <- CANHR.Cutpoints(counts)
  expect_true(all(result == "moderate"))
})

test_that("CANHR.Cutpoints classifies vigorous correctly", {
  counts <- c(6001, 8000, 10000)
  result <- CANHR.Cutpoints(counts)
  expect_true(all(result == "vigorous"))
})

test_that("CANHR.Cutpoints classifies very vigorous correctly", {
  counts <- c(10001, 15000, 25000)
  result <- CANHR.Cutpoints(counts)
  expect_true(all(result == "very_vigorous"))
})

test_that("CANHR.Cutpoints handles boundary values", {
  counts <- c(150, 151, 2200, 2201, 6000, 6001, 10000, 10001)
  result <- CANHR.Cutpoints(counts)
  expect_equal(as.character(result[1]), "sedentary")
  expect_equal(as.character(result[2]), "light")
  expect_equal(as.character(result[3]), "light")
  expect_equal(as.character(result[4]), "moderate")
})

test_that("intensity function summarizes correctly", {
  intensity.levels <- factor(c(rep("sedentary", 50), rep("light", 30), rep("moderate", 20)),
                            levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"),
                            ordered = TRUE)
  result <- intensity(intensity.levels)
  expect_true(nrow(result) >= 3)
  expect_true("minutes" %in% names(result))
  expect_true("percentage" %in% names(result))
  expect_equal(sum(result$minutes), 100)
  expect_equal(round(sum(result$percentage)), 100)
})

test_that("intensity function handles wear time filtering", {
  intensity.levels <- factor(rep("moderate", 100),
                            levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"),
                            ordered = TRUE)
  wear.time <- rep(TRUE, 100)
  wear.time[1:20] <- FALSE
  result <- intensity(intensity.levels, wear.time)
  expect_equal(sum(result$minutes), 80)
})

test_that("intensity function validates length mismatch", {
  intensity.levels <- factor(rep("light", 100),
                            levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"))
  wear.time <- rep(TRUE, 50)
  expect_error(intensity(intensity.levels, wear.time), "same length")
})

test_that("mvpa calculates moderate + vigorous minutes", {
  intensity.levels <- factor(c(rep("sedentary", 50), rep("light", 20),
                               rep("moderate", 15), rep("vigorous", 10), rep("very_vigorous", 5)),
                            levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"),
                            ordered = TRUE)
  result <- mvpa(intensity.levels)
  expect_equal(result, 30)
})

test_that("mvpa handles wear time filtering", {
  intensity.levels <- factor(c(rep("moderate", 50), rep("vigorous", 50)),
                            levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"),
                            ordered = TRUE)
  wear.time <- rep(TRUE, 100)
  wear.time[1:50] <- FALSE
  result <- mvpa(intensity.levels, wear.time)
  expect_equal(result, 50)
})

test_that("mvpa excludes vigorous when specified", {
  intensity.levels <- factor(c(rep("moderate", 50), rep("vigorous", 50)),
                            levels = c("sedentary", "light", "moderate", "vigorous", "very_vigorous"),
                            ordered = TRUE)
  result <- mvpa(intensity.levels, include_vigorous = FALSE)
  expect_equal(result, 50)
})

test_that("cut point functions handle empty input", {
  counts <- numeric(0)
  result1 <- freedson(counts)
  result2 <- CANHR.Cutpoints(counts)
  expect_equal(length(result1), 0)
  expect_equal(length(result2), 0)
})

test_that("cut point functions handle single value", {
  result1 <- freedson(500)
  result2 <- CANHR.Cutpoints(500)
  expect_equal(length(result1), 1)
  expect_equal(length(result2), 1)
})

test_that("cut point functions preserve vector length", {
  counts <- 1:1000
  result1 <- freedson(counts)
  result2 <- CANHR.Cutpoints(counts)
  expect_equal(length(result1), length(counts))
  expect_equal(length(result2), length(counts))
})
