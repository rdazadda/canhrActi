# Regression tests for the analyze_agd MVPA summary additions:
# bouted/sporadic split (epoch-aware) and the 150 min/week guideline.

# Bouted MVPA must be epoch-length-aware: detect.mvpa.bouts counts epochs as
# units, so a 30-minute MVPA block must yield 30 bouted minutes at ANY epoch
# length once the epochs<->minutes conversion is applied (the analyze_agd logic).
test_that("bouted MVPA conversion is epoch-aware (60s/30s/15s agree)", {
  bouted_min <- function(epl) {
    epm <- 60 / epl
    intensity <- c(rep("moderate", as.integer(30 * epm)),
                   rep("sedentary", as.integer(30 * epm)))   # 30 min MVPA + 30 min sed
    bs <- summarize.mvpa.bouts(detect.mvpa.bouts(
      intensity,
      min_bout_length = max(1, round(10 * epm)),
      drop_time_allowance = max(0, round(2 * epm))))
    round(bs$total_bouted_mvpa * epl / 60, 1)
  }
  expect_equal(bouted_min(60), 30)
  expect_equal(bouted_min(30), 30)
  expect_equal(bouted_min(15), 30)
})

test_that("sporadic = total - bouted, never negative", {
  # A short 6-min MVPA burst is below the 10-min bout threshold -> all sporadic.
  intensity <- c(rep("moderate", 6), rep("sedentary", 54))   # 60 x 60s epochs
  bs <- summarize.mvpa.bouts(detect.mvpa.bouts(intensity, min_bout_length = 10,
                                               drop_time_allowance = 2))
  bouted <- bs$total_bouted_mvpa
  total <- sum(intensity == "moderate")
  sporadic <- max(0, total - bouted)
  expect_equal(bouted, 0)        # no >=10-min bout
  expect_equal(sporadic, 6)      # the burst is sporadic
})
