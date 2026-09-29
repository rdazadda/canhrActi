# MVPA bout detection at several epoch lengths, and the bouted and sporadic MVPA
# minutes canhrActi() reports.

# Bouted MVPA must be epoch-length-aware: detect.mvpa.bouts counts epochs as
# units, so a 30-minute MVPA block must yield 30 bouted minutes at ANY epoch
# length once the epochs<->minutes conversion is applied (the analyze_agd logic).
test_that("detect.mvpa.bouts finds a 30-minute bout at 60, 30 and 15 s epochs", {
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

test_that("detect.mvpa.bouts finds no bout in a 6-minute burst", {
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

test_that("canhrActi reports bouted and sporadic MVPA minutes at 30 s epochs", {
  # One day of light counts (600 cpm) with moderate spells (3000 cpm): 6 + 6 min
  # split by 1.5 min of light at 08:00, 30 min at 12:00 and 6 min at 16:00
  at <- function(h, m = 0) h * 120 + m * 2 + 1
  axis1 <- rep(300, 2880)
  axis1[c(at(8) + 0:11, at(8, 7.5) + 0:11, at(12) + 0:59, at(16) + 0:11)] <- 1500
  ts <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + (0:2879) * 30
  p <- tempfile(fileext = ".agd")
  on.exit(unlink(p), add = TRUE)
  write.agd(data.frame(timestamp = ts, axis1 = axis1, axis2 = 0, axis3 = 0), p, epoch_length = 30)

  invisible(capture.output(res <- canhrActi(p, output_summary = FALSE, calculate_mets = FALSE,
                                            calculate_fragmentation = FALSE,
                                            calculate_circadian = FALSE)))

  s <- res$overall_summary
  expect_equal(res$parameters$epoch_length, 30)
  # the split spell is one bout (the 2 min drop allowance is 4 epochs) and the
  # 6 min spell is shorter than a bout (10 min is 20 epochs)
  expect_equal(c(s$mvpa_minutes, s$mvpa_bouted_minutes, s$mvpa_sporadic_minutes,
                 s$mvpa_bout_count), c(48, 42, 6, 2))
})
