# Tests for the ActiLife sleep export in R/export_sleep.R, which canhrActi.sleep() writes.

test_that("canhrActi.sleep writes its two ActiLife files without a word when verbose = FALSE", {
  out <- file.path(withr::local_tempdir(), "sleep")
  expect_silent(canhrActi.sleep(example_agd(1), output_dir = out, verbose = FALSE))
  expect_setequal(list.files(out), c("BatchSleepExportDetails.csv", "BatchSleepExportSummary.csv"))
})

test_that("the sleep export keeps each clock time on the night the clocks spring forward", {
  withr::local_timezone("America/Anchorage")
  # in bed 01:50 and asleep 03:10 on the recording's clock, across the hour that a
  # computer in Alaska skips at 02:00 on 9 March 2025
  periods <- data.frame(in_bed_time = "2025-03-09 01:50:00", out_bed_time = "2025-03-09 07:00:00",
                        onset = "2025-03-09 03:10:00", sleep_efficiency = 90, sleep_time = 300,
                        wake_time = 10, number_of_awakenings = 2, average_awakening = 5,
                        total_counts = 100, movement_index = 1, fragmentation_index = 2)
  path <- file.path(withr::local_tempdir(), "night.agd")
  res <- list(night.agd = list(sleep_periods = periods,
                               parameters = list(file_path = path, sleep_algorithm = "cole_kripke")))

  expect_identical(.create.actilife.batch.details(res)$Latency, 80)
  expect_identical(.create.actilife.batch.summary(res)$`Average Latency`, 80)
  expect_identical(.format.actilife.datetime("2025-03-09 02:30:00"), "3/9/2025 2:30:00 AM")
  expect_identical(.format.average.time("2025-03-09 02:30:00"), "2:30 AM")
})
