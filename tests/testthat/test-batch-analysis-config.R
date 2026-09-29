# Tests for batch.config() in R/batch_analysis.R: it names the fields canhrActi.batch()
# reads, and a config changes the run it is given to.

test_that("batch.config() keeps only the settings it is given", {
  cfg <- batch.config()
  expect_identical(class(cfg), c("canhrActi_batch_config", "list"))
  expect_identical(length(cfg), 0L)
  cfg <- batch.config(wear = "troiano", min_wear = 8, fragmentation = FALSE, cores = 2)
  expect_identical(unclass(cfg), list(wear = "troiano", min_wear = 8, fragmentation = FALSE, cores = 2))
})

test_that("every batch.config() field is one canhrActi.batch() reads", {
  src <- paste(deparse(body(canhrActi.batch)), collapse = "\n")
  read <- regmatches(src, gregexpr("config\\$[A-Za-z_]+", src))[[1]]
  read <- sort(unique(sub("config$", "", read, fixed = TRUE)), method = "radix")
  expect_identical(read, sort(names(formals(batch.config)), method = "radix"))
})

test_that("a batch.config() changes the run canhrActi.batch() makes", {
  cfg <- batch.config(wear = "troiano", intensity = "CANHR", min_wear = 20, mets = FALSE,
                      fragmentation = FALSE, circadian = FALSE, parallel = FALSE)
  utils::capture.output(res <- canhrActi.batch(example_agd(2), config = cfg, export = FALSE,
                                               verbose = FALSE))
  expect_identical(res$settings[c("wear_time_algorithm", "intensity_algorithm", "parallel")],
                   list(wear_time_algorithm = "troiano", intensity_algorithm = "CANHR",
                        parallel = FALSE))
  p <- res$participants[[1]]
  expect_identical(p$parameters[c("min_wear_hours", "calculate_mets", "calculate_fragmentation",
                                  "calculate_circadian")],
                   list(min_wear_hours = 20, calculate_mets = FALSE,
                        calculate_fragmentation = FALSE, calculate_circadian = FALSE))
  cnt <- agd.counts(read.agd(example_agd(2), verbose = FALSE))
  wear <- wear.troiano(cnt$axis1)
  expect_identical(p$epoch_data$wear_time, wear)
  expect_identical(p$epoch_data$intensity, as.character(CANHR.Cutpoints(cnt$axis1)))
  # the two part days of about 15 h fail a 20 h rule
  expect_identical(p$valid_days, valid.days(cnt$timestamp, wear, min.wear.hours = 20)$valid_days)
  expect_identical(length(p$valid_days), 6L)
})
