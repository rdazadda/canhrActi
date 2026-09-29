# Tests for the ActiLife-style exports in R/export_canhrActi.R: the Summary, DailyDetailed
# and HourlyDetailed files, the batch writer behind export_canhrActi(), the sedentary bout
# export and the inter-bout interval summary. A small made-up analysis gives exact
# numbers; the first sample recording checks the same rules on real data.

ID_COLS <- c("Subject", "Filename", "Epoch", "Weight (lbs)", "Age", "Gender")
CLASS_COLS <- c("Sedentary", "Light", "Moderate", "Vigorous", "Very Vigorous",
                "% in Sedentary", "% in Light", "% in Moderate", "% in Vigorous",
                "% in Very Vigorous", "Total MVPA", "% in MVPA")
COUNT_COLS <- c("Axis 1 Counts", "Axis 2 Counts", "Axis 3 Counts", "Axis 1 Average Counts",
                "Axis 2 Average Counts", "Axis 3 Average Counts", "Axis 1 Max Counts",
                "Axis 2 Max Counts", "Axis 3 Max Counts", "Axis 1 CPM", "Axis 2 CPM", "Axis 3 CPM",
                "Vector Magnitude Counts", "Vector Magnitude Average Counts",
                "Vector Magnitude Max Counts", "Vector Magnitude CPM", "Steps Counts",
                "Steps Average Counts", "Steps Max Counts", "Steps Per Minute",
                "Lux Average Counts", "Lux Max Counts", "Number of Epochs", "Time", "Calendar Days")
SUMMARY_COLS <- c(ID_COLS, CLASS_COLS, "Average MVPA Per day", COUNT_COLS)
DAILY_COLS <- c(ID_COLS, "Date", "Day of Week", "Day of Week Num", CLASS_COLS,
                "Average MVPA Per Hour", COUNT_COLS)
HOURLY_COLS <- c(ID_COLS, "Date", "Hour", "Day of Week", "Day of Week Num", CLASS_COLS, COUNT_COLS)
BOUT_COLS <- c(ID_COLS, "Sedentary Bout Start", "Sedentary Bout End", "Time in Sedentary Bout",
               "Time since last Sedentary Bout", COUNT_COLS)

# the fields of a canhrActi_analysis the exports read
as_analysis <- function(e, valid, subject = "P01") {
  e$date <- as.Date(e$timestamp)
  structure(list(epoch_data = e,
                 daily_summary = data.frame(date = names(valid), is.valid = unname(valid),
                                            stringsAsFactors = FALSE),
                 subject_info = list(subject_id = subject, sex = "F", age = 40, weight_lbs = 150),
                 parameters = list(file_path = file.path("study", paste0(subject, ".agd")))),
            class = c("canhrActi_analysis", "list"))
}

# Monday 1 January 2024: six epochs over two hours, the fifth off the body and one worn
# epoch in each class; Tuesday: three epochs on a day that failed the wear rule
fake_analysis <- function(epoch = 60, subject = "P01", all_wear = FALSE) {
  ts <- c(as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + c(0, 1, 2, 3, 60, 61) * epoch,
          as.POSIXct("2024-01-02 08:00:00", tz = "UTC") + c(0, 1, 2) * epoch)
  e <- data.frame(timestamp = ts,
                  axis1 = c(10, 500, 2500, 6000, 0, 10000, 30, 3000, 20),
                  axis2 = c(0, 100, 200, 300, 0, 400, 10, 20, 30),
                  axis3 = c(5, 50, 60, 70, 0, 80, 0, 0, 0),
                  steps = c(0, 2, 10, 20, 0, 30, 0, 5, 0),
                  wear_time = c(TRUE, TRUE, TRUE, TRUE, all_wear, TRUE, TRUE, TRUE, TRUE))
  e$intensity <- as.character(freedson(to_cpm(e$axis1, epoch)))
  as_analysis(e, c("2024-01-01" = TRUE, "2024-01-02" = FALSE), subject)
}

# Sedentary runs at 23:52 (3 min), 23:56 to midnight (5 min) and 00:04 (3 min): the first
# two split by one active minute, the last two by two active minutes and one off the body
bout_analysis <- function() {
  axis1 <- c(0, 10, 99, 500, 0, 0, 0, 0, 0, 100, 800, 0, 0, 0, 20)
  e <- data.frame(timestamp = as.POSIXct("2024-01-01 23:52:00", tz = "UTC") + (0:14) * 60,
                  axis1 = axis1, axis2 = rep(c(0, 5, 20), 5), axis3 = 0,
                  steps = c(0, 0, 1, 12, rep(0, 11)), lux = c(rep(0, 12), 5, 9, 0),
                  wear_time = c(rep(TRUE, 11), FALSE, rep(TRUE, 3)))
  e$intensity <- as.character(freedson(e$axis1))
  as_analysis(e, c("2024-01-01" = TRUE, "2024-01-02" = TRUE))
}

batch_of <- function(...) {
  structure(list(participants = list(...)), class = c("canhrActi_batch", "list"))
}

read_back <- function(path) {
  utils::read.csv(path, check.names = FALSE, colClasses = "character", stringsAsFactors = FALSE)
}

# a column by its ActiLife name, or by the name data.frame() makes of it
column <- function(d, nm) d[[if (nm %in% names(d)) nm else make.names(nm)]]

.memo <- new.env()
sample_analysis <- function() {
  if (is.null(.memo$a)) {
    utils::capture.output(.memo$a <- canhrActi(example_agd(1), output_summary = FALSE,
                                               calculate_mets = FALSE,
                                               calculate_fragmentation = FALSE,
                                               calculate_circadian = FALSE))
  }
  .memo$a
}

test_that("export_summary() writes one Summary row from the valid-day wear epochs", {
  path <- withr::local_tempfile(fileext = ".csv")
  s <- export_summary(fake_analysis(), path)
  back <- read_back(path)
  expect_identical(names(s), SUMMARY_COLS)
  expect_identical(names(back), SUMMARY_COLS)
  expect_identical(nrow(back), 1L)
  expect_identical(unlist(back[1, c(ID_COLS, CLASS_COLS)], use.names = FALSE),
                   c("P01", "P01.agd", "60", "150", "40", "F", "1", "1", "1", "1", "1",
                     rep("20.00%", 5), "3", "60.00%"))
  expect_equal(s$`Average MVPA Per day`, 3)
  expect_equal(c(s$`Axis 1 Counts`, s$`Axis 1 Average Counts`, s$`Axis 1 Max Counts`, s$`Axis 1 CPM`),
               c(19010, 3802, 10000, 3802))
  expect_equal(s$`Vector Magnitude Max Counts`, round(sqrt(10000^2 + 400^2 + 80^2), 1))
  expect_equal(c(s$`Steps Counts`, s$`Steps Per Minute`), c(62, 12.4))
  expect_equal(c(s$`Number of Epochs`, s$Time, s$`Calendar Days`), c(5, 5, 1))
})

test_that("export_summary() scales the per-minute columns by the epoch length", {
  s <- export_summary(fake_analysis(epoch = 30), withr::local_tempfile(fileext = ".csv"))
  expect_equal(c(s$Epoch, s$`Axis 1 CPM`, s$`Steps Per Minute`, s$Time), c(30, 7604, 24.8, 2.5))
})

test_that("export_summary() takes the person from the analysis unless told otherwise", {
  path <- withr::local_tempfile(fileext = ".csv")
  a <- fake_analysis()
  s <- export_summary(a, path, subject_id = "X9", weight_lbs = 200, age = 7, gender = "M")
  expect_identical(list(s$Subject, s$`Weight (lbs)`, s$Age, s$Gender), list("X9", 200, 7, "M"))
  a$subject_info$subject_id <- NA
  expect_identical(export_summary(a, path)$Subject, "P01")   # the file name
  a$subject_info <- NULL
  s <- export_summary(a, path)
  expect_identical(list(s$Subject, s$`Weight (lbs)`, s$Age, s$Gender), list("P01", 0, 0, ""))
})

test_that("export_summary() refuses other objects and writes nothing without a valid day", {
  path <- withr::local_tempfile(fileext = ".csv")
  expect_error(export_summary(list(), path), "canhrActi_analysis")
  a <- fake_analysis()
  a$daily_summary$is.valid <- FALSE
  expect_warning(res <- export_summary(a, path), "No valid days/epochs to export")
  expect_identical(res, NULL)
  expect_identical(file.exists(path), FALSE)
  names(a$daily_summary)[2] <- "valid"
  expect_error(export_summary(a, path), "Could not find validity column")
})

test_that("export_summary() counts a sample recording's valid-day wear epochs", {
  r <- sample_analysis()
  s <- export_summary(r, withr::local_tempfile(fileext = ".csv"))
  e <- r$epoch_data
  keep <- e$wear_time & as.character(e$date) %in% r$valid_days
  expect_identical(c(s$Subject, s$Gender), c("A002", "F"))
  expect_equal(s$Sedentary, sum(e$intensity[keep] == "sedentary"))
  expect_equal(s$`Total MVPA`, sum(e$intensity[keep] %in% c("moderate", "vigorous", "very_vigorous")))
  expect_equal(c(s$`Number of Epochs`, s$`Calendar Days`), c(sum(keep), length(r$valid_days)))
  expect_equal(s$`Axis 1 Counts`, sum(e$axis1[keep]))
  pct <- as.numeric(sub("%", "", unlist(s[CLASS_COLS[6:10]]), fixed = TRUE))
  expect_lt(abs(sum(pct) - 100), 0.03)
})

test_that("export_daily_detailed() writes a row per day from that day's wear epochs", {
  path <- withr::local_tempfile(fileext = ".csv")
  d <- export_daily_detailed(fake_analysis(), path)
  back <- read_back(path)
  expect_identical(nrow(back), 2L)
  expect_identical(column(back, "Date"), c("01/01/2024", "01/02/2024"))
  expect_identical(column(back, "Day of Week"), c("Monday", "Tuesday"))
  expect_identical(column(back, "Day of Week Num"), c("1", "2"))
  monday <- unlist(lapply(CLASS_COLS[c(1:5, 11)], function(nm) column(d, nm)[1]))
  expect_equal(monday, c(1, 1, 1, 1, 1, 3))
  expect_identical(column(d, "% in Sedentary")[1], "20.00%")
  expect_equal(column(d, "Average MVPA Per Hour")[1], 36)   # 3 MVPA minutes in 5 worn
  expect_equal(column(d, "Steps Per Minute")[1], 12.4)
  expect_equal(column(d, "Number of Epochs")[1], 5)
})

test_that("export_daily_detailed() writes zeros for a day that failed the wear rule", {
  d <- export_daily_detailed(fake_analysis(), withr::local_tempfile(fileext = ".csv"))
  tuesday <- unlist(lapply(c(CLASS_COLS[c(1:5, 11)], "Number of Epochs"), function(nm) column(d, nm)[2]))
  expect_equal(tuesday, rep(0, 7))
  expect_identical(column(d, "% in Sedentary")[2], "0.00%")
})

test_that("export_daily_detailed() on a sample recording counts each valid day's wear epochs", {
  r <- sample_analysis()
  d <- export_daily_detailed(r, withr::local_tempfile(fileext = ".csv"))
  e <- r$epoch_data
  expect_identical(nrow(d), nrow(r$daily_summary))
  for (day in r$valid_days) {
    row <- column(d, "Date") == format(as.Date(day), "%m/%d/%Y")
    worn <- e$wear_time & as.character(e$date) == day
    expect_equal(column(d, "Number of Epochs")[row], sum(worn), info = day)
    expect_equal(column(d, "Sedentary")[row], sum(worn & e$intensity == "sedentary"), info = day)
  }
})

test_that("the days of a sample recording add up to its Summary row", {
  r <- sample_analysis()
  d <- export_daily_detailed(r, withr::local_tempfile(fileext = ".csv"))
  s <- export_summary(r, withr::local_tempfile(fileext = ".csv"))
  expect_gt(nrow(d), length(r$valid_days))   # some days failed the wear rule
  for (nm in c("Sedentary", "Light", "Total MVPA", "Axis 1 Counts", "Steps Counts",
               "Number of Epochs", "Time", "Calendar Days")) {
    expect_equal(sum(column(d, nm)), s[[nm]], info = nm)
  }
})

test_that("export_hourly_detailed() writes a row per hour and zeros for a failed day", {
  path <- withr::local_tempfile(fileext = ".csv")
  h <- export_hourly_detailed(fake_analysis(), path)
  back <- read_back(path)
  expect_identical(nrow(back), 3L)
  expect_identical(column(back, "Date"), c("01/01/2024", "01/01/2024", "01/02/2024"))
  expect_identical(column(back, "Hour"), c("10:00 AM", "11:00 AM", "08:00 AM"))
  # 10:00 holds four worn epochs; 11:00 one off the body and one very vigorous
  expect_equal(column(h, "Sedentary"), c(1, 0, 0))
  expect_equal(column(h, "Very Vigorous"), c(0, 1, 0))
  expect_identical(column(h, "% in Sedentary"), c("25.00%", "0.00%", "0.00%"))
  expect_identical(column(h, "% in Very Vigorous"), c("0.00%", "100.00%", "0.00%"))
  expect_equal(column(h, "Number of Epochs"), c(4, 1, 0))
  expect_equal(column(h, "Axis 1 Counts"), c(9010, 10000, 0))
})

test_that("export_hourly_detailed() on a sample recording covers every hour it holds", {
  r <- sample_analysis()
  h <- export_hourly_detailed(r, withr::local_tempfile(fileext = ".csv"))
  e <- r$epoch_data
  expect_identical(nrow(h), nrow(unique(data.frame(e$date, format(e$timestamp, "%H")))))
  keep <- e$wear_time & as.character(e$date) %in% r$valid_days
  expect_equal(sum(column(h, "Number of Epochs")), sum(keep))
  expect_equal(sum(column(h, "Total MVPA")),
               sum(keep & e$intensity %in% c("moderate", "vigorous", "very_vigorous")))
})

test_that("the DailyDetailed, HourlyDetailed and bout files carry ActiLife's column names", {
  dir <- withr::local_tempdir()
  export_daily_detailed(fake_analysis(), file.path(dir, "d.csv"))
  export_hourly_detailed(fake_analysis(), file.path(dir, "h.csv"))
  suppressMessages(export_sedentary_bouts(bout_analysis(), file.path(dir, "b.csv")))
  expect_identical(names(read_back(file.path(dir, "d.csv"))), DAILY_COLS)
  expect_identical(names(read_back(file.path(dir, "h.csv"))), HOURLY_COLS)
  expect_identical(names(read_back(file.path(dir, "b.csv"))), BOUT_COLS)
  # a first day that failed the wear rule puts its zero rows first
  a <- fake_analysis()
  a$daily_summary$is.valid <- c(FALSE, TRUE)
  export_daily_detailed(a, file.path(dir, "d0.csv"))
  export_hourly_detailed(a, file.path(dir, "h0.csv"))
  expect_identical(names(read_back(file.path(dir, "d0.csv"))), DAILY_COLS)
  expect_identical(names(read_back(file.path(dir, "h0.csv"))), HOURLY_COLS)
})

test_that("export_canhrActi() writes the three reports into a folder it makes", {
  a <- fake_analysis()
  dir <- file.path(withr::local_tempdir(), "reports", "wave1")
  expect_message(paths <- export_canhrActi(a, dir, prefix = "w1"), "Exported ActiLife reports to")
  expect_identical(unlist(paths, use.names = FALSE),
                   file.path(dir, c("w1_Summary.csv", "w1_DailyDetailed.csv", "w1_HourlyDetailed.csv")))
  alone <- withr::local_tempdir()
  export_summary(a, file.path(alone, "s.csv"))
  export_daily_detailed(a, file.path(alone, "d.csv"))
  export_hourly_detailed(a, file.path(alone, "h.csv"))
  expect_identical(read_back(paths$summary), read_back(file.path(alone, "s.csv")))
  expect_identical(read_back(paths$daily_detailed), read_back(file.path(alone, "d.csv")))
  expect_identical(read_back(paths$hourly_detailed), read_back(file.path(alone, "h.csv")))
})

test_that("export_canhrActi() writes one file per report for a batch", {
  people <- list(P01 = fake_analysis(all_wear = TRUE),
                 P02 = fake_analysis(subject = "P02", all_wear = TRUE))
  dir <- withr::local_tempdir()
  expect_message(paths <- export_canhrActi(do.call(batch_of, people), file.path(dir, "batch")),
                 "2 participants")
  # all worn at 60 s, so each person's rows are the ones their own export writes
  alone <- suppressMessages(lapply(names(people), function(id) {
    export_canhrActi(people[[id]], file.path(dir, id))
  }))
  for (f in c("summary", "daily_detailed", "hourly_detailed")) {
    expect_identical(read_back(paths[[f]]),
                     rbind(read_back(alone[[1]][[f]]), read_back(alone[[2]][[f]])), info = f)
  }
  expect_identical(read_back(paths$summary)$Subject, c("P01", "P02"))
})

test_that("export_canhrActi() writes the same reports for an analysis alone or in a batch", {
  for (epoch in c(60, 30)) {
    a <- fake_analysis(epoch = epoch)
    dir <- withr::local_tempdir()
    alone <- suppressMessages(export_canhrActi(a, file.path(dir, "alone")))
    batch <- suppressMessages(export_canhrActi(batch_of(P01 = a), file.path(dir, "batch")))
    for (f in c("summary", "daily_detailed", "hourly_detailed")) {
      expect_identical(read_back(batch[[f]]), read_back(alone[[f]]), info = paste(f, epoch))
    }
  }
})

test_that("export_sedentary_bouts() finds each sedentary run of worn epochs under 100 CPM", {
  path <- withr::local_tempfile(fileext = ".csv")
  expect_message(b <- export_sedentary_bouts(bout_analysis(), path), "Exported 3 sedentary bouts")
  expect_identical(nrow(read_back(path)), 3L)
  expect_identical(column(b, "Sedentary Bout Start"),
                   c("01/01/2024 11:52:00 PM", "01/01/2024 11:56:00 PM", "01/02/2024 12:04:00 AM"))
  expect_identical(column(b, "Sedentary Bout End"),
                   c("01/01/2024 11:54:00 PM", "01/02/2024 12:00:00 AM", "01/02/2024 12:06:00 AM"))
  expect_equal(column(b, "Time in Sedentary Bout"), c(3, 5, 3))
  expect_equal(column(b, "Number of Epochs"), c(3, 5, 3))
  expect_equal(column(b, "Calendar Days"), c(1, 2, 1))
  expect_equal(column(b, "Axis 1 Counts"), c(109, 0, 20))
  expect_equal(column(b, "Axis 1 Max Counts"), c(99, 0, 20))
  expect_equal(column(b, "Axis 2 Counts"), c(25, 50, 25))
  expect_equal(column(b, "Steps Counts"), c(1, 0, 0))
  expect_equal(column(b, "Lux Max Counts"), c(0, 0, 9))
  expect_identical(column(b, "Subject"), rep("P01", 3))
})

test_that("export_sedentary_bouts() applies the threshold, the minimum length and the epoch", {
  expect_equal(column(export_sedentary_bouts(bout_analysis(), sedentary_threshold = 10),
                      "Time in Sedentary Bout"), c(1, 5, 2))
  expect_equal(column(export_sedentary_bouts(bout_analysis(), min_bout_length = 4),
                      "Time in Sedentary Bout"), 5)
  expect_message(none <- export_sedentary_bouts(bout_analysis(), min_bout_length = 6),
                 "No sedentary bouts >= 6 minutes detected")
  expect_identical(dim(none), c(0L, 0L))
  busy <- bout_analysis()
  busy$epoch_data$axis1 <- 5000
  expect_message(export_sedentary_bouts(busy), "No sedentary bouts detected")
  # at 30 s, 49 counts is 98 CPM and 50 is 100 CPM
  e <- data.frame(timestamp = as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + (0:3) * 30,
                  axis1 = c(0, 49, 50, 0), axis2 = 0, axis3 = 0, steps = 0, wear_time = TRUE,
                  intensity = "sedentary")
  b <- export_sedentary_bouts(as_analysis(e, c("2024-01-01" = TRUE)))
  expect_equal(c(column(b, "Epoch"), column(b, "Time in Sedentary Bout")), c(30, 1))
  expect_error(export_sedentary_bouts(list()), "canhrActi_analysis")
})

test_that("export_sedentary_bouts() times a break from the end of the last bout", {
  b <- export_sedentary_bouts(bout_analysis())
  expect_equal(column(b, "Time since last Sedentary Bout")[-1], c(1, 3))
  # a one-minute break is a micro-break
  expect_identical(analyze_inter_bout_intervals(b)$break_classifications$count[1], 1L)
  # at 30 s a break of one epoch is half a minute
  e <- data.frame(timestamp = as.POSIXct("2024-01-01 10:00:00", tz = "UTC") + (0:3) * 30,
                  axis1 = c(0, 49, 50, 0), axis2 = 0, axis3 = 0, steps = 0, wear_time = TRUE,
                  intensity = "sedentary")
  b30 <- export_sedentary_bouts(as_analysis(e, c("2024-01-01" = TRUE)), min_bout_length = 0.5)
  expect_equal(column(b30, "Time since last Sedentary Bout"), c(0, 0.5))
})

test_that("export_sedentary_bouts() counts calendar days in the zone its times are written in", {
  # 14:50 to 15:19 in Anchorage is 23:50 to 00:19 in UTC
  e <- data.frame(timestamp = as.POSIXct("2024-03-04 14:50:00", tz = "America/Anchorage") + (0:29) * 60,
                  axis1 = 0, axis2 = 0, axis3 = 0, steps = 0, wear_time = TRUE, intensity = "sedentary")
  b <- export_sedentary_bouts(as_analysis(e, c("2024-03-04" = TRUE)))
  expect_identical(c(column(b, "Sedentary Bout Start"), column(b, "Sedentary Bout End")),
                   c("03/04/2024 02:50:00 PM", "03/04/2024 03:19:00 PM"))
  expect_equal(column(b, "Calendar Days"), 1)
})

test_that("the bouts of a sample recording come back from their .csv unchanged", {
  r <- sample_analysis()
  path <- withr::local_tempfile(fileext = ".csv")
  expect_message(b <- export_sedentary_bouts(r, path), "sedentary bouts to")
  e <- r$epoch_data
  sed <- e$axis1 < 100 & e$wear_time
  # at 60 s every sedentary run is a bout of at least a minute
  expect_equal(sum(column(b, "Number of Epochs")), sum(sed))
  back <- utils::read.csv(path)
  expect_identical(nrow(back), nrow(b))
  keep <- c("n_breaks", "mean_ibi", "median_ibi", "break_classifications")
  expect_identical(unclass(analyze_inter_bout_intervals(back))[keep],
                   unclass(analyze_inter_bout_intervals(b))[keep])
})

test_that("analyze_inter_bout_intervals() summarises the breaks and sorts them into five kinds", {
  got <- analyze_inter_bout_intervals(
    data.frame(`Time since last Sedentary Bout` = c(0, 1, 3, 10, 20, 45), check.names = FALSE))
  ibi <- c(1, 3, 10, 20, 45)
  expect_identical(class(got), c("canhrActi_ibi_analysis", "list"))
  expect_identical(got$n_breaks, 5L)
  expect_equal(c(got$mean_ibi, got$median_ibi, got$sd_ibi, got$min_ibi, got$max_ibi, got$iqr_ibi),
               round(c(mean(ibi), median(ibi), sd(ibi), min(ibi), max(ibi), IQR(ibi)), 2))
  expect_equal(got$cv_ibi, round(sd(ibi) / mean(ibi), 3))
  expect_equal(unname(got$percentiles),
               unname(round(quantile(ibi, c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95)), 2)))
  expect_identical(got$break_classifications$category,
                   c("Micro-break (<2 min)", "Short break (2-5 min)", "Medium break (5-15 min)",
                     "Long break (15-30 min)", "Extended break (>30 min)"))
  expect_identical(got$break_classifications$count, rep(1L, 5))
  expect_equal(got$break_classifications$percent, rep(20, 5))
  expect_equal(c(got$pct_breaks_under_5min, got$pct_breaks_over_15min), c(40, 40))
})

test_that("analyze_inter_bout_intervals() puts a break on a class boundary in the longer class", {
  got <- analyze_inter_bout_intervals(
    data.frame(`Time since last Sedentary Bout` = c(0, 1.9, 2, 5, 15, 30), check.names = FALSE))
  expect_identical(got$break_classifications$count, rep(1L, 5))
})

test_that("analyze_inter_bout_intervals() reads the column as read.csv() names it, and needs it", {
  got <- analyze_inter_bout_intervals(data.frame(Time.since.last.Sedentary.Bout = c(0, 4)))
  expect_identical(got$n_breaks, 1L)
  expect_error(analyze_inter_bout_intervals(data.frame(x = 1)), "Time since last Sedentary Bout")
  none <- analyze_inter_bout_intervals(
    data.frame(`Time since last Sedentary Bout` = 0, check.names = FALSE))
  expect_identical(list(none$n_breaks, none$mean_ibi, none$message),
                   list(0, NA, "No inter-bout intervals found"))
})

test_that("analyze_inter_bout_intervals() leaves out a missing interval", {
  got <- analyze_inter_bout_intervals(
    data.frame(`Time since last Sedentary Bout` = c(NA, 1, 3), check.names = FALSE))
  expect_identical(got$n_breaks, 2L)
  expect_equal(got$mean_ibi, 2)
})
