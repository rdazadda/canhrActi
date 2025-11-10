#' Analyze Multiple AGD Files at Once (Batch Processing)
#'
#' Process multiple participants in one go - just provide a folder path
#' or a vector of file paths. Results are automatically exported to CSV.
#'
#' @param files Character vector of file paths OR a folder path
#' @param export Logical. Export results to CSV? (default: TRUE)
#' @param output_folder Where to save CSV files (default: current directory)
#' @return List with all results plus a combined summary table
#'
#' @examples
#' \dontrun{
#' # Option 1: Give it a folder
#' results <- canhrActi.batch("C:/My Data Folder")
#'
#' # Option 2: Give it files
#' files <- list.files("C:/My Data", pattern = ".agd", full.names = TRUE)
#' results <- canhrActi.batch(files)
#'
#' # View combined summary
#' print(results$summary)
#'
#' # Access individual participant
#' print(results$participants[[1]]$daily_summary)
#' }
#'
#' @export
canhrActi.batch <- function(files,
                         wear_time_algorithm = c("choi", "troiano", "CANHR2025"),
                         intensity_algorithm = c("freedson1998", "CANHR"),
                         min_wear_hours = 10,
                         axis_to_analyze = c("axis1", "vector_magnitude"),
                         export = TRUE,
                         output_folder = ".",
                         lfe_mode = FALSE) {

  wear_time_algorithm <- match.arg(wear_time_algorithm)
  intensity_algorithm <- match.arg(intensity_algorithm)
  axis_to_analyze <- match.arg(axis_to_analyze)

  if (length(files) == 1 && dir.exists(files)) {
    folder <- files
    files <- list.files(folder, pattern = "\\.agd$", full.names = TRUE, ignore.case = TRUE)
    if (length(files) == 0) {
      stop("No .agd files found in: ", folder)
    }
    cat("Found", length(files), "AGD files in folder\n")
  }

  if (length(files) == 0) {
    stop("No files provided")
  }

  cat("\nProcessing", length(files), "files...\n")
  cat("[", sep = "")

  all.results <- list()
  participant.ids <- character(length(files))
  failed.files <- character(0)

  for (i in seq_along(files)) {
    file <- files[i]

    subject.name <- tryCatch({
      con <- DBI::dbConnect(RSQLite::SQLite(), file)
      tables <- DBI::dbListTables(con)

      if (!"settings" %in% tables) {
        DBI::dbDisconnect(con)
        stop("No settings table found")
      }

      settings <- DBI::dbReadTable(con, "settings")
      DBI::dbDisconnect(con)

      subj <- settings$settingValue[settings$settingName == "subjectname"]
      if (length(subj) > 0 && !is.na(subj) && subj != "" && subj != "0") {
        subj  # Return subject name
      } else {
        stop("No valid subject name found")
      }
    }, error = function(e) {
      # More robust fallback ID extraction
      id.fallback <- basename(file)
      # Remove .agd extension
      id.fallback <- sub("\\.[aA][gG][dD]$", "", id.fallback)
      # Remove anything in parentheses (like dates)
      id.fallback <- sub("\\s*\\(.*\\)\\s*", "", id.fallback)
      # Trim whitespace
      id.fallback <- trimws(id.fallback)
      # If still empty, use filename
      if (id.fallback == "" || is.na(id.fallback)) {
        id.fallback <- paste0("subject_", i)
      }
      id.fallback  # Return fallback ID
    })

    result <- tryCatch({
      .canhrActi.single.internal(file, wear_time_algorithm, intensity_algorithm,
                                 min_wear_hours, axis_to_analyze,
                                 output_summary = FALSE, lfe_mode = lfe_mode)
    }, error = function(e) {
      cat("x")
      failed.files <<- c(failed.files, file)
      return(NULL)
    })

    if (!is.null(result)) {
      all.results[[subject.name]] <- result
      participant.ids[i] <- subject.name
      cat(".")
    }
  }

  cat("]\n")

  if (length(failed.files) > 0) {
    cat("\n", length(failed.files), " file(s) failed\n\n", sep = "")
  }

  summary.table <- data.frame(
    Subject = character(),
    Filename = character(),
    Epoch = numeric(),
    "Weight (lbs)" = numeric(),
    Age = numeric(),
    Gender = character(),
    Sedentary = numeric(),
    Light = numeric(),
    Moderate = numeric(),
    Vigorous = numeric(),
    "Very Vigorous" = numeric(),
    "% in Sedentary" = character(),
    "% in Light" = character(),
    "% in Moderate" = character(),
    "% in Vigorous" = character(),
    "% in Very Vigorous" = character(),
    "Total MVPA" = numeric(),
    "% in MVPA" = character(),
    "Average MVPA Per day" = numeric(),
    "Axis 1 Counts" = numeric(),
    "Axis 2 Counts" = numeric(),
    "Axis 3 Counts" = numeric(),
    "Axis 1 Average Counts" = numeric(),
    "Axis 2 Average Counts" = numeric(),
    "Axis 3 Average Counts" = numeric(),
    "Axis 1 Max Counts" = numeric(),
    "Axis 2 Max Counts" = numeric(),
    "Axis 3 Max Counts" = numeric(),
    "Axis 1 CPM" = numeric(),
    "Axis 2 CPM" = numeric(),
    "Axis 3 CPM" = numeric(),
    "Vector Magnitude Counts" = numeric(),
    "Vector Magnitude Average Counts" = numeric(),
    "Vector Magnitude Max Counts" = numeric(),
    "Vector Magnitude CPM" = numeric(),
    "Steps Counts" = numeric(),
    "Steps Average Counts" = numeric(),
    "Steps Max Counts" = numeric(),
    "Steps Per Minute" = numeric(),
    "Lux Average Counts" = numeric(),
    "Lux Max Counts" = numeric(),
    "Number of Epochs" = numeric(),
    Time = numeric(),
    "Calendar Days" = numeric(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  for (id in names(all.results)) {
    res <- all.results[[id]]
    file.path <- files[which(participant.ids == id)[1]]

    con <- DBI::dbConnect(RSQLite::SQLite(), file.path)

    tables <- DBI::dbListTables(con)
    raw.data <- DBI::dbReadTable(con, "data")
    settings <- DBI::dbReadTable(con, "settings")

    if ("filters" %in% tables) {
      filters <- DBI::dbReadTable(con, "filters")
    } else {
      filters <- data.frame()
    }

    DBI::dbDisconnect(con)

    if (nrow(filters) > 0) {
      filter.condition <- rep(FALSE, nrow(raw.data))
      for (f in 1:nrow(filters)) {
        filter.condition <- filter.condition |
          (raw.data$dataTimestamp >= filters$filterStartTimestamp[f] &
           raw.data$dataTimestamp <= filters$filterStopTimestamp[f])
      }
      raw.data <- raw.data[filter.condition, ]
    }

    age <- settings$settingValue[settings$settingName == "age"]
    sex <- settings$settingValue[settings$settingName == "sex"]
    mass.kg <- settings$settingValue[settings$settingName == "mass"]
    epoch.len <- as.numeric(settings$settingValue[settings$settingName == "epochlength"])

    age.val <- if(length(age) > 0 && !is.na(age) && age != "") as.numeric(age) else 0
    gender.val <- if(length(sex) > 0 && !is.na(sex) && sex != "") {
      ifelse(substr(sex, 1, 1) == "F", "F", ifelse(substr(sex, 1, 1) == "M", "M", ""))
    } else ""
    weight.lbs <- if(length(mass.kg) > 0 && !is.na(mass.kg) && mass.kg != "") {
      round(as.numeric(mass.kg) * 2.20462)
    } else 0

    axis1.total <- sum(raw.data$axis1, na.rm = TRUE)
    axis2.total <- sum(raw.data$axis2, na.rm = TRUE)
    axis3.total <- sum(raw.data$axis3, na.rm = TRUE)
    axis1.avg <- mean(raw.data$axis1, na.rm = TRUE)
    axis2.avg <- mean(raw.data$axis2, na.rm = TRUE)
    axis3.avg <- mean(raw.data$axis3, na.rm = TRUE)
    axis1.max <- max(raw.data$axis1, na.rm = TRUE)
    axis2.max <- max(raw.data$axis2, na.rm = TRUE)
    axis3.max <- max(raw.data$axis3, na.rm = TRUE)

    raw.data$vm <- sqrt(raw.data$axis1^2 + raw.data$axis2^2 + raw.data$axis3^2)
    vm.total <- sum(raw.data$vm, na.rm = TRUE)
    vm.avg <- mean(raw.data$vm, na.rm = TRUE)
    vm.max <- max(raw.data$vm, na.rm = TRUE)

    steps.total <- sum(raw.data$steps, na.rm = TRUE)
    steps.avg <- mean(raw.data$steps, na.rm = TRUE)
    steps.max <- max(raw.data$steps, na.rm = TRUE)

    if ("lux" %in% names(raw.data)) {
      lux.avg <- mean(raw.data$lux, na.rm = TRUE)
      lux.max <- max(raw.data$lux, na.rm = TRUE)
    } else {
      lux.avg <- NA
      lux.max <- NA
    }

    if (axis_to_analyze == "axis1") {
      counts.for.analysis <- raw.data$axis1
    } else {
      counts.for.analysis <- raw.data$vm
    }

    if (intensity_algorithm == "freedson1998") {
      raw.data$intensity <- freedson(counts.for.analysis)
    } else if (intensity_algorithm == "CANHR") {
      raw.data$intensity <- CANHR.Cutpoints(counts.for.analysis)
    } else {
      raw.data$intensity <- CANHR.Cutpoints(counts.for.analysis)
    }

    sedentary.min <- sum(raw.data$intensity == "sedentary") * (epoch.len / 60)
    light.min <- sum(raw.data$intensity == "light") * (epoch.len / 60)
    moderate.min <- sum(raw.data$intensity == "moderate") * (epoch.len / 60)
    vigorous.min <- sum(raw.data$intensity == "vigorous") * (epoch.len / 60)
    very.vigorous.min <- sum(raw.data$intensity == "very_vigorous") * (epoch.len / 60)
    mvpa.min <- moderate.min + vigorous.min + very.vigorous.min

    total.min <- nrow(raw.data) * (epoch.len / 60)
    sed.pct <- (sedentary.min / total.min) * 100
    light.pct <- (light.min / total.min) * 100
    mod.pct <- (moderate.min / total.min) * 100
    vig.pct <- (vigorous.min / total.min) * 100
    vvig.pct <- (very.vigorous.min / total.min) * 100
    mvpa.pct <- (mvpa.min / total.min) * 100

    total.hours <- total.min / 60
    axis1.cpm <- axis1.avg
    axis2.cpm <- axis2.avg
    axis3.cpm <- axis3.avg
    vm.cpm <- vm.avg
    steps.per.min <- steps.avg

    calendar.days <- res$overall_summary$valid_days
    avg.mvpa.per.day <- mvpa.min / calendar.days

    summary.table <- rbind(summary.table, data.frame(
      Subject = id,
      Filename = basename(file.path),
      Epoch = epoch.len,
      "Weight (lbs)" = weight.lbs,
      Age = age.val,
      Gender = gender.val,
      Sedentary = round(sedentary.min),
      Light = round(light.min),
      Moderate = round(moderate.min),
      Vigorous = round(vigorous.min),
      "Very Vigorous" = round(very.vigorous.min),
      "% in Sedentary" = paste0(round(sed.pct, 2), "%"),
      "% in Light" = paste0(round(light.pct, 2), "%"),
      "% in Moderate" = paste0(round(mod.pct, 2), "%"),
      "% in Vigorous" = paste0(round(vig.pct, 2), "%"),
      "% in Very Vigorous" = paste0(round(vvig.pct, 2), "%"),
      "Total MVPA" = round(mvpa.min),
      "% in MVPA" = paste0(round(mvpa.pct, 2), "%"),
      "Average MVPA Per day" = round(avg.mvpa.per.day, 1),
      "Axis 1 Counts" = axis1.total,
      "Axis 2 Counts" = axis2.total,
      "Axis 3 Counts" = axis3.total,
      "Axis 1 Average Counts" = round(axis1.avg, 1),
      "Axis 2 Average Counts" = round(axis2.avg, 1),
      "Axis 3 Average Counts" = round(axis3.avg, 1),
      "Axis 1 Max Counts" = axis1.max,
      "Axis 2 Max Counts" = axis2.max,
      "Axis 3 Max Counts" = axis3.max,
      "Axis 1 CPM" = round(axis1.cpm, 1),
      "Axis 2 CPM" = round(axis2.cpm, 1),
      "Axis 3 CPM" = round(axis3.cpm, 1),
      "Vector Magnitude Counts" = round(vm.total, 1),
      "Vector Magnitude Average Counts" = round(vm.avg, 1),
      "Vector Magnitude Max Counts" = round(vm.max, 1),
      "Vector Magnitude CPM" = round(vm.cpm, 1),
      "Steps Counts" = steps.total,
      "Steps Average Counts" = round(steps.avg, 1),
      "Steps Max Counts" = steps.max,
      "Steps Per Minute" = round(steps.per.min, 1),
      "Lux Average Counts" = round(lux.avg, 1),
      "Lux Max Counts" = round(lux.max, 1),
      "Number of Epochs" = nrow(raw.data),
      Time = round(total.hours, 1),
      "Calendar Days" = calendar.days,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  if (export) {
    if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
    summary.file <- file.path(output_folder, "batch_summary.csv")
    utils::write.csv(summary.table, summary.file, row.names = FALSE)
    cat("\nExported:", summary.file, "\n\n")
  }

  cat("\nBatch Analysis Complete\n")
  cat("Participants:", nrow(summary.table), "\n")
  cat("Mean MVPA:", round(mean(summary.table$`Total MVPA`), 1), "min\n\n")

  result <- list(
    summary = summary.table,
    participants = all.results,
    n_participants = length(all.results),
    group_stats = list(
      mean_valid_days = mean(summary.table$`Calendar Days`),
      mean_wear_hours = mean(summary.table$Time),
      mean_mvpa_minutes = mean(summary.table$`Total MVPA`)
    )
  )

  class(result) <- c("canhrActi_batch", "list")
  return(result)
}


#' Print Method for Batch Results
#' @param x An object of class canhrActi_batch
#' @param ... Additional arguments
#' @export
print.canhrActi_batch <- function(x, ...) {
  cat("\ncanhrActi Batch Analysis\n")
  cat("Participants:", x$n_participants, "\n")
  cat("Mean MVPA:", round(x$group_stats$mean_mvpa_minutes, 1), "min\n\n")
  print(x$summary)
  invisible(x)
}


