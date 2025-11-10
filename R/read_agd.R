#' Read ActiGraph .agd File
#'
#' @param filepath Path to .agd file
#' @return Data frame with epoch data
#' @export
read.agd <- function(filepath) {

  if (!file.exists(filepath)) {
    stop(sprintf("File not found: %s", filepath))
  }

  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Package 'RSQLite' is required. Install it with: install.packages('RSQLite')")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), filepath)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tables <- DBI::dbListTables(con)

  if ("data" %in% tables) {
    data <- DBI::dbReadTable(con, "data")
  } else if ("epochs" %in% tables) {
    data <- DBI::dbReadTable(con, "epochs")
  } else {
    stop(sprintf("Could not find 'data' or 'epochs' table in .agd file. Available tables: %s",
                 paste(tables, collapse = ", ")))
  }

  if ("settings" %in% tables) {
    settings <- DBI::dbReadTable(con, "settings")
  }

  return(list(
    data = data,
    settings = if(exists("settings")) settings else NULL
  ))
}

#' Extract Counts from .agd Data
#'
#' @param agd_data List returned from read.agd()
#' @param convert.timestamps Logical. Convert ActiGraph timestamps to POSIXct (default: TRUE)
#' @return Data frame with counts per minute and timestamps
#' @export
agd.counts <- function(agd_data, convert.timestamps = TRUE) {
  data <- agd_data$data

  if ("dataTimestamp" %in% names(data)) {
    if (convert.timestamps) {
      timestamps <- as.POSIXct((data$dataTimestamp / 10000000 - 62135596800),
                               origin = '1970-01-01', tz = 'UTC')
    } else {
      timestamps <- data$dataTimestamp
    }
  } else {
    timestamps <- seq_len(nrow(data))
  }

  data.frame(
    timestamp = timestamps,
    axis1 = if ("axis1" %in% names(data)) data$axis1 else NA,
    axis2 = if ("axis2" %in% names(data)) data$axis2 else NA,
    axis3 = if ("axis3" %in% names(data)) data$axis3 else NA,
    steps = if ("steps" %in% names(data)) data$steps else NA,
    stringsAsFactors = FALSE
  )
}

#' Extract Subject Information from AGD Settings
#'
#' @param agd_data List returned from read.agd()
#' @return List with subject information
#' @keywords internal
extract.subject.info <- function(agd_data) {
  if (is.null(agd_data$settings)) {
    return(list(
      subject_id = NA,
      sex = NA,
      age = NA,
      height = NA,
      mass = NA,
      weight_lbs = NA
    ))
  }

  settings <- agd_data$settings

  get_setting <- function(name) {
    value <- settings$settingValue[settings$settingName == name]
    if (length(value) == 0) return(NA)
    value <- as.character(value)
    if (value == "" || value == "0") return(NA)
    return(value)
  }

  subject_id <- get_setting("subjectname")
  sex <- get_setting("sex")
  age <- get_setting("age")
  height <- get_setting("height")
  mass <- get_setting("mass")

  weight_lbs <- NA
  if (!is.na(mass) && mass != "0") {
    weight_lbs <- round(as.numeric(mass) * 2.20462, 1)
  }

  if (!is.na(age) && age != "0") {
    age <- as.numeric(age)
  } else {
    age <- NA
  }

  if (!is.na(sex) && sex != "") {
    sex_lower <- tolower(as.character(sex))
    if (sex_lower %in% c("male", "m", "1", "true")) {
      sex <- "M"
    } else if (sex_lower %in% c("female", "f", "2", "false")) {
      sex <- "F"
    } else if (sex_lower %in% c("undefined", "0", "")) {
      sex <- ""
    } else {
      sex <- toupper(sex)
    }
  } else {
    sex <- ""
  }

  return(list(
    subject_id = if (!is.na(subject_id)) subject_id else NA,
    sex = sex,
    age = if (!is.na(age)) age else 0,
    height = if (!is.na(height)) as.numeric(height) else NA,
    mass = if (!is.na(mass)) as.numeric(mass) else NA,
    weight_lbs = if (!is.na(weight_lbs)) weight_lbs else 0
  ))
}
