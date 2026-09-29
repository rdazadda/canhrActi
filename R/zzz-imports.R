#' @importFrom stats sd median acf aggregate lm coef pf setNames approx ave na.pass pnorm quantile time lm.fit qt IQR
#' @importFrom utils read.csv write.csv head flush.console tail
#' @importFrom graphics hist

utils::globalVariables(c(
  "hour", "mean_counts", "sd_counts", "day_num", "timestamp", "axis1",
  "date_label", "wear_hours", "sedentary", "percentage", "minutes",
  "date_factor", "start_hour", "end_hour", "bout_length",
  "activity_scaled", "sleep_numeric", "start", "end", "intensity",
  "RA", "activity", "all_of", "color", "cumulative_steps", "day_type",
  "hr_value",
  "label", "level", "lower", "lux_value", "mean_activity", "met_goal", "metric",
  "percent", "pos", "posture",
  "short_label",
  "steps_value", "survival_prob", "threshold", "time_of_day", "upper", "valid",
  "value", "wear_status", "x",
  "yintercept", "zone",
  "y",
  "acceleration", "percentile", "bin", "bin_mid",
  "density", "from", "prob", "steps", "to", "ymax", "ymin",
  "n_bouts", "mean_duration", "duration", "survival", "type", "segment_id",
  "wake_band", "sleep_band",
  "id", "label_x", "label_y",
  "group", "ci_lower", "ci_upper", "time_bin"
))

#' @importFrom ggplot2 .data
NULL
