#' @importFrom stats var sd cor median rnorm acf aggregate lm coef pf setNames start end approx ave fitted na.pass optim pnorm quantile residuals time as.formula predict lm.fit qt IQR
#' @importFrom utils read.csv write.csv head flush.console packageVersion tail
#' @importFrom graphics hist

utils::globalVariables(c(
  "hour", "mean_counts", "sd_counts", "fit", "day_num", "timestamp", "axis1",
  "date_label", "wear_hours", "sedentary", "percentage", "minutes",
  "date_factor", "start_hour", "end_hour", "bout_length",
  "activity_scaled", "sleep_numeric", "start", "end", "intensity",
  "RA", "activity", "all_of", "color", "cumulative_steps", "day_type",
  "efficiency", "end_time", "event", "hr_value",
  "label", "level", "lower", "lux_value", "mean_activity", "met_goal", "metric",
  "number_of_awakenings", "percent", "pos", "posture", "req",
  "scaled_activity", "short_label", "sleep_time", "start_time",
  "steps_value", "survival_prob", "threshold", "time_of_day", "upper", "valid",
  "validity", "value", "waso", "wear_status", "week", "weekday_num", "x", "x_pos",
  "yintercept", "zone",
  "X", "component", "proportion", "midpoint", "y", "score", "Metric", "Value",
  "duration_minutes", "is_brief", "acceleration", "percentile", "bin", "bin_mid",
  "density", "from", "intensity_cat", "prob", "steps", "to", "ymax", "ymin",
  "n_bouts", "mean_duration", "duration", "survival", "type", "segment_id",
  "wake_band", "sleep_band", "subject",
  "id", "label_x", "label_y", "outside_x", "outside_y", "hjust",
  "group", "ci_lower", "ci_upper", "time_bin",
  "anchor_x", "anchor_y", "label_pos_x", "label_pos_y"
))

#' @importFrom ggplot2 ggplot aes geom_tile geom_line geom_point geom_col geom_area geom_text geom_rect geom_ribbon geom_hline annotate scale_fill_manual scale_color_manual scale_x_continuous scale_x_datetime scale_y_discrete scale_fill_gradient scale_alpha_manual labs theme element_text element_rect element_blank element_line margin coord_cartesian coord_polar
#' @import ggplot2
NULL
