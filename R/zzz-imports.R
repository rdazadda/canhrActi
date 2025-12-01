#' @importFrom stats var sd cor median rnorm acf aggregate lm coef pf setNames start end
#' @importFrom utils read.csv write.csv

utils::globalVariables(c(
  "hour", "mean_counts", "sd_counts", "fit", "day_num", "timestamp", "axis1",
  "date_label", "wear_hours", "sedentary", "percentage", "minutes",
  "date_factor", "start_hour", "end_hour", "bout_length",
  "activity_scaled", "sleep_numeric", "start", "end", "intensity"
))

#' @importFrom ggplot2 ggplot aes geom_tile geom_line geom_point geom_col geom_area geom_text geom_rect geom_ribbon geom_hline annotate scale_fill_manual scale_color_manual scale_x_continuous scale_x_datetime scale_y_discrete scale_fill_gradient scale_alpha_manual labs theme element_text element_rect element_blank element_line margin coord_cartesian coord_polar
#' @import ggplot2
NULL
