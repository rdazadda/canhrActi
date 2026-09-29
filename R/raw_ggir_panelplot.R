# GGIR's own report panel, lifted rather than reproduced. The blocks marked "GGIR verbatim"
# are copied from GGIR 3.3-6 R/visualReport.R; only the indent changed. Ours is the
# wrapper that supplies the locals visualReport() would have had, drawing onto the current
# device instead of a pdf(), no page header, and one row per window instead of eight to a
# page. The input is the milestone tree raw.ggir.report() writes, which is what GGIR reads.
# GGIR is Apache-2.0, copyright Vincent van Hees and the GGIR contributors,
# https://github.com/wadpac/GGIR. The copied blocks carry that licence; a copy is at
# inst/LICENSE.GGIR.

# legend band as a fraction of a panel row; GGIR's whole row also holds the title and Info block
LEGEND_ROW_FRAC <- 0.7

#' Draw GGIR's Time Series Report Panel
#'
#' Runs GGIR's own \code{panelplot()} over a GGIR milestone directory, drawing
#' onto the current graphics device. The drawing code is copied from GGIR
#' rather than reproduced, so the result is the figure GGIR's
#' \code{visualReport()} puts in \code{report_<id>.pdf}, without the page
#' header and at whatever resolution the device was opened at.
#'
#' Size the device before calling. GGIR gives each window one eighth of an A4
#' page, so a faithful row is 8.27 by 1.461 inches. Call with
#' \code{count_only = TRUE} first to learn how many rows there will be.
#'
#' @param metadatadir A GGIR milestone directory, as written by
#'   \code{\link{write.ggir.milestone}} or by GGIR itself.
#' @param part6_threshold_combi The threshold folder under
#'   \code{meta/ms5.outraw}. Read off the directory when NULL.
#' @param file_index Which recording in that folder to draw.
#' @param hrsPerRow Hours on one row. GGIR's default is 24.
#' @param focus \code{"day"} starts rows at midnight, \code{"night"} at noon.
#' @param validcrit Drop a window whose valid fraction is below this. 0 keeps
#'   every window, and is GGIR's default.
#' @param desiredtz Time zone, GGIR's \code{params_general[["desiredtz"]]}.
#' @param params_sleep GGIR sleep parameters. Only \code{loglocation} is read,
#'   and only to decide whether the diary rows are labelled.
#' @param legend TRUE to draw GGIR's colour legend as the first row.
#' @param count_only TRUE to return the row count without drawing.
#'
#' @return Invisibly: \code{state}, \code{rows}, \code{panels},
#'   \code{height_rows} (rows scaled for the short legend band) and
#'   \code{legend}.
#' @importFrom graphics abline arrows axis legend lines mtext par plot.new
#' @importFrom graphics rect text
#' @export
raw.ggir.panelplot <- function(metadatadir,
                               part6_threshold_combi = NULL,
                               file_index = 1,
                               hrsPerRow = 24,
                               focus = "day",
                               validcrit = 0,
                               desiredtz = "",
                               params_sleep = NULL,
                               legend = TRUE,
                               count_only = FALSE) {
  out <- list(state = "ok", rows = 0L, panels = 0L, height_rows = 0,
              legend = isTRUE(legend))
  if (!requireNamespace("data.table", quietly = TRUE)) {
    out$state <- "no_data.table"
    return(invisible(out))
  }

  # GGIR verbatim, visualReport.R:22-46
  correctRect = function(starti, endi, NR, epochSize) {
    if (endi[length(endi)] > NR) {
      if (starti[length(starti)] < endi[length(endi)] - 1) {
        endi[length(endi)] = endi[length(endi)] - 1
      } else {
        starti = starti[1:(length(starti) - 1)]
        endi = endi[1:(length(endi) - 1)]
      }
    }
    invisible(list(starti = starti, endi = endi))
  }
  
  changepoints = function(x) {
    Nvalues = length(x)
    if (Nvalues > 3) {
      changes = which(x[2:Nvalues] != x[1:(Nvalues - 1)])
      changes = sort(unique(c(1, changes, changes + 1, Nvalues)))
      if (length(changes) == 0) {
        changes = 1:Nvalues
      }
    } else {
      changes = 1:Nvalues
    } 
    return(changes)
  }  

  # GGIR verbatim, visualReport.R:48-376
  panelplot = function(mdat, ylabels_plot2, binary_vars,
                       behavioral_code_names, behavioral_codes, title = "", 
                       hrsPerRow = NULL, plotid = 0, 
                       legend_items = NULL, lux_available = FALSE,
                       step_count_available = FALSE, epochSize = 60, focus = "day",
                       temperature_available = FALSE) {
    window_duration = mdat$timenum[nrow(mdat)] - mdat$timenum[1]
    signalcolor = "black"
    
    # Prepare time tick points
    date = paste0(as.numeric(format(mdat$timestamp, "%d")), " ", .format_english(mdat$timestamp, "%b"))
    hour = as.numeric(format(mdat$timestamp, "%H"))
    min = as.numeric(format(mdat$timestamp, "%M"))
    sec = as.numeric(format(mdat$timestamp, "%S"))
    ticks = which(min == 0 & sec == 0 & hour %in% seq(0, 24, by = 1))
    atTime = mdat$timestamp[ticks]
    datLIM = as.Date(min(mdat$timestamp, na.rm = TRUE), tz = desiredtz)
    if (focus == "day") {
      XLIM = as.POSIXct(paste0(datLIM[1], " 00:00:00"), tz = desiredtz)
    } else if (focus == "night") {
      XLIM = as.POSIXct(paste0(datLIM[1], " 12:00:00"), tz = desiredtz)
    }
    XLIM[2] = XLIM[1] + hrsPerRow * 3600
    
    if (step_count_available == TRUE) {
      steps_per_hour = round(zoo::rollsum(x = mdat$step_count, k = epochSize, fill = NA) / 100)
      steps_per_hour = as.character(steps_per_hour)
      steps_per_hour = gsub(pattern = "0", replacement = "", x = steps_per_hour)
    }
    if (lux_available == TRUE) {
      lux_per_hour = round(zoo::rollmax(x = mdat$lightpeak, k = epochSize, fill = NA) / 1000)
      lux_per_hour = as.character(lux_per_hour)
      lux_per_hour = gsub(pattern = "0", replacement = "", x = lux_per_hour)
    }
    
    par(mar = c(3, 0, 1.5, 2.5))
    # acceleration and angle signal on a 0 - 100 scale, angle in the bottom half
    if (lux_available == TRUE) {
      Ymax = 110
    } else {
      Ymax = 100
    }
    if (temperature_available == TRUE) {
      Ymin = -10
    } else {
      Ymin = 0
    }
    accy = (mdat$ACC / 10) + 50
    
    # identify angle columns
    anglecols = sort(grep(pattern = "angle", x = colnames(mdat), value = TRUE))
    Nangles = length(anglecols)
    if (Nangles == 1) {
      ang1 = (((mdat[, anglecols] + 90) * 40) / 180)
    } else if (Nangles == 2) {
      ang1 = (((mdat[, anglecols[1]] + 90) * 40) / 180)
      ang2 = (((mdat[, anglecols[1]] + 90) * 40) / 180)
    } else if (Nangles == 3) {
      ang1 = (((mdat[, anglecols[1]] + 90) * 40) / 180)
      ang2 = (((mdat[, anglecols[2]] + 90) * 40) / 180)
      ang3 = (((mdat[, anglecols[3]] + 90) * 40) / 180)
    }
    
    if (lux_available == TRUE) {
      luxy = (pmin(mdat$lightpeak, 20000) / 2000) + 100
    }
    if (temperature_available == TRUE) {
      temperaturey = round(pmax(pmin(diff(c(0, mdat$temperature)), 1), -1))
      temperaturey[which(temperaturey < 0)] = -1
      temperaturey[which(temperaturey > 0)] = 1
    }
    plot(0:1, 0:1,
         ylim = c(Ymin, Ymax), xlim = XLIM, 
         xaxt = 'n', axes = FALSE,
         xlab = "", ylab = "")
    
    cex_axis = 0.5
    cex_mtext = ifelse(hrsPerRow <= 36, yes = 0.4, no = 0.35)
    line = 0
    line_delta = 0.5
    # assign timestamp axis:
    mtext(text = hour[ticks], side = 1, line = line, cex = cex_mtext, at = atTime)
    mtext(text = "hour", side = 1, line = line, cex = cex_mtext,
          at = atTime[length(atTime)] + 5000, adj = 0)
    
    line = line + line_delta
    # with window numbers
    windowNow = mdat$window[ticks]
    changes = changepoints(windowNow)
    mtext(text = windowNow[changes], side = 1, line = line, cex = cex_mtext,
          at = atTime[changes])
    mtext(text = "window", side = 1, line = line, cex = cex_mtext,
          at = atTime[length(atTime)] + 5000, adj = 0)
    
    line = line + line_delta
    # with dates
    dateNow = as.numeric(format(mdat$timestamp[ticks], "%d"))
    
    changes = changepoints(dateNow)
    mtext(text = dateNow[changes], side = 1, line = line, cex = cex_mtext,
          at = atTime[changes])
    mtext(text = "date", side = 1, line = line, cex = cex_mtext,
          at = atTime[length(atTime)] + 5000, adj = 0)
    line = line + line_delta
    
    # optional variables
    if (lux_available == TRUE) {
      mtext(text = lux_per_hour[ticks], side = 1, line = line, cex = cex_mtext,
            at = atTime)
      mtext(text = "kLux", side = 1, line = line, cex = cex_mtext,
            at = atTime[length(atTime)] + 5000, adj = 0)
      line = line + line_delta
    }
    if (step_count_available == TRUE) {
      mtext(text = steps_per_hour[ticks], side = 1, line = line, cex = cex_mtext,
            at = atTime)
      mtext(text = "steps x100", side = 1, line = line, cex = cex_mtext,
            at = atTime[length(atTime)] + 5000, adj = 0)
    }
    
    # add rectangular blocks to reflect classes
    Nlevels = max(legend_items$level, na.rm = TRUE)
    for (labi in 1:length(legend_items$name)) {
      if (legend_items$name[labi] == "invalid") {
        # Will be plotted further down
      } else if (legend_items$name[labi] %in% c("nap", "sib")) {
        bin_ts = mdat$sib
        if (legend_items$name[labi] == "sib") {
          bin_ts[which(bin_ts != 1)] = 0
        } else if (legend_items$name[labi] == "nap") {
          bin_ts[which(bin_ts != 2)] = 0
          bin_ts[which(bin_ts == 2)] = 1
        }
        freqtab = table(bin_ts)
        if (length(freqtab) > 1 || names(freqtab)[1] == "1") {
          starti = which(diff(c(0, bin_ts)) == 1)
          endi = which(diff(c(bin_ts, 0)) == -1) + 1
          newi = correctRect(starti, endi, NR = nrow(mdat), epochSize)
          t0 = mdat$timestamp[starti = newi$starti]
          t1 = mdat$timestamp[endi = newi$endi]
          y0 = 25
          y1 = 50
          if (legend_items$name[labi] == "nap") {
            y1 = 100
          }
          col = legend_items$col[labi]
          rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1, col = col, border = FALSE)
        }
      } else {
        diary_names = c("diary_nap", "diary_nonwear", "diary_sleepwindow", "diary_timeinbed")
        if (legend_items$name[labi] %in% diary_names) {
          # Diary-based classes
          if (legend_items$name[labi] == "diary_sleepwindow") {
            relevant_labels = c("sleeplog", "sleeplog+bedlog")
          } else if (legend_items$name[labi] == "diary_timeinbed") {
            relevant_labels = c("bedlog", "sleeplog+bedlog")
          } else {
            relevant_labels = legend_items$name[labi]
          }
          tempi = which(mdat$selfreported %in% relevant_labels)
          y0 = 0
          y1 = 25
          if (legend_items$name[labi] == "diary_sleepwindow") {
            y0 = 0
            y1 = 12.5
          } else if (legend_items$name[labi] == "diary_timeinbed") {
            y0 = 12.5
            y1 = 25
          }
        } else {
          # Accelerometer- based classes
          tempi = which(mdat$class_id == legend_items$code[labi])
          y0 = 50
          if (length(grep(pattern = "unbt", x = legend_items$name[labi])) > 0) {
            y1 = 90 # lower rectangle for unbouted behaviour
          } else {
            y1 = 100
          }
        }
        if (length(tempi) > 0) {
          A = rep(0, nrow(mdat))
          A[tempi] = 1
          starti = which(diff(c(0, A)) == 1)
          endi = which(diff(c(A, 0)) == -1) + 1
          newi = correctRect(starti, endi, NR = nrow(mdat), epochSize)
          starti = newi$starti
          endi = newi$endi
          t0 = mdat$timestamp[starti]
          t1 = mdat$timestamp[endi]
          col = legend_items$col[labi]
          rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1 , col = col, border = FALSE)
        }
      }
    }
    
    # Add angle grid lines
    abline(h = c(10, 30), lty = 2, lwd = 0.3)
    if (Nangles > 1) {
      abline(h = c(0, 20, 40), lty = 3, lwd = 0.3)
    }
    # Add acc line
    lines(mdat$timestamp, accy, type = "l",
          col = signalcolor,
          lwd = 0.3)
    
    if (lux_available == TRUE) {
      # Add lux
      lines(mdat$timestamp, luxy, type = "l",
            col = signalcolor,
            lwd = 0.3)
    }
    if (temperature_available == TRUE) {
      # Add temperature
      text(x = mdat$timestamp[which(temperaturey == -1)], y = -9, labels = "-", cex = 0.5)
      text(x = mdat$timestamp[which(temperaturey == 1)], y = -9, labels = "+", cex = 0.5)
    }
    angleColor = ifelse(Nangles > 1, yes = "orange", no = signalcolor)
    # Add angle lines on top
    if (Nangles > 0) {
      lines(mdat$timestamp, ang1, type = "l", col = angleColor, lwd = 0.3)
    }
    if (Nangles > 1) {
      lines(mdat$timestamp, ang2, type = "l", col = "red", lwd = 0.3)
    }
    if (Nangles > 2) {
      lines(mdat$timestamp, ang3, type = "l", col = "green", lwd = 0.3)
    }
    text(x = mdat$timestamp[1], y = 60, labels = "Acceleration",
         pos = 4, cex = 0.7, col = signalcolor, font = 2)
    textAngle = ifelse(Nangles > 1, yes = "Angles", no = "Angle")
    text(x = mdat$timestamp[1], y = 40, labels = textAngle,
         pos = 4, cex = 0.7, col = signalcolor, font = 2)
    
    if (lux_available == TRUE) {
      text(x = mdat$timestamp[1], y = 105, labels = "Lux",
           pos = 4, cex = 0.7, col = signalcolor, font = 2)
    }
    if (temperature_available == TRUE) {
      text(x = mdat$timestamp[1], y = -2, labels = "Temperature change",
           pos = 4, cex = 0.7, col = signalcolor, font = 2)
    }
    # Highlight invalid epochs as hashed area on top of all rects
    if ("invalid" %in% legend_items$name) {
      freqtab = table(mdat$invalid)
      if (length(freqtab) > 1 || names(freqtab)[1] == "1") {
        starti = which(diff(c(0, mdat$invalid)) == 1)
        endi = which(diff(c(mdat$invalid, 0)) == -1) + 1
        newi = correctRect(starti, endi, NR = nrow(mdat), epochSize)
        t0 = mdat$timestamp[newi$starti]
        t1 = mdat$timestamp[newi$endi]
        col = legend_items$col[which(legend_items$name == "invalid")]
        if (temperature_available == TRUE) {
          y0 = -5
        } else {
          y0 = 5
        }
        if (lux_available == TRUE) {
          y1 = 105
        } else {
          y1 = 95
        }
        transparantWhite = grDevices::adjustcolor(col = "white", alpha.f = 0.8)
        rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1,
             col = transparantWhite, lwd = 0.8, border = "grey")
      }
    }
    
    # marker button
    if ("marker" %in% colnames(mdat)) {
      marker_moments = which(mdat$marker == 1)
      if (length(marker_moments) > 0) {
        arrows(x0 = mdat$timestamp[marker_moments],
               x1 = mdat$timestamp[marker_moments],
               y0 = 30, y1 = 0, col = "purple", lwd = 1, length = 0.06)
        text(x = mdat$timestamp[marker_moments],y = 30, labels = "M", font = 2, col = "purple", cex = 1.2)
      }
    }
    
    # onset/wake lines:
    window_edges = which(diff(mdat$SleepPeriodTime) != 0)
    if (length(window_edges) > 0) {
      abline(v = mdat$timestamp[window_edges], col = "black", lwd = 1.5, lty = 2)
      for (wei in 1:length(window_edges)) {
        # add text labels for onset, wake, weekday and guider being used
        guider_names = c('unknown', 'sleeplog', 'HDCZA', 'setwindow', 
                         'L512', 'HorAngle', 'NotWorn', 'markerbutton',
                         'HLRB', 'MotionWare')
        guider_name =  paste0("guided by: ", guider_names[mdat$guider[window_edges[wei]] + 1])        
        
        if (mdat$SleepPeriodTime[window_edges[wei]] == 1) {
          toptext = paste0("wake-up ", .in_c_time(weekdays(mdat$timestamp[window_edges[wei]])))
          
          text(x = mdat$timestamp[window_edges[wei]], y = 92,
               labels = guider_name, las = 2, srt = 90, pos = 2,
               offset = 0.4, adj = 0, cex = 0.6)
        } else {
          toptext = "onset"  
          text(x = mdat$timestamp[window_edges[wei]], y = 2,
               labels = guider_name, las = 2, srt = 90, pos = 4,
               offset = 0.4, adj = 0, cex = 0.6)
        }
        mtext(text = toptext, side = 3, line = 0, cex = 0.5,
              at = mdat$timestamp[window_edges[wei]])
        
      }
    }
    # diaryImputationCode availabe
    if ("diaryImputationCode" %in% names(mdat)) {
      temp_index_0 = pmax(1, round(nrow(mdat) * (hrsPerRow - 24) / 24))
      diaryImputationCode = unique(mdat$diaryImputationCode[temp_index_0:nrow(mdat)])
      diaryImputationCode = diaryImputationCode[is.na(diaryImputationCode) == FALSE]
      if (length(diaryImputationCode) > 0) {
        diaryImputationCode = unlist(strsplit(as.character(diaryImputationCode), ""))
        missingZeros = 4 - length(diaryImputationCode)
        if (missingZeros > 0) {
          diaryImputationCode = c(rep("0", missingZeros), diaryImputationCode)
        }
        diaryImputationCode = paste0(diaryImputationCode, collapse = "")
        diaryImputationCode = paste0("diary imputation: ", diaryImputationCode)
        mtext(text = diaryImputationCode, side = 4, line = -2, cex = 0.3, las = 1)
      }
    }
  }

  # GGIR verbatim, visualReport.R:378-403
  gen_col_names = function(behavioral_code_names, behavioral_codes = NULL,
                           name, legend_items, colour = NULL,
                           level = NULL, reverse = TRUE) {
    # generate colour and names for legend
    vars = grep(pattern = name, x = behavioral_code_names, value = TRUE)
    Nitems = length(vars)
    if (Nitems > 0) {
      legend_items$name = c(legend_items$name, vars)
      legend_items$level = c(legend_items$level, rep(level, Nitems))
      if (!is.null(behavioral_codes)) {
        legend_items$code = c(legend_items$code, behavioral_codes[which(behavioral_code_names %in% vars)])
      } else {
        legend_items$code = c(legend_items$code, rep(-1, Nitems))
      }
      col = rep(colour, length.out = Nitems)
      if (length(colour) == 1) {
        for (ci in 1:Nitems) {
          col[ci] = grDevices::adjustcolor(col = col[ci],
                                           alpha.f = 0.2 + (ci / Nitems) * 0.8)
        }
      }
      if (reverse == TRUE) col = rev(col)
      legend_items$col = c(legend_items$col, col)
    }
    return(legend_items)
  }

  # end GGIR verbatim

  # the locals visualReport() holds at this point, so the verbatim blocks run unchanged
  if (is.null(part6_threshold_combi)) {
    combi <- setdiff(list.dirs(file.path(metadatadir, "meta", "ms5.outraw"),
                               recursive = FALSE, full.names = FALSE),
                     "sib.reports")
    if (length(combi) == 0) {
      out$state <- "no_ms5outraw"
      return(invisible(out))
    }
    part6_threshold_combi <- combi[1]
  }
  fnames.ms5raw <- dir(paste0(metadatadir, "/meta/ms5.outraw/", part6_threshold_combi),
                       pattern = "[.]RData")
  if (length(fnames.ms5raw) == 0) {
    out$state <- "no_timeseries"
    return(invisible(out))
  }
  i <- f0 <- f1 <- max(1L, min(as.integer(file_index), length(fnames.ms5raw)))
  params_output <- list(visualreport_hrsPerRow = hrsPerRow,
                        visualreport_focus = focus,
                        visualreport_validcrit = validcrit)
  ylabels_plot2 <- binary_vars <- NULL
  mdat <- NULL

  # GGIR verbatim, visualReport.R:652-680, the behavioural class names and codes
  legendfiles = list.files(path = paste0(metadatadir, "/meta/ms5.outraw"),
                           pattern = "codes", full.names = TRUE)
  df = file.info(legendfiles)
  legendfiles = rownames(df)[which.max(df$mtime)]
  legendF = data.table::fread(file = rownames(df)[which.max(df$mtime)], data.table = FALSE)
  behavioral_code_names = legendF$class_name # behavioural class names (characters)
  behavioral_codes = legendF$class_id # behavioural class codes (numeric)
  # reorder and rename behavioural class names and codes:
  neworder = c(grep("sleep", x = behavioral_code_names), grep("IN", x = behavioral_code_names),
               grep("LIG", x = behavioral_code_names), grep("MOD", x = behavioral_code_names),
               grep("VIG", x = behavioral_code_names), grep("MVPA", x = behavioral_code_names)) 
  behavioral_code_names = behavioral_code_names[neworder]
  behavioral_codes = behavioral_codes[neworder]
  behavioral_code_names = gsub("day_|spt_", "", x = behavioral_code_names)
  behavioral_code_names = gsub("sleep", "spt_sleep", x = behavioral_code_names)
  behavioral_code_names = gsub("wake_IN", "spt_wake_inactive", x = behavioral_code_names)
  behavioral_code_names = gsub("wake_LIG", "spt_wake_lipa", x = behavioral_code_names)
  behavioral_code_names = gsub("wake_MOD", "spt_wake_moderate", x = behavioral_code_names)
  behavioral_code_names = gsub("wake_VIG", "spt_wake_vigorous", x = behavioral_code_names)
  behavioral_code_names = tolower(behavioral_code_names)
  behavioral_code_names = gsub("lig_", "lipa_", x = behavioral_code_names)
  behavioral_code_names = gsub("in_bts", "inactive_bts", x = behavioral_code_names)
  behavioral_code_names = gsub("in_unbt", "inactive_unbt", x = behavioral_code_names)
  # move unbouted to the end for logical order
  neworder = c(grep(pattern = "unbt", x = behavioral_code_names, invert = TRUE),
               grep(pattern = "unbt", x = behavioral_code_names)) 
  behavioral_code_names = behavioral_code_names[neworder]
  behavioral_codes = behavioral_codes[neworder]

  # GGIR verbatim, visualReport.R:684-713, mdat and the row labels
  suitable_file_found = FALSE
  filei = f0
  while (suitable_file_found == FALSE && filei <= f1) {
    load(file = paste0(metadatadir, "/meta/ms5.outraw/",
                       part6_threshold_combi, "/", fnames.ms5raw[filei]))
    if (length(mdat) != 0 && nrow(mdat) != 0) {
      names(mdat)[which(names(mdat) == "sibdetection")] = "sib"
      names(mdat)[which(names(mdat) == "invalidepoch")] = "invalid"
      mdat$sib[which(mdat$SleepPeriodTime == 1)] = 0
      ylabels_plot2 = NULL
      # rename selfreported terminology
      if ("selfreported" %in% colnames(mdat) &&
          !is.null(params_sleep) &&
          !is.null(params_sleep[["loglocation"]])) {
        ylabels_plot2 = c("nap", "nonwear", "sleeplog", "bedlog")
      }
      binary_vars = c("SleepPeriodTime", "sibdetection", "invalidepoch")
      ylabels_plot2 = c(binary_vars, ylabels_plot2)
      ylabels_plot2 = gsub("invalidepoch", "invalid", x = ylabels_plot2)
      ylabels_plot2 = gsub("SleepPeriodTime", "spt", x = ylabels_plot2)
      ylabels_plot2 = gsub("sibdetection", "sib", x = ylabels_plot2)
      ylabels_plot2 = gsub("nap", "diary_nap", x = ylabels_plot2)
      ylabels_plot2 = gsub("nonwear", "diary_nonwear", x = ylabels_plot2)
      ylabels_plot2 = gsub("sleeplog", "diary_sleepwindow", x = ylabels_plot2)
      ylabels_plot2 = gsub("bedlog", "diary_timeinbed", x = ylabels_plot2)
      ylabels_plot2 = tolower(ylabels_plot2)
      suitable_file_found = TRUE
    }
    filei = filei + 1
  }

  # end GGIR verbatim

  if (is.null(mdat) || nrow(mdat) == 0) {
    out$state <- "empty"
    return(invisible(out))
  }

  # GGIR verbatim, visualReport.R:406-541, legend_items and the window index; the if() it
  # opens is closed after the drawing loop
mdat = NULL
load(file = paste0(metadatadir, "/meta/ms5.outraw/",
                   part6_threshold_combi, "/", fnames.ms5raw[i]))
if (length(mdat) != 0 && nrow(mdat) != 0) {
  names(mdat)[which(names(mdat) == "sibdetection")] = "sib"
  names(mdat)[which(names(mdat) == "invalidepoch")] = "invalid"
  mdat$sib[which(mdat$SleepPeriodTime == 1)] = 0
  if ("selfreported" %in% colnames(mdat)) {
    sr_levelnames = levels(mdat$selfreported)
    sr_levelnames = gsub("nap", "diary_nap", x = sr_levelnames)
    sr_levelnames = gsub("nonwear", "diary_nonwear", x = sr_levelnames)
    levels(mdat$selfreported) = sr_levelnames
  }
  # assess which information is available
  if ("lightpeak" %in% colnames(mdat)) {
    lux_available = TRUE
  } else {
    lux_available = FALSE
  }
  if ("temperature" %in% colnames(mdat)) {
    temperature_available = TRUE
  } else {
    temperature_available = FALSE
  }
  if ("step_count" %in% colnames(mdat)) {
    step_count_available = TRUE
  } else {
    step_count_available = FALSE
  }
  epochSize = diff(mdat$timenum[1:2])
  # Define legend
  legend_items = list(col = NULL, name = NULL, code = NULL, level = NULL)
  
  # Sleep diary
  legend_items = gen_col_names(ylabels_plot2, name = "diary",
                               legend_items = legend_items,
                               colour = c("#FF00FF", "#FFD700", "#7CFC00", "red"),
                               level = 1, reverse = TRUE)
  # SIB (day time)
  legend_items$col = c(legend_items$col, "steelblue4")
  legend_items$name = c(legend_items$name, "sib")
  legend_items$code = c(legend_items$code, -1)
  legend_items$level = c(legend_items$level, 2)
  
  legend_items$col = c(legend_items$col, "steelblue1")
  legend_items$name = c(legend_items$name, "nap")
  legend_items$code = c(legend_items$code, -1)
  legend_items$level = c(legend_items$level, 2)
  
  # Sleep in SPT
  legend_items = gen_col_names(behavioral_code_names,
                               behavioral_codes = behavioral_codes,
                               name = "spt_sleep",
                               legend_items = legend_items, colour = "white",
                               level = 2)
  # Wake in SPT
  legend_items = gen_col_names(behavioral_code_names,
                               behavioral_codes = behavioral_codes,
                               name = "spt_wake",
                               legend_items = legend_items, colour = "yellow3",
                               level = 2, reverse = FALSE)
  # Inactivity
  not_spt = grep(pattern = "spt", x = behavioral_code_names, invert = TRUE)
  legend_items = gen_col_names(behavioral_code_names = behavioral_code_names[not_spt],
                               behavioral_codes = behavioral_codes[not_spt],
                               name = "inactive",
                               legend_items = legend_items, colour = "#CC79A7",
                               level = 2, reverse = FALSE)
  # LIPA
  legend_items = gen_col_names(behavioral_code_names = behavioral_code_names[not_spt],
                               behavioral_codes = behavioral_codes[not_spt],
                               name = "lipa",
                               legend_items = legend_items, colour = "#009E73",
                               level = 2, reverse = FALSE)
  # MVPA
  legend_items = gen_col_names(behavioral_code_names = behavioral_code_names[not_spt],
                               behavioral_codes = behavioral_codes[not_spt],
                               name = "mod|vig|mvpa",
                               legend_items = legend_items, colour = "#D55E00",
                               level = 2, reverse = FALSE)
  # Invalid
  legend_items$col = c(legend_items$col, "grey")
  legend_items$name = c(legend_items$name, "invalid")
  legend_items$code = c(legend_items$code, -1)
  legend_items$level = c(legend_items$level, 0)
  
  simple_filename = gsub(pattern = ".RData", "", x = fnames.ms5raw[i] )
  hrsPerRow = params_output[["visualreport_hrsPerRow"]]
  focus = params_output[["visualreport_focus"]]
  if (focus == "day") {
    dayedges = which(format(mdat$timestamp, "%H") == "00" &
                       format(mdat$timestamp, "%M") == "00" &
                       format(mdat$timestamp, "%S") == "00")
  } else if (focus == "night") {
    dayedges = which(format(mdat$timestamp, "%H") == "12" &
                       format(mdat$timestamp, "%M") == "00" &
                       format(mdat$timestamp, "%S") == "00")
  }
  # Identify indices for start and end of each window
  if (dayedges[1] == 1) {
    # recording starts at edge
    subploti = dayedges
    dayEnds = c(dayedges[2:length(dayedges)] + ((hrsPerRow - 24) * (3600/epochSize)) - 1, nrow(mdat))
  } else {
    # recording does not start at edge
    subploti = c(1, dayedges)
    dayEnds = c(dayedges + ((hrsPerRow - 24) * (3600/epochSize)) - 1, nrow(mdat))
    
  }
  subploti = cbind(subploti, dayEnds)
  invalid = which(mdat$invalidepoch == 1)
  subploti[which(subploti[,2] > nrow(mdat)), 2] = nrow(mdat)
  NdaysPerPage = 8
  skip_pdf_generation = FALSE
  if ( params_output[["visualreport_validcrit"]] > 0) {
    # Only keep windows that meet the fraction of valid data
    for (si in 1:nrow(subploti)) {
      Nvalid = length(which(mdat$invalid[(subploti[si, 1] + 1):subploti[si, 2]] == 0))
      Ntotal =  length(mdat$invalid[(subploti[si, 1] + 1):subploti[si, 2]])
      frac_valid = Nvalid / Ntotal
      if (frac_valid < params_output[["visualreport_validcrit"]]) {
        subploti[si, ] = -1
      }
    }
    valid_rows = which(subploti[,1] > 0)
    if (length(valid_rows) > 0) {
      subploti = subploti[valid_rows, , drop = FALSE]
    }
    if (length(valid_rows) == 0 || inherits(x = subploti, what = "matrix") == FALSE || nrow(subploti) == 0) {
      message(paste0("Recording ", simple_filename, " skipped from visual",
                     " report generation because valid",
                     " data criteria (visualreport_validcrit) not met."))
      skip_pdf_generation = TRUE
    }
  }

  # end GGIR verbatim

    # one row per window on the current device; GGIR's own one hour gate decides which
    # windows are drawn
    keep <- integer(0)
    if (skip_pdf_generation == FALSE && nrow(subploti) > 0) {
      keep <- which(subploti[, 2] - subploti[, 1] > 60 * (60 / epochSize))
    }
    out$panels <- length(keep)
    out$rows <- out$panels + as.integer(isTRUE(legend) && out$panels > 0)
    # the legend needs about half a row without the title and Info block; layout() lets
    # one row be shorter, and height_rows is what the device should be sized to
    lg <- isTRUE(legend) && out$panels > 0
    out$height_rows <- out$panels + (if (lg) LEGEND_ROW_FRAC else 0)
    if (isTRUE(count_only) || out$rows == 0) return(invisible(out))

    old <- graphics::par(no.readonly = TRUE)
    on.exit(try(graphics::par(old), silent = TRUE), add = TRUE)
    graphics::layout(matrix(seq_len(out$rows), ncol = 1),
                     heights = c(if (lg) LEGEND_ROW_FRAC, rep(1, out$panels)))
    graphics::par(mgp = c(2, 0.8, 0), omi = c(0, 0, 0, 0), bty = "n")

    if (isTRUE(legend)) {
      # a band half as tall as GGIR's cannot spare its three line bottom margin, or legend()
      # clips to one row
      par(mar = c(0.2, 0, 0.6, 0.2))
      # GGIR verbatim, visualReport.R:555-584, the colour legend without the title and Info block
        plot.new()
        legendnames = legend_items$name
        legendnames[which(legendnames == "sib")] = "no movement (sib daytime)"
        legendnames[which(legendnames == "nap")] = "nap"
        legendnames[which(legendnames == "sleep_in_spt")] = "sleep"
        legendnames = gsub(pattern = "_", replacement = " ", x = legendnames)
        boutvars = grep(pattern = "bts", x = legendnames, value = FALSE)
        for (bvi in 1:length(boutvars)) {
          tmp_split = unlist(strsplit(legendnames[boutvars[bvi]], " "))
          if (length(tmp_split) == 3) {
            legendnames[boutvars[bvi]] = paste0(tmp_split[1], " ", tmp_split[2],
                                                " >=", tmp_split[3])
          } else {
            legendnames[boutvars[bvi]] = paste0(tmp_split[1], " ",tmp_split[2],
                                                " [", tmp_split[3], ",", tmp_split[4], ")")
          }
        }
        legendnames[boutvars] = paste0(legendnames[boutvars], " mins")
        legendcolors = legend_items$col
        
        not_invalid = which(legendnames != "invalid")
        legendpch = rep(15, length(legendnames))
        legendpch[which(legendnames == "invalid")] = 0
        legendcolors[which(legendnames == "invalid")] = "grey"
        legendnames[which(legendnames == "invalid")] = "ignored/imputed"
        legend("topright", legend = legendnames,
               col = legendcolors,
               ncol = length(legend_items$name) %/% 6 + 1, cex = 0.8,
               pch = legendpch, pt.cex = 2, bty = "n", title = "Legend:",
               title.font = 2, title.adj = 0)
      # end GGIR verbatim
    }

    for (ani in keep) {
      # GGIR verbatim, visualReport.R:619-625
        panelplot(mdat[(subploti[ani, 1] + 1):subploti[ani, 2], ],
                  ylabels_plot2, binary_vars,
                  behavioral_code_names, behavioral_codes, title = "",
                  hrsPerRow = hrsPerRow, plotid = ani,
                  legend_items = legend_items, lux_available = lux_available,
                  step_count_available = step_count_available, epochSize = epochSize,
                  focus = focus, temperature_available = temperature_available)
      # end GGIR verbatim
    }
  }
  invisible(out)
}

# GGIR's page geometry in inches: pdf(paper = "a4") insets the plotting region by a
# quarter inch on every side, so a panel is 7.77 wide and eight share 11.19 of height
GGIR_PANEL_W <- 7.77
GGIR_PANEL_H <- 11.19 / 8

#' Render GGIR's Report Panel to a png
#'
#' Draws \code{\link{raw.ggir.panelplot}} into a pdf at GGIR's own page geometry and
#' rasterises it at the width asked for. At a five second epoch one class bout is a
#' twentieth of a pixel wide, which a vector device still marks and a raster device
#' antialiases away, so drawing straight to png loses most of the behavioural classes.
#'
#' @param metadatadir A GGIR milestone directory.
#' @param file Where to write the png.
#' @param width_px Width of the image in pixels. Pass the panel width times
#'   the device pixel ratio.
#' @param ... Passed to \code{\link{raw.ggir.panelplot}}.
#'
#' @return Invisibly, a list with \code{state}, \code{file}, \code{width},
#'   \code{height} and \code{rows}.
#' @export
raw.ggir.panel.png <- function(metadatadir, file, width_px = 1600, ...) {
  out <- list(state = "ok", file = NA_character_, width = 0L, height = 0L, rows = 0L)
  if (!requireNamespace("pdftools", quietly = TRUE) ||
      !requireNamespace("png", quietly = TRUE)) {
    out$state <- "no_pdftools"
    return(invisible(out))
  }
  n <- raw.ggir.panelplot(metadatadir, count_only = TRUE, ...)
  if (!identical(n$state, "ok") || n$rows == 0) {
    out$state <- if (identical(n$state, "ok")) "no_windows" else n$state
    return(invisible(out))
  }
  out$rows <- n$rows
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::pdf(tmp, width = GGIR_PANEL_W, height = GGIR_PANEL_H * n$height_rows)
  drew <- tryCatch({
    raw.ggir.panelplot(metadatadir, ...)
    TRUE
  }, error = function(e) conditionMessage(e))
  grDevices::dev.off()
  if (!isTRUE(drew)) {
    out$state <- "draw_failed"
    out$messages <- as.character(drew)
    return(invisible(out))
  }
  dpi <- width_px / GGIR_PANEL_W
  bmp <- pdftools::pdf_render_page(tmp, page = 1, dpi = dpi)
  png::writePNG(bmp, file)
  out$file <- file
  out$width <- dim(bmp)[2]
  out$height <- dim(bmp)[3]
  invisible(out)
}
