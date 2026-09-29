# GGIR's sleep visualisation, lifted rather than redrawn: the drawing calls are copied from
# GGIR 3.3-6 R/g.part4.R. Two edits: the plot() call drops main = paste0("Page ", pagei),
# and the left margin in par(mar) is measured from this recording's row labels instead of
# GGIR's 5 lines. No title or caption is added. The rest is plumbing: the current device,
# one recording, GGIR's variable names filled from the episodes and guiders tables on
# raw.sleep.nights()'s "canhrActi" attribute.
# GGIR is Apache-2.0, copyright Vincent van Hees and the GGIR contributors,
# https://github.com/wadpac/GGIR. The copied blocks carry that licence; a copy is at
# inst/LICENSE.GGIR.

#' Draw GGIR's Sleep Visualisation
#'
#' Runs GGIR's own part-4 plotting code over one recording's night summary and
#' draws onto the current graphics device, so the result is the figure GGIR
#' writes to \code{results/visualisation_sleep.pdf}, without its page number.
#'
#' One row per night on GGIR's 12 to 36 axis relabelled 12..24, 1..12. Every
#' sustained inactivity bout is a bar, coloured from
#' \code{rainbow(start = 0.7, end = 1)} when it overlaps the guider window and
#' \code{rainbow(start = 0.2, end = 0.4)} when it does not, one lane per sib
#' definition. The guider window is a hatched black rectangle. A day sleeper
#' gets a mark at 18:00. Nights GGIR would have cleaned out are not drawn, as
#' in GGIR.
#'
#' @param nights A night summary from \code{\link{raw.sleep.nights}}, carrying
#'   its \code{episodes} and \code{guiders} tables.
#' @param nnpp Nights per page, GGIR's own hard-coded 40. Left NULL it is the
#'   kept nights plus the smallest headroom that puts GGIR's legend clear of
#'   the top row at this panel's size. Set it to hold the vertical scale
#'   steady across recordings.
#' @param legend TRUE to draw GGIR's legend.
#'
#' @return Invisibly, a list with \code{state} and \code{nights}, the number of
#'   rows drawn.
#' @importFrom grDevices rainbow
#' @export
raw.ggir.sleepplot <- function(nights, nnpp = NULL, legend = TRUE) {
  out <- list(state = "ok", nights = 0L)
  a <- attr(nights, "canhrActi")
  if (is.null(a) || is.null(a$episodes) || is.null(a$guiders)) {
    out$state <- "no_episodes"
    return(invisible(out))
  }
  ep <- a$episodes
  gu <- a$guiders
  if (!is.data.frame(gu) || nrow(gu) == 0) {
    out$state <- "no_nights"
    return(invisible(out))
  }

  # GGIR's own names, filled from the package's tables; undef is the set of sib definitions
  undef <- unique(as.character(ep$def))
  if (length(undef) == 0) undef <- unique(as.character(nights$sleepparam))
  doplot <- TRUE
  # GGIR drops a night whose cleaningcode reaches the criterion: 1 with a diary, 2 without
  cleaningcriterion <- if (isTRUE(a$dolog)) 1 else 2
  keep <- which(as.numeric(gu$cleaningcode) < cleaningcriterion)
  out$nights <- length(keep)
  if (length(keep) == 0) {
    out$state <- "all_cleaned"
    return(invisible(out))
  }
  daysleeper <- as.logical(gu$daysleeper)
  # GGIR's accid is the recording ID; here it is the file name, hence the measured margin
  accid <- as.character(nights$ID[1] %||% a$id)
  addlegend <- isTRUE(legend)
  legnames <- c(paste0("sib", undef, "_spt"), paste0("sib", undef, "_day"),
                "guider, e.g. diary")

  old <- graphics::par(no.readonly = TRUE)
  on.exit(try(graphics::par(old), silent = TRUE), add = TRUE)

  # GGIR always gives the figure 40 rows, so its legend and label margin always have room.
  # .sleepplot.fit() measures both on this panel, and the headroom below solves
  # k = (f * nk + 0.35) / (1 - f) for the smallest k that clears the legend of the top row.
  fit <- .sleepplot.fit(legnames, min(c(3, length(legnames))),
                        paste0("ID", accid, " night", gu$night[keep]))
  if (is.null(nnpp)) {
    f <- if (addlegend) min(0.6, fit$legend_frac) else 0
    nnpp <- length(keep) + (f * length(keep) + 0.35) / (1 - f)
  }
  idlabels <- rep(0, max(1, floor(nnpp)))
  cnt <- 1

  # GGIR verbatim: the page, with the two edits named in the header
  par(mar = c(4, fit$mar_left, 1, 2) + 0.1)
  plot(c(0, 0), c(1, 1), xlim = c(12, 36), ylim = c(0, nnpp), col = "white", axes = FALSE, xlab = "time",
       ylab = "")
  axis(side = 1, at = 12:36, labels = c(12:24, 1:12), cex.axis = 0.7)
  abline(v = c(18, 24, 30), lwd = 0.2, lty = 2)
  abline(v = c(15, 21, 27, 33), lwd = 0.2, lty = 3, col = "grey")

  for (jj in keep) {
    j <- jj
    GuiderOnset <- as.numeric(gu$guider_onset[j])
    GuiderWake  <- as.numeric(gu$guider_wakeup[j])
    for (defi in undef) {
      spocum.t <- ep[ep$night == gu$night[j] & as.character(ep$def) == defi, , drop = FALSE]
      if (nrow(spocum.t) == 0) next
      # GGIR verbatim: the bars and the guider window
      idlabels[cnt] = paste0("ID", accid, " night", gu$night[j])
      den = 20
      defii = which(undef == defi)
      qtop = ((defii/length(undef)) * 0.6) - 0.3
      qbot = (((defii - 1)/length(undef)) * 0.6) - 0.3
      # add bar for each sleep defintion of accelerometer
      for (pli in 1:nrow(spocum.t)) {
        if (spocum.t$start[pli] > spocum.t$end[pli]) {
          if (pli > 1 & pli < nrow(spocum.t) &
              abs(as.numeric(spocum.t$start[pli]) - as.numeric(spocum.t$end[pli])) < 2) {
            spocum.t[pli, c("start", "end")] = spocum.t[pli, c("end", "start")]
          }
        }
        if (spocum.t$overlapGuider[pli] == 1) {
          colb = rainbow(length(undef), start = 0.7, end = 1)
        } else {
          colb = rainbow(length(undef), start = 0.2, end = 0.4)
        }
        if (spocum.t$start[pli] > spocum.t$end[pli]) {
          # plot sib that starts on the right (morning) and ends on the left (afternoon)
          rect(xleft = spocum.t$start[pli], ybottom = (cnt + qbot), xright = 36,
               ytop = (cnt + qtop), col = colb[defii], border = NA)
          rect(xleft = 12, ybottom = (cnt + qbot), xright = spocum.t$end[pli], ytop = (cnt + qtop),
               col = colb[defii], border = NA)
        } else {
          rect(xleft = spocum.t$start[pli], ybottom = (cnt + qbot), xright = spocum.t$end[pli],
               ytop = (cnt + qtop), col = colb[defii], border = NA)
        }
      }
      GuiderWaken = GuiderWake
      GuiderOnsetn = GuiderOnset
      
      if (GuiderWake > 36) GuiderWaken = GuiderWake - 24
      if (GuiderOnset > 36) GuiderOnsetn = GuiderOnset - 24
      if (defi == undef[length(undef)]) {
        # only plot log for last definition night sleeper
        
        if (GuiderOnsetn > GuiderWaken) {
          # day sleeper
          rect(xleft = GuiderOnsetn, ybottom = (cnt - 0.3), xright = 36, ytop = (cnt + 0.3),
               col = "black", border = TRUE, density = den)
          rect(xleft = 12, ybottom = (cnt - 0.3), xright = GuiderWaken, ytop = (cnt + 0.3),
               col = "black", border = TRUE, density = den)
        } else {
          rect(xleft = GuiderOnsetn, ybottom = (cnt - 0.3), xright = GuiderWaken, ytop = (cnt + 0.3),
               col = "black", border = TRUE, density = den)
        }
      }
    }
    # GGIR verbatim: the row rule and the day sleeper
    lines(x = c(12, 36), y = c(cnt, cnt), lwd = 0.2, lty = 2)  #abline(h=cnt,lwd=0.2,lty=2)
    if (daysleeper[j] == TRUE) {
      lines(x = c(18, 18), y = c((cnt - 0.3), (cnt + 0.3)), lwd = 2, lty = 2, col = "black")
    }
    # only increase count if there was bar plotted and it is the last definition
    if (defi == undef[length(undef)]) {
      cnt = cnt + 1
    }
  }

  # GGIR verbatim: the legend and the night labels down the left
  if (addlegend == TRUE) {
    colb_spt = rainbow(length(undef), start = 0.7, end = 1)
    colb_day = rainbow(length(undef), start = 0.2, end = 0.4)
    colb = c(colb_spt, colb_day)
    legnames = c(paste0("sib", undef, "_spt"), paste0("sib", undef, "_day"), "guider, e.g. diary")
    legend("top", legend = legnames, density = c(rep(NA, 2 * length(undef)), 40), fill = c(colb, "black"),
           border = c(colb, "black"), ncol = min(c(3, length(legnames))), cex = 0.7)
    addlegend = FALSE
  }
  zerolabel = which(idlabels == 0)
  if (length(zerolabel) > 0) idlabels[zerolabel] = " "
  axis(side = 2, at = 1:nnpp, labels = idlabels, las = 1, cex.axis = 0.5)

  invisible(out)
}

# Measure GGIR's legend and row labels on a null device of the same size as the one about to
# be drawn on; legend(plot = FALSE) and strwidth() only report. Returns the legend height as
# a fraction of the plot region and the left margin the labels need, in lines.
.sleepplot.fit <- function(legnames, ncol, labels) {
  out <- list(legend_frac = 0.22, mar_left = 5)
  din <- tryCatch(graphics::par("din"), error = function(e) c(7, 3))
  if (length(din) != 2 || any(!is.finite(din)) || any(din <= 0)) din <- c(7, 3)
  try({
    grDevices::pdf(file = NULL, width = din[1], height = din[2])
    on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
    graphics::par(mar = c(4, 5, 1, 2) + 0.1)
    graphics::plot(0, 0, xlim = c(12, 36), ylim = c(0, 10), type = "n",
                   axes = FALSE, ann = FALSE)
    usr <- graphics::par("usr")
    lg <- graphics::legend("top", legend = legnames, ncol = ncol, cex = 0.7,
                           fill = rep("black", length(legnames)), plot = FALSE)
    out$legend_frac <- as.numeric(lg$rect$h) / (usr[4] - usr[3])
    w <- suppressWarnings(max(graphics::strwidth(labels, units = "inches", cex = 0.5)))
    out$mar_left <- w / graphics::par("csi") + 1.2
  }, silent = TRUE)
  if (!is.finite(out$legend_frac) || out$legend_frac <= 0) out$legend_frac <- 0.22
  if (!is.finite(out$mar_left)) out$mar_left <- 5
  out$mar_left <- max(5, min(16, out$mar_left))
  out
}
