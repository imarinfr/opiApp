pdfReport <- function(file, records, locs, ps = 10) {
  require(plotrix)
  # width and height in inches
  width  <- 6
  height <- 4
  par(mar = c(0, 0, 0, 0))
  pdf(file, width = width, height = height)
  for(i in 1:nrow(records)) {
    scrlist <- mountlayout()
    # info
    screen(scrlist$info)
    fillResults(records[i,], nrow(locs), ps = ps)
    # plot
    screen(scrlist$plot)
    showPlotPDF(records[i,], locs, ps = ps)
    close.screen(all.screens = TRUE)
  }
  invisible(dev.off())
}
########################################################
# internal helper functions for both interactive and pdf
########################################################
mountlayout <- function() {
  # all the boxes are defined in mm divided by the width and height
  # of the page, also in mm
  boxInfo <- c(0.0125, 0.275, 0.025, 0.975)
  boxPlot <- c(0.325, 0.9875, 0.025, 0.975)
  scr     <- split.screen(rbind(boxInfo, boxPlot))
  return(list(info = scr[1], plot = scr[2]))
}
# prepare test results to show next to the plot
fillResults <- function(record, nlocations, ...) {
  par(...)
  id       <- record$id
  eye      <- record$eye
  date     <- format(as.Date(record$date), "%m/%d/%Y")
  time     <- record$time
  age      <- record$age
  type     <- record$type
  rtsd     <- record$rtsd
  rtm      <- record$rtm
  rt150    <- record$rt150
  rt600    <- record$rt600
  nlocs    <- nlocations
  npres    <- record$npres
  duration <- record$duration
  pause    <- record$pause
  fpr      <- paste(round(100 * record$fpr), "%")
  fnr      <- paste(round(100 * record$fnr), "%")

  txt <- paste0("Patient ID: ",              id,    "\n")
  txt <- paste0(txt, "Patient Age: ",        age,   "\n")
  txt <- paste0(txt, "Patient Type: ",       type,  "\n\n")
  txt <- paste0(txt, "Test Eye: ",           eye,   "\n")
  txt <- paste0(txt, "Test Date: ",          date,  "\n")
  txt <- paste0(txt, "Test Start Time: ",    time,  "\n\n")
  txt <- paste0(txt, "Responses < 150 ms: ", rt150, "\n")
  txt <- paste0(txt, "Responses > 600 ms: ", rt600, "\n")
  txt <- paste0(txt, "Mean Response Time: ",  rtm,   " ms\n")
  txt <- paste0(txt, "SD of Response Time: ", rtsd,  " ms\n\n")
  txt <- paste0(txt, "False Positives: ",    fpr,   "\n")
  txt <- paste0(txt, "False Negatives: ",    fnr,   "\n\n")
  txt <- paste0(txt, "Locations: ",          nlocs, "\n")
  txt <- paste0(txt, "Presentations: ",      npres, "\n\n")
  txt <- paste0(txt, "Test Time (mm:ss): ",          duration, "\n")
  txt <- paste0(txt, "Pause Time (mm:ss): ",         pause)
  text(0, 0.5, txt, adj = c(0, 0.5), cex = 0.8)
}
# template of the plot to show
templatePlotPDF <- function(eye, ...) {
  if(eye == "OD") x <- 15
  else            x <- -15
  par(mar = c(0, 0, 0, 0), ...)
  lty <- 1
  lwd <- 1
  linColor     <- "lightgray"
  ellipseColor <- "gray92"
  pchColor     <- "black"
  plot(0 ,0, typ = "n", xlim = c(-31, 31), ylim = c(-31, 31), asp = 1,
       axes = FALSE, ann = FALSE, bty = "n")
  draw.ellipse(x, -1.5, 2.75, 3.75, col = ellipseColor, border = ellipseColor)
  lines(c(-30, 30), c(0, 0), col = linColor, lty = lty, lwd = lwd)
  lines(c(0, 0), c(-30, 30), col = linColor, lty = lty, lwd = lwd)
  text( 31,  0, "+30", adj = c(0, 0.5))
  text(-31,  0, "-30", adj = c(1, 0.5))
  text( 0,  31, "+30", adj = c(0.5, 0))
  text( 0, -31, "-30", adj = c(0.5, 1))
  draw.circle(0, 0, 10, border = linColor, lty = lty, lwd = lwd)
  draw.circle(0, 0, 20, border = linColor, lty = lty, lwd = lwd)
  draw.circle(0, 0, 30, border = linColor, lty = lty, lwd = lwd)
}
# show plot with updated data
showPlotPDF <- function(record, locs, ...) {
  if(is.null(record)) return(NULL)
  if(record$eye == "OS") locs$x <- -locs$x
  templatePlotPDF(record$eye, ...)
  vals <- as.numeric(record[,(ncol(record) - nrow(locs) + 1):ncol(record)])
  # unfinished symbols are presented in gray, finished symbols in black
  cols <- rep("black", length(vals))
  cols[vals >= 5] <- "darkgreen"
  cols[vals < 5]  <- "red"
  fovcol <- ifelse(record$fovea < 5, "red", "darkgreen")
  text(0, 0, record$fovea, col = fovcol, font = 2)
  text(locs$x, locs$y, vals, col = cols, font = 2)
}