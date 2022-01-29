reportUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12, div(dataTableOutput(ns("records")), style = "font-size: 85%"))),
    fluidRow(br(), column(8, plotOutput(ns("plotres"))), column(4, htmlOutput(ns("textres")))),
    fluidRow(column(8, align = "center", br(), disabled(actionButton(ns("genpdf"), "Generate PDF"))))
  )
}

report <- function(input, output, session) {
  ns        <- session$ns
  psel      <- reactive(!is.null(input$records_rows_selected))
  perimetry <- NULL
  reports   <- NULL
  record    <- NULL
  locs      <- NULL
  ####################
  # OBSERVE
  ####################
  # if selected patient db has changed
  observe(
    if(newReports()) {
      newReports(FALSE)
      # get new reports if any exist
      reports <<- getReports(appParams$resPath, patientTable)
      if(!is.null(reports)) {
        # rearrange and change column names for output
        reportsOut <- reports[,c("id", "name", "surname", "type", "date", "time", "eye", "perimetry", "grid")]
        # find the output name for grid
        reportsOut$grid <- lapply(reportsOut$grid, function(gg) gridNames[which(names(grids) %in% gg)])
        names(reportsOut) <- c("ID", "Name", "Surname", "Type", "Date", "Time", "Eye", "Perimetry", "Grid")
        output$records <- renderDataTable(reportsOut, rownames = FALSE, server = FALSE, selection = "single",
                                          options = list(pageLength = 5, lengthChange = FALSE))
      }
    }
  )
  ####################
  # EVENTS
  ####################
  # selected patient
  observeEvent(psel(), {
    if(psel()) {
      perimetry <<- reports$perimetry[input$records_rows_selected]
      record    <<- getRecord(reports[input$records_rows_selected,], appParams$resPath)
      locs      <<- grids[[which(names(grids) %in% reports$grid[input$records_rows_selected])]][,c("x", "y")]
      enable("genpdf")
    } else {
      perimetry <<- NULL
      record    <<- NULL
      locs      <<- NULL
      disable("genpdf")
    }
    output$plotres <- renderPlot(showPlotReport(record, locs))
    output$textres <- renderUI(renderResultReport(record, nrow(locs)))
  }, ignoreInit = TRUE)
  # selected patient
  observeEvent(input$genpdf, {
    tdate <- format(as.Date(record$date), "%Y%m%d")
    ttime <- gsub(":", "", record$time)
    fname <- paste0(appParams$resPath, record$id, "/", record$id)
    fname <- paste(fname, perimetry, record$eye, tdate, ttime, sep = "_")
    fname <- paste0(fname, ".pdf")
    pdfReport(fname, record, locs, ps = 10)
    txt <- paste("The report has been generated as a PDF file and saved in", fname)
    showModal(modalDialog(title = "Report generated", txt, easyClose = TRUE))
  })
}
####################
# ROUTINES
####################
# get all available reports and sort them by date, then time
getReports <- function(path, patientTable) {
  # first get all reports
  reports <- do.call(rbind, lapply(paste0(path, dir(path)), function(path) {
    # for each file
    dat <- lapply(dir(path, pattern = "*.csv"), function(ff) {
      # get the indices to be able to extract the perimetry type and grid used
      idxs <- c(gregexpr(pattern = "_", ff)[[1]][1:2], gregexpr(pattern = "\\.", ff)[[1]][1])
      # read the data keeping only the key fields and add the perimetry type and grid used
      return(cbind(read.csv(paste(path, ff, sep = "/"), colClasses = "character")[,c("id", "eye", "date", "time")],
                   data.frame(perimetry = substr(ff, idxs[1] + 1, idxs[2] - 1), grid = substr(ff, idxs[2] + 1, idxs[3] - 1))))
    })
    # merge all available reports
    return(do.call(rbind, dat))
  }))
  if(is.null(reports)) return(NULL)
  # merge with patient db table to get name, surname, and type of the patient
  reports <- merge(reports, patientTable[,c("id", "name", "surname", "type")], by = "id")
  # sort by date and time
  reports <- reports[order(reports$time, decreasing = TRUE),]
  reports <- reports[order(reports$date, decreasing = TRUE),]
  return(reports)
}
# get record from results
getRecord <- function(dat, path) {
  fname <- paste0(path, dat$id, "/", paste(dat$id, dat$perimetry, dat$grid, sep = "_"), ".csv")
  res   <- read.csv(fname, stringsAsFactors = FALSE)
  return(res[which(res$date == dat$date[1] & res$time == dat$time[1]),])
}
# template of the plot to show
templatePlotReport <- function(eye) {
  if(eye == "OD") x <- 15
  else            x <- -15
  par(mar = c(0, 0, 0, 0))
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
showPlotReport <- function(record, locs) {
  if(is.null(record)) return(NULL)
  if(record$eye == "OS") locs$x <- -locs$x
  templatePlotReport(record$eye)
  vals <- as.numeric(record[,(ncol(record) - nrow(locs) + 1):ncol(record)])
  # unfinished symbols are presented in gray, finished symbols in black
  cols <- rep("black", length(vals))
  cols[vals >= 5] <- "darkgreen"
  cols[vals < 5]  <- "red"
  fovcol <- ifelse(record$fovea < 5, "red", "darkgreen")
  text(0, 0, record$fovea, col = fovcol, font = 2)
  text(locs$x, locs$y, vals, col = cols, font = 2)
}
# prepare test results to show next to the plot
renderResultReport <- function(record, nlocations) {
  id <- eye <- date	<- time <- age <- type <- comments <- nlocs <- npres <- fpr <- fnr <- rtsd <- rtm <- rt150 <- rt600 <- ""
  duration <- pause <- "00:00"
  if(!is.null(record)) {
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
  }
  # patient info
  txt <- paste("<strong>Patient ID:</strong>", id, "<br/><br/>")
  txt <- paste(txt, "<strong>Patient Age:</strong>", age, "<br/>")
  txt <- paste(txt, "<strong>Patient Type:</strong>", type, "<br/><br/>")
  txt <- paste(txt, "<strong>Test Eye:</strong>", eye, "<br/>")
  txt <- paste(txt, "<strong>Test Date:</strong>", date, "<br/>")
  txt <- paste(txt, "<strong>Test Start Time:</strong>", time, "<br/><br/>")
  # Response Times
  txt <- paste(txt, "<strong>Responses < 150 ms:</strong>", rt150, "<br/>")
  txt <- paste(txt, "<strong>Responses > 600 ms:</strong>", rt600, "<br/>")
  txt <- paste(txt, "<strong>Mean Response Time:</strong>", rtm, "ms<br/>")
  txt <- paste(txt, "<strong>SD of Response Time:</strong>", rtsd, "ms<br/><br/>")
  # False positives and negatives
  txt <- paste(txt, "<strong>False Positives:</strong>", fpr, "<br/>")
  txt <- paste(txt, "<strong>False Negatives:</strong>", fnr, "<br/><br/>")
  # Progress
  txt <- paste(txt, "<strong>Locations:</strong>", nlocs, "<br/>")
  txt <- paste(txt, "<strong>Presentations:</strong>", npres, "<br/><br/>")
  # test time and pause time
  txt <- paste(txt, "<strong>Test Time (mm:ss):</strong>", duration, "<br/>")
  txt <- paste(txt, "<strong>Pause Time (mm:ss):</strong>", pause, "<br/>")
  return(HTML(txt))
}