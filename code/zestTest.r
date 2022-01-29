zestTestUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, htmlOutput(ns("selected"))),
      column(8,
        fluidRow(
          if(appParams$runType == "luminance")
            column(6, selectInput(ns("size"), "Stimulus Size", choices = c("Size I", "Size II", "Size III", "Size IV", "Size V", "Size VI"), selected = "Size V"))
          else if(appParams$runType == "size")
            column(6, numericInput(ns("lum"), "Stimulus Luminance (cd/m2)", value = 15))
        ),
        fluidRow(
          column(6, selectInput(ns("eye"), "Eye", choices = c("OD", "OS"), selected = "OD")),
          column(6, selectInput(ns("grid"), "Grid", choices = gridNames, selected = gridNames[1]))
        )
      )
    ),
    fluidRow(column(12, htmlOutput(ns("msgconn")))),
    fluidRow(
      column(3, actionButton(ns("init"),  label = "Initialize OPI", width = "100%")),
      column(2, actionButton(ns("fovea"), label = "Test fovea", width = "100%", disabled = TRUE)),
      column(3, actionButton(ns("close"), label = "Close OPI", width = "100%",  disabled = TRUE)),
      column(4, align = "center",
        actionButton(ns("run"),   label = "Run", disabled = TRUE),
        actionButton(ns("pause"), label = "Pause", disabled = TRUE),
        actionButton(ns("stop"),  label = "Stop", disabled = TRUE)
      )
    ),
    p(),
    fluidRow(br(),
      column(8, plotOutput(ns("plotres"))),
      column(4, htmlOutput(ns("textres")))
    ),
    fluidRow(br(),
      column(8, textInput(ns("comments"), label = "Comments", width = "100%"))
    ),
    fluidRow(br(),
      column(4, disabled(actionButton(ns("save"), label = "Save", width = "100%"))),
      column(4, disabled(actionButton(ns("cancel"), label = "Cancel", width = "100%")))
    )
  )
}

zestTest <- function(input, output, session) {
  ns         <- session$ns
  opiParams  <- NULL  # parameters to pass to OPI implementation
  size       <- 1.72  # stimulus size for Luminance perimetry
  lum        <- 400   # stimulus luminance for Size perimetry
  begin      <- TRUE  # whether the test is to start from beginning or after pause
  opts       <- NULL  # running options
  locs       <- NULL  # test locations
  foveadb    <- NULL  # sensitivity value obtained at the fovea
  listen     <- reactiveTimer(10) # every 10 ms
  msg        <- reactiveVal("Press 'Initialize OPI' to start") # message where to display status of connection, etc
  refreshout <- reactiveVal(TRUE) # refresh output graphs?
  # define routine to initialized all run control variables
  res <- tp0 <- tt <- tp <- tt0 <- NULL
  initRunVariables <- function() {
    res  <<- NULL        # keep log of run
    tp0 <<- tt <<- tp <<- 0 # to control test time and pause
    tt0 <<- Sys.time()      # start test time is current system time
  }
  # then init all those variables
  initRunVariables()
  # outputs plot and results
  output$msgconn <- renderText(msg())
  ####################
  # OBSERVE
  ####################
  # if selected patient changed
  observe(
    if(patientChanged()) {
      patientChanged(FALSE)
      output$selected <- renderUI(parsePatientOutput(patient))
    }
  )
  # if output data changed
  observe(
    if(refreshout()) {
      output$plotres <- renderPlot(showPlot(locs, input$eye, foveadb))
      output$textres <- renderUI(renderResult(locs, res))
      refreshout(FALSE)
    }
  )
  ####################
  # EVENTS
  ####################
  # if grid or eye changes
  observeEvent(input$grid, {
    locs <<- testLocations(grids, gridNames)
    initRunVariables()
    refreshout(TRUE)
  })
  observeEvent(input$eye, {
    locs <<- testLocations(grids, gridNames)
    foveadb <<- NA
    initRunVariables()
  })
  # get stimulus size in degrees of visual angle
  observeEvent(input$size, {
    stimArea  <- c(0.25, 1, 4, 16, 64, 256)
    stimAngle <- round(180 / pi * atan(2 * sqrt(stimArea / pi) / 300), 2)
    size <<- switch(input$size,
                    "Size I"   = stimAngle[1],
                    "Size II"  = stimAngle[2],
                    "Size III" = stimAngle[3],
                    "Size IV"  = stimAngle[4],
                    "Size V"   = stimAngle[5],
                    "Size VI"  = stimAngle[6])
  })
  # change stored luminance
  observeEvent(input$lum, lum <<- input$lum)
  # initialize OPI
  observeEvent(input$init, {
    if(is.na(patient$id))
      errorMessage("Select a patient before proceeding")
    else {
      ShinySender$reset()
      ShinyReceiver$reset()
      disableElements(c("init"))
      opiParams <<- fillOpiParams()
      showModal(modalDialog(title = "OPI connection", "Initializing OPI", footer = NULL))
      print("opening zestServer...")
      print("zestServer open")
      # initialize server
      if(appParams$machine == "PhoneVR")
        source("serverPhoneVR.r", local = TRUE)
      else if(appParams$machine == "OpenGL")
        stop("not ready yet")
      else if(appParams$machine == "Octopus 900")
        stop("not ready yet")
      else
        source("serverSims.r", local = TRUE)
      then(zestServer,
           onFulfilled = function() print("zestServer closed"),
           onRejected = function(e) print(e$message))
      
      # initialize OPI server and change background eye
      ShinySender$push(title = c("CMD", "eye"), message = c("opiInit", "B"))
      while(ShinyReceiver$empty()) Sys.sleep(0.1)
      msgtxt <- ShinyReceiver$pop()
      if(msgtxt$title == "OK") {
        msg(msgtxt$message)
        opiInitialized(TRUE)
        enableElements(c("fovea", "close", "run"))
        locs    <<- initLocations(locs)
        foveadb <<- NA
        initRunVariables()
        refreshout(TRUE)
      }  else {
        enableElements(c("init"))
        msg(errortxt(msgtxt$message))
      }
      removeModal()
    }
  }, ignoreInit = TRUE)
  # close OPI connection
  observeEvent(input$close, {
    disableElements(c("close", "fovea", "run"))
    enableElements(c("init"))
    ShinySender$push(title = "CMD", message = "opiClose")
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    msg(ShinyReceiver$pop()$message)
    opiInitialized(FALSE)
    ShinySender$reset()
    ShinyReceiver$reset()
    locs <<- initLocations(locs)
    foveadb <<- NA
    initRunVariables()
    refreshout(TRUE)
  }, ignoreInit = TRUE)
  # test fovea
  observeEvent(input$fovea, {
    showModal(modalDialog(
      title = "Start testing the fovea?",
      "Press Yes when ready or Cancel",
      footer = tagList(actionButton(ns("okfovea"), "Yes"), modalButton("Cancel"))
    ))
  }, ignoreInit = TRUE)
  # if OK to test fovea
  observeEvent(input$okfovea, {
    removeModal()
    showModal(modalDialog(title = "Testing the fovea", "Please wait while the fovea is being tested", footer = NULL))
    # run foveal test
    ShinySender$push(title = c("CMD", "eye"), message = c("zestFovea", ifelse(input$eye == "OD", "R", "L")))
    # wait for return message
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    foveadb <<- round(as.numeric(ShinyReceiver$pop()$message))
    removeModal()
    msg(paste("Foveal threshold is", foveadb, "dB"))
    refreshout(TRUE)
  }, ignoreInit = TRUE)
  # start or continue test
  observeEvent(input$run, {
    if(begin) {
      locs <<- initLocations(locs)
      initRunVariables()
      disableElements(c("eye", "size", "lum", "grid"))
      ShinySender$push(title   = c("CMD", "grid", "eye", "lum", "size"),
                       message = c("zestInit", input$grid, ifelse(input$eye == "OD", "R", "L"), lum, size))
      begin <<- FALSE
    }
    disableElements(c("close", "run", "fovea", "stop", "save"))
    enableElements(c("pause"))
    msg("OPI ZEST test running")
    ShinySender$push(title = "CMD", message = "zestRun")
  }, ignoreInit = TRUE)
  # pause test halfway through
  observeEvent(input$pause, {
    if(!begin) { # if test has not started or it has finished, do not do this
      ShinySender$push(title = "CMD", message = "opiWait")
      tp0 <<- Sys.time()
      disableElements("pause")
      enableElements(c("run", "stop"))
    }
    msg("ZEST test paused. Pres 'continue' or 'stop' to terminate it")
  }, ignoreInit = TRUE)
  # stop test halfway through
  observeEvent(input$stop, { # stop test
    showModal(modalDialog(
      title = "Are you sure you want to terminate the test?",
      "Press TERMINATE to cancel",
      footer = tagList(actionButton(ns("terminate"), "TERMINATE"), modalButton("Continue"))
    ))
  }, ignoreInit = TRUE)
  # OK to terminate halfway through?
  observeEvent(input$terminate, { # terminate test
    removeModal()
    msg(errortxt("OPI ZEST test TERMINATED"))
    enableElements(c("close", "run", "fovea", "eye", "size", "lum", "grid"))
    disableElements(c("pause", "stop", "save"))
    begin   <<- TRUE
    locs    <<- initLocations(locs)
    foveadb <<- NA
    initRunVariables()
    refreshout(TRUE)
  }, ignoreInit = TRUE)
  # save test once finished
  observeEvent(input$save, { # save and allow to run a fresh test
    saveResults(patient, appParams$resPath, appParams$runType, grids, gridNames)
    updateTextInput(session, "comments", value = "")
    disableElements(c("save", "cancel"))
    enableElements(c("eye", "size", "lum", "grid", "fovea", "close", "run"))
    msg("Results have been saved")
    begin   <<- TRUE
    locs    <<- initLocations(locs)
    foveadb <<- NA
    initRunVariables()
    refreshout(TRUE)
    newReports(TRUE)
  }, ignoreInit = TRUE)
  # cancel save once the test has finished
  observeEvent(input$cancel, { # do not save and allow to run a fresh test
    updateTextInput(session, "comments", value = "")
    disableElements(c("save", "cancel"))
    enableElements(c("eye", "size", "lum", "grid", "fovea", "close", "run"))
    msg(errortxt("Results have not been saved"))
    begin   <<- TRUE
    locs    <<- initLocations(locs)
    foveadb <<- NA
    initRunVariables()
    refreshout(TRUE)
  }, ignoreInit = TRUE)
  ####################################################################
  # main loop: listening to what remote perimetry test code has to say
  ####################################################################
  observe({
    listen()
    if(!ShinyReceiver$empty()) {
      res <<- rbind(res, updateRes(readResults()))
      # update results
      refreshout(TRUE)
      # if all points are done, stop perimetry test
      if(all(locs$done)) {
        msg(paste("ZEST test finished"))
        begin <<- TRUE
        enableElements(c("fovea", "save", "cancel"))
        disableElements(c("run", "pause", "stop", "close"))
        showModal(modalDialog(
          title = "ZEST test finished",
          "The test has finished. You can 'Save' it or 'Cancel' all without saving",
          easyClose = TRUE))
      }
    }
  })
  ####################
  # SUBROUTINES
  ####################
  # get test locations depending on the grid we are interested on
  testLocations <- function(grids, gridNames) {
    if(is.null(grids)) return(NULL)
    locs <- grids[[which(gridNames %in% input$grid)]]
    if(input$eye == "OS") locs$x <- -locs$x
    return(initLocations(locs))
  }
  readResults <- function() {
    if(ShinyReceiver$count() %% 9) stop("error: incorrect number of fields received")
    dat <- NULL
    for(i in 1:(ShinyReceiver$count() / 9)) {
      vals <- ShinyReceiver$pop(9)$message
      type   <- vals[1]
      x      <- as.numeric(vals[2])
      y      <- as.numeric(vals[3])
      level  <- as.numeric(vals[4])
      th     <- round(as.numeric(vals[5]))
      seen   <- as.logical(vals[6])
      time   <- as.numeric(vals[7])
      resWin <- as.numeric(vals[8])
      done   <- as.logical(vals[9])
      if(type == "F") next # if fail (can happen with FP catch trials), then move along
      th <- ifelse(th > 0, th, 0)  # floor to zero
      # if max stimulus was shown, it was not seen and we are done than mark
      # the point as -2, meaning that even the max stimulus was not seen
      if(done & th == 0 & !seen) th <- -2
      # keep run log updated
      dat <- rbind(dat, data.frame(type = type, x = x, y = y, level = level, th = th,
                                   seen = seen, time = time, resWin = resWin,done = done))
      # update locations
      if(type == "Z") {
        idx <- which(locs$x == x & locs$y == y)
        locs$th[idx]   <<- th
        locs$done[idx] <<- done
      }
    }
    # read results returned by ZEST test
    return(dat)
  }
  # update log
  updateRes <- function(dat) {
    # update test and pause times
    if(tp0 != 0) {
      tp <<- tp + as.numeric(difftime(Sys.time(), tp0, units = "secs"))
      tp0 <<- 0
      tt0 <<- Sys.time()
    }
    tt <<- tt + as.numeric(difftime(Sys.time(), tt0, units = "secs"))
    tt0 <<- Sys.time()
    return(data.frame(x = dat$x, y = dat$y, level = dat$level, th = dat$th, type = dat$type,
                      seen = dat$seen, time = dat$time, done = dat$done, tt = tt, tp = tp))
  }
  # save results at the end of the test
  saveResults <- function(patient, path, runType, grids, gridNames) {
    systime  <- Sys.time()
    tdate    <- format(systime, "%Y-%m-%d")
    ttime    <- format(systime, "%H:%M:%S")
    # if patient folder does not exist, then create
    patientDir <- paste0(path, patient$id, "/")
    dir.create(patientDir, showWarnings = FALSE) # create directories if they do not exist
    dir.create(paste0(patientDir, "raw/"), showWarnings = FALSE)
    # save raw log first
    fnamelog <- paste0(patientDir, "raw/", trimws(as.character(patient$id)), "_", runType, "_", input$eye, format(systime, "_%Y%m%d_%H%M%S"), ".csv")
    write.csv(res, file = fnamelog, row.names = FALSE)
    # then save the processed test
    fname <- paste0(patientDir, trimws(as.character(patient$id)), "_", runType, "_", names(grids)[gridNames %in% input$grid], ".csv")
    dat <- prepareToSave(patient$id, input$eye, tdate, ttime, patient$age, patient$type, input$comments, locs, res, foveadb)
    # if file exist, append result
    if(file.exists(fname)) dat <- rbind(read.csv(file = fname, colClasses = "character"), dat)
    # save test for the patient
    write.csv(dat, file = fname, row.names = FALSE)
  }
}
####################
# ROUTINES
####################
# error message
errorMessage <- function(txt) {
  showModal(modalDialog(
    title = HTML("<span style = 'color:red'>Error Message</span>"),
    HTML(paste0("<span style = 'color:red'>", txt, "</span>")),
    easyClose = TRUE))
}
# error texts
errortxt <- function(txt) return(paste("<span style=\"color:#FF0000\">", txt, "</span>"))
# enable and disable elements
enableElements <- function(ids) lapply(ids, enable)
disableElements <- function(ids) lapply(ids, disable)
# fill out parameters to initialize the OPI, but also as side effect, it sets the OPI implementation
fillOpiParams <- function() {
  # choose correct OPI implementation
  chooseOpi(appParams$machine)
  opiParams <- opiGetParams("opiInitialize")
  if(appParams$machine == "PhoneVR") {
    # and use the values in appParams settings for initialization parameters
    opiParams$ip   <- appParams$phoneIP
    opiParams$port <- appParams$phonePort
    opiParams$lut  <- read.csv(paste0("../config/", appParams$gammaFile), header = FALSE)$V1
  } else if(appParams$machine == "Display") {
    opiParams$width    <- appParams$width
    opiParams$height   <- appParams$height
    opiParams$ppi      <- appParams$ppi
    opiParams$viewdist <- appParams$viewdist
  }
  return(opiParams)
}
# patient's information to show: id, name, surname, age, Gender
parsePatientOutput <- function(patient) {
  if(is.na(patient$id)) {
    txt <- errortxt("Please select a patient to continue")
  } else {
    txt <- paste0("<strong>Patient ID:</strong> ", patient$id, "</br>")
    txt <- paste0(txt, " <strong>Name:</strong> ",  patient$name, "</br>")
    txt <- paste0(txt, " <strong>Surname:</strong> ", patient$surname, "</br>")
    txt <- paste0(txt, " <strong>Age:</strong> ", patient$age, "</br>")
    txt <- paste0(txt, " <strong>Gender:</strong> ", patient$gender, "</br>")
    txt <- paste0(txt, " <strong>Type:</strong> ", patient$type, "</br>")
  }
  return(HTML(txt))
}
# initialize locations
initLocations <- function(locs) {
  locs$th <- NA
  locs$done  <- FALSE
  return(locs)
}
# template of the plot to show
templatePlot <- function(eye) {
  if(eye == "OD") x <- 15
  else            x <- -15
  par(mar = c(0, 0, 0, 0))
  lty <- 1
  lwd <- 1
  linColor     <- "lightgray"
  ellipseColor <- "gray92"
  pchColor     <- "black"
  plot(0, 0, typ = "n", xlim = c(-31, 31), ylim = c(-31, 31), asp = 1,
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
showPlot <- function(locs, eye, foveadb) {
  templatePlot(eye)
  # unfinished symbols are presented in gray, finished symbols in black
  cols <- rep("black", length(locs$done))
  cols[locs$done & locs$th >= 5] <- "darkgreen"
  cols[locs$done & locs$th < 5]  <- "red"
  fovcol <- ifelse(foveadb < 5, "red", "darkgreen")
  text(0, 0, foveadb, col = fovcol, font = 2)
  isna <- is.na(locs$th)
  if(any(isna))  text(locs$x[isna], locs$y[isna], ".", cex = 2, col = cols[isna])
  if(any(!isna)) text(locs$x[!isna], locs$y[!isna], locs$th[!isna], col = cols[!isna], font = 2)
}
# prepare test results to show next to the plot
renderResult <- function(locs, res) {
  rtsd <- rtm <- seentxt <- level <- x <- y <- time <- ""
  fp <- fpt <- fpp <- fn <- fnt <- fnp <- 0
  tttxt <- tptxt <- "00:00"
  if(!is.null(res)) {
    level <- tail(res, 1)$level
    x     <- paste(tail(res, 1)$x, "degrees")
    y     <- paste(tail(res, 1)$y, "degrees")
    time  <- tail(res, 1)$time
    if(tail(res$seen, 1)) seentxt <- "Stimulus <strong>seen</strong>"
    else         seentxt <- "Stimulus <strong>not seen</strong>"
    seentxt <- paste(seentxt, "after", round(time), "ms")
  }
  npoints   <- length(locs$done) # total number of locations
  nfinished <- sum(locs$done)    # locations finished
  if(!is.null(res)) {
    # compute false positives and negatives
    fp  <- sum(res$type == "P" & res$seen)
    fpt <- sum(res$type == "P")
    fpp <- ifelse(fpt == 0, 0, round(100 * fp / fpt))
    fn  <- sum(res$type == "N" & !res$seen)
    fnt <- sum(res$type == "N")
    fnp <- ifelse(fnt == 0, 0, round(100 * fn / fnt))
    # compute response time SD and mean
    rt <- res$time[which(res$type == "Z" & res$seen == TRUE)]
    if (length(rt) > 1) { # can only calculate SD if there are more than 2 response times available
      rtm  <- round(mean(rt))
      rtsd <- round(sd(rt))
    } else rtsd <- rtm <- ""
    # calculate test time and pause time
    secs <- res$tt[length(res$tt)]
    mm <- as.character(secs %/% 60)
    ss <- as.character(floor((secs / 60 - secs %/% 60) * 60))
    if(nchar(mm) == 1) mm <- paste0("0", mm)
    if(nchar(ss) == 1) ss <- paste0("0", ss)
    tttxt <- paste(mm, ss, sep = ":")
    secs <- res$tp[length(res$tp)]
    mm <- as.character(secs %/% 60)
    ss <- as.character(floor((secs / 60 - secs %/% 60) * 60))
    if(nchar(mm) == 1) mm <- paste0("0", mm)
    if(nchar(ss) == 1) ss <- paste0("0", ss)
    tptxt <- paste(mm, ss, sep = ":")
  }
  if(level != "") level <- paste(level, "dB")
  if(rtm != "") rtm <- paste(rtm, "ms")
  if(rtsd != "") rtsd <- paste(rtsd, "ms")
  # get state text
  txt <- paste("<strong>Stimulus x:</strong>", x, "<br/>")
  txt <- paste(txt, "<strong>Stimulus y:</strong>", y, "<br/><br/>")
  txt <- paste(txt, "<strong>Level:</strong>", level, "<br/>")
  txt <- paste0(txt, seentxt)
  txt <- paste0(txt, "<br/><br/>")
  # False positives and negatives
  txt <- paste(txt, "<strong>False Positives:</strong>", fp, "of", fpt)
  txt <- paste0(txt, " (", fpp, "%)<br/>")
  txt <- paste(txt, "<strong>False Negatives:</strong>", fn, "of", fnt)
  txt <- paste0(txt, " (", fnp, "%)<br/><br/>")
  # Response Times
  txt <- paste(txt, "<strong>Responses < 150 ms:</strong>", sum(res$type == "Z" & res$time < 150), "<br/>")
  txt <- paste(txt, "<strong>Responses > 600 ms:</strong>", sum(res$type == "Z" & res$time > 600 & res$seen == TRUE), "<br/>")
  txt <- paste(txt, "<strong>Mean Response Time:</strong>", rtm, "<br/>")
  txt <- paste(txt, "<strong>SD of Response Time:</strong>", rtsd, "<br/><br/>")
  # Progress
  txt <- paste(txt, "<strong>Finished:</strong>", nfinished, "of", npoints)
  txt <- paste0(txt, " (", round(100 * nfinished / npoints), "%)<br/>")
  txt <- paste(txt, "<strong>Presentations:</strong>", sum(res$type == "Z"), "<br/><br/>")
  # test time and pause time
  txt <- paste(txt, "<strong>Test Time (mm:ss):</strong>",  tttxt, "<br/>")
  txt <- paste(txt, "<strong>Pause Time (mm:ss):</strong>", tptxt, "<br/>")
  txt <- paste0(txt, "<br/>")
  return(HTML(txt))
}

# prepare results to save
prepareToSave <- function(id, eye, tdate, ttime, age, type, comments, locs, res, foveadb) {
  dat <- data.frame(id = id, eye = eye, date = tdate, time = ttime, age = age, type = type, fpr = NA, fnr = NA,
                    rt150 = sum(res$type == "Z" & res$time < 150),
                    rt600 = sum(res$type == "Z" & res$time > 600 & res$seen == TRUE),
                    rtsd = NA, rtm = NA, duration = NA, pause = NA, npres = sum(res$type == "Z"),
                    comments = comments, fovea = ifelse(is.na(foveadb), "", foveadb))
  
  # test and pause time
  secs <- res$tt[length(res$tt)]
  mm <- as.character(secs %/% 60)
  ss <- as.character(floor((secs / 60 - secs %/% 60) * 60))
  if(nchar(mm) == 1) mm <- paste0("0", mm)
  if(nchar(ss) == 1) ss <- paste0("0", ss)
  dat$duration <- paste(mm, ss, sep = ":")
  secs <- res$tp[length(res$tp)]
  mm <- as.character(secs %/% 60)
  ss <- as.character(floor((secs / 60 - secs %/% 60) * 60))
  if(nchar(mm) == 1) mm <- paste0("0", mm)
  if(nchar(ss) == 1) ss <- paste0("0", ss)
  dat$pause <- paste(mm, ss, sep = ":")
  # false positive and false negatives
  fp  <- sum(res$type == "P" & res$seen)
  fpt <- sum(res$type == "P")
  dat$fpr <- ifelse(fpt == 0, 0, fp / fpt)
  fn  <- sum(res$type == "N" & !res$seen)
  fnt <- sum(res$type == "N")
  dat$fnr <- ifelse(fnt == 0, 0, fn / fnt)
  # compute response time SD and mean
  rt <- res$time[which(res$type == "Z" & res$seen == TRUE)]
  dat$rtsd <- round(sd(rt))
  dat$rtm <- round(mean(rt))
  # get results for each location
  dat[,paste0("l", 1:nrow(locs))] <- res$th[res$type == "Z" & res$done == TRUE]
  return(dat)
}