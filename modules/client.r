clientUI <- function(id) {
  ns <- NS(id)
  opiImpl <- c("PhoneHMD", "Compass", "imo", "Octopus900", "SimHenson", "SimYes", "SimNo")
  gridNames <- names(grids)
  names(gridNames) <- unname(sapply(grids, function(gg) return(gg$name)))
  algorithms <- list("ZEST" = "ZEST", "Full Threshold" = "FT",
                     "Staircase 4-2" = "staircase", "MOCS" = "MOCS")
  tagList(
    fluidRow(
      column(4,
        column(12, fluidRow(htmlOutput(ns("patient")))),
        column(12, fluidRow(htmlOutput(ns("technical"), style = "font-size:75%")))
      ),
      column(8,
        fluidRow(
          column(5, selectInput(ns("machine"), "OPI implementation",
                                choices = opiImpl, selected = appParams$machine)),
          column(5, radioButtons(ns("perimetry"), "Perimetry", inline = TRUE,
                                 choices = c("luminance", "size"), selected = appParams$perimetry))
        ),
        fluidRow(
          column(3, selectInput(ns("eye"), "Eye", choices = list(Right = "R", Left = "L", Both = "B"), selected = "Both")),
          column(3, numericInput(ns("lum"), "lum (cd/m2)", value = appParams$lumForSize)),
          column(3, numericInput(ns("size"), "size (\u00B0)", value = appParams$sizeForLum)),
          column(3, numericInput(ns("dbstep"), "Step (dB)", appParams$dbstep))
        )
      ),
    ),
    fluidRow(
      column(3, selectInput(ns("grid"), "Grid", choices = gridNames, selected = gridNames[1])),
      column(3, selectInput(ns("algorithm"), "Algorithm", choices = algorithms, selected = "ZEST")),
      column(2, numericInput(ns("estSD"), "ZEST SD", value = appParams$estSD)),
      column(2, numericInput(ns("nreps"), "MOCS reps", value = appParams$nreps)),
      column(2, numericInput(ns("range"), "range (dB)", value = appParams$range))
    ),
    fluidRow(column(12, htmlOutput(ns("msgconn")))),
    fluidRow(
      column(2,
        if(is.na(patient()$id)) {
          actionButton(ns("init"),  label = "Start OPI", width = "100%", disabled = TRUE)
        } else {
          actionButton(ns("init"),  label = "Start OPI", width = "100%")
        }
      ),
      column(4, align = "center",
        actionButton(ns("fovea"), label = "Test fovea", width = "45%", disabled = TRUE),
        actionButton(ns("run"), label = "Start test", width = "45%", disabled = TRUE)
      ),
      column(2, actionButton(ns("close"), label = "Close OPI", width = "100%",  disabled = TRUE)),
      column(4, align = "center",
        actionButton(ns("pause"), label = "Pause", width = "45%",  disabled = TRUE),
        actionButton(ns("stop"), label = "Stop", width = "45%",  disabled = TRUE)
      )
    ),
    fluidRow(br(),
      column(8, plotOutput(ns("plotres"))),
      column(4,
        htmlOutput(ns("textres")),
        textInput(ns("comments"), label = "Comments", width = "100%"))
    ),
    fluidRow(
      column(4, disabled(actionButton(ns("save"), label = "Save", width = "100%"))),
      column(4, disabled(actionButton(ns("cancel"), label = "Cancel", width = "100%")))
    )
  )
}

client <- function(input, output, session) {
  ns <- session$ns
  # parameters for OPI
  opiParams <- NULL
  # results
  msg <- reactiveVal()
  resTrial <- reactiveVal(NULL)
  running <- FALSE
  state <- NA
  runType <- NULL
  trialType <- NULL
  lastTrialType <- NULL
  foveadb <- NA
  res <- NULL
  locs <- NULL
  # define routine to initialized all run control variables
  tp0 <- tt <- tp <- tt0 <- NULL
  bglum <- NULL
  maxlum <- NULL
  #########
  # Outputs
  #########
  output$patient <- renderUI(parsePatientOutput(patient()))
  output$msgconn <- renderText(msg())
  output$plotres <- renderPlot(resPlot(resTrial(), locs, input$eye, foveadb, maxlum))
  output$textres <- renderUI(renderResult(resTrial(), res, nrow(locs)))
  ##############
  # main control
  ##############
  # schedule running a trial step or receiving its results
  observe({
    invalidateLater(100)
    req(running)
    if(state == "run") { # run trial
      trialType <<- checkTrialType()
      success <- runStep(trialType)
      lastTrialType <<- trialType
      state <<- "read"
    } else if(state == "read") { # read results
      getTrialResults(trialType)
      state <<- "run"
    }
  })
  # pause test halfway through
  observe({
    running <<- !running
    if(running) {
      if(runType != "F") tp <<- tp + as.numeric(difftime(Sys.time(), tp0, units = "secs"))
      updateActionButton(session, "pause", label = "Pause")
      msg("Continuing test ...")
    } else {
      if(runType != "F") tp0 <<- Sys.time()
      updateActionButton(session, "pause", label = "Continue")
      msg("Test paused. Press 'Continue' to restart the test")
    }
  }) %>% bindEvent(input$pause, ignoreInit = TRUE)
  # stop test halfway through
  observe({
    running <<- FALSE
    showModal(modalDialog(
      title = "Are you sure you want to terminate the test?",
      "Press TERMINATE to cancel",
      footer = tagList(actionButton(ns("stopOK"), "Stop test"), actionButton(ns("stopContinue"), "Continue test"))
    ))
  }) %>% bindEvent(input$stop, ignoreInit = TRUE)
  # confirm terminate test?
  observe({ # terminate test
    if(state == "read") { # read results
      getTrialResults(trialType)
      state <<- "run"
    }
    msg(errortxt("Test run stopped"))
    enableRunElements()
    disableElements(c("pause", "stop", "save"))
    removeModal()
  }) %>% bindEvent(input$stopOK, ignoreInit = TRUE)
  # or continue?
  observe({ # continue test
    running <<- TRUE
    msg("Continuing test ...")
    removeModal()
  }) %>% bindEvent(input$stopContinue, ignoreInit = TRUE)
  ########
  # Events
  ########
  # if machine changes
  observe({
    if(input$machine == "PhoneHMD" | substr(input$machine, 1, 3) == "Sim") { # only PhoneHMD and simulations can test both eyes and size perimetry
      enable("perimetry")
      updateSelectInput(session, "eye", choices = list(Right = "R", Left = "L", Both = "B"),
                        selected = input$eye)
      output$technical <- renderUI(parseTechnicalOutput(appParams))
    } else {
      updateRadioButtons(session, "perimetry", selected = "luminance")
      disable("perimetry")
      updateSelectInput(session, "eye", choices = list(Right = "R", Left = "L"),
                        selected = ifelse(input$eye == "B", "R", input$eye))
      output$technical <- renderUI("")
    }
    if(input$machine == "Octopus900") {
      bglum <<- appParams$bglum
      maxlum <<- ifelse(appParams$O900max, 10000, 4000) / pi
    } else if(input$machine == "PhoneHMD") {
      bgMaxLumDb <- getBgMaxLumDb(appParams)
      bglum <<- bgMaxLumDb$bglum
      maxlum <<- bgMaxLumDb$maxlum
    } else {
      bglum <<- appParams$bglum
      maxlum <<- 10000 / pi
    }
    msg(paste0("'", input$machine, "' implementation selected. Press 'OPI initialize' to start"))
  }) %>% bindEvent(input$machine)
  # if perimetry type changes
  observe({
    if(input$machine == "Octopus900" | input$machine == "Compass") {
      updateRadioButtons(session, "perimetry", selected = "luminance")
      disable("perimetry")
    } else enable("perimetry")
  }) %>% bindEvent(input$perimetry, ignoreInit = TRUE)
  # if algorithm changes delete all
  observe({
    initRunVariables("A")
  }) %>% bindEvent(input$algorithm)
  # if eye changes delete all
  observe({
    initRunVariables("A")
  }) %>% bindEvent(input$eye)
  # if grid changes delete grid results
  observe({
    initRunVariables("A")
  }) %>% bindEvent(input$grid)
  # initialize OPI
  observe({
    # Initialize OPI server
    showModal(modalDialog(title = "OPI connection", "Initializing OPI", footer = NULL))
    ShinySender$reset()
    ShinyReceiver$reset()
    # initialize server
    source("modules/server.r", local = TRUE)
    then(server,
         onFulfilled = function() print("OPI server closed"),
         onRejected = function(e) print(e$message))
    print("OPI server open")
    if(input$machine == "Octopus900")
      statement <- paste("opiInit", input$machine, input$eye)
    else
      statement <- paste("opiInit", input$machine)
    ShinySender$push(title = "opiStatement", message = statement)
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    msgtxt <- ShinyReceiver$pop()
    if(msgtxt$title == "OK") {
      opiInitialized(TRUE)
      disableElements(c("machine", "init"))
      enableElements(c("fovea", "close", "run"))
      msg(msgtxt$message)
    } else {
      ShinySender$push(title = "CMD", message = "opiClose")
      while(ShinyReceiver$empty()) Sys.sleep(0.1)
      opiInitialized(FALSE)
      enableElements(c("machine", "init"))
      msg(errortxt(msgtxt$message))
    }
    removeModal()
    Sys.sleep(0.25)
  }) %>% bindEvent(input$init, ignoreInit = TRUE)
  # close OPI connection
  observe({
    disableElements(c("close", "fovea", "run"))
    enableElements(c("machine", "init"))
    ShinySender$push(title = "CMD", message = "opiClose")
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    msg(ShinyReceiver$pop()$message)
    opiInitialized(FALSE)
    ShinySender$reset()
    ShinyReceiver$reset()
  }) %>% bindEvent(input$close, ignoreInit = TRUE)
  # test fovea
  observe({
    if(input$machine == "PhoneHMD")
      statement <- "opiSetBackground B B annulus"
    else if(input$machine == "Octopus900")
      statement <- "opiSetBackground CENTER"
    else
      statement <- "opiSetBackground"
    ShinySender$push(title = "opiStatement", message = statement)
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    msgtxt <- ShinyReceiver$pop()
    if(msgtxt$title == "OK") {
      showModal(modalDialog(
        title = paste0("Start testing the fovea with Goldmann size III (", input$size, "\u00B0) luminance perimetry"),
        "Press 'Yes' when ready or 'Cancel'",
        footer = tagList(actionButton(ns("foveaOK"), "Start"),
                         actionButton(ns("foveaCancel"), "Cancel"))
      ))
    } else {
      removeModal()
      msg(errortxt(msgtxt$message))
    }
  }) %>% bindEvent(input$fovea, ignoreInit = TRUE)
  # if OK to test fovea
  observe({
    statement <- paste("opiTestInit", input$eye, "luminance", input$algorithm, "fovea",
                       input$size, input$lum, input$dbstep, input$estSD, input$nreps, input$range)
    ShinySender$push(title = "opiStatement", message = statement)
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    msgtxt <- ShinyReceiver$pop()
    if(msgtxt$title == "OK") {
      disableRunElements()
      enableElements(c("pause", "stop"))
      msg(paste0("Running luminance perimetry with at the fovea for a stimulus of size ", input$size, "\u00B0"))
      runType <<- "F"
      initRunVariables(runType)
      running <<- TRUE
    } else {
      msg(errortxt(msgtxt$message))
      if(input$machine == "PhoneHMD")
        statement <- "opiSetBackground B none"
      else if(input$machine == "Octopus900")
        statement <- "opiSetBackground CROSS"
      else
        statement <- "opiSetBackground"
      ShinySender$push(title = "opiStatement", message = statement)
      while(ShinyReceiver$empty()) Sys.sleep(0.1)
      msgtxt <- ShinyReceiver$pop()
    }
    removeModal()
    Sys.sleep(0.5)
  }) %>% bindEvent(input$foveaOK, ignoreInit = TRUE)
  observe({
    if(input$machine == "PhoneHMD")
      statement <- "opiSetBackground B none"
    else if(input$machine == "Octopus900")
      statement <- "opiSetBackground CROSS"
    else
      statement <- "opiSetBackground"
    ShinySender$push(title = "opiStatement", message = statement)
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    msgtxt <- ShinyReceiver$pop()
    if(msgtxt$title != "OK") {
      msg(errortxt(msgtxt$message))
    }
    msg("Test in fovea cancelled")
    removeModal()
    Sys.sleep(0.5)
  }) %>% bindEvent(input$foveaCancel, ignoreInit = TRUE)
  # start or continue test
  observe({
    if(input$machine == "PhoneHMD")
      statement <- paste("opiSetBackground B B", appParams$fixtype)
    else if(input$machine == "Octopus900")
      statement <- "opiSetBackground CROSS"
    else
      statement <- "opiSetBackground"
    ShinySender$push(title = "opiStatement", message = statement)
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    msgtxt <- ShinyReceiver$pop()
    if(msgtxt$title == "OK") {
      showModal(modalDialog(
        title = paste("Start the", input$algorithm, input$perimetry, "perimetry for", input$eye),
        "Press 'Yes' when ready or 'Cancel'",
        footer = tagList(actionButton(ns("runOK"), "Start"), modalButton("Cancel"))
      ))
    } else {
      msg(errortxt(msgtxt$message))
      removeModal()
    }
  }) %>% bindEvent(input$run, ignoreInit = TRUE)
  # if OK to run test in selected grid
  observe({
    statement <- paste("opiTestInit", input$eye, input$perimetry, input$algorithm, input$grid,
                       input$size, input$lum, input$dbstep, input$estSD, input$nreps, input$range)
    ShinySender$push(title = "opiStatement", message = statement)
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    msgtxt <- ShinyReceiver$pop()
    if(msgtxt$title == "OK") {
      disableRunElements()
      enableElements(c("pause", "stop"))
      msg(paste("Running", input$algorithm, input$perimetry, "perimetry for eye", input$eye))
      runType <<- "N"
      initRunVariables(runType)
      running <<- TRUE
    } else msg(errortxt(msgtxt$message))
    removeModal()
    Sys.sleep(0.5)
  }) %>% bindEvent(input$runOK, ignoreInit = TRUE)
  # save test once finished
  observe({ # save and allow to run a fresh test
    saveResults()
    updateTextInput(session, "comments", value = "")
    disableElements(c("save", "cancel"))
    enableRunElements()
    msg("Results have been saved")
    newReports(TRUE)
  }) %>% bindEvent(input$save, ignoreInit = TRUE)
  # cancel save once the test has finished
  observe({ # do not save and allow to run a fresh test
    updateTextInput(session, "comments", value = "")
    disableElements(c("save", "cancel"))
    enableRunElements()
    msg(errortxt("Results have not been saved"))
  }) %>% bindEvent(input$cancel, ignoreInit = TRUE)
  ##################
  # Control routines
  ##################
  # return the type of trial to
  checkTrialType <- function() {
    if(runType == "F") return("F") # fovea trial step
    if(sum(res$type == "N") == 0) return("N") # first step is a normal trial
    fp <- lastTrialType != "FP" && sum(res$type == "N") %% appParams$fprate == 0
    fn <- lastTrialType != "FN" && sum(res$type == "N") %% appParams$fnrate == 0
    if(!fp && !fn) return("N") # if not, then normal step
    # FP has priority over FN
    if(fp) return("FP")
    if(fn) return("FN")
  }
  # run trial step
  runStep <- function(type) {
    if(type == "F" || type == "N") {
      ShinySender$push(title = "opiStatement", message = "opiTestStepRun")
    }
    if(type == "FP") {
      pars <- falsePositivePars(input$machine, input$perimetry, bglum, maxlum, locs, input$eye)
      statement <- paste("opiTestCatchTrial", pars$loc, input$eye, pars$x, pars$y, pars$db)
      ShinySender$push(title = "opiStatement", message = statement)
    }
    if(type == "FN") {
      pars <- falseNegativePars(locs, res, input$eye)
      if(!is.null(pars)) {
        statement <- paste("opiTestCatchTrial", pars$loc, input$eye, pars$x, pars$y, pars$db)
        ShinySender$push(title = "opiStatement", message = statement)
      } else ShinyReceiver$push("FN", "No suitable locations for FN catch trial. Moving on")
    }
  }
  # get results from the trial and return if done
  getTrialResults <- function(type) {
    done <- FALSE
    resTrial(readResults(type))
    if(!is.null(resTrial())) {
      if(type == "F") done <- resTrial()$done
      if(type == "N") done <- all(locs$done)
      if(done) finish()
    }
  }
  # read results
  readResults <- function(type) {
    resReceived <- NULL
    # update times
    if(type != "F") {
      tt <<- tt + as.numeric(difftime(Sys.time(), tt0, units = "secs"))
      tt0 <<- Sys.time()
    }
    # check if all good
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    msgtxt <- ShinyReceiver$pop()
    if(msgtxt$title == "OK") {
      while(ShinyReceiver$empty()) Sys.sleep(0.1)
      received <- strsplit(ShinyReceiver$pop()$message, split = " ")[[1]]
      if(length(received) == 12)
        resReceived <- parseResults(received, type)
      else msg(errortxt("Wrong number of parameters received"))
      if(!is.null(resReceived)) {
        res <<- rbind(res, resReceived)
        if(type == "F") # update results
          foveadb <<- as.numeric(resReceived$th)
        else if(type == "N")
          updateLocations(resReceived)
      }
    } else if(msgtxt$title != "FN") msg(errortxt(msgtxt$message))
    return(resReceived)
  }
  # check if finished
  finish <- function() {
    resTrial(NULL)
    running <<- FALSE
    state <<- "run"
    enableRunElements()
    disableElements(c("pause", "stop", "close"))
    if(runType == "F") msg("Test finished at fovea")
    else msg("Test finished")
    if(sum(res$type == "N") > 0) enableElements(c("save", "cancel"))
    else enableElements("close")
  }
  ##########
  # Routines
  ##########
  initRunVariables <- function(type) {
    resTrial(NA) # to force refresh
    resTrial(NULL)
    state <<- "run"
    # reset time counters
    if(type != "F") {
      tp0 <<- tt <<- tp <<- 0
      tt0 <<- Sys.time()
    }
    if(type == "A" || type == "F") {
      foveadb <<- NA
      # remove all results at the fovea
      res <<- res[res$type != "F",]
    }
    if(type == "A" || type == "N") {
      locs <<- grids[[input$grid]]$locs
      locs$th <<- NA
      locs$done <<- FALSE
      # remove all results but those at the fovea
      res <<- res[res$type == "F",]
    }
  }
  parseResults <- function(received, type) {
    loc <- as.numeric(received[1])
    x <- as.numeric(received[2])
    y <- as.numeric(received[3])
    level <- as.numeric(received[4])
    if(received[5] == "0" | received[5] == "1")
      seen <- as.logical(as.numeric(received[5]))
    else seen <- as.logical(received[5])
    time <- as.numeric(received[6])
    respWin <- as.numeric(received[7])
    done <- as.logical(received[8])
    th <- as.numeric(received[9])
    th <- ifelse(th == 0 && done && !seen, -2, th)
    lum <- as.numeric(received[10])
    size <- as.numeric(received[11])
    col <- received[12]
    return(data.frame(loc = loc, x = x, y = y, level = level,
                      type = type, seen = seen, time = time,
                      respWin = respWin, done = done,
                      lum = lum, size = size, col = col,
                      th = th, tt = tt, tp = tp))
  }
  updateLocations <- function(resReceived) {
    locs$th[resReceived$loc] <<- resReceived$th
    locs$done[resReceived$loc] <<- resReceived$done
  }
  saveResults <- function() {
    systime  <- Sys.time()
    tdate    <- format(systime, "%Y-%m-%d")
    ttime    <- format(systime, "%H:%M:%S")
    # if patient folder does not exist, then create
    dir.create("results/", showWarnings = FALSE) # create directories if they do not exist
    dir.create(paste0("results/logs/"), showWarnings = FALSE)
    fid <- paste(as.character(patient()$id), input$grid, sep = "_")
    # save log first
    fnamelog <- paste0("results/logs/", fid, format(systime, "_%Y%m%d_%H%M%S"), ".csv")
    write.csv(res, file = fnamelog, row.names = FALSE)
    # then save the processed test
    fname <- paste0("results/", fid, ".csv")
    if(input$algorithm == "ZEST")
      algpar <- input$estSD
    else if(input$algorithm == "MOCS")
      algpar <- input$nreps
    else
      algpar <- NA
    if(input$perimetry == "luminance") val <- input$size
    else val <- input$lum
    dat <- prepareToSave(patient(), input$machine, input$perimetry, input$algorithm,
                         input$grid, input$eye, bglum, input$lum, input$size,
                         input$dbstep, input$estSD, input$nreps, input$range,
                         tdate, ttime, input$comments, res, foveadb, locs)
    # if file exist, append result
    if(file.exists(fname)) dat <- rbind(read.csv(file = fname, colClasses = "character"), dat)
    # save test for the patient
    write.csv(dat, file = fname, row.names = FALSE)
  }
}