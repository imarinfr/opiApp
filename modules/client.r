clientUI <- function(id) {
  ns <- NS(id)
  machineChoices <- c("PhoneVR", "Octopus900", "SimHenson", "SimYes", "SimNo")
  gridNames <- names(grids)
  names(gridNames) <- unname(sapply(grids, function(gg) return(gg$name)))
  algorithms <- list("ZEST" = "ZEST", "Full Threshold" = "FT",
                     "Staircase 4-2" = "staircase", "MOCS" = "MOCS")
  tagList(
    fluidRow(
      column(4, htmlOutput(ns("patient"))),
      column(3, selectInput(ns("machine"), "OPI implementation",
                            choices = machineChoices, selected = appParams$machine)),
      column(2, radioButtons(ns("perimetry"), "Perimetry",
                             choices = c("luminance", "size"), selected = "luminance")),
      column(3, numericInput(ns("val"), "placeholder", value = NA)),
    ),
    fluidRow(
      column(3, selectInput(ns("grid"), "Grid", choices = gridNames, selected = gridNames[8])),
      column(2, selectInput(ns("eye"), "Eye", choices = list(Right = "R", Left = "L", Both = "B"),
                            selected = "Both")),
      column(3, selectInput(ns("algorithm"), "Algorithm", choices = algorithms, selected = "ZEST")),
      column(3, numericInput(ns("algval"), "placeholder", value = NA)),
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
  foveadb <- NULL
  res <- NULL
  locs <- NULL
  # define routine to initialized all run control variables
  tp0 <- tt <- tp <- tt0 <- NULL
  #########
  # Outputs
  #########
  output$patient <- renderUI(parsePatientOutput(patient()))
  output$msgconn <- renderText(msg())
  output$plotres <- renderPlot(resPlot(resTrial(), locs, input$eye, foveadb, appParams$maxlum))
  output$textres <- renderUI(renderResult(resTrial(), res, nrow(locs)))
  ##############
  # main control
  ##############
  # schedule running a trial step or receiving its results
  observe({
    invalidateLater(10)
    req(running)
    if(state == "run") { # run trial
      trialType <<- checkTrialType()
      runStep(trialType)
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
      tp <<- tp + as.numeric(difftime(Sys.time(), tp0, units = "secs"))
      updateActionButton(session, "pause", label = "Pause")
      msg("Continuing test ...")
    } else {
      tp0 <<- Sys.time()
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
    if(input$machine == "Octopus900" | input$machine == "SimHenson") { # only luminance available
      updateRadioButtons(session, "perimetry", selected = "luminance")
      disable("perimetry")
    } else enable("perimetry")
    if(input$machine == "PhoneVR") # only PhoneVR can test both eyes
      updateSelectInput(session, "eye", choices = list(Right = "R", Left = "L", Both = "B"),
                        selected = input$eye)
    else {
      updateSelectInput(session, "eye", choices = list(Right = "R", Left = "L"),
                        selected = ifelse(input$eye == "B", "R", input$eye))
    }
    msg(paste0("'", input$machine, "' implementation selected. Press 'OPI initialize' to start"))
  }) %>% bindEvent(input$machine)
  # if perimetry type changes
  observe({
    if(input$machine == "Octopus900" | input$machine == "SimHenson") {
      updateRadioButtons(session, "perimetry", selected = "luminance")
      disable("perimetry")
    } else enable("perimetry")
    fillVal()
  }) %>% bindEvent(input$perimetry, ignoreInit = TRUE)
  observe({
    if(is.na(input$val)) fillVal()
  }) %>% bindEvent(input$val)
  # if algorithm changes
  observe({
    if(is.na(input$algval)) fillAlgVal()
  }) %>% bindEvent(input$algval)
  # if algorithm changes delete all
  observe({
    initRunVariables("A")
    fillAlgVal()
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
    source("server/server.r", local = TRUE)
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
      msg(paste("OPI server ready for", input$machine))
    } else {
      enableElements(c("machine", "init"))
      msg(errortxt(msgtxt$message))
    }
    removeModal()
    Sys.sleep(0.5)
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
    if(input$machine == "PhoneVR")
      statement <- "opiSetBackground B annulus"
    else
      statement <- "opiSetBackground"
    ShinySender$push(title = "opiStatement", message = statement)
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    msgtxt <- ShinyReceiver$pop()
    if(msgtxt$title == "OK") {
      showModal(modalDialog(
        title = "Start testing the fovea with Goldmann size III (0.43\u00B0) luminance perimetry",
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
    statement <- paste("opiTestInit", input$eye, "luminance",
                       input$algorithm, 0.43, input$algval, "fovea")
    ShinySender$push(title = "opiStatement", message = statement)
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    msgtxt <- ShinyReceiver$pop()
    if(msgtxt$title == "OK") {
      disableRunElements()
      enableElements(c("pause", "stop"))
      msg("Running luminance perimetry with Goldmann size III at the fovea")
      runType <<- "F"
      initRunVariables(runType)
      running <<- TRUE
    } else {
      msg(errortxt(msgtxt$message))
      if(input$machine == "PhoneVR")
        statement <- "opiSetBackground B none"
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
    if(input$machine == "PhoneVR")
      statement <- "opiSetBackground B none"
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
    if(input$machine == "PhoneVR")
      statement <- paste("opiSetBackground B", appParams$fixtype)
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
    statement <- paste("opiTestInit", input$eye, input$perimetry, input$algorithm,
                       input$val, input$algval, input$grid)
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
    # check if there are catch trials to run
    lastTrials <- tail(res$type, 2)
    fp <- !any(lastTrials == "FP") && sum(res$type == "N") %% appParams$fprate == 0
    fn <- !any(lastTrials == "FN") && sum(res$type == "N") %% appParams$fnrate == 0
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
      pars <- falsePositivePars(locs, input$eye, appParams$respWin)
      statement <- paste("opiTestCatchTrial", pars$loc, pars$x, pars$y, pars$w, pars$db)
      ShinySender$push(title = "opiStatement", message = statement)
    }
    if(type == "FN") {
      pars <- falseNegativePars(locs, res, input$eye, appParams$respWin)
      if(!is.null(pars)) {
        statement <- paste("opiTestCatchTrial", pars$loc, pars$x, pars$y, pars$w, pars$db)
        ShinySender$push(title = "opiStatement", message = statement)
      }
    }
  }
  # read results
  readResults <- function(type) {
    resReceived <- NULL
    # update times
    tt <<- tt + as.numeric(difftime(Sys.time(), tt0, units = "secs"))
    tt0 <<- Sys.time()
    # check if all good
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    msgtxt <- ShinyReceiver$pop()
    if(msgtxt$title == "OK") {
      while(ShinyReceiver$empty()) Sys.sleep(0.1)
      received <- strsplit(ShinyReceiver$pop()$message, split = " ")[[1]]
      if(length(received) == 12)
        resReceived <- parseResults(received, type)
      else msg(errortxt("Wrong number of parameters"))
      if(!is.null(resReceived)) {
        res <<- rbind(res, resReceived)
        if(type == "F") # update results
          foveadb <<- resReceived$th
        else if(type == "N")
          updateLocations(resReceived)
      }
    } else msg(errortxt(msgtxt$message))
    return(resReceived)
  }
  # get results from the trial and return if done
  getTrialResults <- function(type) {
    done <- FALSE
    resTrial(readResults(type))
    if(!is.null(resTrial())) {
      if(type == "F") done <- resTrial()$done
      if(type == "N") done <- all(locs$done)
      if(done) finish()
    } else msg(errortxt("could not read the trial results from OPI server"))
  }
  # check if finished
  finish <- function() {
    resTrial(NULL)
    running <<- FALSE
    state <<- "run"
    enableRunElements()
    disableElements(c("eye", "algorithm", "algval", "pause", "stop", "close"))
    if(runType == "F")
      msg("Test finished at fovea")
    else
      msg("Test finished")
    enableElements("cancel")
    if(sum(res$type == "N") > 0) enableElements("save")
  }
  ##########
  # Routines
  ##########
  fillVal <- function() {
    if(input$perimetry == "luminance")
      updateNumericInput(session, "val", label = "Stimulus size", value = appParams$sizeForLum)
    else if(input$perimetry == "size")
      updateNumericInput(session, "val", label = "Stimulus luminance", value = appParams$lumForSize)
  }
  fillAlgVal <- function() {
    if(input$algorithm == "staircase" | input$algorithm == "FT" ) {
      updateNumericInput(session, "algval", label = "Initial estimate", value = appParams$est)
    } else if(input$algorithm == "MOCS") {
      updateNumericInput(session, "algval", label = "Repetitions", value = appParams$nreps)
    } else if(input$algorithm == "ZEST") {
      updateNumericInput(session, "algval", label = "Max estimate SD", value = appParams$estSD)
    }
  }
  initRunVariables <- function(type) {
    resTrial(NA) # to force refresh
    resTrial(NULL)
    state <<- "run"
    # reset time counters
    tp0 <<- tt <<- tp <<- 0
    tt0 <<- Sys.time()
    if(type == "A" || type == "F") {
      foveadb <<- NULL
      # remove all results regarding foveal tests
      res <<- res[res$type != "F",]
    }
    if(type == "A" || type == "N") {
      locs <<- grids[[input$grid]]$locs
      if(input$eye == "L") locs$x <<- -locs$x
      locs$th <<- NA
      locs$done <<- FALSE
      # remove all results that are not foveal test
      res <<- res[res$type == "F",]
    }
  }
  parseResults <- function(received, type) {
    loc <- as.numeric(received[1])
    x <- as.numeric(received[2])
    y <- as.numeric(received[3])
    level <- as.numeric(received[4])
    seen <- as.logical(received[5])
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
    dat <- prepareToSave(patient(), input$machine, input$perimetry, input$val,
                         input$grid, input$eye, input$algorithm, input$algval, tdate, ttime,
                         input$comments, res, foveadb, locs)
    # if file exist, append result
    if(file.exists(fname)) dat <- rbind(read.csv(file = fname, colClasses = "character"), dat)
    # save test for the patient
    write.csv(dat, file = fname, row.names = FALSE)
  }
}