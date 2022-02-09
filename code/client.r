clientUI <- function(id) {
  ns <- NS(id)
  gridNames <- names(grids)
  names(gridNames) <- unname(sapply(grids, function(gg) return(gg$name)))
  algorithms <- list("ZEST" = "ZEST", "Full Threshold" = "FT", "Staircase 4-2" = "staircase", "MOCS" = "MOCS")
  tagList(
    fluidRow(
      column(4, htmlOutput(ns("patient"))),
      column(3, selectInput(ns("machine"), "OPI implementation", choices = c("PhoneVR", "Octopus900", "SimHenson", "SimYes", "SimNo"), selected = appParams$machine)),
      column(2, radioButtons(ns("perimetry"), "Perimetry", choices = c("luminance", "size"), selected = "luminance")),
      column(3, numericInput(ns("val"), "placeholder", value = NA)),
    ),
    fluidRow(
      column(3, selectInput(ns("grid"), "Grid", choices = gridNames, selected = gridNames[8])),
      column(2, selectInput(ns("eye"), "Eye", choices = list(Right = "R", Left = "L", Both = "B"), selected = "Both")),
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
  opiParams <- NULL # parameters to pass to OPI implementation
  locs <- reactiveVal(NULL) # test locations
  foveadb <- reactiveVal(NULL) # sensitivity value obtained at the fovea
  res <- reactiveVal(NULL)
  run <- reactiveVal(FALSE)
  nextTrial <- reactiveVal(FALSE)
  malditoTimer <- reactiveTimer(500)
  testFovea <- FALSE
  msg <- reactiveVal() # message where to display status of connection, etc
  # define routine to initialized all run control variables
  tp0 <- tt <- tp <- tt0 <- NULL
  # outputs
  output$patient <- renderUI(parsePatientOutput(patient()))
  output$msgconn <- renderText(msg())
  output$plotres <- renderPlot(showPlot(locs(), input$eye, foveadb()))
  output$textres <- renderUI(renderResult(res(), nrow(locs())))
  ####################
  # EVENTS
  ####################
  # if machine changes
  observeEvent(input$machine, {
    if(input$machine == "Octopus900" | input$machine == "SimHenson") { # only luminance available
      updateRadioButtons(session, "perimetry", selected = "luminance")
      disable("perimetry")
    } else enable("perimetry")
    if(input$machine == "PhoneVR") # only PhoneVR can test both eyes
      updateSelectInput(session, "eye", choices = list(Right = "R", Left = "L", Both = "B"), selected = input$eye)
    else {
      updateSelectInput(session, "eye", choices = list(Right = "R", Left = "L"), selected = ifelse(input$eye == "B", "R", input$eye))
    }
    msg(paste0("'", input$machine, "' implementation selected. Press 'OPI initialize' to start"))
  })
  # if perimetry type changes
  observeEvent(input$perimetry, {
    if(input$machine == "Octopus900" | input$machine == "SimHenson") {
      updateRadioButtons(session, "perimetry", selected = "luminance")
      disable("perimetry")
    } else enable("perimetry")
    fillVal()
  }, ignoreInit = TRUE)
  observeEvent(input$val, {
    if(is.na(input$val)) fillVal()
  })
  # if algorithm changes
  observeEvent(input$algval, {
    if(is.na(input$algval)) fillAlgVal()
  })
  # if algorithm changes
  observeEvent(input$algorithm, {
    fillAlgVal()
    initRunVariables()
    foveadb(NULL)
  })
  # if grid or eye changes
  observeEvent(input$grid, {
    initRunVariables()
  })
  observeEvent(input$eye, {
    initRunVariables()
    foveadb(NULL)
  })
  # initialize OPI
  observeEvent(input$init, {
    # Initialize OPI server
    showModal(modalDialog(title = "OPI connection", "Initializing OPI", footer = NULL))
    ShinySender$reset()
    ShinyReceiver$reset()
    # initialize server
    source("server.r", local = TRUE)
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
      initRunVariables()
      foveadb(NULL)
      msg(paste("OPI server ready for", input$machine))
    } else {
      enableElements(c("machine", "init"))
      msg(errortxt(msgtxt$message))
    }
    removeModal()
    Sys.sleep(0.5)
  }, ignoreInit = TRUE)
  # close OPI connection
  observeEvent(input$close, {
    disableElements(c("close", "fovea", "run"))
    enableElements(c("machine", "init"))
    ShinySender$push(title = "CMD", message = "opiClose")
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    msg(ShinyReceiver$pop()$message)
    opiInitialized(FALSE)
    ShinySender$reset()
    ShinyReceiver$reset()
    foveadb(NULL)
    initRunVariables()
  }, ignoreInit = TRUE)
  # test fovea
  observeEvent(input$fovea, {
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
        footer = tagList(actionButton(ns("foveaOK"), "Start"), actionButton(ns("foveaCancel"), "Cancel"))
      ))
    } else {
      removeModal()
      msg(errortxt(msgtxt$message))
    }
  }, ignoreInit = TRUE)
  # if OK to test fovea
  observeEvent(input$foveaOK, {
    statement <- paste("opiTestInit", input$eye, "luminance", input$algorithm, 0.43, input$algval, "fovea")
    ShinySender$push(title = "opiStatement", message = statement)
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    msgtxt <- ShinyReceiver$pop()
    if(msgtxt$title == "OK") {
      disableRunElements()
      enableElements(c("pause", "stop"))
      msg("Running luminance perimetry with Goldmann size III at the fovea")
      foveadb(NULL)
      resetTimers()      
      run(TRUE)
      testFovea <<- TRUE
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
  }, ignoreInit = TRUE)
  observeEvent(input$foveaCancel, {
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
  }, ignoreInit = TRUE)
  # start or continue test
  observeEvent(input$run, {
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
  }, ignoreInit = TRUE)
  # if OK to run test in selected grid
  observeEvent(input$runOK, {
    statement <- paste("opiTestInit", input$eye, input$perimetry, input$algorithm, input$val, input$algval, input$grid)
    ShinySender$push(title = "opiStatement", message = statement)
    while(ShinyReceiver$empty()) Sys.sleep(0.1)
    msgtxt <- ShinyReceiver$pop()
    if(msgtxt$title == "OK") {
      disableRunElements()
      enableElements(c("pause", "stop"))
      msg(paste("Running", input$algorithm, input$perimetry, "perimetry for eye", input$eye))
      initRunVariables()
      run(TRUE)
      testFovea <<- FALSE
    } else msg(errortxt(msgtxt$message))
    removeModal()
    Sys.sleep(0.5)
  })
  # pause test halfway through
  observeEvent(input$pause, {
    if(run() == TRUE) {
      updateActionButton(session, "pause", label = "Continue")
      msg("Test paused. Press 'Continue' to restart the test")
      run(FALSE)
      tp0 <<- Sys.time()
    } else {
      updateActionButton(session, "pause", label = "Pause")
      msg("Running ...")
      run(TRUE)
      # update pause times
      tp <<- tp + as.numeric(difftime(Sys.time(), tp0, units = "secs"))
    }
  }, ignoreInit = TRUE)
  # stop test halfway through
  observeEvent(input$stop, { # stop test
    showModal(modalDialog(
      title = "Are you sure you want to terminate the test?",
      "Press TERMINATE to cancel",
      footer = tagList(actionButton(ns("stopOK"), "TERMINATE"), modalButton("Cancel"))
    ))
  }, ignoreInit = TRUE)
  # OK to terminate halfway through?
  observeEvent(input$stopOK, { # terminate test
    run(FALSE)
    removeModal()
    msg(errortxt("OPI ZEST test canceled"))
    enableRunElements()
    disableElements(c("pause", "stop", "save"))
  }, ignoreInit = TRUE)
  # save test once finished
  observeEvent(input$save, { # save and allow to run a fresh test
    saveResults()
    updateTextInput(session, "comments", value = "")
    disableElements(c("save", "cancel"))
    enableRunElements()
    msg("Results have been saved")
    newReports(TRUE)
  }, ignoreInit = TRUE)
  # cancel save once the test has finished
  observeEvent(input$cancel, { # do not save and allow to run a fresh test
    updateTextInput(session, "comments", value = "")
    disableElements(c("save", "cancel"))
    enableRunElements()
    msg(errortxt("Results have not been saved"))
  }, ignoreInit = TRUE)
  ####################################################################
  # main loop: listening to what remote perimetry test code has to say
  ####################################################################
  observeEvent(malditoTimer(), {
    if(malditoTimer()) {
      if(run()) nextTrial(TRUE)
    }
  })
  observe({
    if(nextTrial()) {
      nextTrial(FALSE)
      ShinySender$push(title = "opiStatement", message = "opiTestStepRun")
      while(ShinyReceiver$empty()) Sys.sleep(0.1)
      msgtxt <- ShinyReceiver$pop()
      if(msgtxt$title == "OK") {
        # if all good update results
        trialRes <- updateResults("N") # normal trial
        if(!is.null(trialRes)) {
          if(testFovea) { # test at fovea
            foveadb(trialRes$th)
            if(trialRes$done) {
              run(FALSE)
              enableRunElements()
              disableElements(c("eye", "algorithm", "algval", "pause", "stop"))
            }
          } else { # test at locs
            res(rbind(res(), trialRes)) # update results
            # update locations
            updateLocations(trialRes)
            if(all(locs()$done)) {
              msg("Test finished")
              run(FALSE)
              disableElements(c("pause", "stop"))
              enableElements(c("save", "cancel"))
            }
          }
        } else msg(errortxt(msgtxt$message))
        if(!testFovea) {
          # run catch trials as necessary
          if(sum(res()$type == "N") %% appParams$fprate == 0) {
            pars <- falsePositivePars(locs(), appParams$respWin)
            statement <- paste("opiTestCatchTrial", pars$x, pars$y, pars$w, pars$db)
            ShinySender$push(title = "opiStatement", message = statement)
            while(ShinyReceiver$empty()) Sys.sleep(0.1)
            msgtxt <- ShinyReceiver$pop()
            if(msgtxt$title == "OK")
              res(rbind(res(), updateResults("FP"))) # update results with false positive
          }
          if(sum(res()$type == "N") %% appParams$fnrate == 0) {
            pars <- falsePositivePars(res(), appParams$respWin)
            # only if there are locations for testing false negatives, 
            if(!is.null(pars)) {
              statement <- paste("opiTestCatchTrial", pars$x, pars$y, pars$w, pars$db)
              ShinySender$push(title = "opiStatement", message = statement)
              while(ShinyReceiver$empty()) Sys.sleep(0.1)
              msgtxt <- ShinyReceiver$pop()
              if(msgtxt$title == "OK")
                res(rbind(res(), updateResults("FN"))) # update results with false negative
            }
          }
        }
      } else msg(errortxt(msgtxt$message))
    }
  })
  ####################
  # SUBROUTINES
  ####################
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
  initRunVariables <- function() {
    res(NULL)
    # init locations
    ll <- grids[[input$grid]]$locs
    if(input$eye == "L") ll$x <- -ll$x
    ll$th <- NA
    ll$done <- FALSE
    locs(ll)
    resetTimers()
  }
  resetTimers <- function () {
    tp0 <<- tt <<- tp <<- 0 # to control test time and pause
    tt0 <<- Sys.time()      # start test time is current system time
  }
  updateResults <- function(type) {
    msg <- strsplit(ShinyReceiver$pop()$message, split = " ")[[1]]
    if(length(msg) != 9) return("Problem reading results from server")
    tt <<- tt + as.numeric(difftime(Sys.time(), tt0, units = "secs"))
    tt0 <<- Sys.time()
    # read results
    loc <- as.numeric(msg[1])
    x <- as.numeric(msg[2])
    y <- as.numeric(msg[3])
    level <- as.numeric(msg[4])
    seen <- as.logical(msg[5])
    time <- as.numeric(msg[6])
    respWin <- as.numeric(msg[7])
    done <- as.logical(msg[8])
    th <- as.numeric(msg[9])
    th <- ifelse(th == 0 && done && !seen, -2, th)
    return(data.frame(loc = loc, x = x, y = y, level = level, type = type, seen = seen,
                      time = time, respWin = respWin, done = done, th = th,
                      tt = tt, tp = tp))
  }
  # update locations
  updateLocations <- function(trialRes) {
    ll <- locs()
    ll$th[trialRes$loc] <- trialRes$th
    ll$done[trialRes$loc] <- trialRes$done
    locs(ll)
  }
  # save results at the end of the test
  saveResults <- function() {
    systime  <- Sys.time()
    tdate    <- format(systime, "%Y-%m-%d")
    ttime    <- format(systime, "%H:%M:%S")
    # if patient folder does not exist, then create
    dir.create("../results/", showWarnings = FALSE) # create directories if they do not exist
    dir.create(paste0("../results/logs/"), showWarnings = FALSE)
    fid <- paste(as.character(patient()$id), input$grid, sep = "_")
    # save log first
    fnamelog <- paste0("../results/logs/", fid, format(systime, "_%Y%m%d_%H%M%S"), ".csv")
    write.csv(res(), file = fnamelog, row.names = FALSE)
    # then save the processed test
    fname <- paste0("../results/", fid, ".csv")
    dat <- prepareToSave(patient(), input$machine, input$perimetry, input$val,
                         input$grid, input$eye, input$algorithm, input$algval, tdate, ttime,
                         input$comments, res(), foveadb(), locs())
    # if file exist, append result
    if(file.exists(fname)) dat <- rbind(read.csv(file = fname, colClasses = "character"), dat)
    # save test for the patient
    write.csv(dat, file = fname, row.names = FALSE)
  }
}