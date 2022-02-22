source("utils/serverUtils.r", local = TRUE)
# test server
server <- future({
  set.seed(Sys.time())
  pars <- NULL
  repeat{
    Sys.sleep(0.01) # let the system breath
    cmd <- "opiIdle" # idle until instructions received
    #################
    # OPI Get Message
    #################
    # listen for instructions from the GUI. If command is opiIdle, then wait a bit,
    # otherwise, go ahead
    if(!ShinySender$empty()) {
      msg <- parseMessage(ShinySender$pop()$message, appParams)
      if(is.null(msg))
        ShinyReceiver$push("ERR", "OPI server: OPI statement badly constructed")
      else {
        cmd <- msg$cmd
        pars <- msg$pars
      }
    }
    ################
    # OPI Initialize
    ################
    if(cmd == "opiInit") {
      msg <- tryCatch(do.call(what = opiInitialize, args = pars), error = function(e) e$message)
      if(is.null(msg))
        ShinyReceiver$push("OK", paste0("OPI server: OPI initialized for '", chooseOPI()[.OpiEnv$chooser], "'"))
      else
        ShinyReceiver$push("ERR", paste("OPI server:", msg))
    }
    ####################
    # OPI Set Background
    ####################
    if(cmd == "opiSetBackground") {
      msg <- tryCatch(do.call(what = opiSetBackground, args = pars), error = function(e) e$message)
      if(is.null(msg))
        ShinyReceiver$push("OK", "OPI server: background changed")
      else
        ShinyReceiver$push("ERR", paste("OPI server:", msg))
    }
    ###############
    # OPI Test Init
    ###############
    if(cmd == "opiTestInit") {
      if(pars$grid == "fovea")
        locs <- data.frame(x = 0, y = 0, w = 1)
      else
        locs <- grids[[pars$grid]]$locs
      msg <- tryCatch({
        setup <- testSetup(chooseOPI()[.OpiEnv$chooser], appParams, pars$eye, pars$perimetry,
                              pars$algorithm, pars$val, pars$algpar, locs)
        states <- setup$states
        settings <- setup$settings
        if(!is.null(settings))
          msg <- NULL
        else
          msg <- "Wrong test settings"
      }, error = function(e) e$message)
      if(is.null(msg)) {
        ShinyReceiver$push("OK", "OPI server: test settings ready")
      } else
        ShinyReceiver$push("ERR", paste("OPI server:", msg))
    }
    ###################
    # OPI Test Step Run
    ###################
    if(cmd == "opiTestStepRun") {
      # go ahead with the step run
      msg <- tryCatch({
        # perform step
        rs <- testStep(states, settings)
        # update states and settings
        states <- rs$states
        settings <- rs$settings
        NULL
      }, error = function(e) e$message)
      if(is.null(msg)) { # if all good, inform, then send results
        ShinyReceiver$push("OK", "OPI server: test step successful")
        msg <- tryCatch(returnResults(rs$res), error = function(e) e$message)
      }
      else
        ShinyReceiver$push("ERR", paste("OPI server:", msg))
    }
    ######################
    # OPI TEST Catch Trial
    ######################
    if(cmd == "opiTestCatchTrial") {
      msg <- tryCatch({
        # present catch trial
        res <- testCatchTrial(settings, pars)
        NULL
      }, error = function(e) e$message)
      if(is.null(msg)) { # if all good, inform, then send results
        ShinyReceiver$push("OK", "OPI server: catch trial successful")
        msg <- tryCatch(returnResults(res), error = function(e) e$message)
      }
      else
        ShinyReceiver$push("ERR", paste("OPI server:", msg))
    }
    ##############
    # OPI Test End
    ##############
    if(cmd == "opiTestEnd") {
      settings <- NULL
      states <- NULL
      locs <- NULL
      if(is.null(msg))
        ShinyReceiver$push("OK", "OPI server: test ended successfully")
      else
        ShinyReceiver$push("ERR", paste("OPI server:", msg))
    }
    ###########
    # OPI Close
    ###########
    if(cmd == "opiClose") {
      opiClose()
      ShinyReceiver$push("OK", "OPI server: OPI closed")
      break
    }
  }
},
seed = TRUE,
globals = list(appParams = appParams,
               grids = grids,
               ShinySender = ShinySender,
               ShinyReceiver = ShinyReceiver,
               parseMessage = parseMessage,
               testSetup = testSetup,
               testStep = testStep,
               testCatchTrial = testCatchTrial,
               returnResults = returnResults),
packages = c("OPI", "deldir"))