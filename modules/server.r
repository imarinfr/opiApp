source("modules/serverUtils.r", local = TRUE)
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
      res <- parseMessage(ShinySender$pop()$message, appParams)
      if(is.null(res))
        ShinyReceiver$push("ERR", "OPI server: OPI statement badly constructed")
      else {
        cmd <- res$cmd
        pars <- res$pars
      }
    }
    ################
    # OPI Initialize
    ################
    if(cmd == "opiInit") {
      res <- tryCatch(do.call(what = opiInitialize, args = pars), error = function(e) e$message)
      if(is.null(res) || is.null(res$err))
        ShinyReceiver$push("OK", paste0("OPI server: OPI initialized for '", chooseOPI()[.OpiEnv$chooser], "'"))
      else
        ShinyReceiver$push("ERR", paste("OPI server:", res))
    }
    ####################
    # OPI Set Background
    ####################
    if(cmd == "opiSetBackground") {
      res <- tryCatch(do.call(what = opiSetBackground, args = pars), error = function(e) e$message)
      if(is.null(res) || is.null(res$error))
        ShinyReceiver$push("OK", "OPI server: background changed")
      else
        ShinyReceiver$push("ERR", paste("OPI server:", res))
    }
    ###############
    # OPI Test Init
    ###############
    if(cmd == "opiTestInit") {
      if(pars$grid == "fovea")
        locs <- data.frame(x = 0, y = 0, w = 1, est = round(cdTodb(dbTocd(30), appParams$maxlum)))
      else
        locs <- grids[[pars$grid]]$locs
      res <- tryCatch({
        setup <- testSetup(chooseOPI()[.OpiEnv$chooser], appParams, pars, locs)
        states <- setup$states
        settings <- setup$settings
        if(!is.null(settings))
          res <- NULL
        else
          res <- "Wrong test settings"
      }, error = function(e) e$message)
      if(is.null(res)) {
        ShinyReceiver$push("OK", "OPI server: test settings ready")
      } else
        ShinyReceiver$push("ERR", paste("OPI server:", res))
    }
    ###################
    # OPI Test Step Run
    ###################
    if(cmd == "opiTestStepRun") {
      # go ahead with the step run
      res <- tryCatch({
        # perform step
        rs <- testStep(states, settings)
        # update states and settings
        states <- rs$states
        settings <- rs$settings
        NULL
      }, error = function(e) e$message)
      if(is.null(res)) { # if all good, inform, then send results
        ShinyReceiver$push("OK", "OPI server: test step successful")
        res <- tryCatch(returnResults(rs$res), error = function(e) e$message)
      }
      else
        ShinyReceiver$push("ERR", paste("OPI server:", res))
    }
    ######################
    # OPI TEST Catch Trial
    ######################
    if(cmd == "opiTestCatchTrial") {
      res <- tryCatch({
        # present catch trial
        catch <- testCatchTrial(settings, pars)
        NULL
      }, error = function(e) e$message)
      if(is.null(res)) { # if all good, inform, then send results
        ShinyReceiver$push("OK", "OPI server: catch trial successful")
        res <- tryCatch(returnResults(catch), error = function(e) e$message)
      }
      else
        ShinyReceiver$push("ERR", paste("OPI server:", res))
    }
    ##############
    # OPI Test End
    ##############
    if(cmd == "opiTestEnd") {
      settings <- NULL
      states <- NULL
      locs <- NULL
      ShinyReceiver$push("OK", "OPI server: test ended successfully")
    }
    ###########
    # OPI Close
    ###########
    if(cmd == "opiClose") {
      try(opiClose(), silent = TRUE)
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