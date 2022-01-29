source("serverUtils.r", local = TRUE)
###############
# ZEST routines 
###############
# helper to make stimulus
prepareZestRun <- function(eye, size, lum) {
  if(appParams$runType == "luminance") { # Luminance perimetry
    makeStimHelper <- function(x, y) { # returns a function of (db, n)
      ff <- function(db, n) db + n
      body(ff) <- substitute({
        return(list(eye = eye, cx = x, cy = y, sx = size, sy = size,
                    lum = appParams$bg + appParams$maxlum * 10^(-db / 10), col = appParams$color,
                    d = appParams$presTime, w = appParams$respWindow))
      }, list(x = x, y = y))
      return(ff)
    }
    # stimulus maker for false positive trials
    makeStimHelperFP <- function(x, y) { # returns a function of (db)
      ff <- function(db) db
      body(ff) <- substitute({
        return(list(eye = eye, cx = x, cy = y, sx = 0, sy = 0,
                    lum = appParams$bg, col = appParams$color,
                    d = appParams$presTime, w = appParams$respWindow))
      }, list(x = x, y = y))
      return(ff)
    }
    # domain
    domain <- -domainOffset:(round(10 * log10(appParams$maxlum / appParams$minlum)) + domainOffset)
    prior_pmf <- makePMF(domain, 30)
    minStim <- head(domain,1) + domainOffset
    maxStim <- tail(domain,1) - domainOffset
  } else if(appParams$runType == "size") { # Size Perimetry
    makeStimHelper <- function(x, y) { # returns a function of (db, n)
      ff <- function(db, n) db + n
      body(ff) <- substitute({
        return(list(eye = eye, cx = x, cy = y,
                    sx = appParams$maxdiam * 10^(-db / 10),
                    sy = appParams$maxdiam * 10^(-db / 10),
                    lum = appParams$bg + lum, col = appParams$color,
                    d = appParams$presTime, w = appParams$respWindow))
      }, list(x = x, y = y))
      return(ff)
    }
    # stimulus maker for false positive trials
    makeStimHelperFP <- function(x, y) { # returns a function of (db)
      ff <- function(db) db
      body(ff) <- substitute({
        return(list(eye = eye, cx = x, cy = y, sx = 0, sy = 0,
                    lum = appParams$bg, col = appParams$color,
                    d = appParams$presTime, w = appParams$respWindow))
      }, list(x = x, y = y))
      return(ff)
    }
    # domain
    domain <- -domainOffset:(round(10 * log10(appParams$maxdiam / appParams$mindiam)) + domainOffset)
    prior_pmf <- rep(1/length(domain), length(domain)) #uniform prior PMF
    minStim <- head(domain, 1) + domainOffset
    maxStim <- tail(domain, 1) - domainOffset
    # stimulus maker for false positive trials
  } else stop("wrong test type")
  return(list(domain = domain, prior_pmf = prior_pmf,
              minStim = minStim, maxStim = maxStim,
              makeStimHelper   = makeStimHelper,
              makeStimHelperFP = makeStimHelperFP))
}

# ZEST server for foveal test, zest test, etc.
zestServer <- future({
  set.seed(Sys.time())
  ##########################################################################################################
  # Define help functions
  ##########################################################################################################
  # foveal ZEST test
  fovealTest <- function() {
    makeStimHelper <- function() { # returns a function of (db, n)
      ff <- function(db, n) db + n
      body(ff) <- substitute({
        return(list(eye = eye, cx = x, cy = y, sx = 0.43, sy = 0.43,
                    lum = appParams$bg + appParams$maxlum * 10^(-db / 10), col = appParams$color,
                    d = appParams$presTime, w = appParams$respWindow))
      }, list(x = 0, y = 0))
      return(ff)
    }
    # domain
    domain <- -domainOffset:(round(10 * log10(appParams$maxlum / appParams$minlum)) + domainOffset)
    prior_pmf <- makePMF(domain, 30)
    minStim <- head(domain,1) + domainOffset
    maxStim <- tail(domain,1) - domainOffset
    # ZEST algorithm
    state <- ZEST.start(domain = domain, prior = prior_pmf, minStimulus = minStim, maxStimulus = maxStim, makeStim = makeStimHelper())
    while(!ZEST.stop(state)) {
      Sys.sleep(runif(1, min = 300, max = 600) / 1000)
      state <- ZEST.step(state)$state
    }
    return(max(-1, ZEST.final(state)))
  }
  
  # ZEST test
  zestTest <- function() {
    # weight stimulus choice by growth pattern wave
    weight <- 1 / (locs$wave[unfinished]^2)
    # choose next location to test
    if (length(unfinished) == 1) loc <- unfinished[1]
    else loc <- sample(unfinished, 1, prob = weight)
    sr <- ZEST.step(states[[loc]])  # present stimulus and obtain response
    states[[loc]] <<- sr$state      # update state
    if(ZEST.stop(states[[loc]])) {  # remove terminated location from queue
      ii <- which(unfinished == loc)
      unfinished <<- unfinished[-ii]
      finished <<- c(loc, finished)
      new_locs <- openNewLocs(findNeighbors(locs), loc, locs, unfinished, finished) # open up new locations
      unfinished <<- sort(c(unfinished, new_locs)) # add new locations to queue
      # for newly opened locations, create PMF based on previous terminated location's final threshold
      if (length(new_locs) > 0) {
        for (i in new_locs) {
          # find index of neighboring locations that have terminated
          nb <- intersect(which(findNeighbors(locs)[i, ] == TRUE), finished)
          # calculate mean threshold of neighboring locations that have terminated
          avg_th <- mean(sapply(nb, function (x) ZEST.final(states[[x]])))
          states[[i]]$pdf <<- makePMF(zestopt$domain, avg_th)
        }
      }
    }
    # wait if necessary
    if(sr$resp$seen) {
      if(sr$resp$time > 150) {
        respWin <<- c(sr$resp$time, respWin[-tail(respWin,1)]) # update recent response times vector
        appParams$respWindow <<- round(appParams$respWinPed + mean(respWin)) # update response window
      }
      if(substr(appParams$machine, 1, 3) != "Sim")
        Sys.sleep(runif(1, min = appParams$minISI, max = max(appParams$minISI, mean(respWin))) / 1000)
    }
    if(substr(appParams$machine, 1, 3) == "Sim") Sys.sleep(0.5)
    return(list(type = "Z", x = locs$x[loc], y = locs$y[loc],
                level   = tail(sr$state$stimuli, 1),
                th      = ZEST.final(sr$state),
                seen    = sr$resp$seen,
                time    = sr$resp$time,
                respWin = appParams$respWindow,
                done    = ZEST.stop(states[[loc]])))
  }
  zestTestCatchTrial <- function(type) {
    if(type == "P") {
      idx <- sample(1:nrow(locs), 1) # get random location
      level <- round(-10 * log10(0.001 / 400))
      res <- opiPresent(zestopt$makeStimHelperFP(locs$x[idx], locs$y[idx])(level))
    }
    if(type == "N") {
      if(appParams$runType == "luminance")
        # threshold is 100% of contrast sensitivity
        thr <- round(-10 * log10(appParams$bg / appParams$maxlum))       # TODO: CRITERION NEEDS TO BE REVISED
      else if(appParams$runType == "size")
        thr <- round(-10 * log10(appParams$mindiam / appParams$maxdiam)) # TODO: CRITERION NEEDS TO BE REVISED
      # check seen responses
      respseen <- lapply(states, function(ss) return(ss$stimuli[ss$responses]))
      # that are a greater value than the threshold
      idx <- which(sapply(respseen, function(rr) return(any(rr >= thr))))
      # if no point found, no N catch trial and ignore completely
      if(length(idx) == 0) {
        return(list(type = "F", x = NA, y = NA, level = NA, th = NA,
                    seen = NA, time = NA, respWin = NA,done = NA))
      }
      idx <- sample(idx, 1) # get one at random
      level <- 0 # maximum luminance and maximum size for false negatives
      res   <- opiPresent(zestopt$makeStimHelper(locs$x[idx], locs$y[idx])(level))
    }
    return(list(type = type, x = locs$x[idx], y = locs$y[idx], level = level, th = level,
                seen = res$seen, time = res$time, respWin = 0, done = NA))
  }
  ##########################################################################################################
  # Main algorithm
  ##########################################################################################################
  cmd <- "opiWait"
  repeat{
    # listen for instructions from the GUI. If command is opiWait, then wait a bit,
    # otherwise, go ahead
    if(!ShinySender$empty()) cmd <- readCommand(ShinySender)
    else if(cmd == "opiWait") Sys.sleep(0.1)
    else Sys.sleep(0.01) # let the system breath
    ################
    # OPI INITIALIZE
    ################
    if(cmd == "opiInit") {
      # get eye from message
      eye <- ShinySender$pop()$message
      # choose OPI
      if(!chooseOPI(appParams$machine)) {
        ShinyReceiver$push("ERR", "chooseOPI failed")
        break
      }
      msg <- tryCatch(do.call(what = opiInitialise, args = opiParams), error = function(e) e$message)
      if(!is.null(msg)) {
        ShinyReceiver$push("ERR", msg)
        break
      }
      if(!is.null(opiSetBackground(bgeye = eye, bglum = appParams$bg, bgcol = appParams$color,
                                   fixeye = eye, fixtype = "cross", fixsx = 2, fixsy = 2,
                                   fixlum = 3 * appParams$bg, fixcol = "green"))) {
        ShinyReceiver$push("ERR", "opiSetBackground failed")
        break
      }
      ShinyReceiver$push("OK", "OPI initialized")
      cmd <- "opiWait"
    }
    ################
    # OPI CLOSE
    ################
    if(cmd == "opiClose") {
      opiClose()
      ShinyReceiver$push("OK", "OPI closed")
      break
    }
    ################
    # OPI BACKGROUND
    ################
    if(cmd == "opiBg") {
      # get eye from message
      eye <- ShinySender$pop()$message
      if(!is.null(opiSetBackground(bgeye = eye, bglum = appParams$bg, bgcol = appParams$color,
                                   fixeye = eye, fixtype = "cross", fixlum = 3 * appParams$bg,
                                   fixcol = "green"))) {
        ShinyReceiver$push("ERR", "opiSetBackground failed")
        break
      }
      cmd <- "opiWait"
    }
    ################
    # OPI ZEST INIT
    ################
    if(cmd == "zestInit") {
      # get eye, size, and luminance from messages
      grid <- ShinySender$pop()$message
      eye  <- ShinySender$pop()$message
      lum  <- as.numeric(ShinySender$pop()$message)
      size <- as.numeric(ShinySender$pop()$message)
      locs <- grids[[which(gridNames %in% grid)]]
      if(eye == "L") locs$x <- -locs$x
      zestopt <- prepareZestRun(eye, size, lum)
      # Init states
      states <- lapply(1:nrow(locs), function(i)
        ZEST.start(domain = zestopt$domain, prior = zestopt$prior_pmf,
                   stopType  = "S", stopValue = 1.5,
                   minStimulus = zestopt$minStim, maxStimulus = zestopt$maxStim,
                   maxPresentations = 100,
                   makeStim  = zestopt$makeStimHelper(locs$x[i], locs$y[i])))
      # vector of locations with unfinished ZEST business
      unfinished <- which(locs$wave == 1)
      finished <- NULL
      # set up adaptive response window
      respWin <- rep(appParams$respWindow, appParams$respTimesLength)
      npress <- 0 # reset number of presentations to zero
      cmd <- "opiWait"
    }
    ################
    # OPI ZEST STEP
    ################
    if(cmd == "zestRun") {
      # check if there is a request to pause
      if(!ShinySender$empty() && ShinySender$pop()$message == "opiWait")
        cmd <- "opiWait"
      else {
        # check if it is time for a positive catch trial
        # FP = false positive trials
        # FN = false negative trials
        # N  = normal trial
        npress <- npress + 1 # get number of presentations so far
        if(npress %% appParams$fprate == 0)
          writeResults(ShinyReceiver, zestTestCatchTrial("P")) 
        else if(npress %% appParams$fnrate == 0)
          writeResults(ShinyReceiver, zestTestCatchTrial("N"))
        else
          writeResults(ShinyReceiver, zestTest())
        if(length(unfinished) == 0) cmd <- "opiWait"
      }
    }
    ################
    # OPI FOVEA
    ################
    if(cmd == "zestFovea") {
      # get eye from message
      eye <- ShinySender$pop()$message
      # change background for Foveal test and wait for 1 seconds
      do.call(what = opiSetBackground,
              args = list(bgeye = "B", bglum = appParams$bg,
                          fixtype = "annulus", fixsx = 2, fixsy = 2,
                          fixlum = 3 * appParams$bg,
                          bgcol = appParams$color))
      Sys.sleep(1)
      ShinyReceiver$push(title = "VAL", message = as.character(fovealTest()))
      # return back to regular background
      do.call(what = opiSetBackground,
              args = list(bgeye = "B", bglum = appParams$bg,
                          fixtype = "cross", fixsx = 2, fixsy = 2,
                          bgcol = appParams$color))
      cmd <- "opiWait"
    }
  }
},
seed = TRUE,
globals = list(appParams      = appParams,
               opiParams      = opiParams,
               grids          = grids,
               gridNames      = gridNames,
               domainOffset   = domainOffset,
               makePMF        = makePMF,            # ZEST help functions
               findNeighbors  = findNeighbors,
               openNewLocs    = openNewLocs,
               prepareZestRun = prepareZestRun,
               ShinySender    = ShinySender,        # communication files
               ShinyReceiver  = ShinyReceiver,
               readCommand    = readCommand,        # communication help functions
               writeResults   = writeResults),
packages = c("OPI", "deldir", "sp", "rgeos"))