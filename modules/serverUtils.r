################################
# shared functions among servers
################################
# read command from the client
parseMessage <- function(msg, appParams) {
  # get all messages received
  msg <- strsplit(msg, split = " ")[[1]]
  cmd <- msg[1]
  pars <- NULL
  if(cmd != "opiInit" &
     cmd != "opiSetBackground" &
     cmd != "opiTestInit" &
     cmd != "opiTestStepRun" &
     cmd != "opiTestCatchTrial" &
     cmd != "opiTestEnd" &
     cmd != "opiClose")
    return(NULL)
  if(cmd == "opiInit") {
    if(length(msg) < 2) return(NULL)
    if(msg[2] == "Octopus900" && length(msg) != 3) return(NULL)
    if(msg[2] != "Octopus900" && length(msg) != 2) return(NULL)
    if(!chooseOPI(msg[2])) return(NULL)
    pars <- opiInitParams(msg, appParams)
  }
  if(cmd == "opiSetBackground") {
    if(is.na(chooseOPI()[.OpiEnv$chooser])) return(NULL)
    if((chooseOPI()[.OpiEnv$chooser] == "PhoneHMD" & length(msg) == 4) ||
       (chooseOPI()[.OpiEnv$chooser] == "Octopus900" & length(msg) == 2) ||
       length(msg) == 1)
      pars <- opiBackgroundParams(chooseOPI()[.OpiEnv$chooser], appParams, msg)
    else return(NULL)
  }
  if(cmd == "opiTestInit") {
    if(length(msg) != 11) return(NULL)
    pars <- opiTestInitParams(msg)
  }
  if(cmd == "opiTestStepRun") if(length(msg) != 1) return(NULL)
  if(cmd == "opiTestCatchTrial") {
    if(length(msg) != 5) return(NULL)
    pars <- opiTestCatchTrialParams(msg)
  }
  if(cmd == "opiTestEnd") if(length(msg) != 1) return(NULL)
  if(cmd == "opiClose") if(length(msg) != 1) return(NULL)
  return(list(cmd = cmd, pars = pars))
}
# send results to client
returnResults <- function(res)
  ShinyReceiver$push(title = "RESULT",
                     message = paste(res$loc, res$x, res$y,
                                     res$level, res$seen,
                                     res$time, res$respWin,
                                     res$done, res$th,
                                     res$lum, res$size, res$col))
opiInitParams <- function(msg, appParams) {
  pars <- opiGetParams("opiInitialize")
  if(msg[2] == "PhoneHMD") {
    pars$ip <- appParams$ip
    pars$port <- appParams$port
    pars$lut <- appParams$lut
  }
  if(msg[2] == "Compass") {
    pars$ip <- appParams$ip
    pars$port <- appParams$port
  }
  if(msg[2] == "imo") {
    pars$ip <- appParams$ip
    pars$port <- appParams$port
  }
  if(msg[2] == "Octopus900") {
    pars$serverPort <- appParams$port
    pars$eyeSuiteSettingsLocation <- appParams$O900path
    pars$bigWheel <- appParams$O900wheel
    pars$zero_dB_is_10000_asb <- appParams$O900max
    pars$eye <- ifelse(msg[3] == "R", "right", "left")
  }
  return(pars)
}
opiBackgroundParams <- function(machine, appParams, msg) {
  pars <- opiGetParams("opiSetBackground")
  if(machine == "PhoneHMD") {
    pars$bglum <- appParams$bglum
    pars$bgcol <- appParams$bgcol
    pars$fixlum <- appParams$fixlum
    pars$fixcol <- appParams$fixcol
    pars$bgeye <- msg[2]
    pars$fixeye <- msg[3]
    pars$fixtype <- msg[4]
  }
  if(machine == "Compass") {
    # use defaults with no fixation
    pars$fixation <- c(0, 0, 0)
    pars$tracking_on <- FALSE
  }
  if(machine == "imo") {
    # by now use only defaults
  }
  if(machine == "Octopus900") {
    pars$lum <- .OpiEnv$O900$BG_10
    pars$color <- .OpiEnv$O900$MET_COL_WW
    pars$fixation <- ifelse(msg[2] == "CROSS", .OpiEnv$O900$FIX_CROSS,
                            .OpiEnv$O900$FIX_CENTER)
    pars$fixIntensity <- 20
    }
  if(substr(machine, 1, 3) == "Sim") {
    pars$col <- NA
    pars$gridCol <- NA
  }
  return(pars)
}
opiTestInitParams <- function(msg)
  return(list(eye = msg[2], perimetry = msg[3], algorithm = msg[4], grid = msg[5],
              size = as.numeric(msg[6]), lum = as.numeric(msg[7]),
              dbstep = as.numeric(msg[8]), estSD = as.numeric(msg[9]),
              nreps = as.numeric(msg[10]), range = as.numeric(msg[11])))
opiTestCatchTrialParams <- function(msg)
  return(list(loc = as.numeric(msg[2]),
              x = as.numeric(msg[3]), y = as.numeric(msg[4]),
              db = as.numeric(msg[5])))
# prepare test settings
testSetup <- function(machine, appParams, pars, locs) {
  # get stimulus helper for the specific machine (OPI implementation) and
  # perimetry type (luminance or size)
  makeStimHelper <- makeStimHelperConstructor(machine, appParams, pars)
  if(pars$perimetry == "luminance") maxval <- appParams$maxlum
  if(pars$perimetry == "size") maxval <- appParams$maxdiam
  # get the rest of algorithm-dependent parameters
  settings <- NULL
  states <- NULL
  if(machine == "PhoneHMD") return(setupPhoneHMD(machine, appParams, pars, locs, makeStimHelper, maxval))
  if(machine == "Compass") return(setupCompass(machine, appParams, pars, locs, makeStimHelper, maxval))
  if(machine == "Octopus900") return(setupOctopus(machine, appParams, pars, locs, makeStimHelper, maxval))
  if(machine == "imo") return(setupIMO(machine, appParams, pars, locs, makeStimHelper, maxval))
  if(substr(machine, 1, 3) == "Sim") return(setupSimulation(machine, appParams, pars, locs, makeStimHelper, maxval))
}
# set up for PhoneHMD
setupPhoneHMD <- function(machine, appParams, pars, locs, makeStimHelper, maxval) {
  if(pars$perimetry == "luminance") {
    minval <- 0.01 # min incremental value to present. Corresponds to about 48 HFA dB
    minstim <- round(cdTodb(minval, maxval - appParams$bglum), 1)
  }
  if(pars$perimetry == "size") {
    minval <- 0.01 # degrees; approx to Size I / 2.
    minstim <- round(cdTodb(minval, maxval), 1)
  }
  st <- initStates(pars, minstim, maxval, locs)
  settings <- initSettings(machine, appParams, pars, makeStimHelper,
                           st$domain, minstim, maxlum, locs)
  return(list(states = st$states, settings = settings))
}
# set up for Compass
setupCompass <- function(machine, appParams, pars, locs, makeStimHelper, maxval) {
  maxlum <- 10000 / pi
  minstim <- 50
  st <- initStates(pars, minstim, maxval, locs)
  settings <- initSettings(machine, appParams, pars, makeStimHelper,
                           st$domain, minstim, maxlum, locs)
  return(list(states = st$states, settings = settings))
}
# set up Octopus
setupOctopus <- function(machine, appParams, pars, locs, makeStimHelper, maxval) {
  if(appParams$O900max) maxlum <- 10000 / pi
  else maxlum <- 4000 / pi
  minstim <- 50
  st <- initStates(pars, minstim, maxval, locs)
  settings <- initSettings(machine, appParams, pars, makeStimHelper,
                           st$domain, minstim, maxlum, locs)
  return(list(states = st$states, settings = settings))
}
# set up IMO
setupIMO <- function(machine, appParams, pars, locs, makeStimHelper, maxval) {
  maxlum <- 10000 / pi
  minstim <- 50
  st <- initStates(pars, minstim, maxval, locs)
  settings <- initSettings(machine, appParams, pars, makeStimHelper,
                           st$domain, minstim, maxlum, locs)
  return(list(states = st$states, settings = settings))
}
# set up Simulation
setupSimulation <- function(machine, appParams, pars, locs, makeStimHelper, maxval) {
  maxlum <- 10000 / pi
  minstim <- 40
  st <- initStates(pars, minstim, maxval, locs)
  settings <- initSettings(machine, appParams, pars, makeStimHelper,
                           st$domain, minstim, maxlum, locs)
  return(list(states = st$states, settings = settings))
}
# create states and settings
initStates <- function(pars, minstim, maxval, locs) {
  states <- NULL
  if(pars$algorithm == "ZEST") {
    # add offset to the domain
    offset <- 5
    domain <- seq(-offset, minstim + offset, by = pars$dbstep)
    # we do not pass the makeStim here, but generate one at every iteration to be
    # able to modify the response window
    for(i in 1:nrow(locs))
      states[[i]] <- ZEST.start(domain = domain,
                                prior = bimodal_pmf(domain, locs$est[i], pars$dbstep),
                                stopType = "S", stopValue = pars$estSD,
                                minStimulus = 0, maxStimulus = minstim,
                                makeStim = NULL)
  } else if(pars$algorithm == "MOCS") {
    # add n levels around the initial guess (at 0 in the domain) separated by dbstep
    ndB <- pars$range / 2
    domain <- seq(-ndB, ndB, by = pars$dbstep)
    for(i in 1:nrow(locs))
      states[[i]] <- MOCS.start(locs$est[i], domain, pars$nreps, minstim)
  } else {
    domain <- seq(0, minstim, by = pars$dbstep)
    if(pars$algorithm == "FT") {
      for(i in 1:nrow(locs))
        states[[i]] <- FT.start(est = locs$est[i], instRange = c(0, minstim), makeStim = NULL)
    } else if(pars$algorithm == "staircase") {
      for(i in 1:nrow(locs))
        states[[i]] <- fourTwo.start(est = locs$est[i], instRange = c(0, minstim), makeStim = NULL)
    }
  }
  return(list(domain = domain, states = states))
}
# create states and settings
initSettings <- function(machine, appParams, pars, makeStimHelper, domain, minstim, maxval, locs) {
  settings <- NULL
  if(pars$algorithm == "ZEST") {
    settings$stepf <- ZEST.step
    settings$stopf <- ZEST.stop
    settings$finalf <- ZEST.final
  } else if(pars$algorithm == "MOCS") {
    settings$nreps <- pars$nreps
    settings$stepf <- MOCS.step
    settings$stopf <- MOCS.stop
    settings$finalf <- MOCS.final
  } else if(pars$algorithm == "FT") {
    settings$stepf <- FT.step
    settings$stopf <- FT.stop
    settings$finalf <- FT.final2
  } else if(pars$algorithm == "staircase") {
    settings$stepf <- fourTwo.step
    settings$stopf <- fourTwo.stop
    settings$finalf <- fourTwo.final2
  }
  if(!is.null(settings)) {
    settings$machine <- machine
    settings$eye <- pars$eye
    settings$perimetry <- pars$perimetry
    settings$algorithm <- pars$algorithm
    settings$x <- locs$x
    settings$y <- locs$y
    settings$w <- locs$w
    settings$unfinished <- which(settings$w == 1)
    # make stimulus helper, locations waves, initial locations, and response window
    settings$makeStimHelper <- makeStimHelper
    settings$domain <- domain
    settings$maxval <- maxval
    settings$minstim <- minstim
    settings$maxstim <- 0
    settings$dbstep <- pars$dbstep
    settings$nn <- findNeighbors(locs)
    settings$respWin <- appParams$respWin
    settings$winFloor <- appParams$winFloor
    settings$minRespWin <- appParams$minRespWin
    settings$minISI <- appParams$minISI
    settings$movingResp <- rep(appParams$respWin, appParams$slidWidth)
  }
  return(settings)
}
# process test step
testStep <- function(states, settings) {
  # choose next location to test by growth pattern wave
  if(length(settings$unfinished) == 1)
    loc <- settings$unfinished
  else
    loc <- sample(settings$unfinished, 1, prob = 1 / (settings$w[settings$unfinished]^2))
  # update the makeStim with the most current response time
  states[[loc]]$makeStim <- settings$makeStimHelper(settings$x[loc], settings$y[loc], settings$respWin)
  # present stimulus and obtain response
  sr <- tryCatch(settings$stepf(states[[loc]]), error = function(e) NULL)
  if(is.null(sr)) return(NULL)
  if(sr$resp$seen & # simulate random response time
     substr(settings$machine, 1, 3) == "Sim")
    sr$resp$time <- round(runif(1, settings$winFloor, settings$respWin))
  states[[loc]] <- sr$state # update state
  # if location finished
  if(settings$stopf(states[[loc]])) {
    # remove terminated location from queue
    settings$unfinished <- settings$unfinished[-which(settings$unfinished == loc)]
    # update states and add the new locations for testing
    nl <- setupNewLocs(states, settings, loc)
    if(!is.null(nl)) {
      states <- nl$states
      settings$unfinished <- sort(c(settings$unfinished, nl$locs))
    }
  }
  th <- settings$finalf(states[[loc]])
  th <- ifelse(th > settings$minstim, settings$minstim, th)
  th <- ifelse(th < 0, 0, th)
  done <- settings$stopf(states[[loc]])
  level <- tail(sr$state$stimuli, 1)
  stimInfo <- getStepStimInfo(settings$machine, states[[loc]]$makeStim(level, 0))
  # wait times
  if(sr$resp$seen) {
    isi <- round(runif(1, settings$minISI, max(settings$minISI, mean(settings$movingResp))))
    # update response moving window and time
    if(sr$resp$time > settings$winFloor) {
      settings$movingResp <- c(settings$winFloor + sr$resp$time, head(settings$movingResp, length(settings$movingResp) - 1))
      settings$respWin <- round(mean(settings$movingResp))
      if(settings$respWin < settings$minRespWin) settings$respWin <- settings$minRespWin
    }
    Sys.sleep(isi / 1000)
  }
  return(list(states = states, # updated states
              settings = settings, # updated settings
              res = list(loc = loc, x = settings$x[loc], y = settings$y[loc],
                         level = level,
                         seen = sr$resp$seen,
                         time = sr$resp$time,
                         respWin = settings$respWin,
                         done = done,
                         th = round(th, 1),
                         lum = stimInfo$lum, size = stimInfo$size, col = stimInfo$col)))
}
testCatchTrial <- function(settings, pars) {
  # return selected location with values for catch trial
  stim <- settings$makeStimHelper(pars$x, pars$y,settings$respWin)(pars$db, 0)
  res <- opiPresent(stim)
  stimInfo <- getStepStimInfo(settings$machine, stim)
  # wait times
  if(res$seen) {
    isi <- round(runif(1, settings$minISI, max(settings$minISI, mean(settings$movingResp))))
    Sys.sleep(isi / 1000)
  }
  return(list(loc = pars$loc, x = stim$x, y = stim$y,
              level = -1, seen = res$seen,
              time = res$time, respWin = stimInfo$w,
              done = FALSE, th = -1,
              lum = stimInfo$lum, size = stimInfo$size, col = stimInfo$col))
}
getStepStimInfo <- function(machine, stim) {
  if(machine == "PhoneHMD") {
    lum <- stim$lum
    size <- stim$sx
    w <- stim$w
    col <- stim$col
  } else {
    lum <- stim$level
    size <- stim$size
    w <- stim$responseWindow
    col <- "#FFFFFF"
  }
  return(list(lum = lum, size = size, w = w, col = col))
}
# stimulus helper constructor
makeStimHelperConstructor <- function(machine, appParams, pars) {
  if(pars$perimetry == "luminance") {
    if(machine == "PhoneHMD") {
      makeStimHelper <- function(x, y, w) {  # returns a function of (db,n)
        ff <- function(db, n) db + n
        body(ff) <- substitute({
          s <- list(eye = pars$eye,
                    x = ifelse(pars$eye == "L", -x, x), y = y,
                    sx = pars$size, sy = pars$size,
                    lum = appParams$bglum +
                      dbTocd(db, appParams$maxlum - appParams$bglum),
                    col = appParams$stcol, d = appParams$presTime, w = w)
          class(s) <- "opiStaticStimulus"
          return(s)
        }, list(x = x, y = y, w = w))
        return(ff)
      }
    } else if(machine == "imo") {
      makeStimHelper <- function(x, y, w) {  # returns a function of (db,n)
        ff <- function(db, n) db + n
        body(ff) <- substitute({
          s <- list(eye = pars$eye,
                    x = x, y = y,
                    size = pars$size, level = dbTocd(db, appParams$maxlum),
                    duration = appParams$presTime, responseWindow = w)
          class(s) <- "opiStaticStimulus"
          return(s)
        }, list(x = x, y = y, w = w))
        return(ff)
      }
    } else if(machine == "Compass") {
      makeStimHelper <- function(x, y, w) {  # returns a function of (db,n)
        ff <- function(db, n) db + n
        body(ff) <- substitute({
          s <- list(x = x, y = y, size = pars$size,
                    level = dbTocd(db, .OpiEnv$Compass$ZERO_DB_IN_ASB / pi),
                    duration = appParams$presTime, responseWindow = w)
          class(s) <- "opiStaticStimulus"
          return(s)
        }, list(x = x, y = y, w = w))
        return(ff)
      }
    } else if(machine == "Octopus900") {
      if(appParams$O900max) maxlum <- 10000 / pi
      else maxlum <- 4000 / pi
      makeStimHelper <- function(x, y, w) {  # returns a function of (db,n)
        ff <- function(db, n) db + n
        body(ff) <- substitute({
          s <- list(x = x, y = y, size = pars$size,
                    level = dbTocd(db, maxlum),
                    duration = appParams$presTime, responseWindow = w)
          class(s) <- "opiStaticStimulus"
          return(s)
        }, list(x = x, y = y, w = w))
        return(ff)
      }
    } else if(substr(machine, 1, 3) == "Sim") {
      makeStimHelper <- function(x, y, w) {  # returns a function of (db,n)
        ff <- function(db, n) db + n
        body(ff) <- substitute({
          s <- list(x = x, y = y, size = pars$size,
                    level = dbTocd(db, 10000 / pi),
                    duration = appParams$presTime, responseWindow = w)
          class(s) <- "opiStaticStimulus"
          return(s)
        }, list(x = x, y = y, w = w))
        return(ff)
      }
    } else makeStimHelper <- NULL
  } else if(pars$perimetry == "size") {
    if(machine == "PhoneHMD") {
      makeStimHelper <- function(x, y, w) {  # returns a function of (db,n)
        ff <- function(db, n) db + n
        body(ff) <- substitute({
          s <- list(eye = pars$eye,
                    x = ifelse(pars$eye == "L", -x, x), y = y,
                    sx = dbTocd(db, appParams$maxdiam),
                    sy = dbTocd(db, appParams$maxdiam),
                    lum = appParams$bglum + pars$lum,
                    col = appParams$stcol, d = appParams$presTime, w = w)
          return(s)
        }, list(x = x, y = y, w = w))
        return(ff)
      }
    } else makeStimHelper <- NULL
  }
  return(makeStimHelper = makeStimHelper)
}
# Staircase final function
fourTwo.final2 <- function(state) {
  # if done, then send final result. If no reversals yet, then starting estimate
  if(!is.na(state$stairResult)) return(state$stairResult)
  if(length(unique(state$responses)) < 2) return(state$startingEstimate)
  # average the last seen and the last not seen
  return((tail(state$stimuli[state$responses], 1) + tail(state$stimuli[!state$responses], 1) / 2))
}
# Full Threshold final function. Same as for Staircase
FT.final2 <- function(state) {
  est <- FT.final(state)
  return(ifelse(is.na(est), state$startingEstimate, est))
}
# implementation of MOCS
MOCS.start <- function(guess, domain, nreps, minstim, makeStim = NULL, ...) {
  pars <- MOCSpars(guess, domain, nreps, minstim)
  return(list(name = "MOCS",
              domain = pars$domain,
              estimate = pars$guess,
              series = pars$series,                 # random series of presentations
              currentLevel = pars$currentLevel,
              makeStim = makeStim,
              range = c(minstim, 0),                # range of stimulus for the device
              finished = FALSE,                     # flag to say it is finished
              numPresentations = 0,                 # number of presentations so far
              stimuli = NULL,                       # vector of stims shown
              responses = NULL,                     # vector of responses (1 seen, 0 not)
              responseTimes = NULL,                 # vector of response times
              opiResp = NULL,                       # list of opiPresent return values
              opiParams = list(...)))               # the extra params
}
# process step
MOCS.step <- function(state) {
  if (is.null(state$opiParams))
    params <- list(stim = state$makeStim(state$currentLevel, state$numPresentations))
  else
    params <- c(list(stim = state$makeStim(state$currentLevel, state$numPresentations)), state$opiParams)
  opiResp <- do.call(opiPresent, params)
  while(!is.null(opiResp$err))
    opiResp <- do.call(opiPresent, params)
  state$stimuli <- c(state$stimuli, state$currentLevel)
  state$responses <- c(state$responses, opiResp$seen)
  state$responseTimes <- c(state$responseTimes, opiResp$time)
  state$numPresentations <- state$numPresentations + 1
  state$currentLevel <- state$series[state$numPresentations]
  # check if finished
  if(length(state$stimuli) == length(state$series))
    state$finished <- TRUE
  return(list(state = state, resp = opiResp))
}
# check if finished
MOCS.stop <- function(state) {
  return(state$finished)
}
# return best brute-force linear estimate
MOCS.final <- function(state) {
  if(is.null(state$responses) || length(unique(state$stimuli)) < length(state$domain))
    return(state$estimate)
  if(all(state$responses)) return(max(state$domain))
  if(all(!state$responses)) return(min(state$domain))
  # get FOS data
  seen <- table(state$stimuli[state$responses])
  notseen <- table(state$stimuli[!state$responses])
  dat <- matrix(c(rep(0, 2 * length(state$domain))), length(state$domain), 2)
  dat[which(state$domain %in% names(seen)),1] <- seen
  dat[which(state$domain %in% names(notseen)),2] <- notseen
  if(any(dat[,1] + dat[,2] < 4)) return(state$estimate)
  p <- dat[,1] / (dat[,1] + dat[,2])
  idx1 <- which.min(abs(p - 0.5))
  if(idx1 == 1 || idx1 == length(state$domain) || p[idx1] == 0.5)
    idx2 <- idx1
  else if(p[idx1] < 0.5)
    idx2 <- idx1 - 1
  else if(p[idx1] > 0.5)
    idx2 <- idx1 + 1
  return(round((state$domain[idx1] + state$domain[idx2]) / 2, 1))
}
# constructing parameters for MOCS
MOCSpars <- function(guess, domain, nreps, minstim) {
  if(guess < 0) guess <- 0
  if(guess > minstim) guess <- minstim
  domain <- guess + domain
  domain <- domain[domain >= 0 & domain <= minstim]
  series <- sample(rep(domain, nreps), nreps * length(domain), replace = FALSE)
  currentLevel <- series[1]
  return(list(domain = domain, guess = guess, series = series,
              currentLevel = currentLevel))
}
# PMF for healthy subjects
healthy_pmf <- function(domain, guess, dbstep) {
  pmfh <- data.frame(db = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
                     p = c(0.03, 0.05, 0.1, 0.2, 0.3, 0.2, 0.05, 0.025, 0.01))
  pmfh$db <- pmfh$db + guess
  pmf <- approx(pmfh$db, pmfh$p, domain, yleft = 0, yright = 0)$y
  return(pmf / sum(pmf))
}
# PMF for glaucoma patients
glaucoma_pmf <- function(domain, dbstep) {
  pmfg <- data.frame(db = c(-1, 0, 1, 2, 3, 4),
                     p = c(0.2, 0.3, 0.2, 0.15, 0.1, 0.02))
  pmf <- approx(pmfg$db, pmfg$p, domain, yleft = 0, yright = 0)$y
  return(pmf / sum(pmf))
}
# bimodal PMF prior
bimodal_pmf <- function(domain, guess, dbstep, weight = 4) {
  if(guess %% dbstep < dbstep / 2) {
    guess <- guess - guess %% dbstep
  } else guess <- guess + dbstep - guess %% dbstep
  pmfb <- healthy_pmf(domain, guess, dbstep) * weight + glaucoma_pmf(domain, dbstep)
  pmfb[which(pmfb == 0)] <- 0.001
  return(pmfb / sum(pmfb))
}
# Create table of neighboring locations
findNeighbors <- function(locs) {
  if(nrow(locs) == 1) return(NULL)
  # obtain the vertices from Voronoi tessellation
  vertices <- do.call(rbind, lapply(tile.list(deldir(locs)), function(tt)
    return(data.frame(loc = tt$ptNum ,x = tt$x, y = tt$y))))
  nn <- do.call(rbind, lapply(1:max(vertices$loc), function(loc) {
    idx <- which(vertices$loc == loc)
    pntVertices <- vertices[idx,]
    nn <- rep(FALSE, max(vertices$loc))
    idx <- unique(vertices$loc[which(vertices$x %in% pntVertices$x & vertices$y %in% pntVertices$y)])
    sign(locs$y) != sign(locs$y[loc])
    nn[idx] <- TRUE
    # a location is not a neighbor of itself
    nn[loc] <- FALSE
    # locations that are not on the same hemifield (superior or inferior)
    # are not neigbhros
    nn[sign(locs$y) != sign(locs$y[loc])] <- FALSE
    return(nn)
  }))
  return(nn)
}
# open up new locations to test in the growth pattern
setupNewLocs <- function(states, settings, loc) {
  done <- sapply(states, function(s) settings$stopf(s))
  locs <- which(settings$nn[loc,] & !done)
  locs <- locs[!(locs %in% settings$unfinished)] # if they are already open, remove
  if(length(locs) == 0) return(NULL)
  # for each newly opened locations
  for(loc in locs) {
    # select unfinished neighboring locations and
    # calculate average threshold of finished neighboring
    est <- mean(sapply(which(settings$nn[loc,] & done), function(l) settings$finalf(states[[l]])))
    if(est < settings$maxstim) est <- settings$maxstim
    if(est > settings$minstim) est <- settings$minstim
    # for ZEST, create PMF based on the inherited est
    if(settings$algorithm == "ZEST")
      states[[loc]]$pdf <- bimodal_pmf(settings$domain, est, settings$dbstep)
    # for staircase or full threshold, starting estimate is inherited est
    if(settings$algorithm == "staircase" | settings$algorithm == "FT")
      states[[loc]]$startingEstimate <- est
    # for MOCS, mid point of the MOCS algorithm is inherited est
    if(settings$algorithm == "MOCS") {
      pars <- MOCSpars(est, settings$domain, settings$nreps, settings$minstim)
      states[[loc]]$domain <- pars$domain
      states[[loc]]$estimate <- pars$guess
      states[[loc]]$series <- pars$series
      states[[loc]]$currentLevel <-pars$currentLevel
    }
  }
  return(list(states = states, locs = locs))
}