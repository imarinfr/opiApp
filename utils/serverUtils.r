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
    if(msg[2] == "Octopus900" & length(msg) != 3) return(NULL)
    if(length(msg) != 2) return(NULL)
    if(!chooseOPI(msg[2])) return(NULL)
    pars <- opiInitParams(msg, appParams)
  }
  if(cmd == "opiSetBackground") {
    if(is.na(chooseOPI()[.OpiEnv$chooser])) return(NULL)
    if(chooseOPI()[.OpiEnv$chooser] == "PhoneVR") {
      if(length(msg) != 4) return(NULL)
    } else if(length(msg) != 1) return(NULL)
    pars <- opiBackgroundParams(chooseOPI()[.OpiEnv$chooser], appParams, msg)
  }
  if(cmd == "opiTestInit") {
    if(length(msg) != 7) return(NULL)
    pars <- opiTestInitParams(msg)
    if(is.na(pars$val) | is.na(pars$algval)) return(NULL)
  }
  if(cmd == "opiTestStepRun") {
    if(length(msg) != 1) return(NULL)
  }
  if(cmd == "opiTestCatchTrial") {
    if(length(msg) != 5) return(NULL)
    pars <- opiTestCatchTrialParams(msg)
  }
  if(cmd == "opiClose") {
    if(length(msg) != 1) return(NULL)
  }
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
  if(msg[2] == "PhoneVR") {
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
    pars$eye <- msg[3]
  }
  if(msg[2] == "SimHenson") {
    pars$type <- "X"
    pars$A <- 1
    pars$B <- 3.62
    pars$cap <- 6
    pars$maxStim <- appParams$maxlum
  }
  return(pars)
}
opiBackgroundParams <- function(machine, appParams, msg) {
  pars <- opiGetParams("opiSetBackground")
  if(machine == "PhoneVR") {
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
    pars$lum <- NA
    pars$color <- NA
    pars$fixation <- NA
    pars$fixIntensity <- NA
    }
  if(substr(machine, 1, 3) == "Sim") {
    pars$col <- NA
    pars$gridCol <- NA
  }
  return(pars)
}
opiTestInitParams <- function(msg)
  return(list(eye = msg[2], perimetry = msg[3],
              algorithm = msg[4], val = as.numeric(msg[5]),
              algval = as.numeric(msg[6]), grid = msg[7]))
opiTestCatchTrialParams <- function(msg)
  return(list(loc = as.numeric(msg[2]),
              x = as.numeric(msg[3]), y = as.numeric(msg[4]),
              db = as.numeric(msg[5])))
# prepare test settings
testSetup <- function(machine, appParams, eye, perimetry, algorithm, val, algval, locs) {
  # get stimulus helper for the specific machine (OPI implementation) and
  # perimetry type (luminance or size)
  pars <- makeStimHelperConstructor(machine, perimetry, eye, val, appParams)
  # get the rest of algorithm-dependent parameters
  settings <- NULL
  states <- NULL
  maxStimulus <- round(cdTodb(pars$minVal, pars$maxVal))
  domain <- 0:maxStimulus
  if(algorithm == "ZEST") {
    offset <- 5
    domain <- -offset:(maxStimulus + offset)
    guess <- round(85 * maxStimulus / 100) # 85th percentile
    # we do not pass the makeStim here, but generate one at every iteration to be
    # able to modify the response window
    for(i in 1:nrow(locs))
      states[[i]] <- ZEST.start(domain = domain,
                                prior = makePMF(domain, guess),
                                stopType = "S", stopValue = algval,
                                minStimulus = 0, maxStimulus = maxStimulus,
                                makeStim = NULL)
    settings$stepf <- ZEST.step
    settings$stopf <- ZEST.stop
    settings$finalf <- ZEST.final
  } else if(algorithm == "FT") {
    for(i in 1:nrow(locs))
      states[[i]] <- FT.start(est = algval, instRange = c(0, maxStimulus), makeStim = NULL)
    settings$stepf <- FT.step
    settings$stopf <- FT.stop
    settings$finalf <- FT.final2
  } else if(algorithm == "staircase") {
    for(i in 1:nrow(locs))
      states[[i]] <- fourTwo.start(est = algval, instRange = c(0, maxStimulus), makeStim = NULL)
    settings$stepf <- fourTwo.step
    settings$stopf <- fourTwo.stop
    settings$finalf <- fourTwo.final2 # we use a different one to return any value at all
  } else if(algorithm == "MOCS") {
    nreps <- algval
    guess <- round(75 * maxStimulus / 100) # TODO: placeholder. 75th percentile
    levelRange <- c(-6, 6) # TODO: placeholder. Range of levels to test around the estimate
    dbSep <- 2 # TODO: placeholder. Stimulus separation in dBs
    for(i in 1:nrow(locs))
      states[[i]] <- MOCS.start(domain, nreps, guess, levelRange, dbSep, makeStim = NULL)
    settings$stepf <- MOCS.step
    settings$stopf <- MOCS.stop
    settings$finalf <- MOCS.final
  }
  if(!is.null(settings)) {
    settings$machine <- machine
    settings$perimetry <- perimetry
    settings$algorithm <- algorithm
    settings$x <- locs$x
    settings$y <- locs$y
    settings$w <- locs$w
    settings$unfinished <- which(settings$w == 1)
    # make stimulus helper, locations waves, initial locations, and response window
    settings$makeStimHelper <- pars$makeStimHelper
    settings$domain <- domain
    settings$maxStimulus <- maxStimulus
    settings$nn <- findNeighbors(locs)
    settings$respWin <- appParams$respWin
    settings$winFloor <- appParams$winFloor
    settings$minRespWin <- appParams$minRespWin
    settings$minISI <- appParams$minISI
    settings$movingResp <- rep(appParams$respWin, appParams$slidWidth)
  }
  return(list(states = states, settings = settings))
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
  sr <- settings$stepf(states[[loc]])
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
  th <- ifelse(th > settings$maxStimulus, settings$maxStimulus, th)
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
                         th = round(th),
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
  if(machine == "PhoneVR") {
    lum <- round(stim$lum)
    size <- stim$sx
    w <- stim$w
    col <- stim$col
  } else {
    lum <- round(stim$level)
    size <- stim$size
    w <- stim$responseWindow
    col <- "#FFFFFF"
  }
  return(list(lum = lum, size = size, w = w, col = col))
}
# stimulus helper constructor
makeStimHelperConstructor <- function(machine, perimetry, eye, val, appParams) {
  if(perimetry == "luminance") {
    pars <- list(eye = eye, val = val, bglum = appParams$bglum, col = appParams$stcol,
                 minval = appParams$minlum, maxval = appParams$maxlum,
                 presTime = appParams$presTime)
  } else if(perimetry == "size") {
    pars <- list(eye = eye, val = val, bglum = appParams$bglum, col = appParams$stcol,
                 minval = appParams$mindiam, maxval = appParams$maxdiam,
                 presTime = appParams$presTime)
  } else makeStimHelper <- NULL
  if(perimetry == "luminance") {
    if(machine == "PhoneVR") {
      makeStimHelper <- function(x, y, w) {  # returns a function of (db,n)
        ff <- function(db, n) db + n
        body(ff) <- substitute({
          size <- pars$val
          lum <- dbTocd(db, pars$maxval)
          s <- list(eye = pars$eye,
                    x = ifelse(pars$eye == "L", -x, x), y = y,
                    sx = size, sy = size, lum = pars$bglum + lum,
                    col = pars$col,
                    d = pars$presTime, w = w)
          return(s)
        }, list(x = x, y = y, w = w))
        return(ff)
      }
    } else if(machine == "imo") {
      makeStimHelper <- function(x, y, w) {  # returns a function of (db,n)
        ff <- function(db, n) db + n
        body(ff) <- substitute({
          s <- list(eye = pars$eye, x = x, y = y,
                    level = dbTocd(db, pars$maxval), size = pars$val,
                    duration = pars$presTime, responseWindow = w)
          class(s) <- "opiStaticStimulus"
          return(s)
        }, list(x = x, y = y, w = w))
        return(ff)
      }
    } else if(machine == "Compass" | machine == "Octopus900" | substr(machine, 1, 3) == "Sim") {
      makeStimHelper <- function(x, y, w) {  # returns a function of (db,n)
        ff <- function(db, n) db + n
        body(ff) <- substitute({
          s <- list(x = x, y = y,
                    level = dbTocd(db, pars$maxval), size = pars$val,
                    duration = pars$presTime, responseWindow = w)
          class(s) <- "opiStaticStimulus"
          return(s)
        }, list(x = x, y = y, w = w))
        return(ff)
      }
    } else makeStimHelper <- NULL
  } else if(perimetry == "size") {
    if(machine == "PhoneVR") {
      makeStimHelper <- function(x, y, w) {  # returns a function of (db,n)
        ff <- function(db, n) db + n
        body(ff) <- substitute({
          size <- dbTocd(db, pars$maxval)
          lum <- pars$bglum + pars$val
          s <- list(eye = pars$eye,
                    x = ifelse(pars$eye == "L", -x, x), y = y,
                    sx = size, sy = size, lum = lum,
                    col = pars$col,
                    d = pars$presTime, w = w)
          return(s)
        }, list(x = x, y = y, w = w))
        return(ff)
      }
    } else if(machine == "imo") {
      makeStimHelper <- function(x, y, w) {  # returns a function of (db,n)
        ff <- function(db, n) db + n
        body(ff) <- substitute({
          s <- list(eye = pars$eye, x = x, y = y,
                    level = pars$val, size = dbTocd(db, pars$maxval),
                    duration = pars$presTime, responseWindow = w)
          class(s) <- "opiStaticStimulus"
          return(s)
        }, list(x = x, y = y, w = w))
        return(ff)
      }
    } else if(machine == "Compass" | machine == "Octopus900" | substr(machine, 1, 3) == "Sim") {
      makeStimHelper <- function(x, y, w) {  # returns a function of (db,n)
        ff <- function(db, n) db + n
        body(ff) <- substitute({
          s <- list(x = x, y = y,
                    level = pars$val, size = dbTocd(db, pars$maxval),
                    duration = pars$presTime, responseWindow = w)
          class(s) <- "opiStaticStimulus"
          return(s)
        }, list(x = x, y = y, w = w))
        return(ff)
      }
    } else makeStimHelper <- NULL
  }
  return(list(minVal = pars$minval, maxVal = pars$maxval, makeStimHelper = makeStimHelper))
}
# Staircase final function
fourTwo.final2 <- function(state) {
  # if done, then send final result. If no reversals yet, then starting estimate
  if(!is.na(state$stairResult)) return(state$stairResult)
  if(length(unique(state$responses)) < 2) return(state$startingEstimate)
  # average the last seen and the last not seen
  return(round((tail(state$stimuli[state$responses], 1) + tail(state$stimuli[!state$responses], 1)) / 2))
}
# Full Threshold final function. Same as for Staircase
FT.final2 <- function(state) {
  est <- FT.final(state)
  return(ifelse(is.na(est), state$startingEstimate, est))
}
# implementation of MOCS
MOCS.start <- function(domain, nreps, guess, levelRange, dbSep, makeStim, ...) {
  range <- guess + seq(levelRange[1], levelRange[2], by = dbSep)
  if(min(range) < min(domain) || max(range) > max(domain))
    stop("MOCS.start: testing range must be contained device range")
  series <- sample(rep(range, nreps), nreps * length(range), replace = FALSE)
  return(list(name = "MOCS",
              estimate = guess,
              range = range,
              series = series,                      # random series of presentations
              currentLevel = series[1],
              makeStim = makeStim,
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
  if(all(state$responses)) return(99)
  if(all(!state$responses)) return(-2)
  # best linear estimate of the threshold at p(seen) = 0.5
  seenTrials <- state$stimuli[state$responses]
  notSeenTrials <- state$stimuli[!state$responses]
  # get FOS data
  prob <- sapply(state$range, function(db) {
    idxseen <- which(names(seenTrials) %in% db)
    if(length(idxseen) == 0) return(0)
    idxnot <- which(names(notSeenTrials) %in% db)
    if(length(idxnot) == 0) return(1)
    return(seenTrials[idxseen] / (seenTrials[idxseen] + notSeenTrials[idxnot]))
  })
  if(all(prob == 0)) return(mean(state$range))
  est <- approx(prob, state$range, 0.5)$y #TODO placeholder
  return(est)
}
# set up probability mass function (PMF)
makePMF <- function (domain, guess, weight = 4, floor = 0.001) {
  glaucoma_pmf <- rep(0.001,length(domain))
  glaucoma_pmf[1:10] <- c(rep(0.001, 4), 0.2, 0.3, 0.2, 0.15, 0.1, 0.02)
  healthy_pmf <- function(normalModePoint) {
    temp <- c(rep(0.001, 100), 0.001, 0.03, 0.05, 0.1, 0.2, 0.3, 0.2, 0.05, 0.025, 0.01, rep(0.001, 100))
    mode <- which.max(temp)
    return(temp[(mode - normalModePoint + domain[1]):(mode - normalModePoint + domain[length(domain)])])
  }
  # bimodal prior
  makeBimodalPMF <- function(normalModePoint, weight, pdf.floor) {
    npdf <- healthy_pmf(normalModePoint)
    cpdf <- npdf * weight + glaucoma_pmf
    cpdf[which(cpdf < pdf.floor)] = pdf.floor 
    return(cpdf)
  }
  # define prior PMF, minimum stimulus and maximum stimulus for luminance test
  # bimodal Prior PMF
  prior_pmf <- makeBimodalPMF(guess, weight, floor)
  # normalize PMF
  prior_pmf <- prior_pmf / sum(prior_pmf)
  return(prior_pmf)
}
# Create table of neighboring locations
findNeighbors <- function(locs) {
  if(nrow(locs) == 1) return(NULL)
  # obtain the vertices from Voronoi tessellation
  vertices <- do.call(rbind, lapply(tile.list(deldir(locs)), function(tt) return(data.frame(loc = tt$ptNum ,x = tt$x, y = tt$y))))
  nn <- do.call(rbind, lapply(1:max(vertices$loc), function(loc) {
    idx <- which(vertices$loc == loc)
    pntVertices <- vertices[idx,]
    nn <- rep(FALSE, max(vertices$loc))
    idx <- unique(vertices$loc[which(vertices$x %in% pntVertices$x & vertices$y %in% pntVertices$y)])
    nn[idx] <- TRUE
    nn[loc] <- FALSE
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
    th <- mean(sapply(which(settings$nn[loc,] & done), function(l) settings$finalf(states[[l]])))
    # create PMF based on the average threshold
    states[[loc]]$pdf <- makePMF(settings$domain, th)
  }
  return(list(states = states, locs = locs))
}