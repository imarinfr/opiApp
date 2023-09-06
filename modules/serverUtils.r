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
    if(pars$fixtype == "annulus") pars$fixsy <- pars$fixsx <- 4
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
              level = as.numeric(msg[5])))
# prepare test settings
testSetup <- function(machine, appParams, pars, locs) {
  # get the rest of algorithm-dependent parameters
  settings <- NULL
  states <- NULL
  if(machine == "PhoneHMD") return(setupPhoneHMD(machine, appParams, pars, locs))
  if(machine == "Compass") return(setupCompass(machine, appParams, pars, locs))
  if(machine == "Octopus900") return(setupOctopus(machine, appParams, pars, locs))
  if(machine == "imo") return(setupIMO(machine, appParams, pars, locs))
  if(substr(machine, 1, 3) == "Sim") return(setupSimulation(machine, appParams, pars, locs))
}
# set up for PhoneHMD
setupPhoneHMD <- function(machine, appParams, pars, locs)
  return(switch(pars$perimetry,
                "luminance" = setupPhoneHMDLuminance(machine, appParams, pars, locs),
                "size" = setupPhoneHMDSize(machine, appParams, pars, locs)))
# set up for PhoneHMD luminance
setupPhoneHMDLuminance <- function(machine, appParams, pars, locs) {
  bgidx <- which.min(abs(appParams$lut - appParams$bglum))
  appParams$bglum <- appParams$lut[bgidx] # use the correct background luminance applied from LUT
  maxlum <- tail(appParams$lut, 1) - appParams$bglum # use the correct maximum luminance applied from LUT
  minlum <- appParams$lut[bgidx + 1] - appParams$bglum # min incremental value from background
  makeStimHelper <- makeStimHelperConstructorPhoneHMDLuminance(appParams, pars, maxlum)
  dBmax <- round(cdTodb(minlum, maxlum), 1)
  st <- initStates(machine, appParams, pars, dBmax, locs)
  settings <- initSettings(machine, appParams, pars, makeStimHelper,
                           st$domain, dBmax, maxlum, locs)
  return(list(states = st$states, settings = settings))
}
# set up for PhoneHMD size
setupPhoneHMDSize <- function(machine, appParams, pars, locs) {
  maxdiam <- 0.43 * sqrt(10000 / pi / pars$lum)
  dBmax <- 40
  makeStimHelper <- makeStimHelperConstructorPhoneHMDSize(appParams, pars)
  st <- initStatesSize(appParams, pars, dBmax, locs)
  settings <- initSettings(machine, appParams, pars, makeStimHelper,
                           st$domain, dBmax, maxdiam, locs)
  return(list(states = st$states, settings = settings))
}
# set up for Compass
setupCompass <- function(machine, appParams, pars, locs) {
  maxlum <- .OpiEnv$Compass$ZERO_DB_IN_ASB / pi
  dBmax <- ifelse(pars$algorithm == "ZEST", 40, 50)
  makeStimHelper <- makeStimHelperConstructorCompass(appParams, pars, maxlum)
  st <- initStates(machine, appParams, pars, dBmax, locs)
  settings <- initSettings(machine, appParams, pars, makeStimHelper,
                           st$domain, dBmax, maxlum, locs)
  return(list(states = st$states, settings = settings))
}
# set up Octopus
setupOctopus <- function(machine, appParams, pars, locs) {
  maxlum <- ifelse(appParams$O900max, 10000, 4000) / pi
  dBmax <- ifelse(pars$algorithm == "ZEST", 40, 50)
  makeStimHelper <- makeStimHelperConstructorO900(appParams, pars, maxlum)
  st <- initStates(machine, appParams, pars, dBmax, locs)
  settings <- initSettings(machine, appParams, pars, makeStimHelper,
                           st$domain, dBmax, maxlum, locs)
  return(list(states = st$states, settings = settings))
}
# set up IMO
setupIMO <- function(machine, appParams, pars, locs) {
  maxlum <- 10000 / pi
  dBmax <- ifelse(pars$algorithm == "ZEST", 40, 50)
  makeStimHelper <- makeStimHelperConstructorIMO(appParams, pars, maxlum)
  st <- initStates(machine, appParams, pars, dBmax, locs)
  settings <- initSettings(machine, appParams, pars, makeStimHelper,
                           st$domain, dBmax, maxlum, locs)
  return(list(states = st$states, settings = settings))
}
# set up Simulation
setupSimulation <- function(machine, appParams, pars, locs) {
  maxlum <- 10000 / pi
  dBmax <- ifelse(pars$algorithm == "ZEST", 40, 50)
  makeStimHelper <- makeStimHelperConstructorSim(appParams, pars, maxlum)
  st <- initStates(machine, appParams, pars, dBmax, locs)
  settings <- initSettings(machine, appParams, pars, makeStimHelper,
                           st$domain, dBmax, maxlum, locs)
  return(list(states = st$states, settings = settings))
}
# create states and settings
initStates <- function(machine, appParams, pars, dBmax, locs)
  return(switch(pars$algorithm,
                "ZEST" = initStatesZEST(machine, appParams, pars, dBmax, locs),
                "MOCS" = initStatesMOCS(machine, appParams, pars, dBmax, locs),
                "FT" = initStatesFT(pars, dBmax, locs),
                "staircase" = initStatesStaircase(pars, dBmax, locs)))
# create states and settings
initStatesSize <- function(appParams, pars, dBmax, locs)
  return(switch(pars$algorithm,
                "ZEST" = initStatesSizeZEST(appParams, pars, dBmax, locs),
                "MOCS" = initStatesSizeMOCS(appParams, pars, dBmax, locs),
                "FT" = initStatesFT(pars, dBmax, locs),
                "staircase" = initStatesStaircase(pars, dBmax, locs)))
# init states for ZEST luminance algorithm
initStatesZEST <- function(machine, appParams, pars, dBmax, locs) {
  states <- NULL
  offset <- 5
  domain <- seq(0, 40, by = pars$dbstep)
  if(machine == "PhoneHMD") {
    domain <- phoneDomainZEST(domain, appParams$bglum, appParams$lut, pars$dbstep, offset)
    # adjust estimates to the closet feasible stimulus luminance
    locs$est <- domain[sapply(locs$est, function(est) return(which.min(abs(est - domain))))]
    dBmax <- tail(domain, 1)
  }
  tail <- seq(pars$dbstep, offset, by = pars$dbstep)
  domain <- c(-tail[length(tail):1], domain, domain[length(domain)] + tail)
  for(i in 1:nrow(locs))
    states[[i]] <- ZEST.start(domain = domain,
                              prior = bimodal_pmf(domain, locs$est[i], pars$dbstep),
                              stopType = "S", stopValue = pars$estSD,
                              minStimulus = 0, maxStimulus = dBmax,
                              makeStim = NULL)
  return(list(domain = domain, states = states))
}
# init states for MOCS luminance algorithm
initStatesMOCS <- function(machine, appParams, pars, dBmax, locs) {
  states <- NULL
  for(i in 1:nrow(locs)) {
    domain <- locs$est[i] + seq(-pars$range / 2, pars$range / 2, by = pars$dbstep)
    domain <- domain[domain >= 0 & domain <= dBmax]
    if(machine == "PhoneHMD")
      domain <- phoneDomainMOCS(domain, appParams$bglum, appParams$lut,
                                pars$dbstep, pars$range, locs$est[i])
    states[[i]] <- MOCS.start(domain, pars$nreps, dBmax)
  }
  return(list(domain = seq(-pars$range / 2, pars$range / 2, by = pars$dbstep), states = states))
}
# init states for ZEST size algorithm
initStatesSizeZEST <- function(appParams, pars, dBmax, locs) {
  states <- NULL
  offset <- 5
  domain <- seq(-offset, dBmax + offset, by = appParams$dbstep)
  for(i in 1:nrow(locs))
    states[[i]] <- ZEST.start(domain = domain,
                              prior = bimodal_pmf(domain, locs$est[i]),
                              stopType = "S", stopValue = pars$estSD,
                              minStimulus = 0, maxStimulus = dBmax,
                              makeStim = NULL)
  return(list(domain = domain, states = states))
}
# init states for MOCS size algorithm
initStatesSizeMOCS <- function(appParams, pars, dBmax, locs) {
  states <- NULL
  for(i in 1:nrow(locs)) {
    domain <- locs$est[i] + seq(-pars$range / 2, pars$range / 2, by = pars$dbstep)
    domain <- domain[domain >= 0 & domain <= dBmax]
    states[[i]] <- MOCS.start(domain, pars$nreps, dBmax)
  }
  return(list(domain = seq(-pars$range / 2, pars$range / 2, by = pars$dbstep), states = states))
}
# init states for full threshold for both luminance and size
initStatesFT <- function(pars, dBmax, locs) {
  states <- NULL
  domain <- c(0, dBmax)
  for(i in 1:nrow(locs))
    states[[i]] <- FT.start(est = locs$est[i], instRange = c(0, dBmax), makeStim = NULL)
  return(list(domain = domain, states = states))
}
# init states for staircase for both luminance and size
initStatesStaircase <- function(pars, dBmax, locs) {
  states <- NULL
  domain <- c(0, dBmax)
  for(i in 1:nrow(locs))
    states[[i]] <- fourTwo.start(est = locs$est[i], instRange = c(0, dBmax), makeStim = NULL)
  return(list(domain = domain, states = states))
}
# get domain for ZEST luminance for phone
phoneDomainZEST <- function(intended, bglum, lut, dbstep, offset) {
  intended <- intended - cdTodb(tail(lut, 1) - bglum)
  intended <- intended[intended >= 0]
  if(all(intended != 0)) intended <- c(0, intended)
  dbLevels <- cdTodb(lut[lut > bglum] - bglum, tail(lut, 1) - bglum)
  dbLevels <- dbLevels[length(dbLevels):1]
  domain <- NULL
  while(TRUE) {
    idx <- which.min(abs(dbLevels - intended[1]))
    domain <- c(domain, dbLevels[idx])
    intended <- intended[intended > tail(domain, 1) + dbstep / 2]
    if(idx == length(dbLevels)) break
    else dbLevels <- dbLevels[(idx + 1):length(dbLevels)]
  }
  return(domain)
}
# get domain for MOCS luminance for phone
phoneDomainMOCS <- function(intended, bglum, lut, dbstep, range, est) {
  dbLevels <- cdTodb(lut[lut > bglum] - bglum, tail(lut, 1) - bglum)
  dbLevels <- dbLevels[length(dbLevels):1]
  idx <- head(which(dbLevels >= intended[1]), 1)
  if(idx > 1) idx <- idx - 1
  dbLevels <- dbLevels[idx:length(dbLevels)]
  idx <- tail(which(dbLevels <= intended[length(intended)]), 1)
  if(idx < length(dbLevels)) idx <- idx + 1
  dbLevels <- dbLevels[1:idx]
  domain <- NULL
  while(TRUE) {
    idx <- which.min(abs(dbLevels - intended[1]))
    domain <- c(domain, dbLevels[idx])
    intended <- intended[intended > tail(domain, 1) + dbstep / 2]
    if(idx == length(dbLevels)) break
    else dbLevels <- dbLevels[(idx + 1):length(dbLevels)]
  }
  return(domain)
}
# create states and settings
initSettings <- function(machine, appParams, pars, makeStimHelper, domain, dBmax, maxval, locs) {
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
    settings$bglum <- appParams$bglum
    settings$maxval <- maxval
    settings$dBmax <- dBmax
    settings$dBmin <- 0
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
  th <- ifelse(th > settings$dBmax, settings$dBmax, th)
  th <- ifelse(th < 0, 0, th)
  done <- settings$stopf(states[[loc]])
  level <- tail(sr$state$stimuli, 1)
  stimInfo <- getStepStimInfo(settings$machine, states[[loc]]$makeStim(level, 0))
  if(settings$machine == "PhoneHMD" & settings$perimetry == "luminance")
    stimInfo$lum <- stimInfo$lum - settings$bglum
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
  stim <- settings$makeStimHelper(pars$x, pars$y, settings$respWin)(pars$level, 0)
  res <- opiPresent(stim)
  stimInfo <- getStepStimInfo(settings$machine, stim)
  if(settings$machine == "PhoneHMD" & settings$perimetry == "luminance")
    stimInfo$lum <- stimInfo$lum - settings$bglum
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
# stimulus helper constructor for PhoneHMD luminance
makeStimHelperConstructorPhoneHMDLuminance <- function(appParams, pars, maxlum) {
  makeStimHelper <- function(x, y, w) {  # returns a function of (level,n)
    ff <- function(level, n) level + n
    body(ff) <- substitute({
      s <- list(eye = pars$eye,
                x = ifelse(pars$eye == "L", -x, x), y = y,
                sx = pars$size, sy = pars$size,
                lum = appParams$bglum + dbTocd(level, maxlum),
                col = appParams$stcol, d = appParams$presTime, w = w)
      class(s) <- "opiStaticStimulus"
      return(s)
    }, list(x = x, y = y, w = w))
    return(ff)
  }
  return(makeStimHelper)
}
# stimulus helper constructor for PhoneHMD size
makeStimHelperConstructorPhoneHMDSize <- function(appParams, pars) {
  k <- 0.43 * sqrt(10000 / pi / pars$lum)
  makeStimHelper <- function(x, y, w) {  # returns a function of (level,n)
    ff <- function(level, n) level + n
    body(ff) <- substitute({
      diam <- k * 10^(-level / 20)
      s <- list(eye = pars$eye,
                x = ifelse(pars$eye == "L", -x, x), y = y,
                sx = diam, sy = diam,
                lum = appParams$bglum + pars$lum,
                col = appParams$stcol, d = appParams$presTime, w = w)
      return(s)
    }, list(x = x, y = y, w = w))
    return(ff)
  }
  return(makeStimHelper)
}
# stimulus helper constructor for IMO
makeStimHelperConstructorIMO <- function(appParams, pars, maxlum) {
  makeStimHelper <- function(x, y, w) {  # returns a function of (level,n)
    ff <- function(level, n) level + n
    body(ff) <- substitute({
      s <- list(eye = pars$eye,
                x = x, y = y,
                size = pars$size, level = dbTocd(level, maxlum),
                duration = appParams$presTime, responseWindow = w)
      class(s) <- "opiStaticStimulus"
      return(s)
    }, list(x = x, y = y, w = w))
    return(ff)
  }
  return(makeStimHelper)
}
# stimulus helper constructor for Compass
makeStimHelperConstructorCompass <- function(appParams, pars, maxlum) {
  makeStimHelper <- function(x, y, w) {  # returns a function of (level,n)
    ff <- function(level, n) level + n
    body(ff) <- substitute({
      s <- list(x = x, y = y, size = pars$size,
                level = dbTocd(level, maxlum),
                duration = appParams$presTime, responseWindow = w)
      class(s) <- "opiStaticStimulus"
      return(s)
    }, list(x = x, y = y, w = w))
    return(ff)
  }
  return(makeStimHelper)
}
# stimulus helper constructor for Octopus 900
makeStimHelperConstructorO900 <- function(appParams, pars, maxlum) {
  makeStimHelper <- function(x, y, w) {  # returns a function of (level,n)
    ff <- function(level, n) level + n
    body(ff) <- substitute({
      s <- list(x = x, y = y, size = pars$size,
                level = dbTocd(level, maxlum),
                duration = appParams$presTime, responseWindow = w)
      class(s) <- "opiStaticStimulus"
      return(s)
    }, list(x = x, y = y, w = w))
    return(ff)
  }
  return(makeStimHelper)
}
# stimulus helper constructor for simulations
makeStimHelperConstructorSim <- function(appParams, pars, maxlum) {
  makeStimHelper <- function(x, y, w) {  # returns a function of (level,n)
    ff <- function(level, n) level + n
    body(ff) <- substitute({
      s <- list(x = x, y = y, size = pars$size,
                level = dbTocd(level, maxlum),
                duration = appParams$presTime, responseWindow = w)
      class(s) <- "opiStaticStimulus"
      return(s)
    }, list(x = x, y = y, w = w))
    return(ff)
  }
  return(makeStimHelper)
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
MOCS.start <- function(domain, nreps, dBmax, makeStim = NULL, ...) {
  series <- sample(rep(domain, nreps), nreps * length(domain), replace = FALSE)
  return(list(name = "MOCS",
              domain = domain,
              estimate = series[1],
              series = series,                 # random series of presentations
              currentLevel = series[1],
              makeStim = makeStim,
              range = c(dBmax, 0),                  # range of stimulus for the device
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
# PMF for healthy subjects
healthy_pmf <- function(domain, guess) {
  pmfh <- data.frame(db = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
                     p = c(0.03, 0.05, 0.1, 0.2, 0.3, 0.2, 0.05, 0.025, 0.01))
  pmfh$db <- pmfh$db + guess
  pmf <- approx(pmfh$db, pmfh$p, domain, yleft = 0, yright = 0)$y
  return(pmf / sum(pmf))
}
# PMF for glaucoma patients
glaucoma_pmf <- function(domain) {
  pmfg <- data.frame(db = c(-1, 0, 1, 2, 3, 4),
                     p = c(0.2, 0.3, 0.2, 0.15, 0.1, 0.02))
  pmf <- approx(pmfg$db, pmfg$p, domain, yleft = 0, yright = 0)$y
  return(pmf / sum(pmf))
}
# bimodal PMF prior
bimodal_pmf <- function(domain, guess, weight = 4) {
  pmfb <- healthy_pmf(domain, guess) * weight + glaucoma_pmf(domain)
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
    if(est < settings$dBmin) est <- settings$dBmin
    if(est > settings$dBmax) est <- settings$dBmax
    # for ZEST, create PMF based on the inherited est
    if(settings$algorithm == "ZEST")
      states[[loc]]$pdf <- bimodal_pmf(settings$domain, est)
    # for staircase or full threshold, starting estimate is inherited
    if(settings$algorithm == "staircase" | settings$algorithm == "FT")
      states[[loc]]$startingEstimate <- est
    # for MOCS, do nothing
  }
  return(list(states = states, locs = locs))
}