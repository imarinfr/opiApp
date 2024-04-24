library(OPI)
library(deldir)
source("modules/utils.r")
source("modules/serverUtils.r")
load("config/appParams.rda")
load("config/grids.rda")
# setup test
machine <- "PhoneHMD"
eye <- "R"
perimetry <- "luminance"
algorithm <- "MOCS"
grid <- "FOS3"
size <- appParams$size
lum <- appParams$lum
dbstep <- 1
estSD <- appParams$estSD
nreps <- 2
range <- 6

statement <- paste("opiInit", machine)
do.call(what = opiInitialize, args = parseMessage(statement, appParams)$pars)
# set background
if(machine == "PhoneHMD") {
  statement <- paste("opiSetBackground", "B", "B", "cross")
} else {
  statement <- paste("opiSetBackground")
}
do.call(what = opiSetBackground, args = parseMessage(statement, appParams)$pars)


statement <- paste("opiTestInit", eye, perimetry, algorithm, grid,
                   size, lum, dbstep, estSD, nreps, range)
pars <- parseMessage(statement, appParams)$pars
if(pars$grid == "fovea") {
  locs <- data.frame(x = 0, y = 0, w = 1, est = 30)
} else
  locs <- grids[[pars$grid]]$locs
if(chooseOPI()[.OpiEnv$chooser] == "PhoneHMD" & pars$perimetry == "luminance") {
  maxlum <- tail(appParams$lut, 1) - appParams$lut[which.min(abs(appParams$lut - appParams$bglum))]
  locs$est <- round(cdTodb(dbTocd(locs$est), maxlum), 1)
}
setup <- testSetup(chooseOPI()[.OpiEnv$chooser], appParams, pars, locs)
states <- setup$states
settings <- setup$settings

pars <- parseMessage(statement, appParams)$pars
if(grid == "fovea") {
  locs <- data.frame(x = 0, y = 0, w = 1, est = 30)
} else {
  locs <- grids[[pars$grid]]$locs
}

if(chooseOPI()[.OpiEnv$chooser] == "PhoneHMD" & pars$perimetry == "luminance") {
  maxlum <- tail(appParams$lut, 1) - appParams$lut[which.min(abs(appParams$lut - appParams$bglum))]
  locs$est <- round(cdTodb(dbTocd(locs$est), maxlum), 1)
}
setup <- testSetup(machine, appParams, pars, locs)
states <- setup$states
settings <- setup$settings

print("domain:")
print(states[[1]]$domain)
while(!all(sapply(states, function(s) settings$stopf(s)))) {
  rs <- testStep(states, settings)
  states <- rs$states
  settings <- rs$settings
  print("last presented:")
  print(round(sapply(states, function(s) ifelse(is.null(tail(s$stimuli, 1)), NA, tail(s$stimuli, 1))), 1))
  print("estimate:")
  print(round(sapply(states, function(s) settings$finalf(s)), 1))
}

opiClose()